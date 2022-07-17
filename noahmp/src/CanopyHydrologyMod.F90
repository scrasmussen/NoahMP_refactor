module CanopyHydrologyMod

!!! Canopy Hydrology processes for intercepted rain and snow water
!!! Canopy liquid water evaporation and dew; canopy ice water sublimation and frost
  
  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine CanopyHydrology(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CANWATER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                        &
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,    noahmp main time step (s)
              FCEV            => noahmp%energy%flux%FCEV             ,& ! in,    canopy evaporation (w/m2) [+ = to atm]
              FCTR            => noahmp%energy%flux%FCTR             ,& ! in,    transpiration (w/m2) [+ = to atm]
              ELAI            => noahmp%energy%state%ELAI            ,& ! in,    leaf area index, after burying by snow
              ESAI            => noahmp%energy%state%ESAI            ,& ! in,    stem area index, after burying by snow
              TG              => noahmp%energy%state%TG              ,& ! in,    ground temperature (k)
              FVEG            => noahmp%energy%state%FVEG            ,& ! in,    greeness vegetation fraction (-)
              FROZEN_CANOPY   => noahmp%energy%state%FROZEN_CANOPY   ,& ! in,    used to define latent heat pathway
              SnowfallDensity          => noahmp%water%state%SnowfallDensity           ,& ! in,    bulk density of snowfall (kg/m3)
              CH2OP           => noahmp%water%param%CH2OP            ,& ! in,    maximum intercepted water per unit lai+sai (mm)
              CanopyLiqWater          => noahmp%water%state%CanopyLiqWater           ,& ! inout, intercepted canopy liquid water [mm]
              CanopyIce          => noahmp%water%state%CanopyIce           ,& ! inout, intercepted canopy ice [mm]
              TV              => noahmp%energy%state%TV              ,& ! inout, vegetation temperature (k)
              CanopyTotalWater             => noahmp%water%state%CanopyTotalWater              ,& ! out,   total canopy intercepted water [mm]
              CanopyWetFrac            => noahmp%water%state%CanopyWetFrac             ,& ! out,   wetted or snowed fraction of the canopy
              CanopyIceMax          => noahmp%water%state%CanopyIceMax           ,& ! out,   canopy capacity for snow interception [mm]
              CanopyLiqWaterMax          => noahmp%water%state%CanopyLiqWaterMax           ,& ! out,   canopy capacity for rain interception [mm]
              EvapCanopyNet            => noahmp%water%flux%EvapCanopyNet              ,& ! out,   evaporation of intercepted total water [mm/s]
              Transpiration           => noahmp%water%flux%Transpiration             ,& ! out,   transpiration rate [mm/s]
              EvapCanopyLiq           => noahmp%water%flux%EvapCanopyLiq             ,& ! out,   canopy liquid water evaporation rate [mm/s]
              DewCanopyLiq           => noahmp%water%flux%DewCanopyLiq             ,& ! out,   canopy liquid water dew rate [mm/s]
              FrostCanopyIce           => noahmp%water%flux%FrostCanopyIce             ,& ! out,   canopy ice frost rate [mm/s]
              SublimCanopyIce           => noahmp%water%flux%SublimCanopyIce             ,& ! out,   canopy ice sublimation rate [mm/s]
              MeltCanopyIce          => noahmp%water%flux%MeltCanopyIce            ,& ! out,   canopy ice melting rate [mm/s]
              RefrzCanopyLiq           => noahmp%water%flux%RefrzCanopyLiq              & ! out,   canopy water refreezing rate [mm/s]
             )
! --------------------------------------------------------------------

! initialization for out-only variables
    EvapCanopyNet    = 0.0
    Transpiration   = 0.0
    EvapCanopyLiq   = 0.0
    DewCanopyLiq   = 0.0
    FrostCanopyIce   = 0.0
    SublimCanopyIce   = 0.0
    MeltCanopyIce  = 0.0
    RefrzCanopyLiq   = 0.0
    CanopyLiqWaterMax  = 0.0
    CanopyIceMax  = 0.0
    CanopyWetFrac    = 0.0
    CanopyTotalWater     = 0.0

!=== canopy liquid water
    ! maximum canopy intercepted water
    CanopyLiqWaterMax =  CH2OP * (ELAI + ESAI)

    ! canopy evaporation, transpiration, and dew
    if ( FROZEN_CANOPY .eqv. .false. ) then    ! Barlage: change to frozen_canopy
       Transpiration = max( FCTR/ConstLatHeatVapor, 0.0 )
       EvapCanopyLiq = max( FCEV/ConstLatHeatVapor, 0.0 )
       DewCanopyLiq = abs( min( FCEV/ConstLatHeatVapor, 0.0 ) )
       SublimCanopyIce = 0.0
       FrostCanopyIce = 0.0
    else
       Transpiration = max( FCTR/ConstLatHeatSublim, 0.0 )
       EvapCanopyLiq = 0.0
       DewCanopyLiq = 0.0
       SublimCanopyIce = max( FCEV/ConstLatHeatSublim, 0.0 )
       FrostCanopyIce = abs( min( FCEV/ConstLatHeatSublim, 0.0 ) )
    endif

    ! canopy water balance. for convenience allow dew to bring CanopyLiqWater above
    ! maxh2o or else would have to re-adjust drip
    EvapCanopyLiq   = min( CanopyLiqWater/MainTimeStep, EvapCanopyLiq )
    CanopyLiqWater  = max( 0.0, CanopyLiqWater+(DewCanopyLiq-EvapCanopyLiq)*MainTimeStep )
    if ( CanopyLiqWater <= 1.0e-06 ) CanopyLiqWater = 0.0

!=== canopy ice 
    ! maximum canopy intercepted ice
    CanopyIceMax = 6.6 * (0.27 + 46.0/SnowfallDensity) * (ELAI + ESAI)

    ! canopy sublimation and frost
    SublimCanopyIce = min( CanopyIce/MainTimeStep, SublimCanopyIce )
    CanopyIce= max( 0.0, CanopyIce+(FrostCanopyIce-SublimCanopyIce)*MainTimeStep )
    if ( CanopyIce <= 1.0e-6 ) CanopyIce = 0.0

!=== wetted fraction of canopy
    if ( CanopyIce > 0.0 ) then
       CanopyWetFrac = max(0.0,CanopyIce) / max(CanopyIceMax,1.0e-06)
    else
       CanopyWetFrac = max(0.0,CanopyLiqWater) / max(CanopyLiqWaterMax,1.0e-06)
    endif
    CanopyWetFrac = min(CanopyWetFrac, 1.0) ** 0.667

!=== phase change
    ! canopy ice melting
    if ( (CanopyIce > 1.0e-6) .and. (TV > ConstFreezePoint) ) then
       MeltCanopyIce = min( CanopyIce/MainTimeStep, (TV-ConstFreezePoint) * ConstHeatCapacIce * &
                     CanopyIce / ConstDensityIce / (MainTimeStep*ConstLatHeatFusion) )
       CanopyIce = max( 0.0, CanopyIce - MeltCanopyIce*MainTimeStep )
       CanopyLiqWater = max( 0.0, CanopyLiqWater + MeltCanopyIce*MainTimeStep )
       TV     = CanopyWetFrac*ConstFreezePoint + (1.0 - CanopyWetFrac)*TV
    endif

    ! canopy water refreeezing
    if ( (CanopyLiqWater > 1.0e-6) .and. (TV < ConstFreezePoint) ) then
       RefrzCanopyLiq  = min( CanopyLiqWater/MainTimeStep, (ConstFreezePoint-TV) * ConstHeatCapacWater * &
                     CanopyLiqWater / ConstDensityWater / (MainTimeStep*ConstLatHeatFusion) )
       CanopyLiqWater = max( 0.0, CanopyLiqWater - RefrzCanopyLiq*MainTimeStep )
       CanopyIce = max( 0.0, CanopyIce + RefrzCanopyLiq*MainTimeStep )
       TV     = CanopyWetFrac*ConstFreezePoint + (1.0 - CanopyWetFrac)*TV
    ENDIF

!=== update total canopy water
    CanopyTotalWater = CanopyLiqWater + CanopyIce

!=== total canopy net evaporation
    EvapCanopyNet = EvapCanopyLiq + SublimCanopyIce - DewCanopyLiq - FrostCanopyIce

    end associate

  end subroutine CanopyHydrology

end module CanopyHydrologyMod
