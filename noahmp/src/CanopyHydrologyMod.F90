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
              ECAN            => noahmp%water%flux%ECAN              ,& ! out,   evaporation of intercepted water (mm/s) [+]
              ETRAN           => noahmp%water%flux%ETRAN             ,& ! out,   transpiration rate (mm/s) [+]
              QEVAC           => noahmp%water%flux%QEVAC             ,& ! out,   canopy water evaporation rate (mm/s)
              QDEWC           => noahmp%water%flux%QDEWC             ,& ! out,   canopy water dew rate (mm/s)
              QFROC           => noahmp%water%flux%QFROC             ,& ! out,   canopy ice frost rate (mm/s)
              QSUBC           => noahmp%water%flux%QSUBC             ,& ! out,   canopy ice sublimation rate (mm/s)
              QMELTC          => noahmp%water%flux%QMELTC            ,& ! out,   canopy ice melting rate (mm/s)
              QFRZC           => noahmp%water%flux%QFRZC              & ! out,   canopy water refreezing rate (mm/s)
             )
! --------------------------------------------------------------------

! initialization for out-only variables
    ECAN    = 0.0
    ETRAN   = 0.0
    QEVAC   = 0.0
    QDEWC   = 0.0
    QFROC   = 0.0
    QSUBC   = 0.0
    QMELTC  = 0.0
    QFRZC   = 0.0
    CanopyLiqWaterMax  = 0.0
    CanopyIceMax  = 0.0
    CanopyWetFrac    = 0.0
    CanopyTotalWater     = 0.0

!=== canopy liquid water
    ! maximum canopy intercepted water
    CanopyLiqWaterMax =  CH2OP * (ELAI + ESAI)

    ! canopy evaporation, transpiration, and dew
    if ( FROZEN_CANOPY .eqv. .false. ) then    ! Barlage: change to frozen_canopy
       ETRAN = max( FCTR/ConstLatHeatVapor, 0.0 )
       QEVAC = max( FCEV/ConstLatHeatVapor, 0.0 )
       QDEWC = abs( min( FCEV/ConstLatHeatVapor, 0.0 ) )
       QSUBC = 0.0
       QFROC = 0.0
    else
       ETRAN = max( FCTR/ConstLatHeatSublim, 0.0 )
       QEVAC = 0.0
       QDEWC = 0.0
       QSUBC = max( FCEV/ConstLatHeatSublim, 0.0 )
       QFROC = abs( min( FCEV/ConstLatHeatSublim, 0.0 ) )
    endif

    ! canopy water balance. for convenience allow dew to bring CanopyLiqWater above
    ! maxh2o or else would have to re-adjust drip
    QEVAC   = min( CanopyLiqWater/MainTimeStep, QEVAC )
    CanopyLiqWater  = max( 0.0, CanopyLiqWater+(QDEWC-QEVAC)*MainTimeStep )
    if ( CanopyLiqWater <= 1.0e-06 ) CanopyLiqWater = 0.0

!=== canopy ice 
    ! maximum canopy intercepted ice
    CanopyIceMax = 6.6 * (0.27 + 46.0/SnowfallDensity) * (ELAI + ESAI)

    ! canopy sublimation and frost
    QSUBC = min( CanopyIce/MainTimeStep, QSUBC )
    CanopyIce= max( 0.0, CanopyIce+(QFROC-QSUBC)*MainTimeStep )
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
       QMELTC = min( CanopyIce/MainTimeStep, (TV-ConstFreezePoint) * ConstHeatCapacIce * &
                     CanopyIce / ConstDensityIce / (MainTimeStep*ConstLatHeatFusion) )
       CanopyIce = max( 0.0, CanopyIce - QMELTC*MainTimeStep )
       CanopyLiqWater = max( 0.0, CanopyLiqWater + QMELTC*MainTimeStep )
       TV     = CanopyWetFrac*ConstFreezePoint + (1.0 - CanopyWetFrac)*TV
    endif

    ! canopy water refreeezing
    if ( (CanopyLiqWater > 1.0e-6) .and. (TV < ConstFreezePoint) ) then
       QFRZC  = min( CanopyLiqWater/MainTimeStep, (ConstFreezePoint-TV) * ConstHeatCapacWater * &
                     CanopyLiqWater / ConstDensityWater / (MainTimeStep*ConstLatHeatFusion) )
       CanopyLiqWater = max( 0.0, CanopyLiqWater - QFRZC*MainTimeStep )
       CanopyIce = max( 0.0, CanopyIce + QFRZC*MainTimeStep )
       TV     = CanopyWetFrac*ConstFreezePoint + (1.0 - CanopyWetFrac)*TV
    ENDIF

!=== update total canopy water
    CanopyTotalWater = CanopyLiqWater + CanopyIce

!=== total canopy evaporation
    ECAN = QEVAC + QSUBC - QDEWC - QFROC

    end associate

  end subroutine CanopyHydrology

end module CanopyHydrologyMod
