module CanopyWaterInterceptMod

!!! Canopy water processes for snow and rain interception
!!! Subsequent hydrological process for intercepted water is done in CanopyHydrology

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine CanopyWaterIntercept(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: PRECIP_HEAT
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! The water and heat portions of PRECIP_HEAT are separated in refactored code
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: FT         ! temperature factor for unloading rate
    real(kind=kind_noahmp)           :: FV         ! wind factor for unloading rate
    real(kind=kind_noahmp)           :: ICEDRIP    ! canopy ice unloading 

! --------------------------------------------------------------------
    associate(                                                        &
              SurfaceType             => noahmp%config%domain%SurfaceType            ,& ! in,    surface type 1-soil; 2-lake
              MainTimeStep           => noahmp%config%domain%MainTimeStep     ,& ! in,    noahmp main time step (s)
              WindEastwardRefHeight  => noahmp%forcing%WindEastwardRefHeight  ,& ! in,    wind speed [m/s] in eastward direction at reference height
              WindNorthwardRefHeight => noahmp%forcing%WindNorthwardRefHeight ,& ! in,    wind speed [m/s] in northward direction at reference height
              ELAI            => noahmp%energy%state%ELAI            ,& ! in,    leaf area index, after burying by snow
              ESAI            => noahmp%energy%state%ESAI            ,& ! in,    stem area index, after burying by snow
              FVEG            => noahmp%energy%state%FVEG            ,& ! in,    greeness vegetation fraction (-)
              TV              => noahmp%energy%state%TV              ,& ! in,    vegetation temperature (k)
              TG              => noahmp%energy%state%TG              ,& ! in,    ground temperature (k)
              CH2OP           => noahmp%water%param%CH2OP            ,& ! in,    maximum intercepted water per unit lai+sai (mm)
              RAIN            => noahmp%water%flux%RAIN              ,& ! in,    total liquid rainfall (mm/s) before interception
              SNOW            => noahmp%water%flux%SNOW              ,& ! in,    total liquid snowfall (mm/s) before interception
              SnowfallDensity          => noahmp%water%state%SnowfallDensity           ,& ! in,    bulk density of snowfall (kg/m3)
              PrecipAreaFrac              => noahmp%water%state%PrecipAreaFrac               ,& ! in,    fraction of the gridcell that receives precipitation
              CanopyLiqWater          => noahmp%water%state%CanopyLiqWater           ,& ! inout, intercepted canopy liquid water [mm]
              CanopyIce          => noahmp%water%state%CanopyIce           ,& ! inout, intercepted canopy ice [mm]
              CanopyWetFrac            => noahmp%water%state%CanopyWetFrac             ,& ! out,   wetted or snowed fraction of the canopy
              CanopyTotalWater             => noahmp%water%state%CanopyTotalWater              ,& ! out,   total canopy intercepted water [mm]
              CanopyIceMax          => noahmp%water%state%CanopyIceMax           ,& ! out,   canopy capacity for snow interception [mm]
              CanopyLiqWaterMax          => noahmp%water%state%CanopyLiqWaterMax           ,& ! out,   canopy capacity for rain interception [mm]
              QINTR           => noahmp%water%flux%QINTR             ,& ! out,   interception rate for rain (mm/s)
              QDRIPR          => noahmp%water%flux%QDRIPR            ,& ! out,   drip rate for rain (mm/s)
              QTHROR          => noahmp%water%flux%QTHROR            ,& ! out,   throughfall for rain (mm/s)
              QINTS           => noahmp%water%flux%QINTS             ,& ! out,   interception (loading) rate for snowfall (mm/s)
              QDRIPS          => noahmp%water%flux%QDRIPS            ,& ! out,   drip (unloading) rate for intercepted snow (mm/s)
              QTHROS          => noahmp%water%flux%QTHROS            ,& ! out,   throughfall of snowfall (mm/s)
              QRAIN           => noahmp%water%flux%QRAIN             ,& ! out,   rainfall at ground surface (mm/s)
              QSNOW           => noahmp%water%flux%QSNOW             ,& ! out,   snowfall at ground surface (mm/s)
              SNOWHIN         => noahmp%water%flux%SNOWHIN            & ! out,   snow depth increasing rate (m/s)
             )
! ----------------------------------------------------------------------

    ! initialization
    QINTR   = 0.0
    QDRIPR  = 0.0
    QTHROR  = 0.0
    QINTS   = 0.0
    QDRIPS  = 0.0
    QTHROS  = 0.0
    QRAIN   = 0.0
    QSNOW   = 0.0
    SNOWHIN = 0.0
    ICEDRIP = 0.0
    FT      = 0.0
    FV      = 0.0

    ! ----------------------- canopy liquid water ------------------------------
    ! maximum canopy water
    CanopyLiqWaterMax =  CH2OP * (ELAI + ESAI)

    ! average rain interception and throughfall
    if ( (ELAI+ESAI) > 0.0 ) then
       QINTR  = FVEG * RAIN * PrecipAreaFrac  ! interception capability
       QINTR  = min( QINTR, (CanopyLiqWaterMax-CanopyLiqWater)/MainTimeStep * &
                            (1.0-exp(-RAIN*MainTimeStep/CanopyLiqWaterMax)) )
       QINTR  = max( QINTR, 0.0 )
       QDRIPR = FVEG * RAIN - QINTR
       QTHROR = (1.0 - FVEG) * RAIN
       CanopyLiqWater = max( 0.0, CanopyLiqWater + QINTR*MainTimeStep )
    else
       QINTR  = 0.0
       QDRIPR = 0.0
       QTHROR = RAIN
       if ( CanopyLiqWater > 0.0 ) then   ! FOR CASE OF CANOPY GETTING BURIED
          QDRIPR = QDRIPR + CanopyLiqWater / MainTimeStep
          CanopyLiqWater = 0.0
       endif
    endif

    ! ----------------------- canopy ice ------------------------------
    ! maximum canopy ice
    CanopyIceMax = 6.6 * (0.27 + 46.0/SnowfallDensity) * (ELAI + ESAI)

    ! average snow interception and throughfall
    if ( (ELAI+ESAI) > 0.0 ) then
       QINTS = FVEG * SNOW * PrecipAreaFrac
       QINTS = min( QINTS, (CanopyIceMax-CanopyIce)/MainTimeStep * (1.0-exp(-SNOW*MainTimeStep/CanopyIceMax)) )
       QINTS = max( QINTS, 0.0 )
       FT    = max( 0.0, (TV - 270.15) / 1.87e5 )
       FV    = sqrt(WindEastwardRefHeight**2.0 + WindNorthwardRefHeight**2.0) / 1.56e5
       ! MB: changed below to reflect the rain assumption that all precip gets intercepted 
       ICEDRIP = max( 0.0, CanopyIce ) * (FV + FT)
       QDRIPS  = (FVEG * SNOW - QINTS) + ICEDRIP
       QTHROS  = (1.0 - FVEG) * SNOW
       CanopyIce  = max( 0.0, CanopyIce + (QINTS-ICEDRIP)*MainTimeStep )
    else
       QINTS  = 0.0
       QDRIPS = 0.0
       QTHROS = SNOW
       if ( CanopyIce > 0.0 ) then   ! FOR CASE OF CANOPY GETTING BURIED
          QDRIPS = QDRIPS + CanopyIce / MainTimeStep
          CanopyIce = 0.0
       endif
    endif

    ! wetted fraction of canopy
    if ( CanopyIce > 0.0 ) then
       CanopyWetFrac = max( 0.0, CanopyIce ) / max( CanopyIceMax, 1.0e-06 )
    else
       CanopyWetFrac = max( 0.0, CanopyLiqWater ) / max( CanopyLiqWaterMax, 1.0e-06 )
    endif
    CanopyWetFrac    = min( CanopyWetFrac, 1.0 ) ** 0.667

    ! total canopy water
    CanopyTotalWater = CanopyLiqWater + CanopyIce

    ! rain or snow on the ground
    QRAIN   = QDRIPR + QTHROR
    QSNOW   = QDRIPS + QTHROS
    SNOWHIN = QSNOW / SnowfallDensity
    if ( (SurfaceType == 2) .and. (TG > ConstFreezePoint) ) then
       QSNOW   = 0.0
       SNOWHIN = 0.0
    endif

    end associate

  end subroutine CanopyWaterIntercept

end module CanopyWaterInterceptMod
