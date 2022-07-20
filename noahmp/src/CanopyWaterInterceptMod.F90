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
              CanopyLiqHoldCap           => noahmp%water%param%CanopyLiqHoldCap            ,& ! in,    maximum intercepted liquid water per unit lai+sai [mm]
              RainfallRefHeight            => noahmp%water%flux%RainfallRefHeight              ,& ! in,    total liquid rainfall [mm/s] before interception
              SnowfallRefHeight            => noahmp%water%flux%SnowfallRefHeight              ,& ! in,    total snowfall [mm/s] before interception
              SnowfallDensity          => noahmp%water%state%SnowfallDensity           ,& ! in,    bulk density of snowfall (kg/m3)
              PrecipAreaFrac              => noahmp%water%state%PrecipAreaFrac               ,& ! in,    fraction of the gridcell that receives precipitation
              CanopyLiqWater          => noahmp%water%state%CanopyLiqWater           ,& ! inout, intercepted canopy liquid water [mm]
              CanopyIce          => noahmp%water%state%CanopyIce           ,& ! inout, intercepted canopy ice [mm]
              CanopyWetFrac            => noahmp%water%state%CanopyWetFrac             ,& ! out,   wetted or snowed fraction of the canopy
              CanopyTotalWater             => noahmp%water%state%CanopyTotalWater              ,& ! out,   total canopy intercepted water [mm]
              CanopyIceMax          => noahmp%water%state%CanopyIceMax           ,& ! out,   canopy capacity for snow interception [mm]
              CanopyLiqWaterMax          => noahmp%water%state%CanopyLiqWaterMax           ,& ! out,   canopy capacity for rain interception [mm]
              InterceptCanopyRain           => noahmp%water%flux%InterceptCanopyRain             ,& ! out,   interception rate for rain [mm/s]
              DripCanopyRain          => noahmp%water%flux%DripCanopyRain            ,& ! out,   drip rate for intercepted rain [mm/s]
              ThroughfallRain          => noahmp%water%flux%ThroughfallRain            ,& ! out,   throughfall for rain [mm/s]
              InterceptCanopySnow           => noahmp%water%flux%InterceptCanopySnow             ,& ! out,   interception (loading) rate for snowfall [mm/s]
              DripCanopySnow          => noahmp%water%flux%DripCanopySnow            ,& ! out,   drip (unloading) rate for intercepted snow [mm/s]
              ThroughfallSnow          => noahmp%water%flux%ThroughfallSnow            ,& ! out,   throughfall of snowfall [mm/s]
              RainfallGround           => noahmp%water%flux%RainfallGround             ,& ! out,   rainfall at ground surface [mm/s]
              SnowfallGround           => noahmp%water%flux%SnowfallGround             ,& ! out,   snowfall at ground surface [mm/s]
              SnowDepthIncr         => noahmp%water%flux%SnowDepthIncr            & ! out,   snow depth increasing rate (m/s) due to snowfall
             )
! ----------------------------------------------------------------------

    ! initialization
    InterceptCanopyRain   = 0.0
    DripCanopyRain  = 0.0
    ThroughfallRain  = 0.0
    InterceptCanopySnow   = 0.0
    DripCanopySnow  = 0.0
    ThroughfallSnow  = 0.0
    RainfallGround   = 0.0
    SnowfallGround   = 0.0
    SnowDepthIncr = 0.0
    ICEDRIP = 0.0
    FT      = 0.0
    FV      = 0.0

    ! ----------------------- canopy liquid water ------------------------------
    ! maximum canopy water
    CanopyLiqWaterMax =  CanopyLiqHoldCap * (ELAI + ESAI)

    ! average rain interception and throughfall
    if ( (ELAI+ESAI) > 0.0 ) then
       InterceptCanopyRain  = FVEG * RainfallRefHeight * PrecipAreaFrac  ! max interception capability
       InterceptCanopyRain  = min( InterceptCanopyRain, (CanopyLiqWaterMax-CanopyLiqWater)/MainTimeStep * &
                                   (1.0-exp(-RainfallRefHeight*MainTimeStep/CanopyLiqWaterMax)) )
       InterceptCanopyRain  = max( InterceptCanopyRain, 0.0 )
       DripCanopyRain = FVEG * RainfallRefHeight - InterceptCanopyRain
       ThroughfallRain = (1.0 - FVEG) * RainfallRefHeight
       CanopyLiqWater = max( 0.0, CanopyLiqWater + InterceptCanopyRain*MainTimeStep )
    else
       InterceptCanopyRain  = 0.0
       DripCanopyRain = 0.0
       ThroughfallRain = RainfallRefHeight
       if ( CanopyLiqWater > 0.0 ) then   ! FOR CASE OF CANOPY GETTING BURIED
          DripCanopyRain = DripCanopyRain + CanopyLiqWater / MainTimeStep
          CanopyLiqWater = 0.0
       endif
    endif

    ! ----------------------- canopy ice ------------------------------
    ! maximum canopy ice
    CanopyIceMax = 6.6 * (0.27 + 46.0/SnowfallDensity) * (ELAI + ESAI)

    ! average snow interception and throughfall
    if ( (ELAI+ESAI) > 0.0 ) then
       InterceptCanopySnow = FVEG * SnowfallRefHeight * PrecipAreaFrac
       InterceptCanopySnow = min( InterceptCanopySnow, (CanopyIceMax-CanopyIce)/MainTimeStep * &
                                  (1.0-exp(-SnowfallRefHeight*MainTimeStep/CanopyIceMax)) )
       InterceptCanopySnow = max( InterceptCanopySnow, 0.0 )
       FT    = max( 0.0, (TV - 270.15) / 1.87e5 )
       FV    = sqrt(WindEastwardRefHeight**2.0 + WindNorthwardRefHeight**2.0) / 1.56e5
       ! MB: changed below to reflect the rain assumption that all precip gets intercepted 
       ICEDRIP = max( 0.0, CanopyIce ) * (FV + FT)
       DripCanopySnow  = (FVEG * SnowfallRefHeight - InterceptCanopySnow) + ICEDRIP
       ThroughfallSnow  = (1.0 - FVEG) * SnowfallRefHeight
       CanopyIce  = max( 0.0, CanopyIce + (InterceptCanopySnow-ICEDRIP)*MainTimeStep )
    else
       InterceptCanopySnow  = 0.0
       DripCanopySnow = 0.0
       ThroughfallSnow = SnowfallRefHeight
       if ( CanopyIce > 0.0 ) then   ! FOR CASE OF CANOPY GETTING BURIED
          DripCanopySnow = DripCanopySnow + CanopyIce / MainTimeStep
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
    RainfallGround   = DripCanopyRain + ThroughfallRain
    SnowfallGround   = DripCanopySnow + ThroughfallSnow
    SnowDepthIncr = SnowfallGround / SnowfallDensity
    if ( (SurfaceType == 2) .and. (TG > ConstFreezePoint) ) then
       SnowfallGround   = 0.0
       SnowDepthIncr = 0.0
    endif

    end associate

  end subroutine CanopyWaterIntercept

end module CanopyWaterInterceptMod
