module PrecipitationHeatAdvectMod

!!! Estimate heat flux advected from precipitation to vegetation and ground

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine PrecipitationHeatAdvect(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: PRECIP_HEAT
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! The water and heat portions of PRECIP_HEAT are separated in refactored code
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: PAH_AC    ! precipitation advected heat - air to canopy (W/m2)
    real(kind=kind_noahmp)           :: PAH_CG    ! precipitation advected heat - canopy to ground (W/m2)
    real(kind=kind_noahmp)           :: PAH_AG    ! precipitation advected heat - air to ground (W/m2)

! --------------------------------------------------------------------
    associate(                                                        &
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight,& ! in,   air temperature [K] at reference height
              TV              => noahmp%energy%state%TV              ,& ! in,    vegetation temperature (k)
              TG              => noahmp%energy%state%TG              ,& ! in,    ground temperature (k)
              FVEG            => noahmp%energy%state%FVEG            ,& ! in,    greeness vegetation fraction (-)
              RainfallRefHeight            => noahmp%water%flux%RainfallRefHeight              ,& ! in,    total liquid rainfall [mm/s] before interception
              SnowfallRefHeight            => noahmp%water%flux%SnowfallRefHeight              ,& ! in,    total snowfall [mm/s] before interception
              DripCanopyRain          => noahmp%water%flux%DripCanopyRain            ,& ! in,    drip rate for intercepted rain [mm/s]
              ThroughfallRain          => noahmp%water%flux%ThroughfallRain            ,& ! in,    throughfall for rain [mm/s]
              DripCanopySnow          => noahmp%water%flux%DripCanopySnow            ,& ! in,    drip (unloading) rate for intercepted snow [mm/s]
              ThroughfallSnow          => noahmp%water%flux%ThroughfallSnow            ,& ! in,    throughfall of snowfall [mm/s]
              PAHV            => noahmp%energy%flux%PAHV             ,& ! out,   precipitation advected heat - vegetation net (W/m2)
              PAHG            => noahmp%energy%flux%PAHG             ,& ! out,   precipitation advected heat - under canopy net (W/m2)
              PAHB            => noahmp%energy%flux%PAHB              & ! out,   precipitation advected heat - bare ground net (W/m2)
             )
! ----------------------------------------------------------------------

    ! initialization
    PAH_AC  = 0.0
    PAH_CG  = 0.0
    PAH_AG  = 0.0
    PAHV    = 0.0
    PAHG    = 0.0
    PAHB    = 0.0

    ! Heat advection for liquid rainfall
    PAH_AC = FVEG * RainfallRefHeight * (ConstHeatCapacWater/1000.0) * (TemperatureAirRefHeight - TV)
    PAH_CG = DripCanopyRain * (ConstHeatCapacWater/1000.0) * (TV - TG)
    PAH_AG = ThroughfallRain * (ConstHeatCapacWater/1000.0) * (TemperatureAirRefHeight - TG)

    ! Heat advection for snowfall
    PAH_AC = PAH_AC + FVEG * SnowfallRefHeight * (ConstHeatCapacIce/1000.0) * (TemperatureAirRefHeight - TV)
    PAH_CG = PAH_CG + DripCanopySnow * (ConstHeatCapacIce/1000.0) * (TV - TG)
    PAH_AG = PAH_AG + ThroughfallSnow * (ConstHeatCapacIce/1000.0) * (TemperatureAirRefHeight - TG)

    ! net heat advection
    PAHV = PAH_AC - PAH_CG
    PAHG = PAH_CG
    PAHB = PAH_AG

    ! adjust for FVEG
    if ( (FVEG > 0.0) .and. (FVEG < 1.0) ) then
       PAHG = PAHG / FVEG         ! these will be multiplied by fraction later
       PAHB = PAHB / (1.0-FVEG)
    elseif ( FVEG <= 0.0 ) then
       PAHB = PAHG + PAHB         ! for case of canopy getting buried
       PAHG = 0.0
       PAHV = 0.0
    elseif ( FVEG >= 1.0 ) then
       PAHB = 0.0
    endif

    ! Put some artificial limits here for stability
    PAHV = max( PAHV, -20.0 )
    PAHV = min( PAHV,  20.0 )
    PAHG = max( PAHG, -20.0 )
    PAHG = min( PAHG,  20.0 )
    PAHB = max( PAHB, -20.0 )
    PAHB = min( PAHB,  20.0 )

    end associate

  end subroutine PrecipitationHeatAdvect

end module PrecipitationHeatAdvectMod
