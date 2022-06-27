module PrecipitationHeatAdvectGlacierMod

!!! Estimate heat flux advected from precipitation to glacier ground

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine PrecipitationHeatAdvectGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: none (adapted from PRECIP_HEAT)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: PAH_AG    ! precipitation advected heat - air to ground (W/m2)

! --------------------------------------------------------------------
    associate(                                                        &
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight,& ! in,    air temperature [K] at reference height
              TG              => noahmp%energy%state%TG              ,& ! in,    ground temperature (k)
              RAIN            => noahmp%water%flux%RAIN              ,& ! in,    total liquid rainfall (mm/s) before interception
              SNOW            => noahmp%water%flux%SNOW              ,& ! in,    total liquid snowfall (mm/s) before interception
              QSNOW           => noahmp%water%flux%QSNOW             ,& ! out,   snowfall at ground surface (mm/s)
              QRAIN           => noahmp%water%flux%QRAIN             ,& ! out,   rainfall at ground surface (mm/s)
              PAHB            => noahmp%energy%flux%PAHB              & ! out,   precipitation advected heat - bare ground net (W/m2)
             )
! ----------------------------------------------------------------------

    ! initialization
    PAH_AG  = 0.0
    PAHB    = 0.0
    QRAIN   = RAIN
    QSNOW   = SNOW

    ! Heat advection for liquid rainfall
    PAH_AG = QRAIN * (CWAT/1000.0) * (TemperatureAirRefHeight - TG)

    ! Heat advection for snowfall
    PAH_AG = PAH_AG + QSNOW * (CICE/1000.0) * (TemperatureAirRefHeight - TG)

    ! net heat advection
    PAHB = PAH_AG

    ! Put some artificial limits here for stability
    PAHB = max( PAHB, -20.0 )
    PAHB = min( PAHB,  20.0 )

    end associate

  end subroutine PrecipitationHeatAdvectGlacier

end module PrecipitationHeatAdvectGlacierMod
