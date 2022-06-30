module SnowfallBelowCanopyMod

!!! Snowfall process after canopy interception
!!! Update snow water equivalent and snow depth

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowfallAfterCanopyIntercept(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SNOWFALL
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer  :: NEWNODE  ! 0-no new layers, 1-creating new layers

! --------------------------------------------------------------------
    associate(                                                        &
              MainTimeStep            => noahmp%config%domain%MainTimeStep      ,& ! in,    noahmp main time step (s)
              QSNOW           => noahmp%water%flux%QSNOW             ,& ! in,     snow at ground srf (mm/s) [+]
              SNOWHIN         => noahmp%water%flux%SNOWHIN           ,& ! in,     snow depth increasing rate (m/s)
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight ,& ! in,    air temperature [K] at reference height
              NumSnowLayerNeg         => noahmp%config%domain%NumSnowLayerNeg   ,& ! inout,  actual number of snow layers (negative)
              SNOWH           => noahmp%water%state%SNOWH            ,& ! inout,  snow depth [m]
              SNEQV           => noahmp%water%state%SNEQV            ,& ! inout,  snow water equivalent [mm]
              SNICE           => noahmp%water%state%SNICE            ,& ! inout,  snow layer ice [mm]
              SNLIQ           => noahmp%water%state%SNLIQ            ,& ! inout,  snow layer liquid water [mm]
              STC             => noahmp%energy%state%STC             ,& ! inout,  snow and soil layer temperature [k]
              DZSNSO          => noahmp%config%domain%DZSNSO          & ! inout,  thickness of snow/soil layers (m)
             ) 
! ----------------------------------------------------------------------

    NEWNODE  = 0

! shallow snow / no layer
    if ( (NumSnowLayerNeg == 0) .and. (QSNOW > 0.0) ) then
       SNOWH = SNOWH + SNOWHIN * MainTimeStep
       SNEQV = SNEQV + QSNOW * MainTimeStep
    endif

! creating a new layer
    if ( (NumSnowLayerNeg == 0)  .and. (QSNOW > 0.0) .and. (SNOWH >= 0.025) ) then !MB: change limit
!   if ( (NumSnowLayerNeg == 0)  .and. (QSNOW > 0.0) .and. (SNOWH >= 0.05) ) then
       NumSnowLayerNeg     = -1
       NEWNODE   =  1
       DZSNSO(0) = SNOWH
       SNOWH     = 0.0
       STC(0)    = min(273.16, TemperatureAirRefHeight)   ! temporary setup
       SNICE(0)  = SNEQV
       SNLIQ(0)  = 0.0
    endif

! snow with layers
    if ( (NumSnowLayerNeg < 0) .and. (NEWNODE == 0) .and. (QSNOW > 0.0) ) then
       SNICE(NumSnowLayerNeg+1)  = SNICE(NumSnowLayerNeg+1)   + QSNOW   * MainTimeStep
       DZSNSO(NumSnowLayerNeg+1) = DZSNSO(NumSnowLayerNeg+1)  + SNOWHIN * MainTimeStep
    endif

    end associate

  end subroutine SnowfallAfterCanopyIntercept

end module SnowfallBelowCanopyMod
