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
              SnowDepth           => noahmp%water%state%SnowDepth            ,& ! inout,  snow depth [m]
              SnowWaterEquiv           => noahmp%water%state%SnowWaterEquiv            ,& ! inout,  snow water equivalent [mm]
              SnowIce           => noahmp%water%state%SnowIce            ,& ! inout,  snow layer ice [mm]
              SnowLiqWater           => noahmp%water%state%SnowLiqWater            ,& ! inout,  snow layer liquid water [mm]
              STC             => noahmp%energy%state%STC             ,& ! inout,  snow and soil layer temperature [k]
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer          & ! inout,  thickness of snow/soil layers (m)
             ) 
! ----------------------------------------------------------------------

    NEWNODE  = 0

! shallow snow / no layer
    if ( (NumSnowLayerNeg == 0) .and. (QSNOW > 0.0) ) then
       SnowDepth = SnowDepth + SNOWHIN * MainTimeStep
       SnowWaterEquiv = SnowWaterEquiv + QSNOW * MainTimeStep
    endif

! creating a new layer
    if ( (NumSnowLayerNeg == 0)  .and. (QSNOW > 0.0) .and. (SnowDepth >= 0.025) ) then !MB: change limit
!   if ( (NumSnowLayerNeg == 0)  .and. (QSNOW > 0.0) .and. (SnowDepth >= 0.05) ) then
       NumSnowLayerNeg     = -1
       NEWNODE   =  1
       ThicknessSnowSoilLayer(0) = SnowDepth
       SnowDepth     = 0.0
       STC(0)    = min(273.16, TemperatureAirRefHeight)   ! temporary setup
       SnowIce(0)  = SnowWaterEquiv
       SnowLiqWater(0)  = 0.0
    endif

! snow with layers
    if ( (NumSnowLayerNeg < 0) .and. (NEWNODE == 0) .and. (QSNOW > 0.0) ) then
       SnowIce(NumSnowLayerNeg+1)  = SnowIce(NumSnowLayerNeg+1)   + QSNOW   * MainTimeStep
       ThicknessSnowSoilLayer(NumSnowLayerNeg+1) = ThicknessSnowSoilLayer(NumSnowLayerNeg+1)  + SNOWHIN * MainTimeStep
    endif

    end associate

  end subroutine SnowfallAfterCanopyIntercept

end module SnowfallBelowCanopyMod
