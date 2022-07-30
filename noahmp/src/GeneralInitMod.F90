module GeneralInitMod

!!! General initialization for variables

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
 
  implicit none

contains

  subroutine GeneralInit(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in NOAHMP_SFLX)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IZ        ! loop index

! --------------------------------------------------------------------
    associate(                                                        &
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer   ,& ! in,   number of soil layers
              DepthSoilLayer           => noahmp%config%domain%DepthSoilLayer          ,& ! in,   depth [m] of layer-bottom from soil surface
              NumSoilLayerRoot           => noahmp%water%param%NumSoilLayerRoot            ,& ! in,   number of soil layers with root present
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg,& ! in,   actual number of snow layers (negative)
              DepthSnowSoilLayer           => noahmp%config%domain%DepthSnowSoilLayer          ,& ! in,   depth of snow/soil layer-bottom (m)
              TemperatureSoilSnow             => noahmp%energy%state%TemperatureSoilSnow             ,& ! in,   snow and soil layer temperature [k]
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! out,  thickness of snow/soil layers (m)
              TemperatureRootZone           => noahmp%energy%state%TemperatureRootZone            & ! out,  root-zone averaged temperature (k)
             )
! ----------------------------------------------------------------------

    ! initialize snow/soil layer thickness (m)
    do IZ = NumSnowLayerNeg+1, NumSoilLayer
       if ( IZ == NumSnowLayerNeg+1 ) then
          ThicknessSnowSoilLayer(IZ) = - DepthSnowSoilLayer(IZ)
       else
          ThicknessSnowSoilLayer(IZ) = DepthSnowSoilLayer(IZ-1) - DepthSnowSoilLayer(IZ)
       endif
    enddo

    ! initialize root-zone soil temperature
    TemperatureRootZone = 0.0
    do IZ = 1, NumSoilLayerRoot
       TemperatureRootZone = TemperatureRootZone + TemperatureSoilSnow(IZ) * ThicknessSnowSoilLayer(IZ) / (-DepthSoilLayer(NumSoilLayerRoot))
    enddo

    end associate

  end subroutine GeneralInit

end module GeneralInitMod
