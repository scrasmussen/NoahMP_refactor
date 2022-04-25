module RunoffSubSurfaceGroundWaterMod

!!! Calculate subsurface runoff based on TOPMODEL with groundwater (Niu et al 2007)

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use GroundWaterTopModelMod, only :  GroundWaterTopModel

  implicit none

contains

  subroutine RunoffSubSurfaceGroundWater(noahmp)

! ------------------------ Code history --------------------------------------------------
! Originally embeded in WATER subroutine instead of as a separate subroutine
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type)     , intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                        &
              QDIS            => noahmp%water%flux%QDIS              ,& ! out,   groundwater discharge [mm/s]
              RUNSUB          => noahmp%water%flux%RUNSUB             & ! out,   subsurface runoff [mm/s] 
             )
! ----------------------------------------------------------------------

    ! compute ground water
    call GroundWaterTopModel(noahmp)

    ! compute subsurface runoff as groundwater discharge
    RUNSUB = QDIS          !mm/s

    end associate

  end subroutine RunoffSubSurfaceGroundWater

end module RunoffSubSurfaceGroundWaterMod
