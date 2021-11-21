module RunoffSubSurfaceDrainageMod

!!! Calculate subsurface runoff using derived soil water drainage rate

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine RunoffSubSurfaceDrainage(noahmp)

! ------------------------ Code history --------------------------------------------------
! Originally embeded in WATER subroutine instead of as a separate subroutine
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type)     , intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                        &
              QDRAIN          => noahmp%water%flux%QDRAIN            ,& ! in,    soil bottom drainage (m/s)
              RUNSUB          => noahmp%water%flux%RUNSUB             & ! inout, subsurface runoff [mm/s] 
             )
! ----------------------------------------------------------------------

    ! compuate subsurface runoff mm/s
    RUNSUB = RUNSUB + QDRAIN

    end associate

  end subroutine RunoffSubSurfaceDrainage

end module RunoffSubSurfaceDrainageMod
