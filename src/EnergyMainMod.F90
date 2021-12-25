module EnergyMainMod

!!! Main energy module including all energy relevant processes
!!! soil/snow thermal property -> radiation -> ground/vegtation heat flux -> snow/soil temperature solver -> soil/snow phase change

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use GroundThermalPropertyMod, only : GroundThermalProperty
  use RadiationMainMod,         only : RadiationMain

  implicit none

contains

  subroutine EnergyMain(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: ENERGY
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable

! --------------------------------------------------------------------
!    associate(                                                        &
!             )
! ----------------------------------------------------------------------


    ! Thermal properties of soil, snow, lake, and frozen soil
    call GroundThermalProperty(noahmp)

    ! Solar radiation: absorbed & reflected by the ground and canopy
    call RadiationMain(noahmp)


!    end associate

  end subroutine EnergyMain

end module EnergyMainMod
