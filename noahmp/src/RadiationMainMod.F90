module RadiationMainMod

!!! Main radiation module to compute surface albedo and radiative fluxes

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use SurfaceAlbedoMod,    only : SurfaceAlbedo
  use SurfaceRadiationMod, only : SurfaceRadiation

  implicit none

contains

  subroutine RadiationMain(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: RADIATION
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! ----------------------------------------------------------------------

    ! surface abeldo
    call SurfaceAlbedo(noahmp)

    ! surface radiation
    call SurfaceRadiation(noahmp)

  end subroutine RadiationMain

end module RadiationMainMod
