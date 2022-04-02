module SurfaceEmissivityGlacierMod

!!! Compute glacier surface longwave emissivity

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SurfaceEmissivityGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY_GLACIER subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                        &
              SNOW_EMIS       => noahmp%energy%param%SNOW_EMIS       ,& ! in,    snow emissivity
              EICE            => noahmp%energy%param%EICE            ,& ! in,    emissivity ice surface
              FSNO            => noahmp%water%state%FSNO             ,& ! in,    snow cover fraction (-)
              EMG             => noahmp%energy%state%EMG             ,& ! out,   ground emissivity
              EMISSI          => noahmp%energy%state%EMISSI           & ! out,   surface emissivity
             )
! ----------------------------------------------------------------------

    ! ground emissivity
!    EMG = EICE * (1.0 - FSNO) + SNOW_EMIS * FSNO
    EMG = 0.98
    ! surface emissivity
    EMISSI = EMG

    end associate

  end subroutine SurfaceEmissivityGlacier

end module SurfaceEmissivityGlacierMod
