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
              EmissivitySnow       => noahmp%energy%param%EmissivitySnow       ,& ! in,    snow emissivity
              EmissivityIceSfc            => noahmp%energy%param%EmissivityIceSfc            ,& ! in,    emissivity ice surface
              SnowCoverFrac            => noahmp%water%state%SnowCoverFrac             ,& ! in,    snow cover fraction [-]
              EMG             => noahmp%energy%state%EMG             ,& ! out,   ground emissivity
              EMISSI          => noahmp%energy%state%EMISSI           & ! out,   surface emissivity
             )
! ----------------------------------------------------------------------

    ! ground emissivity
    EMG = EmissivityIceSfc * (1.0 - SnowCoverFrac) + EmissivitySnow * SnowCoverFrac

    ! surface emissivity
    EMISSI = EMG

    end associate

  end subroutine SurfaceEmissivityGlacier

end module SurfaceEmissivityGlacierMod
