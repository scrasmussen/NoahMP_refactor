module SnowCoverGlacierMod

!!! Compute glacier ground snow cover fraction

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowCoverGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in RADIATION_GLACIER subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable

! --------------------------------------------------------------------
    associate(                                                        &
              SNEQV           => noahmp%water%state%SNEQV            ,& ! in,    snow water equivalent [mm]
              FSNO            => noahmp%water%state%FSNO              & ! out,   snow cover fraction (-)
             )
! ----------------------------------------------------------------------

    FSNO = 0.0
    if ( SNEQV > 0.0 ) FSNO = 1.0

    end associate

  end subroutine SnowCoverGlacier

end module SnowCoverGlacierMod
