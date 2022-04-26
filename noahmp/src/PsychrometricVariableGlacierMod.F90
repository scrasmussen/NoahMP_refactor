module PsychrometricVariableGlacierMod

!!! Compute psychrometric variables for glacier ground

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine PsychrometricVariableGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY_GLACIER subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                        &
              SFCPRS          => noahmp%forcing%SFCPRS               ,& ! in,    surface air pressure at reference height (pa)
              LATHEAG         => noahmp%energy%state%LATHEAG         ,& ! out,   latent heat of vaporization/subli (j/kg), ground
              GAMMAG          => noahmp%energy%state%GAMMAG           & ! out,   psychrometric constant (pa/K), ground
             )
! ----------------------------------------------------------------------

    LATHEAG = HSUB
    GAMMAG  = CPAIR * SFCPRS / (0.622 * LATHEAG)

    end associate

  end subroutine PsychrometricVariableGlacier

end module PsychrometricVariableGlacierMod
