module PsychrometricVariableMod

!!! Compute psychrometric variables for canopy and ground

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine PsychrometricVariable(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                        &
              PressureAirRefHeight => noahmp%forcing%PressureAirRefHeight ,& ! in,  air pressure [Pa] at reference height
              TV              => noahmp%energy%state%TV              ,& ! in,    vegetation temperature (K)
              TG              => noahmp%energy%state%TG              ,& ! in,    ground temperature (K)
              LATHEAV         => noahmp%energy%state%LATHEAV         ,& ! out,   latent heat of vaporization/subli (j/kg), canopy
              LATHEAG         => noahmp%energy%state%LATHEAG         ,& ! out,   latent heat of vaporization/subli (j/kg), ground
              FROZEN_CANOPY   => noahmp%energy%state%FROZEN_CANOPY   ,& ! out,   used to define latent heat pathway
              FROZEN_GROUND   => noahmp%energy%state%FROZEN_GROUND   ,& ! out,   frozen ground (logical) to define latent heat pathway
              GAMMAV          => noahmp%energy%state%GAMMAV          ,& ! out,   psychrometric constant (pa/K), canopy
              GAMMAG          => noahmp%energy%state%GAMMAG           & ! out,   psychrometric constant (pa/K), ground
             )
! ----------------------------------------------------------------------

    ! for canopy
    if ( TV > TFRZ ) then   ! Barlage: add distinction between ground and vegetation in v3.6
       LATHEAV       = HVAP
       FROZEN_CANOPY = .false.
    else
       LATHEAV       = HSUB
       FROZEN_CANOPY = .true.
    endif
    GAMMAV = CPAIR * PressureAirRefHeight / (0.622 * LATHEAV)

    ! for ground
    if ( TG > TFRZ ) then
       LATHEAG       = HVAP
       FROZEN_GROUND = .false.
    else
       LATHEAG       = HSUB
       FROZEN_GROUND = .true.
    endif
    GAMMAG = CPAIR * PressureAirRefHeight / (0.622 * LATHEAG)

    end associate

  end subroutine PsychrometricVariable

end module PsychrometricVariableMod
