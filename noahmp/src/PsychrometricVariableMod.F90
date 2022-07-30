module PsychrometricVariableMod

!!! Compute psychrometric variables for canopy and ground

  use Machine
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
              TemperatureCanopy              => noahmp%energy%state%TemperatureCanopy              ,& ! in,    vegetation temperature (K)
              TemperatureGrd              => noahmp%energy%state%TemperatureGrd              ,& ! in,    ground temperature (K)
              LATHEAV         => noahmp%energy%state%LATHEAV         ,& ! out,   latent heat of vaporization/subli (j/kg), canopy
              LATHEAG         => noahmp%energy%state%LATHEAG         ,& ! out,   latent heat of vaporization/subli (j/kg), ground
              FlagFrozenCanopy   => noahmp%energy%state%FlagFrozenCanopy   ,& ! out,   used to define latent heat pathway
              FlagFrozenGround   => noahmp%energy%state%FlagFrozenGround   ,& ! out,   frozen ground (logical) to define latent heat pathway
              GAMMAV          => noahmp%energy%state%GAMMAV          ,& ! out,   psychrometric constant (pa/K), canopy
              GAMMAG          => noahmp%energy%state%GAMMAG           & ! out,   psychrometric constant (pa/K), ground
             )
! ----------------------------------------------------------------------

    ! for canopy
    if ( TemperatureCanopy > ConstFreezePoint ) then   ! Barlage: add distinction between ground and vegetation in v3.6
       LATHEAV       = ConstLatHeatVapor
       FlagFrozenCanopy = .false.
    else
       LATHEAV       = ConstLatHeatSublim
       FlagFrozenCanopy = .true.
    endif
    GAMMAV = ConstHeatCapacAir * PressureAirRefHeight / (0.622 * LATHEAV)

    ! for ground
    if ( TemperatureGrd > ConstFreezePoint ) then
       LATHEAG       = ConstLatHeatVapor
       FlagFrozenGround = .false.
    else
       LATHEAG       = ConstLatHeatSublim
       FlagFrozenGround = .true.
    endif
    GAMMAG = ConstHeatCapacAir * PressureAirRefHeight / (0.622 * LATHEAG)

    end associate

  end subroutine PsychrometricVariable

end module PsychrometricVariableMod
