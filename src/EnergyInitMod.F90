module EnergyInitMod

!!! Initialize column (1-D) Noah-MP energy variables
!!! Energy variables should be first defined in EnergyType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use InputType
  use NoahmpType

  implicit none

contains

!=== initialize with default values
  subroutine EnergyInitDefault(noahmp)

    type(noahmp_type) :: noahmp

    ! energy state variable
    noahmp%energy%state%ELAI            = huge(1.0)
    noahmp%energy%state%SLAI            = huge(1.0)
    noahmp%energy%state%FVEG            = huge(1.0)
    noahmp%energy%state%TG              = huge(1.0)
    noahmp%energy%state%TV              = huge(1.0)
    noahmp%energy%state%FROZEN_CANOPY   = .false.

    ! energy flux variable
    noahmp%energy%flux%FCEV             = huge(1.0)
    noahmp%energy%flux%FCTR             = huge(1.0)


  end subroutine EnergyInitDefault

!=== initialize with input data or table values
  subroutine EnergyInitTransfer(noahmp, input)

    type(noahmp_type) :: noahmp
    type(input_type)  :: input

    associate (                                   &
               ILOC => noahmp%config%domain%ILOC ,&
               JLOC => noahmp%config%domain%JLOC  &
              )

    ! energy state variable

    ! energy flux variable
    noahmp%energy%flux%FCEV = input%FCEVIn(ILOC,JLOC)
    noahmp%energy%flux%FCTR = input%FCTRIn(ILOC,JLOC)

  end subroutine EnergyInitTransfer

end module EnergyInitMod
