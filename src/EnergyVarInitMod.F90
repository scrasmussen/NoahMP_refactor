module EnergyVarInitMod

!!! Initialize column (1-D) Noah-MP energy variables
!!! Energy variables should be first defined in EnergyType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use InputVarType
  use NoahmpVarType

  implicit none

contains

!=== initialize with default values
  subroutine EnergyVarInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    associate(                                      &
              NSNOW => noahmp%config%domain%NSNOW  ,&
              NSOIL => noahmp%config%domain%NSOIL   &
             )

    ! energy state variable
    noahmp%energy%state%ELAI            = huge(1.0)
    noahmp%energy%state%SLAI            = huge(1.0)
    noahmp%energy%state%FVEG            = huge(1.0)
    noahmp%energy%state%TG              = huge(1.0)
    noahmp%energy%state%TV              = huge(1.0)
    noahmp%energy%state%FROZEN_CANOPY   = .false.

    allocate( noahmp%energy%state%STC (-NSNOW+1:NSOIL) )

    noahmp%energy%state%STC(:)          = huge(1.0)

    ! energy flux variable
    noahmp%energy%flux%FCEV             = huge(1.0)
    noahmp%energy%flux%FCTR             = huge(1.0)

    end associate

  end subroutine EnergyVarInitDefault

!=== initialize with input data or table values
  subroutine EnergyVarInitTransfer(noahmp, input)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp
    type(input_type) , intent(in)    :: input

    associate(                                      &
              ILOC  => noahmp%config%domain%ILOC   ,&
              JLOC  => noahmp%config%domain%JLOC   ,&
              NSNOW => noahmp%config%domain%NSNOW  ,&
              NSOIL => noahmp%config%domain%NSOIL   &
             )

    ! energy state variable
    noahmp%energy%state%TG  = input%TGIn(ILOC,JLOC)
    noahmp%energy%state%TV  = input%TVIn(ILOC,JLOC)

    noahmp%energy%state%STC(-NSNOW+1:NSOIL) = input%STCIn(ILOC,-NSNOW+1:NSOIL,JLOC)

    ! energy flux variable
    noahmp%energy%flux%FCEV = input%FCEVIn(ILOC,JLOC)
    noahmp%energy%flux%FCTR = input%FCTRIn(ILOC,JLOC)

    end associate

  end subroutine EnergyVarInitTransfer

end module EnergyVarInitMod
