module WaterInitMod

!!! Initialize column (1-D) Noah-MP water variables
!!! Water variables should be first defined in WaterType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use InputType
  use NoahmpType

  implicit none

contains

!=== initialize with default values
  subroutine WaterInitDefault(noahmp)

    type(noahmp_type) :: noahmp

    ! water state variable
    noahmp%water%state%BDFALL      = huge(1.0)
    noahmp%water%state%CANLIQ      = huge(1.0)
    noahmp%water%state%CANICE      = huge(1.0)
    noahmp%water%state%CMC         = huge(1.0)
    noahmp%water%state%FWET        = huge(1.0)
    noahmp%water%state%MAXSNO      = huge(1.0)
    noahmp%water%state%MAXLIQ      = huge(1.0)

    ! water flux variable
    noahmp%water%flux%ECAN         = huge(1.0)
    noahmp%water%flux%ETRAN        = huge(1.0)
    noahmp%water%flux%QEVAC        = huge(1.0)
    noahmp%water%flux%QDEWC        = huge(1.0)
    noahmp%water%flux%QFROC        = huge(1.0)
    noahmp%water%flux%QSUBC        = huge(1.0)
    noahmp%water%flux%QMELTC       = huge(1.0)
    noahmp%water%flux%QFRZC        = huge(1.0)

    ! water parameter variable
    noahmp%water%param%CH2OP       = huge(1.0)


  end subroutine WaterInitDefault

!=== initialize with input data or table values
  subroutine WaterInitTransfer(noahmp, input)

    type(noahmp_type) :: noahmp
    type(input_type)  :: input

    ! water parameter variable
    noahmp%water%param%CH2OP  = input%CH2OPIn


  end subroutine WaterInitTransfer

end module WaterInitMod
