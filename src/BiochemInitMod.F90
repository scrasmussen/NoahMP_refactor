module BiochemInitMod

!!! Initialize column (1-D) Noah-MP biochemistry (carbon,nitrogen,etc) variables
!!! Biochemistry variables should be first defined in BiochemType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use InputType
  use NoahmpType

  implicit none

contains

!=== initialize with default values
  subroutine BiochemInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    noahmp%biochem%flux%xxx = huge(1.0)

  end subroutine BiochemInitDefault

!=== initialize with input data or table values
  subroutine BiochemInitTransfer(noahmp, input)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp
    type(input_type) , intent(in)    :: input

    noahmp%biochem%flux%xxx = input%xxx2D(noahmp%config%domain%iloc,noahmp%config%domain%jloc)

  end subroutine BiochemInitTransfer

end module BiochemInitMod
