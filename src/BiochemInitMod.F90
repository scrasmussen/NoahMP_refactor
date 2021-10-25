module BiochemInitMod

!!! Initialize column (1-D) Noah-MP biochemistry (carbon,nitrogen,etc) variables
!!! Biochemistry variables should be first defined in BiochemType.f90

  use InputType
  use NoahmpType

  implicit none

contains

!=== initialize with default values
  subroutine BiochemInitDefault(noahmp)

    type(noahmp_type) :: noahmp

    noahmp%biochem%flux%xxx = huge(1.0)

  end subroutine BiochemInitDefault

!=== initialize with input data or table values
  subroutine BiochemInitTransfer(noahmp, input)

    type(noahmp_type) :: noahmp
    type(input_type)  :: input

    noahmp%biochem%flux%xxx = input%xxx2D(noahmp%config%domain%iloc,noahmp%config%domain%jloc)

  end subroutine BiochemInitTransfer

end module BiochemInitMod
