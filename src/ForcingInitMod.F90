module ForcingInitMod

!!! Initialize column (1-D) Noah-MP forcing variables
!!! Forcing variables should be first defined in ForcingType.f90

  use InputType
  use NoahmpType

  implicit none

contains

!=== initialize with default values
  subroutine ForcingInitDefault(noahmp)

    type(noahmp_type) :: noahmp

    noahmp%forcing%UU = huge(1.0)

  end subroutine ForcingInitDefault

!=== initialize with input data or table values
  subroutine ForcingInitTransfer(noahmp, input)

    type(noahmp_type) :: noahmp
    type(input_type)  :: input

    noahmp%forcing%UU = input%U2D(noahmp%config%domain%iloc,noahmp%config%domain%jloc)

  end subroutine ForcingInitTransfer

end module ForcingInitMod
