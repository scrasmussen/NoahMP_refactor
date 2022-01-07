module WaterVarInitMod

!!! Initialize column (1-D) Noah-MP water variables
!!! Water variables should be first defined in WaterType.f90

  use InputVarType
  use NoahmpVarType

  implicit none

contains

!=== initialize with default values
  subroutine WaterVarInitDefault(noahmp)

    type(noahmp_type) :: noahmp

    noahmp%water%state%SNOWH = huge(1.0)

  end subroutine WaterVarInitDefault

!=== initialize with input data or table values
  subroutine WaterVarInitTransfer(noahmp, input)

    type(noahmp_type) :: noahmp
    type(input_type)  :: input

    noahmp%water%state%SNOWH = input%SNOWH

  end subroutine WaterVarInitTransfer

end module WaterVarInitMod
