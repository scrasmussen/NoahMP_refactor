module WaterInit
!!! Initialize column (1-D) Noah-MP water variables
!!! Water variables should be first defined in WaterType.f90

  use InputType
  use NoahmpType

  implicit none

contains

!=== initialize with default values
  subroutine WaterInitDefault(noahmp)

    type(noahmp_type) :: noahmp

    noahmp%water%flux%QRAIN = huge(1.0)

  end subroutine WaterInitDefault

!=== initialize with input data or table values
  subroutine WaterInitTransfer(noahmp, input)

    type(noahmp_type) :: noahmp
    type(input_type)  :: input

    noahmp%water%flux%QRAIN = input%RAIN2D(noahmp%config%domain%iloc,noahmp%config%domain%jloc)

  end subroutine WaterInitTransfer

end module WaterInit
