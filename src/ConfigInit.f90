module ConfigInit
!!! Initialize column (1-D) Noah-MP configuration variables
!!! Configuration variables should be first defined in ConfigType.f90

  use InputType
  use NoahmpType

  implicit none

contains

!=== initialize with default values
  subroutine ConfigInitDefault(noahmp)

    type(noahmp_type) :: noahmp

    noahmp%config%nmlist%runoff_option = 1
    noahmp%config%domain%dt            = 1800.0

  end subroutine ConfigInitDefault

!=== initialize with input data or table values
  subroutine ConfigInitTransfer(noahmp, input)

    type(noahmp_type) :: noahmp
    type(input_type)  :: input

    noahmp%config%nmlist%runoff_option = input%OPT_RUN
    noahmp%config%domain%dt            = input%timestep

  end subroutine ConfigInitTransfer

end module ConfigInit
