module ConfigInitMod
!!! Initialize column (1-D) Noah-MP configuration variables
!!! Configuration variables should be first defined in ConfigType.f90

  use InputType
  use NoahmpType

  implicit none

contains

!=== initialize with default values
  subroutine ConfigInitDefault(noahmp)

    type(noahmp_type), intent(inout) :: noahmp

    noahmp%config%nmlist%OPT_SNF     = 1
    noahmp%config%nmlist%OPT_RUN     = 1

    noahmp%config%domain%dt          = 1800.0
    noahmp%config%domain%iloc        = 0
    noahmp%config%domain%jloc        = 0
    noahmp%config%domain%COSZ        = 0.0

  end subroutine ConfigInitDefault

!=== initialize with input data or table values
  subroutine ConfigInitTransfer(noahmp, input)

    type(noahmp_type), intent(inout) :: noahmp
    type(input_type),  intent(in)    :: input

    associate(                                   &
              iloc => noahmp%config%domain%iloc ,&
              jloc => noahmp%config%domain%jloc  &
             )
    
    noahmp%config%nmlist%OPT_RUN = input%IOPT_RUN
    noahmp%config%nmlist%OPT_SNF = input%IOPT_SNF
    noahmp%config%domain%dt      = input%timestep

    noahmp%config%domain%COSZ    = input%COSZIN(iloc,jloc)

    end associate

  end subroutine ConfigInitTransfer

end module ConfigInitMod
