module WaterInitMod
!!! Initialize column (1-D) Noah-MP water variables
!!! Water variables should be first defined in WaterType.f90

  use InputType
  use NoahmpType

  implicit none

contains

!=== initialize with default values
  subroutine WaterInitDefault(noahmp)

    type(noahmp_type) :: noahmp

    noahmp%water%flux%RAIN    = huge(1.0)
    noahmp%water%flux%SNOW    = huge(1.0)
    noahmp%water%flux%PRCP    = huge(1.0)
    noahmp%water%flux%QPRECC  = huge(1.0)
    noahmp%water%flux%QPRECL  = huge(1.0)

    noahmp%water%state%BDFALL = huge(1.0)
    noahmp%water%state%FP     = huge(1.0)
    noahmp%water%state%FPICE  = huge(1.0)

  end subroutine WaterInitDefault

!=== initialize with input data or table values
  subroutine WaterInitTransfer(noahmp, input)


    type(noahmp_type), intent(inout) :: noahmp
    type(input_type), intent(in)     :: input

    associate(                                   &
              iloc => noahmp%config%domain%iloc ,&
              jloc => noahmp%config%domain%jloc  &
             )

    !noahmp%water%flux%QRAIN = input%RAIN2D(iloc,jloc)
    end associate
  end subroutine WaterInitTransfer

end module WaterInitMod
