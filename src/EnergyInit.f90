module EnergyInit
!!! Initialize column (1-D) Noah-MP energy variables
!!! Energy variables should be first defined in EnergyType.f90

  use InputType
  use NoahmpType

  implicit none

contains

!=== initialize with default values
  subroutine EnergyInitDefault(noahmp)

    type(noahmp_type) :: noahmp

    noahmp%energy%flux%SAV = huge(1.0)

  end subroutine EnergyInitDefault

!=== initialize with input data or table values
  subroutine EnergyInitTransfer(noahmp, input)

    type(noahmp_type) :: noahmp
    type(input_type)  :: input

    noahmp%energy%flux%SAV = input%SAV2D(noahmp%config%domain%iloc,noahmp%config%domain%jloc)

  end subroutine EnergyInitTransfer

end module EnergyInit
