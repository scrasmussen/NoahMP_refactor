module EnergyInitMod
!!! Initialize column (1-D) Noah-MP energy variables
!!! Energy variables should be first defined in EnergyType.f90

  use InputType
  use NoahmpType

  implicit none

contains

!=== initialize with default values
  subroutine EnergyInitDefault(noahmp)

    type(noahmp_type), intent(inout) :: noahmp

    noahmp%energy%flux%SAV      = huge(1.0)
    noahmp%energy%flux%SOLAD    = huge(1.0)
    noahmp%energy%flux%SOLAI    = huge(1.0)
    noahmp%energy%flux%SWDOWN   = huge(1.0)

    noahmp%energy%state%TAH     = huge(1.0)
    noahmp%energy%state%THAIR   = huge(1.0)
    noahmp%energy%state%QAIR    = huge(1.0)
    noahmp%energy%state%EAIR    = huge(1.0)
    noahmp%energy%state%RHOAIR  = huge(1.0)

  end subroutine EnergyInitDefault

!=== initialize with input data or table values
  subroutine EnergyInitTransfer(noahmp, input)

    type(noahmp_type), intent(inout) :: noahmp
    type(input_type),  intent(in)    :: input

    associate(                                  &
             iloc => noahmp%config%domain%iloc ,&
             jloc => noahmp%config%domain%jloc  &
             )

    !noahmp%energy%flux%SAV      = input%SAV2D (iloc,jloc)
    end associate
  end subroutine EnergyInitTransfer

end module EnergyInitMod
