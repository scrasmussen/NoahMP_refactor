module NoahmpType

!!! Define column (1-D) Noah-MP model variable data types

  use ForcingType
  use ConfigType
  use EnergyType
  use WaterType
  !use BiochemType

  implicit none
  save
  private

  type, public :: noahmp_type

    ! define specific variable types for Noah-MP
    type(forcing_type)  :: forcing
    type(config_type)   :: config
    type(energy_type)   :: energy
    type(water_type)    :: water
    !type(biochem_type)  :: biochem

  end type noahmp_type

end module NoahmpType
