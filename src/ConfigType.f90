module ConfigType
!!! Define column (1-D) Noah-MP configuration variables
!!! Configuration variable initialization is done in ConfigInit.f90

  use Machine, only : kind_noahmp

  implicit none
  save
  private

!=== define "namelist" sub-type of config_type (config%namelist%variable)
  type :: namelist_type

    ! define specific namelist variables
    integer    :: runoff_option

  end type namelist_type

!=== define "domain" sub-type of config_type (config%domain%variable)
  type :: domain_type

    ! define specific domain variables
    real(kind=kind_noahmp) :: dt ! model timestep (unit: second)

  end type domain_type

!=== define config type that includes namelist & domain subtypes
  type, public :: config_type

    type(namelist_type) :: namelist
    type(domain_type)   :: domain

  end type config_type

end module ConfigType
