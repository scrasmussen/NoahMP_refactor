module ConfigType
!!! Define column (1-D) Noah-MP configuration variables
!!! Configuration variable initialization is done in ConfigInit.f90

  use Machine, only : kind_noahmp

  implicit none
  save
  private

!=== define "namelist" sub-type of config_type (config%nmlist%variable)
  type :: namelist_type

    ! define specific namelist variables
    integer    :: OPT_RUN
    integer    :: OPT_SNF

  end type namelist_type

!=== define "domain" sub-type of config_type (config%domain%variable)
  type :: domain_type

    ! define specific domain variables
    real(kind=kind_noahmp) :: iloc
    real(kind=kind_noahmp) :: jloc
    real(kind=kind_noahmp) :: dt    ! model timestep (unit: second)
    real(kind=kind_noahmp) :: COSZ  !cosine solar zenith angle [0-1]

  end type domain_type

!=== define config type that includes namelist & domain subtypes
  type, public :: config_type

    type(namelist_type) :: nmlist ! not using "namelist" to avoid issue with Fortran intrinsic namelist function
    type(domain_type)   :: domain

  end type config_type

end module ConfigType
