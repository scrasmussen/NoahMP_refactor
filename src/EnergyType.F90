module EnergyType

!!! Define column (1-D) Noah-MP Energy variables
!!! Energy variable initialization is done in EnergyInit.f90

  use Machine, only : kind_noahmp

  implicit none
  save
  private

!=== define "flux" sub-type of energy_type (energy%flux%variable)
  type :: flux_type

    ! define specific energy flux variables
    real(kind=kind_noahmp) :: SAV

  end type flux_type

!=== define "state" sub-type of energy_type (energy%state%variable)
  type :: state_type

    ! define specific energy state variables
    real(kind=kind_noahmp) :: TAH

  end type state_type

!=== define "parameter" sub-type of energy_type (energy%param%variable)
  type :: parameter_type

    ! define specific energy parameter variables
    real(kind=kind_noahmp) :: RAHC

  end type parameter_type

!=== define "diagnose" sub-type of energy_type (energy%diag%variable)
  type :: diagnose_type

    ! define specific energy diagnose variables
    real(kind=kind_noahmp) :: xxx

  end type diagnose_type

!=== define energy type that includes 4 subtypes (flux,state,parameter,diagnose)
  type, public :: energy_type

    type(flux_type)      :: flux
    type(state_type)     :: state
    type(parameter_type) :: param
    type(diagnose_type)  :: diag

  end type energy_type

end module EnergyType
