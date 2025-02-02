module WaterType

!!! Define column (1-D) Noah-MP Water variables
!!! Water variable initialization is done in WaterInit.f90

  use Machine, only : kind_noahmp

  implicit none
  save
  private

!=== define "flux" sub-type of water_type (water%flux%variable)
  type :: flux_type

    ! define specific water flux variables
    real(kind=kind_noahmp) :: QRAIN

  end type flux_type

!=== define "state" sub-type of water_type (water%state%variable)
  type :: state_type

    ! define specific water state variables
    real(kind=kind_noahmp) :: SMC

  end type state_type

!=== define "parameter" sub-type of water_type (water%param%variable)
  type :: parameter_type

    ! define specific water parameter variables
    real(kind=kind_noahmp) :: BEXP

  end type parameter_type

!=== define "diagnose" sub-type of water_type (water%diag%variable)
  type :: diagnose_type

    ! define specific water diagnose variables
    real(kind=kind_noahmp) :: xxx

  end type diagnose_type

!=== define water type that includes 4 subtypes (flux,state,parameter,diagnose)
  type, public :: water_type

    type(flux_type)      :: flux
    type(state_type)     :: state
    type(parameter_type) :: param
    type(diagnose_type)  :: diag

  end type water_type

end module WaterType
