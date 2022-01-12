module WaterVarType

!!! Define column (1-D) Noah-MP Water variables
!!! Water variable initialization is done in WaterInit.f90

  use Machine, only : kind_noahmp

  implicit none
  save
  private

!=== define "flux" sub-type of water_type (water%flux%variable)
  type :: flux_type

    ! define specific water flux variables

  end type flux_type

!=== define "state" sub-type of water_type (water%state%variable)
  type :: state_type

    ! define specific water state variables

    real(kind=kind_noahmp) :: SNOWH  !snow height [m]
    real(kind=kind_noahmp) :: BTRAN  !soil water transpiration factor (0 to 1)
    real(kind=kind_noahmp) :: WROOT  !root zone soil water
    real(kind=kind_noahmp) :: WSTRES !soil water stress

    real(kind=kind_noahmp), allocatable, dimension(:) :: SMC           ! total soil moisture [m3/m3]

  end type state_type

!=== define "parameter" sub-type of water_type (water%param%variable)
  type :: parameter_type

    ! define specific water parameter variables
  integer                :: NROOT            ! number of soil layers with root present

  real(kind=kind_noahmp), allocatable, dimension(:) :: SMCMAX  ! saturated value of soil moisture [m3/m3]


  end type parameter_type

!=== define "diagnose" sub-type of water_type (water%diag%variable)
  type :: diagnose_type

    ! define specific water diagnose variables

  end type diagnose_type

!=== define water type that includes 4 subtypes (flux,state,parameter,diagnose)
  type, public :: water_type

    type(flux_type)      :: flux
    type(state_type)     :: state
    type(parameter_type) :: param
    type(diagnose_type)  :: diag

  end type water_type

end module WaterVarType
