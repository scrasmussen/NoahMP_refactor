module BiochemVarType

!!! Define column (1-D) Noah-MP Biochemistry (carbon,nitrogen,etc) variables
!!! Biochemistry variable initialization is done in BiochemInit.f90

  use Machine, only : kind_noahmp

  implicit none
  save
  private

!=== define "flux" sub-type of biochem_type (biochem%flux%variable)
  type :: flux_type

    ! define specific biochem flux variables
    real(kind=kind_noahmp) :: xxx

  end type flux_type

!=== define "state" sub-type of biochem_type (biochem%state%variable)
  type :: state_type

    ! define specific biochem state variables
    real(kind=kind_noahmp) :: IGS
    integer                :: PGS

  end type state_type

!=== define "parameter" sub-type of biochem_type (biochem%param%variable)
  type :: parameter_type

    ! define specific biochem parameter variables

    real(kind=kind_noahmp) :: PLTDAY
    real(kind=kind_noahmp) :: TMIN   ! minimum temperature for photosynthesis (k)

  end type parameter_type

!=== define "diagnose" sub-type of biochem_type (biochem%diag%variable)
  type :: diagnose_type

    ! define specific biochem diagnose variables
    real(kind=kind_noahmp) :: xxx

  end type diagnose_type

!=== define biochem type that includes 4 subtypes (flux,state,parameter,diagnose)
  type, public :: biochem_type

    type(flux_type)      :: flux
    type(state_type)     :: state
    type(parameter_type) :: param
    type(diagnose_type)  :: diag

  end type biochem_type

end module BiochemVarType
