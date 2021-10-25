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
    real(kind=kind_noahmp)                 :: SAV
    real(kind=kind_noahmp), dimension(1:2) :: SOLAD    !incoming direct solar radiation [w/m2]
    real(kind=kind_noahmp), dimension(1:2) :: SOLAI    !incoming diffuse solar radiation [w/m2]
    real(kind=kind_noahmp)                 :: SWDOWN   !downward solar filtered by sun angle [w/m2]

  end type flux_type

!=== define "state" sub-type of energy_type (energy%state%variable)
  type :: state_type

    ! define specific energy state variables
    real(kind=kind_noahmp) :: TAH      
    real(kind=kind_noahmp) :: THAIR    !potential temperature [k]
    real(kind=kind_noahmp) :: QAIR     !specific humidity [kg/kg]: [q2/(1+q2)]
    real(kind=kind_noahmp) :: EAIR     !vapor pressure air [pa]
    real(kind=kind_noahmp) :: RHOAIR   !density air [kg/m3]
    
    
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
