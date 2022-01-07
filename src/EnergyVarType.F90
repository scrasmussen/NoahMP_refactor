module EnergyVarType

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
    real(kind=kind_noahmp) :: ELAI            ! leaf area index, after burying by snow
    real(kind=kind_noahmp) :: ESAI            ! stem area index, after burying by snow
    real(kind=kind_noahmp) :: LAI             ! leaf area index
    real(kind=kind_noahmp) :: SAI             ! stem area index
    real(kind=kind_noahmp) :: TV              ! vegetation temperature (k)
    real(kind=kind_noahmp) :: TROOT           ! root-zone averaged temperature (k)

  end type state_type

!=== define "parameter" sub-type of energy_type (energy%param%variable)
  type :: parameter_type

    ! define specific energy parameter variables
    real(kind=kind_noahmp) :: RAHC
    real(kind=kind_noahmp) :: HVT              ! top of canopy (m)
    real(kind=kind_noahmp) :: HVB              ! bottom of canopy (m)

    real(kind=kind_noahmp), allocatable, dimension(:) :: LAIM        ! monthly leaf area index, one-sided
    real(kind=kind_noahmp), allocatable, dimension(:) :: SAIM        ! monthly stem area index, one-sided

  end type parameter_type

!=== define "diagnose" sub-type of energy_type (energy%diag%variable)
  type :: diagnose_type

    ! define specific energy diagnose variables
    

  end type diagnose_type

!=== define energy type that includes 4 subtypes (flux,state,parameter,diagnose)
  type, public :: energy_type

    type(flux_type)      :: flux
    type(state_type)     :: state
    type(parameter_type) :: param
    type(diagnose_type)  :: diag

  end type energy_type

end module EnergyVarType
