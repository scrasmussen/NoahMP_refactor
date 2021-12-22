module EnergyVarType

!!! Define column (1-D) Noah-MP Energy variables
!!! Energy variable initialization is done in EnergyInit.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use Machine, only : kind_noahmp

  implicit none
  save
  private

!=== define "flux" sub-type of energy_type (energy%flux%variable)
  type :: flux_type

    ! define specific energy flux variables
    real(kind=kind_noahmp) :: FCEV            ! canopy evaporation (w/m2) [+ = to atm] 
    real(kind=kind_noahmp) :: FCTR            ! transpiration (w/m2) [+ = to atm]
    real(kind=kind_noahmp) :: FGEV            ! soil evap heat (w/m2) [+ to atm]
    real(kind=kind_noahmp) :: FIRR            ! latent heating due to sprinkler irrigation evaporation [w/m2]
    real(kind=kind_noahmp) :: PAHV            ! precipitation advected heat - vegetation net (W/m2)
    real(kind=kind_noahmp) :: PAHG            ! precipitation advected heat - under canopy net (W/m2)
    real(kind=kind_noahmp) :: PAHB            ! precipitation advected heat - bare ground net (W/m2)

  end type flux_type

!=== define "state" sub-type of energy_type (energy%state%variable)
  type :: state_type

    ! define specific energy state variables
    real(kind=kind_noahmp) :: ELAI            ! leaf area index, after burying by snow
    real(kind=kind_noahmp) :: ESAI            ! stem area index, after burying by snow
    real(kind=kind_noahmp) :: LAI             ! leaf area index
    real(kind=kind_noahmp) :: SAI             ! stem area index
    real(kind=kind_noahmp) :: FVEG            ! greeness vegetation fraction
    real(kind=kind_noahmp) :: TG              ! ground temperature (k)
    real(kind=kind_noahmp) :: TV              ! vegetation temperature (k)
    real(kind=kind_noahmp) :: EAIR            ! vapor pressure air (pa)
    logical                :: FROZEN_CANOPY   ! used to define latent heat pathway
    logical                :: FROZEN_GROUND   ! used to define latent heat pathway

    real(kind=kind_noahmp), allocatable, dimension(:) :: STC         ! snow and soil layer temperature [k]
    real(kind=kind_noahmp), allocatable, dimension(:) :: CVSNO       ! snow layer volumetric specific heat (j/m3/k)
    real(kind=kind_noahmp), allocatable, dimension(:) :: TKSNO       ! snow layer thermal conductivity (w/m/k)
    real(kind=kind_noahmp), allocatable, dimension(:) :: CVSOIL      ! soil layer volumetric specific heat (j/m3/k)
    real(kind=kind_noahmp), allocatable, dimension(:) :: TKSOIL      ! soil layer thermal conductivity (w/m/k)
    real(kind=kind_noahmp), allocatable, dimension(:) :: DF          ! thermal conductivity [w/m/k] for all soil and snow layers
    real(kind=kind_noahmp), allocatable, dimension(:) :: HCPCT       ! heat capacity [j/m3/k] for all snow and soil layers
    real(kind=kind_noahmp), allocatable, dimension(:) :: FACT        ! energy factor for soil and snow phase change

  end type state_type

!=== define "parameter" sub-type of energy_type (energy%param%variable)
  type :: parameter_type

    ! define specific energy parameter variables
    real(kind=kind_noahmp) :: CSOIL            ! vol. soil heat capacity [j/m3/K]
    real(kind=kind_noahmp), allocatable, dimension(:) :: LAIM        ! monthly leaf area index, one-sided
    real(kind=kind_noahmp), allocatable, dimension(:) :: SAIM        ! monthly stem area index, one-sided
    real(kind=kind_noahmp), allocatable, dimension(:) :: QUARTZ      ! soil quartz content

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
