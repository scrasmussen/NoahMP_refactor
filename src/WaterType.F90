module WaterType

!!! Define column (1-D) Noah-MP Water variables
!!! Water variable initialization is done in WaterInit.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use Machine, only : kind_noahmp

  implicit none
  save
  private

!=== define "flux" sub-type of water_type (water%flux%variable)
  type :: flux_type

    ! define specific water flux variables
    real(kind=kind_noahmp) :: ECAN    ! evaporation of intercepted water (mm/s) [+]
    real(kind=kind_noahmp) :: ETRAN   ! transpiration rate (mm/s) [+]
    real(kind=kind_noahmp) :: QEVAC   ! canopy water evaporation rate (mm/s)
    real(kind=kind_noahmp) :: QDEWC   ! canopy water dew rate (mm/s)
    real(kind=kind_noahmp) :: QFROC   ! canopy ice frost rate (mm/s)
    real(kind=kind_noahmp) :: QSUBC   ! canopy ice sublimation rate (mm/s)
    real(kind=kind_noahmp) :: QMELTC  ! canopy ice melting rate (mm/s)
    real(kind=kind_noahmp) :: QFRZC   ! canopy water refreezing rate (mm/s)

  end type flux_type

!=== define "state" sub-type of water_type (water%state%variable)
  type :: state_type

    ! define specific water state variables
    real(kind=kind_noahmp) :: CMC     ! total canopy intercepted water (mm)
    real(kind=kind_noahmp) :: FWET    ! wetted or snowed fraction of the canopy
    real(kind=kind_noahmp) :: BDFALL  ! bulk density of snowfall (kg/m3)
    real(kind=kind_noahmp) :: CANLIQ  ! intercepted liquid water (mm)
    real(kind=kind_noahmp) :: CANICE  ! intercepted ice mass (mm)
    real(kind=kind_noahmp) :: MAXSNO  ! canopy capacity for snow interception (mm)
    real(kind=kind_noahmp) :: MAXLIQ  ! canopy capacity for rain interception (mm)

  end type state_type

!=== define "parameter" sub-type of water_type (water%param%variable)
  type :: parameter_type

    ! define specific water parameter variables
    real(kind=kind_noahmp) :: CH2OP   ! maximum canopy intercepted water per unit lai+sai (mm)


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
