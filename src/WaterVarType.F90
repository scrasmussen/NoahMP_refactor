module WaterVarType

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
    real(kind=kind_noahmp) :: ECAN          ! evaporation of intercepted water (mm/s) [+]
    real(kind=kind_noahmp) :: ETRAN         ! transpiration rate (mm/s) [+]
    real(kind=kind_noahmp) :: QEVAC         ! canopy water evaporation rate (mm/s)
    real(kind=kind_noahmp) :: QDEWC         ! canopy water dew rate (mm/s)
    real(kind=kind_noahmp) :: QFROC         ! canopy ice frost rate (mm/s)
    real(kind=kind_noahmp) :: QSUBC         ! canopy ice sublimation rate (mm/s)
    real(kind=kind_noahmp) :: QMELTC        ! canopy ice melting rate (mm/s)
    real(kind=kind_noahmp) :: QFRZC         ! canopy water refreezing rate (mm/s)
    real(kind=kind_noahmp) :: QSNOW         ! snow at ground surface (mm/s) [+]
    real(kind=kind_noahmp) :: SNOWHIN       ! snow depth increasing rate (m/s)
    real(kind=kind_noahmp) :: QSNFRO        ! snow surface frost rate[mm/s]
    real(kind=kind_noahmp) :: QSNSUB        ! snow surface sublimation rate[mm/s]
    real(kind=kind_noahmp) :: QRAIN         ! snow surface rain rate[mm/s]
    real(kind=kind_noahmp) :: QSNBOT        ! melting water out of snow bottom [mm/s]
    real(kind=kind_noahmp) :: SNOFLOW       ! glacier flow [mm/s]
    real(kind=kind_noahmp) :: IRFIRATE      ! flood irrigation water rate [m/timestep]
    real(kind=kind_noahmp) :: IRMIRATE      ! micro irrigation water rate [m/timestep]
    real(kind=kind_noahmp) :: QINSUR        ! water input on soil surface [mm/s]

    real(kind=kind_noahmp), allocatable, dimension(:) :: DDZ1      ! rate of settling of snowpack due to destructive metamorphism [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: DDZ2      ! rate of compaction of snowpack due to overburden [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: DDZ3      ! rate of compaction of snowpack due to melt [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: PDZDTC    ! rate of change in fractional-thickness due to compaction [fraction/s]

  end type flux_type

!=== define "state" sub-type of water_type (water%state%variable)
  type :: state_type

    ! define specific water state variables
    real(kind=kind_noahmp) :: CMC       ! total canopy intercepted water (mm)
    real(kind=kind_noahmp) :: FWET      ! wetted or snowed fraction of the canopy
    real(kind=kind_noahmp) :: BDFALL    ! bulk density of snowfall (kg/m3)
    real(kind=kind_noahmp) :: CANLIQ    ! intercepted liquid water (mm)
    real(kind=kind_noahmp) :: CANICE    ! intercepted ice mass (mm)
    real(kind=kind_noahmp) :: MAXSNO    ! canopy capacity for snow interception (mm)
    real(kind=kind_noahmp) :: MAXLIQ    ! canopy capacity for rain interception (mm)
    real(kind=kind_noahmp) :: SNOWH     ! snow depth [m]
    real(kind=kind_noahmp) :: SNEQV     ! snow water equivalent [mm]
    real(kind=kind_noahmp) :: PONDING1  ! surface ponding 1 (mm)
    real(kind=kind_noahmp) :: PONDING2  ! surface ponding 2 (mm)
    real(kind=kind_noahmp) :: FIFAC     ! fraction of grid under flood irrigation (0 to 1)
    real(kind=kind_noahmp) :: IRAMTFI   ! flood irrigation water amount [m]
    real(kind=kind_noahmp) :: MIFAC     ! fraction of grid under micro irrigation (0 to 1)
    real(kind=kind_noahmp) :: IRAMTMI   ! micro irrigation water amount [m]
    real(kind=kind_noahmp) :: ZWT       ! water table depth [m]
    real(kind=kind_noahmp) :: SICEMAX   ! maximum soil ice content (m3/m3)

    integer               , allocatable, dimension(:) :: IMELT     ! phase change index [0-none;1-melt;2-refreeze]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SNICE     ! snow layer ice [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SNLIQ     ! snow layer liquid water [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: FICEOLD   ! ice fraction at last timestep
    real(kind=kind_noahmp), allocatable, dimension(:) :: FICE      ! ice fraction at current timestep
    real(kind=kind_noahmp), allocatable, dimension(:) :: SH2O      ! soil liquid moisture (m3/m3)
    real(kind=kind_noahmp), allocatable, dimension(:) :: SICE      ! soil ice moisture (m3/m3)
    real(kind=kind_noahmp), allocatable, dimension(:) :: SMC       ! total soil moisture [m3/m3]

  end type state_type

!=== define "parameter" sub-type of water_type (water%param%variable)
  type :: parameter_type

    ! define specific water parameter variables
    real(kind=kind_noahmp) :: CH2OP            ! maximum canopy intercepted water per unit lai+sai (mm)
    real(kind=kind_noahmp) :: C2_SnowCompact   ! overburden snow compaction parameter (m3/kg) default 21.e-3
    real(kind=kind_noahmp) :: C3_SnowCompact   ! snow desctructive metamorphism compaction parameter1 [1/s]
    real(kind=kind_noahmp) :: C4_SnowCompact   ! snow desctructive metamorphism compaction parameter2 [1/k]
    real(kind=kind_noahmp) :: C5_SnowCompact   ! snow desctructive metamorphism compaction parameter3 
    real(kind=kind_noahmp) :: DM_SnowCompact   ! upper Limit on destructive metamorphism compaction [kg/m3]
    real(kind=kind_noahmp) :: ETA0_SnowCompact ! snow viscosity coefficient [kg-s/m2], Anderson1979: 0.52e6~1.38e6
    real(kind=kind_noahmp) :: SNLIQMAXFRAC     ! maximum liquid water fraction in snow
    real(kind=kind_noahmp) :: SSI              ! liquid water holding capacity for snowpack (m3/m3)
    real(kind=kind_noahmp) :: SNOW_RET_FAC     ! snowpack water release timescale factor (1/s)
    real(kind=kind_noahmp) :: FIRTFAC          ! flood application rate factor
    real(kind=kind_noahmp) :: MICIR_RATE       ! micro irrigation rate (mm/hr)

    real(kind=kind_noahmp), allocatable, dimension(:) :: SMCMAX  ! saturated value of soil moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: DWSAT   ! saturated soil hydraulic diffusivity (m2/s)
    real(kind=kind_noahmp), allocatable, dimension(:) :: DKSAT   ! saturated soil hydraulic conductivity [m/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: BEXP    ! soil B parameter
    real(kind=kind_noahmp), allocatable, dimension(:) :: PSISAT  ! saturated soil matric potential (m)

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
