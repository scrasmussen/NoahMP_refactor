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
    real(kind=kind_noahmp) :: RAIN          ! liquid rainfall rate (mm/s)
    real(kind=kind_noahmp) :: SNOW          ! snowfall rate (mm/s)
    real(kind=kind_noahmp) :: ECAN          ! evaporation of intercepted water (mm/s) [+]
    real(kind=kind_noahmp) :: ETRAN         ! transpiration rate (mm/s) [+]
    real(kind=kind_noahmp) :: QEVAC         ! canopy water evaporation rate (mm/s)
    real(kind=kind_noahmp) :: QDEWC         ! canopy water dew rate (mm/s)
    real(kind=kind_noahmp) :: QFROC         ! canopy ice frost rate (mm/s)
    real(kind=kind_noahmp) :: QSUBC         ! canopy ice sublimation rate (mm/s)
    real(kind=kind_noahmp) :: QMELTC        ! canopy ice melting rate (mm/s)
    real(kind=kind_noahmp) :: QFRZC         ! canopy water refreezing rate (mm/s)
    real(kind=kind_noahmp) :: QSNOW         ! snowfall at ground surface (mm/s) [+]
    real(kind=kind_noahmp) :: SNOWHIN       ! snow depth increasing rate (m/s)
    real(kind=kind_noahmp) :: QSNFRO        ! snow surface frost rate[mm/s]
    real(kind=kind_noahmp) :: QSNSUB        ! snow surface sublimation rate[mm/s]
    real(kind=kind_noahmp) :: QRAIN         ! ground (soil/snow) surface rain rate[mm/s]
    real(kind=kind_noahmp) :: QSNBOT        ! melting water out of snow bottom [mm/s]
    real(kind=kind_noahmp) :: SNOFLOW       ! glacier flow [mm/s]
    real(kind=kind_noahmp) :: IRFIRATE      ! flood irrigation water rate [m/timestep]
    real(kind=kind_noahmp) :: IRMIRATE      ! micro irrigation water rate [m/timestep]
    real(kind=kind_noahmp) :: IRSIRATE      ! rate of irrigation by sprinkler [m/timestep]
    real(kind=kind_noahmp) :: IREVPLOS      ! loss of irrigation water to evaporation,sprinkler [m/timestep]
    real(kind=kind_noahmp) :: QINSUR        ! water input on soil surface [mm/s]
    real(kind=kind_noahmp) :: RUNSRF        ! surface runoff [mm/s]
    real(kind=kind_noahmp) :: RUNSUB        ! subsurface runoff [mm/s]
    real(kind=kind_noahmp) :: PDDUM         ! infiltration rate at surface (mm/s)
    real(kind=kind_noahmp) :: QSEVA         ! evaporation from soil surface [mm/s]
    real(kind=kind_noahmp) :: QDRAIN        ! soil bottom drainage (m/s)
    real(kind=kind_noahmp) :: QTLDRN        ! tile drainage (mm/s)
    real(kind=kind_noahmp) :: QIN           ! groundwater recharge [mm/s]
    real(kind=kind_noahmp) :: QDIS          ! groundwater discharge [mm/s]
    real(kind=kind_noahmp) :: QVAP          ! soil surface evaporation rate[mm/s]
    real(kind=kind_noahmp) :: QDEW          ! soil surface dew rate[mm/s]
    real(kind=kind_noahmp) :: QSDEW         ! soil surface dew rate [mm/s]
    real(kind=kind_noahmp) :: EIRR          ! evaporation of irrigation water to evaporation,sprinkler [mm/s]
    real(kind=kind_noahmp) :: QINTR         ! interception rate for rain (mm/s)
    real(kind=kind_noahmp) :: QDRIPR        ! drip rate for rain (mm/s)
    real(kind=kind_noahmp) :: QTHROR        ! throughfall for rain (mm/s)
    real(kind=kind_noahmp) :: QINTS         ! interception (loading) rate for snowfall (mm/s)
    real(kind=kind_noahmp) :: QDRIPS        ! drip (unloading) rate for intercepted snow (mm/s)
    real(kind=kind_noahmp) :: QTHROS        ! throughfall of snowfall (mm/s)
    real(kind=kind_noahmp) :: EDIR          ! net direct soil evaporation (mm/s)

    real(kind=kind_noahmp), allocatable, dimension(:) :: ETRANI    ! evapotranspiration from soil layers [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: DDZ1      ! rate of settling of snowpack due to destructive metamorphism [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: DDZ2      ! rate of compaction of snowpack due to overburden [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: DDZ3      ! rate of compaction of snowpack due to melt [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: PDZDTC    ! rate of change in fractional-thickness due to compaction [fraction/s]

  end type flux_type

!=== define "state" sub-type of water_type (water%state%variable)
  type :: state_type

    ! define specific water state variables
    integer                :: IRCNTSI     ! irrigation event number, Sprinkler
    integer                :: IRCNTMI     ! irrigation event number, Micro
    integer                :: IRCNTFI     ! irrigation event number, Flood
    real(kind=kind_noahmp) :: CMC         ! total canopy intercepted water (mm)
    real(kind=kind_noahmp) :: FWET        ! wetted or snowed fraction of the canopy
    real(kind=kind_noahmp) :: BDFALL      ! bulk density of snowfall (kg/m3)
    real(kind=kind_noahmp) :: CANLIQ      ! intercepted liquid water (mm)
    real(kind=kind_noahmp) :: CANICE      ! intercepted ice mass (mm)
    real(kind=kind_noahmp) :: MAXSNO      ! canopy capacity for snow interception (mm)
    real(kind=kind_noahmp) :: MAXLIQ      ! canopy capacity for rain interception (mm)
    real(kind=kind_noahmp) :: SNOWH       ! snow depth [m]
    real(kind=kind_noahmp) :: SNEQV       ! snow water equivalent [mm]
    real(kind=kind_noahmp) :: PONDING     ! surface ponding (mm)
    real(kind=kind_noahmp) :: PONDING1    ! surface ponding 1 (mm)
    real(kind=kind_noahmp) :: PONDING2    ! surface ponding 2 (mm)
    real(kind=kind_noahmp) :: FIFAC       ! fraction of grid under flood irrigation (0 to 1)
    real(kind=kind_noahmp) :: IRAMTFI     ! flood irrigation water amount [m]
    real(kind=kind_noahmp) :: MIFAC       ! fraction of grid under micro irrigation (0 to 1)
    real(kind=kind_noahmp) :: IRAMTMI     ! micro irrigation water amount [m]
    real(kind=kind_noahmp) :: SIFAC       ! sprinkler irrigation fraction (0 to 1)
    real(kind=kind_noahmp) :: IRAMTSI     ! sprinkler irrigation water amount [m]
    real(kind=kind_noahmp) :: ZWT         ! water table depth [m]
    real(kind=kind_noahmp) :: SICEMAX     ! maximum soil ice content (m3/m3)
    real(kind=kind_noahmp) :: SH2OMIN     ! minimum soil liquid water content (m3/m3)
    real(kind=kind_noahmp) :: FSAT        ! fractional saturated area for soil moisture
    real(kind=kind_noahmp) :: FCRMAX      ! maximum fraction of imperviousness (FCR)
    real(kind=kind_noahmp) :: SMCWTD      ! soil moisture between bottom of the soil and the water table
    real(kind=kind_noahmp) :: DEEPRECH    ! recharge to or from the water table when deep [m]
    real(kind=kind_noahmp) :: RECH        ! groundwater recharge (net vertical flux across the water table), positive up
    real(kind=kind_noahmp) :: WPLUS       ! saturation excess of the total soil [m]
    real(kind=kind_noahmp) :: WATBLED     ! water table depth estimated in WRF-Hydro fine grids
    real(kind=kind_noahmp) :: TDFRACMP    ! tile drainage map(fraction)
    real(kind=kind_noahmp) :: WA          ! water storage in aquifer [mm]
    real(kind=kind_noahmp) :: WT          ! water storage in aquifer + saturated soil [mm]
    real(kind=kind_noahmp) :: WSLAKE      ! water storage in lake (can be -) (mm) 
    real(kind=kind_noahmp) :: sfcheadrt   ! surface water head (mm)
    real(kind=kind_noahmp) :: IRRFRA      ! irrigation fraction
    real(kind=kind_noahmp) :: FP          ! fraction of the gridcell that receives precipitation

    integer               , allocatable, dimension(:) :: IMELT         ! phase change index [0-none;1-melt;2-refreeze]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SNICE         ! snow layer ice [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SNLIQ         ! snow layer liquid water [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: FICEOLD_SNOW  ! ice fraction in snow layers at last timestep
    real(kind=kind_noahmp), allocatable, dimension(:) :: FICE_SNOW     ! ice fraction in snow layers at current timestep
    real(kind=kind_noahmp), allocatable, dimension(:) :: FICE_SOIL     ! ice fraction in soil layers at current timestep
    real(kind=kind_noahmp), allocatable, dimension(:) :: EPORE_SNOW    ! snow effective porosity (m3/m3) 
    real(kind=kind_noahmp), allocatable, dimension(:) :: SH2O          ! soil liquid moisture (m3/m3)
    real(kind=kind_noahmp), allocatable, dimension(:) :: SICE          ! soil ice moisture (m3/m3)
    real(kind=kind_noahmp), allocatable, dimension(:) :: SMC           ! total soil moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: FCR           ! fraction of imperviousness due to frozen soil
    real(kind=kind_noahmp), allocatable, dimension(:) :: WCND          ! soil hydraulic conductivity (m/s)
    real(kind=kind_noahmp), allocatable, dimension(:) :: WDF           ! soil water diffusivity (m2/s)
    real(kind=kind_noahmp), allocatable, dimension(:) :: EPORE_SOIL    ! soil effective porosity (m3/m3) 
    real(kind=kind_noahmp), allocatable, dimension(:) :: SMCEQ         ! equilibrium soil water  content [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: BTRANI        ! soil water transpiration factor (0 to 1)

  end type state_type

!=== define "parameter" sub-type of water_type (water%param%variable)
  type :: parameter_type

    ! define specific water parameter variables
    integer                :: DRAIN_LAYER_OPT  ! starting soil layer for drainage
    integer                :: TD_DEPTH         ! depth of drain tube from the soil surface
    integer                :: NROOT            ! number of soil layers with root present
    integer                :: IRR_HAR          ! number of days before harvest date to stop irrigation
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
    real(kind=kind_noahmp) :: KDT              ! parameter to calculate maximum infiltration rate
    real(kind=kind_noahmp) :: FRZX             ! parameter to calculate frozen soil impermeable fraction
    real(kind=kind_noahmp) :: BVIC             ! VIC model infiltration parameter
    real(kind=kind_noahmp) :: AXAJ             ! Tension water distribution inflection parameter
    real(kind=kind_noahmp) :: BXAJ             ! Tension water distribution shape parameter
    real(kind=kind_noahmp) :: XXAJ             ! Free water distribution shape parameter
    real(kind=kind_noahmp) :: BBVIC            ! DVIC heterogeniety parameter for infiltration
    real(kind=kind_noahmp) :: GDVIC            ! DVIC Mean Capillary Drive (m) for infiltration models
    real(kind=kind_noahmp) :: BDVIC            ! DVIC model infiltration parameter
    real(kind=kind_noahmp) :: SLOPE            ! slope index for soil drainage
    real(kind=kind_noahmp) :: TD_DC            ! drainage coefficient (mm d^-1)
    real(kind=kind_noahmp) :: TDSMC_FAC        ! drainage factor for soil moisture
    real(kind=kind_noahmp) :: TD_DCOEF         ! drainage coefficent (m d^-1)
    real(kind=kind_noahmp) :: TD_ADEPTH        ! Actual depth to impermeable layer form surface
    real(kind=kind_noahmp) :: KLAT_FAC         ! multiplication factor to determine lateral hydraulic conductivity
    real(kind=kind_noahmp) :: TD_DDRAIN        ! Depth of drain (m)
    real(kind=kind_noahmp) :: TD_SPAC          ! distance between two drain tubes or tiles (m)
    real(kind=kind_noahmp) :: TD_RADI          ! effective radius of drains (m)
    real(kind=kind_noahmp) :: TD_D             ! depth to impervious layer from drain water level (m)
    real(kind=kind_noahmp) :: FFF              ! runoff decay factor (m-1)
    real(kind=kind_noahmp) :: RSBMX            ! baseflow coefficient [mm/s]
    real(kind=kind_noahmp) :: TIMEAN           ! gridcell mean topgraphic index (global mean)
    real(kind=kind_noahmp) :: FSATMX           ! maximum surface saturated fraction (global mean)
    real(kind=kind_noahmp) :: ROUS             ! specific yield [-] for Niu et al. 2007 groundwater scheme
    real(kind=kind_noahmp) :: CMIC             ! microprore content (0.0-1.0), 0.0: close to free drainage
    real(kind=kind_noahmp) :: WSLMAX           ! maximum lake water storage (mm)
    real(kind=kind_noahmp) :: SWEMAXGLA        ! Maximum SWE allowed at glaciers (mm)
    real(kind=kind_noahmp) :: REFDK            ! Parameter in the surface runoff parameterization
    real(kind=kind_noahmp) :: REFKDT           ! Parameter in the surface runoff parameterization
    real(kind=kind_noahmp) :: FRZK             ! Frozen ground parameter
    real(kind=kind_noahmp) :: IRR_LAI          ! minimum lai to trigger irrigation
    real(kind=kind_noahmp) :: IRR_MAD          ! management allowable deficit (0-1)
    real(kind=kind_noahmp) :: FILOSS           ! fraction of flood irrigation loss (0-1)
    real(kind=kind_noahmp) :: SPRIR_RATE       ! sprinkler irrigation rate (mm/h)
    real(kind=kind_noahmp) :: IRR_FRAC         ! irrigation Fraction
    real(kind=kind_noahmp) :: IR_RAIN          ! maximum precipitation to stop irrigation trigger
    real(kind=kind_noahmp) :: SNOWDEN_MIN      ! minimum fresh snowfall density (kg/m3)

    real(kind=kind_noahmp), allocatable, dimension(:) :: SMCMAX  ! saturated value of soil moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SMCWLT  ! wilting point soil moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SMCREF  ! reference soil moisture (field capacity) (m3/m3)
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
