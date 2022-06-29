module ConfigVarType

!!! Define column (1-D) Noah-MP configuration variables
!!! Configuration variable initialization is done in ConfigVarInitMod.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use Machine

  implicit none
  save
  private

!=== define "namelist" sub-type of config_type (config%nmlist%variable)
  type :: namelist_type

    ! define specific namelist variables
    integer                   :: OptDynamicVeg               ! options for dynamic vegetation
                                                                ! 1 -> off (use table LAI; use FVEG = SHDFAC from input)
                                                                ! 2 -> on  (together with OptStomataResistance = 1)
                                                                ! 3 -> off (use table LAI; calculate FVEG)
                                                                ! 4 -> off (use table LAI; use maximum vegetation fraction) (default)
                                                                ! 5 -> on  (use maximum vegetation fraction)
                                                                ! 6 -> on  (use FVEG = SHDFAC from input)
                                                                ! 7 -> off (use input LAI; use FVEG = SHDFAC from input)
                                                                ! 8 -> off (use input LAI; calculate FVEG)
                                                                ! 9 -> off (use input LAI; use maximum vegetation fraction)
    integer                   :: OptRainSnowPartition        ! options for partitioning  precipitation into rainfall & snowfall
                                                                ! 1 -> Jordan (1991) scheme (default)
                                                                ! 2 -> BATS: when TemperatureAirRefHeight < freezing point+2.2 
                                                                ! 3 -> TemperatureAirRefHeight < freezing point
                                                                ! 4 -> Use WRF microphysics output
                                                                ! 5 -> Use wetbulb temperature (Wang et al., 2019)
    integer                   :: OptSoilWaterTranspiration   ! options for soil moisture factor for stomatal resistance & evapotranspiration
                                                                ! 1 -> Noah (soil moisture) (default) 
                                                                ! 2 -> CLM  (matric potential)
                                                                ! 3 -> SSiB (matric potential)
    integer                   :: OptGroundResistanceEvap     ! options for ground resistent to evaporation/sublimation
                                                                ! 1 -> Sakaguchi and Zeng, 2009 (default)
                                                                ! 2 -> Sellers (1992)
                                                                ! 3 -> adjusted Sellers to decrease RSURF for wet soil
                                                                ! 4 -> option 1 for non-snow; rsurf = rsurf_snow for snow (set in table)
    integer                   :: OptSurfaceDrag              ! options for surface layer drag/exchange coefficient
                                                                ! 1 -> Monin-Obukhov (M-O) Similarity Theory (MOST) (default)
                                                                ! 2 -> original Noah (Chen et al. 1997)
    integer                   :: OptStomataResistance        ! options for canopy stomatal resistance
                                                                ! 1 -> Ball-Berry scheme (default)
                                                                ! 2 -> Jarvis scheme
    integer                   :: OptSnowAlbedo               ! options for ground snow surface albedo
                                                                ! 1 -> BATS snow albedo scheme (default)
                                                                ! 2 -> CLASS snow albedo scheme
    integer                   :: OptCanopyRadiationTransfer  ! options for canopy radiation transfer
                                                                ! 1 -> modified two-stream (gap=F(solar angle,3D structure, etc)<1-FVEG)
                                                                ! 2 -> two-stream applied to grid-cell (gap = 0)
                                                                ! 3 -> two-stream applied to vegetated fraction (gap=1-FVEG) (default)
    integer                   :: OptSnowSoilTempTime         ! options for snow/soil temperature time scheme (only layer 1)
                                                                ! 1 -> semi-implicit; flux top boundary condition (default)
                                                                ! 2 -> full implicit (original Noah); temperature top boundary condition
                                                                ! 3 -> same as 1, but FSNO for TS calculation (generally improves snow)
    integer                   :: OptSnowThermConduct         ! options for snow thermal conductivity
                                                                ! 1 -> Stieglitz(yen,1965) scheme (default)
                                                                ! 2 -> Anderson, 1976 scheme
                                                                ! 3 -> constant
                                                                ! 4 -> Verseghy (1991) scheme
                                                                ! 5 -> Douvill(Yen, 1981) scheme
    integer                   :: OptSoilTemperatureBottom    ! options for lower boundary condition of soil temperature
                                                                ! 1 -> zero heat flux from bottom (ZBOT & TemperatureSoilBottom not used)
                                                                ! 2 -> TBOT at ZBOT (8m) read from a file (original Noah) (default)
    integer                   :: OptSoilSupercoolWater       ! options for soil supercooled liquid water
                                                                ! 1 -> no iteration (Niu and Yang, 2006 JHM) (default)
                                                                ! 2 -> Koren's iteration (Koren et al., 1999 JGR)
    integer                   :: OptRunoffSurface            ! options for surface runoff
                                                                ! 1 -> TOPMODEL with groundwater
                                                                ! 2 -> TOPMODEL with an equilibrium water table
                                                                ! 3 -> original surface and subsurface runoff (free drainage) (default)
                                                                ! 4 -> BATS surface and subsurface runoff (free drainage)
                                                                ! 5 -> Miguez-Macho&Fan groundwater scheme
                                                                ! 6 -> Variable Infiltration Capacity Model surface runoff scheme
                                                                ! 7 -> Xiananjiang Infiltration and surface runoff scheme 
                                                                ! 8 -> Dynamic VIC surface runoff scheme
    integer                   :: OptRunoffSubsurface         ! options for drainage & subsurface runoff
                                                                ! separated from original NoahMP runoff option
                                                                ! currently tested & recommended the same option# as surface runoff (default)
    integer                   :: OptSoilPermeabilityFrozen   ! options for frozen soil permeability
                                                                ! 1 -> linear effects, more permeable (default)
                                                                ! 2 -> nonlinear effects, less permeable
    integer                   :: OptDynVicInfiltration       ! options for infiltration in dynamic VIC runoff scheme
                                                                ! 1 -> Philip scheme (default)
                                                                ! 2 -> Green-Ampt scheme 
                                                                ! 3 -> Smith-Parlange scheme    
    integer                   :: OptTileDrainage             ! options for tile drainage 
                                                                ! currently only tested & calibrated to work with runoff option=3
                                                                ! 0 -> No tile drainage (default)
                                                                ! 1 -> on (simple scheme)
                                                                ! 2 -> on (Hooghoudt's scheme)
    integer                   :: OptIrrigation               ! options for irrigation
                                                                ! 0 -> No irrigation (default)
                                                                ! 1 -> Irrigation ON
                                                                ! 2 -> irrigation trigger based on crop season Planting and harvesting dates
                                                                ! 3 -> irrigation trigger based on LAI threshold
    integer                   :: OptIrrigationMethod         ! options for irrigation method
                                                                ! only works when OptIrrigation > 0
                                                                ! 0 -> method based on geo_em fractions (default)
                                                                ! 1 -> sprinkler method
                                                                ! 2 -> micro/drip irrigation
                                                                ! 3 -> surface flooding
    integer                   :: OptCropModel                ! options for crop model
                                                                ! 0 -> No crop model (default)
                                                                ! 1 -> Liu, et al. 2016 crop scheme
    integer                   :: OptSoilProperty             ! options for defining soil properties
                                                                ! 1 -> use input dominant soil texture (default)
                                                                ! 2 -> use input soil texture that varies with depth
                                                                ! 3 -> use soil composition (sand, clay, orgm) and pedotransfer function
                                                                ! 4 -> use input soil properties (BEXP_3D, SMCMAX_3D, etc.)
    integer                   :: OptPedotransfer             ! options for pedotransfer functions 
                                                                ! only works when OptSoilProperty = 3
                                                                ! 1 -> Saxton and Rawls (2006) scheme (default)
    integer                   :: OptGlacierTreatment         ! options for glacier treatment
                                                                ! 1 -> include phase change of ice (default)
                                                                ! 2 -> ice treatment more like original Noah

  end type namelist_type

!=== define "domain" sub-type of config_type (config%domain%variable)
  type :: domain_type

    ! define specific domain variables
    character(len=256)        :: LLANDUSE      ! landuse data name (USGS or MODIS_IGBP)
    logical                   :: URBAN_FLAG    ! flag for urban grid
    logical                   :: CROPLU        ! flag to identify croplands
    logical                   :: CROP_ACTIVE   ! flag to activate crop model
    logical                   :: DVEG_ACTIVE   ! flag to activate dynamic vegetation model
    integer                   :: ILOC          ! model grid index
    integer                   :: JLOC          ! model grid index
    integer                   :: VEGTYP        ! vegetation type
    integer                   :: CROPTYP       ! crop type
    integer                   :: NSOIL         ! number of soil layers
    integer                   :: NSNOW         ! maximum number of snow layers
    integer                   :: ISNOW         ! actual number of snow layers
    integer                   :: IST           ! surface type 1-soil; 2-lake
    integer                   :: NBAND         ! number of solar radiation wave bands
    integer                   :: SOILCOLOR     ! soil texture type for albedo
    integer                   :: ICE           ! flag for seaice point (=1: ice point)
    integer                   :: ISWATER       ! flag to identify water
    integer                   :: ISBARREN      ! flag to identify barren land
    integer                   :: ISICE         ! flag to identify ice
    integer                   :: ISCROP        ! flag to identify crop
    integer                   :: EBLFOREST     ! flag to identify EBL Forest
    integer                   :: NSTAGE        ! number of growth stages
    integer                   :: YEARLEN       ! Number of days in the particular year
    integer                   :: SLOPETYP      ! underground runoff slope term
    real(kind=kind_noahmp)    :: DT            ! noahmp timestep (s)
    real(kind=kind_noahmp)    :: DX            ! noahmp model grid spacing (m)
    real(kind=kind_noahmp)    :: JULIAN        ! julian day of the year
    real(kind=kind_noahmp)    :: COSZ          ! cosine solar zenith angle
    real(kind=kind_noahmp)    :: ZREF          ! reference height  (m)
    real(kind=kind_noahmp)    :: DZ8W          ! thickness of surface atmospheric layers [m]
    real(kind=kind_noahmp)    :: ZLVL          ! thickness of surface atmospheric layers [m]
    real(kind=kind_noahmp)    :: LAT           ! latitude (radians)

    integer               , allocatable, dimension(:) :: SOILTYP ! soil type for each soil layer
    real(kind=kind_noahmp), allocatable, dimension(:) :: ZSOIL   ! depth of layer-bottom from soil surface
    real(kind=kind_noahmp), allocatable, dimension(:) :: DZSNSO  ! thickness of snow/soil layers (m)
    real(kind=kind_noahmp), allocatable, dimension(:) :: ZSNSO   ! depth of snow/soil layer-bottom (m)
    real(kind=kind_noahmp), allocatable, dimension(:) :: ZLAYER  ! soil layer thickness (m)

  end type domain_type

!=== define config type that includes namelist & domain subtypes
  type, public :: config_type

    type(namelist_type) :: nmlist ! not using "namelist" to avoid issue with Fortran intrinsic namelist function
    type(domain_type)   :: domain

  end type config_type

end module ConfigVarType
