module WaterVarType

!!! Define column (1-D) Noah-MP Water variables
!!! Water variable initialization is done in WaterVarInitMod.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (July 2022)
! -------------------------------------------------------------------------

  use Machine

  implicit none
  save
  private

!=== define "flux" sub-type of water_type (water%flux%variable)
  type :: flux_type

    ! define specific water flux variables
    real(kind=kind_noahmp) :: RainfallRefHeight          ! liquid rainfall rate [mm/s] at reference height
    real(kind=kind_noahmp) :: SnowfallRefHeight          ! snowfall rate [mm/s] at reference height
    real(kind=kind_noahmp) :: PrecipTotRefHeight         ! total precipitation [mm/s] at reference height
    real(kind=kind_noahmp) :: PrecipConvTotRefHeight     ! total convective precipitation [mm/s] at reference height
    real(kind=kind_noahmp) :: PrecipLargeSclRefHeight    ! large-scale precipitation [mm/s] at reference height
    real(kind=kind_noahmp) :: EvapCanopyNet              ! net evaporation of canopy intercepted total water [mm/s]
    real(kind=kind_noahmp) :: Transpiration              ! transpiration rate [mm/s]
    real(kind=kind_noahmp) :: EvapCanopyLiq              ! canopy liquid water evaporation rate [mm/s]
    real(kind=kind_noahmp) :: DewCanopyLiq               ! canopy water dew rate [mm/s]
    real(kind=kind_noahmp) :: FrostCanopyIce             ! canopy ice frost rate [mm/s]
    real(kind=kind_noahmp) :: SublimCanopyIce            ! canopy ice sublimation rate [mm/s]
    real(kind=kind_noahmp) :: MeltCanopyIce              ! canopy ice melting rate [mm/s]
    real(kind=kind_noahmp) :: RefrzCanopyLiq             ! canopy water refreezing rate [mm/s]
    real(kind=kind_noahmp) :: SnowfallGround             ! snowfall on the ground (below canopy) [mm/s]
    real(kind=kind_noahmp) :: SnowDepthIncr              ! snow depth increasing rate [m/s] due to snowfall
    real(kind=kind_noahmp) :: FrostSnowSfcIce            ! snow surface ice frost rate[mm/s]
    real(kind=kind_noahmp) :: SublimSnowSfcIce           ! snow surface ice sublimation rate[mm/s]
    real(kind=kind_noahmp) :: RainfallGround             ! ground surface rain rate [mm/s]
    real(kind=kind_noahmp) :: SnowBotOutflow             ! total water (snowmelt + rain through pack) out of snowpack bottom [mm/s]
    real(kind=kind_noahmp) :: GlacierExcessFlow          ! glacier excess flow [mm/s]
    real(kind=kind_noahmp) :: IrrigationRateFlood        ! flood irrigation water rate [m/timestep]
    real(kind=kind_noahmp) :: IrrigationRateMicro        ! micro irrigation water rate [m/timestep]
    real(kind=kind_noahmp) :: IrrigationRateSprinkler    ! sprinkler irrigation water rate [m/timestep]
    real(kind=kind_noahmp) :: IrriEvapLossSprinkler      ! loss of irrigation water to evaporation,sprinkler [m/timestep]
    real(kind=kind_noahmp) :: SoilSfcInflow              ! water input on soil surface [mm/s]
    real(kind=kind_noahmp) :: RunoffSurface              ! surface runoff [mm/s]
    real(kind=kind_noahmp) :: RunoffSubsurface           ! subsurface runoff [mm/s]
    real(kind=kind_noahmp) :: InfilRateSfc               ! infiltration rate at surface [mm/s]
    real(kind=kind_noahmp) :: EvapSoilSfcLiq             ! soil surface water evaporation [mm/s]
    real(kind=kind_noahmp) :: DrainSoilBot               ! soil bottom drainage [m/s]
    real(kind=kind_noahmp) :: TileDrain                  ! tile drainage [mm/s]
    real(kind=kind_noahmp) :: RechargeGw                 ! groundwater recharge rate [mm/s]
    real(kind=kind_noahmp) :: DischargeGw                ! groundwater discharge rate [mm/s]
    real(kind=kind_noahmp) :: VaporizeGrd                ! ground vaporize rate total (evap+sublim) [mm/s]
    real(kind=kind_noahmp) :: CondenseVapGrd             ! ground vapor condense rate total (dew+frost) [mm/s]
    real(kind=kind_noahmp) :: DewSoilSfcLiq              ! soil surface water dew rate [mm/s]
    real(kind=kind_noahmp) :: EvapIrriSprinkler          ! evaporation of irrigation water, sprinkler [mm/s]
    real(kind=kind_noahmp) :: InterceptCanopyRain        ! interception rate for rain [mm/s]
    real(kind=kind_noahmp) :: DripCanopyRain             ! drip rate for intercepted rain [mm/s]
    real(kind=kind_noahmp) :: ThroughfallRain            ! throughfall for rain [mm/s]
    real(kind=kind_noahmp) :: InterceptCanopySnow        ! interception (loading) rate for snowfall [mm/s]
    real(kind=kind_noahmp) :: DripCanopySnow             ! drip (unloading) rate for intercepted snow [mm/s]
    real(kind=kind_noahmp) :: ThroughfallSnow            ! throughfall of snowfall [mm/s]
    real(kind=kind_noahmp) :: EvapSoilNet                ! net direct soil evaporation [mm/s]
    real(kind=kind_noahmp) :: MeltGroundSnow             ! ground snow melting rate [mm/s]
    real(kind=kind_noahmp) :: WaterToAtmosTotal          ! total surface water vapor flux to atmosphere [mm/s]

    real(kind=kind_noahmp), allocatable, dimension(:) :: TranspWatLossSoil     ! transpiration water loss from soil layers [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: CompactionSnowAging   ! rate of snow compaction due to destructive metamorphism/aging [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: CompactionSnowBurden  ! rate of snow compaction due to overburden [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: CompactionSnowMelt    ! rate of snow compaction due to melt [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: CompactionSnowTot     ! rate of total snow compaction [fraction/timestep]

  end type flux_type


!=== define "state" sub-type of water_type (water%state%variable)
  type :: state_type

    ! define specific water state variables
    integer                :: IrrigationCntSprinkler     ! irrigation event number, Sprinkler
    integer                :: IrrigationCntMicro         ! irrigation event number, Micro
    integer                :: IrrigationCntFlood         ! irrigation event number, Flood
    real(kind=kind_noahmp) :: CanopyTotalWater           ! total (liquid+ice) canopy intercepted water [mm]
    real(kind=kind_noahmp) :: CanopyWetFrac              ! wetted or snowed fraction of the canopy
    real(kind=kind_noahmp) :: SnowfallDensity            ! bulk density of snowfall (kg/m3)
    real(kind=kind_noahmp) :: CanopyLiqWater             ! intercepted canopy liquid water [mm]
    real(kind=kind_noahmp) :: CanopyIce                  ! intercepted canopy ice [mm]
    real(kind=kind_noahmp) :: CanopyIceMax               ! canopy capacity for snow interception [mm]
    real(kind=kind_noahmp) :: CanopyLiqWaterMax          ! canopy capacity for rain interception [mm]
    real(kind=kind_noahmp) :: SnowDepth                  ! snow depth [m]
    real(kind=kind_noahmp) :: SnowWaterEquiv             ! snow water equivalent (ice+liquid) [mm]
    real(kind=kind_noahmp) :: SnowWaterEquivPrev         ! snow water equivalent at previous time step (mm)
    real(kind=kind_noahmp) :: PondSfcThinSnwMelt         ! surface ponding [mm] from snowmelt when snow has no layer
    real(kind=kind_noahmp) :: PondSfcThinSnwComb         ! surface ponding [mm] from liquid in thin snow layer combination
    real(kind=kind_noahmp) :: PondSfcThinSnwTrans        ! surface ponding [mm] from thin snow liquid during transition from multilayer to no layer
    real(kind=kind_noahmp) :: IrrigationFracFlood        ! fraction of grid under flood irrigation (0 to 1)
    real(kind=kind_noahmp) :: IrrigationAmtFlood         ! flood irrigation water amount [m]
    real(kind=kind_noahmp) :: IrrigationFracMicro        ! fraction of grid under micro irrigation (0 to 1)
    real(kind=kind_noahmp) :: IrrigationAmtMicro         ! micro irrigation water amount [m]
    real(kind=kind_noahmp) :: IrrigationFracSprinkler    ! fraction of grid under sprinkler irrigation (0 to 1)
    real(kind=kind_noahmp) :: IrrigationAmtSprinkler     ! sprinkler irrigation water amount [m]
    real(kind=kind_noahmp) :: WaterTableDepth            ! water table depth [m]
    real(kind=kind_noahmp) :: SoilIceMax                 ! maximum soil ice content [m3/m3]
    real(kind=kind_noahmp) :: SoilLiqWaterMin            ! minimum soil liquid water content [m3/m3]
    real(kind=kind_noahmp) :: SoilSaturateFrac           ! fractional saturated area for soil moisture
    real(kind=kind_noahmp) :: SoilImpervFracMax          ! maximum soil imperviousness fraction
    real(kind=kind_noahmp) :: SoilMoistureToWT           ! soil moisture between bottom of the soil and the water table
    real(kind=kind_noahmp) :: RechargeGwDeepWT           ! groundwater recharge to or from the water table when deep [m]
    real(kind=kind_noahmp) :: RechargeGwShallowWT        ! groundwater recharge to or from shallow water table [m]
    real(kind=kind_noahmp) :: SoilSaturationExcess       ! saturation excess of the total soil [m]
    real(kind=kind_noahmp) :: WaterTableHydro            ! water table depth estimated in WRF-Hydro fine grids [m]
    real(kind=kind_noahmp) :: TileDrainFrac              ! tile drainage map(fraction)
    real(kind=kind_noahmp) :: WaterStorageAquifer        ! water storage in aquifer [mm]
    real(kind=kind_noahmp) :: WaterStorageSoilAqf        ! water storage in aquifer + saturated soil [mm]
    real(kind=kind_noahmp) :: WaterStorageLake           ! water storage in lake (can be negative) [mm] 
    real(kind=kind_noahmp) :: WaterHeadSfc               ! surface water head [mm]
    real(kind=kind_noahmp) :: IrrigationFracGrid         ! total irrigation fraction from input for a grid
    real(kind=kind_noahmp) :: PrecipAreaFrac             ! fraction of the gridcell that receives precipitation
    real(kind=kind_noahmp) :: SnowCoverFrac              ! snow cover fraction (-)
    real(kind=kind_noahmp) :: SoilTranspFacAcc           ! accumulated soil water transpiration factor (0 to 1)
    real(kind=kind_noahmp) :: FrozenPrecipFrac           ! fraction of frozen precip in total precipitation
    real(kind=kind_noahmp) :: SoilWaterRootZone          ! root zone soil water
    real(kind=kind_noahmp) :: SoilWaterStress            ! soil water stress
    real(kind=kind_noahmp) :: WaterStorageTotBeg         ! total water storage [mm] at the begining before NoahMP process
    real(kind=kind_noahmp) :: WaterBalanceError          ! water balance error [mm]
    real(kind=kind_noahmp) :: WaterStorageTotEnd         ! total water storage [mm] at the end of NoahMP process

    integer               , allocatable, dimension(:) :: IndexPhaseChange      ! phase change index [0-none;1-melt;2-refreeze]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowIce               ! snow layer ice [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowLiqWater          ! snow layer liquid water [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowIceFracPrev       ! ice fraction in snow layers at previous timestep
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowIceFrac           ! ice fraction in snow layers at current timestep
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilIceFrac           ! ice fraction in soil layers at current timestep
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowEffPorosity       ! snow effective porosity [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilLiqWater          ! soil liquid moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilIce               ! soil ice moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilMoisture          ! total soil moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilImpervFrac        ! fraction of imperviousness due to frozen soil
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilWatConductivity   ! soil hydraulic/water conductivity [m/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilWatDiffusivity    ! soil water diffusivity [m2/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilEffPorosity       ! soil effective porosity (m3/m3) 
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilMoistureEqui      ! equilibrium soil water  content [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilTranspFac         ! soil water transpiration factor (0 to 1)
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowIceVol            ! partial volume of snow ice [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SnowLiqWaterVol       ! partial volume of snow liquid water [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilSupercoolWater    ! supercooled water in soil [kg/m2]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilMatPotential      ! soil matric potential [m]

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
    real(kind=kind_noahmp) :: FILOSS           ! factor of flood irrigation loss
    real(kind=kind_noahmp) :: SPRIR_RATE       ! sprinkler irrigation rate (mm/h)
    real(kind=kind_noahmp) :: IRR_FRAC         ! irrigation Fraction
    real(kind=kind_noahmp) :: IR_RAIN          ! maximum precipitation to stop irrigation trigger
    real(kind=kind_noahmp) :: SNOWDEN_MIN      ! minimum fresh snowfall density (kg/m3)
    real(kind=kind_noahmp) :: SWEMX            ! new snow mass to fully cover old snow (mm)
    real(kind=kind_noahmp) :: PSIWLT           ! soil metric potential for wilting point (m)
    real(kind=kind_noahmp) :: MFSNO            ! snowmelt m parameter
    real(kind=kind_noahmp) :: SCFFAC           ! snow cover factor (m) (originally hard-coded 2.5*z0 in SCF formulation)

    real(kind=kind_noahmp), allocatable, dimension(:) :: SMCMAX  ! saturated value of soil moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SMCWLT  ! wilting point soil moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SMCREF  ! reference soil moisture (field capacity) (m3/m3)
    real(kind=kind_noahmp), allocatable, dimension(:) :: SMCDRY  ! dry soil moisture threshold (m3/m3)
    real(kind=kind_noahmp), allocatable, dimension(:) :: DWSAT   ! saturated soil hydraulic diffusivity (m2/s)
    real(kind=kind_noahmp), allocatable, dimension(:) :: DKSAT   ! saturated soil hydraulic conductivity [m/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: BEXP    ! soil B parameter
    real(kind=kind_noahmp), allocatable, dimension(:) :: PSISAT  ! saturated soil matric potential (m)
    real(kind=kind_noahmp), allocatable, dimension(:) :: QUARTZ  !soil quartz content

  end type parameter_type


!=== define water type that includes 3 subtypes (flux,state,parameter)
  type, public :: water_type

    type(flux_type)      :: flux
    type(state_type)     :: state
    type(parameter_type) :: param

  end type water_type

end module WaterVarType
