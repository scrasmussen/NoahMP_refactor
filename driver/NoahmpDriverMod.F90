Program NoahmpDriverMod

  use Machine, only : kind_noahmp
  use ConstantDefineMod
  use NoahmpVarType
  use InputVarType
  use InputVarInitMod
  use ConfigVarInitMod
  use EnergyVarInitMod
  use ForcingVarInitMod
  use WaterVarInitMod
  use BiochemVarInitMod
  use NoahmpOutputMod
  use NoahmpMainMod

  implicit none
!---------------------------------------------------------------------
!  types
!---------------------------------------------------------------------
  type(input_type)    :: input
  type(noahmp_type)   :: noahmp

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------

  integer                :: itime, iz, isoil   ! some loop counters
  integer                :: ntime      = 0     ! number of timesteps to run
  integer                :: rain_steps = 0     ! number of timesteps in rain event
  integer                :: dry_steps  = 0     ! number of timesteps between rain events
  integer                :: rain_step  = 0     ! number of timesteps in current event
  integer                :: dry_step   = 0     ! number of timesteps in current event
  real(kind=kind_noahmp) :: totalwat   = 0.0   ! total soil water [mm]
  real(kind=kind_noahmp) :: tw0        = 0.0   ! initial total soil water [mm]
  real(kind=kind_noahmp) :: errwat     = 0.0   ! water balance error at each timestep [mm]
  logical                :: raining            ! .true. if raining
  logical                :: VEG                ! true if covered by vegetation
  real(kind=kind_noahmp) :: ERRENG     = 0.0   ! error in surface energy balance [w/m2]
  real(kind=kind_noahmp) :: ERRSW      = 0.0   ! error in shortwave radiation balance [w/m2]

!---------------------------------------------------------------------
!  read in input data from table and initial file
!---------------------------------------------------------------------
  call InputVarInitDefault(input)
  call ReadNamelist(input)
  call ReadNoahmpTable(input)

!---------------------------------------------------------------------
!  initialize
!---------------------------------------------------------------------
  call ConfigVarInitDefault(noahmp)
  call ConfigVarInitTransfer(noahmp, input)
  call ForcingVarInitDefault(noahmp)
  call ForcingVarInitTransfer(noahmp, input)
  call EnergyVarInitDefault(noahmp)
  call EnergyVarInitTransfer(noahmp, input)
  call WaterVarInitDefault(noahmp)
  call WaterVarInitTransfer(noahmp, input)
  call BiochemVarInitDefault(noahmp)
  call BiochemVarInitTransfer(noahmp, input)

!---------------------------------------------------------------------

!---------------------------------------------------------------------
  associate(                                                        &
            DT              => noahmp%config%domain%DT             ,& ! in,     main noahmp timestep (s)
            NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
            ZSNSO           => noahmp%config%domain%ZSNSO          ,& ! inout,  depth of snow/soil layer-bottom (m)
            SFCTMP          => noahmp%forcing%SFCTMP               ,& ! in,     surface air temperature [k] from Atmos forcing
            TV              => noahmp%energy%state%TV              ,& ! inout,  vegetation temperature (k)
            TG              => noahmp%energy%state%TG              ,& ! in,     ground temperature (k)
            IMELT           => noahmp%water%state%IMELT            ,& ! in,     phase change index [0-none;1-melt;2-refreeze]
            CANLIQ          => noahmp%water%state%CANLIQ           ,& ! inout,  intercepted liquid water (mm)
            CANICE          => noahmp%water%state%CANICE           ,& ! inout,  intercepted ice mass (mm)
            STC             => noahmp%energy%state%STC             ,& ! inout,  snow and soil layer temperature [k]
            SH2O            => noahmp%water%state%SH2O             ,& ! inout,  soil water content [m3/m3]
            SICE            => noahmp%water%state%SICE             ,& ! inout,  soil ice moisture (m3/m3)
            IST             => noahmp%config%domain%IST            ,& ! in,     surface type 1-soil; 2-lake 
            DX              => noahmp%config%domain%DX             ,& ! in,     noahmp model grid spacing (m)
            PONDING         => noahmp%water%state%PONDING          ,& ! inout,  melting water from snow when there is no layer (mm)
            FICEOLD         => noahmp%water%state%FICEOLD_SNOW     ,& ! in,     ice fraction in snow layers at last timestep
            FVEG            => noahmp%energy%state%FVEG            ,& ! in,     greeness vegetation fraction (-)
            SMCEQ           => noahmp%water%state%SMCEQ            ,& ! in,     equilibrium soil water  content [m3/m3]
            BDFALL          => noahmp%water%state%BDFALL           ,& ! in,     bulk density of snowfall (kg/m3)
            QRAIN           => noahmp%water%flux%QRAIN             ,& ! in,     snow surface rain rate[mm/s]
            QSNOW           => noahmp%water%flux%QSNOW             ,& ! in,     snow at ground srf (mm/s) [+]
            SNOWHIN         => noahmp%water%flux%SNOWHIN           ,& ! in,     snow depth increasing rate (m/s)
            ISNOW           => noahmp%config%domain%ISNOW          ,& ! inout,  actual number of snow layers
            SNOWH           => noahmp%water%state%SNOWH            ,& ! inout,  snow depth [m]
            SNEQV           => noahmp%water%state%SNEQV            ,& ! inout,  snow water equivalent [mm]
            SNICE           => noahmp%water%state%SNICE            ,& ! inout,  snow layer ice [mm]
            SNLIQ           => noahmp%water%state%SNLIQ            ,& ! inout,  snow layer liquid water [mm]
            SMC             => noahmp%water%state%SMC              ,& ! inout,  total soil moisture [m3/m3]
            ZWT             => noahmp%water%state%ZWT              ,& ! inout,  water table depth [m]
            WA              => noahmp%water%state%WA               ,& ! inout,  water storage in aquifer [mm]
            WT              => noahmp%water%state%WT               ,& ! inout,  water storage in aquifer + saturated soil [mm]
            WSLAKE          => noahmp%water%state%WSLAKE           ,& ! inout,  water storage in lake (can be -) (mm)
            SMCWTD          => noahmp%water%state%SMCWTD           ,& ! inout,  soil moisture between bottom of the soil and the water table
            DEEPRECH        => noahmp%water%state%DEEPRECH         ,& ! inout,  recharge to or from the water table when deep [m]
            RECH            => noahmp%water%state%RECH             ,& ! out,    groundwater recharge (net vertical flux across the water table), positive up
            CMC             => noahmp%water%state%CMC              ,& ! out,    total canopy intercepted water (mm)
            ECAN            => noahmp%water%flux%ECAN              ,& ! out,    evaporation of intercepted water (mm/s) [+]
            ETRAN           => noahmp%water%flux%ETRAN             ,& ! out,    transpiration rate (mm/s) [+]
            FWET            => noahmp%water%state%FWET             ,& ! out,    wetted or snowed fraction of the canopy
            RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! out,    surface runoff [mm/s]
            RUNSUB          => noahmp%water%flux%RUNSUB            ,& ! out,    subsurface runoff [mm/s] 
            QDIS            => noahmp%water%flux%QDIS              ,& ! out,    groundwater discharge [mm/s]
            QIN             => noahmp%water%flux%QIN               ,& ! out,    groundwater recharge [mm/s] 
            PONDING1        => noahmp%water%state%PONDING1         ,& ! out,    surface ponding 1 (mm)
            PONDING2        => noahmp%water%state%PONDING2         ,& ! out,    surface ponding 2 (mm)
            QSNBOT          => noahmp%water%flux%QSNBOT            ,& ! out,    melting water out of snow bottom [mm/s]
            QTLDRN          => noahmp%water%flux%QTLDRN            ,& ! inout,  tile drainage (mm/s)
            QINSUR          => noahmp%water%flux%QINSUR            ,& ! in,     water input on soil surface [mm/s]
            QSEVA           => noahmp%water%flux%QSEVA             ,& ! in,     evaporation from soil surface [mm/s]
            ETRANI          => noahmp%water%flux%ETRANI            ,& ! in,     evapotranspiration from soil layers [mm/s]
            QSNFRO          => noahmp%water%flux%QSNFRO            ,& ! in,     snow surface frost rate[mm/s]
            QSNSUB          => noahmp%water%flux%QSNSUB            ,& ! in,     snow surface sublimation rate[mm/s]
            SNOFLOW         => noahmp%water%flux%SNOFLOW           ,& ! out,    glacier flow [mm/s]
            QSDEW           => noahmp%water%flux%QSDEW             ,& ! inout,  soil surface dew rate [mm/s]
            QDRAIN          => noahmp%water%flux%QDRAIN            ,& ! in,     soil bottom drainage (m/s)
            FCRMAX          => noahmp%water%state%FCRMAX           ,& ! in,     maximum fraction of imperviousness (FCR)
            WCND            => noahmp%water%state%WCND             ,& ! out,    soil hydraulic conductivity (m/s)
            sfcheadrt       => noahmp%water%state%sfcheadrt        ,& ! inout,  surface water head (mm) 
            WATBLED         => noahmp%water%state%WATBLED          ,& ! in,     water table depth estimated in WRF-Hydro fine grids (m)
            FROZEN_CANOPY   => noahmp%energy%state%FROZEN_CANOPY   ,& ! in,     used to define latent heat pathway
            FROZEN_GROUND   => noahmp%energy%state%FROZEN_GROUND   ,& ! in,     frozen ground (logical) to define latent heat pathway
            QVAP            => noahmp%water%flux%QVAP              ,& ! in,     soil surface evaporation rate[mm/s]
            QDEW            => noahmp%water%flux%QDEW              ,& ! in,     soil surface dew rate[mm/s]
            BTRANI          => noahmp%water%state%BTRANI           ,& ! in,     soil water transpiration factor (0 to 1)
            OPT_IRR         => noahmp%config%nmlist%OPT_IRR        ,& ! in,     options for irrigation
            OPT_IRRM        => noahmp%config%nmlist%OPT_IRRM       ,& ! in,     options for irrigation method
            CROPLU          => noahmp%config%domain%CROPLU         ,& ! in,     flag to identify croplands
            IRRFRA          => noahmp%water%state%IRRFRA           ,& ! in,     irrigation fraction
            SIFAC           => noahmp%water%state%SIFAC            ,& ! in,     sprinkler irrigation fraction (0 to 1)
            MIFAC           => noahmp%water%state%MIFAC            ,& ! in,     fraction of grid under micro irrigation (0 to 1)
            FIFAC           => noahmp%water%state%FIFAC            ,& ! in,     fraction of grid under flood irrigation (0 to 1)
            IRAMTFI         => noahmp%water%state%IRAMTFI          ,& ! inout,  flood irrigation water amount [m]
            IRAMTMI         => noahmp%water%state%IRAMTMI          ,& ! inout,  micro irrigation water amount [m]
            IRFIRATE        => noahmp%water%flux%IRFIRATE          ,& ! inout,  flood irrigation water rate [m/timestep]
            IRMIRATE        => noahmp%water%flux%IRMIRATE          ,& ! inout,  micro irrigation water rate [m/timestep]
            OPT_TDRN        => noahmp%config%nmlist%OPT_TDRN       ,& ! in,     options for tile drainage
            TDFRACMP        => noahmp%water%state%TDFRACMP         ,& ! in,     tile drainage map(fraction)
            SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
            DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! in,     thickness of snow/soil layers (m)
            LAIM            => noahmp%energy%param%LAIM            ,& ! in,     monthly LAI from table
            SAIM            => noahmp%energy%param%SAIM            ,& ! in,     monthly SAI from table
            ELAI            => noahmp%energy%state%ELAI            ,& ! out,    leaf area index, after burying by snow
            ESAI            => noahmp%energy%state%ESAI            ,& ! out,    stem area index, after burying by snow
            Q2              => noahmp%forcing%Q2                   ,& ! in,     specific humidity kg/kg
            IRAMTSI         => noahmp%water%state%IRAMTSI          ,& ! inout,  irrigation water amount [m] to be applied, Sprinkler
            IRSIRATE        => noahmp%water%flux%IRSIRATE          ,& ! inout,  rate of irrigation by sprinkler [m/timestep]
            IRCNTSI         => noahmp%water%state%IRCNTSI          ,& ! inout,  irrigation event number, Sprinkler
            IRCNTMI         => noahmp%water%state%IRCNTMI          ,& ! inout,  irrigation event number, Micro
            IRCNTFI         => noahmp%water%state%IRCNTFI          ,& ! inout,  irrigation event number, Flood
            EAIR            => noahmp%energy%state%EAIR            ,& ! in,     vapor pressure air (pa)
            IREVPLOS        => noahmp%water%flux%IREVPLOS          ,& ! inout,  loss of irrigation water to evaporation,sprinkler [m/timestep]
            FIRR            => noahmp%energy%flux%FIRR             ,& ! inout,  latent heating due to sprinkler evaporation [w/m2]
            EIRR            => noahmp%water%flux%EIRR              ,& ! inout,  evaporation of irrigation water to evaporation,sprinkler [mm/s]
            SFCPRS          => noahmp%forcing%SFCPRS               ,& ! in,     surface pressure (pa)
            IRR_FRAC        => noahmp%water%param%IRR_FRAC         ,& ! in,     irrigation fraction parameter
            RAIN            => noahmp%water%flux%RAIN              ,& ! inout,  rainfall rate
            SNOW            => noahmp%water%flux%SNOW              ,& ! inout,  snowfall rate
            IR_RAIN         => noahmp%water%param%IR_RAIN          ,& ! in,     maximum precipitation to stop irrigation trigger
            QINTR           => noahmp%water%flux%QINTR             ,& ! out,    interception rate for rain (mm/s)
            QDRIPR          => noahmp%water%flux%QDRIPR            ,& ! out,    drip rate for rain (mm/s)
            QTHROR          => noahmp%water%flux%QTHROR            ,& ! out,    throughfall for rain (mm/s)
            QINTS           => noahmp%water%flux%QINTS             ,& ! out,    interception (loading) rate for snowfall (mm/s)
            QDRIPS          => noahmp%water%flux%QDRIPS            ,& ! out,    drip (unloading) rate for intercepted snow (mm/s)
            QTHROS          => noahmp%water%flux%QTHROS            ,& ! out,    throughfall of snowfall (mm/s)
            PAHV            => noahmp%energy%flux%PAHV             ,& ! out,    precipitation advected heat - vegetation net (W/m2)
            PAHG            => noahmp%energy%flux%PAHG             ,& ! out,    precipitation advected heat - under canopy net (W/m2)
            PAHB            => noahmp%energy%flux%PAHB             ,& ! out,    precipitation advected heat - bare ground net (W/m2)
            EDIR            => noahmp%water%flux%EDIR              ,& ! out,    net soil evaporation
            FP              => noahmp%water%state%FP               ,& ! out,    precipitation area fraction
            DF              => noahmp%energy%state%DF              ,& ! out,    thermal conductivity [w/m/k] for all soil & snow
            HCPCT           => noahmp%energy%state%HCPCT           ,& ! out,    heat capacity [j/m3/k] for all soil & snow
            FACT            => noahmp%energy%state%FACT            ,& ! out,    energy factor for soil & snow phase change
            SNICEV          => noahmp%water%state%SNICEV           ,& ! out,    partial volume of ice [m3/m3]
            SNLIQV          => noahmp%water%state%SNLIQV           ,& ! out,    partial volume of liquid water [m3/m3]
            EPORE_SNOW      => noahmp%water%state%EPORE_SNOW2      ,& ! out,    snow effective porosity (m3/m3) used in snow heat capacity
            SNEQVO          => noahmp%water%state%SNEQVO           ,& ! in,     snow mass at last time step(mm)
            COSZ            => noahmp%config%domain%COSZ           ,& ! in,     cosine solar zenith angle
            FSNO            => noahmp%water%state%FSNO             ,& ! in,     snow cover fraction (-)
            SOLAD           => noahmp%energy%flux%SOLAD            ,& ! in,     incoming direct solar radiation (w/m2)
            SOLAI           => noahmp%energy%flux%SOLAI            ,& ! in,     incoming diffuse solar radiation (w/m2)
            ALBOLD          => noahmp%energy%state%ALBOLD          ,& ! in,     snow albedo at last time step
            TAUSS           => noahmp%energy%state%TAUSS           ,& ! inout,  non-dimensional snow age
            FSUN            => noahmp%energy%state%FSUN            ,& ! in,     sunlit fraction of canopy
            LAISUN          => noahmp%energy%state%LAISUN          ,& ! in,     sunlit leaf area
            LAISHA          => noahmp%energy%state%LAISHA          ,& ! in,     shaded leaf area
            PARSUN          => noahmp%energy%flux%PARSUN           ,& ! out,    average absorbed par for sunlit leaves (w/m2)
            PARSHA          => noahmp%energy%flux%PARSHA           ,& ! out,    average absorbed par for shaded leaves (w/m2)
            SAV             => noahmp%energy%flux%SAV              ,& ! out,    solar radiation absorbed by vegetation (w/m2)
            SAG             => noahmp%energy%flux%SAG              ,& ! out,    solar radiation absorbed by ground (w/m2)
            FSA             => noahmp%energy%flux%FSA              ,& ! out,    total absorbed solar radiation (w/m2)
            FSR             => noahmp%energy%flux%FSR              ,& ! out,    total reflected solar radiation (w/m2)
            FSRV            => noahmp%energy%flux%FSRV             ,& ! out,    reflected solar radiation by vegetation (w/m2)
            FSRG            => noahmp%energy%flux%FSRG             ,& ! out,    reflected solar radiation by ground (w/m2)
            BGAP            => noahmp%energy%state%BGAP            ,& ! out,    between canopy gap fraction for beam
            WGAP            => noahmp%energy%state%WGAP            ,& ! out,    within canopy gap fraction for beam
            ALBSND          => noahmp%energy%state%ALBSND          ,& ! out,    snow albedo for direct(1=vis, 2=nir)
            ALBSNI          => noahmp%energy%state%ALBSNI          ,& ! out,    snow albedo for diffuse(1=vis, 2=nir)
            SWDOWN          => noahmp%energy%flux%SWDOWN           ,& ! in,     downward surface radiation
            CM              => noahmp%energy%state%CM              ,& ! inout,  momentum exchange coefficient (m/s), above ZPD, vegetated
            CH              => noahmp%energy%state%CH              ,& ! inout,  heat exchange coefficient (m/s), above ZPD, vegetated
            LWDN            => noahmp%forcing%LWDN                 ,& ! in,     downward longwave radiation [w/m2]
            UU              => noahmp%forcing%UU                   ,& ! in,     u direction wind
            VV              => noahmp%forcing%VV                   ,& ! in,     v direction wind
            UR              => noahmp%energy%state%UR              ,& ! in,     wind speed (m/s) at reference height ZLVL
            THAIR           => noahmp%energy%state%THAIR           ,& ! in,     potential temp at reference height (k)           
            QAIR            => noahmp%energy%state%QAIR            ,& ! in,     specific humidity at reference height (kg/kg)
            LAI             => noahmp%energy%state%LAI             ,& ! in,     leaf area index (m2/m2)
            SAI             => noahmp%energy%state%SAI             ,& ! inout,  stem area index (m2/m2)
            FB_snow         => noahmp%energy%state%FB_snow         ,& ! out,    fraction of canopy buried by snow
            RHOAIR          => noahmp%energy%state%RHOAIR          ,& ! in,     density air (kg/m3)
            Z0MG            => noahmp%energy%state%Z0MG            ,& ! out,    roughness length, momentum, ground (m)
            Z0SNO           => noahmp%energy%param%Z0SNO           ,& ! out,    snow surface roughness length (m) (0.002)
            VAI             => noahmp%energy%state%VAI             ,& ! in,     one-sided leaf+stem area index (m2/m2)
            LATHEAV         => noahmp%energy%state%LATHEAV         ,& ! out,   latent heat of vaporization/subli (j/kg), canopy
            GAMMAV          => noahmp%energy%state%GAMMAV          ,& ! out,   psychrometric constant (pa/K), canopy
            LATHEAG         => noahmp%energy%state%LATHEAG         ,& ! out,   latent heat of vaporization/subli (j/kg), ground
            GAMMAG          => noahmp%energy%state%GAMMAG          ,& ! out,   psychrometric constant (pa/K), ground
            Z0M             => noahmp%energy%state%Z0M             ,& ! in,    roughness length, momentum, (m), surface
            Z0MVT           => noahmp%energy%param%Z0MVT           ,& ! in,    momentum roughness length (m)
            ZPD             => noahmp%energy%state%ZPD             ,& ! in,    zero plane displacement (m)
            HVT             => noahmp%energy%param%HVT             ,& ! in,    top of canopy (m)
            ZPDG            => noahmp%energy%state%ZPD             ,& ! out,   ground zero plane displacement (m)
            ZLVL            => noahmp%energy%state%ZLVL            ,& ! in,    reference height  (m)
            EMV             => noahmp%energy%state%EMV             ,& ! out,   vegetation emissivity
            EMG             => noahmp%energy%state%EMG             ,& ! out,   ground emissivity
            EG              => noahmp%energy%param%EG              ,& ! in,    emissivity soil surface
            SNOW_EMIS       => noahmp%energy%param%SNOW_EMIS       ,& ! in,    snow emissivity
            RSURF           => noahmp%energy%state%RSURF           ,& ! out,   ground surface resistance (s/m)
            IGS             => noahmp%biochem%state%IGS            ,& ! in,    growing season index (0=off, 1=on)
            FOLN            => noahmp%biochem%state%FOLN           ,& ! in,    foliage nitrogen concentration (%)
            CO2AIR          => noahmp%energy%state%CO2AIR          ,& ! in,    atmospheric co2 concentration (pa)
            O2AIR           => noahmp%energy%state%O2AIR           ,& ! in,    atmospheric o2 concentration (pa)
            BTRAN           => noahmp%water%state%BTRAN            ,& ! in,    soil water transpiration factor (0 to 1)
            PSISAT          => noahmp%water%param%PSISAT           ,& ! in,     saturated soil matric potential (m)
            PSI             => noahmp%water%state%PSI              ,& ! out,   surface layer soil matrix potential (m)
            BEXP            => noahmp%water%param%BEXP             ,& ! in,    soil B parameter
            RHSUR           => noahmp%energy%state%RHSUR           ,& ! out,   raltive humidity in surface soil/snow air space (-)
            DZ8W            => noahmp%config%domain%DZ8W           ,& ! in,    thickness of surface atmospheric layers [m]
            PSFC            => noahmp%forcing%PSFC                 ,& ! in,    pressure at lowest model layer
            TAH             => noahmp%energy%state%TAH             ,& ! in,    canopy air temperature (K)
            EAH             => noahmp%energy%state%EAH             ,& ! in,    canopy air vapor pressure (pa)
            TGV             => noahmp%energy%state%TGV             ,& ! out,   vegetated ground (below-canopy) temperature (K)
            CMV             => noahmp%energy%state%CMV             ,& ! out,   drag coefficient for momentum, above ZPD, vegetated
            CHV             => noahmp%energy%state%CHV             ,& ! out,   drag coefficient for heat, above ZPD, vegetated
            QSFC            => noahmp%energy%state%QSFC            ,& ! inout, water vapor mixing ratio at lowest model layer
            RSSUN           => noahmp%energy%state%RSSUN           ,& ! out,   sunlit leaf stomatal resistance (s/m)
            RSSHA           => noahmp%energy%state%RSSHA           ,& ! out,   shaded leaf stomatal resistance (s/m)
            TAUXV           => noahmp%energy%state%TAUXV           ,& ! out,   wind stress: east-west (n/m2) above canopy
            TAUYV           => noahmp%energy%state%TAUYV           ,& ! out,   wind stress: north-south (n/m2) above canopy
            IRG             => noahmp%energy%flux%IRG              ,& ! out,   ground net longwave radiation (w/m2) [+= to atm]
            IRC             => noahmp%energy%flux%IRC              ,& ! out,   canopy net longwave radiation (w/m2) [+= to atm]
            SHG             => noahmp%energy%flux%SHG              ,& ! out,   ground sensible heat flux (w/m2)     [+= to atm]
            SHC             => noahmp%energy%flux%SHC              ,& ! out,   canopy sensible heat flux (w/m2)     [+= to atm]
            EVG             => noahmp%energy%flux%EVG              ,& ! out,   ground evaporation heat flux (w/m2)  [+= to atm]
            EVC             => noahmp%energy%flux%EVC              ,& ! out,   canopy evaporation heat flux (w/m2)  [+= to atm]
            TR              => noahmp%energy%flux%TR               ,& ! out,   canopy transpiration heat flux (w/m2)[+= to atm]
            GHV             => noahmp%energy%flux%GHV              ,& ! out,   vegetated ground heat (w/m2) [+ = to soil]
            T2MV            => noahmp%energy%state%T2MV            ,& ! out,   2 m height air temperature (k), vegetated
            PSNSUN          => noahmp%biochem%flux%PSNSUN          ,& ! out,   sunlit leaf photosynthesis (umol co2 /m2 /s)
            PSNSHA          => noahmp%biochem%flux%PSNSHA          ,& ! out,   shaded leaf photosynthesis (umol co2 /m2 /s)
            Q2V             => noahmp%energy%state%Q2V             ,& ! out,   water vapor mixing ratio at 2m vegetated
            CHV2            => noahmp%energy%state%CHV2            ,& ! out,   2m sensible heat exchange coefficient (m/s)
            CHLEAF          => noahmp%energy%state%CHLEAF          ,& ! out,   leaf sensible heat exchange coefficient (m/s),leaf surface to canopy air
            CHUC            => noahmp%energy%state%CHUC            ,& ! out,   under canopy sensible heat exchange coefficient (m/s)
            TGB             => noahmp%energy%state%TGB             ,& ! out,   bare ground temperature (K)
            CMB             => noahmp%energy%state%CMB             ,& ! out,   drag coefficient for momentum, above ZPD, bare ground
            CHB             => noahmp%energy%state%CHB             ,& ! out,   drag coefficient for heat, above ZPD, bare ground
            TAUXB           => noahmp%energy%state%TAUXB           ,& ! out,   wind stress: east-west (n/m2) bare ground
            TAUYB           => noahmp%energy%state%TAUYB           ,& ! out,   wind stress: north-south (n/m2) bare ground
            IRB             => noahmp%energy%flux%IRB              ,& ! out,   net longwave rad (w/m2) bare ground [+ to atm]
            SHB             => noahmp%energy%flux%SHB              ,& ! out,   sensible heat flux (w/m2) bare ground [+ to atm]
            EVB             => noahmp%energy%flux%EVB              ,& ! out,   latent heat flux (w/m2) bare ground [+ to atm]
            GHB             => noahmp%energy%flux%GHB              ,& ! out,   bare ground heat flux (w/m2) [+ to soil]
            T2MB            => noahmp%energy%state%T2MB            ,& ! out,   2 m height air temperature (k) bare ground
            Q2B             => noahmp%energy%state%Q2B             ,& ! out,   bare ground 2-m water vapor mixing ratio
            EHB2            => noahmp%energy%state%EHB2            ,& ! out,   bare ground 2-m sensible heat exchange coefficient (m/s)
            SSOIL           => noahmp%energy%flux%SSOIL            ,& ! out,   soil heat flux (w/m2) [+ to soil]
            TBOT            => noahmp%forcing%TBOT                 ,& ! in,    bottom soil temp. at ZBOT (K)
            QMELT           => noahmp%water%flux%QMELT             ,& ! out,   snowmelt rate [mm/s]
            ICE             => noahmp%config%domain%ICE            ,& ! in,    flag for ice point
            Z0WRF           => noahmp%energy%state%Z0WRF           ,& ! out,   roughness length, momentum, surface, sent to coupled model
            T2M             => noahmp%energy%state%T2M             ,& ! out,   grid mean 2-m air temperature (K)
            TAUX            => noahmp%energy%state%TAUX            ,& ! out,   wind stress: east-west (n/m2) grid mean
            TAUY            => noahmp%energy%state%TAUY            ,& ! out,   wind stress: north-south (n/m2) grid mean
            FIRA            => noahmp%energy%flux%FIRA             ,& ! out,   total net LW. rad (w/m2)   [+ to atm]
            FSH             => noahmp%energy%flux%FSH              ,& ! out,   total sensible heat (w/m2) [+ to atm]
            TRAD            => noahmp%energy%state%TRAD            ,& ! out,   radiative temperature (K)
            PSN             => noahmp%biochem%flux%PSN             ,& ! out,   total leaf photosynthesis (umol co2 /m2 /s)
            APAR            => noahmp%energy%flux%APAR             ,& ! out,   total photosyn. active energy (w/m2)
            TS              => noahmp%energy%state%TS              ,& ! inout, surface temperature (K)
            Q2E             => noahmp%energy%state%Q2E             ,& ! out,   grid mean 2-m water vapor mixing ratio
            EMISSI          => noahmp%energy%state%EMISSI          ,& ! out,   surface emissivity
            PAH             => noahmp%energy%flux%PAH              ,& ! out,   precipitation advected heat - total (W/m2)
            Q1              => noahmp%energy%state%Q1               & ! inout, surface layer water vapor mixing ratio
           )
!---------------------------------------------------------------------

! start with a default value at time 0

! input used to adjust for snow and non-snow cases
  if ( input%runsnow .eqv. .true. ) then
     SFCTMP    = 265.0 
     FB_snow   = 0.5
     TV        = 268.0
     TG        = 270.0
     !IMELT     = 2  
     CANLIQ    = 0.0
     CANICE    = 0.0
     STC(1:4)  = 265.0
     STC(-2:0) = 0.0
     SH2O(1:4) = 0.03
     SICE(1:4) = 0.2
     FSNO      = 0.8
  else
     SFCTMP    = 298.0
     FB_snow   = 0.0
     TV        = 293.0
     TG        = 285.0
     !IMELT     = 1
     CANLIQ    = 0.0
     CANICE    = 0.0
     STC(1:4)  = 298.0
     STC(-2:0) = 0.0
     SH2O(1:4) = 0.2
     SICE(1:4) = 0.03
     FSNO      = 0.0
  end if

! others
  IST     = 1                     ! surface type 1-soil; 2-lake
  DX      = 4000.0                ! grid spacing 4km
  LAI     = LAIM(6)               ! June LAI as an example
  SAI     = SAIM(6)               ! June SAI
  ELAI    = LAI * (1.0 - FB_snow) ! leaf area index, after burying by snow
  ESAI    = SAI * (1.0 - FB_snow) ! stem area index, after burying by snow 
  PONDING = 0.0
  FICEOLD = 0.0
  FVEG    = input%SHDMAXIn / 100.0  ! yearly max vegetation fraction
  if ( FVEG <= 0.05 ) FVEG = 0.05
  SMCEQ(1:4) = 0.3                ! used only for MMF, so set to fixed value
  BDFALL     = 120.0              ! bulk density of snowfall (kg/m3)
  FP         = 0.9 
  RAIN       = 0.0                ! total rain
  SNOW       = 0.0                ! total snowfall (mm/s)
  QRAIN      = RAIN * 0.99
  QSNOW      = SNOW * 0.9
  SNOWHIN    = QSNOW / BDFALL     ! m/s
  ISNOW      = 0
  SNOWH      = 0.0
  SNEQV      = 0.0
  SNICE      = 0.0
  SNLIQ      = 0.0
  SMC        = SH2O + SICE        ! initial volumetric soil water
  ZWT        = 2.5
  WA         = 0.0
  WT         = 0.0
  WSLAKE     = 0.0
  SMCWTD     = 0.3                ! should only be needed for run=5
  DEEPRECH   = 0.0                ! should only be needed for run=5
  RECH       = 0.0                ! should only be needed for run=5
  CMC        = 0.0
  ECAN       = 0.0
  ETRAN      = 0.0
  FWET       = 0.0  
  RUNSRF     = 0.0
  RUNSUB     = 0.0
  QDIS       = 0.0
  QIN        = 0.0
  PONDING1   = 0.0
  PONDING2   = 0.0
  QSNBOT     = 0.0
  QTLDRN     = 0.0
  QINSUR     = 0.0
  QSEVA      = 0.0
  ETRANI     = 0.0
  QSNFRO     = 0.0
  QSNSUB     = 0.0
  SNOFLOW    = 0.0
  QSDEW      = 0.0
  QDRAIN     = 0.0
  FCRMAX     = 0.0
  WCND       = 0.0
  sfcheadrt  = 0.0
  WATBLED    = 0.0
! canopy water and heat vars
  QINTR      = 0.0
  QDRIPR     = 0.0
  QTHROR     = 0.0
  QINTS      = 0.0
  QDRIPS     = 0.0
  QTHROS     = 0.0
  PAHV       = 0.0
  PAHG       = 0.0
  PAHB       = 0.0
! thermoprop new vars
  DF         = 0.0
  HCPCT      = 0.0
  SNICEV     = 0.0
  SNLIQV     = 0.0
  EPORE_SNOW = 0.0
  FACT       = 0.0
! radiation new vars
  COSZ     = 0.5
  SOLAD(1) = SWDOWN*0.7*0.5     ! direct  vis
  SOLAD(2) = SWDOWN*0.7*0.5     ! direct  nir
  SOLAI(1) = SWDOWN*0.3*0.5     ! diffuse vis
  SOLAI(2) = SWDOWN*0.3*0.5     ! diffuse nir
  SNEQVO   = 0.0
  ALBOLD   = 0.0
  TAUSS    = 0.0
  FSUN     = 0.0
  LAISUN   = 0.0
  LAISHA   = 0.0
  PARSUN   = 0.0
  PARSHA   = 0.0
  SAV      = 0.0
  SAG      = 0.0
  FSA      = 0.0
  FSR      = 0.0
  FSRV     = 0.0
  FSRG     = 0.0
  BGAP     = 0.0
  WGAP     = 0.0
  ALBSND(:)= 0.0
  ALBSNI(:)= 0.0
! Energy main new vars
  ICE = 0


!====== vege_flux new vars
!!! in 
  CM     = 0.1
  CH     = 0.01
  LWDN   = input%LWDNIn
  UR     = max( sqrt(UU**2.0 + VV**2.0), 1.0 )
  THAIR  = SFCTMP * (SFCPRS / SFCPRS)**(RAIR / CPAIR)
  QAIR   = Q2
  EAIR   = QAIR * SFCPRS / (0.622 + 0.378*QAIR)
  RHOAIR = (SFCPRS - 0.378*EAIR) / (RAIR*SFCTMP)
  Z0MG   = 0.002 * (1.0 - FSNO) + FSNO * Z0SNO
! needs to update each time step
  VAI    = ELAI + ESAI
  VEG    = .false.
  if ( VAI > 0.0 ) VEG = .true.
  if ( TV > TFRZ ) then
     LATHEAV       = HVAP
     FROZEN_CANOPY = .false.
  else
     LATHEAV       = HSUB
     FROZEN_CANOPY = .true.
  endif
  GAMMAV = CPAIR * SFCPRS / (0.622 * LATHEAV)
  if ( TG > TFRZ ) then
      LATHEAG       = HVAP
      FROZEN_GROUND = .false.
  else
      LATHEAG       = HSUB
      frozen_ground = .true.
  endif
   GAMMAG = CPAIR * SFCPRS / (0.622 * LATHEAG)
   ZPDG  = SNOWH
   if ( VEG .eqv. .true. ) then
      Z0M = Z0MVT
      ZPD = 0.65 * HVT
      if ( SNOWH > ZPD ) ZPD = SNOWH
   else
      Z0M = Z0MG
      ZPD = ZPDG
   endif
   !ZLVL   = max( ZPD, HVT ) + 10.0
   !IF(ZPDG >= ZLVL) ZLVL = ZPDG + 10.0
   EMV    = 1.0 - exp(-(ELAI+ESAI)/1.0)
   EMG    = EG(IST) * (1.0-FSNO) + SNOW_EMIS*FSNO
   RSURF  = FSNO * 1.0 + (1.0-FSNO)* exp(8.25-4.225*(max(0.0,SH2O(1)/SMCMAX(1)))) !Sellers (1992)
   IGS    = 1.0
   FOLN   = 1.0
   CO2AIR = 395.0e-06 * SFCPRS
   O2AIR  = 0.209 * SFCPRS
   BTRAN  = 0.2
   PSI(1) = -PSISAT(1)*(max(0.01,SH2O(1))/SMCMAX(1))**(-BEXP(1))
   RHSUR  = FSNO + (1.0-FSNO) * EXP(PSI(1)*GRAV/(RW*TG))
   DZ8W   = 20.0
   PSFC   = SFCPRS
!!! inout
   ! TV
   TAH    = TV
   EAH    = Q2 * SFCPRS / (0.622 + 0.378*Q2)
   TGV    = TG
   CMV    = CM
   CHV    = CH
   QSFC   = 0.622 * EAIR / (PSFC - 0.378*EAIR)
!!! out
   RSSUN  = 0.0
   RSSHA  = 0.0
   TAUXV  = 0.0
   TAUYV  = 0.0
   IRG    = 0.0
   IRC    = 0.0
   SHG    = 0.0
   SHC    = 0.0
   EVG    = 0.0
   EVC    = 0.0
   TR     = 0.0
   GHV    = 0.0
   T2MV   = 0.0
   PSNSUN = 0.0
   PSNSHA = 0.0
   Q2V    = 0.0
   CHV2   = 0.0
   CHLEAF = 0.0
   CHUC   = 0.0
!====== vege_flux end

!====== bare_flux new vars
   TGB    = TG
   CMB    = CM
   CHB    = CH
   TAUXB  = 0.0
   TAUYB  = 0.0
   IRB    = 0.0
   SHB    = 0.0
   EVB    = 0.0
   GHB    = 0.0
   T2MB   = 0.0
   Q2B    = 0.0
   EHB2   = 0.0
   SSOIL  = 0.0
!====== bare_flux end

!=== TSNOSOI new vars
if (input%runsnow) then
   TBOT = 270.0
else
   TBOT = 290.0
endif
!=== TSNOSOI end

!==== phasechange new vars
   QMELT    = 0.0
   IMELT(:) = 0
!==== phasechange end

!==== Energy Main 
   ERRENG = 0.0
   ERRSW  = 0.0
   Z0WRF  = 0.0
   T2M    = 0.0
   TAUX   = 0.0
   TAUY   = 0.0
   FIRA   = 0.0
   FSH    = 0.0
   TRAD   = 0.0
   PSN    = 0.0
   APAR   = 0.0
   BTRANI = 0.2 ! 0~1
   BTRAN  = 0.2
   TS     = 0.0
   Q2E    = 0.0
   EMISSI = 0.0
   PAH    = 0.0
!==== Energy end

!============= irrigation related
  if ( OPT_IRR > 0) then
     IRRFRA = 1.0  ! irrigation fraction
     CROPLU = .true.
  else
     IRRFRA = 0.0
     CROPLU = .false.
  endif
  if ( OPT_IRRM == 0 ) then
     SIFAC    = 0.3
     MIFAC    = 0.3
     FIFAC    = 0.4
     IRAMTFI  = 0.25
     IRAMTMI  = 0.25
     IRAMTSI  = 0.5
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  elseif ( OPT_IRRM == 1 ) then ! sprinkler
     SIFAC    = 1.0
     MIFAC    = 0.0
     FIFAC    = 0.0
     IRAMTFI  = 0.0
     IRAMTMI  = 0.0
     IRAMTSI  = 0.5
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  elseif ( OPT_IRRM == 2 ) then ! micro
     SIFAC    = 0.0
     MIFAC    = 1.0
     FIFAC    = 0.0
     IRAMTFI  = 0.0
     IRAMTMI  = 0.5
     IRAMTSI  = 0.0
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  elseif ( OPT_IRRM == 3 ) then ! flood
     SIFAC    = 0.0
     MIFAC    = 0.0
     FIFAC    = 1.0
     IRAMTFI  = 0.5
     IRAMTMI  = 0.0
     IRAMTSI  = 0.0
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  endif
  IRCNTSI = 0
  IRCNTMI = 0
  IRCNTFI = 0
  IREVPLOS = 0.0
  FIRR = 0.0
  EIRR = 0.0
  if ( OPT_TDRN > 0 ) then
      TDFRACMP = 0.5
      ZWT      = 0.2  ! to allow the drainage effect to show up
  else
      TDFRACMP = 0.0
  endif
!================= irrigation end

! for other variables
  DT         = input%DTIn
  ntime      = nint(input%maxtime * 3600.0 / DT)
  rain_steps = input%rain_duration * 3600.0 / DT
  dry_steps  = input%dry_duration * 3600.0 / DT
  raining    = input%raining

! prevent too large SMC initial values
  do isoil = 1, NSOIL
     if ( SMC(isoil) > SMCMAX(isoil) ) then
        SH2O(isoil) = SMCMAX(isoil) * SH2O(isoil) / SMC(isoil)
        SMC(isoil)  = SMCMAX(isoil)
        SICE(isoil) = max( 0.0, SMC(isoil)-SH2O(isoil) )
     endif
  enddo

!!!!!!========= initialization complete ==================================

!---------------------------------------------------------------------
! create output file and add initial values
!---------------------------------------------------------------------

  call initialize_output(noahmp, input, ntime+1)
  call add_to_output(0, noahmp, errwat, ERRSW, ERRENG)


!---------------------------------------------------------------------
! start the time loop
!---------------------------------------------------------------------

  do itime = 1, ntime

    tw0 = sum(DZSNSO(1:NSOIL) * SMC * 1000.0) + SNEQV + WA + CANLIQ + CANICE ! [mm] 

    IRFIRATE = 0.0
    IRMIRATE = 0.0
    IRSIRATE = 0.0
    IREVPLOS = 0.0 
    FIRR     = 0.0
    EIRR     = 0.0

  !---------------------------------------------------------------------
  ! calculate the input water
  !---------------------------------------------------------------------

    if ( raining .eqv. .true. ) then
       RAIN      = input%rainrate / 3600.0    ! input water [m/s]
       rain_step = rain_step + 1
       if ( rain_step == rain_steps ) then            ! event length met
          rain_step = 0
          raining   = .false.
      endif
    else
      RAIN     = 0.0                        ! stop water input [m/s]
      dry_step = dry_step + 1
      if ( dry_step == dry_steps ) then              ! between event length met
        dry_step = 0
        raining  = .true.
      endif
    endif

    if ( input%runsnow .eqv. .true. ) then
       SNOW = RAIN * 1.0
       RAIN = 0.0
    else
       SNOW = 0.0
    endif


!--------------------------------------------------------------------- 
! main noahmplsm subroutine below

    call NoahmpMain(noahmp)

!---------------------------------------------------------------------


! some updates from last time step for use in next step (from drv)

    FICEOLD(ISNOW+1:0) = SNICE(ISNOW+1:0) /(SNICE(ISNOW+1:0) + SNLIQ(ISNOW+1:0))

!!! extracted from ERROR subroutine
! add energy balance check to NoahmpMain

    !---------------------------------------------------------------------
    ! add to output file
    !---------------------------------------------------------------------

    call add_to_output(itime, noahmp, errwat, ERRSW, ERRENG)
   
  end do ! time loop

  call finalize_output()
   
  end associate

  print*, 'model run successfully completed ...'


end program NoahmpDriverMod
