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
  use NoahmpMainGlacierMod

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
  logical                :: raining            ! .true. if raining

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
            IST             => noahmp%config%domain%IST            ,& ! in,     surface type 1-soil; 2-lake 
            ICE             => noahmp%config%domain%ICE            ,& ! in,    flag for ice point
            DX              => noahmp%config%domain%DX             ,& ! in,     noahmp model grid spacing (m)
            DT              => noahmp%config%domain%DT             ,& ! in,     main noahmp timestep (s)
            COSZ            => noahmp%config%domain%COSZ           ,& ! in,     cosine solar zenith angle
            NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
            ZSOIL           => noahmp%config%domain%ZSOIL          ,& ! in,     depth of layer-bottom from soil surface
            ZSNSO           => noahmp%config%domain%ZSNSO          ,& ! inout,  depth of snow/soil layer-bottom (m)
            TBOT            => noahmp%forcing%TBOT                 ,& ! in,    bottom soil temp. at ZBOT (K)
            SFCTMP          => noahmp%forcing%SFCTMP               ,& ! in,     surface air temperature [k] from Atmos forcing
            TV              => noahmp%energy%state%TV              ,& ! inout,  vegetation temperature (k)
            TG              => noahmp%energy%state%TG              ,& ! in,     ground temperature (k)
            STC             => noahmp%energy%state%STC             ,& ! inout,  snow and soil layer temperature [k]
            SH2O            => noahmp%water%state%SH2O             ,& ! inout,  soil water content [m3/m3]
            FICEOLD         => noahmp%water%state%FICEOLD_SNOW     ,& ! in,     ice fraction in snow layers at last timestep
            SMC             => noahmp%water%state%SMC              ,& ! inout,  total soil moisture [m3/m3]
            LAIM            => noahmp%energy%param%LAIM            ,& ! in,     monthly LAI from table
            SAIM            => noahmp%energy%param%SAIM            ,& ! in,     monthly SAI from table
            LAI             => noahmp%energy%state%LAI             ,& ! in,     leaf area index (m2/m2)
            SAI             => noahmp%energy%state%SAI             ,& ! inout,  stem area index (m2/m2)
            SFCPRS          => noahmp%forcing%SFCPRS               ,& ! in,     surface pressure (pa)
            PSFC            => noahmp%forcing%PSFC                 ,& ! in,    pressure at lowest model layer
            PRCPCONV        => noahmp%forcing%PRCPCONV             ,& ! in,   convective precipitation entering  [mm/s]
            PRCPNONC        => noahmp%forcing%PRCPNONC             ,& ! in,   non-convective precipitation entering [mm/s]
            PRCPSHCV        => noahmp%forcing%PRCPSHCV             ,& ! in,   shallow convective precip entering  [mm/s]
            PRCPSNOW        => noahmp%forcing%PRCPSNOW             ,& ! in,   snow entering land model [mm/s]
            PRCPGRPL        => noahmp%forcing%PRCPGRPL             ,& ! in,   graupel entering land model [mm/s]
            PRCPHAIL        => noahmp%forcing%PRCPHAIL             ,& ! in,   hail entering land model [mm/s]
            SMCEQ           => noahmp%water%state%SMCEQ            ,& ! in,     equilibrium soil water  content [m3/m3]
            CO2AIR          => noahmp%energy%state%CO2AIR          ,& ! in,    atmospheric co2 concentration (pa)
            O2AIR           => noahmp%energy%state%O2AIR           ,& ! in,    atmospheric o2 concentration (pa)
            FOLN            => noahmp%biochem%state%FOLN           ,& ! in,    foliage nitrogen concentration (%)
            ALBOLD          => noahmp%energy%state%ALBOLD          ,& ! in,     snow albedo at last time step
            SNEQVO          => noahmp%water%state%SNEQVO           ,& ! in,     snow mass at last time step(mm)
            TAH             => noahmp%energy%state%TAH             ,& ! in,    canopy air temperature (K)
            EAH             => noahmp%energy%state%EAH             ,& ! in,    canopy air vapor pressure (pa)
            Q2              => noahmp%forcing%Q2                   ,& ! in,     specific humidity kg/kg
            FWET            => noahmp%water%state%FWET             ,& ! out,    wetted or snowed fraction of the canopy
            CANLIQ          => noahmp%water%state%CANLIQ           ,& ! inout,  intercepted liquid water (mm)
            CANICE          => noahmp%water%state%CANICE           ,& ! inout,  intercepted ice mass (mm)
            QSFC            => noahmp%energy%state%QSFC            ,& ! inout, water vapor mixing ratio at lowest model layer
            QRAIN           => noahmp%water%flux%QRAIN             ,& ! in,     snow surface rain rate[mm/s]
            QSNOW           => noahmp%water%flux%QSNOW             ,& ! in,     snow at ground srf (mm/s) [+]
            ISNOW           => noahmp%config%domain%ISNOW          ,& ! inout,  actual number of snow layers
            SNICE           => noahmp%water%state%SNICE            ,& ! inout,  snow layer ice [mm]
            SNLIQ           => noahmp%water%state%SNLIQ            ,& ! inout,  snow layer liquid water [mm]
            SNOWH           => noahmp%water%state%SNOWH            ,& ! inout,  snow depth [m]
            SNEQV           => noahmp%water%state%SNEQV            ,& ! inout,  snow water equivalent [mm]
            ZWT             => noahmp%water%state%ZWT              ,& ! inout,  water table depth [m]
            WA              => noahmp%water%state%WA               ,& ! inout,  water storage in aquifer [mm]
            WT              => noahmp%water%state%WT               ,& ! inout,  water storage in aquifer + saturated soil [mm]
            WSLAKE          => noahmp%water%state%WSLAKE           ,& ! inout,  water storage in lake (can be -) (mm)
            LFMASS          => noahmp%biochem%state%LFMASS         ,& ! inout, leaf mass [g/m2]
            RTMASS          => noahmp%biochem%state%RTMASS         ,& ! inout, mass of fine roots [g/m2]
            STMASS          => noahmp%biochem%state%STMASS         ,& ! inout, stem mass [g/m2]
            WOOD            => noahmp%biochem%state%WOOD           ,& ! inout, mass of wood (incl. woody roots) [g/m2]
            STBLCP          => noahmp%biochem%state%STBLCP         ,& ! inout, stable carbon in deep soil [g/m2]
            FASTCP          => noahmp%biochem%state%FASTCP         ,& ! inout, short-lived carbon in shallow soil [g/m2]
            CM              => noahmp%energy%state%CM              ,& ! inout,  momentum exchange coefficient (m/s), above ZPD, vegetated
            CH              => noahmp%energy%state%CH              ,& ! inout,  heat exchange coefficient (m/s), above ZPD, vegetated
            TAUSS           => noahmp%energy%state%TAUSS           ,& ! inout,  non-dimensional snow age
            GRAIN           => noahmp%biochem%state%GRAIN          ,& ! inout, mass of grain (XING) [g/m2]
            GDD             => noahmp%biochem%state%GDD            ,& ! inout, growing degree days (XING)
            PGS             => noahmp%biochem%state%PGS            ,& ! in,    plant growing stage
            SMCWTD          => noahmp%water%state%SMCWTD           ,& ! inout,  soil moisture between bottom of the soil and the water table
            DEEPRECH        => noahmp%water%state%DEEPRECH         ,& ! inout,  recharge to or from the water table when deep [m]
            RECH            => noahmp%water%state%RECH             ,& ! out,    groundwater recharge (net vertical flux across the water table), positive up
            QTLDRN          => noahmp%water%flux%QTLDRN            ,& ! inout,  tile drainage (mm/s)
            Z0WRF           => noahmp%energy%state%Z0WRF           ,& ! out,   roughness length, momentum, surface, sent to coupled model
            FSA             => noahmp%energy%flux%FSA              ,& ! out,    total absorbed solar radiation (w/m2)
            FSR             => noahmp%energy%flux%FSR              ,& ! out,    total reflected solar radiation (w/m2)
            FIRA            => noahmp%energy%flux%FIRA             ,& ! out,   total net LW. rad (w/m2)   [+ to atm]
            FSH             => noahmp%energy%flux%FSH              ,& ! out,   total sensible heat (w/m2) [+ to atm]
            SSOIL           => noahmp%energy%flux%SSOIL            ,& ! out,   soil heat flux (w/m2) [+ to soil]
            FCEV            => noahmp%energy%flux%FCEV             ,& ! in,    canopy evaporation (w/m2) [+ = to atm]
            FCTR            => noahmp%energy%flux%FCTR             ,& ! in,    transpiration (w/m2) [+ = to atm]
            FGEV            => noahmp%energy%flux%FGEV             ,& ! in,    ground (soil/snow) evap heat (w/m2) [+ to atm]
            ECAN            => noahmp%water%flux%ECAN              ,& ! out,    evaporation of intercepted water (mm/s) [+]
            ETRAN           => noahmp%water%flux%ETRAN             ,& ! out,    transpiration rate (mm/s) [+]
            EDIR            => noahmp%water%flux%EDIR              ,& ! out,    net soil evaporation
            TRAD            => noahmp%energy%state%TRAD            ,& ! out,   radiative temperature (K)
            TGB             => noahmp%energy%state%TGB             ,& ! out,   bare ground temperature (K)
            TGV             => noahmp%energy%state%TGV             ,& ! out,   vegetated ground (below-canopy) temperature (K)
            T2MV            => noahmp%energy%state%T2MV            ,& ! out,   2 m height air temperature (k), vegetated
            T2MB            => noahmp%energy%state%T2MB            ,& ! out,   2 m height air temperature (k) bare ground
            Q2V             => noahmp%energy%state%Q2V             ,& ! out,   water vapor mixing ratio at 2m vegetated
            Q2B             => noahmp%energy%state%Q2B             ,& ! out,   bare ground 2-m water vapor mixing ratio
            RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! out,    surface runoff [mm/s]
            RUNSUB          => noahmp%water%flux%RUNSUB            ,& ! out,    subsurface runoff [mm/s] 
            PSN             => noahmp%biochem%flux%PSN             ,& ! out,   total leaf photosynthesis (umol co2 /m2 /s)
            APAR            => noahmp%energy%flux%APAR             ,& ! out,   total photosyn. active energy (w/m2)
            SAV             => noahmp%energy%flux%SAV              ,& ! out,    solar radiation absorbed by vegetation (w/m2)
            SAG             => noahmp%energy%flux%SAG              ,& ! out,    solar radiation absorbed by ground (w/m2)
            FSNO            => noahmp%water%state%FSNO             ,& ! in,     snow cover fraction (-)
            NEE             => noahmp%biochem%flux%NEE             ,& ! out,   net ecosystem exchange [g/m2/s CO2]
            NPP             => noahmp%biochem%flux%NPP             ,& ! out,   net primary productivity [g/m2/s C]
            GPP             => noahmp%biochem%flux%GPP             ,& ! out,   net instantaneous assimilation [g/m2/s C] 
            FVEG            => noahmp%energy%state%FVEG            ,& ! in,     greeness vegetation fraction (-)
            ALBEDO          => noahmp%energy%state%ALBEDO          ,& ! out,   total shortwave surface albedo
            PONDING         => noahmp%water%state%PONDING          ,& ! inout,  melting water from snow when there is no layer (mm)
            PONDING1        => noahmp%water%state%PONDING1         ,& ! out,    surface ponding 1 (mm)
            PONDING2        => noahmp%water%state%PONDING2         ,& ! out,    surface ponding 2 (mm)
            QSNBOT          => noahmp%water%flux%QSNBOT            ,& ! out,    melting water out of snow bottom [mm/s]
            RSSUN           => noahmp%energy%state%RSSUN           ,& ! out,   sunlit leaf stomatal resistance (s/m)
            RSSHA           => noahmp%energy%state%RSSHA           ,& ! out,   shaded leaf stomatal resistance (s/m)
            ALBSND          => noahmp%energy%state%ALBSND          ,& ! out,    snow albedo for direct(1=vis, 2=nir)
            ALBSNI          => noahmp%energy%state%ALBSNI          ,& ! out,    snow albedo for diffuse(1=vis, 2=nir)
            BGAP            => noahmp%energy%state%BGAP            ,& ! out,    between canopy gap fraction for beam
            WGAP            => noahmp%energy%state%WGAP            ,& ! out,    within canopy gap fraction for beam
            CHV             => noahmp%energy%state%CHV             ,& ! out,   drag coefficient for heat, above ZPD, vegetated
            CHB             => noahmp%energy%state%CHB             ,& ! out,   drag coefficient for heat, above ZPD, bare ground
            EMISSI          => noahmp%energy%state%EMISSI          ,& ! out,   surface emissivity
            SHG             => noahmp%energy%flux%SHG              ,& ! out,   ground sensible heat flux (w/m2)     [+= to atm]
            SHC             => noahmp%energy%flux%SHC              ,& ! out,   canopy sensible heat flux (w/m2)     [+= to atm]
            SHB             => noahmp%energy%flux%SHB              ,& ! out,   sensible heat flux (w/m2) bare ground [+ to atm]
            IRG             => noahmp%energy%flux%IRG              ,& ! out,   ground net longwave radiation (w/m2) [+= to atm]
            IRC             => noahmp%energy%flux%IRC              ,& ! out,   canopy net longwave radiation (w/m2) [+= to atm]
            EVG             => noahmp%energy%flux%EVG              ,& ! out,   ground evaporation heat flux (w/m2)  [+= to atm]
            EVC             => noahmp%energy%flux%EVC              ,& ! out,   canopy evaporation heat flux (w/m2)  [+= to atm]
            TR              => noahmp%energy%flux%TR               ,& ! out,   canopy transpiration heat flux (w/m2)[+= to atm]
            GHV             => noahmp%energy%flux%GHV              ,& ! out,   vegetated ground heat (w/m2) [+ = to soil]
            IRB             => noahmp%energy%flux%IRB              ,& ! out,   net longwave rad (w/m2) bare ground [+ to atm]
            EVB             => noahmp%energy%flux%EVB              ,& ! out,   latent heat flux (w/m2) bare ground [+ to atm]
            GHB             => noahmp%energy%flux%GHB              ,& ! out,   bare ground heat flux (w/m2) [+ to soil]
            CHV2            => noahmp%energy%state%CHV2            ,& ! out,   2m sensible heat exchange coefficient (m/s)
            CHLEAF          => noahmp%energy%state%CHLEAF          ,& ! out,   leaf sensible heat exchange coefficient (m/s),leaf surface to canopy air
            CHUC            => noahmp%energy%state%CHUC            ,& ! out,   under canopy sensible heat exchange coefficient (m/s)
            FPICE           => noahmp%water%state%FPICE            ,& ! out,  snowfall fraction
            PAHV            => noahmp%energy%flux%PAHV             ,& ! out,    precipitation advected heat - vegetation net (W/m2)
            PAHG            => noahmp%energy%flux%PAHG             ,& ! out,    precipitation advected heat - under canopy net (W/m2)
            PAHB            => noahmp%energy%flux%PAHB             ,& ! out,    precipitation advected heat - bare ground net (W/m2)
            PAH             => noahmp%energy%flux%PAH              ,& ! out,   precipitation advected heat - total (W/m2)
            LAISUN          => noahmp%energy%state%LAISUN          ,& ! in,     sunlit leaf area
            LAISHA          => noahmp%energy%state%LAISHA          ,& ! in,     shaded leaf area
            sfcheadrt       => noahmp%water%state%sfcheadrt        ,& ! inout,  surface water head (mm) 
            WATBLED         => noahmp%water%state%WATBLED          ,& ! in,     water table depth estimated in WRF-Hydro fine grids (m)
            FIRR            => noahmp%energy%flux%FIRR             ,& ! inout,  latent heating due to sprinkler evaporation [w/m2]
            EIRR            => noahmp%water%flux%EIRR              ,& ! inout,  evaporation of irrigation water to evaporation,sprinkler [mm/s]
            IRCNTSI         => noahmp%water%state%IRCNTSI          ,& ! inout,  irrigation event number, Sprinkler
            IRCNTMI         => noahmp%water%state%IRCNTMI          ,& ! inout,  irrigation event number, Micro
            IRCNTFI         => noahmp%water%state%IRCNTFI          ,& ! inout,  irrigation event number, Flood
            OPT_IRR         => noahmp%config%nmlist%OPT_IRR        ,& ! in,     options for irrigation
            OPT_IRRM        => noahmp%config%nmlist%OPT_IRRM       ,& ! in,     options for irrigation method
            IRRFRA          => noahmp%water%state%IRRFRA           ,& ! in,     irrigation fraction
            OPT_TDRN        => noahmp%config%nmlist%OPT_TDRN       ,& ! in,     options for tile drainage
            TDFRACMP        => noahmp%water%state%TDFRACMP         ,& ! in,     tile drainage map(fraction)
            SIFRA           => noahmp%water%state%SIFRA            ,& ! in,     sprinkler irrigation fraction (0 to 1)
            MIFRA           => noahmp%water%state%MIFRA            ,& ! in,     fraction of grid under micro irrigation (0 to 1)
            FIFRA           => noahmp%water%state%FIFRA            ,& ! in,     fraction of grid under flood irrigation (0 to 1)
            IRAMTFI         => noahmp%water%state%IRAMTFI          ,& ! inout,  flood irrigation water amount [m]
            IRAMTMI         => noahmp%water%state%IRAMTMI          ,& ! inout,  micro irrigation water amount [m]
            IRAMTSI         => noahmp%water%state%IRAMTSI          ,& ! inout,  irrigation water amount [m] to be applied, Sprinkler
            IRFIRATE        => noahmp%water%flux%IRFIRATE          ,& ! inout,  flood irrigation water rate [m/timestep]
            IRMIRATE        => noahmp%water%flux%IRMIRATE          ,& ! inout,  micro irrigation water rate [m/timestep]
            IRSIRATE        => noahmp%water%flux%IRSIRATE          ,& ! inout,  rate of irrigation by sprinkler [m/timestep]
            CHB2            => noahmp%energy%state%EHB2            ,& ! inout,  origianl CHB2 in NoahMP
            SMCMAX          => noahmp%water%param%SMCMAX            & ! in,     saturated value of soil moisture [m3/m3]
           )
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!  initialize required variables
!---------------------------------------------------------------------
! input used to adjust for snow and non-snow cases
  if ( input%runsnow .eqv. .true. ) then
     TBOT     = 270.0
     SFCTMP   = 265.0
     TV       = 268.0
     TG       = 270.0
     SH2O(1:4)= 0.03
  else
     TBOT     = 290.0
     SFCTMP   = 298.0
     TV       = 293.0
     TG       = 285.0
     SH2O(1:4)= 0.2
  endif

  ICE         = 0
  FICEOLD     = 0.0
  STC(1:4)    = SFCTMP
  STC(-2:0)   = 0.0
  SMC         = 0.23
  LAI         = LAIM(6)
  SAI         = SAIM(6)
  PSFC        = SFCPRS + 50.0
  PRCPCONV    = 0.0
  PRCPSHCV    = 0.0
  PRCPGRPL    = 0.0
  PRCPHAIL    = 0.0
  PRCPNONC    = 0.0
  PRCPSNOW    = 0.0
  SMCEQ(1:4)  = 0.3
  CO2AIR      = 395.e-06 * SFCPRS
  O2AIR       = 0.209 * SFCPRS
  FOLN        = 1.0
  ALBOLD      = 0.0
  SNEQVO      = 0.0
  TAH         = TV - 1.0
  EAH         = Q2 * SFCPRS / (0.622 + 0.378*Q2)
  FWET        = 0.0
  CANLIQ      = 0.0
  CANICE      = 0.0
  QSFC        = Q2
  QRAIN       = 0.0
  QSNOW       = 0.0
  ISNOW       = 0
  ZSNSO(-2:0) = 0.0
  ZSNSO(1:4)  = ZSOIL(1:4)
  SNOWH       = 0.0
  SNEQV       = 0.0
  SNICE       = 0.0
  SNLIQ       = 0.0
  ZWT         = 2.5
  WA          = 0.0
  WT          = 0.0
  WSLAKE      = 0.0
  LFMASS      = LAI / 0.01
  RTMASS      = 500.0
  STMASS      = SAI / 0.003
  WOOD        = 500.0
  STBLCP      = 1000.0
  FASTCP      = 1000.0
  CM          = 0.1
  CH          = 0.01
  TAUSS       = 0.0
  GRAIN       = 1e-10
  GDD         = 0.0
  PGS         = 1
  SMCWTD      = 0.3
  DEEPRECH    = 0.0
  RECH        = 0.0
  QTLDRN      = 0.0
  Z0WRF       = 0.0
  FSA         = 0.0
  FSR         = 0.0
  FIRA        = 0.0
  FSH         = 0.0
  SSOIL       = 0.0
  FCEV        = 0.0
  FCTR        = 0.0
  FGEV        = 0.0
  ECAN        = 0.0
  ETRAN       = 0.0
  EDIR        = 0.0
  TRAD        = 0.0
  TGB         = 0.0
  TGV         = 0.0
  T2MV        = 0.0
  T2MB        = 0.0
  Q2V         = 0.0
  Q2B         = 0.0
  RUNSRF      = 0.0
  RUNSUB      = 0.0
  PSN         = 0.0
  APAR        = 0.0
  SAV         = 0.0
  SAG         = 0.0
  FSNO        = 0.0
  NEE         = 0.0
  GPP         = 0.0
  NPP         = 0.0
  FVEG        = 0.0
  ALBEDO      = 0.0
  PONDING     = 0.0
  PONDING1    = 0.0
  PONDING2    = 0.0
  QSNBOT      = 0.0
  RSSUN       = 0.0
  RSSHA       = 0.0
  ALBSND      = 0.0
  ALBSNI      = 0.0
  BGAP        = 0.0
  WGAP        = 0.0
  CHV         = 0.0
  CHB         = 0.0
  EMISSI      = 0.0
  SHG         = 0.0
  SHC         = 0.0
  SHB         = 0.0
  EVG         = 0.0
  EVB         = 0.0
  GHV         = 0.0
  GHB         = 0.0
  IRG         = 0.0
  IRC         = 0.0
  IRB         = 0.0
  EVC         = 0.0
  TR          = 0.0
  CHV2        = 0.0
  CHLEAF      = 0.0
  CHUC        = 0.0
  CHB2        = 0.0
  FPICE       = 0.0
  PAHV        = 0.0
  PAHG        = 0.0
  PAHB        = 0.0
  PAH         = 0.0
  LAISUN      = 0.0
  LAISHA      = 0.0
#ifdef WRF_HYDRO
  sfcheadrt   = 0.0
  WATBLED     = 0.0
#endif
!= irrigation related
  FIRR        = 0.0
  EIRR        = 0.0
  IRCNTSI     = 0
  IRCNTMI     = 0
  IRCNTFI     = 0
  if ( OPT_IRR > 0 ) then
     IRRFRA   = 1.0
  else
     IRRFRA   = 0.0
  endif
  if ( OPT_TDRN > 0 ) then
     TDFRACMP = 0.5
     ZWT      = 0.2
  else
     TDFRACMP = 0.0
  endif
  if ( OPT_IRRM == 0 ) then
     SIFRA    = 0.3
     MIFRA    = 0.3
     FIFRA    = 0.4
     IRAMTFI  = 0.25
     IRAMTMI  = 0.25
     IRAMTSI  = 0.5
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  elseif ( OPT_IRRM == 1 ) then
     SIFRA    = 1.0
     MIFRA    = 0.0
     FIFRA    = 0.0
     IRAMTFI  = 0.0
     IRAMTMI  = 0.0
     IRAMTSI  = 0.5
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  elseif ( OPT_IRRM == 2 ) then
     SIFRA    = 0.0
     MIFRA    = 1.0
     FIFRA    = 0.0
     IRAMTFI  = 0.0
     IRAMTMI  = 0.5
     IRAMTSI  = 0.0
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  elseif ( OPT_IRRM == 3 ) then
     SIFRA    = 0.0
     MIFRA    = 0.0
     FIFRA    = 1.0
     IRAMTFI  = 0.5
     IRAMTMI  = 0.0
     IRAMTSI  = 0.0
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  endif
!==== irrigation end


! intialize for forcing
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
     endif
  enddo

!!!!!!========= initialization complete ==================================


!---------------------------------------------------------------------
! create output file and add initial values
!---------------------------------------------------------------------

  call initialize_output(noahmp, input, ntime+1)
  call add_to_output(0, noahmp)


!---------------------------------------------------------------------
! start the time loop
!---------------------------------------------------------------------

  do itime = 1, ntime

    IRFIRATE = 0.0
    IRMIRATE = 0.0
    IRSIRATE = 0.0
    FIRR     = 0.0
    EIRR     = 0.0

  !---------------------------------------------------------------------
  ! calculate the input water
  !---------------------------------------------------------------------

    if ( raining .eqv. .true. ) then
       PRCPNONC  = input%rainrate / 3600.0    ! input water [m/s]
       rain_step = rain_step + 1
       if ( rain_step == rain_steps ) then            ! event length met
          rain_step = 0
          raining   = .false.
      endif
    else
      PRCPNONC = 0.0                        ! stop water input [m/s]
      dry_step = dry_step + 1
      if ( dry_step == dry_steps ) then              ! between event length met
        dry_step = 0
        raining  = .true.
      endif
    endif

    if ( input%runsnow .eqv. .true. ) then
       PRCPSNOW = PRCPNONC
    endif

    ! varying temperature forcing
    if ( input%runsnow .eqv. .true. ) then
       SFCTMP = 265.0 + (itime-1)*0.1
    else
       SFCTMP = 298.0 + (itime-1)* (-0.05)
    endif

    !--------------------------------------------------------------------- 
    ! main noahmplsm subroutine below
    if ( input%runglacier .eqv. .true. ) then
       ICE = -1
       call NoahmpMainGlacier(noahmp)
       FSNO   = 1.0       
       TGB    = TG 
       CHB    = CH 
       IRB    = FIRA
       SHB    = FSH
       EVB    = FGEV
       GHB    = SSOIL
       Z0WRF  = 0.002
    else
       ICE = 0
       call NoahmpMain(noahmp)
    endif

    !---------------------------------------------------------------------

    ! some updates from last time step for use in next step (from drv)

    FICEOLD(ISNOW+1:0) = SNICE(ISNOW+1:0) /(SNICE(ISNOW+1:0) + SNLIQ(ISNOW+1:0))


    !---------------------------------------------------------------------
    ! add to output file
    !---------------------------------------------------------------------

    call add_to_output(itime, noahmp)


  end do ! time loop

  call finalize_output()
   
  end associate

  print*, 'model run successfully completed ...'

end program NoahmpDriverMod
