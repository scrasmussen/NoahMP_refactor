program noahmp_driver

use noahmp_output
use noahmp_routines
USE NOAHMP_TABLES

  implicit none

!---------------------------------------------------------------------
!  declare namelist input variable start
!---------------------------------------------------------------------
! timing
  real          :: dt
  integer       :: maxtime
  character*256 :: output_filename
  logical       :: runsnow
  real          :: JULIAN
! forcing
  real          :: rainrate
  integer       :: rain_duration
  integer       :: dry_duration
  logical       :: raining
  real          :: uwind
  real          :: vwind
  real          :: sfcpres
  real          :: fcev_e
  real          :: fctr_e
  real          :: fgev_e
  real          :: Q2
  real          :: SWDOWN
  real          :: LWDOWN
! structure
  integer       :: isltyp
  integer       :: vegtype
  integer       :: soilcolor
  integer       :: slopetype
  integer       :: croptype
  integer       :: nsoil
  integer       :: nsnow
  integer       :: structure_option
  real          :: soil_depth
  real          :: vegfra
  real          :: vegmax
  real          :: shdmax
! fixed_initial
  real, allocatable, dimension(:) :: zsoil   ! depth of layer-bottom from soil surface
  real, allocatable, dimension(:) :: DZSNSO  ! snow/soil layer thickness [m]
! uniform_initial
  logical :: initial_uniform                 ! initial all levels the same
  real    :: initial_sh2o_value              ! constant sh2o value
  real    :: initial_sice_value              ! constant sice value
! options
  integer :: idveg,iopt_crs,iopt_btr,iopt_run,iopt_sfc,iopt_frz,&
             iopt_inf,iopt_rad,iopt_alb,iopt_snf,iopt_tbot,iopt_stc, &
             iopt_rsf,iopt_soil,iopt_pedo,iopt_crop,iopt_irr,iopt_irrm,iopt_infdv,iopt_tdrn

  !--------------------!
  !  namelist structure   !
  !--------------------!
  namelist / timing          / dt,maxtime,output_filename,runsnow,JULIAN
  namelist / forcing         / rainrate,rain_duration,dry_duration,&
                               raining,uwind,vwind,sfcpres,fcev_e,fctr_e,fgev_e,Q2,SWDOWN,LWDOWN
  namelist / structure       / isltyp,VEGTYPE,soilcolor,slopetype,croptype,nsoil,nsnow,structure_option,soil_depth,&
                               vegfra,vegmax,shdmax
  namelist / fixed_initial   / zsoil,dzsnso
  namelist / uniform_initial / initial_uniform,initial_sh2o_value,&
                               initial_sice_value
  namelist / options  /  idveg,iopt_crs,iopt_btr,iopt_run,iopt_sfc,iopt_frz,&
                             iopt_inf,iopt_rad,iopt_alb,iopt_snf,iopt_tbot,iopt_stc, &
                             iopt_rsf,iopt_soil,iopt_pedo,iopt_crop,iopt_irr,iopt_irrm,iopt_infdv,iopt_tdrn
 
!---------------------------------------------------------------------
!  inputs end
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!  additional variables required or passed to water subroutine
!---------------------------------------------------------------------
  type (noahmp_parameters) :: parameters

  integer                         :: ILOC  = 1   !grid index
  integer                         :: JLOC  = 1   !grid index
  integer                         :: VEGTYP 
  integer, dimension(4)           :: SOILTYPE
  REAL                            :: REFDK
  REAL                            :: REFKDT
  REAL                            :: FRZK
  REAL                            :: FRZFACT
  INTEGER                         :: ISOIL
  integer, allocatable, dimension(:) :: IMELT  !phase change index
  real                            :: UU      !u-direction wind speed [m/s]
  real                            :: VV      !v-direction wind speed [m/s]
  real                            :: FCEV    !canopy evaporation (w/m2) [+ to atm ]
  real                            :: FCTR    !transpiration (w/m2) [+ to atm]
  REAL                            :: QPRECC  !convective precipitation (mm/s)
  REAL                            :: QPRECL  !large-scale precipitation (mm/s)
  real                            :: ELAI    !leaf area index, after burying by snow
  real                            :: ESAI    !stem area index, after burying by snow
  real                            :: LAI     !leaf area index
  real                            :: SAI     !stem area index
  real                            :: SFCTMP  !model-level temperature (k)
  real                            :: FB_snow  ! canopy fraction buried by snow
  real                            :: QVAP    !soil surface evaporation rate[mm/s] 
  REAL                            :: QDEW    !ground surface dew rate [mm/s]
  real                            :: TV      ! canopy temperature
  real                            :: TG      ! ground temperature
  logical                         :: frozen_canopy ! used to define latent heat pathway for canopy
  logical                         :: frozen_ground ! used to define latent heat pathway for ground
  real                            :: SFCPRS !surface pressure (pa)
  real                            :: FGEV   ! FGEV   !ground evap heat (w/m2) [+ to atm]
  REAL, allocatable, dimension(:) :: BTRANI !Soil water transpiration factor (0 - 1) !!!!!!! Cenlin
  real                            :: IRRFRA   ! irrigation fraction
  real                            :: MIFAC    ! micro irrigation fraction
  real                            :: FIFAC    ! flood irrigation fraction
  real                            :: SIFAC    ! sprinkler irrigation fraction
  logical                         :: CROPLU   ! flag to identify croplands
  real, allocatable, dimension(:) :: FICEOLD !ice fraction at last timestep
  real, allocatable, dimension(:) :: ZSNSO   !depth of snow/soil layer-bottom
  REAL                            :: PONDING ![mm]
  integer                         :: IST      !surface type 1-soil; 2-lake
  real                            :: FVEG    !greeness vegetation fraction (-) 
  real, allocatable, dimension(:) :: SMCEQ   !equilibrium soil water content [m3/m3] (used in m-m&f groundwater dynamics)
  real                            :: BDFALL   !bulk density of snowfall (kg/m3) ! MB/AN: v3.7
  real                            :: FP      !fraction of the gridcell that receives precipitation
  real                            :: SNOW    !snowfall (mm/s)
  real                            :: RAIN    !ralfall mm/s 
  REAL                            :: QRAIN   !rain at ground srf (mm/s) [+]
  REAL                            :: QSNOW   !snow at ground srf (mm/s) [+]
  REAL                            :: SNOWHIN !snow depth increasing rate (m/s)
  real                            :: DX      !horisontal resolution, used for tile drainage
  real                            :: TDFRACMP !tile drain fraction map
  integer                         :: ISNOW   !actual no. of snow layers
  real                            :: CANLIQ  !intercepted liquid water (mm)
  real                            :: CANICE  !intercepted ice mass (mm)
  REAL                            :: SNOWH   !snow height [m]
  REAL                            :: SNEQV   !snow water eqv. [mm]
  real, allocatable, dimension(:) :: SNICE   !snow layer ice [mm]
  real, allocatable, dimension(:) :: SNLIQ   !snow layer liquid water [mm]
  real, allocatable, dimension(:) :: STC     !snow/soil layer temperature [k]
  real, allocatable, dimension(:) :: SICE    ! soil ice content [m3/m3]
  real, allocatable, dimension(:) :: SH2O    ! soil liquid water content [m3/m3]
  real, allocatable, dimension(:) :: SMC         !total soil water content [m3/m3]
  real                            :: ZWT        !the depth to water table [m]
  real                            :: WA      !water storage in aquifer [mm]
  real                            :: WT      !water storage in aquifer + stuarated soil [mm]
  REAL                            :: WSLAKE  !water storage in lake (can be -) (mm)
  real                            :: SMCWTD      !soil water content between bottom of the soil and water table [m3/m3]
  real                            :: DEEPRECH    !recharge to or from the water table when deep [m]
  real                            :: RECH !recharge to or from the water table when shallow [m] (diagnostic)
  real                            :: IRAMTFI  ! irrigation water amount [m] to be applied, flood
  real                            :: IRAMTMI  ! irrigation water amount [m] to be applied, Micro
  real                            :: IRAMTSI     !total irrigation water amount [m]
  real                            :: IRFIRATE ! rate of irrigation by flood [m/timestep]
  real                            :: IRMIRATE ! rate of irrigation by micro [m/timestep]
  real                            :: IRSIRATE !rate of irrigation by sprinkler [m/timestep]
  real                            :: CMC     !intercepted water per ground area (mm)
  real                            :: ECAN    !evap of intercepted water (mm/s) [+]
  real                            :: ETRAN   !transpiration rate (mm/s) [+]
  real                            :: FWET    !wetted/snowed fraction of canopy (-)
  real                            :: RUNSRF      !surface runoff [mm/s] 
  real                            :: RUNSUB      !baseflow (sturation excess) [mm/s]
  real                            :: QIN     !groundwater recharge [mm/s]
  real                            :: QDIS    !groundwater discharge [mm/s]
  REAL                            :: PONDING1 ![mm]
  REAL                            :: PONDING2 ![mm]
  REAL                            :: QSNBOT !melting water out of snow bottom [mm/s]
  real                            :: QTLDRN   !tile drainage (mm/s)
  real                            :: QINSUR      !water input on soil surface [m/s]
  real                            :: QSEVA   !soil surface evap rate [mm/s]
  real, allocatable, dimension(:) :: ETRANI      !transpiration rate (mm/s) [+]
  REAL                            :: QSNFRO  !snow surface frost rate[mm/s]
  REAL                            :: QSNSUB  !snow surface sublimation rate [mm/s]
  REAL                            :: SNOFLOW !glacier flow [mm/s]
  REAL                            :: QSDEW   !soil surface dew rate [mm/s]
  real                            :: QDRAIN      !soil-bottom free drainage [mm/s] 
  real                            :: FCRMAX      !maximum of fcr (-)
  real, allocatable, dimension(:) :: WCND        !hydraulic conductivity (m/s)
  integer                         :: IRCNTSI !irrigation event number, Sprinkler
  integer                         :: IRCNTMI !irrigation event number, Micro
  integer                         :: IRCNTFI !irrigation event number, Flood 
  real                            :: IREVPLOS    !loss of irrigation water to evaporation,sprinkler [m/timestep]
  real                            :: FIRR           ! irrigation:latent heating due to sprinkler evaporation [w/m2]
  real                            :: EIRR           ! evaporation of irrigation water to evaporation,sprinkler [mm/s]
#ifdef WRF_HYDRO
  REAL                            :: sfcheadrt, WATBLED
#endif
  REAL                            :: QINTR   !interception rate for rain (mm/s)
  REAL                            :: QDRIPR  !drip rate for rain (mm/s)
  REAL                            :: QTHROR  !throughfall for rain (mm/s)
  REAL                            :: QINTS   !interception (loading) rate for snowfall (mm/s)
  REAL                            :: QDRIPS  !drip (unloading) rate for intercepted snow (mm/s)
  REAL                            :: QTHROS  !throughfall of snowfall (mm/s)
  REAL                            :: PAHV    !precipitation advected heat - vegetation net (W/m2)
  REAL                            :: PAHG    !precipitation advected heat - under canopy net (W/m2)
  REAL                            :: PAHB    !precipitation advected heat - bare ground net (W/m2)
  REAL                            :: EDIR    !net soil evaporation (mm/s)
! thermoprop new vars
  REAL                            :: LAT     !latitude (radians)
  real, allocatable, dimension(:) :: DF      !thermal conductivity [w/m/k]
  real, allocatable, dimension(:) :: HCPCT   !heat capacity [j/m3/k]
  real, allocatable, dimension(:) :: SNICEV  !partial volume of ice [m3/m3]
  real, allocatable, dimension(:) :: SNLIQV  !partial volume of liquid water [m3/m3]
  real, allocatable, dimension(:) :: EPORE   !effective porosity [m3/m3]
  real, allocatable, dimension(:) :: FACT    !computing energy for phase change
! radiation new vars
  integer                         :: ICE     ! value of ist for land ice
  real                            :: SNEQVO  ! SWE at last time step
  real                            :: COSZ    ! cosine solar zenith angle [0-1]
  real                            :: FSNO    ! snow cover fraction
  REAL, DIMENSION(1:2)            :: SOLAD   ! incoming direct solar radiation (w/m2)
  REAL, DIMENSION(1:2)            :: SOLAI   ! incoming diffuse solar radiation (w/m2)
  real                            :: ALBOLD  ! snow albedo at last time step(CLASS type)
  real                            :: TAUSS   ! non-dimensional snow age
  REAL                            :: FSUN    ! sunlit fraction of canopy (-)
  REAL                            :: LAISUN  ! sunlit leaf area (-)
  REAL                            :: LAISHA  ! shaded leaf area (-)
  REAL                            :: PARSUN  ! average absorbed par for sunlit leaves (w/m2)
  REAL                            :: PARSHA  ! average absorbed par for shaded leaves (w/m2)
  REAL                            :: SAV     ! solar radiation absorbed by vegetation (w/m2)
  REAL                            :: SAG     ! solar radiation absorbed by ground (w/m2)
  REAL                            :: FSA     ! total absorbed solar radiation (w/m2)
  REAL                            :: FSR     ! total reflected solar radiation (w/m2)
  REAL                            :: FSRV    ! veg. reflected solar radiation (w/m2)
  REAL                            :: FSRG    ! ground reflected solar radiation (w/m2)
  REAL                            :: BGAP
  REAL                            :: WGAP
  REAL, DIMENSION(1:2)            :: ALBSND  ! snow albedo (direct)
  REAL, DIMENSION(1:2)            :: ALBSNI  ! snow albedo (diffuse)
! vege_flux new vars
  REAL                            :: CM      ! momentum drag coefficient
  REAL                            :: CMV     ! momentum drag coefficient
  REAL                            :: CH      !sensible heat exchange coefficient
  REAL                            :: CHV     !sensible heat exchange coefficient
  REAL                            :: TGV    ! vegetated ground temperature
  logical                         :: VEG    !true if vegetated surface
  REAL                            :: LWDN   !atmospheric longwave radiation (w/m2)
  REAL                            :: UR     !wind speed at height zlvl (m/s)
  REAL                            :: THAIR  !potential temp at reference height (k)
  REAL                            :: QAIR   !specific humidity at zlvl (kg/kg)
  REAL                            :: EAIR   !vapor pressure air at zlvl (pa)
  REAL                            :: RHOAIR !density air (kg/m**3)
  REAL                            :: VAI    !total leaf area index + stem area index
  real                            :: GAMMAV  !psychrometric constant (pa/k)
  real                            :: GAMMAG  !psychrometric constant (pa/k)
  REAL                            :: CWP    !canopy wind parameter
  REAL                            :: ZLVL    !reference height (m)
  REAL                            :: ZPD    !zero plane displacement (m)
  REAL                            :: Z0M    !roughness length, momentum (m)
  REAL                            :: Z0MG   !roughness length, momentum, ground (m)
  REAL                            :: EMV    !vegetation emissivity
  REAL                            :: EMG    !ground emissivity
  REAL                            :: RSSUN        !sunlit leaf stomatal resistance (s/m)
  REAL                            :: RSSHA        !shaded leaf stomatal resistance (s/m)
  REAL                            :: RSURF  !ground surface resistance (s/m)
  real                            :: LATHEAV !latent heat vap./sublimation (j/kg)
  real                            :: LATHEAG !latent heat vap./sublimation (j/kg)
  REAL                            :: IGS    !growing season index (0=off, 1=on)
  REAL                            :: FOLN   !foliage nitrogen (%)
  REAL                            :: CO2AIR !atmospheric co2 concentration (pa)
  REAL                            :: O2AIR  !atmospheric o2 concentration (pa)
  REAL                            :: BTRAN  !soil water transpiration factor (0 to 1)
  REAL                            :: RHSUR  !raltive humidity in surface soil/snow air space (-)
  REAL                            :: PSI    !surface layer soil matrix potential (m)
  REAL                            :: EAH    !canopy air vapor pressure (pa)
  REAL                            :: TAH    !canopy air temperature (k)
  REAL                            :: DZ8W   !thickness of lowest layer
  REAL                            :: TAUXV  !wind stress: e-w (n/m2)
  REAL                            :: TAUYV  !wind stress: n-s (n/m2)
  REAL                            :: IRG    !net longwave radiation (w/m2) [+= to atm]
  REAL                            :: IRC    !net longwave radiation (w/m2) [+= to atm]
  REAL                            :: SHG    !sensible heat flux (w/m2)     [+= to atm]
  REAL                            :: SHC    !sensible heat flux (w/m2)     [+= to atm]
  REAL                            :: EVG    !evaporation heat flux (w/m2)  [+= to atm]
  REAL                            :: EVC    !evaporation heat flux (w/m2)  [+= to atm]
  REAL                            :: TR     !transpiration heat flux (w/m2)[+= to atm]
  REAL                            :: GHV    !ground heat (w/m2) [+ = to soil]
  REAL                            :: T2MV   !2 m height air temperature (k)
  REAL                            :: PSNSUN !sunlit leaf photosynthesis (umolco2/m2/s)
  REAL                            :: PSNSHA !shaded leaf photosynthesis (umolco2/m2/s)
  REAL                            :: QC     !cloud water mixing ratio
  REAL                            :: QSFC   !mixing ratio at lowest model layer
  REAL                            :: PSFC   !pressure at lowest model layer
  REAL                            :: Q2V
  REAL                            :: CHV2         !sensible heat conductance for diagnostics
  REAL                            :: CHLEAF !leaf exchange coefficient
  REAL                            :: CHUC   !under canopy exchange coefficient
  REAL                            :: ZPDG
! bare_flux new vars
  REAL                            :: TGB ! bare ground temperature
  REAL                            :: CMB     ! momentum drag coefficient
  REAL                            :: CHB      !sensible heat exchange coefficient
  REAL                            :: TAUXB  !wind stress: e-w (n/m2)
  REAL                            :: TAUYB  !wind stress: n-s (n/m2)
  REAL                            :: IRB
  REAL                            :: SHB
  REAL                            :: EVB
  REAL                            :: GHB
  REAL                            :: T2MB
  REAL                            :: Q2B
  REAL                            :: CHB2
  REAL                            :: SSOIL
! TSNOSOI new vars
  REAL                            :: TBOT   !bottom condition for soil temp. [K]


!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------

  integer :: itime, iz          ! some loop counters
  integer :: ntime      = 0     ! number of timesteps to run
  integer :: rain_steps = 0     ! number of timesteps in rain event
  integer :: dry_steps  = 0     ! number of timesteps between rain events
  integer :: rain_step  = 0     ! number of timesteps in current event
  integer :: dry_step   = 0     ! number of timesteps in current event
  real    :: totalwat   = 0.0   ! total soil water [mm]
  real    :: tw0        = 0.0   ! initial total soil water [mm]
  real    :: errwat     = 0.0   ! water balance error at each timestep [mm]


!---------------------------------------------------------------------
!  read input file
!---------------------------------------------------------------------

  open(30, file="namelist.input", form="formatted")
   read(30, timing)
   read(30, forcing)
   read(30, structure)
   read(30, uniform_initial)
   read(30, options)
  close(30)

!---------------------------------------------------------------------
!  allocate for dynamic levels
!---------------------------------------------------------------------
  allocate (IMELT (-nsnow+1:0    ))   !phase change index [1-melt; 2-freeze]
  allocate (zsoil (       1:nsoil))   !depth of layer-bottom from soil surface
  allocate (ZSNSO (-nsnow+1:nsoil))   !depth of snow/soil layer-bottom
  allocate (DZSNSO(-nsnow+1:nsoil))   !snow/soil layer thickness [m]
  allocate (BTRANI(       1:nsoil))   !!soil water stress factor (0 to 1)
  allocate (FICEOLD(-nsnow+1:0   ))   !ice fraction at last timestep
  allocate (smceq (       1:nsoil))   !equilibrium soil water  content [m3/m3]
  allocate (SNICE (-nsnow+1:0    ))   !snow layer ice [mm]
  allocate (SNLIQ (-nsnow+1:0    ))   !snow layer liquid water [mm]
  allocate (STC   (-nsnow+1:nsoil))   !snow/soil layer temperature [k]
  allocate (SICE  (       1:nsoil))   !soil ice content [m3/m3]
  allocate (SH2O  (       1:nsoil))   !soil liquid water content [m3/m3]
  allocate (SMC   (       1:nsoil))   !total soil water content [m3/m3]
  allocate (ETRANI(       1:nsoil))   !transpiration rate (mm/s) [+]
  allocate (WCND  (       1:nsoil))   !hydraulic conductivity (m/s)

! thermoprop new vars
  allocate (DF    (-nsnow+1:nsoil))
  allocate (HCPCT (-nsnow+1:nsoil))
  allocate (SNICEV(-nsnow+1:0    ))
  allocate (SNLIQV(-nsnow+1:0    ))
  allocate (EPORE (-nsnow+1:0    ))
  allocate (FACT  (-nsnow+1:nsoil))


!---------------------------------------------------------------------
!  read input file, part 2: initialize
!---------------------------------------------------------------------
  if(structure_option == 1) then       ! user-defined levels
    open(30, file="namelist.input", form="formatted")
     read(30, fixed_initial)
    close(30)
  end if
  ZSNSO(-2:0) = 0.0
  ZSNSO(1:4) = zsoil(1:4)

  if(initial_uniform) then
    SH2O = initial_sh2o_value
    SICE = initial_sice_value
  end if

  SOILTYPE(1:4) = isltyp

!!!============================================= READ Table parameter values
    CALL NOAHMP_OPTIONS(IDVEG  ,IOPT_CRS  ,IOPT_BTR  ,IOPT_RUN  ,IOPT_SFC  ,IOPT_FRZ , &
                IOPT_INF  ,IOPT_RAD  ,IOPT_ALB  ,IOPT_SNF  ,IOPT_TBOT, IOPT_STC  ,     &
                IOPT_RSF  ,IOPT_SOIL ,IOPT_PEDO ,IOPT_CROP ,IOPT_IRR , IOPT_IRRM ,     &
                IOPT_INFDV,IOPT_TDRN )

!---------------------------------------------------------------------
!  read parameter tables
!---------------------------------------------------------------------
    call read_mp_veg_parameters("MODIFIED_IGBP_MODIS_NOAH")
    call read_mp_soil_parameters()
    call read_mp_rad_parameters()
    call read_mp_global_parameters()
    call read_mp_crop_parameters()
    call read_tiledrain_parameters()
    call read_mp_optional_parameters()
    call read_mp_irrigation_parameters()

!---------------------------------------------------------------------
!  transfer parameters  based on TRANSFER_MP_PARAMETERS
!---------------------------------------------------------------------
  parameters%ISWATER   =   ISWATER_TABLE
  parameters%ISBARREN  =  ISBARREN_TABLE
  parameters%ISICE     =     ISICE_TABLE
  parameters%ISCROP    =    ISCROP_TABLE
  parameters%EBLFOREST = EBLFOREST_TABLE
  parameters%URBAN_FLAG = .FALSE.

!------------------------------------------------------------------------------------------!
! Transfer veg parameters
!------------------------------------------------------------------------------------------!

  parameters%CH2OP  =  CH2OP_TABLE(VEGTYPE)       !maximum intercepted h2o per unit lai+sai (mm)
  parameters%DLEAF  =  DLEAF_TABLE(VEGTYPE)       !characteristic leaf dimension (m)
  parameters%Z0MVT  =  Z0MVT_TABLE(VEGTYPE)       !momentum roughness length (m)
  parameters%HVT    =    HVT_TABLE(VEGTYPE)       !top of canopy (m)
  parameters%HVB    =    HVB_TABLE(VEGTYPE)       !bottom of canopy (m)
  parameters%DEN    =    DEN_TABLE(VEGTYPE)       !tree density (no. of trunks per m2)
  parameters%RC     =     RC_TABLE(VEGTYPE)       !tree crown radius (m)
  parameters%MFSNO  =  MFSNO_TABLE(VEGTYPE)       !snowmelt m parameter ()
  parameters%SCFFAC = SCFFAC_TABLE(VEGTYPE)       !snow cover factor (m) (originally hard-coded 2.5*z0 in SCF formulation)
  parameters%SAIM   =   SAIM_TABLE(VEGTYPE,:)     !monthly stem area index, one-sided
  parameters%LAIM   =   LAIM_TABLE(VEGTYPE,:)     !monthly leaf area index, one-sided
  parameters%SLA    =    SLA_TABLE(VEGTYPE)       !single-side leaf area per Kg [m2/kg]
  parameters%DILEFC = DILEFC_TABLE(VEGTYPE)       !coeficient for leaf stress death [1/s]
  parameters%DILEFW = DILEFW_TABLE(VEGTYPE)       !coeficient for leaf stress death [1/s]
  parameters%FRAGR  =  FRAGR_TABLE(VEGTYPE)       !fraction of growth respiration  !original was 0.3 
  parameters%LTOVRC = LTOVRC_TABLE(VEGTYPE)       !leaf turnover [1/s]

  parameters%C3PSN  =  C3PSN_TABLE(VEGTYPE)       !photosynthetic pathway: 0. = c4, 1. = c3
  parameters%KC25   =   KC25_TABLE(VEGTYPE)       !co2 michaelis-menten constant at 25c (pa)
  parameters%AKC    =    AKC_TABLE(VEGTYPE)       !q10 for kc25
  parameters%KO25   =   KO25_TABLE(VEGTYPE)       !o2 michaelis-menten constant at 25c (pa)
  parameters%AKO    =    AKO_TABLE(VEGTYPE)       !q10 for ko25
  parameters%VCMX25 = VCMX25_TABLE(VEGTYPE)       !maximum rate of carboxylation at 25c (umol co2/m**2/s)
  parameters%AVCMX  =  AVCMX_TABLE(VEGTYPE)       !q10 for vcmx25
  parameters%BP     =     BP_TABLE(VEGTYPE)       !minimum leaf conductance (umol/m**2/s)
  parameters%MP     =     MP_TABLE(VEGTYPE)       !slope of conductance-to-photosynthesis relationship
  parameters%QE25   =   QE25_TABLE(VEGTYPE)       !quantum efficiency at 25c (umol co2 / umol photon)
  parameters%AQE    =    AQE_TABLE(VEGTYPE)       !q10 for qe25
  parameters%RMF25  =  RMF25_TABLE(VEGTYPE)       !leaf maintenance respiration at 25c (umol co2/m**2/s)
  parameters%RMS25  =  RMS25_TABLE(VEGTYPE)       !stem maintenance respiration at 25c (umol co2/kg bio/s)
  parameters%RMR25  =  RMR25_TABLE(VEGTYPE)       !root maintenance respiration at 25c (umol co2/kg bio/s)
  parameters%ARM    =    ARM_TABLE(VEGTYPE)       !q10 for maintenance respiration
  parameters%FOLNMX = FOLNMX_TABLE(VEGTYPE)       !foliage nitrogen concentration when f(n)=1 (%)
  parameters%TMIN   =   TMIN_TABLE(VEGTYPE)       !minimum temperature for photosynthesis (k)

  parameters%XL     =     XL_TABLE(VEGTYPE)       !leaf/stem orientation index
  parameters%RHOL   =   RHOL_TABLE(VEGTYPE,:)     !leaf reflectance: 1=vis, 2=nir
  parameters%RHOS   =   RHOS_TABLE(VEGTYPE,:)     !stem reflectance: 1=vis, 2=nir
  parameters%TAUL   =   TAUL_TABLE(VEGTYPE,:)     !leaf transmittance: 1=vis, 2=nir
  parameters%TAUS   =   TAUS_TABLE(VEGTYPE,:)     !stem transmittance: 1=vis, 2=nir

  parameters%MRP    =    MRP_TABLE(VEGTYPE)       !microbial respiration parameter (umol co2 /kg c/ s)
  parameters%CWPVT  =  CWPVT_TABLE(VEGTYPE)       !empirical canopy wind parameter

  parameters%WRRAT  =  WRRAT_TABLE(VEGTYPE)       !wood to non-wood ratio
  parameters%WDPOOL = WDPOOL_TABLE(VEGTYPE)       !wood pool (switch 1 or 0) depending on woody or not [-]
  parameters%TDLEF  =  TDLEF_TABLE(VEGTYPE)       !characteristic T for leaf freezing [K]

  parameters%NROOT  =  NROOT_TABLE(VEGTYPE)       !number of soil layers with root present
  parameters%RGL    =    RGL_TABLE(VEGTYPE)       !Parameter used in radiation stress function
  parameters%RSMIN  =     RS_TABLE(VEGTYPE)       !Minimum stomatal resistance [s m-1]
  parameters%HS     =     HS_TABLE(VEGTYPE)       !Parameter used in vapor pressure deficit function
  parameters%TOPT   =   TOPT_TABLE(VEGTYPE)       !Optimum transpiration air temperature [K]
  parameters%RSMAX  =  RSMAX_TABLE(VEGTYPE)       !Maximal stomatal resistance [s m-1]

!------------------------------------------------------------------------------------------!
! Transfer rad parameters
!------------------------------------------------------------------------------------------!

   parameters%ALBSAT    = ALBSAT_TABLE(SOILCOLOR,:)
   parameters%ALBDRY    = ALBDRY_TABLE(SOILCOLOR,:)
   parameters%ALBICE    = ALBICE_TABLE
   parameters%ALBLAK    = ALBLAK_TABLE
   parameters%OMEGAS    = OMEGAS_TABLE
   parameters%BETADS    = BETADS_TABLE
   parameters%BETAIS    = BETAIS_TABLE
   parameters%EG        = EG_TABLE

!------------------------------------------------------------------------------------------!
! Transfer crop parameters
!------------------------------------------------------------------------------------------!
 IF(CROPTYPE > 0) THEN
   parameters%PLTDAY    =    PLTDAY_TABLE(CROPTYPE)    ! Planting date
   parameters%HSDAY     =     HSDAY_TABLE(CROPTYPE)    ! Harvest date
   parameters%PLANTPOP  =  PLANTPOP_TABLE(CROPTYPE)    ! Plant density [per ha] - used?
   parameters%IRRI      =      IRRI_TABLE(CROPTYPE)    ! Irrigation strategy 0= non-irrigation 1=irrigation (no water-stress)
   parameters%GDDTBASE  =  GDDTBASE_TABLE(CROPTYPE)    ! Base temperature for GDD accumulation [C]
   parameters%GDDTCUT   =   GDDTCUT_TABLE(CROPTYPE)    ! Upper temperature for GDD accumulation [C]
   parameters%GDDS1     =     GDDS1_TABLE(CROPTYPE)    ! GDD from seeding to emergence
   parameters%GDDS2     =     GDDS2_TABLE(CROPTYPE)    ! GDD from seeding to initial vegetative 
   parameters%GDDS3     =     GDDS3_TABLE(CROPTYPE)    ! GDD from seeding to post vegetative 
   parameters%GDDS4     =     GDDS4_TABLE(CROPTYPE)    ! GDD from seeding to intial reproductive
   parameters%GDDS5     =     GDDS5_TABLE(CROPTYPE)    ! GDD from seeding to pysical maturity 
   parameters%C3PSN     =     C3PSNI_TABLE(CROPTYPE)   ! parameters from stomata ! Zhe Zhang 2020-07-13
   parameters%KC25      =      KC25I_TABLE(CROPTYPE)
   parameters%AKC       =       AKCI_TABLE(CROPTYPE)
   parameters%KO25      =      KO25I_TABLE(CROPTYPE)
   parameters%AKO       =       AKOI_TABLE(CROPTYPE)
   parameters%AVCMX     =     AVCMXI_TABLE(CROPTYPE)
   parameters%VCMX25    =    VCMX25I_TABLE(CROPTYPE)
   parameters%BP        =        BPI_TABLE(CROPTYPE)
   parameters%MP        =        MPI_TABLE(CROPTYPE)
   parameters%FOLNMX    =    FOLNMXI_TABLE(CROPTYPE)
   parameters%QE25      =      QE25I_TABLE(CROPTYPE)   ! ends here
   parameters%C3C4      =      C3C4_TABLE(CROPTYPE)    ! photosynthetic pathway:  1. = c3 2. = c4
   parameters%AREF      =      AREF_TABLE(CROPTYPE)    ! reference maximum CO2 assimulation rate 
   parameters%PSNRF     =     PSNRF_TABLE(CROPTYPE)    ! CO2 assimulation reduction factor(0-1) (caused by non-modeling part,e.g.pest,weeds)
   parameters%I2PAR     =     I2PAR_TABLE(CROPTYPE)    ! Fraction of incoming solar radiation to photosynthetically active radiation
   parameters%TASSIM0   =   TASSIM0_TABLE(CROPTYPE)    ! Minimum temperature for CO2 assimulation [C]
   parameters%TASSIM1   =   TASSIM1_TABLE(CROPTYPE)    ! CO2 assimulation linearly increasing until temperature reaches T1 [C]
   parameters%TASSIM2   =   TASSIM2_TABLE(CROPTYPE)    ! CO2 assmilation rate remain at Aref until temperature reaches T2 [C]
   parameters%K         =         K_TABLE(CROPTYPE)    ! light extinction coefficient
   parameters%EPSI      =      EPSI_TABLE(CROPTYPE)    ! initial light use efficiency
   parameters%Q10MR     =     Q10MR_TABLE(CROPTYPE)    ! q10 for maintainance respiration
   parameters%FOLN_MX   =   FOLN_MX_TABLE(CROPTYPE)    ! foliage nitrogen concentration when f(n)=1 (%)
   parameters%LEFREEZ   =   LEFREEZ_TABLE(CROPTYPE)    ! characteristic T for leaf freezing [K]
   parameters%DILE_FC   =   DILE_FC_TABLE(CROPTYPE,:)  ! coeficient for temperature leaf stress death [1/s]
   parameters%DILE_FW   =   DILE_FW_TABLE(CROPTYPE,:)  ! coeficient for water leaf stress death [1/s]
   parameters%FRA_GR    =    FRA_GR_TABLE(CROPTYPE)    ! fraction of growth respiration
   parameters%LF_OVRC   =   LF_OVRC_TABLE(CROPTYPE,:)  ! fraction of leaf turnover  [1/s]
   parameters%ST_OVRC   =   ST_OVRC_TABLE(CROPTYPE,:)  ! fraction of stem turnover  [1/s]
   parameters%RT_OVRC   =   RT_OVRC_TABLE(CROPTYPE,:)  ! fraction of root tunrover  [1/s]
   parameters%LFMR25    =    LFMR25_TABLE(CROPTYPE)    ! leaf maintenance respiration at 25C [umol CO2/m**2  /s]
   parameters%STMR25    =    STMR25_TABLE(CROPTYPE)    ! stem maintenance respiration at 25C [umol CO2/kg bio/s]
   parameters%RTMR25    =    RTMR25_TABLE(CROPTYPE)    ! root maintenance respiration at 25C [umol CO2/kg bio/s]
   parameters%GRAINMR25 = GRAINMR25_TABLE(CROPTYPE)    ! grain maintenance respiration at 25C [umol CO2/kg bio/s]
   parameters%LFPT      =      LFPT_TABLE(CROPTYPE,:)  ! fraction of carbohydrate flux to leaf
   parameters%STPT      =      STPT_TABLE(CROPTYPE,:)  ! fraction of carbohydrate flux to stem
   parameters%RTPT      =      RTPT_TABLE(CROPTYPE,:)  ! fraction of carbohydrate flux to root
   parameters%GRAINPT   =   GRAINPT_TABLE(CROPTYPE,:)  ! fraction of carbohydrate flux to grain
   parameters%LFCT      =      LFCT_TABLE(CROPTYPE,:)  ! fraction of translocation to grain ! Zhe Zhang 2020-07-13
   parameters%STCT      =      STCT_TABLE(CROPTYPE,:)  ! fraction of translocation to grain
   parameters%RTCT      =      RTCT_TABLE(CROPTYPE,:)  ! fraction of translocation to grain
   parameters%BIO2LAI   =   BIO2LAI_TABLE(CROPTYPE)    ! leaf are per living leaf biomass [m^2/kg]
 END IF
!------------------------------------------------------------------------------------------!
! Transfer global parameters
!------------------------------------------------------------------------------------------!

   parameters%CO2        =         CO2_TABLE
   parameters%O2         =          O2_TABLE
   parameters%TIMEAN     =      TIMEAN_TABLE
   parameters%FSATMX     =      FSATMX_TABLE
   parameters%Z0SNO      =       Z0SNO_TABLE
   parameters%SSI        =         SSI_TABLE
   parameters%SNOW_RET_FAC = SNOW_RET_FAC_TABLE
   parameters%SNOW_EMIS  =   SNOW_EMIS_TABLE
   parameters%SWEMX        =     SWEMX_TABLE
   parameters%TAU0         =      TAU0_TABLE
   parameters%GRAIN_GROWTH = GRAIN_GROWTH_TABLE
   parameters%EXTRA_GROWTH = EXTRA_GROWTH_TABLE
   parameters%DIRT_SOOT    =    DIRT_SOOT_TABLE
   parameters%BATS_COSZ    =    BATS_COSZ_TABLE
   parameters%BATS_VIS_NEW = BATS_VIS_NEW_TABLE
   parameters%BATS_NIR_NEW = BATS_NIR_NEW_TABLE
   parameters%BATS_VIS_AGE = BATS_VIS_AGE_TABLE
   parameters%BATS_NIR_AGE = BATS_NIR_AGE_TABLE
   parameters%BATS_VIS_DIR = BATS_VIS_DIR_TABLE
   parameters%BATS_NIR_DIR = BATS_NIR_DIR_TABLE
   parameters%RSURF_SNOW =  RSURF_SNOW_TABLE
   parameters%RSURF_EXP  =   RSURF_EXP_TABLE

! ----------------------------------------------------------------------
!  Transfer soil parameters
! ----------------------------------------------------------------------

    do isoil = 1, size(soiltype)
      parameters%BEXP(isoil)   = BEXP_TABLE   (SOILTYPE(isoil))
      parameters%DKSAT(isoil)  = DKSAT_TABLE  (SOILTYPE(isoil))
      parameters%DWSAT(isoil)  = DWSAT_TABLE  (SOILTYPE(isoil))
      parameters%PSISAT(isoil) = PSISAT_TABLE (SOILTYPE(isoil))
      parameters%QUARTZ(isoil) = QUARTZ_TABLE (SOILTYPE(isoil))
      parameters%SMCDRY(isoil) = SMCDRY_TABLE (SOILTYPE(isoil))
      parameters%SMCMAX(isoil) = SMCMAX_TABLE (SOILTYPE(isoil))
      parameters%SMCREF(isoil) = SMCREF_TABLE (SOILTYPE(isoil))
      parameters%SMCWLT(isoil) = SMCWLT_TABLE (SOILTYPE(isoil))
    end do
    parameters%F1     = F1_TABLE(SOILTYPE(1))
    parameters%REFDK  = REFDK_TABLE
    parameters%REFKDT = REFKDT_TABLE
    parameters%BVIC   = BVIC_TABLE(SOILTYPE(1))
    parameters%AXAJ   = AXAJ_TABLE(SOILTYPE(1))
    parameters%BXAJ   = BXAJ_TABLE(SOILTYPE(1))
    parameters%XXAJ   = XXAJ_TABLE(SOILTYPE(1))
    parameters%BDVIC  = BDVIC_TABLE(SOILTYPE(1))
    parameters%GDVIC  = GDVIC_TABLE(SOILTYPE(1))
    parameters%BBVIC  = BBVIC_TABLE(SOILTYPE(1))

!------------------------------------------------------------------------------------------!
! Transfer irrigation parameters
!------------------------------------------------------------------------------------------!
    parameters%IRR_FRAC   = IRR_FRAC_TABLE      ! irrigation Fraction
    parameters%IRR_HAR    = IRR_HAR_TABLE       ! number of days before harvest date to stop irrigation 
    parameters%IRR_LAI    = IRR_LAI_TABLE       ! minimum lai to trigger irrigation
    parameters%IRR_MAD    = IRR_MAD_TABLE       ! management allowable deficit (0-1)
    parameters%FILOSS     = FILOSS_TABLE        ! fraction of flood irrigation loss (0-1) 
    parameters%SPRIR_RATE = SPRIR_RATE_TABLE    ! mm/h, sprinkler irrigation rate
    parameters%MICIR_RATE = MICIR_RATE_TABLE    ! mm/h, micro irrigation rate
    parameters%FIRTFAC    = FIRTFAC_TABLE       ! flood application rate factor
    parameters%IR_RAIN    = IR_RAIN_TABLE       ! maximum precipitation to stop irrigation trigger

!------------------------------------------------------------------------------------------!
! Transfer tiledrain parameters
!------------------------------------------------------------------------------------------!
   parameters%KLAT_FAC        = KLAT_FAC_TABLE(SOILTYPE(1))
   parameters%TDSMC_FAC       = TDSMCFAC_TABLE(SOILTYPE(1))
   parameters%TD_DC           = TD_DC_TABLE(SOILTYPE(1))
   parameters%TD_DCOEF        = TD_DCOEF_TABLE(SOILTYPE(1))
   parameters%TD_RADI         = TD_RADI_TABLE(SOILTYPE(1))
   parameters%TD_SPAC         = TD_SPAC_TABLE(SOILTYPE(1))
   parameters%TD_DDRAIN       = TD_DDRAIN_TABLE(SOILTYPE(1))
   parameters%TD_DEPTH        = TD_DEPTH_TABLE(SOILTYPE(1))
   parameters%TD_ADEPTH       = TD_ADEPTH_TABLE(SOILTYPE(1))
   parameters%DRAIN_LAYER_OPT = DRAIN_LAYER_OPT_TABLE
   parameters%TD_D            = TD_D_TABLE(SOILTYPE(1))

! ----------------------------------------------------------------------
! Transfer GENPARM parameters
! ----------------------------------------------------------------------
    parameters%CSOIL  = CSOIL_TABLE
    parameters%ZBOT   = ZBOT_TABLE
    parameters%CZIL   = CZIL_TABLE

    FRZK   = FRZK_TABLE
    parameters%KDT    = parameters%REFKDT * parameters%DKSAT(1) / parameters%REFDK
    parameters%SLOPE  = SLOPE_TABLE(SLOPETYPE)

    IF(parameters%URBAN_FLAG)THEN  ! Hardcoding some urban parameters for soil
       parameters%SMCMAX = 0.45
       parameters%SMCREF = 0.42
       parameters%SMCWLT = 0.40
       parameters%SMCDRY = 0.40
       parameters%CSOIL  = 3.E6
    ENDIF

! adjust FRZK parameter to actual soil type: FRZK * FRZFACT

    IF(SOILTYPE(1) /= 14) then
      FRZFACT = (parameters%SMCMAX(1) / parameters%SMCREF(1)) * (0.412 / 0.468)
      parameters%FRZX = FRZK * FRZFACT
    END IF

! ----------------------------------------

!---------------------------------------------------------------------
!  initialize required variables
!---------------------------------------------------------------------
! input used to adjust for snow and non-snow cases
if (runsnow) then
  SFCTMP = 265.0 !model-level temperature (k)
  FB_snow = 0.5
  TV = 268.0
  TG = 270.0
  IMELT = 2  ! freeze
  CANLIQ = 0.0
  CANICE = 0.0
  STC(1:4) = 265.0
  STC(-2:0) = 0.0
  SH2O(1:4) = 0.03
  SICE(1:4) = 0.2
  FSNO = 0.8
else
  SFCTMP = 298.0 !model-level temperature (k)
  FB_snow = 0.0
  TV = 293.0
  TG = 285.0
  IMELT = 1 ! melt
  CANLIQ = 0.0
  CANICE = 0.0
  STC(1:4) = 298.0
  STC(-2:0) = 0.0
  SH2O(1:4) = 0.2
  SICE(1:4) = 0.03
  FSNO = 0.0
end if
! others
  IST = 1   !surface type 1-soil; 2-lake
  DX = 4000.0  ! grid spacing 4km
  UU = uwind ! wind speed m/s
  VV = vwind
  SFCPRS = sfcpres ! surface pressure: 900hPa
  FCEV = fcev_e  !canopy evaporation (w/m2) [+ to atm ]
  FCTR = fctr_e  !transpiration (w/m2) [+ to atm]
  FGEV = fgev_e  ! soil evap heat (w/m2) [+ to atm]
  QPRECC = 0.0  ! not used
  QPRECL = 0.0  ! not used
  LAI = parameters%LAIM(6) ! June LAI as an example
  SAI = parameters%SAIM(6) ! June SAI
  ELAI = LAI * (1. - FB_snow) !leaf area index, after burying by snow
  ESAI = SAI * (1. - FB_snow) !!stem area index, after burying by snow 
  PONDING = 0.0
  FICEOLD = 0.0
  FVEG = shdmax / 100.0        !yearly max vegetation fraction
  IF(FVEG <= 0.05) FVEG = 0.05
  SMCEQ(1:4) = 0.3 ! used only for MMF, so set to fixed value
  BDFALL = 120.0       !bulk density of snowfall (kg/m3)
  FP = 0.9 ! (not used) fraction of the gridcell that receives precipitation
  RAIN = 0.0 ! total rain
  SNOW = 0.0  !total snowfall (mm/s)
  QRAIN = RAIN * 0.99
  QSNOW = SNOW * 0.9
  SNOWHIN = QSNOW / BDFALL ! m/s
  ISNOW = 0
  SNOWH = 0.0
  SNEQV = 0.0
  SNICE = 0.0
  SNLIQ = 0.0
  SMC   = SH2O + SICE  ! initial volumetric soil water
  ZWT   = 2.5
  WA    = 0.0
  WT    = 0.0
  WSLAKE = 0.0
  SMCWTD    = 0.3          ! should only be needed for run=5
  DEEPRECH  = 0.0          ! should only be needed for run=5
  RECH  = 0.0    ! should only be needed for run=5
  CMC  = 0.0
  ECAN = 0.0
  ETRAN = 0.0
  FWET = 0.0  ! canopy fraction wet or snow
  RUNSRF = 0.0
  RUNSUB = 0.0
  QDIS = 0.0
  QIN  = 0.0
  PONDING1 = 0.0
  PONDING2 = 0.0
  QSNBOT = 0.0
  QTLDRN = 0.0
  QINSUR = 0.0
  QSEVA = 0.0
  ETRANI = 0.0
  QSNFRO = 0.0
  QSNSUB = 0.0
  SNOFLOW = 0.0
  QSDEW = 0.0
  QDRAIN = 0.0
  FCRMAX = 0.0
  WCND = 0.0
#ifdef WRF_HYDRO
  sfcheadrt = 0.0
  WATBLED  0.0
#endif
  VEGTYP = vegtype
  QINTR  = 0.0
  QDRIPR = 0.0
  QTHROR = 0.0
  QINTS  = 0.0
  QDRIPS = 0.0
  QTHROS = 0.0
  PAHV   = 0.0
  PAHG   = 0.0
  PAHB   = 0.0
! thermoprop new vars
  LAT    = 120.0 * 3.1415 / 180.0  ! 120E -> radian
  DF     = 0.0
  HCPCT  = 0.0
  SNICEV = 0.0
  SNLIQV = 0.0
  EPORE  = 0.0
  FACT   = 0.0
! radiation new vars
  ICE    = 0
  COSZ   = 0.5
  SOLAD(1) = SWDOWN*0.7*0.5     ! direct  vis
  SOLAD(2) = SWDOWN*0.7*0.5     ! direct  nir
  SOLAI(1) = SWDOWN*0.3*0.5     ! diffuse vis
  SOLAI(2) = SWDOWN*0.3*0.5     ! diffuse nir
  SNEQVO = 0.0
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

!====== vege_flux new vars
!!! in 
  CM = 0.1
  CH = 0.01
  LWDN = LWDOWN
  UR     = MAX( SQRT(UU**2.0 + VV**2.0), 1.0 )
  THAIR  = SFCTMP * (SFCPRS/SFCPRS)**(RAIR/CPAIR) 
  QAIR = Q2
  EAIR   = QAIR*SFCPRS / (0.622+0.378*QAIR)
  RHOAIR = (SFCPRS-0.378*EAIR) / (RAIR*SFCTMP)
   CWP = parameters%CWPVT
   Z0MG = 0.002 * (1.0-FSNO) + FSNO * parameters%Z0SNO
! needs to update each time step
  VAI = ELAI + ESAI
  VEG = .FALSE.
  IF(VAI > 0.0) VEG = .TRUE.
  IF (TV .GT. TFRZ) THEN  
      LATHEAV = HVAP   
      frozen_canopy = .false.
   ELSE
      LATHEAV = HSUB
      frozen_canopy = .true.
   END IF
   GAMMAV = CPAIR*SFCPRS/(0.622*LATHEAV)
   IF (TG .GT. TFRZ) THEN
      LATHEAG = HVAP
      frozen_ground = .false.
   ELSE
      LATHEAG = HSUB
      frozen_ground = .true.
   END IF
   GAMMAG = CPAIR*SFCPRS/(0.622*LATHEAG)
   ZPDG  = SNOWH
   IF(VEG) THEN
      Z0M  = parameters%Z0MVT
      ZPD  = 0.65 * parameters%HVT
      IF(SNOWH.GT.ZPD) ZPD  = SNOWH
   ELSE
      Z0M  = Z0MG
      ZPD  = ZPDG
   END IF
   ZLVL = MAX(ZPD,parameters%HVT) + 10.0
   IF(ZPDG >= ZLVL) ZLVL = ZPDG + 10.0
   EMV = 1.0 - EXP(-(ELAI+ESAI)/1.0)
   EMG = parameters%EG(IST)*(1.0-FSNO) + parameters%SNOW_EMIS*FSNO
   RSURF = FSNO * 1.0 + (1.0-FSNO)* EXP(8.25-4.225*(MAX(0.0,SH2O(1)/parameters%SMCMAX(1)))) !Sellers (1992)
   IGS = 1.0
   FOLN   = 1.0 
   CO2AIR = 395.e-06 * SFCPRS
   O2AIR  = 0.209 * SFCPRS
   BTRAN  = 0.2
   PSI   = -parameters%PSISAT(1)*(MAX(0.01,SH2O(1))/parameters%SMCMAX(1))**(-parameters%BEXP(1)) 
   RHSUR = FSNO + (1.0-FSNO) * EXP(PSI*GRAV/(RW*TG))
   DZ8W = 20.0
   QC = 0.0005
   PSFC = SFCPRS
!!! inout
   ! TV
   TAH = TV
   EAH = Q2 * SFCPRS / (0.622 + 0.378*Q2)
   TGV = TG
   CMV = CM
   CHV = CH
   QSFC = 0.622*EAIR/(PSFC-0.378*EAIR) 
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
   TGB = TG
   CMB = CM
   CHB = CH
   TAUXB = 0.0
   TAUYB = 0.0
   IRB = 0.0
   SHB = 0.0
   EVB = 0.0
   GHB = 0.0
   T2MB = 0.0
   Q2B = 0.0
   CHB2 = 0.0
   SSOIL = 0.0
!====== bare_flux end

!=== TSNOSOI new vars
if (runsnow) then
   TBOT = 270.0
else
   TBOT = 290.0
endif


!==== TSNOSOI end

!============================
  QVAP = MAX( FGEV/LATHEAG, 0.)       ! positive part of fgev; Barlage change to ground v3.6
  QDEW = ABS( MIN(FGEV/LATHEAG, 0.))  ! negative part of fgev
  BTRANI(1:nsoil) = 0.2 ! 0~1
  EDIR = QVAP - QDEW   ! net soil evaporation

!============= irrigation related
  IF (OPT_IRR .gt. 0) then
     IRRFRA = 1.0  ! irrigation fraction
     CROPLU = .true.
  ELSE
     IRRFRA = 0.0
     CROPLU = .false.
  END IF  
  IF(OPT_IRRM .EQ. 0) THEN
      SIFAC = 0.3
      MIFAC = 0.3
      FIFAC = 0.4
     IRAMTFI = 0.25
     IRAMTMI = 0.25
     IRAMTSI = 0.5
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  ELSE IF(OPT_IRRM .EQ. 1) THEN
      SIFAC = 1.
      MIFAC = 0
      FIFAC = 0.
     IRAMTFI = 0.0
     IRAMTMI = 0.0
     IRAMTSI = 0.5
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  ELSE IF(OPT_IRRM .EQ. 2) THEN ! micro
      SIFAC = 0.
      MIFAC = 1.
      FIFAC = 0.
     IRAMTFI = 0.0
     IRAMTMI = 0.5
     IRAMTSI = 0.0
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  ELSE IF(OPT_IRRM .EQ. 3) THEN ! flood
      SIFAC = 0.
      MIFAC = 0.
      FIFAC = 1.
     IRAMTFI = 0.5
     IRAMTMI = 0.0
     IRAMTSI = 0.0
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  END IF
  IRCNTSI = 0
  IRCNTMI = 0
  IRCNTFI = 0
  IREVPLOS = 0.0
  FIRR = 0.0
  EIRR = 0.0
  IF(OPT_TDRN .gt. 0) THEN
      TDFRACMP = 0.5
      ZWT   = 0.2  ! to allow the drainage effect to show up
  ELSE
      TDFRACMP = 0.0
  END IF
!================= irrigation end




  ntime      =  nint(maxtime * 3600.0 / dt)
  rain_steps = rain_duration * 3600.0 / dt
  dry_steps  =  dry_duration * 3600.0 / dt

! prevent too large SMC initial values
  DO isoil = 1,nsoil
     IF (SMC(isoil) .gt. parameters%SMCMAX(isoil)) THEN
        SH2O(isoil) = parameters%SMCMAX(isoil) * SH2O(isoil) / SMC(isoil)
        SMC(isoil) = parameters%SMCMAX(isoil)
        SICE(isoil) = max(0.0,SMC(isoil) - SH2O(isoil))
     ENDIF
  END DO

!!!!!!========= initialization complete ==================================

!---------------------------------------------------------------------
! create output file and add initial values
!---------------------------------------------------------------------

  call initialize_output(output_filename, ntime+1, nsoil, nsnow)
  call add_to_output(0,NSOIL,NSNOW,ISNOW,CANLIQ,CANICE,TV,SNOWH,SNEQV,&
                     SNICE,SNLIQ,STC,ZSNSO,SH2O,SMC,SICE,ZWT,WA,WT,DZSNSO,&
                     WSLAKE,SMCWTD,DEEPRECH,RECH,IRAMTFI,IRAMTMI,IRFIRATE,IRMIRATE,&
                     CMC,ECAN,ETRAN,FWET,RUNSRF,RUNSUB,QIN,QDIS,PONDING1,PONDING2,&
                     QSNBOT,QTLDRN,QINSUR,QSEVA,QSDEW,QSNFRO,QSNSUB,ETRANI,&
                     WCND,QDRAIN,SNOFLOW,FCRMAX,FICEOLD,errwat,QRAIN,QSNOW,QVAP,&
                     IRAMTSI,IRSIRATE,IRCNTSI,IRCNTMI,IRCNTFI,RAIN,SNOW,IREVPLOS,FIRR,EIRR,&
                     SNOWHIN,TG,QINTR,QDRIPR,QTHROR,QINTS,QDRIPS,QTHROS,PAHV,PAHG,PAHB,EDIR,&
                     DF,HCPCT,SNICEV,SNLIQV,EPORE,FACT,FSUN,LAISUN,LAISHA,PARSUN,PARSHA,SAV,&
                     SAG,FSA,FSR,FSRV,FSRG,BGAP,WGAP,ALBSND,ALBSNI,ALBOLD,TAUSS,SNEQVO,&
                     TAH,TGV,EAH,CMV,CM,CHV,CH,QSFC,RSSUN,RSSHA,TAUXV,TAUYV,IRG,IRC,SHG,SHC,&
                     EVG,EVC,TR,GHV,T2MV,PSNSUN,PSNSHA,Q2V,CHV2,CHLEAF,CHUC,&
                     TGB,CMB,CHB,TAUXB,TAUYB,IRB,SHB,EVB,GHB,T2MB,Q2B,CHB2,SSOIL)

!---------------------------------------------------------------------
! start the time loop
!---------------------------------------------------------------------

  do itime = 1, ntime
 
  tw0 = sum(DZSNSO(1:nsoil)*SMC*1000.0) + SNEQV + WA + CANLIQ + CANICE ! [mm] 

     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
     IREVPLOS = 0.0 
     FIRR     = 0.0
     EIRR     = 0.0

  !---------------------------------------------------------------------
  ! calculate the input water
  !---------------------------------------------------------------------

    if(raining) then
      RAIN = rainrate/3600.0 ! input water mm/s
      rain_step = rain_step + 1
      if(rain_step == rain_steps) then      ! event length met
        rain_step = 0
        raining   = .false.
      end if
    else
      RAIN   = 0.0                        ! stop water input [m/s]
      dry_step = dry_step + 1
      if(dry_step == dry_steps) then        ! between event length met
        dry_step = 0
        raining  = .true.
      end if
    end if

   if (runsnow) then
     SNOW = RAIN * 1.0
     RAIN = 0.0
   else
     SNOW = 0.0
   end if 


!!!============================================= Start the original NoahMP Subroutine ==========================================
! extract from NOAHMP_SFLX
! snow/soil layer thickness (m)
     DO IZ = ISNOW+1, NSOIL
         IF(IZ == ISNOW+1) THEN
           DZSNSO(IZ) = - ZSNSO(IZ)
         ELSE
           DZSNSO(IZ) = ZSNSO(IZ-1) - ZSNSO(IZ)
         END IF
     END DO

! extract from phenology to update ELAI and ESAI temporally
  FB_snow = MIN( MAX(SNOWH - parameters%HVB,0.0), parameters%HVT-parameters%HVB ) / &
       MAX(1.0E-06,parameters%HVT-parameters%HVB)
     IF(parameters%HVT> 0.0 .AND. parameters%HVT <= 1.0) THEN          !MB: change to 1.0 and 0.2 to reflect
       FB_snow     = MIN(SNOWH,(parameters%HVT*EXP(-SNOWH/0.2)) )/(parameters%HVT*EXP(-SNOWH/0.2) )
     ENDIF
     ELAI =  LAI*(1.0-FB_snow)
     ESAI =  SAI*(1.0-FB_snow)
     IF (ESAI < 0.05 .and. CROPTYPE == 0) ESAI = 0.0                   ! MB: ESAI CHECK, change to 0.05 v3.6
     IF ((ELAI < 0.05 .OR. ESAI == 0.0) .and. CROPTYPE == 0) ELAI = 0.0  ! MB: LAI CHECK


!!!============================ Irrigation trigger and sprinkler 
! Call triggering function
     IF((CROPLU .EQV. .TRUE.) .AND. (IRRFRA .GE. parameters%IRR_FRAC) .AND. &
       (RAIN .LT. (parameters%IR_RAIN/3600.0)) .AND. ((IRAMTSI+IRAMTMI+IRAMTFI) .EQ. 0.0) )THEN

        CALL TRIGGER_IRRIGATION(parameters,NSOIL,ZSOIL,SH2O,FVEG,JULIAN,IRRFRA,LAI, & !in
                                  SIFAC,MIFAC,FIFAC,                                & !in
                                  IRCNTSI,IRCNTMI,IRCNTFI,                          & !inout
                                  IRAMTSI,IRAMTMI,IRAMTFI)                            !inout
     END IF

! set irrigation off if parameters%IR_RAIN mm/h for this time step and irr triggered last time step
     IF((RAIN .GE. (parameters%IR_RAIN/3600.0)) .OR. (IRRFRA .LT. parameters%IRR_FRAC))THEN
        IRAMTSI = 0.0
        IRAMTMI = 0.0
        IRAMTFI = 0.0
     END IF

! call sprinkler irrigation before CANWAT/PRECIP_HEAT to have canopy interception
     IF((CROPLU .EQV. .TRUE.) .AND. (IRAMTSI .GT. 0.0)) THEN

        CALL SPRINKLER_IRRIGATION(parameters,NSOIL,DT,SH2O,SMC,SICE,& !in
                                  SFCTMP,UU,VV,EAIR,SIFAC,          & !in
                                  IRAMTSI,IREVPLOS,IRSIRATE)          !inout
        RAIN = RAIN + (IRSIRATE*1000.0/DT) ![mm/s]
        ! cooling and humidification due to sprinkler evaporation, per m^2 calculation 
        FIRR     = IREVPLOS*1000.0*HVAP/DT                              ! heat used for evaporation (W/m2)
        EIRR     = IREVPLOS*1000.0/DT                                   ! sprinkler evaporation (mm/s)
     END IF
! call for micro irrigation and flood irrigation are implemented in WATER subroutine
! end irrigation call-prasanth


!!!============================ Canopy interception water & heat
    CALL PRECIP_HEAT(parameters,ILOC   ,JLOC   ,VEGTYP ,DT     ,UU     ,VV     , & !in
                     ELAI   ,ESAI   ,FVEG   ,IST    ,                 & !in
                     BDFALL ,RAIN   ,SNOW   ,FP     ,                 & !in
                     CANLIQ ,CANICE ,TV     ,SFCTMP ,TG     ,         & !in
                     QINTR  ,QDRIPR ,QTHROR ,QINTS  ,QDRIPS ,QTHROS , & !out
                     PAHV   ,PAHG   ,PAHB   ,QRAIN  ,QSNOW  ,SNOWHIN, & !out
	             FWET   ,CMC                                    )   !out


!!!============================ Energy module all

!=== some input variables need to be updated every time step (temporally, will be included in completed Energy subroutine)

  VAI = ELAI + ESAI
  VEG = .FALSE.
  IF(VAI > 0.0) VEG = .TRUE.

  IF (TV .GT. TFRZ) THEN
      LATHEAV = HVAP
      frozen_canopy = .false.
   ELSE
      LATHEAV = HSUB
      frozen_canopy = .true.
   END IF
   GAMMAV = CPAIR*SFCPRS/(0.622*LATHEAV)
   IF (TG .GT. TFRZ) THEN
      LATHEAG = HVAP
      frozen_ground = .false.
   ELSE
      LATHEAG = HSUB
      frozen_ground = .true.
   END IF
   GAMMAG = CPAIR*SFCPRS/(0.622*LATHEAG)
   ZPDG  = SNOWH
   IF(VEG) THEN
      Z0M  = parameters%Z0MVT
      ZPD  = 0.65 * parameters%HVT
      IF(SNOWH.GT.ZPD) ZPD  = SNOWH
   ELSE
      Z0M  = Z0MG
      ZPD  = ZPDG
   END IF
   ZLVL = MAX(ZPD,parameters%HVT) + 10.0
   IF(ZPDG >= ZLVL) ZLVL = ZPDG + 10.0
   RSURF = FSNO * 1.0 + (1.0-FSNO)* EXP(8.25-4.225*(MAX(0.0,SH2O(1)/parameters%SMCMAX(1)))) !Sellers (1992)
   PSI   = -parameters%PSISAT(1)*(MAX(0.01,SH2O(1))/parameters%SMCMAX(1))**(-parameters%BEXP(1))
   RHSUR = FSNO + (1.0-FSNO) * EXP(PSI*GRAV/(RW*TG))
   EMV = 1.0 - EXP(-(ELAI+ESAI)/1.0)
   EMG = parameters%EG(IST)*(1.0-FSNO) + parameters%SNOW_EMIS*FSNO

!=== input variable update end


! Thermal properties of soil, snow, lake, and frozen soil

  CALL THERMOPROP (parameters,NSOIL   ,NSNOW   ,ISNOW   ,IST     ,DZSNSO  , & !in
                   DT      ,SNOWH   ,SNICE   ,SNLIQ   , & !in
                   SMC     ,SH2O    ,TG      ,STC     ,UR      , & !in
                   LAT     ,Z0M     ,ZLVL    ,VEGTYP  , & !in
                   DF      ,HCPCT   ,SNICEV  ,SNLIQV  ,EPORE   , & !out
                   FACT    )                              !out

! Solar radiation: absorbed & reflected by the ground and canopy

  CALL  RADIATION (parameters,VEGTYP  ,IST     ,ICE     ,NSOIL   , & !in 
                   SNEQVO  ,SNEQV   ,DT      ,COSZ    ,SNOWH   , & !in
                   TG      ,TV      ,FSNO    ,QSNOW   ,FWET    , & !in
                   ELAI    ,ESAI    ,SMC     ,SOLAD   ,SOLAI   , & !in
                   FVEG    ,ILOC    ,JLOC    ,                   & !in
                   ALBOLD  ,TAUSS   ,                            & !inout
                   FSUN    ,LAISUN  ,LAISHA  ,PARSUN  ,PARSHA  , & !out
                   SAV     ,SAG     ,FSR     ,FSA     ,FSRV    , & 
                   FSRG    ,ALBSND  ,ALBSNI  ,BGAP    ,WGAP     )  !out

! Surface temperatures of the ground and canopy and energy fluxes
    IF (VEG .AND. FVEG > 0) THEN
    TGV = TG
    CMV = CM
    CHV = CH
    CALL VEGE_FLUX (parameters,NSNOW   ,NSOIL   ,ISNOW   ,VEGTYP  ,VEG     , & !in
                    DT      ,SAV     ,SAG     ,LWDN    ,UR      , & !in
                    UU      ,VV      ,SFCTMP  ,THAIR   ,QAIR    , & !in
                    EAIR    ,RHOAIR  ,SNOWH   ,VAI     ,GAMMAV   ,GAMMAG   , & !in
                    FWET    ,LAISUN  ,LAISHA  ,CWP     ,DZSNSO  , & !in
                    ZLVL    ,ZPD     ,Z0M     ,FVEG    , & !in
                    Z0MG    ,EMV     ,EMG     ,CANLIQ  ,FSNO, & !in
                    CANICE  ,STC     ,DF      ,RSSUN   ,RSSHA   , & !in
                    RSURF   ,LATHEAV ,LATHEAG ,PARSUN  ,PARSHA  ,IGS     , & !in
                    FOLN    ,CO2AIR  ,O2AIR   ,BTRAN   ,SFCPRS  , & !in
                    RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHV  ,PAHG  , & !in
                    EAH     ,TAH     ,TV      ,TGV     ,CMV     , & !inout
                    CHV     ,DX      ,DZ8W    ,                   & !inout
                    TAUXV   ,TAUYV   ,IRG     ,IRC     ,SHG     , & !out
                    SHC     ,EVG     ,EVC     ,TR      ,GHV     , & !out
                    T2MV    ,PSNSUN  ,PSNSHA  ,                   & !out
                    QC      ,QSFC    ,PSFC    , & !in
                    Q2V     ,CHV2, CHLEAF, CHUC)               !inout 
    END IF

! Surface temperatures and energy flux of bare ground
    TGB = TG
    CMB = CM
    CHB = CH
    CALL BARE_FLUX (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,SAG     , & !in
                    LWDN    ,UR      ,UU      ,VV      ,SFCTMP  , & !in
                    THAIR   ,QAIR    ,EAIR    ,RHOAIR  ,SNOWH   , & !in
                    DZSNSO  ,ZLVL    ,ZPDG    ,Z0MG    ,FSNO,          & !in
                    EMG     ,STC     ,DF      ,RSURF   ,LATHEAG  , & !in
                    GAMMAG   ,RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHB  , & !in
                    TGB     ,CMB     ,CHB     ,                   & !inout
                    TAUXB   ,TAUYB   ,IRB     ,SHB     ,EVB     , & !out
                    GHB     ,T2MB    ,DX      ,DZ8W    ,VEGTYP  , & !out
                    QC      ,QSFC    ,PSFC    , & !in
                    SFCPRS  ,Q2B,   CHB2)                          !in 

    IF (VEG .AND. FVEG > 0) THEN
        FGEV  = FVEG * EVG       + (1.0 - FVEG) * EVB
        SSOIL = FVEG * GHV       + (1.0 - FVEG) * GHB
        FCEV  = EVC
        FCTR  = TR
        TG    = FVEG * TGV       + (1.0 - FVEG) * TGB
        CM    = FVEG * CMV       + (1.0 - FVEG) * CMB      ! better way to average?
        CH    = FVEG * CHV       + (1.0 - FVEG) * CHB
    ELSE
        FGEV  = EVB
        SSOIL = GHB
        TG    = TGB
        FCEV  = 0.0
        FCTR  = 0.0
        CM    = CMB
        CH    = CHB
        RSSUN = 0.0
        RSSHA = 0.0
        TGV   = TGB
        CHV   = CHB
    END IF

! 3L snow & 4L soil temperatures

    CALL TSNOSOI (parameters,ICE     ,NSOIL   ,NSNOW   ,ISNOW   ,IST     , & !in
                  TBOT    ,ZSNSO   ,SSOIL   ,DF      ,HCPCT   , & !in
                  SAG     ,DT      ,SNOWH   ,DZSNSO  , & !in
                  TG      ,ILOC    ,JLOC    ,                   & !in
                  STC     )                                       !inout






!!!============================ Energy module end

!!!============================
    SICE(:) = MAX(0.0, SMC(:) - SH2O(:))   
    SNEQVO  = SNEQV
    QVAP = MAX( FGEV/LATHEAG, 0.0)       ! positive part of fgev; Barlage change to ground v3.6
    QDEW = ABS( MIN(FGEV/LATHEAG, 0.0))  ! negative part of fgev
    EDIR = QVAP - QDEW


!!!============================ Water module all
     CALL WATER (parameters,VEGTYPE ,NSNOW  ,NSOIL  ,IMELT  ,DT     ,UU     , & !in
                 VV     ,FCEV   ,FCTR   ,QPRECC ,QPRECL ,ELAI   , & !in
                 ESAI   ,SFCTMP ,QVAP   ,QDEW   ,ZSOIL  ,BTRANI , & !in
                 IRRFRA ,MIFAC  ,FIFAC  ,CROPLU ,                 & !in
                 FICEOLD,PONDING,TG     ,IST    ,FVEG   ,ILOC,JLOC , SMCEQ , & !in
                 BDFALL ,FP     ,RAIN   ,SNOW   ,                 & !in  MB/AN: v3.7
                 QSNOW  ,QRAIN  ,SNOWHIN,LATHEAV,LATHEAG,frozen_canopy,frozen_ground,  & !in  MB
                 DX     ,TDFRACMP,                                & !in PVK tile drainage
                 ISNOW  ,CANLIQ ,CANICE ,TV     ,SNOWH  ,SNEQV  , & !inout
                 SNICE  ,SNLIQ  ,STC    ,ZSNSO  ,SH2O   ,SMC    , & !inout
                 SICE   ,ZWT    ,WA     ,WT     ,DZSNSO ,WSLAKE , & !inout
                 SMCWTD ,DEEPRECH,RECH                          , & !inout
                 IRAMTFI,IRAMTMI ,IRFIRATE ,IRMIRATE,             & !inout
                 CMC    ,ECAN   ,ETRAN  ,FWET   ,RUNSRF ,RUNSUB , & !out
                 QIN    ,QDIS   ,PONDING1       ,PONDING2,        &
                 QSNBOT ,QTLDRN                                 , &
                 QINSUR,QSEVA,QSDEW,QSNFRO,QSNSUB,ETRANI,WCND,QDRAIN,SNOFLOW,FCRMAX & ! added output
#ifdef WRF_HYDRO
                        ,sfcheadrt, WATBLED                       &
#endif
                 )  !out
!!!============================


!!!============================ Crop main module


!!!============================ Error check subroutine


!!!============================ end of Noahmplsm main subroutine ============================


! some updates from last time step for use in next step (from drv)

   FICEOLD(ISNOW+1:0) = SNICE(ISNOW+1:0) &  ! snow ice fraction  
       /(SNICE(ISNOW+1:0)+SNLIQ(ISNOW+1:0))

 
!!!============================ Error balance check
! balance check for soil and snow layers  
    totalwat = sum(DZSNSO(1:nsoil)*SMC*1000.0) + SNEQV + WA + CANLIQ + CANICE    ! total soil+snow water+canopy water [mm]
    errwat = (RAIN+SNOW+IRMIRATE*1000/DT+IRFIRATE*1000/DT-EDIR-ETRAN-RUNSRF-RUNSUB-QTLDRN-ECAN)*DT - (totalwat - tw0)  ! accum error [mm]
   if (abs(errwat) > 0.1) print*,'water not balanced ....'

!!!============================

  !---------------------------------------------------------------------
  ! add to output file
  !---------------------------------------------------------------------

  call add_to_output(itime,NSOIL,NSNOW,ISNOW,CANLIQ,CANICE,TV,SNOWH,SNEQV,&
                     SNICE,SNLIQ,STC,ZSNSO,SH2O,SMC,SICE,ZWT,WA,WT,DZSNSO,&
                     WSLAKE,SMCWTD,DEEPRECH,RECH,IRAMTFI,IRAMTMI,IRFIRATE,IRMIRATE,&
                     CMC,ECAN,ETRAN,FWET,RUNSRF,RUNSUB,QIN,QDIS,PONDING1,PONDING2,&
                     QSNBOT,QTLDRN,QINSUR,QSEVA,QSDEW,QSNFRO,QSNSUB,ETRANI,&
                     WCND,QDRAIN,SNOFLOW,FCRMAX,FICEOLD,errwat,QRAIN,QSNOW,QVAP,&
                     IRAMTSI,IRSIRATE,IRCNTSI,IRCNTMI,IRCNTFI,RAIN,SNOW,IREVPLOS,FIRR,EIRR,&
                     SNOWHIN,TG,QINTR,QDRIPR,QTHROR,QINTS,QDRIPS,QTHROS,PAHV,PAHG,PAHB,EDIR,&
                     DF,HCPCT,SNICEV,SNLIQV,EPORE,FACT,FSUN,LAISUN,LAISHA,PARSUN,PARSHA,SAV,&
                     SAG,FSA,FSR,FSRV,FSRG,BGAP,WGAP,ALBSND,ALBSNI,ALBOLD,TAUSS,SNEQVO,&
                     TAH,TGV,EAH,CMV,CM,CHV,CH,QSFC,RSSUN,RSSHA,TAUXV,TAUYV,IRG,IRC,SHG,SHC,&
                     EVG,EVC,TR,GHV,T2MV,PSNSUN,PSNSHA,Q2V,CHV2,CHLEAF,CHUC,&
                     TGB,CMB,CHB,TAUXB,TAUYB,IRB,SHB,EVB,GHB,T2MB,Q2B,CHB2,SSOIL)

 
  end do ! time loop

  call finalize_output()
  
  print*, ' model run successfully completed ...' 
end program
