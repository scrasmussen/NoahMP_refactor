program noahmp_driver

use noahmp_output
use module_sf_noahmplsm
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
                               raining,uwind,vwind,sfcpres,Q2,SWDOWN,LWDOWN
  namelist / structure       / isltyp,VEGTYPE,soilcolor,slopetype,croptype,nsoil,nsnow,structure_option,soil_depth,&
                               vegfra,vegmax,shdmax
  namelist / fixed_initial   / zsoil
  namelist / uniform_initial / initial_uniform,initial_sh2o_value,&
                               initial_sice_value
  namelist / options         / idveg,iopt_crs,iopt_btr,iopt_run,iopt_sfc,iopt_frz,&
                               iopt_inf,iopt_rad,iopt_alb,iopt_snf,iopt_tbot,iopt_stc, &
                               iopt_rsf,iopt_soil,iopt_pedo,iopt_crop,iopt_irr,iopt_irrm,iopt_infdv,iopt_tdrn
 
!---------------------------------------------------------------------
!  inputs end
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!  additional variables required or passed to water subroutine
!---------------------------------------------------------------------
  type (noahmp_parameters)        :: parameters

  CHARACTER(LEN=256)              :: LLANDUSE = "MODIFIED_IGBP_MODIS_NOAH"  ! landuse data name (USGS or MODIS_IGBP)
  integer                         :: ILOC     = 1                           ! grid index
  integer                         :: JLOC     = 1                           ! grid index
  integer                         :: ISOIL
  REAL                            :: LAT     
  INTEGER                         :: YEARLEN  = 365 
  real                            :: COSZ     = 0.5       ! cosine solar zenith angle [0-1]
  real                            :: DX       = 4000.0    ! horisontal resolution, used for tile drainage
  REAL                            :: DZ8W     = 20.0      ! thickness of lowest layer
  real                            :: SHDFAC               ! greeness vegetation fraction (-) 
  real                            :: FVGMAX               ! max yearly veg fract
  integer                         :: VEGTYP
  integer                         :: ICE      = 0         ! value of ist for land ice
  integer                         :: IST      = 1         ! surface type 1-soil; 2-lake
  real                            :: SFCTMP               ! model-level temperature (k)
  real                            :: SFCPRS               ! surface pressure (pa)
  REAL                            :: PSFC                 ! pressure at lowest model layer
  real                            :: UU                   ! u-direction wind speed [m/s]
  real                            :: VV                   ! v-direction wind speed [m/s]
  REAL                            :: QC       = 0.0005    ! cloud water mixing ratio
  real                            :: PRCPCONV
  real                            :: PRCPNONC
  real                            :: PRCPSHCV
  real                            :: PRCPSNOW
  real                            :: PRCPGRPL
  real                            :: PRCPHAIL
  REAL                            :: TBOT                 ! bottom condition for soil temp. [K]
  REAL                            :: CO2AIR               ! atmospheric co2 concentration (pa)
  REAL                            :: O2AIR                ! atmospheric o2 concentration (pa)
  REAL                            :: FOLN                 ! foliage nitrogen (%)
  REAL                            :: ZLVL     = 10.0      ! reference height (m)
  real                            :: IRRFRA               ! irrigation fraction
  real                            :: MIFRA                ! micro irrigation fraction
  real                            :: FIFRA                ! flood irrigation fraction
  real                            :: SIFRA                ! sprinkler irrigation fraction
  real                            :: ALBOLD               ! snow albedo at last time step(CLASS type)
  real                            :: SNEQVO               ! SWE at last time step
  REAL                            :: EAH                  ! canopy air vapor pressure (pa)
  REAL                            :: TAH                  ! canopy air temperature (k)
  real                            :: FWET                 ! wetted/snowed fraction of canopy (-)
  real                            :: CANLIQ               ! intercepted liquid water (mm)
  real                            :: CANICE               ! intercepted ice mass (mm)
  real                            :: TV                   ! canopy temperature
  real                            :: TG                   ! ground temperature
  REAL                            :: QSFC                 ! mixing ratio at lowest model layer
  REAL                            :: QRAIN                ! rain at ground srf (mm/s) [+]
  REAL                            :: QSNOW                ! snow at ground srf (mm/s) [+]
  integer                         :: ISNOW                ! actual no. of snow layers
  REAL                            :: SNOWH                ! snow height [m]
  REAL                            :: SNEQV                ! snow water eqv. [mm]
  real                            :: ZWT                  ! the depth to water table [m]
  real                            :: WA                   ! water storage in aquifer [mm]
  real                            :: WT                   ! water storage in aquifer + stuarated soil [mm]
  REAL                            :: WSLAKE               ! water storage in lake (can be -) (mm)
  REAL                            :: LFMASS               ! leaf mass [g/m2]
  REAL                            :: RTMASS               ! mass of fine roots [g/m2]
  REAL                            :: STMASS               ! stem mass [g/m2]
  REAL                            :: WOOD 
  REAL                            :: STBLCP               ! stable carbon in deep soil [g/m2]
  REAL                            :: FASTCP
  real                            :: LAI                  ! leaf area index
  real                            :: SAI                  ! stem area index
  REAL                            :: CM                   ! momentum drag coefficient
  REAL                            :: CH                   ! sensible heat exchange coefficient
  real                            :: TAUSS                ! non-dimensional snow age
  real                            :: GRAIN
  real                            :: GDD
  INTEGER                         :: PGS                  ! stem respiration [g/m2/s]
  real                            :: SMCWTD               ! soil water content between bottom of the soil and water table [m3/m3]
  real                            :: DEEPRECH             ! recharge to or from the water table when deep [m]
  real                            :: RECH                 ! recharge to or from the water table when shallow [m] (diagnostic)
  real                            :: QTLDRN               ! tile drainage (mm/s)
  real                            :: TDFRACMP             ! tile drain fraction map
  REAL                            :: Z0WRF
  integer                         :: IRCNTSI              ! irrigation event number, Sprinkler
  integer                         :: IRCNTMI              ! irrigation event number, Micro
  integer                         :: IRCNTFI              ! irrigation event number, Flood 
  real                            :: IRAMTFI              ! irrigation water amount [m] to be applied, flood
  real                            :: IRAMTMI              ! irrigation water amount [m] to be applied, Micro
  real                            :: IRAMTSI              ! total irrigation water amount [m]
  real                            :: IRFIRATE             ! rate of irrigation by flood [m/timestep]
  real                            :: IRMIRATE             ! rate of irrigation by micro [m/timestep]
  real                            :: IRSIRATE             ! rate of irrigation by sprinkler [m/timestep]
  real                            :: FIRR                 ! irrigation:latent heating due to sprinkler evaporation [w/m2]
  real                            :: EIRR                 ! evaporation of irrigation water to evaporation,sprinkler [mm/s]
  REAL                            :: FSA                  ! total absorbed solar radiation (w/m2)
  REAL                            :: FSR                  ! total reflected solar radiation (w/m2)
  real                            :: FIRA
  real                            :: FSH
  REAL                            :: SSOIL
  real                            :: FCEV                 ! canopy evaporation (w/m2) [+ to atm ]
  real                            :: FGEV                 ! ground evap heat (w/m2) [+ to atm]
  real                            :: FCTR                 ! transpiration (w/m2) [+ to atm]
  real                            :: ECAN                 ! evap of intercepted water (mm/s) [+]
  real                            :: ETRAN                ! transpiration rate (mm/s) [+]
  REAL                            :: EDIR                 ! net soil evaporation (mm/s)
  real                            :: TRAD
  REAL                            :: TGB                  ! bare ground temperature
  REAL                            :: TGV                  ! vegetated ground temperature
  REAL                            :: T2MV                 ! 2 m height air temperature (k)
  REAL                            :: T2MB
  REAL                            :: Q2V
  REAL                            :: Q2B
  real                            :: RUNSRF               ! surface runoff [mm/s] 
  real                            :: RUNSUB               ! baseflow (sturation excess) [mm/s]
  real                            :: PSN
  real                            :: APAR
  REAL                            :: SAV                  ! solar radiation absorbed by vegetation (w/m2)
  REAL                            :: SAG                  ! solar radiation absorbed by ground (w/m2)
  real                            :: FSNO                 ! snow cover fraction
  REAL                            :: NEE                  ! net ecosys exchange (g/m2/s CO2)
  REAL                            :: GPP                  ! gross primary assimilation [g/m2/s C]
  REAL                            :: NPP                  ! net primary productivity [g/m2/s C]
  REAL                            :: FVEG
  REAL                            :: ALBEDO
  REAL                            :: PONDING 
  REAL                            :: PONDING1
  REAL                            :: PONDING2
  REAL                            :: QSNBOT               ! melting water out of snow bottom [mm/s]
  REAL                            :: RSSUN                ! sunlit leaf stomatal resistance (s/m)
  REAL                            :: RSSHA                ! shaded leaf stomatal resistance (s/m)
  REAL, DIMENSION(1:2)            :: ALBSND               ! snow albedo (direct)
  REAL, DIMENSION(1:2)            :: ALBSNI               ! snow albedo (diffuse)
  REAL                            :: BGAP
  REAL                            :: WGAP
  REAL                            :: CHV                  ! sensible heat exchange coefficient
  REAL                            :: CHB                  ! sensible heat exchange coefficient
  real                            :: EMISSI
  REAL                            :: SHG                  ! sensible heat flux (w/m2)     [+= to atm]
  REAL                            :: SHC                  ! sensible heat flux (w/m2)     [+= to atm]
  REAL                            :: SHB
  REAL                            :: EVG                  ! evaporation heat flux (w/m2)  [+= to atm]
  REAL                            :: EVB                  ! evaporation heat flux (w/m2)  [+= to atm]
  REAL                            :: GHV                  ! ground heat (w/m2) [+ = to soil]
  REAL                            :: GHB                  ! ground heat (w/m2) [+ = to soil]
  REAL                            :: IRG                  ! net longwave radiation (w/m2) [+= to atm]
  REAL                            :: IRC                  ! net longwave radiation (w/m2) [+= to atm]
  REAL                            :: IRB                  ! net longwave radiation (w/m2) [+= to atm]
  REAL                            :: EVC                  ! evaporation heat flux (w/m2)  [+= to atm]
  REAL                            :: TR                   ! transpiration heat flux (w/m2)[+= to atm]
  REAL                            :: CHV2                 ! sensible heat conductance for diagnostics
  REAL                            :: CHLEAF               ! leaf exchange coefficient
  REAL                            :: CHUC                 ! under canopy exchange coefficient
  REAL                            :: CHB2
  REAL                            :: FPICE
  REAL                            :: PAHV                 ! precipitation advected heat - vegetation net (W/m2)
  REAL                            :: PAHG                 ! precipitation advected heat - under canopy net (W/m2)
  REAL                            :: PAHB                 ! precipitation advected heat - bare ground net (W/m2)
  REAL                            :: PAH
  REAL                            :: LAISUN               ! sunlit leaf area (-)
  REAL                            :: LAISHA               ! shaded leaf area (-)
  real                            :: RB
#ifdef WRF_HYDRO
  REAL                            :: sfcheadrt, WATBLED
#endif
! additional for driver 
  integer, dimension(4)           :: SOILTYPE
  REAL                            :: FRZK
  REAL                            :: FRZFACT
  real, allocatable, dimension(:) :: FICEOLD              ! ice fraction at last timestep
  real, allocatable, dimension(:) :: SMCEQ                ! equilibrium soil water content [m3/m3] (used in m-m&f groundwater dynamics)
  real, allocatable, dimension(:) :: STC                  ! snow/soil layer temperature [k]
  real, allocatable, dimension(:) :: SH2O                 ! soil liquid water content [m3/m3]
  real, allocatable, dimension(:) :: SICE
  real, allocatable, dimension(:) :: SMC                  ! total soil water content [m3/m3]
  real, allocatable, dimension(:) :: ZSNSO                ! depth of snow/soil layer-bottom
  real, allocatable, dimension(:) :: SNICE                ! snow layer ice [mm]
  real, allocatable, dimension(:) :: SNLIQ                ! snow layer liquid water [mm]

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
  allocate (FICEOLD(-nsnow+1:0   ))   !ice fraction at last timestep
  allocate (SMCEQ  (       1:nsoil))   !equilibrium soil water  content [m3/m3]
  allocate (STC   (-nsnow+1:nsoil))   !snow/soil layer temperature [k]
  allocate (zsoil (       1:nsoil))   !depth of layer-bottom from soil surface
  allocate (SH2O  (       1:nsoil))   !soil liquid water content [m3/m3]
  allocate (SICE  (       1:nsoil))   !soil liquid water content [m3/m3]
  allocate (SMC   (       1:nsoil))   !total soil water content [m3/m3]
  allocate (ZSNSO (-nsnow+1:nsoil))   !depth of snow/soil layer-bottom
  allocate (SNICE (-nsnow+1:0    ))   !snow layer ice [mm]
  allocate (SNLIQ (-nsnow+1:0    ))   !snow layer liquid water [mm]

!---------------------------------------------------------------------
!  read input file, part 2: initialize
!---------------------------------------------------------------------
  if(structure_option == 1) then       ! user-defined levels
    open(30, file="namelist.input", form="formatted")
     read(30, fixed_initial)
    close(30)
  end if

  if(initial_uniform) then
    SH2O = initial_sh2o_value
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
  parameters%ISWATER    =   ISWATER_TABLE
  parameters%ISBARREN   =  ISBARREN_TABLE
  parameters%ISICE      =     ISICE_TABLE
  parameters%ISCROP     =    ISCROP_TABLE
  parameters%EBLFOREST  = EBLFOREST_TABLE
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
   parameters%CO2          =         CO2_TABLE
   parameters%O2           =          O2_TABLE
   parameters%TIMEAN       =      TIMEAN_TABLE
   parameters%FSATMX       =      FSATMX_TABLE
   parameters%Z0SNO        =       Z0SNO_TABLE
   parameters%SSI          =         SSI_TABLE
   parameters%SNOW_RET_FAC = SNOW_RET_FAC_TABLE
   parameters%SNOW_EMIS    =   SNOW_EMIS_TABLE
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
   parameters%RSURF_SNOW   =  RSURF_SNOW_TABLE
   parameters%RSURF_EXP    =   RSURF_EXP_TABLE

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
  if (runsnow) then
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
  FICEOLD     = 0.0
  STC(1:4)    = SFCTMP
  STC(-2:0)   = 0.0
  SMC         = 0.23
  LAT         = 40.0 * 3.1415 / 180.0
  LAI         = parameters%LAIM(6)
  SAI         = parameters%SAIM(6)
  SHDFAC      = vegfra / 100.0
  FVGMAX      = shdmax / 100.0
  VEGTYP      = vegtype
  SFCPRS      = sfcpres
  PSFC        = sfcpres + 50.0
  UU          = uwind
  VV          = vwind
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
  ZSNSO(1:4)  = zsoil(1:4)
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
  RB          = 0.0
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
  IF (OPT_IRR .gt. 0) then
     IRRFRA   = 1.0
  ELSE
     IRRFRA   = 0.0
  END IF
  IF(OPT_TDRN .gt. 0) THEN
     TDFRACMP = 0.5
     ZWT      = 0.2
  ELSE
     TDFRACMP = 0.0
  END IF
  IF(OPT_IRRM .EQ. 0) THEN
     SIFRA    = 0.3
     MIFRA    = 0.3
     FIFRA    = 0.4
     IRAMTFI  = 0.25
     IRAMTMI  = 0.25
     IRAMTSI  = 0.5
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  ELSE IF(OPT_IRRM .EQ. 1) THEN
     SIFRA    = 1.0
     MIFRA    = 0.0
     FIFRA    = 0.0
     IRAMTFI  = 0.0
     IRAMTMI  = 0.0
     IRAMTSI  = 0.5
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  ELSE IF(OPT_IRRM .EQ. 2) THEN
     SIFRA    = 0.0
     MIFRA    = 1.0
     FIFRA    = 0.0
     IRAMTFI  = 0.0
     IRAMTMI  = 0.5
     IRAMTSI  = 0.0
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  ELSE IF(OPT_IRRM .EQ. 3) THEN
     SIFRA    = 0.0
     MIFRA    = 0.0
     FIFRA    = 1.0
     IRAMTFI  = 0.5
     IRAMTMI  = 0.0
     IRAMTSI  = 0.0
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  END IF
!= irrigation end

! intialize for forcing
  ntime      =  nint(maxtime * 3600.0 / dt)
  rain_steps = rain_duration * 3600.0 / dt
  dry_steps  =  dry_duration * 3600.0 / dt

! prevent too large SMC initial values
  DO isoil = 1,nsoil
     IF (SMC(isoil) .gt. parameters%SMCMAX(isoil)) THEN
        SH2O(isoil) = parameters%SMCMAX(isoil) * SH2O(isoil) / SMC(isoil)
        SMC(isoil) = parameters%SMCMAX(isoil)
     ENDIF
  END DO
  SICE = 0.0

!!!!!!========= initialization complete ==================================


!---------------------------------------------------------------------
! create output file and add initial values
!---------------------------------------------------------------------

  call initialize_output(output_filename, ntime+1, nsoil, nsnow)
  call add_to_output(0,NSOIL,NSNOW,ALBOLD,SNEQVO,STC,SH2O,SMC,TAH,EAH,FWET,CANLIQ,CANICE,&
                     TV,TG,QSFC,QSNOW,QRAIN,ISNOW,ZSNSO,SNOWH,SNEQV,SNICE,SNLIQ,ZWT,WA,WT,WSLAKE,&
                     LFMASS,RTMASS,STMASS,WOOD,STBLCP,FASTCP,LAI,SAI,CM,CH,TAUSS,GRAIN,GDD,PGS,&
                     SMCWTD,DEEPRECH,RECH,QTLDRN,Z0WRF,IRCNTSI,IRCNTMI,IRCNTFI,IRAMTSI,&
                     IRAMTMI,IRAMTFI,IRSIRATE,IRMIRATE,IRFIRATE,FIRR,EIRR,FSA,FSR,FIRA,FSH,SSOIL,&
                     FCEV,FGEV,FCTR,ECAN,ETRAN,EDIR,TRAD,TGB,TGV,T2MV,T2MB,Q2V,Q2B,RUNSRF,RUNSUB,&
                     APAR,PSN,SAV,SAG,FSNO,NEE,GPP,NPP,FVEG,ALBEDO,QSNBOT,PONDING,PONDING1,PONDING2,&
                     RSSUN,RSSHA,ALBSND,ALBSNI,BGAP,WGAP,CHV,CHB,EMISSI,SHG,SHC,SHB,EVG,EVB,GHV,GHB,&
                     IRG,IRC,IRB,TR,EVC,CHLEAF,CHUC,CHV2,CHB2,FPICE,PAHV,PAHG,PAHB,PAH,LAISUN,LAISHA,FICEOLD)

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
  ! calculate the input water & temperature
  !---------------------------------------------------------------------
    if(raining) then
      PRCPNONC  = rainrate/3600.0 ! input water mm/s
      rain_step = rain_step + 1
      if(rain_step == rain_steps) then      ! event length met
        rain_step = 0
        raining   = .false.
      end if
    else
      PRCPNONC   = 0.0                        ! stop water input [m/s]
      dry_step   = dry_step + 1
      if(dry_step == dry_steps) then        ! between event length met
        dry_step = 0
        raining  = .true.
      end if
    end if

    if (runsnow) then
       PRCPSNOW = PRCPNONC
    endif

   ! varying temperature forcing
  if (runsnow) then
     SFCTMP   = 265.0 + (itime-1)*0.05
  else
     SFCTMP   = 298.0 + (itime-1)* (-0.05)
  endif


!!!============================================= Start the original NoahMP Subroutine ==========================================
  call NOAHMP_SFLX (parameters, &
                   ILOC    , JLOC    , LAT     , YEARLEN , JULIAN  , COSZ    , & ! IN : Time/Space-related
                   DT      , DX      , DZ8W    , NSOIL   , ZSOIL   , NSNOW   , & ! IN : Model configuration 
                   SHDFAC  , FVGMAX  , VEGTYP  , ICE     , IST     , CROPTYPE, & ! IN : Vegetation/Soil characteristics
                   SMCEQ   ,                                                   & ! IN : Vegetation/Soil characteristics
                   SFCTMP  , SFCPRS  , PSFC    , UU      , VV      , Q2      , & ! IN : Forcing
                   QC      , SWDOWN  , LWDOWN  ,                               & ! IN : Forcing
                   PRCPCONV, PRCPNONC, PRCPSHCV, PRCPSNOW, PRCPGRPL, PRCPHAIL, & ! IN : Forcing
                   TBOT    , CO2AIR  , O2AIR   , FOLN    , FICEOLD , ZLVL    , & ! IN : Forcing
                   IRRFRA  , SIFRA   , MIFRA   , FIFRA   , LLANDUSE,           & ! IN : Irrigation: fractions
                   ALBOLD  , SNEQVO  ,                                         & ! IN/OUT : 
                   STC     , SH2O    , SMC     , TAH     , EAH     , FWET    , & ! IN/OUT : 
                   CANLIQ  , CANICE  , TV      , TG      , QSFC    , QSNOW   , & ! IN/OUT :
                   QRAIN   ,                                                   & ! IN/OUT : 
                   ISNOW   , ZSNSO   , SNOWH   , SNEQV   , SNICE   , SNLIQ   , & ! IN/OUT : 
                   ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS  , & ! IN/OUT : 
                   STMASS  , WOOD    , STBLCP  , FASTCP  , LAI     , SAI     , & ! IN/OUT : 
                   CM      , CH      , TAUSS   ,                               & ! IN/OUT : 
                   GRAIN   , GDD     , PGS     ,                               & ! IN/OUT 
                   SMCWTD  ,DEEPRECH , RECH    ,                               & ! IN/OUT :
                   QTLDRN  , TDFRACMP,                                         & ! IN/OUT :
                   Z0WRF   ,                                                   &
                   IRCNTSI , IRCNTMI , IRCNTFI , IRAMTSI , IRAMTMI , IRAMTFI , & ! IN/OUT : Irrigation: vars
                   IRSIRATE, IRMIRATE, IRFIRATE, FIRR    , EIRR    ,           & ! IN/OUT : Irrigation: vars
                   FSA     , FSR     , FIRA    , FSH     , SSOIL   , FCEV    , & ! OUT : 
                   FGEV    , FCTR    , ECAN    , ETRAN   , EDIR    , TRAD    , & ! OUT :
                   TGB     , TGV     , T2MV    , T2MB    , Q2V     , Q2B     , & ! OUT :
                   RUNSRF  , RUNSUB  , APAR    , PSN     , SAV     , SAG     , & ! OUT :
                   FSNO    , NEE     , GPP     , NPP     , FVEG    , ALBEDO  , & ! OUT :
                   QSNBOT  , PONDING , PONDING1, PONDING2, RSSUN   , RSSHA   , & ! OUT :
                   ALBSND  , ALBSNI  ,                                         & ! OUT :
                   BGAP    , WGAP    , CHV     , CHB     , EMISSI  ,           & ! OUT :
                   SHG     , SHC     , SHB     , EVG     , EVB     , GHV     , & ! OUT :
                   GHB     , IRG     , IRC     , IRB     , TR      , EVC     , & ! OUT :
                   CHLEAF  , CHUC    , CHV2    , CHB2    , FPICE   , PAHV    , &
                   PAHG    , PAHB    , PAH     , LAISUN  , LAISHA  , RB        & ! OUT
#ifdef WRF_HYDRO
                   ,SFCHEADRT, WATBLED                                         & ! IN/OUT :
#endif
                   )


!!!============================ end of Noahmplsm main subroutine ============================


! some updates from last time step for use in next step (from drv)
   FICEOLD(ISNOW+1:0) = SNICE(ISNOW+1:0) &  ! snow ice fraction  
       /(SNICE(ISNOW+1:0)+SNLIQ(ISNOW+1:0))

 
!!!============================

  !---------------------------------------------------------------------
  ! add to output file
  !---------------------------------------------------------------------
  call add_to_output(itime,NSOIL,NSNOW,ALBOLD,SNEQVO,STC,SH2O,SMC,TAH,EAH,FWET,CANLIQ,CANICE,&
                     TV,TG,QSFC,QSNOW,QRAIN,ISNOW,ZSNSO,SNOWH,SNEQV,SNICE,SNLIQ,ZWT,WA,WT,WSLAKE,&
                     LFMASS,RTMASS,STMASS,WOOD,STBLCP,FASTCP,LAI,SAI,CM,CH,TAUSS,GRAIN,GDD,PGS,&
                     SMCWTD,DEEPRECH,RECH,QTLDRN,Z0WRF,IRCNTSI,IRCNTMI,IRCNTFI,IRAMTSI,&
                     IRAMTMI,IRAMTFI,IRSIRATE,IRMIRATE,IRFIRATE,FIRR,EIRR,FSA,FSR,FIRA,FSH,SSOIL,&
                     FCEV,FGEV,FCTR,ECAN,ETRAN,EDIR,TRAD,TGB,TGV,T2MV,T2MB,Q2V,Q2B,RUNSRF,RUNSUB,&
                     APAR,PSN,SAV,SAG,FSNO,NEE,GPP,NPP,FVEG,ALBEDO,QSNBOT,PONDING,PONDING1,PONDING2,&
                     RSSUN,RSSHA,ALBSND,ALBSNI,BGAP,WGAP,CHV,CHB,EMISSI,SHG,SHC,SHB,EVG,EVB,GHV,GHB,&
                     IRG,IRC,IRB,TR,EVC,CHLEAF,CHUC,CHV2,CHB2,FPICE,PAHV,PAHG,PAHB,PAH,LAISUN,LAISHA,FICEOLD)

  end do ! time loop

  call finalize_output()
  
  print*, ' model run successfully completed ...' 
end program
