program water_driver

use water_output
use water_routines
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
! structure
  integer       :: isltyp
  integer       :: vegtype
  integer       :: soilcolor
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
  namelist / timing          / dt,maxtime,output_filename,runsnow
  namelist / forcing         / rainrate,rain_duration,dry_duration,&
                               raining,uwind,vwind,sfcpres,fcev_e,fctr_e,fgev_e
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

  integer                        :: ILOC  = 1   !grid index
  integer                        :: JLOC  = 1   !grid index
  integer, dimension(4) :: SOILTYPE
  REAL    :: REFDK
  REAL    :: REFKDT
  REAL    :: FRZK
  REAL    :: FRZFACT
  INTEGER :: ISOIL
 integer, allocatable, dimension(:) :: IMELT  !phase change index
  real                           :: UU      !u-direction wind speed [m/s]
  real                           :: VV      !v-direction wind speed [m/s]
  real                           :: FCEV    !canopy evaporation (w/m2) [+ to atm ]
  real                           :: FCTR    !transpiration (w/m2) [+ to atm]
  REAL                           :: QPRECC  !convective precipitation (mm/s)
  REAL                           :: QPRECL  !large-scale precipitation (mm/s)
  real                           :: ELAI    !leaf area index, after burying by snow
  real                           :: ESAI    !stem area index, after burying by snow
  real                           :: LAI     !leaf area index
  real                           :: SAI     !stem area index
  real                           :: SFCTMP  !model-level temperature (k)
  real                           :: FB_snow  ! canopy fraction buried by snow
  real                           :: QVAP    !soil surface evaporation rate[mm/s] 
  REAL                           :: QDEW    !ground surface dew rate [mm/s]
  real                           :: TV      ! canopy temperature
  real                           :: TG      ! ground temperature
  real                           :: LATHEAV !latent heat vap./sublimation (j/kg)
  real                           :: LATHEAG !latent heat vap./sublimation (j/kg)
  logical                        :: frozen_canopy ! used to define latent heat pathway for canopy
  logical                        :: frozen_ground ! used to define latent heat pathway for ground
  real                           :: GAMMAV  !psychrometric constant (pa/k)
  real                           :: GAMMAG  !psychrometric constant (pa/k)
  real                           :: SFCPRS !surface pressure (pa)
  real                           :: FGEV   ! FGEV   !ground evap heat (w/m2) [+ to atm]
  REAL, allocatable, dimension(:):: BTRANI !Soil water transpiration factor (0 - 1) !!!!!!! Cenlin
  real                           :: IRRFRA   ! irrigation fraction
  real                           :: MIFAC    ! micro irrigation fraction
  real                           :: FIFAC    ! flood irrigation fraction
  real                           :: SIFAC    ! sprinkler irrigation fraction
  real                           :: CROPLU   ! flag to identify croplands
  real, allocatable, dimension(:) :: FICEOLD !ice fraction at last timestep
  real, allocatable, dimension(:) :: ZSNSO   !depth of snow/soil layer-bottom
  REAL                           :: PONDING ![mm]
  integer                        :: IST      !surface type 1-soil; 2-lake
  real                           :: FVEG    !greeness vegetation fraction (-) 
  real, allocatable, dimension(:) :: SMCEQ   !equilibrium soil water content [m3/m3] (used in m-m&f groundwater dynamics)
  real                           :: BDFALL   !bulk density of snowfall (kg/m3) ! MB/AN: v3.7
  real                           :: FP      !fraction of the gridcell that receives precipitation
  real                           :: SNOW    !snowfall (mm/s)
  real                           :: RAIN    !ralfall mm/s 
  REAL                           :: QRAIN   !rain at ground srf (mm/s) [+]
  REAL                           :: QSNOW   !snow at ground srf (mm/s) [+]
  REAL                           :: SNOWHIN !snow depth increasing rate (m/s)
  real                           :: DX      !horisontal resolution, used for tile drainage
  real                           :: TDFRACMP !tile drain fraction map
  integer                        :: ISNOW   !actual no. of snow layers
  real                           :: CANLIQ  !intercepted liquid water (mm)
  real                           :: CANICE  !intercepted ice mass (mm)
  REAL                           :: SNOWH   !snow height [m]
  REAL                           :: SNEQV   !snow water eqv. [mm]
  real, allocatable, dimension(:) :: SNICE   !snow layer ice [mm]
  real, allocatable, dimension(:) :: SNLIQ   !snow layer liquid water [mm]
  real, allocatable, dimension(:) :: STC     !snow/soil layer temperature [k]
  real, allocatable, dimension(:) :: SICE    ! soil ice content [m3/m3]
  real, allocatable, dimension(:) :: SH2O    ! soil liquid water content [m3/m3]
  real, allocatable, dimension(:) :: SMC         !total soil water content [m3/m3]
  real                            :: ZWT        !the depth to water table [m]
  real                            :: WA      !water storage in aquifer [mm]
  real                            :: WT      !water storage in aquifer + stuarated soil [mm]
  REAL                           :: WSLAKE  !water storage in lake (can be -) (mm)
  real                            :: SMCWTD      !soil water content between bottom of the soil and water table [m3/m3]
  real                            :: DEEPRECH    !recharge to or from the water table when deep [m]
  real                            :: RECH !recharge to or from the water table when shallow [m] (diagnostic)
  real                            :: IRAMTFI  ! irrigation water amount [m] to be applied, flood
  real                            :: IRAMTMI  ! irrigation water amount [m] to be applied, Micro
  real                            :: IRFIRATE ! rate of irrigation by flood [m/timestep]
  real                            :: IRMIRATE ! rate of irrigation by micro [m/timestep]
  real                           :: CMC     !intercepted water per ground area (mm)
  real                           :: ECAN    !evap of intercepted water (mm/s) [+]
  real                           :: ETRAN   !transpiration rate (mm/s) [+]
  real                           :: FWET    !wetted/snowed fraction of canopy (-)
  real                            :: RUNSRF      !surface runoff [mm/s] 
  real                            :: RUNSUB      !baseflow (sturation excess) [mm/s]
  real                            :: QIN     !groundwater recharge [mm/s]
  real                            :: QDIS    !groundwater discharge [mm/s]
  REAL                           :: PONDING1 ![mm]
  REAL                           :: PONDING2 ![mm]
  REAL                           :: QSNBOT !melting water out of snow bottom [mm/s]
  real                           :: QTLDRN   !tile drainage (mm/s)
  real                            :: QINSUR      !water input on soil surface [m/s]
  real                            :: QSEVA   !soil surface evap rate [mm/s]
  real, allocatable, dimension(:) :: ETRANI      !transpiration rate (mm/s) [+]
  REAL                           :: QSNFRO  !snow surface frost rate[mm/s]
  REAL                           :: QSNSUB  !snow surface sublimation rate [mm/s]
  REAL                           :: SNOFLOW !glacier flow [mm/s]
  REAL                           :: QSDEW   !soil surface dew rate [mm/s]
  real                            :: QDRAIN      !soil-bottom free drainage [mm/s] 
  real                            :: FCRMAX      !maximum of fcr (-)
  real, allocatable, dimension(:) :: WCND        !hydraulic conductivity (m/s)

#ifdef WRF_HYDRO
  REAL                           :: sfcheadrt, WATBLED
#endif

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
  allocate (WCND (       1:nsoil))   !hydraulic conductivity (m/s)

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
  TV = 265.0
  TG = 265.0
  IMELT = 2  ! freeze
  CANLIQ = 0.1
  CANICE = 4.0
  STC(1:4) = 265.0
  STC(-2:0) = 0.0
  SH2O(1:4) = 0.03
  SICE(1:4) = 0.3
else
  SFCTMP = 298.0 !model-level temperature (k)
  FB_snow = 0.0
  TV = 298.0
  TG = 298.0
  IMELT = 1 ! melt
  CANLIQ = 0.4
  CANICE = 0.0
  STC(1:4) = 298.0
  STC(-2:0) = 0.0
  SH2O(1:4) = 0.3
  SICE(1:4) = 0.03
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
  ZWT   = -1.5
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

! set psychrometric constant
  IF (TV .GT. TFRZ) THEN           ! Barlage: add distinction between ground and 
      LATHEAV = HVAP                ! vegetation in v3.6
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
  QVAP = MAX( FGEV/LATHEAG, 0.)       ! positive part of fgev; Barlage change to ground v3.6
  QDEW = ABS( MIN(FGEV/LATHEAG, 0.))  ! negative part of fgev
  BTRANI(1:nsoil) = 0.2 ! 0~1
  IF (OPT_IRR .gt. 0) then
     IRRFRA = 0.5  ! irrigation fraction
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
     IRFIRATE = 0.0
     IRMIRATE = 0.0
  ELSE IF(OPT_IRRM .EQ. 1) THEN
      SIFAC = 1.
      MIFAC = 0
      FIFAC = 0.
     IRAMTFI = 0.0
     IRAMTMI = 0.0
     IRFIRATE = 0.0
     IRMIRATE = 0.0
  ELSE IF(OPT_IRRM .EQ. 2) THEN ! micro
      SIFAC = 0.
      MIFAC = 1.
      FIFAC = 0.
     IRAMTFI = 0.0
     IRAMTMI = 0.5
     IRFIRATE = 0.0
     IRMIRATE = 0.0
  ELSE IF(OPT_IRRM .EQ. 3) THEN ! flood
      SIFAC = 0.
      MIFAC = 0.
      FIFAC = 1.
     IRAMTFI = 0.5
     IRAMTMI = 0.0
     IRFIRATE = 0.0
     IRMIRATE = 0.0
  END IF

  IF(OPT_TDRN .gt. 0) THEN
      TDFRACMP = 0.5
      ZWT   = -0.2  ! to allow the drainage effect to show up
  ELSE
      TDFRACMP = 0.0
  END IF


  ntime      =  nint(maxtime * 3600.0 / dt)
  rain_steps = rain_duration * 3600.0 / dt
  dry_steps  =  dry_duration * 3600.0 / dt

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
                     WCND,QDRAIN,SNOFLOW,FCRMAX,FICEOLD,errwat)

!---------------------------------------------------------------------
! start the time loop
!---------------------------------------------------------------------

  do itime = 1, ntime
  
  tw0 = sum(DZSNSO(1:nsoil)*SMC*1000.0) + SNEQV + WA ! [mm] 

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

   QRAIN = RAIN * 0.99
   QSNOW = SNOW * 0.9
   SNOWHIN = QSNOW / BDFALL

!!!============================================= Start the original Water Subroutine ==========================================

! compute water budgets (water storages, ET components, and runoff)

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


! some updates from last time step for use in next step (from drv)

   FICEOLD(ISNOW+1:0) = SNICE(ISNOW+1:0) &  ! snow ice fraction  
       /(SNICE(ISNOW+1:0)+SNLIQ(ISNOW+1:0))

 
! balance check for soil and snow layers  
    totalwat = sum(DZSNSO(1:nsoil)*SMC*1000.0) + SNEQV + WA      ! total soil+snow water [mm]
    errwat = (QRAIN+QSNOW+QDEW-QVAP-ETRAN-RUNSRF-RUNSUB-QTLDRN)*DT - (totalwat - tw0)  ! accum error [mm]
   
  !---------------------------------------------------------------------
  ! add to output file
  !---------------------------------------------------------------------

  call add_to_output(itime,NSOIL,NSNOW,ISNOW,CANLIQ,CANICE,TV,SNOWH,SNEQV,&
                     SNICE,SNLIQ,STC,ZSNSO,SH2O,SMC,SICE,ZWT,WA,WT,DZSNSO,&
                     WSLAKE,SMCWTD,DEEPRECH,RECH,IRAMTFI,IRAMTMI,IRFIRATE,IRMIRATE,&
                     CMC,ECAN,ETRAN,FWET,RUNSRF,RUNSUB,QIN,QDIS,PONDING1,PONDING2,&
                     QSNBOT,QTLDRN,QINSUR,QSEVA,QSDEW,QSNFRO,QSNSUB,ETRANI,&
                     WCND,QDRAIN,SNOFLOW,FCRMAX,FICEOLD,errwat)

 
  end do ! time loop

  call finalize_output()
   
end program
