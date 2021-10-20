!
! compile: 
!

program water_driver

use output
use water_routines
USE NOAHMP_TABLES

  implicit none

!---------------------------------------------------------------------
!  inputs start
!---------------------------------------------------------------------

  real          :: dt
  integer       :: maxtime
  character*256 :: output_filename
  real          :: rainrate
  integer       :: rain_duration
  integer       :: dry_duration
  logical       :: raining
  real          :: evaprate
  real          :: tranrate
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
  integer       :: VEGTYPE
  real          :: shdmax
  real          :: fcev_e
  real          :: fctr_e
  real          :: qvap

  real, allocatable, dimension(:) :: zsoil   ! depth of layer-bottom from soil surface
  real, allocatable, dimension(:) :: dzsnso  ! snow/soil layer thickness [m]
  real, allocatable, dimension(:) :: sice    ! soil ice content [m3/m3]
  real, allocatable, dimension(:) :: sh2o    ! soil liquid water content [m3/m3]
  logical :: initial_uniform                 ! initial all levels the same
  real    :: initial_sh2o_value              ! constant sh2o value
  real    :: initial_sice_value              ! constant sice value
  integer :: idveg,iopt_crs,iopt_btr,iopt_run,iopt_sfc,iopt_frz,&
             iopt_inf,iopt_rad,iopt_alb,iopt_snf,iopt_tbot,iopt_stc, &
             iopt_rsf,iopt_soil,iopt_pedo,iopt_crop,iopt_irr,iopt_irrm,iopt_infdv,iopt_tdrn



  !--------------------!
  !  soil parameters   !
  !--------------------!
  integer, dimension(4) :: SOILTYPE
  REAL    :: REFDK
  REAL    :: REFKDT
  REAL    :: FRZK
  REAL    :: FRZFACT
  INTEGER :: ISOIL


  real, dimension(12) ::      bb  ! b parameter
  real, dimension(12) ::   satdk  ! conductivity at saturation
  real, dimension(12) ::   satdw  ! diffusivity at saturation
  real, dimension(12) ::  maxsmc  ! porosity
  real, dimension(12) ::  satpsi  ! matric potential at saturation
  real, dimension(12) ::  wltsmc  ! wilting point
  real, dimension(12) ::  refsmc  ! field capacity
  real, dimension(12) :: pctsand  ! percent sand
  real, dimension(12) :: pctclay  ! percent clay
  real                ::   slope  ! free drainage parameter
! addrunoff
  real, dimension(12) ::   bvic   !VIC or DVIC model infiltration parameter
  real, dimension(12) ::   AXAJ   !Xinanjiang: Tension water distribution inflection parameter [-]
  real, dimension(12) ::   BXAJ   !Xinanjiang: Tension water distribution shape parameter [-]
  real, dimension(12) ::   XXAJ   !Xinanjiang: Free water distribution shape parameter [-]
  real, dimension(12) ::   G      !Mean Capillary Drive (m) for infiltration models
  real, dimension(12) ::   BBVIC  !DVIC heterogeniety parameter for infiltration 

  !--------------------!
  ! vegetation parameters   !
  !--------------------!
  real, dimension(20) ::  CH2OP !maximum intercepted h2o per unit lai+sai (mm)
  real, dimension(20) ::  SAI_APR 
  real, dimension(20) ::  LAI_APR

! read namelist (for test, include MPTABLE in namelist) 
  namelist / timing          / dt,maxtime,output_filename
  namelist / forcing         / rainrate,rain_duration,dry_duration,&
                               raining,evaprate,tranrate,fcev_e,fctr_e,qvap
  namelist / structure       / isltyp,VEGTYPE,soilcolor,slopetype,croptype,nsoil,nsnow,structure_option,soil_depth,&
                               vegfra,vegmax,shdmax
  namelist / fixed_initial   / zsoil,dzsnso,sice,sh2o
  namelist / uniform_initial / initial_uniform,initial_sh2o_value,&
                               initial_sice_value
  namelist / options  /  idveg,iopt_crs,iopt_btr,iopt_run,iopt_sfc,iopt_frz,&
                             iopt_inf,iopt_rad,iopt_alb,iopt_snf,iopt_tbot,iopt_stc, &
                             iopt_rsf,iopt_soil,iopt_pedo,iopt_crop,iopt_irr,iopt_irrm,iopt_infdv,iopt_tdrn
 
!---------------------------------------------------------------------
!  inputs end
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!  additional variables passed to soilwater
!---------------------------------------------------------------------

  real                            :: qinsur      !water input on soil surface [m/s]
  real                            :: qseva       !soil surface evap rate [mm/s]
  real                            :: runsrf      !surface runoff [mm/s] 
  real                            :: runsub      !baseflow (sturation excess) [mm/s]
  real                            :: qdrain      !soil-bottom free drainage [mm/s] 
  real                            :: zwt         !the depth to water table [m]
  real                            :: smcwtd      !soil water content between bottom of the soil and water table [m3/m3]
  real                            :: deeprech    !recharge to or from the water table when deep [m]
  real                            :: fcrmax      !maximum of fcr (-)
  real                            :: WA      !water storage in aquifer [mm]
  real                            :: WT      !water storage in aquifer + stuarated soil [mm]
  real                            :: RECH !recharge to or from the water table when shallow [m] (diagnostic)
  real, allocatable, dimension(:) :: etrani      !transpiration rate (mm/s) [+]
  real, allocatable, dimension(:) :: smc         !total soil water content [m3/m3]
  real, allocatable, dimension(:) :: wcnd        !hydraulic conductivity (m/s)
  integer                         :: iloc  = 1   !grid index
  integer                         :: jloc  = 1   !grid index

!---------------------------------------------------------------------
!  additional variables passed to canopy water
!---------------------------------------------------------------------
  real                           :: FCEV    !canopy evaporation (w/m2) [+ to atm ]
  real                           :: FCTR    !transpiration (w/m2) [+ to atm]
  real                           :: ELAI    !leaf area index, after burying by snow
  real                           :: ESAI    !stem area index, after burying by snow
  real                           :: TG      !ground temperature (k)
  real                           :: FVEG    !greeness vegetation fraction (-)        
  real                           :: BDFALL   !bulk density of snowfall (kg/m3) ! MB/AN: v3.7
  real                           :: CANLIQ  !intercepted liquid water (mm)
  real                           :: CANICE  !intercepted ice mass (mm)
  real                           :: TV      !vegetation temperature (k)
  real                           :: CMC     !intercepted water per ground area (mm)
  real                           :: ECAN    !evap of intercepted water (mm/s) [+]
  real                           :: ETRAN   !transpiration rate (mm/s) [+]
  REAL, allocatable, dimension(:):: BTRANI !Soil water transpiration factor (0 - 1) !!!!!!! Cenlin
  real                           :: FWET    !wetted/snowed fraction of canopy (-)
  real                           :: FVGMAX  !annual max greeness vegetation fraction (-) 
  real                           :: EAH     !canopy vapor pressure [Pa]
  real                           :: TAH      ! canopy temperature [K]       
  real                           :: LAI
  real                           :: SAI
  real                           :: FB_snow  ! canopy fraction buried by snow
  LOGICAL                        :: FROZEN_CANOPY ! used to define latent heat pathway
  real                           :: UU      !u-direction wind speed [m/s]
  real                           :: VV      !v-direction wind speed [m/s]
  integer                        :: IST     !surface type 1-soil; 2-lake
  real                           :: SNOW    !snowfall (mm/s)
  real                           :: RAIN    !ralfall mm/s 
  real                           :: FP      !fraction of the gridcell that receives precipitation
  real                           :: SFCTMP  !model-level temperature (k)
  REAL                           :: QINTR   !interception rate for rain (mm/s)
  REAL                           :: QDRIPR  !drip rate for rain (mm/s)
  REAL                           :: QTHROR  !throughfall for rain (mm/s)
  REAL                           :: QINTS   !interception (loading) rate for snowfall (mm/s)
  REAL                           :: QDRIPS  !drip (unloading) rate for intercepted snow (mm/s)
  REAL                           :: QTHROS  !throughfall of snowfall (mm/s)
  REAL                           :: QRAIN   !rain at ground srf (mm/s) [+]
  REAL                           :: QSNOW   !snow at ground srf (mm/s) [+]
  REAL                           :: SNOWHIN !snow depth increasing rate (m/s)

!---------------------------------------------------------------------
!  additional variables passed to snow water
!---------------------------------------------------------------------
  integer                        :: ISNOW   !actual no. of snow layers
  REAL                           :: SNOWH   !snow height [m]
  REAL                           :: SNEQV   !snow water eqv. [mm]
  REAL                           :: WSLAKE  !water storage in lake (can be -) (mm)
  REAL                           :: PONDING ![mm]
  REAL                           :: PONDING1 ![mm]
  REAL                           :: PONDING2 ![mm]
  REAL                           :: QSNBOT !melting water out of snow bottom [mm/s]
!  REAL                           :: LATHEAV !latent heat vap./sublimation (j/kg) 
!  REAL                           :: LATHEAG !latent heat vap./sublimation (j/kg)
  LOGICAL                        :: FROZEN_GROUND ! used to define latent heat pathway
  REAL                           :: QSNFRO  !snow surface frost rate[mm/s]
  REAL                           :: QSNSUB  !snow surface sublimation rate [mm/s]
  REAL                           :: SNOFLOW !glacier flow [mm/s]
  REAL                           :: QDEW    !ground surface dew rate [mm/s]
  REAL                           :: QSDEW   !soil surface dew rate [mm/s]
  real, allocatable, dimension(:) :: SNICE   !snow layer ice [mm]
  real, allocatable, dimension(:) :: SNLIQ   !snow layer liquid water [mm]
  real, allocatable, dimension(:) :: STC     !snow/soil layer temperature [k]
  real, allocatable, dimension(:) :: ZSNSO   !depth of snow/soil layer-bottom
  integer, allocatable, dimension(:) :: IMELT  !phase change index
  real, allocatable, dimension(:) :: FICEOLD !ice fraction at last timestep

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------

  integer :: itime, iz          ! some loop counters
  integer :: ntime      = 0     ! number of timesteps to run
  integer :: rain_steps = 0     ! number of timesteps in rain event
  integer :: dry_steps  = 0     ! number of timesteps between rain events
  integer :: rain_step  = 0     ! number of timesteps in current event
  integer :: dry_step   = 0     ! number of timesteps in current event
  real    :: dtheta_max = 0.0   ! maximum value of theta change in all levels
  real    :: totalwat   = 0.0   ! total soil water [mm]
  real    :: tw0        = 0.0   ! initial total soil water [mm]
  real    :: acsrf      = 0.0   ! accumulated surface runoff [mm]
  real    :: acsub      = 0.0   ! accumulated drainage [mm]
  real    :: acpcp      = 0.0   ! accumulated precipitation [mm]
  real    :: errwat     = 0.0   ! accumulated error [mm]
  logical :: done               ! logical check
  real, allocatable, dimension(:) :: smcold        !previous timestep smc
  real    :: WSLMAX = 5000.      !maximum lake water storage (mm)

!---------------------------------------------------------------------
!  parameters
!---------------------------------------------------------------------

  type (noahmp_parameters) :: parameters
  
!---------------------------------------------------------------------
!  end declarations
!---------------------------------------------------------------------


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

  allocate (zsoil (       1:nsoil))   !depth of layer-bottom from soil surface
  allocate (dzsnso(-nsnow+1:nsoil))   !snow/soil layer thickness [m]
  allocate (etrani(       1:nsoil))   !transpiration rate (mm/s) [+]
  allocate (btrani(       1:nsoil))   !!!!!!!! Cenlin
  allocate (sice  (       1:nsoil))   !soil ice content [m3/m3]
  allocate (sh2o  (       1:nsoil))   !soil liquid water content [m3/m3]
  allocate (smc   (       1:nsoil))   !total soil water content [m3/m3]
  allocate (wcnd  (       1:nsoil))   !hydraulic conductivity (m/s)
  allocate (smcold(       1:nsoil)) 

  allocate (SNICE (-nsnow+1:0    ))   !snow layer ice [mm]
  allocate (SNLIQ (-nsnow+1:0    ))   !snow layer liquid water [mm]
  allocate (STC   (-nsnow+1:nsoil))   !snow/soil layer temperature [k]
  allocate (zsnso (-nsnow+1:nsoil))   !depth of snow/soil layer-bottom
  allocate (IMELT (-nsnow+1:0    ))   !phase change index [1-melt; 2-freeze]
  allocate (FICEOLD(-nsnow+1:0   ))   !ice fraction at last timestep

  allocate (parameters%bexp  (nsoil))
  allocate (parameters%smcmax(nsoil))
  allocate (parameters%smcwlt(nsoil))
  allocate (parameters%smcref(nsoil))
  allocate (parameters%dksat (nsoil))
  allocate (parameters%dwsat (nsoil))
  allocate (parameters%psisat(nsoil))


  SOILTYPE(1:4) = isltyp

!=============== read MPTABLE
    call read_mp_veg_parameters(trim(MMINLU))
    call read_mp_soil_parameters()
    call read_mp_rad_parameters()
    call read_mp_global_parameters()
    call read_mp_crop_parameters()
    call read_tiledrain_parameters()
    call read_mp_optional_parameters()
    if(iopt_irr  >= 1) call read_mp_irrigation_parameters()

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
! ---------------------------------------- complete table value transfer

 
!---------------------------------------------------------------------
!  read input file, part 2: initialize
!---------------------------------------------------------------------

  if(structure_option == 1) then       ! user-defined levels
    open(30, file="namelist.input", form="formatted")
     read(30, fixed_initial)
    close(30)
  else if(structure_option == 2) then  ! fixed levels
    dzsnso = soil_depth / nsoil
    do iz = 1, nsoil
      zsoil(iz) = -1. * sum(dzsnso(1:iz))
    end do
    if(.not.initial_uniform) &
      stop "structure_option > 1 must have initial_uniform == .true."
  end if

! add for snow water
  zsnso(-2:0) = 0.0
  zsnso(1:4) = zsoil(1:4)

  if(initial_uniform) then
    sh2o = initial_sh2o_value
    sice = initial_sice_value
  end if

!!!============================================= READ Table parameter values
    CALL NOAHMP_OPTIONS(IDVEG  ,IOPT_CRS  ,IOPT_BTR  ,IOPT_RUN  ,IOPT_SFC  ,IOPT_FRZ , &
                IOPT_INF  ,IOPT_RAD  ,IOPT_ALB  ,IOPT_SNF  ,IOPT_TBOT, IOPT_STC  ,     &
                IOPT_RSF  ,IOPT_SOIL ,IOPT_PEDO ,IOPT_CROP ,IOPT_IRR , IOPT_IRRM ,     &
                IOPT_INFDV,IOPT_TDRN )


!---------------------------------------------------------------------
! initialize any other values
!---------------------------------------------------------------------

  smc       = sh2o + sice  ! initial volumetric soil water

  zwt       = -100.0       ! should only be needed for run=1
  smcwtd    = 0.0          ! should only be needed for run=5
  deeprech  = 0.0          ! should only be needed for run=5
  qinsur    = 0.0          ! 
  runsrf    = 0.0          ! 
  runsub    = 0.0          ! 
  qdrain    = 0.0          ! 
  wcnd      = 0.0          ! 
  fcrmax    = 0.0          ! 

  rain = 0.0
  qintr = 0.0
  qints = 0.0
  qdripr = 0.0
  qdrips = 0.0
  qthror = 0.0
  qthros = 0.0
  qrain = 0.0
  qsnow = 0.0
  snowhin = 0.0
  fwet = 0.0
  cmc = 0.0
  canliq = 0.0
  canice = 0.0
  ecan = 0.0
  etran = 0.0

  qseva           = evaprate/3600.0 ! soil evaporation [mm/s]
  btrani(1:nsoil) = 0.0
  totalwat = sum(dzsnso(1:nsoil)*smc*1000.0) ! [mm]
  tw0 = totalwat

  ntime      =  nint(maxtime * 3600.0 / dt)
  rain_steps = rain_duration * 3600.0 / dt
  dry_steps  =  dry_duration * 3600.0 / dt

!!!!!!!!!!!!!!!!!! for canopy water
   LAI = LAI_APR(VEGTYPE)
   SAI = SAI_APR(VEGTYPE)
   FB_snow = 0.0
!  FVEG   = VEGFRA(I,J)/100.       ! vegetation fraction [0-1]
  FVEG = SHDMAX/100.        !yearly max vegetation fraction
  IF(FVEG <= 0.05) FVEG = 0.05

  FVGMAX = VEGMAX/100.      ! Vegetation fraction annual max [0-1]
  TV = 298   ! leaf temperature [K]
  TG = 298   ! ground temperature [K]
  CANLIQ = 0.0  ! canopy liquid water [mm]
  CANICE = 0.0  ! canopy frozen water [mm]
  EAH = 400 ! canopy vapor pressure [Pa]
  TAH = 298 ! canopy temperature [K]
  FWET = 0.0  ! canopy fraction wet or snow
  FCEV = fcev_e  !canopy evaporation (w/m2) [+ to atm ]
  FCTR = fctr_e  !transpiration (w/m2) [+ to atm]
  ELAI = LAI * (1. - FB_snow) !leaf area index, after burying by snow
  ESAI = SAI * (1. - FB_snow) !!stem area index, after burying by snow 
  BDFALL = 120.0       !bulk density of snowfall (kg/m3)
  IF (TV .GT. 273.15) THEN 
     frozen_canopy = .false.
  ELSE
     frozen_canopy = .true.
  END IF
! intercepted water
  UU = 3.0 ! wind speed
  VV = 3.0 
  IST = 1 !surface type 1-soil; 2-lake
  SNOW = 0.0  !snowfall (mm/s)
  FP = 1.0 !fraction of the gridcell that receives precipitation
  SFCTMP = 298.0 !model-level temperature (k)

!!!!!!!!!!!!!!!!!! for snow water
  IF (TG .GT. 273.15) THEN
     frozen_ground = .false.
  ELSE
     frozen_ground = .true.
  END IF
  ISNOW = 0
  SNOWH = 0.0
  SNEQV = 0.0
  WSLAKE = 0.0
  PONDING = 0.0
  PONDING1 = 0.0
  PONDING2 = 0.0
  QSNBOT = 0.0
  QSNFRO = 0.0
  QSNSUB = 0.0
  QDEW = 0.0
  QSDEW = 0.0
  SNICE = 0.0
  SNLIQ = 0.0
  STC = 298.0
  IMELT = 1 ! freeze
  FICEOLD = 0.0


!!!============================================= READ Table parameter values




!---------------------------------------------------------------------
! create output file and add initial values
!---------------------------------------------------------------------

  call initialize_output(output_filename, ntime+1, nsoil, nsnow)
  call add_to_output(0,nsoil,dzsnso,dt,qinsur,runsrf,runsub,qseva,etrani,smc,rain,&
                  qintr,qints,qdripr,qdrips,qthror,qthros,qrain,qsnow,snowhin,fwet,&
                  cmc,canliq,canice,ecan,etran,nsnow,snowh,sneqv,ponding,ponding1,ponding2,&
                  QSNBOT,QSNFRO,QSNSUB,SNICE,SNLIQ,STC,zsnso)

!---------------------------------------------------------------------
! start the time loop
!---------------------------------------------------------------------

  do itime = 1, ntime
   
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

    !SNOW = RAIN * 0.9
    !RAIN = RAIN * 0.1


!!!============================================= Start the original Water Subroutine ==========================================

! compute water budgets (water storages, ET components, and runoff)

     CALL WATER (parameters,VEGTYP ,NSNOW  ,NSOIL  ,IMELT  ,DT     ,UU     , & !in
                 VV     ,FCEV   ,FCTR   ,QPRECC ,QPRECL ,ELAI   , & !in
                 ESAI   ,SFCTMP ,QVAP   ,QDEW   ,ZSOIL  ,BTRANI , & !in
                 IRRFRA ,MIFAC  ,FIFAC  ,CROPLU ,                 & !in
                 FICEOLD,PONDING,TG     ,IST    ,FVEG   ,iloc,jloc , SMCEQ , & !in
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
                 QSNBOT ,QTLDRN                                   &
#ifdef WRF_HYDRO
                        ,sfcheadrt, WATBLED                       &
#endif
                 )  !out

!     write(*,'(a20,10F15.5)') 'SFLX:RUNOFF=',RUNSRF*DT,RUNSUB*DT,EDIR*DT

 
  !---------------------------------------------------------------------
  ! accumulate some fields and error checks
  !---------------------------------------------------------------------

    ! here needs more thoughts, originally only for OPT_RUN=3,4,5 !addrunoff also for 6,7,8
    runsub = qdrain + runsub              ! drainage [mm/s]
    acsrf  = acsrf + runsrf * dt          ! accumulated surface runoff [mm]
    acsub  = acsub + runsub * dt          ! accumulated drainage [mm]
    acpcp  = acpcp + qinsur * dt * 1000.0 ! accumulated precipitation [mm]
   
    dtheta_max = maxval(abs(smc-smcold))
!    if (dtheta_max .lt. 0.00001) done = .true.
   
    totalwat = sum(dzsnso(1:nsoil)*smc*1000.0)         ! total soil water [mm]
    errwat = acpcp - acsrf - acsub - (totalwat - tw0)  ! accum error [mm]
   
  !---------------------------------------------------------------------
  ! add to output file
  !---------------------------------------------------------------------

  call add_to_output(itime,nsoil,dzsnso,dt,qinsur,runsrf,runsub,qseva,etrani,smc,rain,&
                  qintr,qints,qdripr,qdrips,qthror,qthros,qrain,qsnow,snowhin,fwet,&
                  cmc,canliq,canice,ecan,etran,nsnow,snowh,sneqv,ponding,ponding1,ponding2,&
                  QSNBOT,QSNFRO,QSNSUB,SNICE,SNLIQ,STC,zsnso)
 
  end do ! time loop

  call finalize_output()
   
end program
