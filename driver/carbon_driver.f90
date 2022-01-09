program carbon_driver

use carbon_routines
use output
USE NOAHMP_TABLES

  implicit none

!---------------------------------------------------------------------
!  declare namelist input variable start
!---------------------------------------------------------------------
! timing
  real          :: dt
  integer       :: maxtime
  integer       :: YEARLEN
  character*256 :: output_filename
  real          :: JULIAN

! forcing
  real          :: lat
  real          :: tv
  real          :: tg
  real          :: foln
  real          :: btran
  real          :: psn
  real          :: apar
  real          :: igs
  real          :: fveg
  real          :: troot
  integer       :: ist    !surface type 1->soil; 2->lake     
  real          :: t2m
  real          :: soldn
  real          :: SNOWH

  real, allocatable, dimension(:) :: smc
  real, allocatable, dimension(:) :: stc

! structure
  integer       :: iloc
  integer       :: jloc
  integer       :: isltyp
  integer       :: vegtype,vegtyp
  integer       :: croptype
  integer       :: soilcolor
  integer       :: slopetype
  integer       :: nsoil
  integer       :: nsnow

! options
  integer       :: idveg
  integer       :: iopt_crop

! fixed_initial
  real, allocatable, dimension(:) :: zsoil   ! depth of layer-bottom from soil surface
  real, allocatable, dimension(:) :: DZSNSO  ! snow/soil layer thickness [m]

! locals

! input & output (carbon)

  REAL         :: LFMASS !leaf mass [g/m2]
  REAL         :: RTMASS !mass of fine roots [g/m2]
  REAL         :: STMASS !stem mass [g/m2]
  REAL         :: WOOD   !mass of wood (incl. woody roots) [g/m2]
  REAL         :: STBLCP !stable carbon in deep soil [g/m2]
  REAL         :: FASTCP !short-lived carbon in shallow soil [g/m2]
  REAL         :: GRAIN  !mass of GRAIN [g/m2]
  REAL         :: GDD    !growing degree days

! outputs: (carbon)

  REAL         :: GPP    !net instantaneous assimilation [g/m2/s C]
  REAL         :: NPP    !net primary productivity [g/m2/s C]
  REAL         :: NEE    !net ecosystem exchange [g/m2/s CO2]
  REAL         :: AUTORS !net ecosystem respiration [g/m2/s C]
  REAL         :: HETERS !organic respiration [g/m2/s C]
  REAL         :: TOTSC  !total soil carbon [g/m2 C]
  REAL         :: TOTLB  !total living carbon ([g/m2 C]
  REAL         :: LAI    !leaf area index [-]
  REAL         :: SAI    !stem area index [-]
  REAL         :: VOCFLX(5) ! voc fluxes [ug C m-2 h-1]

  real         :: ELAI
  real         :: ESAI
  integer      :: PGS
  !----------------------!
  !  namelist structure  !
  !----------------------!

  namelist / timing          / dt,maxtime,output_filename,yearlen
  namelist / forcing         / lat,tv,tg,foln,btran,psn,apar,fveg,troot,&
                               ist,t2m,soldn,snowh
  namelist / structure       / iloc,jloc,isltyp,vegtype,croptype,nsoil,nsnow,&
                               slopetype,soilcolor
  namelist / options         / idveg,iopt_crop
  namelist / fixed_initial   / zsoil,dzsnso

!---------------------------------------------------------------------
!  inputs end
!---------------------------------------------------------------------

  type (noahmp_parameters) :: parameters

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------

  integer :: itime, iz          ! some loop counters
  integer :: ntime      = 0     ! number of timesteps to run
  integer :: isoil
  
  integer, dimension(4)           :: SOILTYPE

  logical :: crop_active = .false.
  logical :: dveg_active = .false.

  real    :: FRZK, FRZFACT
!---------------------------------------------------------------------
!  read input file
!---------------------------------------------------------------------


  open(30, file="namelist.input", form="formatted")
   read(30, timing)
   read(30, structure)
   read(30, options)
  close(30)

!---------------------------------------------------------------------
!  allocate for dynamic levels
!---------------------------------------------------------------------

  allocate (zsoil (       1:nsoil))   !depth of layer-bottom from soil surface
  allocate (DZSNSO(-nsnow+1:nsoil))   !snow/soil layer thickness [m]
  allocate (SMC   (       1:nsoil))   !total soil water content [m3/m3]
  allocate (STC   (-nsnow+1:nsoil))   !snow/soil layer temperature [k]

  open(30, file="namelist.input", form="formatted")
   read(30, forcing)
   read(30, fixed_initial)
  close(30)

!---------------------------------------------------------------------
!  Initialize locals
!---------------------------------------------------------------------

  LFMASS = 0.0
  RTMASS = 0.0
  STMASS = 0.0
  WOOD   = 0.0
  STBLCP = 0.0
  FASTCP = 0.0
  GRAIN  = 0.0
  GDD    = 0.0

! outputs: (carbon)

  GPP    = 0.0
  NPP    = 0.0
  NEE    = 0.0
  AUTORS = 0.0
  HETERS = 0.0
  TOTSC  = 0.0
  TOTLB  = 0.0
  LAI    = 0.0
  SAI    = 0.0
  VOCFLX = 0.0
  ELAI   = 0.0
  ESAI   = 0.0
  IGS    = 0.0

  SMC    = 0.29
  STC    = 280.0

  VEGTYP = VEGTYPE

  SOILTYPE(1:4) = isltyp


!---------------------------------------------------------------------
!  transfer noah-mp options
!---------------------------------------------------------------------

  call noahmp_options(idveg, iopt_crop)

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

  ntime  = maxtime

  call initialize_output(output_filename, ntime, nsoil, nsnow)

  do itime = 1, ntime, dt

     JULIAN = itime * 1.0
     
     call PHENOLOGY (parameters, VEGTYPE, croptype, SNOWH,   TV,  LAT, YEARLEN, JULIAN, & !in
                            LAI,    SAI,    TROOT,  ELAI, ESAI,  IGS, PGS)


    
! compute carbon budgets (carbon storages and co2 & bvoc fluxes)

     crop_active = .false.
     dveg_active = .false.
     IF (DVEG == 2 .OR. DVEG == 5 .OR. DVEG == 6) dveg_active = .true.
     IF (OPT_CROP > 0 .and. CROPTYPE > 0) THEN
       crop_active = .true.
       dveg_active = .false.
     ENDIF

     IF (dveg_active) THEN
       CALL CARBON (parameters,NSNOW  ,NSOIL  ,VEGTYP ,DT     ,ZSOIL  , & !in
                   DZSNSO ,STC    ,SMC    ,TV     ,TG     ,PSN    , & !in
                   FOLN   ,BTRAN  ,APAR   ,FVEG   ,IGS    , & !in
                   TROOT  ,IST    ,LAT    ,iloc   ,jloc   , & !in
                   LFMASS ,RTMASS ,STMASS ,WOOD   ,STBLCP ,FASTCP , & !inout
                   GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,TOTSC  , & !out
                   TOTLB  ,LAI    ,SAI    )                   !out
     END IF


     IF (OPT_CROP == 1 .and. crop_active) THEN
       CALL CARBON_CROP (parameters,NSNOW  ,NSOIL  ,VEGTYP ,DT     ,ZSOIL  ,JULIAN , & !in 
                         DZSNSO ,STC    ,SMC    ,TV     ,PSN    ,FOLN   ,BTRAN  , & !in
			 SOLDN  ,T2M    ,                                         & !in
                         LFMASS ,RTMASS ,STMASS ,WOOD   ,STBLCP ,FASTCP ,GRAIN  , & !inout
			 LAI    ,SAI    ,GDD    ,                                 & !inout
                         GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,TOTSC  ,TOTLB, PGS    ) !out
     END IF
     
     call add_to_output(itime, VEGTYP, croptype, SNOWH,     TV,    LAT, YEARLEN, JULIAN, &
                          LAI,     SAI,    TROOT,  ELAI,   ESAI,    IGS,     PGS,  ZSOIL, &
                       DZSNSO,     STC,      SMC,   PSN,   FOLN,  BTRAN,   SOLDN,    T2M, &
                       LFMASS,  RTMASS,   STMASS,  WOOD, STBLCP, FASTCP,   GRAIN,    GDD, &
                          GPP,     NPP,      NEE,AUTORS, HETERS,  TOTSC,   TOTLB,     TG, &
                        NSNOW,    NSOIL,    DT,   APAR,   FVEG)

      
  end do

  call finalize_output()

end program carbon_driver
