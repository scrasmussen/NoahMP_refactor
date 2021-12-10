module noahmp_routines

 IMPLICIT NONE

! =====================================options for different schemes================================
! **recommended

  INTEGER :: DVEG     ! options for dynamic vegetation: 
                      !   1 -> off (use table LAI; use FVEG = SHDFAC from input)
                      !   2 -> on  (together with OPT_CRS = 1)
                      !   3 -> off (use table LAI; calculate FVEG)
                      ! **4 -> off (use table LAI; use maximum vegetation fraction)
                      ! **5 -> on  (use maximum vegetation fraction)
                      !   6 -> on  (use FVEG = SHDFAC from input)
                      !   7 -> off (use input LAI; use FVEG = SHDFAC from input)
                      !   8 -> off (use input LAI; calculate FVEG)
                      !   9 -> off (use input LAI; use maximum vegetation fraction)

  INTEGER :: OPT_CRS  ! options for canopy stomatal resistance
                      ! **1 -> Ball-Berry
                      !   2 -> Jarvis

  INTEGER :: OPT_BTR  ! options for soil moisture factor for stomatal resistance
                      ! **1 -> Noah (soil moisture) 
                      !   2 -> CLM  (matric potential)
                      !   3 -> SSiB (matric potential)

  INTEGER :: OPT_RUN  ! options for runoff and groundwater
                      ! **1 -> TOPMODEL with groundwater (Niu et al. 2007 JGR) ;
                      !   2 -> TOPMODEL with an equilibrium water table (Niu et al. 2005 JGR) ;
                      !   3 -> original surface and subsurface runoff (free drainage)
                      !   4 -> BATS surface and subsurface runoff (free drainage)
                      !   5 -> Miguez-Macho&Fan groundwater scheme (Miguez-Macho et al. 2007 JGR; Fan et al. 2007 JGR)
                      !          (needs further testing for public use)
                      !   6 -> Variable Infiltration Capacity Model surface runoff scheme (Wood et al., 1992, JGR)
                      !   7 -> Xiananjiang Infiltration and surface runoff scheme ((Jayawardena and Zhou, 2000)
                      !   8 -> Dynamic VIC surface runoff scheme (Liang and Xie, 2001) 

  INTEGER :: OPT_SFC  ! options for surface layer drag coeff (CH & CM)
                      ! **1 -> M-O
                      ! **2 -> original Noah (Chen97)
                      ! **3 -> MYJ consistent; 4->YSU consistent. MB: removed in v3.7 for further testing

  INTEGER :: OPT_FRZ  ! options for supercooled liquid water (or ice fraction)
                      ! **1 -> no iteration (Niu and Yang, 2006 JHM)
                      !   2 -> Koren's iteration 

  INTEGER :: OPT_INF  ! options for frozen soil permeability
                      ! **1 -> linear effects, more permeable (Niu and Yang, 2006, JHM)
                      !   2 -> nonlinear effects, less permeable (old)

  INTEGER :: OPT_RAD  ! options for radiation transfer
                      !   1 -> modified two-stream (gap = F(solar angle, 3D structure ...)<1-FVEG)
                      !   2 -> two-stream applied to grid-cell (gap = 0)
                      ! **3 -> two-stream applied to vegetated fraction (gap=1-FVEG)

  INTEGER :: OPT_ALB  ! options for ground snow surface albedo
                      !   1 -> BATS
                      ! **2 -> CLASS
  INTEGER :: OPT_SNF  ! options for partitioning  precipitation into rainfall & snowfall
                      ! **1 -> Jordan (1991)
                      !   2 -> BATS: when SFCTMP<TFRZ+2.2 
                      !   3 -> SFCTMP < TFRZ
                      !   4 -> Use WRF microphysics output
                      !   5 -> Use wetbulb temperature (Wang et al., 2019 GRL) C.He, 12/18/2020

  INTEGER :: OPT_TBOT ! options for lower boundary condition of soil temperature
                      !   1 -> zero heat flux from bottom (ZBOT and TBOT not used)
                      ! **2 -> TBOT at ZBOT (8m) read from a file (original Noah)

  INTEGER :: OPT_STC  ! options for snow/soil temperature time scheme (only layer 1)
                      ! **1 -> semi-implicit; flux top boundary condition
                      !   2 -> full implicit (original Noah); temperature top boundary condition
                      !   3 -> same as 1, but FSNO for TS calculation (generally improves snow; v3.7)

  INTEGER :: OPT_RSF  ! options for surface resistent to evaporation/sublimation
                      ! **1 -> Sakaguchi and Zeng, 2009
                      !   2 -> Sellers (1992)
                      !   3 -> adjusted Sellers to decrease RSURF for wet soil
                      !   4 -> option 1 for non-snow; rsurf = rsurf_snow for snow (set in MPTABLE); AD v3.8

  INTEGER :: OPT_SOIL ! options for defining soil properties
                      ! **1 -> use input dominant soil texture
                      !   2 -> use input soil texture that varies with depth
                      !   3 -> use soil composition (sand, clay, orgm) and pedotransfer functions (OPT_PEDO)
                      !   4 -> use input soil properties (BEXP_3D, SMCMAX_3D, etc.)

  INTEGER :: OPT_PEDO ! options for pedotransfer functions (used when OPT_SOIL = 3)
                      ! **1 -> Saxton and Rawls (2006)

  INTEGER :: OPT_CROP ! options for crop model
                      ! **0 -> No crop model, will run default dynamic vegetation
                      !   1 -> Liu, et al. 2016

  INTEGER :: OPT_IRR  ! options for irrigation
                      ! **0 -> No irrigation
                      !   1 -> Irrigation ON
                      !   2 -> irrigation trigger based on crop season Planting and harvesting dates
                      !  *3 -> irrigation trigger based on LAI threshold

  INTEGER :: OPT_IRRM ! options for irrigation method
                      ! **0 -> method based on geo_em fractions
                      !   1 -> sprinkler method
                      !   2 -> micro/drip irrigation
                      !   3 -> surface flooding

  INTEGER :: OPT_INFDV! options for infiltration in dynamic VIC runoff scheme
                      ! **1 -> Philip scheme
                      !   2 -> Green-Ampt scheme
                      !   3 -> Smith-Parlange scheme

  INTEGER :: OPT_TDRN ! options for crop model (currently only tested & calibrated to work with opt_run=3)
                      ! **0 -> No tile drainage
                      !   1 -> on (simple scheme)
                      !   2 -> on (Hooghoudt's scheme)

!------------------------------------------------------------------------------------------!
! Physical Constants:                                                                      !
!------------------------------------------------------------------------------------------!

  REAL, PARAMETER :: GRAV   = 9.80616   !acceleration due to gravity (m/s2)
  REAL, PARAMETER :: SB     = 5.67E-08  !Stefan-Boltzmann constant (w/m2/k4)
  REAL, PARAMETER :: VKC    = 0.40      !von Karman constant
  REAL, PARAMETER :: TFRZ   = 273.16    !freezing/melting point (k)
  REAL, PARAMETER :: HSUB   = 2.8440E06 !latent heat of sublimation (j/kg)
  REAL, PARAMETER :: HVAP   = 2.5104E06 !latent heat of vaporization (j/kg)
  REAL, PARAMETER :: HFUS   = 0.3336E06 !latent heat of fusion (j/kg)
  REAL, PARAMETER :: CWAT   = 4.188E06  !specific heat capacity of water (j/m3/k)
  REAL, PARAMETER :: CICE   = 2.094E06  !specific heat capacity of ice (j/m3/k)
  REAL, PARAMETER :: CPAIR  = 1004.64   !heat capacity dry air at const pres (j/kg/k)
  REAL, PARAMETER :: TKWAT  = 0.6       !thermal conductivity of water (w/m/k)
  REAL, PARAMETER :: TKICE  = 2.2       !thermal conductivity of ice (w/m/k)
  REAL, PARAMETER :: TKAIR  = 0.023     !thermal conductivity of air (w/m/k) (not used MB: 20140718)
  REAL, PARAMETER :: RAIR   = 287.04    !gas constant for dry air (j/kg/k)
  REAL, PARAMETER :: RW     = 461.269   !gas constant for  water vapor (j/kg/k)
  REAL, PARAMETER :: DENH2O = 1000.0    !density of water (kg/m3)
  REAL, PARAMETER :: DENICE = 917.0     !density of ice (kg/m3)

  INTEGER, PRIVATE, PARAMETER :: MBAND = 2
  INTEGER, PRIVATE, PARAMETER :: NSOIL = 4
  INTEGER, PRIVATE, PARAMETER :: NSTAGE = 8

  TYPE noahmp_parameters ! define a NoahMP parameters type

!------------------------------------------------------------------------------------------!
! From the veg section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!

    LOGICAL :: URBAN_FLAG
    INTEGER :: ISWATER
    INTEGER :: ISBARREN
    INTEGER :: ISICE
    INTEGER :: ISCROP
    INTEGER :: EBLFOREST

    REAL :: CH2OP              !maximum intercepted h2o per unit lai+sai (mm)
    REAL :: DLEAF              !characteristic leaf dimension (m)
    REAL :: Z0MVT              !momentum roughness length (m)
    REAL :: HVT                !top of canopy (m)
    REAL :: HVB                !bottom of canopy (m)
    REAL :: DEN                !tree density (no. of trunks per m2)
    REAL :: RC                 !tree crown radius (m)
    REAL :: MFSNO              !snowmelt m parameter ()
    REAL :: SCFFAC             !snow cover factor (m) (originally hard-coded 2.5*z0 in SCF formulation)
    REAL :: SAIM(12)           !monthly stem area index, one-sided
    REAL :: LAIM(12)           !monthly leaf area index, one-sided
    REAL :: SLA                !single-side leaf area per Kg [m2/kg]
    REAL :: DILEFC             !coeficient for leaf stress death [1/s]
    REAL :: DILEFW             !coeficient for leaf stress death [1/s]
    REAL :: FRAGR              !fraction of growth respiration  !original was 0.3 
    REAL :: LTOVRC             !leaf turnover [1/s]

    REAL :: C3PSN              !photosynthetic pathway: 0. = c4, 1. = c3
    REAL :: KC25               !co2 michaelis-menten constant at 25c (pa)
    REAL :: AKC                !q10 for kc25
    REAL :: KO25               !o2 michaelis-menten constant at 25c (pa)
    REAL :: AKO                !q10 for ko25
    REAL :: VCMX25             !maximum rate of carboxylation at 25c (umol co2/m**2/s)
    REAL :: AVCMX              !q10 for vcmx25
    REAL :: BP                 !minimum leaf conductance (umol/m**2/s)
    REAL :: MP                 !slope of conductance-to-photosynthesis relationship
    REAL :: QE25               !quantum efficiency at 25c (umol co2 / umol photon)
    REAL :: AQE                !q10 for qe25
    REAL :: RMF25              !leaf maintenance respiration at 25c (umol co2/m**2/s)
    REAL :: RMS25              !stem maintenance respiration at 25c (umol co2/kg bio/s)
    REAL :: RMR25              !root maintenance respiration at 25c (umol co2/kg bio/s)
    REAL :: ARM                !q10 for maintenance respiration
    REAL :: FOLNMX             !foliage nitrogen concentration when f(n)=1 (%)
    REAL :: TMIN               !minimum temperature for photosynthesis (k)

    REAL :: XL                 !leaf/stem orientation index
    REAL :: RHOL(MBAND)        !leaf reflectance: 1=vis, 2=nir
    REAL :: RHOS(MBAND)        !stem reflectance: 1=vis, 2=nir
    REAL :: TAUL(MBAND)        !leaf transmittance: 1=vis, 2=nir
    REAL :: TAUS(MBAND)        !stem transmittance: 1=vis, 2=nir

    REAL :: MRP                !microbial respiration parameter (umol co2 /kg c/ s)
    REAL :: CWPVT              !empirical canopy wind parameter

    REAL :: WRRAT              !wood to non-wood ratio
    REAL :: WDPOOL             !wood pool (switch 1 or 0) depending on woody or not [-]
    REAL :: TDLEF              !characteristic T for leaf freezing [K]

  INTEGER :: NROOT              !number of soil layers with root present
     REAL :: RGL                !Parameter used in radiation stress function
     REAL :: RSMIN              !Minimum stomatal resistance [s m-1]
     REAL :: HS                 !Parameter used in vapor pressure deficit function
     REAL :: TOPT               !Optimum transpiration air temperature [K]
     REAL :: RSMAX              !Maximal stomatal resistance [s m-1]

     REAL :: SLAREA
     REAL :: EPS(5)

!------------------------------------------------------------------------------------------!
! From the rad section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!

     REAL :: ALBSAT(MBAND)       !saturated soil albedos: 1=vis, 2=nir
     REAL :: ALBDRY(MBAND)       !dry soil albedos: 1=vis, 2=nir
     REAL :: ALBICE(MBAND)       !albedo land ice: 1=vis, 2=nir
     REAL :: ALBLAK(MBAND)       !albedo frozen lakes: 1=vis, 2=nir
     REAL :: OMEGAS(MBAND)       !two-stream parameter omega for snow
     REAL :: BETADS              !two-stream parameter betad for snow
     REAL :: BETAIS              !two-stream parameter betad for snow
     REAL :: EG(2)               !emissivity

!------------------------------------------------------------------------------------------!
! From the globals section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!

     REAL :: CO2          !co2 partial pressure
     REAL :: O2           !o2 partial pressure
     REAL :: TIMEAN       !gridcell mean topgraphic index (global mean)
     REAL :: FSATMX       !maximum surface saturated fraction (global mean)
     REAL :: Z0SNO        !snow surface roughness length (m) (0.002)
     REAL :: SSI          !liquid water holding capacity for snowpack (m3/m3)
     REAL :: SNOW_RET_FAC !snowpack water release timescale factor (1/s)
     REAL :: SNOW_EMIS    !snow emissivity
     REAL :: SWEMX        !new snow mass to fully cover old snow (mm)
     REAL :: TAU0         !tau0 from Yang97 eqn. 10a
     REAL :: GRAIN_GROWTH !growth from vapor diffusion Yang97 eqn. 10b
     REAL :: EXTRA_GROWTH !extra growth near freezing Yang97 eqn. 10c
     REAL :: DIRT_SOOT    !dirt and soot term Yang97 eqn. 10d
     REAL :: BATS_COSZ    !zenith angle snow albedo adjustment; b in Yang97 eqn. 15
     REAL :: BATS_VIS_NEW !new snow visible albedo
     REAL :: BATS_NIR_NEW !new snow NIR albedo
     REAL :: BATS_VIS_AGE !age factor for diffuse visible snow albedo Yang97 eqn. 17
     REAL :: BATS_NIR_AGE !age factor for diffuse NIR snow albedo Yang97 eqn. 18
     REAL :: BATS_VIS_DIR !cosz factor for direct visible snow albedo Yang97 eqn. 15
     REAL :: BATS_NIR_DIR !cosz factor for direct NIR snow albedo Yang97 eqn. 16
     REAL :: RSURF_SNOW   !surface resistance for snow(s/m)
     REAL :: RSURF_EXP    !exponent in the shape parameter for soil resistance option 1

!------------------------------------------------------------------------------------------!
! From the irrigation section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!
     REAL :: IRR_FRAC         ! irrigation Fraction
  INTEGER :: IRR_HAR          ! number of days before harvest date to stop irrigation 
     REAL :: IRR_LAI          ! Minimum lai to trigger irrigation
     REAL :: IRR_MAD          ! management allowable deficit (0-1)
     REAL :: FILOSS           ! fraction of flood irrigation loss (0-1) 
     REAL :: SPRIR_RATE       ! mm/h, sprinkler irrigation rate
     REAL :: MICIR_RATE       ! mm/h, micro irrigation rate
     REAL :: FIRTFAC          ! flood application rate factor
     REAL :: IR_RAIN          ! maximum precipitation to stop irrigation trigger

!------------------------------------------------------------------------------------------!
! From the crop section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!

  INTEGER :: PLTDAY           ! Planting date
  INTEGER :: HSDAY            ! Harvest date
     REAL :: PLANTPOP         ! Plant density [per ha] - used?
     REAL :: IRRI             ! Irrigation strategy 0= non-irrigation 1=irrigation (no water-stress)
     REAL :: GDDTBASE         ! Base temperature for GDD accumulation [C]
     REAL :: GDDTCUT          ! Upper temperature for GDD accumulation [C]
     REAL :: GDDS1            ! GDD from seeding to emergence
     REAL :: GDDS2            ! GDD from seeding to initial vegetative 
     REAL :: GDDS3            ! GDD from seeding to post vegetative 
     REAL :: GDDS4            ! GDD from seeding to intial reproductive
     REAL :: GDDS5            ! GDD from seeding to pysical maturity 
  INTEGER :: C3C4             ! photosynthetic pathway:  1 = c3 2 = c4
     REAL :: AREF             ! reference maximum CO2 assimulation rate 
     REAL :: PSNRF            ! CO2 assimulation reduction factor(0-1) (caused by non-modeling part,e.g.pest,weeds)
     REAL :: I2PAR            ! Fraction of incoming solar radiation to photosynthetically active radiation
     REAL :: TASSIM0          ! Minimum temperature for CO2 assimulation [C]
     REAL :: TASSIM1          ! CO2 assimulation linearly increasing until temperature reaches T1 [C]
     REAL :: TASSIM2          ! CO2 assmilation rate remain at Aref until temperature reaches T2 [C]
     REAL :: K                ! light extinction coefficient
     REAL :: EPSI             ! initial light use efficiency
     REAL :: Q10MR            ! q10 for maintainance respiration
     REAL :: FOLN_MX          ! foliage nitrogen concentration when f(n)=1 (%)
     REAL :: LEFREEZ          ! characteristic T for leaf freezing [K]
     REAL :: DILE_FC(NSTAGE)  ! coeficient for temperature leaf stress death [1/s]
     REAL :: DILE_FW(NSTAGE)  ! coeficient for water leaf stress death [1/s]
     REAL :: FRA_GR           ! fraction of growth respiration 
     REAL :: LF_OVRC(NSTAGE)  ! fraction of leaf turnover  [1/s]
     REAL :: ST_OVRC(NSTAGE)  ! fraction of stem turnover  [1/s]
     REAL :: RT_OVRC(NSTAGE)  ! fraction of root tunrover  [1/s]
     REAL :: LFMR25           ! leaf maintenance respiration at 25C [umol CO2/m**2  /s]
     REAL :: STMR25           ! stem maintenance respiration at 25C [umol CO2/kg bio/s]
     REAL :: RTMR25           ! root maintenance respiration at 25C [umol CO2/kg bio/s]
     REAL :: GRAINMR25        ! grain maintenance respiration at 25C [umol CO2/kg bio/s]
     REAL :: LFPT(NSTAGE)     ! fraction of carbohydrate flux to leaf
     REAL :: STPT(NSTAGE)     ! fraction of carbohydrate flux to stem
     REAL :: RTPT(NSTAGE)     ! fraction of carbohydrate flux to root
     REAL :: LFCT(NSTAGE)     ! fraction of carbohydrate flux transallocate from leaf to grain ! Zhe Zhang 2020-07-13
     REAL :: STCT(NSTAGE)     ! fraction of carbohydrate flux transallocate from stem to grain
     REAL :: RTCT(NSTAGE)     ! fraction of carbohydrate flux transallocate from root to grain
     REAL :: GRAINPT(NSTAGE)  ! fraction of carbohydrate flux to grain
     REAL :: BIO2LAI          ! leaf are per living leaf biomass [m^2/kg]

!------------------------------------------------------------------------------------------!
! From the SOILPARM.TBL tables, as functions of soil category.
!------------------------------------------------------------------------------------------!
     REAL :: BEXP(NSOIL)   !B parameter
     REAL :: SMCDRY(NSOIL) !dry soil moisture threshold where direct evap from top
                           !layer ends (volumetric) (not used MB: 20140718)
     REAL :: SMCWLT(NSOIL) !wilting point soil moisture (volumetric)
     REAL :: SMCREF(NSOIL) !reference soil moisture (field capacity) (volumetric)
     REAL :: SMCMAX(NSOIL) !porosity, saturated value of soil moisture (volumetric)
     REAL :: PSISAT(NSOIL) !saturated soil matric potential
     REAL :: DKSAT(NSOIL)  !saturated soil hydraulic conductivity
     REAL :: DWSAT(NSOIL)  !saturated soil hydraulic diffusivity
     REAL :: QUARTZ(NSOIL) !soil quartz content
     REAL :: F1            !soil thermal diffusivity/conductivity coef (not used MB: 20140718)
     REAL :: BVIC          !VIC model infiltration parameter for opt_run=6
     REAL :: AXAJ          !Xinanjiang: Tension water distribution inflection parameter [-] for opt_run=7
     REAL :: BXAJ          !Xinanjiang: Tension water distribution shape parameter [-] for opt_run=7
     REAL :: XXAJ          !Xinanjiang: Free water distribution shape parameter [-] for opt_run=7
     REAL :: BDVIC         !DVIC model infiltration parameter [-] for opt_run=8
     REAL :: GDVIC         !Mean Capillary Drive (m) for infiltration models for opt_run=8
     REAL :: BBVIC         !DVIC heterogeniety parameter for infiltration for opt_run=8

!------------------------------------------------------------------------------------------!
! From the GENPARM.TBL file
!------------------------------------------------------------------------------------------!
     REAL :: SLOPE       !slope index (0 - 1)
     REAL :: CSOIL       !vol. soil heat capacity [j/m3/K]
     REAL :: ZBOT        !Depth (m) of lower boundary soil temperature
     REAL :: CZIL        !Calculate roughness length of heat
     REAL :: REFDK
     REAL :: REFKDT

     REAL :: KDT         !used in compute maximum infiltration rate (in INFIL)
     REAL :: FRZX        !used in compute maximum infiltration rate (in INFIL)

!------------------------------------------------------------------------------------------!
! From the tiledrain section of the MPTABLE.TBL file
!------------------------------------------------------------------------------------------!
     REAL    :: TDSMC_FAC
     REAL    :: TD_DC
     INTEGER :: TD_DEPTH
     INTEGER :: DRAIN_LAYER_OPT

     REAL :: TD_DCOEF           ! m d^-1, drainage coefficent
     REAL :: TD_D               ! m, depth to impervious layer from drain water level (D)
     REAL :: TD_ADEPTH          ! m, actual depth of impervious layer from land surface
     REAL :: TD_RADI            ! m, effective radius of drains (ro)
     REAL :: TD_SPAC            ! m, distance between two drain tubes or tiles (L)
     REAL :: TD_DDRAIN          ! m, Depth of drain
     REAL :: KLAT_FAC           ! multiplication factor to determine conk(j1,j) from sol_k(j1,j) for HRU


  END TYPE noahmp_parameters

contains

!!!======================================== Start the default Water subroutine ================================
!== begin water ====================================================================================

  SUBROUTINE WATER (parameters,VEGTYP ,NSNOW  ,NSOIL  ,IMELT  ,DT     ,UU     , & !in
                    VV     ,FCEV   ,FCTR   ,QPRECC ,QPRECL ,ELAI   , & !in
                    ESAI   ,SFCTMP ,QVAP   ,QDEW   ,ZSOIL  ,BTRANI , & !in
                    IRRFRA ,MIFAC  ,FIFAC  ,CROPLU ,                 & !in
                    FICEOLD,PONDING,TG     ,IST    ,FVEG   ,ILOC   ,JLOC ,SMCEQ , & !in
                    BDFALL ,FP     ,RAIN   ,SNOW,                    & !in  MB/AN: v3.7
                    QSNOW  ,QRAIN  ,SNOWHIN,LATHEAV,LATHEAG,frozen_canopy,frozen_ground,    & !in  MB
                    DX     ,TDFRACMP,                                &
                    ISNOW  ,CANLIQ ,CANICE ,TV     ,SNOWH  ,SNEQV  , & !inout
                    SNICE  ,SNLIQ  ,STC    ,ZSNSO  ,SH2O   ,SMC    , & !inout
                    SICE   ,ZWT    ,WA     ,WT     ,DZSNSO ,WSLAKE , & !inout
                    SMCWTD ,DEEPRECH,RECH                          , & !inout
                    IRAMTFI,IRAMTMI ,IRFIRATE ,IRMIRATE,             & !inout
                    CMC    ,ECAN   ,ETRAN  ,FWET   ,RUNSRF ,RUNSUB , & !out
                    QIN    ,QDIS   ,PONDING1       ,PONDING2,        &
                    QSNBOT ,QTLDRN,                                  &
                 QINSUR,QSEVA,QSDEW,QSNFRO,QSNSUB,ETRANI,WCND,QDRAIN,SNOFLOW,FCRMAX & ! added output
#ifdef WRF_HYDRO
                        ,sfcheadrt,WATBLED                           &
#endif
                    )  !out
! ----------------------------------------------------------------------  
! Code history:
! Initial code: Guo-Yue Niu, Oct. 2007
! ----------------------------------------------------------------------
  implicit none
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                         INTENT(IN)    :: ILOC    !grid index
  INTEGER,                         INTENT(IN)    :: JLOC    !grid index
  INTEGER,                         INTENT(IN)    :: VEGTYP  !vegetation type
  INTEGER,                         INTENT(IN)    :: NSNOW   !maximum no. of snow layers
  INTEGER                        , INTENT(IN)    :: IST     !surface type 1-soil; 2-lake
  INTEGER,                         INTENT(IN)    :: NSOIL   !no. of soil layers
  INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT   !melting state index [1-melt; 2-freeze]
  REAL,                            INTENT(IN)    :: DT      !main time step (s)
  REAL,                            INTENT(IN)    :: UU      !u-direction wind speed [m/s]
  REAL,                            INTENT(IN)    :: VV      !v-direction wind speed [m/s]
  REAL,                            INTENT(IN)    :: FCEV    !canopy evaporation (w/m2) [+ to atm ]
  REAL,                            INTENT(IN)    :: FCTR    !transpiration (w/m2) [+ to atm]
  REAL,                            INTENT(IN)    :: QPRECC  !convective precipitation (mm/s)
  REAL,                            INTENT(IN)    :: QPRECL  !large-scale precipitation (mm/s)
  REAL,                            INTENT(IN)    :: ELAI    !leaf area index, after burying by snow
  REAL,                            INTENT(IN)    :: ESAI    !stem area index, after burying by snow
  REAL,                            INTENT(IN)    :: SFCTMP  !surface air temperature [k]
  REAL,                            INTENT(IN)    :: QVAP    !soil surface evaporation rate[mm/s]
  REAL,                            INTENT(IN)    :: QDEW    !soil surface dew rate[mm/s]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL   !depth of layer-bottom from soil surface
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: BTRANI  !soil water stress factor (0 to 1)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD !ice fraction at last timestep
!  REAL                           , INTENT(IN)    :: PONDING ![mm]
  REAL                           , INTENT(IN)    :: TG      !ground temperature (k)
  REAL                           , INTENT(IN)    :: FVEG    !greeness vegetation fraction (-)
  REAL                           , INTENT(IN)    :: BDFALL   !bulk density of snowfall (kg/m3) ! MB/AN: v3.7
  REAL                           , INTENT(IN)    :: FP       !fraction of the gridcell that receives precipitation ! MB/AN: v3.7
  REAL                           , INTENT(IN)    :: RAIN     !rainfall (mm/s) ! MB/AN: v3.7
  REAL                           , INTENT(IN)    :: SNOW     !snowfall (mm/s) ! MB/AN: v3.7
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: SMCEQ   !equilibrium soil water content [m3/m3] (used in m-m&f groundwater dynamics)
  REAL                           , INTENT(IN)    :: QSNOW   !snow at ground srf (mm/s) [+]
  REAL                           , INTENT(IN)    :: QRAIN   !rain at ground srf (mm) [+]
  REAL                           , INTENT(IN)    :: SNOWHIN !snow depth increasing rate (m/s)

! input/output
  INTEGER,                         INTENT(INOUT) :: ISNOW   !actual no. of snow layers
  REAL,                            INTENT(INOUT) :: CANLIQ  !intercepted liquid water (mm)
  REAL,                            INTENT(INOUT) :: CANICE  !intercepted ice mass (mm)
  REAL,                            INTENT(INOUT) :: TV      !vegetation temperature (k)
  REAL,                            INTENT(INOUT) :: SNOWH   !snow height [m]
  REAL,                            INTENT(INOUT) :: SNEQV   !snow water eqv. [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE   !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ   !snow layer liquid water [mm]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC     !snow/soil layer temperature [k]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO   !depth of snow/soil layer-bottom
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO  !snow/soil layer thickness [m]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O    !soil liquid water content [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE    !soil ice content [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC     !total soil water content [m3/m3]
  REAL,                            INTENT(INOUT) :: ZWT     !the depth to water table [m]
  REAL,                            INTENT(INOUT) :: WA      !water storage in aquifer [mm]
  REAL,                            INTENT(INOUT) :: WT      !water storage in aquifer 
                                                            !+ stuarated soil [mm]
  REAL,                            INTENT(INOUT) :: WSLAKE  !water storage in lake (can be -) (mm)
   REAL                           , INTENT(INOUT) :: PONDING ![mm]
  REAL,                            INTENT(INOUT) :: SMCWTD !soil water content between bottom of the soil and water table [m3/m3]
  REAL,                            INTENT(INOUT) :: DEEPRECH !recharge to or from the water table when deep [m]
  REAL,                            INTENT(INOUT) :: RECH !recharge to or from the water table when shallow [m] (diagnostic)
  REAL,                            INTENT(INOUT) :: QTLDRN   !tile drainage (mm/s)
  REAL,                            INTENT(IN)    :: DX
  REAL,                            INTENT(IN)    :: TDFRACMP !tile drain fraction map

! output
  REAL,                            INTENT(OUT)   :: CMC     !intercepted water per ground area (mm)
  REAL,                            INTENT(OUT)   :: ECAN    !evap of intercepted water (mm/s) [+]
  REAL,                            INTENT(OUT)   :: ETRAN   !transpiration rate (mm/s) [+]
  REAL,                            INTENT(OUT)   :: FWET    !wetted/snowed fraction of canopy (-)
  REAL,                            INTENT(OUT)   :: RUNSRF  !surface runoff [mm/s] 
  REAL,                            INTENT(OUT)   :: RUNSUB  !baseflow (sturation excess) [mm/s]
  REAL,                            INTENT(OUT)   :: QIN     !groundwater recharge [mm/s]
  REAL,                            INTENT(OUT)   :: QDIS    !groundwater discharge [mm/s]
  REAL,                            INTENT(OUT)   :: PONDING1
  REAL,                            INTENT(OUT)   :: PONDING2
  REAL,                            INTENT(OUT)   :: QSNBOT  !melting water out of snow bottom [mm/s]
  REAL                              , INTENT(IN)   :: LATHEAV !latent heat vap./sublimation (j/kg)
  REAL                              , INTENT(IN)   :: LATHEAG !latent heat vap./sublimation (j/kg)
  LOGICAL                           , INTENT(IN)   :: FROZEN_GROUND ! used to define latent heat pathway
  LOGICAL                           , INTENT(IN)   :: FROZEN_CANOPY ! used to define latent heat pathway

! irrigation 
  REAL,                              INTENT(IN)   :: IRRFRA   ! irrigation fraction
  REAL,                              INTENT(IN)   :: MIFAC    ! micro irrigation fraction
  REAL,                              INTENT(IN)   :: FIFAC    ! flood irrigation fraction
  REAL,                              INTENT(INOUT):: IRAMTFI  ! irrigation water amount [m] to be applied, Sprinkler
  REAL,                              INTENT(INOUT):: IRAMTMI  ! irrigation water amount [m] to be applied, Micro
  REAL,                              INTENT(INOUT):: IRFIRATE ! rate of irrigation by micro [m/timestep]
  REAL,                              INTENT(INOUT):: IRMIRATE ! rate of irrigation by micro [m/timestep]
  LOGICAL,                           INTENT(IN)   :: CROPLU   ! flag to identify croplands

! local
  INTEGER                                        :: IZ
  REAL,                              INTENT(INOUT) :: QINSUR  !water input on soil surface [m/s]
  REAL                             ,INTENT(INOUT):: QSEVA   !soil surface evap rate [mm/s]
  REAL                             ,INTENT(INOUT):: QSDEW   !soil surface dew rate [mm/s]
  REAL                             ,INTENT(INOUT):: QSNFRO  !snow surface frost rate[mm/s]
  REAL                             ,INTENT(INOUT):: QSNSUB  !snow surface sublimation rate [mm/s]
  REAL, DIMENSION(       1:NSOIL)  ,INTENT(INOUT):: ETRANI  !transpiration rate (mm/s) [+]
  REAL, DIMENSION(       1:NSOIL)  ,INTENT(INOUT):: WCND   !hydraulic conductivity (m/s)
  REAL                             ,INTENT(INOUT):: QDRAIN  !soil-bottom free drainage [mm/s] 
  REAL                             ,INTENT(INOUT):: SNOFLOW !glacier flow [mm/s]
  REAL                             ,INTENT(INOUT):: FCRMAX !maximum of FCR (-)

  REAL, PARAMETER ::  WSLMAX = 5000.0      !maximum lake water storage (mm)

#ifdef WRF_HYDRO
  REAL                           , INTENT(INOUT)    :: sfcheadrt, WATBLED
#endif

! ----------------------------------------------------------------------
! initialize

   ETRANI(1:NSOIL) = 0.0
   SNOFLOW         = 0.0
   RUNSUB          = 0.0
   QINSUR          = 0.0

! canopy-intercepted snowfall/rainfall, drips, and throughfall

   CALL CANWATER (parameters,VEGTYP ,DT     , & !in
                  FCEV   ,FCTR   ,ELAI   , & !in
                  ESAI   ,TG     ,FVEG   ,ILOC   , JLOC, & !in
                  BDFALL ,FROZEN_CANOPY  , & !in     
                  CANLIQ ,CANICE ,TV     ,                 & !inout
                  CMC    ,ECAN   ,ETRAN  , & !out
                  FWET      )                           !out

! sublimation, frost, evaporation, and dew

     QSNSUB = 0.0
     IF (SNEQV > 0.0) THEN
       QSNSUB = MIN(QVAP, SNEQV/DT)
     ENDIF
     QSEVA = QVAP-QSNSUB

     QSNFRO = 0.0
     IF (SNEQV > 0.0) THEN
        QSNFRO = QDEW
     ENDIF
     QSDEW = QDEW - QSNFRO

      CALL SNOWWATER (parameters,NSNOW  ,NSOIL  ,IMELT  ,DT     ,ZSOIL  , & !in
          &          SFCTMP ,SNOWHIN,QSNOW  ,QSNFRO ,QSNSUB , & !in
          &          QRAIN  ,FICEOLD,ILOC   ,JLOC   ,         & !in
          &          ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  , & !inout
          &          SH2O   ,SICE   ,STC    ,ZSNSO  ,DZSNSO , & !inout
          &          QSNBOT ,SNOFLOW,PONDING1       ,PONDING2)  !out

   IF(FROZEN_GROUND) THEN
      SICE(1) =  SICE(1) + (QSDEW-QSEVA)*DT/(DZSNSO(1)*1000.0)
      QSDEW = 0.0
      QSEVA = 0.0
      IF(SICE(1) < 0.0) THEN
         SH2O(1) = SH2O(1) + SICE(1)
         SICE(1) = 0.0
      END IF
   END IF

! convert units (mm/s -> m/s)

    !PONDING: melting water from snow when there is no layer
    QINSUR = (PONDING+PONDING1+PONDING2)/DT * 0.001
!    QINSUR = PONDING/DT * 0.001

    IF(ISNOW == 0) THEN
       QINSUR = QINSUR+(QSNBOT + QSDEW + QRAIN) * 0.001
    ELSE
       QINSUR = QINSUR+(QSNBOT + QSDEW) * 0.001
    ENDIF

    QSEVA  = QSEVA * 0.001

    DO IZ = 1, parameters%NROOT
       ETRANI(IZ) = ETRAN * BTRANI(IZ) * 0.001
    ENDDO

#ifdef WRF_HYDRO
       QINSUR = QINSUR+sfcheadrt/DT*0.001  !sfcheadrt units (m)
#endif

! irrigation: call flood irrigation-pvk
    IF((CROPLU .EQV. .TRUE.) .AND. (IRAMTFI .GT. 0.0))THEN
       ! call flood irrigation and add to QINSUR
       CALL FLOOD_IRRIGATION(parameters,NSOIL,DT,SH2O,SMC,SICE,FIFAC,& !in
                             IRAMTFI,IRFIRATE)                         !inout
       QINSUR = QINSUR + (IRFIRATE/DT)                                ![m/s]
    END IF

! irrigation: call micro irrigation-pvk
    IF((CROPLU .EQV. .TRUE.) .AND. (IRAMTMI .GT. 0.0))THEN
       ! call micro irrigation, assuming we implement drip in first layer 
       ! of the Noah-MP. Change layer 1 moisture wrt to MI rate-pvk
       CALL MICRO_IRRIGATION(parameters,NSOIL,DT,SH2O,SMC,SICE,MIFAC, & !in
                             IRAMTMI,IRMIRATE)                          !inout
       SH2O(1) = SH2O(1) + (IRMIRATE/(-1.0*ZSOIL(1)))
    END IF

! lake/soil water balances

    IF (IST == 2) THEN                                        ! lake
       RUNSRF = 0.0
       IF(WSLAKE >= WSLMAX) RUNSRF = QINSUR*1000.0             !mm/s
       WSLAKE = WSLAKE + (QINSUR-QSEVA)*1000.0*DT -RUNSRF*DT   !mm
    ELSE                                                      ! soil
       CALL      SOILWATER (parameters,NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & !in
                            QINSUR ,QSEVA  ,ETRANI ,SICE   ,ILOC   , JLOC ,     & !in
                            TDFRACMP,DX    ,                                    & !in
                            SH2O   ,SMC    ,ZWT    ,VEGTYP ,                    & !inout
                            SMCWTD, DEEPRECH,                                   & !inout
                            RUNSRF ,QDRAIN ,RUNSUB ,WCND   ,FCRMAX, QTLDRN      & !out
#ifdef WRF_HYDRO 
                            ,WATBLED                                            & !in for tile drainage
#endif
                            )

       IF(OPT_RUN == 1) THEN
          CALL GROUNDWATER (parameters,NSNOW  ,NSOIL  ,DT     ,SICE   ,ZSOIL  , & !in
                            STC    ,WCND   ,FCRMAX ,ILOC   ,JLOC   , & !in
                            SH2O   ,ZWT    ,WA     ,WT     ,         & !inout
                            QIN    ,QDIS   )                           !out
          RUNSUB       = QDIS          !mm/s
       END IF

       IF(OPT_RUN == 3 .or. OPT_RUN == 4 .or. &
          OPT_RUN == 6 .or. OPT_RUN == 7 .or. OPT_RUN == 8) THEN
          RUNSUB       = RUNSUB + QDRAIN        !mm/s
       END IF

       DO IZ = 1,NSOIL
           SMC(IZ) = SH2O(IZ) + SICE(IZ)
       ENDDO

       IF(OPT_RUN == 5) THEN
          CALL SHALLOWWATERTABLE (parameters,NSNOW  ,NSOIL, ZSOIL, DT       , & !in
                         DZSNSO ,SMCEQ   ,ILOC , JLOC        , & !in
                         SMC    ,ZWT    ,SMCWTD ,RECH, QDRAIN  ) !inout

          SH2O(NSOIL) = SMC(NSOIL) - SICE(NSOIL)
          RUNSUB = RUNSUB + QDRAIN !it really comes from subroutine watertable, which is not called with the same frequency as the soil routines here
          WA = 0.0
       ENDIF

    ENDIF

    RUNSUB       = RUNSUB + SNOFLOW         !mm/s

  END SUBROUTINE WATER

!== begin canwater =================================================================================

  SUBROUTINE CANWATER (parameters,VEGTYP ,DT     , & !in
                       FCEV   ,FCTR   ,ELAI   , & !in
                       ESAI   ,TG     ,FVEG   ,ILOC   , JLOC , & !in
                       BDFALL ,FROZEN_CANOPY  ,  & !in      
                       CANLIQ ,CANICE ,TV     ,                 & !inout
                       CMC    ,ECAN   ,ETRAN  , & !out
                       FWET      )                           !out

! ------------------------ code history ------------------------------
! canopy hydrology
! --------------------------------------------------------------------
  IMPLICIT NONE
! ------------------------ input/output variables --------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,INTENT(IN)  :: ILOC    !grid index
  INTEGER,INTENT(IN)  :: JLOC    !grid index
  INTEGER,INTENT(IN)  :: VEGTYP  !vegetation type
  REAL,   INTENT(IN)  :: DT      !main time step (s)
  REAL,   INTENT(IN)  :: FCEV    !canopy evaporation (w/m2) [+ = to atm]
  REAL,   INTENT(IN)  :: FCTR    !transpiration (w/m2) [+ = to atm]
  REAL,   INTENT(IN)  :: ELAI    !leaf area index, after burying by snow
  REAL,   INTENT(IN)  :: ESAI    !stem area index, after burying by snow
  REAL,   INTENT(IN)  :: TG      !ground temperature (k)
  REAL,   INTENT(IN)  :: FVEG    !greeness vegetation fraction (-)
  LOGICAL                           , INTENT(IN)   :: FROZEN_CANOPY ! used to define latent heat pathway
  REAL                           , INTENT(IN)    :: BDFALL   !bulk density of snowfall (kg/m3) ! MB/AN: v3.7

! input & output
  REAL, INTENT(INOUT) :: CANLIQ  !intercepted liquid water (mm)
  REAL, INTENT(INOUT) :: CANICE  !intercepted ice mass (mm)
  REAL, INTENT(INOUT) :: TV      !vegetation temperature (k)

! output
  REAL, INTENT(OUT)   :: CMC     !intercepted water (mm)
  REAL, INTENT(OUT)   :: ECAN    !evaporation of intercepted water (mm/s) [+]
  REAL, INTENT(OUT)   :: ETRAN   !transpiration rate (mm/s) [+]
  REAL, INTENT(OUT)   :: FWET    !wetted or snowed fraction of the canopy (-)
! --------------------------------------------------------------------

! ------------------------ local variables ---------------------------
  REAL                :: MAXSNO  !canopy capacity for snow interception (mm)
  REAL                :: MAXLIQ  !canopy capacity for rain interception (mm)
  REAL                :: QEVAC   !evaporation rate (mm/s)
  REAL                :: QDEWC   !dew rate (mm/s)
  REAL                :: QFROC   !frost rate (mm/s)
  REAL                :: QSUBC   !sublimation rate (mm/s)
  REAL                :: QMELTC  !melting rate of canopy snow (mm/s)
  REAL                :: QFRZC   !refreezing rate of canopy liquid water (mm/s)
  REAL                :: CANMAS  !total canopy mass (kg/m2)
! --------------------------------------------------------------------
! initialization

      ECAN    = 0.0

! --------------------------- liquid water ------------------------------
! maximum canopy water

      MAXLIQ =  parameters%CH2OP * (ELAI+ ESAI)

! evaporation, transpiration, and dew

      IF (.NOT.FROZEN_CANOPY) THEN             ! Barlage: change to frozen_canopy
        ETRAN = MAX( FCTR/HVAP, 0.0 )
        QEVAC = MAX( FCEV/HVAP, 0.0 )
        QDEWC = ABS( MIN( FCEV/HVAP, 0.0 ) )
        QSUBC = 0.0
        QFROC = 0.0
      ELSE
        ETRAN = MAX( FCTR/HSUB, 0.0 )
        QEVAC = 0.0
        QDEWC = 0.0
        QSUBC = MAX( FCEV/HSUB, 0.0 )
        QFROC = ABS( MIN( FCEV/HSUB, 0.0 ) )
      ENDIF

! canopy water balance. for convenience allow dew to bring CANLIQ above
! maxh2o or else would have to re-adjust drip

       QEVAC = MIN(CANLIQ/DT,QEVAC)
       CANLIQ=MAX(0.0,CANLIQ+(QDEWC-QEVAC)*DT)
       IF(CANLIQ <= 1.E-06) CANLIQ = 0.0

! --------------------------- canopy ice ------------------------------
! for canopy ice

      MAXSNO = 6.6*(0.27+46.0/BDFALL) * (ELAI+ ESAI)

      QSUBC = MIN(CANICE/DT,QSUBC)
      CANICE= MAX(0.0,CANICE + (QFROC-QSUBC)*DT)
      IF(CANICE.LE.1.E-6) CANICE = 0.0

! wetted fraction of canopy

      IF(CANICE.GT.0.0) THEN
           FWET = MAX(0.0,CANICE) / MAX(MAXSNO,1.E-06)
      ELSE
           FWET = MAX(0.0,CANLIQ) / MAX(MAXLIQ,1.E-06)
      ENDIF
      FWET = MIN(FWET, 1.0) ** 0.667

! phase change

      QMELTC = 0.0
      QFRZC = 0.0

      IF(CANICE.GT.1.E-6.AND.TV.GT.TFRZ) THEN
         QMELTC = MIN(CANICE/DT,(TV-TFRZ)*CICE*CANICE/DENICE/(DT*HFUS))
         CANICE = MAX(0.0,CANICE - QMELTC*DT)
         CANLIQ = MAX(0.0,CANLIQ + QMELTC*DT)
         TV     = FWET*TFRZ + (1.0 - FWET)*TV
      ENDIF

      IF(CANLIQ.GT.1.E-6.AND.TV.LT.TFRZ) THEN
         QFRZC  = MIN(CANLIQ/DT,(TFRZ-TV)*CWAT*CANLIQ/DENH2O/(DT*HFUS))
         CANLIQ = MAX(0.0,CANLIQ - QFRZC*DT)
         CANICE = MAX(0.0,CANICE + QFRZC*DT)
         TV     = FWET*TFRZ + (1.0 - FWET)*TV
      ENDIF

! total canopy water

      CMC = CANLIQ + CANICE

! total canopy evaporation

      ECAN = QEVAC + QSUBC - QDEWC - QFROC

  END SUBROUTINE CANWATER

!== begin snowwater ================================================================================

  SUBROUTINE SNOWWATER (parameters,NSNOW  ,NSOIL  ,IMELT  ,DT     ,ZSOIL  , & !in
                        SFCTMP ,SNOWHIN,QSNOW  ,QSNFRO ,QSNSUB , & !in
                        QRAIN  ,FICEOLD,ILOC   ,JLOC   ,         & !in
                        ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  , & !inout
                        SH2O   ,SICE   ,STC    ,ZSNSO  ,DZSNSO , & !inout
                        QSNBOT ,SNOFLOW,PONDING1       ,PONDING2)  !out
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                         INTENT(IN)    :: ILOC   !grid index
  INTEGER,                         INTENT(IN)    :: JLOC   !grid index
  INTEGER,                         INTENT(IN)    :: NSNOW  !maximum no. of snow layers
  INTEGER,                         INTENT(IN)    :: NSOIL  !no. of soil layers
  INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT  !melting state index [0-no melt;1-melt]
  REAL,                            INTENT(IN)    :: DT     !time step (s)
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !depth of layer-bottom from soil surface
  REAL,                            INTENT(IN)    :: SFCTMP !surface air temperature [k]
  REAL,                            INTENT(IN)    :: SNOWHIN!snow depth increasing rate (m/s)
  REAL,                            INTENT(IN)    :: QSNOW  !snow at ground srf (mm/s) [+]
  REAL,                            INTENT(IN)    :: QSNFRO !snow surface frost rate[mm/s]
  REAL,                            INTENT(IN)    :: QSNSUB !snow surface sublimation rate[mm/s]
  REAL,                            INTENT(IN)    :: QRAIN  !snow surface rain rate[mm/s]
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(IN)    :: FICEOLD!ice fraction at last timestep

! input & output
  INTEGER,                         INTENT(INOUT) :: ISNOW  !actual no. of snow layers
  REAL,                            INTENT(INOUT) :: SNOWH  !snow height [m]
  REAL,                            INTENT(INOUT) :: SNEQV  !snow water eqv. [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !soil liquid moisture (m3/m3)
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE   !soil ice moisture (m3/m3)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow layer temperature [k]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  !depth of snow/soil layer-bottom
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO !snow/soil layer thickness [m]

! output
  REAL,                              INTENT(OUT) :: QSNBOT !melting water out of snow bottom [mm/s]
  REAL,                              INTENT(OUT) :: SNOFLOW!glacier flow [mm]
  REAL,                              INTENT(OUT) :: PONDING1
  REAL,                              INTENT(OUT) :: PONDING2

! local
  INTEGER :: IZ,i
  REAL    :: BDSNOW  !bulk density of snow (kg/m3)
! ----------------------------------------------------------------------
   SNOFLOW = 0.0
   PONDING1 = 0.0
   PONDING2 = 0.0

   CALL SNOWFALL (parameters,NSOIL  ,NSNOW  ,DT     ,QSNOW  ,SNOWHIN, & !in
                  SFCTMP ,ILOC   ,JLOC   ,                 & !in
                  ISNOW  ,SNOWH  ,DZSNSO ,STC    ,SNICE  , & !inout
                  SNLIQ  ,SNEQV  )                           !inout

! MB: do each if block separately

   IF(ISNOW < 0) &        ! when multi-layer
   CALL  COMPACT (parameters,NSNOW  ,NSOIL  ,DT     ,STC    ,SNICE  , & !in
                  SNLIQ  ,ZSOIL  ,IMELT  ,FICEOLD,ILOC   , JLOC ,& !in
                  ISNOW  ,DZSNSO ,ZSNSO  )                   !inout

   IF(ISNOW < 0) &        !when multi-layer
   CALL  COMBINE (parameters,NSNOW  ,NSOIL  ,ILOC   ,JLOC   ,         & !in
                  ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & !inout
                  DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & !inout
                  PONDING1       ,PONDING2)                  !out

   IF(ISNOW < 0) &        !when multi-layer
   CALL   DIVIDE (parameters,NSNOW  ,NSOIL  ,                         & !in
                  ISNOW  ,STC    ,SNICE  ,SNLIQ  ,DZSNSO )   !inout

   CALL  SNOWH2O (parameters,NSNOW  ,NSOIL  ,DT     ,QSNFRO ,QSNSUB , & !in 
                  QRAIN  ,ILOC   ,JLOC   ,                 & !in
                  ISNOW  ,DZSNSO ,SNOWH  ,SNEQV  ,SNICE  , & !inout
                  SNLIQ  ,SH2O   ,SICE   ,STC    ,         & !inout
                  QSNBOT ,PONDING1       ,PONDING2)           !out

!set empty snow layers to zero

   do iz = -nsnow+1, isnow
        snice(iz) = 0.0
        snliq(iz) = 0.0
        stc(iz)   = 0.0
        dzsnso(iz)= 0.0
        zsnso(iz) = 0.0
   enddo

!to obtain equilibrium state of snow in glacier region

   IF(SNEQV > 5000.0) THEN   ! 5000 mm -> maximum water depth
      BDSNOW      = SNICE(0) / DZSNSO(0)
      SNOFLOW     = (SNEQV - 5000.0)
      SNICE(0)    = SNICE(0)  - SNOFLOW
      DZSNSO(0)   = DZSNSO(0) - SNOFLOW/BDSNOW
      SNOFLOW     = SNOFLOW / DT
   END IF

! sum up snow mass for layered snow

   IF(ISNOW < 0) THEN  ! MB: only do for multi-layer
       SNEQV = 0.0
       DO IZ = ISNOW+1,0
             SNEQV = SNEQV + SNICE(IZ) + SNLIQ(IZ)
       ENDDO
   END IF

! Reset ZSNSO and layer thinkness DZSNSO

   DO IZ = ISNOW+1, 0
        DZSNSO(IZ) = -DZSNSO(IZ)
   END DO

   DZSNSO(1) = ZSOIL(1)
   DO IZ = 2,NSOIL
        DZSNSO(IZ) = (ZSOIL(IZ) - ZSOIL(IZ-1))
   END DO

   ZSNSO(ISNOW+1) = DZSNSO(ISNOW+1)
   DO IZ = ISNOW+2 ,NSOIL
       ZSNSO(IZ) = ZSNSO(IZ-1) + DZSNSO(IZ)
   ENDDO

   DO IZ = ISNOW+1 ,NSOIL
       DZSNSO(IZ) = -DZSNSO(IZ)
   END DO

  END SUBROUTINE SNOWWATER

!== begin snowfall =================================================================================

  SUBROUTINE SNOWFALL (parameters,NSOIL  ,NSNOW  ,DT     ,QSNOW  ,SNOWHIN , & !in
                       SFCTMP ,ILOC   ,JLOC   ,                  & !in
                       ISNOW  ,SNOWH  ,DZSNSO ,STC    ,SNICE   , & !inout
                       SNLIQ  ,SNEQV  )                            !inout
! ----------------------------------------------------------------------
! snow depth and density to account for the new snowfall.
! new values of snow depth & density returned.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                            INTENT(IN) :: ILOC   !grid index
  INTEGER,                            INTENT(IN) :: JLOC   !grid index
  INTEGER,                            INTENT(IN) :: NSOIL  !no. of soil layers
  INTEGER,                            INTENT(IN) :: NSNOW  !maximum no. of snow layers
  REAL,                               INTENT(IN) :: DT     !main time step (s)
  REAL,                               INTENT(IN) :: QSNOW  !snow at ground srf (mm/s) [+]
  REAL,                               INTENT(IN) :: SNOWHIN!snow depth increasing rate (m/s)
  REAL,                               INTENT(IN) :: SFCTMP !surface air temperature [k]

! input and output

  INTEGER,                         INTENT(INOUT) :: ISNOW  !actual no. of snow layers
  REAL,                            INTENT(INOUT) :: SNOWH  !snow depth [m]
  REAL,                            INTENT(INOUT) :: SNEQV  !swow water equivalent [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO !thickness of snow/soil layers (m)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow layer temperature [k]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]

! local

  INTEGER :: NEWNODE            ! 0-no new layers, 1-creating new layers
! ----------------------------------------------------------------------
    NEWNODE  = 0

! shallow snow / no layer

    IF(ISNOW == 0 .and. QSNOW > 0.0)  THEN
      SNOWH = SNOWH + SNOWHIN * DT
      SNEQV = SNEQV + QSNOW * DT
    END IF

! creating a new layer

    IF(ISNOW == 0  .AND. QSNOW>0.0 .AND. SNOWH >= 0.025) THEN !MB: change limit
!    IF(ISNOW == 0  .AND. QSNOW>0. .AND. SNOWH >= 0.05) THEN
      ISNOW    = -1
      NEWNODE  =  1
      DZSNSO(0)= SNOWH
      SNOWH    = 0.0
      STC(0)   = MIN(273.16, SFCTMP)   ! temporary setup
      SNICE(0) = SNEQV
      SNLIQ(0) = 0.0
    END IF

! snow with layers

    IF(ISNOW <  0 .AND. NEWNODE == 0 .AND. QSNOW > 0.0) then
         SNICE(ISNOW+1)  = SNICE(ISNOW+1)   + QSNOW   * DT
         DZSNSO(ISNOW+1) = DZSNSO(ISNOW+1)  + SNOWHIN * DT
    ENDIF

! ----------------------------------------------------------------------
  END SUBROUTINE SNOWFALL

!== begin combine ==================================================================================

  SUBROUTINE COMBINE (parameters,NSNOW  ,NSOIL  ,ILOC   ,JLOC   ,         & !in
                      ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & !inout
                      DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & !inout
                      PONDING1       ,PONDING2)                  !out
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER, INTENT(IN)     :: ILOC
    INTEGER, INTENT(IN)     :: JLOC
    INTEGER, INTENT(IN)     :: NSNOW                        !maximum no. of snow layers
    INTEGER, INTENT(IN)     :: NSOIL                        !no. of soil layers

! input and output

    INTEGER,                         INTENT(INOUT) :: ISNOW !actual no. of snow layers
    REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O  !soil liquid moisture (m3/m3)
    REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE  !soil ice moisture (m3/m3)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC   !snow layer temperature [k]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE !snow layer ice [mm]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ !snow layer liquid water [mm]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO!snow layer depth [m]
    REAL,                            INTENT(INOUT) :: sneqv !snow water equivalent [m]
    REAL,                            INTENT(INOUT) :: snowh !snow depth [m]
    REAL,                            INTENT(OUT) :: PONDING1
    REAL,                            INTENT(OUT) :: PONDING2

! local variables:

    INTEGER :: I,J,K,L               ! node indices
    INTEGER :: ISNOW_OLD             ! number of top snow layer
    INTEGER :: MSSI                  ! node index
    INTEGER :: NEIBOR                ! adjacent node selected for combination
    REAL    :: ZWICE                 ! total ice mass in snow
    REAL    :: ZWLIQ                 ! total liquid water in snow

    REAL    :: DZMIN(3)              ! minimum of top snow layer
!    DATA DZMIN /0.045, 0.05, 0.2/
    DATA DZMIN /0.025, 0.025, 0.1/  ! MB: change limit
!-----------------------------------------------------------------------

       ISNOW_OLD = ISNOW

       DO J = ISNOW_OLD+1,0
          IF (SNICE(J) <= 0.1) THEN
             IF(J /= 0) THEN
                SNLIQ(J+1) = SNLIQ(J+1) + SNLIQ(J)
                SNICE(J+1) = SNICE(J+1) + SNICE(J)
                DZSNSO(J+1) = DZSNSO(J+1) + DZSNSO(J)
             ELSE
               IF (ISNOW_OLD < -1) THEN    ! MB/KM: change to ISNOW
                SNLIQ(J-1) = SNLIQ(J-1) + SNLIQ(J)
                SNICE(J-1) = SNICE(J-1) + SNICE(J)
                DZSNSO(J-1) = DZSNSO(J-1) + DZSNSO(J)
               ELSE
                 IF(SNICE(J) >= 0.0) THEN
                  PONDING1 = SNLIQ(J)    ! ISNOW WILL GET SET TO ZERO BELOW; PONDING1 WILL GET 
                  SNEQV = SNICE(J)       ! ADDED TO PONDING FROM PHASECHANGE PONDING SHOULD BE
                  SNOWH = DZSNSO(J)      ! ZERO HERE BECAUSE IT WAS CALCULATED FOR THIN SNOW
                 ELSE   ! SNICE OVER-SUBLIMATED EARLIER
                  PONDING1 = SNLIQ(J) + SNICE(J)
                  IF(PONDING1 < 0.0) THEN  ! IF SNICE AND SNLIQ SUBLIMATES REMOVE FROM SOIL
                   SICE(1) = MAX(0.0,SICE(1)+PONDING1/(DZSNSO(1)*1000.0))
                   PONDING1 = 0.0
                  END IF
                  SNEQV = 0.0
                  SNOWH = 0.0
                 END IF
                 SNLIQ(J) = 0.0
                 SNICE(J) = 0.0
                 DZSNSO(J) = 0.0
               ENDIF
!                SH2O(1) = SH2O(1)+SNLIQ(J)/(DZSNSO(1)*1000.)
!                SICE(1) = SICE(1)+SNICE(J)/(DZSNSO(1)*1000.)
             ENDIF

             ! shift all elements above this down by one.
             IF (J > ISNOW+1 .AND. ISNOW < -1) THEN
                DO I = J, ISNOW+2, -1
                   STC(I)   = STC(I-1)
                   SNLIQ(I) = SNLIQ(I-1)
                   SNICE(I) = SNICE(I-1)
                   DZSNSO(I)= DZSNSO(I-1)
                END DO
             END IF
             ISNOW = ISNOW + 1
          END IF
       END DO

! to conserve water in case of too large surface sublimation

       IF(SICE(1) < 0.0) THEN
          SH2O(1) = SH2O(1) + SICE(1)
          SICE(1) = 0.0
       END IF

       IF(ISNOW ==0) RETURN   ! MB: get out if no longer multi-layer

       SNEQV  = 0.0
       SNOWH  = 0.0
       ZWICE  = 0.0
       ZWLIQ  = 0.0

       DO J = ISNOW+1,0
             SNEQV = SNEQV + SNICE(J) + SNLIQ(J)
             SNOWH = SNOWH + DZSNSO(J)
             ZWICE = ZWICE + SNICE(J)
             ZWLIQ = ZWLIQ + SNLIQ(J)
       END DO

! check the snow depth - all snow gone
! the liquid water assumes ponding on soil surface.

       IF (SNOWH < 0.025 .AND. ISNOW < 0 ) THEN ! MB: change limit
!       IF (SNOWH < 0.05 .AND. ISNOW < 0 ) THEN
          ISNOW  = 0
          SNEQV = ZWICE
          PONDING2 = ZWLIQ           ! LIMIT OF ISNOW < 0 MEANS INPUT PONDING
          IF(SNEQV <= 0.0) SNOWH = 0.0 ! SHOULD BE ZERO; SEE ABOVE
       END IF

!       IF (SNOWH < 0.05 ) THEN
!          ISNOW  = 0
!          SNEQV = ZWICE
!          SH2O(1) = SH2O(1) + ZWLIQ / (DZSNSO(1) * 1000.)
!          IF(SNEQV <= 0.) SNOWH = 0.
!       END IF

! check the snow depth - snow layers combined

       IF (ISNOW < -1) THEN

          ISNOW_OLD = ISNOW
          MSSI     = 1

          DO I = ISNOW_OLD+1,0
             IF (DZSNSO(I) < DZMIN(MSSI)) THEN

                IF (I == ISNOW+1) THEN
                   NEIBOR = I + 1
                ELSE IF (I == 0) THEN
                   NEIBOR = I - 1
                ELSE
                   NEIBOR = I + 1
                   IF ((DZSNSO(I-1)+DZSNSO(I)) < (DZSNSO(I+1)+DZSNSO(I))) NEIBOR = I-1
                END IF


                ! Node l and j are combined and stored as node j.
                IF (NEIBOR > I) THEN
                   J = NEIBOR
                   L = I
                ELSE
                   J = I
                   L = NEIBOR
                END IF

                CALL COMBO (parameters,DZSNSO(J), SNLIQ(J), SNICE(J), &
                   STC(J), DZSNSO(L), SNLIQ(L), SNICE(L), STC(L) )

                ! Now shift all elements above this down one.
                IF (J-1 > ISNOW+1) THEN
                   DO K = J-1, ISNOW+2, -1
                      STC(K)   = STC(K-1)
                      SNICE(K) = SNICE(K-1)
                      SNLIQ(K) = SNLIQ(K-1)
                      DZSNSO(K) = DZSNSO(K-1)
                   END DO
                END IF

                ! Decrease the number of snow layers
                ISNOW = ISNOW + 1
                IF (ISNOW >= -1) EXIT
             ELSE

                ! The layer thickness is greater than the prescribed minimum value
                MSSI = MSSI + 1

             END IF
          END DO

       END IF

  END SUBROUTINE COMBINE


!== begin divide ===================================================================================

  SUBROUTINE DIVIDE (parameters,NSNOW  ,NSOIL  ,                         & !in
                     ISNOW  ,STC    ,SNICE  ,SNLIQ  ,DZSNSO  )  !inout
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER, INTENT(IN)                            :: NSNOW !maximum no. of snow layers [ =3]
    INTEGER, INTENT(IN)                            :: NSOIL !no. of soil layers [ =4]

! input and output

    INTEGER                        , INTENT(INOUT) :: ISNOW !actual no. of snow layers 
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC   !snow layer temperature [k]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE !snow layer ice [mm]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ !snow layer liquid water [mm]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO!snow layer depth [m]

! local variables:

    INTEGER                                        :: J     !indices
    INTEGER                                        :: MSNO  !number of layer (top) to MSNO (bot)
    REAL                                           :: DRR   !thickness of the combined [m]
    REAL, DIMENSION(       1:NSNOW)                :: DZ    !snow layer thickness [m]
    REAL, DIMENSION(       1:NSNOW)                :: SWICE !partial volume of ice [m3/m3]
    REAL, DIMENSION(       1:NSNOW)                :: SWLIQ !partial volume of liquid water [m3/m3]
    REAL, DIMENSION(       1:NSNOW)                :: TSNO  !node temperature [k]
    REAL                                           :: ZWICE !temporary
    REAL                                           :: ZWLIQ !temporary
    REAL                                           :: PROPOR!temporary
    REAL                                           :: DTDZ  !temporary
! ----------------------------------------------------------------------

    DO J = 1,NSNOW
          IF (J <= ABS(ISNOW)) THEN
             DZ(J)    = DZSNSO(J+ISNOW)
             SWICE(J) = SNICE(J+ISNOW)
             SWLIQ(J) = SNLIQ(J+ISNOW)
             TSNO(J)  = STC(J+ISNOW)
          END IF
    END DO

       MSNO = ABS(ISNOW)

       IF (MSNO == 1) THEN
          ! Specify a new snow layer
          IF (DZ(1) > 0.05) THEN
             MSNO = 2
             DZ(1)    = DZ(1)/2.0
             SWICE(1) = SWICE(1)/2.0
             SWLIQ(1) = SWLIQ(1)/2.0
             DZ(2)    = DZ(1)
             SWICE(2) = SWICE(1)
             SWLIQ(2) = SWLIQ(1)
             TSNO(2)  = TSNO(1)
          END IF
       END IF

       IF (MSNO > 1) THEN
          IF (DZ(1) > 0.05) THEN
             DRR      = DZ(1) - 0.05
             PROPOR   = DRR/DZ(1)
             ZWICE    = PROPOR*SWICE(1)
             ZWLIQ    = PROPOR*SWLIQ(1)
             PROPOR   = 0.05/DZ(1)
             SWICE(1) = PROPOR*SWICE(1)
             SWLIQ(1) = PROPOR*SWLIQ(1)
             DZ(1)    = 0.05

             CALL COMBO (parameters,DZ(2), SWLIQ(2), SWICE(2), TSNO(2), DRR, &
                  ZWLIQ, ZWICE, TSNO(1))

             ! subdivide a new layer
             IF (MSNO <= 2 .AND. DZ(2) > 0.20) THEN  ! MB: change limit
!             IF (MSNO <= 2 .AND. DZ(2) > 0.10) THEN
                MSNO = 3
                DTDZ = (TSNO(1) - TSNO(2))/((DZ(1)+DZ(2))/2.0)
                DZ(2)    = DZ(2)/2.0
                SWICE(2) = SWICE(2)/2.0
                SWLIQ(2) = SWLIQ(2)/2.0
                DZ(3)    = DZ(2)
                SWICE(3) = SWICE(2)
                SWLIQ(3) = SWLIQ(2)
                TSNO(3) = TSNO(2) - DTDZ*DZ(2)/2.0
                IF (TSNO(3) >= TFRZ) THEN
                   TSNO(3)  = TSNO(2)
                ELSE
                   TSNO(2) = TSNO(2) + DTDZ*DZ(2)/2.0
                ENDIF

             END IF
          END IF
       END IF

       IF (MSNO > 2) THEN
          IF (DZ(2) > 0.2) THEN
             DRR = DZ(2) - 0.2
             PROPOR   = DRR/DZ(2)
             ZWICE    = PROPOR*SWICE(2)
             ZWLIQ    = PROPOR*SWLIQ(2)
             PROPOR   = 0.2/DZ(2)
             SWICE(2) = PROPOR*SWICE(2)
             SWLIQ(2) = PROPOR*SWLIQ(2)
             DZ(2)    = 0.2
             CALL COMBO (parameters,DZ(3), SWLIQ(3), SWICE(3), TSNO(3), DRR, &
                  ZWLIQ, ZWICE, TSNO(2))
          END IF
       END IF

       ISNOW = -MSNO

    DO J = ISNOW+1,0
             DZSNSO(J) = DZ(J-ISNOW)
             SNICE(J) = SWICE(J-ISNOW)
             SNLIQ(J) = SWLIQ(J-ISNOW)
             STC(J)   = TSNO(J-ISNOW)
    END DO


!    DO J = ISNOW+1,NSOIL
!    WRITE(*,'(I5,7F10.3)') J, DZSNSO(J), SNICE(J), SNLIQ(J),STC(J)
!    END DO

  END SUBROUTINE DIVIDE

!== begin combo ====================================================================================

  SUBROUTINE COMBO(parameters,DZ,  WLIQ,  WICE, T, DZ2, WLIQ2, WICE2, T2)
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------s
! input

  type (noahmp_parameters), intent(in) :: parameters
    REAL, INTENT(IN)    :: DZ2   !nodal thickness of 2 elements being combined [m]
    REAL, INTENT(IN)    :: WLIQ2 !liquid water of element 2 [kg/m2]
    REAL, INTENT(IN)    :: WICE2 !ice of element 2 [kg/m2]
    REAL, INTENT(IN)    :: T2    !nodal temperature of element 2 [k]
    REAL, INTENT(INOUT) :: DZ    !nodal thickness of 1 elements being combined [m]
    REAL, INTENT(INOUT) :: WLIQ  !liquid water of element 1
    REAL, INTENT(INOUT) :: WICE  !ice of element 1 [kg/m2]
    REAL, INTENT(INOUT) :: T     !node temperature of element 1 [k]

! local 

    REAL                :: DZC   !total thickness of nodes 1 and 2 (DZC=DZ+DZ2).
    REAL                :: WLIQC !combined liquid water [kg/m2]
    REAL                :: WICEC !combined ice [kg/m2]
    REAL                :: TC    !combined node temperature [k]
    REAL                :: H     !enthalpy of element 1 [J/m2]
    REAL                :: H2    !enthalpy of element 2 [J/m2]
    REAL                :: HC    !temporary

!-----------------------------------------------------------------------

    DZC = DZ+DZ2
    WICEC = (WICE+WICE2)
    WLIQC = (WLIQ+WLIQ2)
    H = (CICE*WICE+CWAT*WLIQ) * (T-TFRZ)+HFUS*WLIQ
    H2= (CICE*WICE2+CWAT*WLIQ2) * (T2-TFRZ)+HFUS*WLIQ2

    HC = H + H2
    IF(HC < 0.0)THEN
       TC = TFRZ + HC/(CICE*WICEC + CWAT*WLIQC)
    ELSE IF (HC.LE.HFUS*WLIQC) THEN
       TC = TFRZ
    ELSE
       TC = TFRZ + (HC - HFUS*WLIQC) / (CICE*WICEC + CWAT*WLIQC)
    END IF

    DZ = DZC
    WICE = WICEC
    WLIQ = WLIQC
    T = TC

  END SUBROUTINE COMBO


!== begin compact ==================================================================================

  SUBROUTINE COMPACT (parameters,NSNOW  ,NSOIL  ,DT     ,STC    ,SNICE  , & !in
                      SNLIQ  ,ZSOIL  ,IMELT  ,FICEOLD,ILOC   , JLOC , & !in
                      ISNOW  ,DZSNSO ,ZSNSO )                    !inout
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
   INTEGER,                         INTENT(IN)    :: ILOC   !grid index
   INTEGER,                         INTENT(IN)    :: JLOC   !grid index
   INTEGER,                         INTENT(IN)    :: NSOIL  !no. of soil layers [ =4]
   INTEGER,                         INTENT(IN)    :: NSNOW  !maximum no. of snow layers [ =3]
   INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT  !melting state index [0-no melt;1-melt]
   REAL,                            INTENT(IN)    :: DT     !time step (sec)
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: STC    !snow layer temperature [k]
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: SNICE  !snow layer ice [mm]
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: SNLIQ  !snow layer liquid water [mm]
   REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !depth of layer-bottom from soil srf
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD!ice fraction at last timestep

! input and output
   INTEGER,                         INTENT(INOUT) :: ISNOW  ! actual no. of snow layers
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO ! snow layer thickness [m]
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  ! depth of snow/soil layer-bottom

! local
   REAL, PARAMETER     :: C2 = 21.e-3   ![m3/kg] ! default 21.e-3
   REAL, PARAMETER     :: C3 = 2.5e-6   ![1/s]  
   REAL, PARAMETER     :: C4 = 0.04     ![1/k]
   REAL, PARAMETER     :: C5 = 2.0      !
   REAL, PARAMETER     :: DM = 100.0    !upper Limit on destructive metamorphism compaction [kg/m3]
   REAL, PARAMETER     :: ETA0 = 0.8e+6 !viscosity coefficient [kg-s/m2] 
                                        !according to Anderson, it is between 0.52e6~1.38e6
   REAL :: BURDEN !pressure of overlying snow [kg/m2]
   REAL :: DDZ1   !rate of settling of snow pack due to destructive metamorphism.
   REAL :: DDZ2   !rate of compaction of snow pack due to overburden.
   REAL :: DDZ3   !rate of compaction of snow pack due to melt [1/s]
   REAL :: DEXPF  !EXPF=exp(-c4*(273.15-STC)).
   REAL :: TD     !STC - TFRZ [K]
   REAL :: PDZDTC !nodal rate of change in fractional-thickness due to compaction [fraction/s]
   REAL :: VOID   !void (1 - SNICE - SNLIQ)
   REAL :: WX     !water mass (ice + liquid) [kg/m2]
   REAL :: BI     !partial density of ice [kg/m3]
   REAL, DIMENSION(-NSNOW+1:0) :: FICE   !fraction of ice at current time step

   INTEGER  :: J

! ----------------------------------------------------------------------
    BURDEN = 0.0
    DO J = ISNOW+1, 0

        WX      = SNICE(J) + SNLIQ(J)
        FICE(J) = SNICE(J) / WX
        VOID    = 1.0 - (SNICE(J)/DENICE + SNLIQ(J)/DENH2O) / DZSNSO(J)

        ! Allow compaction only for non-saturated node and higher ice lens node.
        IF (VOID > 0.001 .AND. SNICE(J) > 0.1) THEN
           BI = SNICE(J) / DZSNSO(J)
           TD = MAX(0.0,TFRZ-STC(J))
           DEXPF = EXP(-C4*TD)

           ! Settling as a result of destructive metamorphism

           DDZ1 = -C3*DEXPF

           IF (BI > DM) DDZ1 = DDZ1*EXP(-46.0E-3*(BI-DM))

           ! Liquid water term

           IF (SNLIQ(J) > 0.01*DZSNSO(J)) DDZ1=DDZ1*C5

           ! Compaction due to overburden

           DDZ2 = -(BURDEN+0.5*WX)*EXP(-0.08*TD-C2*BI)/ETA0 ! 0.5*WX -> self-burden

           ! Compaction occurring during melt

           IF (IMELT(J) == 1) THEN
              DDZ3 = MAX(0.0,(FICEOLD(J) - FICE(J))/MAX(1.E-6,FICEOLD(J)))
              DDZ3 = - DDZ3/DT           ! sometimes too large
           ELSE
              DDZ3 = 0.0
           END IF

           ! Time rate of fractional change in DZ (units of s-1)

           PDZDTC = (DDZ1 + DDZ2 + DDZ3)*DT
           PDZDTC = MAX(-0.5,PDZDTC)

           ! The change in DZ due to compaction

           DZSNSO(J) = DZSNSO(J)*(1.+PDZDTC)
           DZSNSO(J) = max(DZSNSO(J),SNICE(J)/DENICE + SNLIQ(J)/DENH2O)
        END IF

        ! Pressure of overlying snow

        BURDEN = BURDEN + WX

    END DO

  END SUBROUTINE COMPACT

!== begin snowh2o ==================================================================================

  SUBROUTINE SNOWH2O (parameters,NSNOW  ,NSOIL  ,DT     ,QSNFRO ,QSNSUB , & !in 
                      QRAIN  ,ILOC   ,JLOC   ,                 & !in
                      ISNOW  ,DZSNSO ,SNOWH  ,SNEQV  ,SNICE  , & !inout
                      SNLIQ  ,SH2O   ,SICE   ,STC    ,         & !inout
                      QSNBOT ,PONDING1       ,PONDING2)          !out
! ----------------------------------------------------------------------
! Renew the mass of ice lens (SNICE) and liquid (SNLIQ) of the
! surface snow layer resulting from sublimation (frost) / evaporation (dew)
! ----------------------------------------------------------------------
   IMPLICIT NONE
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
   INTEGER,                         INTENT(IN)    :: ILOC   !grid index
   INTEGER,                         INTENT(IN)    :: JLOC   !grid index
   INTEGER,                         INTENT(IN)    :: NSNOW  !maximum no. of snow layers[=3]
   INTEGER,                         INTENT(IN)    :: NSOIL  !No. of soil layers[=4]
   REAL,                            INTENT(IN)    :: DT     !time step
   REAL,                            INTENT(IN)    :: QSNFRO !snow surface frost rate[mm/s]
   REAL,                            INTENT(IN)    :: QSNSUB !snow surface sublimation rate[mm/s]
   REAL,                            INTENT(IN)    :: QRAIN  !snow surface rain rate[mm/s]

! output

   REAL,                            INTENT(OUT)   :: QSNBOT !melting water out of snow bottom [mm/s]

! input and output

   INTEGER,                         INTENT(INOUT) :: ISNOW  !actual no. of snow layers
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO ! snow layer depth [m]
   REAL,                            INTENT(INOUT) :: SNOWH  !snow height [m]
   REAL,                            INTENT(INOUT) :: SNEQV  !snow water eqv. [mm]
   REAL, DIMENSION(-NSNOW+1:0),     INTENT(INOUT) :: SNICE  !snow layer ice [mm]
   REAL, DIMENSION(-NSNOW+1:0),     INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]
   REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !soil liquid moisture (m3/m3)
   REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE   !soil ice moisture (m3/m3)
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow layer temperature [k]

! local variables:

   INTEGER                     :: J         !do loop/array indices
   REAL                        :: QIN       !water flow into the element (mm/s)
   REAL                        :: QOUT      !water flow out of the element (mm/s)
   REAL                        :: WGDIF     !ice mass after minus sublimation
   REAL, DIMENSION(-NSNOW+1:0) :: VOL_LIQ   !partial volume of liquid water in layer
   REAL, DIMENSION(-NSNOW+1:0) :: VOL_ICE   !partial volume of ice lens in layer
   REAL, DIMENSION(-NSNOW+1:0) :: EPORE     !effective porosity = porosity - VOL_ICE
   REAL :: PROPOR, TEMP
   REAL :: PONDING1, PONDING2
   REAL, PARAMETER :: max_liq_mass_fraction = 0.4
! ----------------------------------------------------------------------

!for the case when SNEQV becomes '0' after 'COMBINE'

   IF(SNEQV == 0.0) THEN
      SICE(1) =  SICE(1) + (QSNFRO-QSNSUB)*DT/(DZSNSO(1)*1000.0)  ! Barlage: SH2O->SICE v3.6
      IF(SICE(1) < 0.0) THEN
         SH2O(1) = SH2O(1) + SICE(1)
         SICE(1) = 0.0
      END IF
   END IF

! for shallow snow without a layer
! snow surface sublimation may be larger than existing snow mass. To conserve water,
! excessive sublimation is used to reduce soil water. Smaller time steps would tend 
! to aviod this problem.

   IF(ISNOW == 0 .and. SNEQV > 0.0) THEN
      TEMP   = SNEQV
      SNEQV  = SNEQV - QSNSUB*DT + QSNFRO*DT
      PROPOR = SNEQV/TEMP
      SNOWH  = MAX(0.0,PROPOR * SNOWH)
      SNOWH  = MIN(MAX(SNOWH,SNEQV/500.0),SNEQV/50.0)  ! limit adjustment to a reasonable density

      IF(SNEQV < 0.0) THEN
         SICE(1) = SICE(1) + SNEQV/(DZSNSO(1)*1000.0)
         SNEQV   = 0.0
         SNOWH   = 0.0
      END IF
      IF(SICE(1) < 0.0) THEN
         SH2O(1) = SH2O(1) + SICE(1)
         SICE(1) = 0.0
      END IF
   END IF

   IF(SNOWH <= 1.E-8 .OR. SNEQV <= 1.E-6) THEN
     SNOWH = 0.0
     SNEQV = 0.0
   END IF

! for deep snow

   IF ( ISNOW < 0 ) THEN !KWM added this IF statement to prevent out-of-bounds array references

      WGDIF = SNICE(ISNOW+1) - QSNSUB*DT + QSNFRO*DT
      SNICE(ISNOW+1) = WGDIF
      IF (WGDIF < 1.e-6 .and. ISNOW <0) THEN
         CALL  COMBINE (parameters,NSNOW  ,NSOIL  ,ILOC, JLOC   , & !in
              ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & !inout
              DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,         & !inout
              PONDING1, PONDING2 )                       !out
      ENDIF
      !KWM:  Subroutine COMBINE can change ISNOW to make it 0 again?
      IF ( ISNOW < 0 ) THEN !KWM added this IF statement to prevent out-of-bounds array references
         SNLIQ(ISNOW+1) = SNLIQ(ISNOW+1) + QRAIN * DT
         SNLIQ(ISNOW+1) = MAX(0.0, SNLIQ(ISNOW+1))
      ENDIF

   ENDIF !KWM  -- Can the ENDIF be moved toward the end of the subroutine (Just set QSNBOT=0)?

! Porosity and partial volume

   DO J = ISNOW+1, 0
     VOL_ICE(J)      = MIN(1.0, SNICE(J)/(DZSNSO(J)*DENICE))
     EPORE(J)        = 1.0 - VOL_ICE(J)
   END DO

   QIN = 0.0
   QOUT = 0.0

   DO J = ISNOW+1, 0
     SNLIQ(J) = SNLIQ(J) + QIN
     VOL_LIQ(J) = SNLIQ(J)/(DZSNSO(J)*DENH2O)
     QOUT = MAX(0.0,(VOL_LIQ(J)-parameters%SSI*EPORE(J))*DZSNSO(J))
     IF(J == 0) THEN
       QOUT = MAX((VOL_LIQ(J)- EPORE(J))*DZSNSO(J) , parameters%SNOW_RET_FAC*DT*QOUT)
     END IF
     QOUT = QOUT*DENH2O
     SNLIQ(J) = SNLIQ(J) - QOUT
     IF((SNLIQ(J)/(SNICE(J)+SNLIQ(J))) > max_liq_mass_fraction) THEN
       QOUT = QOUT + (SNLIQ(J) - max_liq_mass_fraction/(1.0 - max_liq_mass_fraction)*SNICE(J))
       SNLIQ(J) = max_liq_mass_fraction/(1.0 - max_liq_mass_fraction)*SNICE(J)
     ENDIF
     QIN = QOUT
   END DO

   DO J = ISNOW+1, 0
     DZSNSO(J) = MAX(DZSNSO(J),SNLIQ(J)/DENH2O + SNICE(J)/DENICE)
   END DO

! Liquid water from snow bottom to soil

   QSNBOT = QOUT / DT           ! mm/s

  END SUBROUTINE SNOWH2O

!== begin soilwater ================================================================================

  SUBROUTINE SOILWATER (parameters,NSOIL,NSNOW,DT    ,ZSOIL  ,DZSNSO , & !in
                        QINSUR ,QSEVA  ,ETRANI ,SICE   ,ILOC   , JLOC, & !in
                        TDFRACMP, DX   ,                               & !in
                        SH2O   ,SMC    ,ZWT    ,VEGTYP ,               & !inout
                        SMCWTD, DEEPRECH,                              & !inout
                        RUNSRF ,QDRAIN ,RUNSUB ,WCND   ,FCRMAX, QTLDRN & !out
#ifdef WRF_HYDRO
                        ,WATBLED                                       & !in for tile drainage
#endif
                        )
! ----------------------------------------------------------------------
! calculate surface runoff and soil moisture.
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                     INTENT(IN) :: ILOC   !grid index
  INTEGER,                     INTENT(IN) :: JLOC   !grid index
  INTEGER,                     INTENT(IN) :: NSOIL  !no. of soil layers
  INTEGER,                     INTENT(IN) :: NSNOW  !maximum no. of snow layers
  REAL,                        INTENT(IN) :: DT     !time step (sec)
  REAL, INTENT(IN)                        :: QINSUR !water input on soil surface [mm/s]
  REAL, INTENT(IN)                        :: QSEVA  !evap from soil surface [mm/s]
  REAL, DIMENSION(1:NSOIL),    INTENT(IN) :: ZSOIL  !depth of soil layer-bottom [m]
  REAL, DIMENSION(1:NSOIL),    INTENT(IN) :: ETRANI !evapotranspiration from soil layers [mm/s]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer depth [m]
  REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: SICE   !soil ice content [m3/m3]
  REAL,                     INTENT(IN)    :: DX
  REAL,                     INTENT(IN)    :: TDFRACMP! tile drainage map(fraction)

  INTEGER,                     INTENT(IN) :: VEGTYP

! input & output
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SH2O   !soil liquid water content [m3/m3]
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SMC    !total soil water content [m3/m3]
  REAL, INTENT(INOUT)                     :: ZWT    !water table depth [m]
  REAL,                     INTENT(INOUT) :: SMCWTD !soil moisture between bottom of the soil and the water table [m3/m3]
  REAL                    , INTENT(INOUT) :: DEEPRECH
  REAL                    , INTENT(INOUT) :: QTLDRN ! tile drainage (mm/s)
#ifdef WRF_HYDRO
  REAL                    , INTENT(INOUT) :: WATBLED!in for tile drainage
#endif

! output
  REAL, INTENT(OUT)                       :: QDRAIN !soil-bottom free drainage [mm/s] 
  REAL, INTENT(OUT)                       :: RUNSRF !surface runoff [mm/s] 
  REAL, INTENT(OUT)                       :: RUNSUB !subsurface runoff [mm/s] 
  REAL, INTENT(OUT)                       :: FCRMAX !maximum of FCR (-)
  REAL, DIMENSION(1:NSOIL), INTENT(OUT)   :: WCND   !hydraulic conductivity (m/s)

! local
  INTEGER                                 :: K,IZ   !do-loop index
  INTEGER                                 :: ITER   !iteration index
  REAl                                    :: DTFINE !fine time step (s)
  REAL, DIMENSION(1:NSOIL)                :: RHSTT  !right-hand side term of the matrix
  REAL, DIMENSION(1:NSOIL)                :: AI     !left-hand side term
  REAL, DIMENSION(1:NSOIL)                :: BI     !left-hand side term
  REAL, DIMENSION(1:NSOIL)                :: CI     !left-hand side term

  REAL                                    :: FFF    !runoff decay factor (m-1)
  REAL                                    :: RSBMX  !baseflow coefficient [mm/s]
  REAL                                    :: PDDUM  !infiltration rate at surface (m/s)
  REAL                                    :: FICE   !ice fraction in frozen soil
  REAL                                    :: WPLUS  !saturation excess of the total soil [m]
  REAL                                    :: RSAT   !accumulation of WPLUS (saturation excess) [m]
  REAL                                    :: SICEMAX!maximum soil ice content (m3/m3)
  REAL                                    :: SH2OMIN!minimum soil liquid water content (m3/m3)
  REAL                                    :: WTSUB  !sum of WCND(K)*DZSNSO(K)
  REAL                                    :: MH2O   !water mass removal (mm)
  REAL                                    :: FSAT   !fractional saturated area (-)
  REAL, DIMENSION(1:NSOIL)                :: MLIQ   !
  REAL                                    :: XS     !
  REAL                                    :: WATMIN !
  REAL                                    :: QDRAIN_SAVE !
  REAL                                    :: RUNSRF_SAVE !
  REAL                                    :: EPORE  !effective porosity [m3/m3]
  REAL, DIMENSION(1:NSOIL)                :: FCR    !impermeable fraction due to frozen soil
  INTEGER                                 :: NITER  !iteration times soil moisture (-)
  REAL                                    :: SMCTOT !2-m averaged soil moisture (m3/m3)
  REAL                                    :: DZTOT  !2-m soil depth (m)
  REAL                                    :: FACC   !accumulated infiltration rate (m/s)
  REAL, PARAMETER :: A = 4.0
! ----------------------------------------------------------------------
    RUNSRF = 0.0
    PDDUM  = 0.0
    RSAT   = 0.0

! for the case when snowmelt water is too large

    DO K = 1,NSOIL
       EPORE   = MAX ( 1.E-4 , ( parameters%SMCMAX(K) - SICE(K) ) )
       RSAT    = RSAT + MAX(0.0,SH2O(K)-EPORE)*DZSNSO(K)
       SH2O(K) = MIN(EPORE,SH2O(K))
    END DO

!impermeable fraction due to frozen soil

    DO K = 1,NSOIL
       FICE    = MIN(1.0,SICE(K)/parameters%SMCMAX(K))
       FCR(K)  = MAX(0.0,EXP(-A*(1.-FICE))- EXP(-A)) /  &
                        (1.0              - EXP(-A))
    END DO

! maximum soil ice content and minimum liquid water of all layers

    SICEMAX = 0.0
    FCRMAX  = 0.0
    SH2OMIN = parameters%SMCMAX(1)
    DO K = 1,NSOIL
       IF (SICE(K) > SICEMAX) SICEMAX = SICE(K)
       IF (FCR(K)  > FCRMAX)  FCRMAX  = FCR(K)
       IF (SH2O(K) < SH2OMIN) SH2OMIN = SH2O(K)
    END DO

!subsurface runoff for runoff scheme option 2

    IF(OPT_RUN == 2) THEN
        FFF   = 2.0
        RSBMX = 4.0
        CALL ZWTEQ (parameters,NSOIL  ,NSNOW  ,ZSOIL  ,DZSNSO ,SH2O   ,ZWT)
        RUNSUB = (1.0-FCRMAX) * RSBMX * EXP(-parameters%TIMEAN) * EXP(-FFF*ZWT)   ! mm/s
    END IF

!surface runoff and infiltration rate using different schemes

!jref impermable surface at urban
    IF ( parameters%urban_flag ) FCR(1)= 0.95


    IF(OPT_RUN == 1) THEN
       FFF = 6.0
       FSAT   = parameters%FSATMX*EXP(-0.5*FFF*(ZWT-2.0))
       IF(QINSUR > 0.0) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s 
       END IF
    END IF

    IF(OPT_RUN == 5) THEN
       FFF = 6.0
       FSAT   = parameters%FSATMX*EXP(-0.5*FFF*MAX(-2.0-ZWT,0.0))
       IF(QINSUR > 0.0) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s
       END IF
    END IF

    IF(OPT_RUN == 2) THEN
       FFF   = 2.0
       FSAT   = parameters%FSATMX*EXP(-0.5*FFF*ZWT)
       IF(QINSUR > 0.0) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s 
       END IF
    END IF

    IF(OPT_RUN == 3) THEN
       CALL INFIL (parameters,NSOIL  ,DT     ,ZSOIL  ,SH2O   ,SICE   , & !in
                   SICEMAX,QINSUR ,                         & !in
                   PDDUM  ,RUNSRF )                           !out
    END IF

    IF(OPT_RUN == 4) THEN
       SMCTOT = 0.0
       DZTOT  = 0.0
       DO K = 1,NSOIL
          DZTOT   = DZTOT  + DZSNSO(K)
          SMCTOT  = SMCTOT + SMC(K)/parameters%SMCMAX(K)*DZSNSO(K)
          IF(DZTOT >= 2.0) EXIT
       END DO
       SMCTOT = SMCTOT/DZTOT
       FSAT   = MAX(0.01,SMCTOT) ** 4.0        !BATS

       IF(QINSUR > 0.0) THEN
         RUNSRF = QINSUR * ((1.0-FCR(1))*FSAT+FCR(1))
         PDDUM  = QINSUR - RUNSRF                       ! m/s
       END IF
    END IF

    IF (OPT_RUN == 6) THEN
       CALL COMPUTE_VIC_SURFRUNOFF(parameters,DT,NSOIL,SMC,ZSOIL,QINSUR,FSAT,RUNSRF,PDDUM)
    END IF

    IF (OPT_RUN == 7) THEN
       CALL COMPUTE_XAJ_SURFRUNOFF(parameters,DT,FCR,NSOIL,SMC,ZSOIL,QINSUR,RUNSRF,PDDUM)
    END IF

    IF(OPT_RUN == 8)THEN
       FACC = 1E-06
       CALL DYNAMIC_VIC(parameters,DT,SMC,SH2O,SICE,SICEMAX,NSOIL,ZSOIL,QINSUR,FACC,PDDUM,RUNSRF)
    END IF

! determine iteration times and finer time step

    NITER = 1
!    IF(OPT_INF == 1) THEN    !OPT_INF =2 may cause water imbalance
       NITER = 3
       IF (PDDUM*DT>DZSNSO(1)*parameters%SMCMAX(1) ) THEN
          NITER = NITER*2
       END IF
!    END IF                 

    DTFINE  = DT / NITER
! solve soil moisture
    FACC        = 1E-06
    QDRAIN_SAVE = 0.0
    RUNSRF_SAVE = 0.0

    DO ITER = 1, NITER
       IF(QINSUR > 0.0 .and. OPT_RUN == 3) THEN
          CALL INFIL (parameters,NSOIL  ,DTFINE     ,ZSOIL  ,SH2O   ,SICE   , & !in
                      SICEMAX,QINSUR ,                         & !in
                      PDDUM  ,RUNSRF )                           !out
       END IF

       IF(QINSUR > 0.0 .and. OPT_RUN == 6) THEN
          CALL COMPUTE_VIC_SURFRUNOFF(parameters,DTFINE,NSOIL,SMC,ZSOIL,QINSUR,& !in
                                      FSAT,RUNSRF,PDDUM)                         !out
       END IF

       IF (QINSUR > 0.0 .AND. OPT_RUN == 7) THEN
          CALL COMPUTE_XAJ_SURFRUNOFF(parameters,DTFINE,FCR,NSOIL,SMC,ZSOIL,QINSUR,& ! in
                                      RUNSRF,PDDUM)                                  ! out
       END IF

       IF(QINSUR > 0.0 .and. OPT_RUN == 8) THEN
          CALL DYNAMIC_VIC(parameters,DTFINE,SMC,SH2O,SICE,SICEMAX,NSOIL,&
                           ZSOIL,QINSUR,FACC,PDDUM,RUNSRF)
       END IF

       CALL SRT   (parameters,NSOIL  ,ZSOIL  ,DTFINE ,PDDUM  ,ETRANI , & !in
                   QSEVA  ,SH2O   ,SMC    ,ZWT    ,FCR    , & !in
                   SICEMAX,FCRMAX ,ILOC   ,JLOC   ,SMCWTD ,         & !in
                   RHSTT  ,AI     ,BI     ,CI     ,QDRAIN , & !out
                   WCND   )                                   !out

       CALL SSTEP (parameters,NSOIL  ,NSNOW  ,DTFINE ,ZSOIL  ,DZSNSO , & !in
                   SICE   ,ILOC   ,JLOC   ,ZWT            ,                 & !in
                   SH2O   ,SMC    ,AI     ,BI     ,CI     , & !inout
                   RHSTT  ,SMCWTD ,QDRAIN ,DEEPRECH,                                 & !inout
                   WPLUS)                                     !out
       RSAT =  RSAT + WPLUS
       QDRAIN_SAVE = QDRAIN_SAVE + QDRAIN
       RUNSRF_SAVE = RUNSRF_SAVE + RUNSRF

    END DO

    QDRAIN = QDRAIN_SAVE/NITER
    RUNSRF = RUNSRF_SAVE/NITER

    RUNSRF = RUNSRF * 1000.0 + RSAT * 1000.0/DT  ! m/s -> mm/s
    QDRAIN = QDRAIN * 1000.0


! Calling tile drainage ! pvk
    IF (OPT_TDRN == 1 .AND. TDFRACMP .GT. 0.3 .AND. OPT_RUN == 3) THEN
        print*, "simple tile drain scheme is on"
        CALL TILE_DRAIN (parameters,NSOIL,SH2O,SMC,SICE,ZSOIL,QTLDRN,DT)
    ELSE IF (OPT_TDRN == 2 .AND. TDFRACMP .GT. 0.1 .AND. OPT_RUN == 3) THEN
        print*, "Hooghoudt tile drain scheme is on"
        CALL TILE_HOOGHOUDT (parameters,WCND,NSOIL,NSNOW,SH2O,SMC,SICE,&
                             ZSOIL,DZSNSO,DT,DX,QTLDRN,ZWT             &
#ifdef WRF_HYDRO
                             ,WATBLED                                  &
#endif
                            )
    END IF

!WRF_HYDRO_DJG...
!yw    INFXSRT = RUNSRF * DT   !mm/s -> mm

! removal of soil water due to groundwater flow (option 2)

    IF(OPT_RUN == 2) THEN
         WTSUB = 0.0
         DO K = 1, NSOIL
           WTSUB = WTSUB + WCND(K)*DZSNSO(K)
         END DO
         DO K = 1, NSOIL
           MH2O    = RUNSUB*DT*(WCND(K)*DZSNSO(K))/WTSUB       ! mm
           SH2O(K) = SH2O(K) - MH2O/(DZSNSO(K)*1000.0)
         END DO
    END IF

! Limit MLIQ to be greater than or equal to watmin.
! Get water needed to bring MLIQ equal WATMIN from lower layer.
   IF(OPT_RUN /= 1) THEN
      DO IZ = 1, NSOIL
         MLIQ(IZ) = SH2O(IZ)*DZSNSO(IZ)*1000.0
      END DO

      WATMIN = 0.01           ! mm
      DO IZ = 1, NSOIL-1
          IF (MLIQ(IZ) .LT. 0.0) THEN
             XS = WATMIN-MLIQ(IZ)
          ELSE
             XS = 0.0
          END IF
          MLIQ(IZ  ) = MLIQ(IZ  ) + XS
          MLIQ(IZ+1) = MLIQ(IZ+1) - XS
      END DO

        IZ = NSOIL
        IF (MLIQ(IZ) .LT. WATMIN) THEN
           XS = WATMIN-MLIQ(IZ)
        ELSE
           XS = 0.0
        END IF
        MLIQ(IZ) = MLIQ(IZ) + XS
        RUNSUB   = RUNSUB - XS/DT
        IF(OPT_RUN == 5)DEEPRECH = DEEPRECH - XS*1.E-3

      DO IZ = 1, NSOIL
        SH2O(IZ)     = MLIQ(IZ) / (DZSNSO(IZ)*1000.0)
      END DO
   END IF
  END SUBROUTINE SOILWATER

!== begin zwteq ====================================================================================

  SUBROUTINE ZWTEQ (parameters,NSOIL  ,NSNOW  ,ZSOIL  ,DZSNSO ,SH2O   ,ZWT)
! ----------------------------------------------------------------------
! calculate equilibrium water table depth (Niu et al., 2005)
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                         INTENT(IN) :: NSOIL  !no. of soil layers
  INTEGER,                         INTENT(IN) :: NSNOW  !maximum no. of snow layers
  REAL, DIMENSION(1:NSOIL),        INTENT(IN) :: ZSOIL  !depth of soil layer-bottom [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer depth [m]
  REAL, DIMENSION(1:NSOIL),        INTENT(IN) :: SH2O   !soil liquid water content [m3/m3]

! output

  REAL,                           INTENT(OUT) :: ZWT    !water table depth [m]

! locals

  INTEGER :: K                      !do-loop index
  INTEGER, PARAMETER :: NFINE = 100 !no. of fine soil layers of 6m soil
  REAL    :: WD1                    !water deficit from coarse (4-L) soil moisture profile
  REAL    :: WD2                    !water deficit from fine (100-L) soil moisture profile
  REAL    :: DZFINE                 !layer thickness of the 100-L soil layers to 6.0 m
  REAL    :: TEMP                   !temporary variable
  REAL, DIMENSION(1:NFINE) :: ZFINE !layer-bottom depth of the 100-L soil layers to 6.0 m
! ----------------------------------------------------------------------

   WD1 = 0.0
   DO K = 1,NSOIL
     WD1 = WD1 + (parameters%SMCMAX(1)-SH2O(K)) * DZSNSO(K) ! [m]
   ENDDO

   DZFINE = 3.0 * (-ZSOIL(NSOIL)) / NFINE
   do K =1,NFINE
      ZFINE(K) = FLOAT(K) * DZFINE
   ENDDO

   ZWT = -3.0*ZSOIL(NSOIL) - 0.001   ! initial value [m]

   WD2 = 0.0
   DO K = 1,NFINE
     TEMP  = 1.0 + (ZWT-ZFINE(K))/parameters%PSISAT(1)
     WD2   = WD2 + parameters%SMCMAX(1)*(1.0-TEMP**(-1.0/parameters%BEXP(1)))*DZFINE
     IF(ABS(WD2-WD1).LE.0.01) THEN
        ZWT = ZFINE(K)
        EXIT
     ENDIF
   ENDDO

  END SUBROUTINE ZWTEQ

!== begin infil ====================================================================================

  SUBROUTINE INFIL (parameters,NSOIL  ,DT     ,ZSOIL  ,SH2O   ,SICE   , & !in
                    SICEMAX,QINSUR ,                         & !in
                    PDDUM  ,RUNSRF )                           !out
! --------------------------------------------------------------------------------
! compute inflitration rate at soil surface and surface runoff
! --------------------------------------------------------------------------------
    IMPLICIT NONE
! --------------------------------------------------------------------------------
! inputs
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                  INTENT(IN) :: NSOIL  !no. of soil layers
  REAL,                     INTENT(IN) :: DT     !time step (sec)
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: ZSOIL  !depth of soil layer-bottom [m]
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SH2O   !soil liquid water content [m3/m3]
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SICE   !soil ice content [m3/m3]
  REAL,                     INTENT(IN) :: QINSUR !water input on soil surface [mm/s]
  REAL,                     INTENT(IN) :: SICEMAX!maximum soil ice content (m3/m3)

! outputs
  REAL,                    INTENT(OUT) :: RUNSRF !surface runoff [mm/s] 
  REAL,                    INTENT(OUT) :: PDDUM  !infiltration rate at surface

! locals
  INTEGER :: IALP1, J, JJ,  K
  REAL                     :: VAL
  REAL                     :: DDT
  REAL                     :: PX
  REAL                     :: DT1, DD, DICE
  REAL                     :: FCR
  REAL                     :: SUM
  REAL                     :: ACRT
  REAL                     :: WDF
  REAL                     :: WCND
  REAL                     :: SMCAV
  REAL                     :: INFMAX
  REAL, DIMENSION(1:NSOIL) :: DMAX
  INTEGER, PARAMETER       :: CVFRZ = 3
! --------------------------------------------------------------------------------

    IF (QINSUR >  0.0) THEN
       DT1 = DT /86400.0
       SMCAV = parameters%SMCMAX(1) - parameters%SMCWLT(1)

! maximum infiltration rate

       DMAX(1)= -ZSOIL(1) * SMCAV
       DICE   = -ZSOIL(1) * SICE(1)
       DMAX(1)= DMAX(1)* (1.0-(SH2O(1) + SICE(1) - parameters%SMCWLT(1))/SMCAV)

       DD = DMAX(1)

       DO K = 2,NSOIL
          DICE    = DICE + (ZSOIL(K-1) - ZSOIL(K) ) * SICE(K)
          DMAX(K) = (ZSOIL(K-1) - ZSOIL(K)) * SMCAV
          DMAX(K) = DMAX(K) * (1.0-(SH2O(K) + SICE(K) - parameters%SMCWLT(K))/SMCAV)
          DD      = DD + DMAX(K)
       END DO

       VAL = (1.0 - EXP ( - parameters%KDT * DT1))
       DDT = DD * VAL
       PX  = MAX(0.0,QINSUR * DT)
       INFMAX = (PX * (DDT / (PX + DDT)))/ DT

! impermeable fraction due to frozen soil

       FCR = 1.0
       IF (DICE >  1.E-2) THEN
          ACRT = CVFRZ * parameters%FRZX / DICE
          SUM = 1.0
          IALP1 = CVFRZ - 1
          DO J = 1,IALP1
             K = 1
             DO JJ = J +1,IALP1
                K = K * JJ
             END DO
             SUM = SUM + (ACRT ** (CVFRZ - J)) / FLOAT(K)
          END DO
          FCR = 1.0 - EXP (-ACRT) * SUM
       END IF

! correction of infiltration limitation

       INFMAX = INFMAX * FCR

! jref for urban areas
!       IF ( parameters%urban_flag ) INFMAX == INFMAX * 0.05

       CALL WDFCND2 (parameters,WDF,WCND,SH2O(1),SICEMAX,1)
       INFMAX = MAX (INFMAX,WCND)
       INFMAX = MIN (INFMAX,PX)

       RUNSRF= MAX(0.0, QINSUR - INFMAX)
       PDDUM = QINSUR - RUNSRF

    END IF

  END SUBROUTINE INFIL

!== begin srt ======================================================================================

  SUBROUTINE SRT (parameters,NSOIL  ,ZSOIL  ,DT     ,PDDUM  ,ETRANI , & !in
                  QSEVA  ,SH2O   ,SMC    ,ZWT    ,FCR    , & !in
                  SICEMAX,FCRMAX ,ILOC   ,JLOC   ,SMCWTD ,         & !in
                  RHSTT  ,AI     ,BI     ,CI     ,QDRAIN , & !out
                  WCND   )                                   !out
! ----------------------------------------------------------------------
! calculate the right hand side of the time tendency term of the soil
! water diffusion equation.  also to compute ( prepare ) the matrix
! coefficients for the tri-diagonal matrix of the implicit time scheme.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
!input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                  INTENT(IN)  :: ILOC   !grid index
    INTEGER,                  INTENT(IN)  :: JLOC   !grid index
    INTEGER,                  INTENT(IN)  :: NSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: ZSOIL
    REAL,                     INTENT(IN)  :: DT
    REAL,                     INTENT(IN)  :: PDDUM
    REAL,                     INTENT(IN)  :: QSEVA
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: ETRANI
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SH2O
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SMC
    REAL,                     INTENT(IN)  :: ZWT    ! water table depth [m]
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: FCR
    REAL, INTENT(IN)                      :: FCRMAX !maximum of FCR (-)
    REAL,                     INTENT(IN)  :: SICEMAX!maximum soil ice content (m3/m3)
    REAL,                     INTENT(IN)  :: SMCWTD !soil moisture between bottom of the soil and the water table

! output

    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: RHSTT
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: AI
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: BI
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: CI
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: WCND    !hydraulic conductivity (m/s)
    REAL,                     INTENT(OUT) :: QDRAIN  !bottom drainage (m/s)

! local
    INTEGER                               :: K
    REAL, DIMENSION(1:NSOIL)              :: DDZ
    REAL, DIMENSION(1:NSOIL)              :: DENOM
    REAL, DIMENSION(1:NSOIL)              :: DSMDZ
    REAL, DIMENSION(1:NSOIL)              :: WFLUX
    REAL, DIMENSION(1:NSOIL)              :: WDF
    REAL, DIMENSION(1:NSOIL)              :: SMX
    REAL                                  :: TEMP1
    REAL                                  :: SMXWTD !soil moisture between bottom of the soil and water table
    REAL                                  :: SMXBOT  !soil moisture below bottom to calculate flux

! Niu and Yang (2006), J. of Hydrometeorology
! ----------------------------------------------------------------------

    IF(OPT_INF == 1) THEN
      DO K = 1, NSOIL
        CALL WDFCND1 (parameters,WDF(K),WCND(K),SMC(K),FCR(K),K)
        SMX(K) = SMC(K)
      END DO
        IF(OPT_RUN == 5)SMXWTD=SMCWTD
    END IF

    IF(OPT_INF == 2) THEN
      DO K = 1, NSOIL
        CALL WDFCND2 (parameters,WDF(K),WCND(K),SH2O(K),SICEMAX,K)
        SMX(K) = SH2O(K)
      END DO
          IF(OPT_RUN == 5)SMXWTD=SMCWTD*SH2O(NSOIL)/SMC(NSOIL)  !same liquid fraction as in the bottom layer
    END IF

    DO K = 1, NSOIL
       IF(K == 1) THEN
          DENOM(K) = - ZSOIL (K)
          TEMP1    = - ZSOIL (K+1)
          DDZ(K)   = 2.0 / TEMP1
          DSMDZ(K) = 2.0 * (SMX(K) - SMX(K+1)) / TEMP1
          WFLUX(K) = WDF(K) * DSMDZ(K) + WCND(K) - PDDUM + ETRANI(K) + QSEVA
       ELSE IF (K < NSOIL) THEN
          DENOM(k) = (ZSOIL(K-1) - ZSOIL(K))
          TEMP1    = (ZSOIL(K-1) - ZSOIL(K+1))
          DDZ(K)   = 2.0 / TEMP1
          DSMDZ(K) = 2.0 * (SMX(K) - SMX(K+1)) / TEMP1
          WFLUX(K) = WDF(K  ) * DSMDZ(K  ) + WCND(K  )         &
                   - WDF(K-1) * DSMDZ(K-1) - WCND(K-1) + ETRANI(K)
       ELSE
          DENOM(K) = (ZSOIL(K-1) - ZSOIL(K))
          IF(OPT_RUN == 1 .or. OPT_RUN == 2) THEN
             QDRAIN   = 0.0
          END IF
          IF(OPT_RUN == 3 .OR. OPT_RUN == 6 .OR. OPT_RUN == 7 .OR. OPT_RUN == 8) THEN
             QDRAIN   = parameters%SLOPE*WCND(K)
          END IF
          IF(OPT_RUN == 4) THEN
             QDRAIN   = (1.0-FCRMAX)*WCND(K)
          END IF
          IF(OPT_RUN == 5) THEN   !gmm new m-m&f water table dynamics formulation
             TEMP1    = 2.0 * DENOM(K)
             IF(ZWT < ZSOIL(NSOIL)-DENOM(NSOIL))THEN
!gmm interpolate from below, midway to the water table, to the middle of the auxiliary layer below the soil bottom
                SMXBOT = SMX(K) - (SMX(K)-SMXWTD) *  DENOM(K) * 2.0/ (DENOM(K) + ZSOIL(K) - ZWT)
             ELSE
                SMXBOT = SMXWTD
             ENDIF
             DSMDZ(K) = 2.0 * (SMX(K) - SMXBOT) / TEMP1
             QDRAIN   = WDF(K  ) * DSMDZ(K  ) + WCND(K  )
          END IF

          WFLUX(K) = -(WDF(K-1)*DSMDZ(K-1))-WCND(K-1)+ETRANI(K) + QDRAIN
       END IF
    END DO

    DO K = 1, NSOIL
       IF(K == 1) THEN
          AI(K)    =   0.0
          BI(K)    =   WDF(K  ) * DDZ(K  ) / DENOM(K)
          CI(K)    = - BI (K)
       ELSE IF (K < NSOIL) THEN
          AI(K)    = - WDF(K-1) * DDZ(K-1) / DENOM(K)
          CI(K)    = - WDF(K  ) * DDZ(K  ) / DENOM(K)
          BI(K)    = - ( AI (K) + CI (K) )
       ELSE
          AI(K)    = - WDF(K-1) * DDZ(K-1) / DENOM(K)
          CI(K)    = 0.0
          BI(K)    = - ( AI (K) + CI (K) )
       END IF
          RHSTT(K) = WFLUX(K) / (-DENOM(K))
    END DO

! ----------------------------------------------------------------------
  END SUBROUTINE SRT

!== begin sstep ====================================================================================

  SUBROUTINE SSTEP (parameters,NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & !in
                    SICE   ,ILOC   ,JLOC   ,ZWT            ,                 & !in
                    SH2O   ,SMC    ,AI     ,BI     ,CI     , & !inout
                    RHSTT  ,SMCWTD ,QDRAIN ,DEEPRECH,                                 & !inout
                    WPLUS  )                                   !out

! ----------------------------------------------------------------------
! calculate/update soil moisture content values 
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
!input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                         INTENT(IN) :: ILOC   !grid index
    INTEGER,                         INTENT(IN) :: JLOC   !grid index
    INTEGER,                         INTENT(IN) :: NSOIL  !
    INTEGER,                         INTENT(IN) :: NSNOW  !
    REAL, INTENT(IN)                            :: DT
    REAL, INTENT(IN)                            :: ZWT
    REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL
    REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SICE
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO ! snow/soil layer thickness [m]

!input and output
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SH2O
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SMC
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: AI
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: BI
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: CI
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: RHSTT
    REAL                    , INTENT(INOUT) :: SMCWTD
    REAL                    , INTENT(INOUT) :: QDRAIN
    REAL                    , INTENT(INOUT) :: DEEPRECH

!output
    REAL, INTENT(OUT)                       :: WPLUS     !saturation excess water (m)

!local
    INTEGER                                 :: K
    REAL, DIMENSION(1:NSOIL)                :: RHSTTIN
    REAL, DIMENSION(1:NSOIL)                :: CIIN
    REAL                                    :: STOT
    REAL                                    :: EPORE
    REAL                                    :: WMINUS
! ----------------------------------------------------------------------
    WPLUS = 0.0

    DO K = 1,NSOIL
       RHSTT (K) =   RHSTT(K) * DT
       AI (K)    =      AI(K) * DT
       BI (K)    = 1.0 + BI(K) * DT
       CI (K)    =      CI(K) * DT
    END DO

! copy values for input variables before calling rosr12

    DO K = 1,NSOIL
       RHSTTIN(k) = RHSTT(K)
       CIIN(k)    = CI(K)
    END DO

! call ROSR12 to solve the tri-diagonal matrix

    CALL ROSR12 (CI,AI,BI,CIIN,RHSTTIN,RHSTT,1,NSOIL,0)

    DO K = 1,NSOIL
        SH2O(K) = SH2O(K) + CI(K)
    ENDDO

!  excessive water above saturation in a layer is moved to
!  its unsaturated layer like in a bucket

!gmmwith opt_run=5 there is soil moisture below nsoil, to the water table
  IF(OPT_RUN == 5) THEN

!update smcwtd

     IF(ZWT < ZSOIL(NSOIL)-DZSNSO(NSOIL))THEN
!accumulate qdrain to update deep water table and soil moisture later
        DEEPRECH =  DEEPRECH + DT * QDRAIN
     ELSE
        SMCWTD = SMCWTD + DT * QDRAIN  / DZSNSO(NSOIL)
        WPLUS        = MAX((SMCWTD-parameters%SMCMAX(NSOIL)), 0.0) * DZSNSO(NSOIL)
        WMINUS       = MAX((1.E-4-SMCWTD), 0.0) * DZSNSO(NSOIL)

        SMCWTD = MAX( MIN(SMCWTD,parameters%SMCMAX(NSOIL)) , 1.E-4)
        SH2O(NSOIL)    = SH2O(NSOIL) + WPLUS/DZSNSO(NSOIL)

!reduce fluxes at the bottom boundaries accordingly
        QDRAIN = QDRAIN - WPLUS/DT
        DEEPRECH = DEEPRECH - WMINUS
     ENDIF

  ENDIF

    DO K = NSOIL,2,-1
      EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(K) - SICE(K) ) )
      WPLUS        = MAX((SH2O(K)-EPORE), 0.0) * DZSNSO(K)
      SH2O(K)      = MIN(EPORE,SH2O(K))
      SH2O(K-1)    = SH2O(K-1) + WPLUS/DZSNSO(K-1)
    END DO

    EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(1) - SICE(1) ) )
    WPLUS        = MAX((SH2O(1)-EPORE), 0.0) * DZSNSO(1)
    SH2O(1)      = MIN(EPORE,SH2O(1))

   IF(WPLUS > 0.0) THEN
    SH2O(2)      = SH2O(2) + WPLUS/DZSNSO(2)
    DO K = 2,NSOIL-1
      EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(K) - SICE(K) ) )
      WPLUS        = MAX((SH2O(K)-EPORE), 0.0) * DZSNSO(K)
      SH2O(K)      = MIN(EPORE,SH2O(K))
      SH2O(K+1)    = SH2O(K+1) + WPLUS/DZSNSO(K+1)
    END DO

    EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(NSOIL) - SICE(NSOIL) ) )
    WPLUS        = MAX((SH2O(NSOIL)-EPORE), 0.0) * DZSNSO(NSOIL)
    SH2O(NSOIL)  = MIN(EPORE,SH2O(NSOIL))
   END IF

    SMC = SH2O + SICE

  END SUBROUTINE SSTEP

!== begin rosr12 ===================================================================================

  SUBROUTINE ROSR12 (P,A,B,C,D,DELTA,NTOP,NSOIL,NSNOW)
! ----------------------------------------------------------------------
! SUBROUTINE ROSR12
! ----------------------------------------------------------------------
! INVERT (SOLVE) THE TRI-DIAGONAL MATRIX PROBLEM SHOWN BELOW:
! ###                                            ### ###  ###   ###  ###
! #B(1), C(1),  0  ,  0  ,  0  ,   . . .  ,    0   # #      #   #      #
! #A(2), B(2), C(2),  0  ,  0  ,   . . .  ,    0   # #      #   #      #
! # 0  , A(3), B(3), C(3),  0  ,   . . .  ,    0   # #      #   # D(3) #
! # 0  ,  0  , A(4), B(4), C(4),   . . .  ,    0   # # P(4) #   # D(4) #
! # 0  ,  0  ,  0  , A(5), B(5),   . . .  ,    0   # # P(5) #   # D(5) #
! # .                                          .   # #  .   # = #   .  #
! # .                                          .   # #  .   #   #   .  #
! # .                                          .   # #  .   #   #   .  #
! # 0  , . . . , 0 , A(M-2), B(M-2), C(M-2),   0   # #P(M-2)#   #D(M-2)#
! # 0  , . . . , 0 ,   0   , A(M-1), B(M-1), C(M-1)# #P(M-1)#   #D(M-1)#
! # 0  , . . . , 0 ,   0   ,   0   ,  A(M) ,  B(M) # # P(M) #   # D(M) #
! ###                                            ### ###  ###   ###  ###
! ----------------------------------------------------------------------
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: NTOP
    INTEGER, INTENT(IN)   :: NSOIL,NSNOW
    INTEGER               :: K, KK

    REAL, DIMENSION(-NSNOW+1:NSOIL),INTENT(IN):: A, B, D
    REAL, DIMENSION(-NSNOW+1:NSOIL),INTENT(INOUT):: C,P,DELTA

! ----------------------------------------------------------------------
! INITIALIZE EQN COEF C FOR THE LOWEST SOIL LAYER
! ----------------------------------------------------------------------
    C (NSOIL) = 0.0
    P (NTOP) = - C (NTOP) / B (NTOP)
! ----------------------------------------------------------------------
! SOLVE THE COEFS FOR THE 1ST SOIL LAYER
! ----------------------------------------------------------------------
    DELTA (NTOP) = D (NTOP) / B (NTOP)
! ----------------------------------------------------------------------
! SOLVE THE COEFS FOR SOIL LAYERS 2 THRU NSOIL
! ----------------------------------------------------------------------
    DO K = NTOP+1,NSOIL
       P (K) = - C (K) * ( 1.0 / (B (K) + A (K) * P (K -1)) )
       DELTA (K) = (D (K) - A (K)* DELTA (K -1))* (1.0/ (B (K) + A (K)&
            * P (K -1)))
    END DO
! ----------------------------------------------------------------------
! SET P TO DELTA FOR LOWEST SOIL LAYER
! ----------------------------------------------------------------------
    P (NSOIL) = DELTA (NSOIL)
! ----------------------------------------------------------------------
! ADJUST P FOR SOIL LAYERS 2 THRU NSOIL
! ----------------------------------------------------------------------
    DO K = NTOP+1,NSOIL
       KK = NSOIL - K + (NTOP-1) + 1
       P (KK) = P (KK) * P (KK +1) + DELTA (KK)
    END DO
! ----------------------------------------------------------------------
  END SUBROUTINE ROSR12

!==begin VIC subroutines ========================================================================== 

  SUBROUTINE COMPUTE_VIC_SURFRUNOFF(parameters,DT,NSOIL,SMC,ZSOIL,QINSUR,ASAT,RUNSRF,PDDUM)

! ----------------------------------------------------------------------
! Calculate the saturated area and runoff based on VIC runoff scheme.
! This scheme adopted from VIC model
! Author: Prasanth Valayamkunnath <prasanth@ucar.edu>
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------

! Inputs
  TYPE (noahmp_parameters), INTENT(IN)   :: parameters
  INTEGER,                  INTENT(IN)   :: NSOIL
  REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: SMC
  REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: ZSOIL
  REAL                    , INTENT(IN)   :: QINSUR
  REAL                    , INTENT(IN)   :: DT
  REAL                    , INTENT(OUT)  :: ASAT
! Output
  REAL                    , INTENT(INOUT):: RUNSRF
  REAL                    , INTENT(INOUT):: PDDUM
!------------------------------------------------------------------------
!local 
  REAL    :: EX, I_0, I_MAX, BASIS, TOP_MOIST, TOP_MAX_MOIST
  INTEGER :: IZ

! Initialize Variables  
  EX    = 0.0
  ASAT  = 0.0
  I_MAX = 0.0
  I_0   = 0.0
  BASIS = 0.0
  TOP_MOIST     = 0.0
  TOP_MAX_MOIST = 0.0
  RUNSRF = 0.0


  DO IZ=1,NSOIL-2
    TOP_MOIST     = TOP_MOIST + (SMC(IZ)*-1.0*ZSOIL(IZ)) ! m
    TOP_MAX_MOIST = TOP_MAX_MOIST + (parameters%SMCMAX(IZ)*-1.0*ZSOIL(IZ)) ! m  
  END DO

  ! Saturated area from soil moisture
  EX    = parameters%BVIC/(1.0+parameters%BVIC)
  ASAT  = 1.0 - (( max(0.0,(1.0 - (TOP_MOIST/TOP_MAX_MOIST))))**EX)  ! 
  ASAT  = MAX(0.0, ASAT)
  ASAT  = MIN(1.0, ASAT)


  ! Infiltration for the previous time-step soil moisture based on ASAT
  I_MAX = (1.0 + parameters%BVIC)*TOP_MAX_MOIST ! m
  I_0   = I_MAX*(1.0 - (1.0 - ASAT)**(1.0/parameters%BVIC)) !m

  ! Solve for surface runoff
  IF(QINSUR .EQ. 0.0) THEN
     RUNSRF = 0.0
  ELSE IF(I_MAX .EQ. 0.0) THEN
     RUNSRF = QINSUR*DT
  ELSE IF( (I_0 + (QINSUR*DT)) .GT. I_MAX ) THEN
     RUNSRF = (QINSUR*DT) - TOP_MAX_MOIST + TOP_MOIST
  ELSE
     BASIS  = 1.0 - ((I_0 + (QINSUR*DT))/I_MAX)
     RUNSRF = (QINSUR*DT) - TOP_MAX_MOIST + TOP_MOIST + &
              TOP_MAX_MOIST*(basis**(1.0+parameters%BVIC))
  END IF

  RUNSRF = RUNSRF/(DT) ! m/s
  IF (RUNSRF .LT. 0.0) RUNSRF = 0.0
  IF (RUNSRF .GT. QINSUR) RUNSRF = QINSUR

  PDDUM = QINSUR - RUNSRF    ! m/s

 END SUBROUTINE COMPUTE_VIC_SURFRUNOFF

! End VIC subroutines

!== begin xinanjiag=================================================================================

  SUBROUTINE COMPUTE_XAJ_SURFRUNOFF(parameters,DT,FCR,NSOIL,SMC,ZSOIL,QINSUR,RUNSRF,PDDUM)

! ----------------------------------------------------------------------
! Calculate the saturated area and runoff based on Xinanjiag runoff scheme.
! Reference: Knoben, W. J., Freer, J. E., Fowler, K. J., Peel, M. C., & Woods, R. A. (2019). 
! Modular Assessment of Rainfall-Runoff Models Toolbox (MARRMoT) v1. 2: 
! an open-source, extendable framework providing implementations of 46 conceptual 
! hydrologic models as continuous state-space formulations.
! ----------------------------------------------------------------------
! Author: Prasanth Valayamkunnath <prasanth@ucar.edu>
! Date: August 03, 2020
! ----------------------------------------------------------------------

    IMPLICIT NONE
! ----------------------------------------------------------------------
! Inputs
    TYPE (noahmp_parameters), INTENT(IN)   :: parameters
    INTEGER,                  INTENT(IN)   :: NSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: SMC
    REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: ZSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: FCR      !fraction of imperviousness (-) = IMP
    REAL                    , INTENT(IN)   :: QINSUR
    REAL                    , INTENT(IN)   :: DT
! Output
    REAL                    , INTENT(INOUT):: RUNSRF
    REAL                    , INTENT(INOUT):: PDDUM
! local
    REAL    :: WM,WM_MAX,SM,SM_MAX,IRUNOFF,PRUNOFF
    INTEGER :: IZ
!------------------------------------------------------------------------

!initialize  
    WM      = 0.0
    WM_MAX  = 0.0
    SM      = 0.0
    SM_MAX  = 0.0
    IRUNOFF = 0.0
    PRUNOFF = 0.0
    RUNSRF  = 0.0

    DO IZ=1,NSOIL-2
       IF ((SMC(IZ)-parameters%SMCREF(IZ)) .GT. 0.0) THEN ! soil moisture greater than field capacity
          SM     = SM + (SMC(IZ) - parameters%SMCREF(IZ) )*-1.0*ZSOIL(IZ) !m
          WM     = WM + (parameters%SMCREF(IZ)*-1.0*ZSOIL(IZ))            !m  
       ELSE
          WM     = WM + (SMC(IZ)*-1.0*ZSOIL(IZ))
       END IF
       WM_MAX = WM_MAX + (parameters%SMCREF(IZ)*-1.0*ZSOIL(IZ))
       SM_MAX = SM_MAX + (parameters%SMCMAX(IZ) - parameters%SMCREF(IZ))*-1.0*ZSOIL(IZ)
    END DO
    WM = MIN(WM,WM_MAX) ! tension water (m) 
    SM = MIN(SM,SM_MAX) ! free water (m)

! impervious surface runoff R_IMP    
    IRUNOFF = FCR(1)*QINSUR*DT

! solve pervious surface runoff (m) based on Eq. (310)
    IF ((WM/WM_MAX) .LE. (0.5-parameters%AXAJ))THEN
       PRUNOFF = (1.0-FCR(1))*QINSUR*DT*((0.5-parameters%AXAJ)**(1.0-parameters%BXAJ))*((WM/WM_MAX)**parameters%BXAJ)
    ELSE
       PRUNOFF = (1.0-FCR(1))*QINSUR*DT*(1.0-(((0.5+parameters%AXAJ)**(1.0-parameters%BXAJ))*((1.0-(WM/WM_MAX))**parameters%BXAJ)))
    END IF

! estimate surface runoff based on Eq. (313)
    IF(QINSUR .EQ. 0.0) THEN
      RUNSRF  = 0.0
    ELSE
      RUNSRF = PRUNOFF*(1.0-((1.0-(SM/SM_MAX))**parameters%XXAJ))+IRUNOFF
    END IF
    RUNSRF = RUNSRF/DT !m/s
    RUNSRF = MAX(0.0,    RUNSRF)
    RUNSRF = MIN(QINSUR, RUNSRF)
    PDDUM  = QINSUR - RUNSRF

  END SUBROUTINE COMPUTE_XAJ_SURFRUNOFF

!== end xinanjiag ==================================================================================

!== begin dynamic VIC ==============================================================================

  SUBROUTINE  DYNAMIC_VIC(parameters,DT,SMC,SH2O,SICE,SICEMAX,NSOIL,ZSOIL,QINSUR,FACC,PDDUM,RUNSRF)

! --------------------------------------------------------------------------------
! compute inflitration rate at soil surface and estimate surface runoff based on 
! Liang, X., & Xie, Z. (2001). A new surface runoff parameterization with subgrid-scale
! soil heterogeneity for land surface models. Advances in Water Resources, 24(9-10), 1173-1193.
! Author: Prasanth Valayamkunnath <prasanth@ucar.edu>
! Date  : August 3, 2020
! --------------------------------------------------------------------------------

   IMPLICIT NONE
! --------------------------------------------------------------------------------
! inputs
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                  INTENT(IN) :: NSOIL         !no. of soil layers
  REAL,                     INTENT(IN) :: DT            !time step (sec)
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: ZSOIL         !depth of soil layer-bottom [m]
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SH2O          !soil liquid water content [m3/m3]
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SICE          !soil ice content [m3/m3]
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SMC           !soil moisture content [m3/m3]
  REAL,                     INTENT(IN) :: QINSUR        !water input on soil surface [m/s]
  REAL,                     INTENT(IN) :: SICEMAX       !maximum soil ice content (m3/m3)
! inouts
  REAL,                     INTENT(INOUT) :: FACC       !accumulated infiltration (m)
! outputs
  REAL,                    INTENT(OUT) :: RUNSRF        !surface runoff [mm/s] 
  REAL,                    INTENT(OUT) :: PDDUM         !infiltration rate at surface
! locals
  REAL                                 :: BB            !B parameter for infiltration scaling curve
  REAL                                 :: TOP_MOIST     !actual water depth in top layers (m)
  REAL                                 :: TOP_MAX_MOIST !water depth in top layers (m)
  REAL                                 :: DP            !water input on soil surface (m)
  REAL                                 :: I_0           !initial water depth (m)
  REAL                                 :: I_MAX         !maximum water depth (m)
  REAL                                 :: FSUR          !surface infiltration rate (m/s)
  REAL                                 :: FMAX          !maximum infiltration rate (m/s)
  REAL                                 :: RUNOFFSAT     !saturation excess runoff (m/s)
  REAL                                 :: RUNOFFINF     !infiltration excess runoff (m/s)
  REAL                                 :: INFILTRTN     !infiltration (m/s)
  REAL                                 :: TEMPR1        !temporary saturation excess runoff (m/s)
  REAL                                 :: TEMPR2        !temporary infiltration excess runoff (m/s)
  REAL                                 :: R1            !saturation excess runoff (m/s)
  REAL                                 :: R2            !infiltration excess runoff (m/s)
  REAL                                 :: YD            !initial depth Y (m)
  REAL                                 :: YD_OLD        !initial depth Y (m)
  REAL                                 :: YD0           !initial depth Y (m)
  REAL                                 :: TEMP1, ERROR
  INTEGER                              :: IZ, IZMAX, INFLMAX
!---------------------------------------------------------------------------------

  TOP_MOIST     = 0.0
  TOP_MAX_MOIST = 0.0
  BB            = 1.0
  DP            = 0.0
  I_MAX         = 0.0
  I_0           = 0.0
  RUNOFFSAT     = 0.0
  RUNOFFINF     = 0.0
  INFILTRTN     = 0.0
  RUNSRF        = 0.0
  IZMAX         = 20
  ERROR         = 1.388889E-07*DT ! 0.5 mm per hour time step
  BB = parameters%BBVIC

  DO IZ=1,NSOIL-2
    TOP_MOIST     = TOP_MOIST + (SMC(IZ)*-1.0*ZSOIL(IZ))                                   ! actual moisture in top layers, [m]
    TOP_MAX_MOIST = TOP_MAX_MOIST + (parameters%SMCMAX(IZ)*-1.0*ZSOIL(IZ))                 ! maximum moisture in top layers, [m]  
  END DO
  IF(TOP_MOIST .GT. TOP_MAX_MOIST) TOP_MOIST = TOP_MAX_MOIST
  DP     = QINSUR * DT                                                                   ! precipitation depth, [m]
  I_MAX  = TOP_MAX_MOIST * (parameters%BDVIC+1.0)                                         ! maximum infiltration capacity, im, [m], Eq. 14
  I_0    = I_MAX * (1.0-(1.0-(TOP_MOIST/TOP_MAX_MOIST)**(1.0/(1.0+parameters%BDVIC))))        ! infiltration capacity, i [m] in the Eq. 1
  ! I_MAX = CAP_minf ; I_0 = A  
  INFLMAX = 0


  IF (OPT_INFDV .EQ. 1) THEN
     CALL PHILIP_INFIL(parameters,NSOIL,SMC,SICE,QINSUR,DT,FACC,FSUR,INFLMAX)
  ELSE IF (OPT_INFDV .EQ. 2) THEN
     CALL GREEN_AMPT_INFIL(parameters,NSOIL,ZSOIL,SMC,SICE,QINSUR,FACC,FSUR,INFLMAX)
  ELSE IF (OPT_INFDV .EQ. 3) THEN
     CALL SMITH_PARLANGE_INFIL(parameters,NSOIL,ZSOIL,SMC,SICE,QINSUR,FACC,FSUR,INFLMAX)
  END IF

  ! I_MM = FSUR; I_M = FMAX  
  FMAX = (BB+1.0)*FSUR
  IF(DP .LE. 0.0) THEN
    RUNOFFSAT = 0.0
    RUNOFFINF = 0.0
    INFILTRTN = 0.0
    GOTO 2001
  ELSE
    IF((TOP_MOIST .GE. TOP_MAX_MOIST) .AND. (I_0 .GE. I_MAX)) THEN
      TOP_MOIST = TOP_MAX_MOIST
      I_0       = I_MAX
      RUNOFFSAT = DP
      RUNOFFINF = 0.0
      INFILTRTN = 0.0
      GOTO 2001
    ELSE
      I_0 = I_MAX * (1.0-(1.0-(TOP_MOIST/TOP_MAX_MOIST)**(1.0/(1.0+parameters%BDVIC))))
      IF((DP+I_0) .GT. I_MAX)THEN
        IF((FMAX*DT) .GE. DP) THEN
          YD     = I_MAX - I_0
          TEMPR1 = 0.0
          CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
          TEMP1  = I_MAX-I_0-TEMPR1-((FSUR*DT) * (1.0 - (1.0-((DP-TEMPR1)/(FMAX*DT))**(BB+1.0))))
          IF(TEMP1 .LE. 0.0) THEN
            YD        = I_MAX - I_0
            INFILTRTN = TOP_MAX_MOIST - TOP_MOIST
            RUNOFFSAT = DP - INFILTRTN
            RUNOFFINF = 0.0
            TOP_MOIST = TOP_MAX_MOIST
            I_0       = I_MAX
            GOTO 2001
          ELSE
            YD        = 0.0
            DO IZ = 1,IZMAX ! loop : IITERATION1
               YD_OLD = YD
               TEMPR1 = 0.0
               CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
               YD     = TEMPR1 + ((FSUR*DT) * (1.0 - (1.0-((DP-TEMPR1)/(FMAX*DT))**(BB+1.0))))
               IF ((ABS(YD-YD_OLD) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                  GOTO 1003
               END IF
            END DO
          END IF
        ELSE
          TEMPR1 = 0.0
          CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
          IF((TEMPR1+(FMAX*DT)) .LE. DP) THEN
            IF((I_MAX-I_0-TEMPR1-(FMAX*DT)) .LE. 0.0)THEN
              YD        = I_MAX - I_0
              INFILTRTN = TOP_MAX_MOIST - TOP_MOIST
              RUNOFFSAT = DP - INFILTRTN
              RUNOFFINF = 0.0
              TOP_MOIST = TOP_MAX_MOIST
              I_0       = I_MAX
              GOTO 2001
            ELSE
              YD        = 0.0

              DO IZ = 1,IZMAX ! loop : IITERATION2
                 YD_OLD = YD
                 TEMPR1 = 0.0
                 CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
                 YD     = TEMPR1 + (FSUR*DT)
                 IF ((ABS(YD-YD_OLD) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                    GOTO 1003
                 END IF
              END DO
            END IF
          ELSE

            YD = DP/2.0
            DO IZ = 1,IZMAX ! loop : IITERATION30
               YD_OLD = YD
               TEMPR1 = 0.0
               CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
               YD     = YD - TEMPR1 - (FSUR*DT) + DP
               IF (YD .LE. 0.0) YD = 0.0
               IF (YD .GE. DP) YD = DP
               IF ((ABS(YD-YD_OLD) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                  YD0 = YD
                  EXIT
               END IF
            END DO
            DO IZ = 1,IZMAX ! loop : IITERATION3
               YD_OLD = YD
               TEMPR1 = 0.0
               TEMPR2 = 0.0
               CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
               CALL RR2(YD,YD0,TEMPR1,FMAX,FSUR,DT,DP,BB,TEMPR2)
               YD     = DP - TEMPR2
               IF ((ABS(YD-YD_OLD) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                  GOTO 1003
               END IF
            END DO
1003        IF(YD .LE. 0.0) YD = 0.0
            IF(YD .GE. DP) YD = DP
            CALL RR1(parameters,I_0,I_MAX,YD,R1)
            RUNOFFSAT = R1
            RUNOFFINF = DP - YD
            INFILTRTN = YD - RUNOFFSAT
            TOP_MOIST = TOP_MOIST + INFILTRTN
            YD        = I_0+YD
            IF (TOP_MOIST .LE. 0.0) TOP_MOIST=0.0
            IF (TOP_MOIST .GE. TOP_MAX_MOIST) TOP_MOIST = TOP_MAX_MOIST
            I_0       = I_MAX * (1.0-(1.0-(TOP_MOIST/TOP_MAX_MOIST)**(1.0/(1.0+parameters%BDVIC))))
            GOTO 2001
          END IF
        END IF

      ELSE
        IF((FMAX*DT) .GE. DP) THEN
          YD = DP/2.0
          DO IZ = 1,IZMAX ! ITERATION1
             YD_OLD = YD
             TEMPR1 = 0.0
             CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
             YD = TEMPR1 + ((FSUR*DT) * (1.0 - (1.0-((DP-TEMPR1)/(FMAX*DT))**(BB+1.0))))
             IF ((ABS(YD - YD_OLD) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                GOTO 1004
             END IF
          END DO
        ELSE
          TEMPR1 = 0.0
          CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
          IF((TEMPR1+(FMAX*DT)) .LE. DP)THEN
              YD = DP/2.0
              DO IZ = 1,IZMAX ! ITERATION2
                 YD_OLD = YD
                 TEMPR1 = 0.0
                 CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
                 YD     = TEMPR1+(FSUR*DT)
                 IF((ABS(YD - YD_OLD) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                    GOTO 1004
                 END IF
              END DO
          ELSE
              YD = 0.0
              DO IZ = 1,IZMAX ! ITERATION30
                 YD_OLD = YD
                 TEMPR1 = 0.0
                 CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
                 YD     = (DP - (FMAX*DT)) + YD - TEMPR1
                 IF(YD .LE. 0.0) YD = 0.0
                 IF(YD .GE. DP) YD = DP
                 TEMPR1 = 0.0
                 CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
                 IF ((ABS(TEMPR1+(FMAX*DT)-DP) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                    YD0 = YD
                    EXIT
                 END IF
              END DO
              DO  IZ = 1,IZMAX ! ITERATION3
                 YD_OLD = YD
                 TEMPR1 = 0.0
                 TEMPR2 = 0.0
                 CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1)
                 CALL RR2(YD,YD0,TEMPR1,FMAX,FSUR,DT,DP,BB,TEMPR2)
                 YD     = DP - TEMPR2
                 IF ((ABS(YD-YD_OLD) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                   GOTO 1004
                 END IF
              END DO
          END IF
        END IF
1004    IF(YD .LE. 0.0) YD = 0.0
        IF(YD .GE. DP)  YD = DP
        R1 = 0.0
        CALL RR1(parameters,I_0,I_MAX,YD,R1)
        RUNOFFSAT = R1
        RUNOFFINF = DP - YD
        INFILTRTN = YD - RUNOFFSAT
        TOP_MOIST = TOP_MOIST + INFILTRTN
        IF (TOP_MOIST .LE. 0.0) TOP_MOIST=0.0
        IF (TOP_MOIST .GE. TOP_MAX_MOIST) TOP_MOIST = TOP_MAX_MOIST
        I_0       = I_MAX * (1.0-(1.0-(TOP_MOIST/TOP_MAX_MOIST)**(1.0/(1.0+parameters%BDVIC))))
      END IF
    END IF
  END IF

2001 RUNSRF = (RUNOFFSAT + RUNOFFINF)/DT
     RUNSRF = MIN(RUNSRF,QINSUR)
     RUNSRF = MAX(RUNSRF,0.0)
     PDDUM  = QINSUR - RUNSRF

  END SUBROUTINE DYNAMIC_VIC

! ---------------------------  Runoff subroutines for dynamic VIC ----------------------------

  SUBROUTINE RR1 (parameters,I_0,I_MAX,YD,R1)
!---------------------------------------------------------------------------------------------
! This subroutine estimate saturation excess runoff, R1
! Author: Prasanth Valayamkunnath <prasanth@ucar.edu>
!---------------------------------------------------------------------------------------------
   IMPLICIT NONE
! --------------------------------------------------------------------------------------------
   type (noahmp_parameters), intent(in) :: parameters
   REAL,                     INTENT(IN) :: I_0,I_MAX,YD
   REAL,                     INTENT(OUT):: R1
   REAL                                 :: TDEPTH
!------------------------------------------------------

   TDEPTH = I_0 + YD
   IF(TDEPTH .GT. I_MAX) TDEPTH = I_MAX

   !Saturation excess runoff , Eq 5.
   R1 = YD - ( (I_MAX/(parameters%BDVIC+1.0)) * ( ((1.0 - (I_0/I_MAX))**(parameters%BDVIC+1.0)) &
                                               - ((1.0 - (TDEPTH/I_MAX))**(parameters%BDVIC+1.0))))

   IF (R1 .LT. 0.0) R1 = 0.0

  END SUBROUTINE RR1

!---------------------------------------------------------------------------------------------
  SUBROUTINE RR2 (YD,Y0,R1,FMAX,FSUR,DT,DP,BB,R2)
!---------------------------------------------------------------------------------------------
! This subroutine estimate infiltration excess runoff, R1
! Author: Prasanth Valayamkunnath <prasanth@ucar.edu>
!---------------------------------------------------------------------------------------------
   IMPLICIT NONE
! --------------------------------------------------------------------------------------------
   REAL,                     INTENT(IN) :: YD,Y0,R1,FMAX,FSUR,DT,DP,BB
   REAL,                     INTENT(OUT):: R2
!------------------------------------------------------

   IF(YD .GE. Y0)THEN
     R2 = DP - R1 - (FMAX*DT* (1.0 - ((1.0 - (DP-R1)/(FMAX*DT))**(BB+1.0))))
   ELSE
     R2 = DP - R1 - (FMAX*DT)
   END IF

   IF(R2 .LT. 0.0) R2 =0.0

END SUBROUTINE RR2

!== end dynamic VIC ================================================================================

!== begin smith-parlange infiltration ===============================================================

  SUBROUTINE SMITH_PARLANGE_INFIL(parameters,NSOIL,ZSOIL,SMC,SICE,QINSUR,FACC,FSUR,INFLMAX)

!---------------------------------------------------------------------------------------------
! This function estimate infiltration rate based on Smith-Parlange equation. We use its three
! parameter version of the equation (Eq. 6.25) from Smith, R.E. (2002) Infiltration Theory for
! Hydrologic Applications, Water Resources Monograph 15, AGU. 
! Author: Prasanth Valayamkunnath <prasanth@ucar.edu>
!---------------------------------------------------------------------------------------------
   IMPLICIT NONE
! --------------------------------------------------------------------------------------------
   type (noahmp_parameters), intent(in) :: parameters
   INTEGER,                  INTENT(IN) :: NSOIL  !no of soil layers (4)
   REAL, DIMENSION(1:NSOIL), INTENT(IN) :: ZSOIL  !depth of soil layer-bottom [m]
   REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SMC    !soil moisture content [m3/m3]
   REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SICE   !soil ice content [m3/m3]
   REAL,                     INTENT(IN) :: QINSUR !water input on soil surface [m/s]
   INTEGER,                  INTENT(IN) :: INFLMAX!check for maximum infiltration at SMCWLT
! in outs
   REAL,                     INTENT(INOUT) :: FACC   !accumulated infiltration rate (m/s)
! outputs
   REAL,                     INTENT(OUT)   :: FSUR   !surface infiltration rate (m/s)
! local variables
   REAL                                    :: WDF    ! soil water diffusivity       
   REAL                                    :: WCND   ! soil water conductivity[m/s]
   REAL                                    :: GAM    ! smith-parlang weighing parameter[-]
   REAL                                    :: JJ     ! dummy variable
   INTEGER                                 :: ISOIL
!---------------------------------------------------------------------------------

   ! smith-parlang weighing parameter, GAMMA
   GAM = 0.82
   ISOIL = 1
   ! check whether we are estimating infiltration for current SMC or SMCWLT
   IF (INFLMAX .EQ. 1)THEN ! not active for now as the maximum infiltration is estimated based on table values

      ! estimate initial soil hydraulic conductivty (Ki in the equation), WCND (m/s)
      CALL WDFCND2 (parameters,WDF,WCND,parameters%SMCWLT(ISOIL),0.0,ISOIL)

      ! Maximum infiltrability based on the Eq. 6.25. (m/s)
      JJ   = parameters%GDVIC * (parameters%SMCMAX(ISOIL) - parameters%SMCWLT(ISOIL)) * -1.0 * ZSOIL(ISOIL)
      FSUR = parameters%DKSAT(ISOIL) + (GAM * (parameters%DKSAT(ISOIL) - WCND) / (EXP(GAM * 1E-05 / JJ) -1.0))

      ! infiltration rate at surface
      IF(parameters%DKSAT(ISOIL) .LT. QINSUR)THEN
        FSUR = MIN(QINSUR,FSUR)
      ELSE
        FSUR = QINSUR
      END IF
      IF(FSUR .LT. 0.0) FSUR = WCND

   ELSE

      ! estimate initial soil hydraulic conductivty (Ki in the equation), WCND (m/s)
      CALL WDFCND2 (parameters,WDF,WCND,SMC(ISOIL),SICE(ISOIL),ISOIL)

      ! Maximum infiltrability based on the Eq. 6.25. (m/s)
      JJ   = parameters%GDVIC * max(0.0,(parameters%SMCMAX(ISOIL) - SMC(ISOIL))) * -1.0 * ZSOIL(ISOIL)
      IF(JJ .eq. 0.0)THEN ! infiltration at surface == saturated hydraulic conductivity
        FSUR = WCND
      ELSE
        FSUR = parameters%DKSAT(ISOIL) + (GAM * (parameters%DKSAT(ISOIL) - WCND) / (EXP(GAM * FACC / JJ) -1.0))
      END IF

      ! infiltration rate at surface  
      IF(parameters%DKSAT(ISOIL) .LT. QINSUR)THEN
        FSUR = MIN(QINSUR,FSUR)
      ELSE
        FSUR = QINSUR
      END IF

      ! accumulated infiltration function
      FACC = FACC + FSUR

   END IF

  END SUBROUTINE SMITH_PARLANGE_INFIL

!== end smith-parlang infiltration =================================================================

!== begin Green_Ampt infiltration ==================================================================

  SUBROUTINE GREEN_AMPT_INFIL(parameters,NSOIL,ZSOIL,SMC,SICE,QINSUR,FACC,FSUR,INFLMAX)

!-------------------------------------------------------------------------------------------------
! This function estimate infiltration rate based on Green-Ampt equation. We use its three
! parameter version of the smith-parlage equation (Eq. 6.25) from Smith, R.E. (2002) Infiltration Theory for
! Hydrologic Applications, Water Resources Monograph 15, AGU. Where gamma = 0, Eq 6.25 = Green-Ampt.
! Author: Prasanth Valayamkunnath <prasanth@ucar.edu>
!-------------------------------------------------------------------------------------------------
   IMPLICIT NONE
! ------------------------------------------------------------------------------------------------
   type (noahmp_parameters), intent(in) :: parameters
   INTEGER,                  INTENT(IN) :: NSOIL  !no of soil layers (4)
   REAL, DIMENSION(1:NSOIL), INTENT(IN) :: ZSOIL  !depth of soil layer-bottom [m]
   REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SMC    !soil moisture content [m3/m3]
   REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SICE   !soil ice content [m3/m3]
   REAL,                     INTENT(IN) :: QINSUR !water input on soil surface [m/s]
   INTEGER,                  INTENT(IN) :: INFLMAX!check for maximum infiltration at SMCWLT
! in outs
   REAL,                     INTENT(INOUT) :: FACC   !accumulated infiltration rate (m/s)
! outputs
   REAL,                     INTENT(OUT)   :: FSUR   !surface infiltration rate (m/s)
! local variables
   REAL                                    :: WDF    ! soil water diffusivity
   REAL                                    :: WCND   ! soil water conductivity[m/s]
   REAL                                    :: JJ     ! dummy variable 
   INTEGER                                 :: ISOIL
!---------------------------------------------------------------------------------

   ISOIL = 1
   IF(INFLMAX .EQ. 1)THEN

     ! estimate initial soil hydraulic conductivty (Ki in the equation), WCND (m/s)
     CALL WDFCND2 (parameters,WDF,WCND,parameters%SMCWLT(ISOIL),0.0,ISOIL)

     ! Maximum infiltrability based on the Eq. 6.25. (m/s)
     JJ   = parameters%GDVIC * (parameters%SMCMAX(ISOIL) - parameters%SMCWLT(ISOIL)) * -1.0 * ZSOIL(ISOIL)
     FSUR = parameters%DKSAT(ISOIL) + ((JJ/1E-05) * (parameters%DKSAT(ISOIL) - WCND))

     !maximum infiltration rate at surface
     IF(FSUR .LT. 0.0) FSUR = WCND

   ELSE

     ! estimate initial soil hydraulic conductivty (Ki in the equation), WCND (m/s)
     CALL WDFCND2 (parameters,WDF,WCND,SMC(ISOIL),SICE(ISOIL),ISOIL)

     ! Maximum infiltrability based on the Eq. 6.25. (m/s)
     JJ   = parameters%GDVIC * max(0.0,(parameters%SMCMAX(ISOIL) - SMC(ISOIL))) * -1.0 * ZSOIL(ISOIL)
     FSUR = parameters%DKSAT(ISOIL) + ((JJ/FACC) * (parameters%DKSAT(ISOIL) - WCND))

     ! infiltration rate at surface
     IF(parameters%DKSAT(ISOIL) .LT. QINSUR)THEN
        FSUR = MIN(QINSUR,FSUR)
     ELSE
        FSUR = QINSUR
     END IF
     ! accumulated infiltration function
     FACC = FACC + FSUR

   END IF

  END SUBROUTINE GREEN_AMPT_INFIL

!== end Green-Ampt infiltration ====================================================================

!== begin Philip's infiltration ====================================================================

  SUBROUTINE PHILIP_INFIL(parameters,NSOIL,SMC,SICE,QINSUR,DT,FACC,FSUR,INFLMAX)

!-------------------------------------------------------------------------------------------------------
! This function estimate infiltration rate based on Philip's two parameter equation (Eq. 2) presented in
! Valiantzas (2010). New linearized two-parameter infiltration equation for direct determination 
! of conductivity and sorptivity, J. Hydrology.
! Author: Prasanth Valayamkunnath <prasanth@ucar.edu>
!---------------------------------------------------------------------------------------------
   IMPLICIT NONE
! --------------------------------------------------------------------------------------------
   type (noahmp_parameters), intent(in) :: parameters
   INTEGER,                  INTENT(IN) :: NSOIL  !no of soil layers (4)
   REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SMC    !soil moisture content [m3/m3]
   REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SICE   !soil ice content [m3/m3]
   REAL,                     INTENT(IN) :: QINSUR !water input on soil surface [m/s]
   REAL,                     INTENT(IN) :: DT     !time-step (sec)
   INTEGER,                  INTENT(IN) :: INFLMAX!check for maximum infiltration at SMCWLT 
! in outs
   REAL,                     INTENT(INOUT) :: FACC   !accumulated infiltration rate (m/s)
! outputs
   REAL,                     INTENT(OUT)   :: FSUR   !surface infiltration rate (m/s)
! local variables
   REAL                                    :: WDF    ! soil water diffusivity (m2/s)
   REAL                                    :: WCND   ! soil water conductivity[m/s]
   REAL                                    :: SP     ! sorptivity (LT^-1/2)
   REAL                                    :: AP     ! intial hydraulic conductivity (m/s,L/T)
   REAL                                    :: FMAX   ! Maximum infiltration (m/s)
   INTEGER                                 :: ISOIL
!---------------------------------------------------------------------------------

   ISOIL = 1
   IF (INFLMAX .EQ. 1) THEN

     ! estimate initial soil hydraulic conductivty and diffusivity (Ki, D(theta) in the equation)
     CALL WDFCND2 (parameters,WDF,WCND,parameters%SMCWLT(ISOIL),0.0,ISOIL)

     ! Sorptivity based on Eq. 10b from Kutílek, Miroslav, and Jana Valentová (1986)
     ! Sorptivity approximations. Transport in Porous Media 1.1, 57-62.
     SP = SQRT(2.0 * (parameters%SMCMAX(ISOIL) - parameters%SMCWLT(ISOIL)) * (parameters%DWSAT(ISOIL) - WDF))

     ! Parameter A in Eq. 9 of Valiantzas (2010) is given by
     AP = MIN(WCND, (2.0/3.0)*parameters%DKSAT(ISOIL))
     AP = MAX(AP,   (1.0/3.0)*parameters%DKSAT(ISOIL))

     ! Maximun infiltration rate, m
     FSUR = (1.0/2.0)*SP*(DT**(-1.0/2.0))+AP ! m/s
     IF(FSUR .LT. 0.0) FSUR = WCND

   ELSE


     ! estimate initial soil hydraulic conductivty and diffusivity (Ki, D(theta) in the equation)
     CALL WDFCND2 (parameters,WDF,WCND,SMC(ISOIL),SICE(ISOIL),ISOIL)

     ! Sorptivity based on Eq. 10b from Kutílek, Miroslav, and Jana Valentová (1986) 
     ! Sorptivity approximations. Transport in Porous Media 1.1, 57-62.
     SP = SQRT(2.0 * max(0.0,(parameters%SMCMAX(ISOIL) - SMC(ISOIL))) * (parameters%DWSAT(ISOIL) - WDF))
     ! Parameter A in Eq. 9 of Valiantzas (2010) is given by
     AP = MIN(WCND, (2.0/3.0)*parameters%DKSAT(ISOIL))
     AP = MAX(AP,   (1.0/3.0)*parameters%DKSAT(ISOIL))

     ! Maximun infiltration rate, m
     FSUR = (1.0/2.0)*SP*(DT**(-1.0/2.0)) + AP ! m/s

     ! infiltration rate at surface
     IF(parameters%DKSAT(ISOIL) .LT. QINSUR)THEN
       FSUR = MIN(QINSUR,FSUR)
     ELSE
       FSUR = QINSUR
     END IF
     ! accumulated infiltration function
     FACC = FACC + FSUR

   END IF

  END SUBROUTINE PHILIP_INFIL

!== end Phillips infiltration ======================================================================

!== begin tile_drain ===============================================================================

  SUBROUTINE TILE_DRAIN (parameters,NSOIL,SH2O,SMC,SICE,ZSOIL,QTLDRN,DT)

! ----------------------------------------------------------------------
! Calculate tile drainage discharge (mm) based on simple model  ! pvk
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! inout
    type (noahmp_parameters), intent(in) :: parameters
    INTEGER,INTENT(IN)                          :: NSOIL
    REAL,   INTENT(IN)                          :: DT
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT)     :: SH2O
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT)     :: SMC
    REAL,INTENT(INOUT)                          :: QTLDRN
! input
    REAL, DIMENSION(1:NSOIL), INTENT(IN)        :: SICE
    REAL, DIMENSION(1:NSOIL), INTENT(IN)        :: ZSOIL
! local 
    INTEGER                                     :: K
    REAL                                        :: TDRVOL  ! temp variable for drainage volume (mm)
    REAL,DIMENSION(1:NSOIL)                     :: OVRFC   ! temp variable for volume of water above field capacity
    REAL,DIMENSION(1:NSOIL)                     :: AVFC    ! Available field capacity = FC - SICE (m3/m3)
    REAL,DIMENSION(1:NSOIL)                     :: ZLAYER  ! thickness of soil layer
    REAL,DIMENSION(1:NSOIL)                     :: TDFRAC
    REAL                                        :: TDDC
    REAL                                        :: TDSUM
! ----------------------------------------------------------------------

    TDRVOL = 0.0
    OVRFC  = 0.0
    QTLDRN = 0.0
    ZLAYER = 0.0
    AVFC   = 0.0
    TDSUM  = 0.0
    TDFRAC = 0.0
    TDDC   = parameters%TD_DC * DT/(24.0*3600.0)

    DO K = 1, NSOIL
      IF (K == 1) THEN
         ZLAYER(K) = -1.0 * ZSOIL(K)
      ELSE
         ZLAYER(K) = (ZSOIL(K-1)-ZSOIL(K))
      END IF
    END DO
      IF (parameters%DRAIN_LAYER_OPT == 0) THEN ! drainage from one specified layer in MPTABLE.TBL
         !print*, "CASE = 1"
         K = parameters%TD_DEPTH
         AVFC(K)  = parameters%SMCREF(K) - SICE (K)
         OVRFC(K) = (SH2O(K) - (parameters%TDSMC_FAC*AVFC(K))) * ZLAYER(K) * 1000.0 ! mm
         IF (OVRFC(K) > 0.0) THEN
            IF (OVRFC(K) > TDDC) OVRFC(K) = TDDC
            TDRVOL   = TDRVOL  + OVRFC(K)
            SH2O(K)  = SH2O(K) - (OVRFC(K)/(ZLAYER(K) * 1000.0))
            SMC(K)   = SH2O(K) + SICE (K)
         END IF
      ELSE IF (parameters%DRAIN_LAYER_OPT == 1) THEN
         !print*, "CASE = 2. Draining from layer 1 and 2"
         DO K = 1, 2
            AVFC(K)  = parameters%SMCREF(K) - SICE (K)
            OVRFC(K) = (SH2O(K) - (parameters%TDSMC_FAC*AVFC(K))) * ZLAYER(K) * 1000.0 ! mm
            IF(OVRFC(K) < 0.0) OVRFC(K) = 0.0
            TDSUM    = TDSUM + OVRFC(K)
         END DO
         DO K = 1, 2
            IF(OVRFC(K) .NE. 0.0) THEN
              TDFRAC(K)   = OVRFC(K)/TDSUM
            END IF
         END DO
         IF (TDSUM > 0.0) THEN
            IF (TDSUM > TDDC) TDSUM = TDDC
            TDRVOL   = TDRVOL  + TDSUM
            DO K = 1, 2
              OVRFC(K) = TDFRAC(K)*TDSUM
              SH2O(K)  = SH2O(K) - (OVRFC(K)/(ZLAYER(K) * 1000.0))
              SMC(K)   = SH2O(K) + SICE (K)
            END DO
         END IF
      ELSE IF (parameters%DRAIN_LAYER_OPT == 2) THEN
         !print*, "CASE = 3.  Draining from layer 1 2 and 3"
         DO K = 1, 3
            AVFC(K)  = parameters%SMCREF(K) - SICE (K)
            OVRFC(K) = (SH2O(K) - (parameters%TDSMC_FAC*AVFC(K))) * ZLAYER(K) * 1000.0 ! mm
            IF(OVRFC(K) < 0.0) OVRFC(K) = 0.0
            TDSUM    = TDSUM + OVRFC(K)
         END DO
         DO K = 1, 3
            IF(OVRFC(K) .NE. 0.0) THEN
              TDFRAC(K)   = OVRFC(K)/TDSUM
            END IF
         END DO
         IF (TDSUM > 0.0) THEN
            IF (TDSUM > TDDC) TDSUM = TDDC
            TDRVOL   = TDRVOL  + TDSUM
            DO K = 1, 3
              OVRFC(K) = TDFRAC(K)*TDSUM
              SH2O(K)  = SH2O(K) - (OVRFC(K)/(ZLAYER(K) * 1000.0))
              SMC(K)   = SH2O(K) + SICE (K)
            END DO
         END IF

      ELSE IF (parameters%DRAIN_LAYER_OPT == 3) THEN
         !print*, "CASE = 3.  Draining from layer 2 and 3"
         DO K = 2, 3
            AVFC(K)  = parameters%SMCREF(K) - SICE (K)
            OVRFC(K) = (SH2O(K) - (parameters%TDSMC_FAC*AVFC(K))) * ZLAYER(K) * 1000.0 ! mm
            IF(OVRFC(K) < 0.0) OVRFC(K) = 0.0
            TDSUM    = TDSUM + OVRFC(K)
         END DO
         DO K = 2, 3
            IF(OVRFC(K) .NE. 0.0) THEN
              TDFRAC(K)   = OVRFC(K)/TDSUM
            END IF
         END DO
         IF (TDSUM > 0.0) THEN
            IF (TDSUM > TDDC) TDSUM = TDDC
            TDRVOL   = TDRVOL  + TDSUM
            DO K = 2, 3
              OVRFC(K) = TDFRAC(K)*TDSUM
              SH2O(K)  = SH2O(K) - (OVRFC(K)/(ZLAYER(K) * 1000.0))
              SMC(K)   = SH2O(K) + SICE (K)
            END DO
         END IF
      ELSE IF (parameters%DRAIN_LAYER_OPT == 4) THEN

         !print*, "CASE = 4.  Draining from layer 3 and 4"
         DO K = 3, 4
            AVFC(K)  = parameters%SMCREF(K) - SICE (K)
            OVRFC(K) = (SH2O(K) - (parameters%TDSMC_FAC*AVFC(K))) * ZLAYER(K) * 1000.0 ! mm
            IF(OVRFC(K) < 0.0) OVRFC(K) = 0.0
            TDSUM    = TDSUM + OVRFC(K)
         END DO
         DO K = 3, 4
            IF(OVRFC(K) .NE. 0.0) THEN
              TDFRAC(K)   = OVRFC(K)/TDSUM
            END IF
         END DO


         IF (TDSUM > 0.0) THEN
            IF (TDSUM > TDDC) TDSUM = TDDC
            TDRVOL   = TDRVOL  + TDSUM

            DO K = 3, 4
              OVRFC(K) = TDFRAC(K)*TDSUM
              SH2O(K)  = SH2O(K) - (OVRFC(K)/(ZLAYER(K) * 1000.0))
              SMC(K)   = SH2O(K) + SICE (K)
            END DO
         END IF

      ELSE IF (parameters%DRAIN_LAYER_OPT == 5) THEN ! from all the four layers

         !print*, "CASE = 5  Draining from all four layers"
         DO K = 1, 4
            AVFC(K)  = parameters%SMCREF(K) - SICE (K)
            OVRFC(K) = (SH2O(K) - (parameters%TDSMC_FAC*AVFC(K))) * ZLAYER(K) * 1000.0 ! mm
            IF(OVRFC(K) < 0.0) OVRFC(K) = 0.0
            TDSUM    = TDSUM + OVRFC(K)
         END DO
         DO K = 1, 4
            IF(OVRFC(K) .NE. 0.0) THEN
              TDFRAC(K)   = OVRFC(K)/TDSUM
            END IF
         END DO

         IF (TDSUM > 0.0) THEN
            IF (TDSUM > TDDC) TDSUM = TDDC
            TDRVOL   = TDRVOL  + TDSUM
            DO K = 1, 4
              OVRFC(K) = TDFRAC(K)*TDSUM
              SH2O(K)  = SH2O(K) - (OVRFC(K)/(ZLAYER(K) * 1000.0))
              SMC(K)   = SH2O(K) + SICE (K)
            END DO
         END IF
      END IF

    QTLDRN = TDRVOL / DT

  END SUBROUTINE TILE_DRAIN

!===================================================================================================

 SUBROUTINE TILE_HOOGHOUDT (parameters,WCND,NSOIL,NSNOW,SH2O,SMC,SICE, &
                            ZSOIL,DZSNSO,DT,DX,QTLDRN,ZWT              &
#ifdef WRF_HYDRO
                            ,WATBLED                                  &
#endif
                            )

!------------------------------------------------------------------------------------------
! calculate tile drainage discharge (mm) based on Hooghoudt's equation ! pvk
!------------------------------------------------------------------------------------------
  IMPLICIT NONE
!------------------------------------------------------------------------------------------
! Definitions
!TD_SATZ      = Thickness of saturated zone in layer considered ~ W
!TD_SPAC      = Tile Drain SPACing ~ SDRAIN
!TD_HAIL      = Height of water table in the drain Above Impermeable Layer (de)  -     
!                HDRAIN
!TD_HEMD      = Effective Height between water level in the drains to the water 
!               table MiDpoint  ~ EM
!ZLAYER(K)    = Thickness of layer K ~DZ(I)
!TD_DTWT      = Depth To Water Table (cm) ~ TD_DTWT
!TD_FLUX      = Drainge Flux cm/hr~ DFLUX
!TD_DEPTH     = Effective Depth to impermeable layer from soil surface~ DEPTH
!TD_TTSZ      = Total Thickness of Saturated Zone ~ DEEP
!KLAT         = Lateral hydraulic conductivity, CONE
!KLATK        = Lateral hydraulic conductivity of a specific layer ~ CONK
!DTOPL        = Depth of top of the layer considered~ ABOVE
!TD_DC        = Drainage Coefficient ~ DC
!TD_DDRN      = Depth of drain ~ DDRAIN
!TD_ADEPTH    = Actual depth to impermeable layer form surface. ADEPTH  >= TD_DEPTH         
!------------------------------------------------------------------------------------------
! inout
    type (noahmp_parameters), intent(in)        :: parameters
    INTEGER,INTENT(IN)                          :: NSOIL
    INTEGER,INTENT(IN)                          :: NSNOW
    REAL,   INTENT(IN)                          :: DT
    REAL,   INTENT(IN)                          :: DX
    REAL, DIMENSION(1:NSOIL), INTENT(IN)        :: SICE
    REAL, DIMENSION(1:NSOIL), INTENT(IN)        :: ZSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(IN)        :: DZSNSO
    REAL, DIMENSION(1:NSOIL), INTENT(IN)        :: WCND
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT)     :: SH2O
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT)     :: SMC
    REAL, INTENT(INOUT)                         :: QTLDRN  ! tile drain discharge mm/s
    REAL, INTENT(INOUT)                         :: ZWT     ! water table depth
! local 
    INTEGER                                     :: K
    REAL, DIMENSION(1:NSOIL)                    :: TD_SATZ ! thickness of saturated zone
    REAL, DIMENSION(1:NSOIL)                    :: KLATK   ! lateral hydraulic ocnductivity kth layer
    REAL                                        :: TD_TTSZ ! total satutation thickness
    REAL                                        :: TD_LQ   ! lateral flow
    REAL                                        :: DTOPL   ! depth to top of the layer
    REAL                                        :: XX
    REAL                                        :: YY
    REAL                                        :: KLAT    ! average lateral hydruaic conductivity
    REAL                                        :: TD_HAIL
    REAL                                        :: TD_DEPTH
    REAL                                        :: TD_HEMD
    REAL,DIMENSION(1:NSOIL)                     :: ZLAYER
    REAL,DIMENSION(1:NSOIL)                     :: OVRFC
    INTEGER                                     :: NDRAINS
    REAL                                        :: TDDC
    REAL,DIMENSION(1:NSOIL)                     :: RMSH2O
    REAL                                        :: QTLDRN1
    REAL                                        :: TD_DD
    REAL                                        :: OVRFCS
#ifdef WRF_HYDRO
    REAL                                        :: WATBLED ! water table depth estimated in WRF-Hydro fine grids
#endif
!----------------------------------------------------------------------------

    TD_SATZ = 0.0
    DTOPL   = 0.0
    TD_LQ   = 0.0
    TD_TTSZ = 0.0
    TDDC    = parameters%TD_DCOEF* 1000.0 * DT/(24.0 * 3600.0) ! m per day to mm per timestep

! Thickness of soil layers    
    DO K = 1, NSOIL
      IF (K == 1) THEN
         ZLAYER(K) = -1.0 * ZSOIL(K)
      ELSE
         ZLAYER(K) = (ZSOIL(K-1)-ZSOIL(K))
      END IF
    END DO

#ifdef WRF_HYDRO
! Depth to water table, m
    YY = WATBLED
#else
    CALL TD_FINDZWAT(parameters,NSOIL,SMC,SH2O,SICE,ZSOIL,ZLAYER,ZWT)
    !CALL ZWTEQ (parameters,NSOIL, NSNOW, ZSOIL, DZSNSO, SH2O, ZWT)
! Depth to water table, m
    YY = ZWT
#endif
    IF (YY .GT. parameters%TD_ADEPTH) YY = parameters%TD_ADEPTH

! Depth of saturated zone
    DO K=1, NSOIL
       IF (YY .GT. (-1.0*ZSOIL(K))) THEN
          TD_SATZ(K) = 0.0
       ELSE
          TD_SATZ(K) = (-1.0*ZSOIL(K)) - YY
          XX =  (-1.0*ZSOIL(K)) - DTOPL
          IF(TD_SATZ(K) .GT. XX) TD_SATZ(K) = XX
       END IF
       !print*,"K = ", K
       !print*,"-1*ZSOIL(K)=",-1*ZSOIL(K)
       !print*,"TD_SATZ(K)=",TD_SATZ(K)
       !print*,"DTOPL=",DTOPL
       DTOPL = -1.0*ZSOIL(K)
    END DO

! amount of water over field capacity
    OVRFCS = 0.0
    DO K=1, NSOIL
      OVRFC(K) = (SH2O(K) - (parameters%SMCREF(K)-SICE(K))) * ZLAYER(K) * 1000.0 !mm
      IF(OVRFC(K) .LT. 0.0) OVRFC(K) = 0.0
      OVRFCS   = OVRFCS + OVRFC(K)
    END DO

! lateral hydr. conductivity and total lateral flow
    DO K=1, NSOIL
       KLATK(K)= WCND(K)*parameters%KLAT_FAC * DT ! m/s to m/timestep
       TD_LQ   = TD_LQ + (TD_SATZ(K) * KLATK(K))
       TD_TTSZ = TD_TTSZ + TD_SATZ(K)
    END DO
    IF (TD_TTSZ .LT. 0.001) TD_TTSZ = 0.001 ! unit is m
    IF (TD_LQ   .LT. 0.001) TD_LQ   = 0.0    ! unit is m
    KLAT = TD_LQ/TD_TTSZ ! lateral hydraulic conductivity per timestep
    TD_DD = parameters%TD_ADEPTH - parameters%TD_DDRAIN

    CALL TD_EQUIVALENT_DEPTH (TD_DD,    &
                              parameters%TD_SPAC, &
                              parameters%TD_RADI, &
                              TD_HAIL)
    TD_DEPTH= TD_HAIL + parameters%TD_DDRAIN
    TD_HEMD = parameters%TD_DDRAIN - YY
    IF (TD_HEMD .LE. 0.0) THEN
       QTLDRN = 0.0
    ELSE
       QTLDRN = ((8.0*KLAT*TD_HAIL*TD_HEMD) + (4.0*KLAT*TD_HEMD*TD_HEMD))& ! m per timestep
                 /(parameters%TD_SPAC*parameters%TD_SPAC)
    END IF
    QTLDRN = QTLDRN * 1000.0 ! m per timestep to mm/timestep /one tile
    IF(QTLDRN .LE. 0.0) QTLDRN = 0.0
    IF(QTLDRN .GT. TDDC) QTLDRN = TDDC
    NDRAINS = INT(DX/parameters%TD_SPAC)
    QTLDRN = QTLDRN * NDRAINS
    IF(QTLDRN .GT. OVRFCS) QTLDRN = OVRFCS

! update soil moisture after drainage: moisture drains from top to bottom
    QTLDRN1 = QTLDRN
    DO K=1, NSOIL
      IF(QTLDRN1 .GT. 0.0) THEN
         IF((TD_SATZ(K) .GT. 0.0) .AND. (OVRFC(K) .GT. 0.0)) THEN
           RMSH2O(K) = OVRFC(K) - QTLDRN1 ! remaining water after tile drain
           IF (RMSH2O(K) .GT. 0.0) THEN
              SH2O(K) = (parameters%SMCREF(K) - SICE (K)) + RMSH2O(K)/(ZLAYER(K) * 1000.0)
              SMC(K)  = SH2O(K) + SICE (K)
              EXIT
           ELSE
              SH2O(K) = (parameters%SMCREF(K) - SICE (K))
              SMC(K)  = SH2O(K) + SICE (K)
              QTLDRN1 = QTLDRN1 - OVRFC(K)
           END IF
         END IF
      END IF
    END DO
    QTLDRN = QTLDRN/DT ![mm/s]

 END SUBROUTINE TILE_HOOGHOUDT

!----------------------------------------------------------------------- 

 SUBROUTINE TD_EQUIVALENT_DEPTH (TD_D,TD_L,TD_RD,TD_DE)

! ----------------------------------------------------------------------
! calculate tile drainage equivalent depth from d.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
 REAL, INTENT(IN)  :: TD_D
 REAL, INTENT(IN)  :: TD_L
 REAL, INTENT(IN)  :: TD_RD
 REAL, INTENT(OUT) :: TD_DE
 REAL              :: PII = 22.0/7.0
 REAL              :: TD_X
 REAL              :: TD_FX, EX,TERM
 INTEGER           :: I
!-------------------------------------

 TD_FX = 0.0
 EX     = 0.0
 TERM   = 0.0
 TD_X = (2.0*PII*TD_D)/TD_L
 IF (TD_X .GT. 0.5) THEN
    DO I=1,45,2
       EX     = EXP(-2.0*I*TD_X)
       TERM   = (4.0*EX)/(I*(1.0-EX))
       TD_FX = TD_FX + TERM
       IF(TERM .LT. 1.E-6) THEN
         TD_DE = ((PII*TD_L)/8.0)/(LOG(TD_L/(PII*TD_RD))+TD_FX)
         EXIT
       END IF
    END DO
 ELSE IF (TD_X .LT. 1.E-8) THEN
    TD_DE  = TD_D
 ELSE
    TD_FX = ((PII*PII)/(4.0*TD_X))+(LOG(TD_X/(2.0*PII)))
    TD_DE = ((PII*TD_L)/8.0)/(LOG(TD_L/(PII*TD_RD))+TD_FX)
 END IF
 IF (TD_DE .LT. 0. .AND. I .LE. 2) TD_DE = TD_D

 END SUBROUTINE TD_EQUIVALENT_DEPTH

!----------------------------------------------------------------------------

 SUBROUTINE TD_FINDZWAT(parameters,NSOIL,SMC,SH2O,SICE,ZSOIL,SLDPTH,WATBLED)

!----------------------------------------------------------------------------
! Calculate watertable depth as on WRF-Hydro/NWM
!----------------------------------------------------------------------------
  IMPLICIT NONE
!-------- DECLARATIONS ------------------------
    type (noahmp_parameters), intent(in)   :: parameters
    INTEGER, INTENT(IN)                    :: NSOIL
    REAL, INTENT(IN), DIMENSION(NSOIL)     :: SMC,SH2O,SICE
    REAL, INTENT(IN), DIMENSION(NSOIL)     :: ZSOIL
    REAL, INTENT(IN), DIMENSION(NSOIL)     :: SLDPTH
    REAL, INTENT(OUT)                      :: WATBLED
    REAL                                   :: CWATAVAIL
    INTEGER                                :: SATLYRCHK
! Local Variables
    INTEGER :: K,i,j
!------------------------------------------------------------

        SATLYRCHK = 0  !set flag for sat. layers
        CWATAVAIL = 0.0  !set wat avail for subsfc rtng = 0.
        DO K=NSOIL,1,-1
           IF ( (SMC(K).GE.parameters%SMCREF(K)).AND.&
           (parameters%SMCREF(K) .GT.parameters%SMCWLT(K)) ) THEN
                  IF((SATLYRCHK .EQ.K+1) .OR. (K.EQ.NSOIL)) SATLYRCHK = K
           END IF
        END DO
        IF (SATLYRCHK .NE. 0) THEN
            IF (SATLYRCHK .NE. 1) then  ! soil column is partially sat.
                WATBLED = -ZSOIL(SATLYRCHK-1)
            ELSE  ! soil column is fully saturated to sfc.
                WATBLED = 0.0
            END IF
            DO K = SATLYRCHK,NSOIL
               CWATAVAIL = CWATAVAIL+(SMC(K)-parameters%SMCREF(K))*SLDPTH(K)
            END DO
        ELSE  ! no saturated layers...
            WATBLED   = -ZSOIL(NSOIL)
            SATLYRCHK = NSOIL + 1
        END IF

 END SUBROUTINE TD_FINDZWAT

!== end tile drainage subroutines ==================================================================

!== begin wdfcnd1 ==================================================================================

  SUBROUTINE WDFCND1 (parameters,WDF,WCND,SMC,FCR,ISOIL)
! ----------------------------------------------------------------------
! calculate soil water diffusivity and soil hydraulic conductivity.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input 
  type (noahmp_parameters), intent(in) :: parameters
    REAL,INTENT(IN)  :: SMC
    REAL,INTENT(IN)  :: FCR
    INTEGER,INTENT(IN)  :: ISOIL

! output
    REAL,INTENT(OUT) :: WCND
    REAL,INTENT(OUT) :: WDF

! local
    REAL :: EXPON
    REAL :: FACTR
    REAL :: VKWGT
! ----------------------------------------------------------------------

! soil water diffusivity

    FACTR = MAX(0.01, SMC/parameters%SMCMAX(ISOIL))
    EXPON = parameters%BEXP(ISOIL) + 2.0
    WDF   = parameters%DWSAT(ISOIL) * FACTR ** EXPON
    WDF   = WDF * (1.0 - FCR)

! hydraulic conductivity

    EXPON = 2.0*parameters%BEXP(ISOIL) + 3.0
    WCND  = parameters%DKSAT(ISOIL) * FACTR ** EXPON
    WCND  = WCND * (1.0 - FCR)

  END SUBROUTINE WDFCND1

!== begin wdfcnd2 ==================================================================================

  SUBROUTINE WDFCND2 (parameters,WDF,WCND,SMC,SICE,ISOIL)
! ----------------------------------------------------------------------
! calculate soil water diffusivity and soil hydraulic conductivity.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
    REAL,INTENT(IN)  :: SMC
    REAL,INTENT(IN)  :: SICE
    INTEGER,INTENT(IN)  :: ISOIL

! output
    REAL,INTENT(OUT) :: WCND
    REAL,INTENT(OUT) :: WDF

! local
    REAL :: EXPON
    REAL :: FACTR1,FACTR2
    REAL :: VKWGT
! ----------------------------------------------------------------------

! soil water diffusivity

    FACTR1 = 0.05/parameters%SMCMAX(ISOIL)
    FACTR2 = MAX(0.01, SMC/parameters%SMCMAX(ISOIL))
    FACTR1 = MIN(FACTR1,FACTR2)
    EXPON = parameters%BEXP(ISOIL) + 2.0
    WDF   = parameters%DWSAT(ISOIL) * FACTR2 ** EXPON

    IF (SICE > 0.0) THEN
    VKWGT = 1.0/ (1.0 + (500.0* SICE)**3.0)
    WDF   = VKWGT * WDF + (1.0-VKWGT)*parameters%DWSAT(ISOIL)*(FACTR1)**EXPON
    END IF

! hydraulic conductivity

    EXPON = 2.0*parameters%BEXP(ISOIL) + 3.0
    WCND  = parameters%DKSAT(ISOIL) * FACTR2 ** EXPON

  END SUBROUTINE WDFCND2

!==========begin irrigation subroutines============================================================
  SUBROUTINE TRIGGER_IRRIGATION(parameters,NSOIL,ZSOIL,SH2O,FVEG,                   & !in
                                JULIAN,IRRFRA,LAI,                                  & !in
                                SIFAC,MIFAC,FIFAC,                                  & !in
                                IRCNTSI,IRCNTMI,IRCNTFI,                            & !inout
                                IRAMTSI,IRAMTMI,IRAMTFI)                              !inout
  !-----------------------------------------------------------------------------------------------
  ! This subroutine trigger irrigation if soil moisture less than the management allowable deficit 
  ! (MAD) and estimate irrigation water depth (m) using current rootzone soil moisture and field 
  ! capacity. There are two options here to trigger the irrigation scheme based on MAD
  ! OPT_IRR = 1 -> if irrigated fraction > threshold fraction
  ! OPT_IRR = 2 -> if irrigated fraction > threshold fraction and within crop season
  ! OPT_IRR = 3 -> if irrigated fraction > threshold fraction and LAI > threshold LAI
  ! Author: Prasanth Valayamkunnath (NCAR) <prasanth@ucar.edu>
  ! Date  : 08/06/2020
  !-----------------------------------------------------------------------------------------------
    IMPLICIT NONE
  ! ----------------------------------------------------------------------------------------------
    ! inputs
    type (noahmp_parameters), intent(in)   :: parameters
    INTEGER,                  INTENT(IN)   :: NSOIL          ! number of soil layers
    REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: ZSOIL          ! depth of layers from surface, [m]
    REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: SH2O           ! volumteric liquid water content [%]
    REAL,                     INTENT(IN)   :: FVEG           ! green vegetation fraction [-]
    REAL,                     INTENT(IN)   :: IRRFRA         ! irrigated area fraction [-]
    REAL,                     INTENT(IN)   :: LAI            ! leaf area index [m^2/m^2]
    REAL,                     INTENT(IN)   :: JULIAN         ! julian day
    REAL,                     INTENT(IN)   :: SIFAC          ! sprinkler irrigation fraction [-]
    REAL,                     INTENT(IN)   :: MIFAC          ! micro irrigation fraction [-]
    REAL,                     INTENT(IN)   :: FIFAC          ! flood irrigation fraction [-]
    ! inouts
    INTEGER,                  INTENT(INOUT):: IRCNTSI        ! irrigation event number, Sprinkler
    INTEGER,                  INTENT(INOUT):: IRCNTMI        ! irrigation event number, Micro
    INTEGER,                  INTENT(INOUT):: IRCNTFI        ! irrigation event number, Flood 
    REAL,                     INTENT(INOUT):: IRAMTSI        ! irrigation water amount [m] to be applied, Sprinkler
    REAL,                     INTENT(INOUT):: IRAMTMI        ! irrigation water amount [m] to be applied, Micro
    REAL,                     INTENT(INOUT):: IRAMTFI        ! irrigation water amount [m] to be applied, Flood
    ! local
    REAL                                   :: SMCAVL         ! available soil moisture [m] at timestep
    REAL                                   :: SMCLIM         ! maximum available moisture [m] (FC-PWD)
    REAL                                   :: SMCSAT         ! maximum saturation moisture [m] (POROSITY-FC)
    REAL                                   :: IRRWATAMT      ! irrigation water amount [m]
    LOGICAL                                :: IRR_ACTIVE     ! irrigation check
    INTEGER                                :: K
  !---------------------------------------------------------------------------------------------
    IRR_ACTIVE =  .TRUE.
   
    ! check if irrigation is can be activated or not
    IF(OPT_IRR .EQ. 2)THEN
      ! activate irrigation if within crop season
      IF ((JULIAN .LT. parameters%PLTDAY).OR.&
          (JULIAN .GT. (parameters%HSDAY - parameters%IRR_HAR))) IRR_ACTIVE = .FALSE.
    ELSE IF (OPT_IRR .EQ. 3) THEN
      
      ! activate if LAI > threshold LAI
      IF(LAI .LT. parameters%IRR_LAI) IRR_ACTIVE = .FALSE.
      
    ELSE IF ( (OPT_IRR .GT. 3) .OR. (OPT_IRR .LT. 1)) THEN
      IRR_ACTIVE = .FALSE.
    END IF
    IF(IRR_ACTIVE)THEN
      SMCAVL = 0.0
      SMCLIM = 0.0
      ! estimate available water and field capacity for the root zone
      SMCAVL = (SH2O(1)-parameters%SMCWLT(1))*-1.0*ZSOIL(1)              ! current soil water (m) 
      SMCLIM = (parameters%SMCREF(1)-parameters%SMCWLT(1))*-1.0*ZSOIL(1) ! available water (m)
      DO K = 2, parameters%NROOT
         SMCAVL = SMCAVL + (SH2O(K)-parameters%SMCWLT(K))*(ZSOIL(K-1) - ZSOIL(K))
         SMCLIM = SMCLIM + (parameters%SMCREF(K)-parameters%SMCWLT(K))*(ZSOIL(K-1) - ZSOIL(K))
      END DO
    
      ! check if root zone soil moisture < MAD
      IF((SMCAVL/SMCLIM) .LE. parameters%IRR_MAD) THEN 
         ! parameters%IRR_MAD- calibratable
         ! amount of water need to be added to bring soil moisture back to 
         ! field capacity, i.e., irrigation water amount (m)
         IRRWATAMT = (SMCLIM - SMCAVL)*IRRFRA*FVEG  
         ! sprinkler irrigation amount (m) based on 2D SIFAC
         IF((IRAMTSI .EQ. 0.0) .AND. (SIFAC .GT. 0.0) .AND. (OPT_IRRM .EQ. 0)) THEN
            IRAMTSI = SIFAC*IRRWATAMT 
            IRCNTSI = IRCNTSI + 1
         ! sprinkler irrigation amount (m) based on namelist choice
         ELSE IF ((IRAMTSI .EQ. 0.0) .AND. (OPT_IRRM .EQ. 1)) THEN
            IRAMTSI = IRRWATAMT
            IRCNTSI = IRCNTSI + 1
         END IF
         ! micro irrigation amount (m) based on 2D MIFAC
         IF((IRAMTMI .EQ. 0.0) .AND. (MIFAC .GT. 0.0) .AND. (OPT_IRRM .EQ. 0)) THEN
            IRAMTMI = MIFAC*IRRWATAMT
            IRCNTMI = IRCNTMI + 1
         ! micro irrigation amount (m) based on namelist choice
         ELSE IF ((IRAMTMI .EQ. 0.0) .AND. (OPT_IRRM .EQ. 2)) THEN
            IRAMTMI = IRRWATAMT 
            IRCNTMI = IRCNTMI + 1
         END IF
         ! flood irrigation amount (m): Assumed to saturate top two layers and 
         ! third layer to FC. As water moves from one end of the field to
         ! another, surface layers will be saturated. 
         ! flood irrigation amount (m) based on 2D FIFAC
         IF((IRAMTFI .EQ. 0.0) .AND. (FIFAC .GT. 0.0) .AND. (OPT_IRRM .EQ. 0)) THEN
            IRAMTFI = FIFAC*(IRRWATAMT)*(parameters%FILOSS+1)
            IRCNTFI = IRCNTFI + 1
         !flood irrigation amount (m) based on namelist choice
         ELSE IF((IRAMTFI .EQ. 0.0) .AND. (OPT_IRRM .EQ. 3)) THEN
            IRAMTFI = (IRRWATAMT)*(parameters%FILOSS+1)
            IRCNTFI = IRCNTFI + 1
         END IF  
      ELSE
         IRRWATAMT = 0.0
         IRAMTSI   = 0.0
         IRAMTMI   = 0.0
         IRAMTFI   = 0.0
      END IF
    END IF   
  END SUBROUTINE TRIGGER_IRRIGATION
  
 !============================================================================================================
 
  SUBROUTINE SPRINKLER_IRRIGATION(parameters,NSOIL,DT,SH2O,SMC,SICE,& !in
                                  T2,WINDU,WINDV,EAIR,SIFAC,        & !in
                                  IRAMTSI,IREVPLOS,IRSIRATE)          !inout
  !---------------------------------------------------------------------------------------------
  ! This subroutine estimate irrigation water depth (m) based on sprinkler method defined in
  ! chapter 11 of NRCS, Part 623 National Engineering Handbook. Irrigation water will be applied 
  ! over the canopy considering, present soil moisture, infiltration rate of the soil, and 
  ! evaporative loss. This subroutine will be called before CANWAT subroutine to estimate them
  ! canopy water storage loss. 
  ! Author: Prasanth Valayamkunnath (NCAR) <prasanth@ucar.edu>
  ! Date  : 08/06/2020
  !---------------------------------------------------------------------------------------------
    IMPLICIT NONE
  ! --------------------------------------------------------------------------------------------
    ! inputs
    type (noahmp_parameters), intent(in)    :: parameters
    INTEGER,                  INTENT(IN)    :: NSOIL
    REAL,                     INTENT(IN)    :: DT
    REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: SH2O
    REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: SMC
    REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: SICE
    REAL,                     INTENT(IN)    :: T2
    REAL,                     INTENT(IN)    :: WINDU
    REAL,                     INTENT(IN)    :: WINDV
    REAL,                     INTENT(IN)    :: EAIR
    REAL,                     INTENT(IN)    :: SIFAC       ! sprinkler irrigation fraction
    !inouts
    REAL,                     INTENT(INOUT) :: IRAMTSI     !total irrigation water amount [m] during this schedule
    REAL,                     INTENT(INOUT) :: IREVPLOS    !loss of irrigation water to evaporation,sprinkler [m/timestep]
    REAL,                     INTENT(INOUT) :: IRSIRATE    !rate of irrigation by sprinkler [m/timestep]
    ! local
    REAL                                    :: FSUR        !infiltration rate [m/s]
    REAL                                    :: TEMP_RATE
    REAL                                    :: WINDSPEED
    REAL                                    :: IRRLOSS     !temporary var for irr loss [%]
    REAL                                    :: ESAT1
    !-------------------------------------------------------------------------------------------    
    ! estimate infiltration rate based on Philips Eq.
    CALL IRR_PHILIP_INFIL(parameters,SMC,SH2O,SICE,DT,NSOIL,FSUR)           
    ! irrigation rate of sprinkler
    TEMP_RATE = parameters%SPRIR_RATE*(1/1000.0)*DT/3600.0         !NRCS rate/time step - calibratable
    IRSIRATE  = MIN(FSUR*DT,IRAMTSI,TEMP_RATE)                   !Limit the application rate to minimum of infiltration rate
                                                                 !and to the NRCS recommended rate, (m)
    
    ! evaporative loss from droplets: Based on Bavi et al., (2009). Evaporation 
    ! losses from sprinkler irrigation systems under various operating 
    ! conditions. Journal of Applied Sciences, 9(3), 597-600.
    WINDSPEED = SQRT((WINDU**2.0)+(WINDV**2.0))                                 ! [m/s]
    ESAT1     = 610.8*EXP((17.27*(T2-273.15))/(237.3+(T2-273.15)))              ! [Pa]
    IF(T2 .GT. 273.15)THEN ! Equation (3)
       IRRLOSS   = 4.375*(EXP(0.106*WINDSPEED))*(((ESAT1-EAIR)*0.01)**(-0.092))*((T2-273.15)**(-0.102)) ! [%]
    ELSE ! Equation (4)
       IRRLOSS   = 4.337*(EXP(0.077*WINDSPEED))*(((ESAT1-EAIR)*0.01)**(-0.098)) ! [%]
    END IF
    ! Old PGI Fortran compiler does not support ISNAN
    IF ( isnan_lsm(IRRLOSS) ) IRRLOSS=4.0 ! In case if IRRLOSS is NaN
    IF ( (IRRLOSS .GT. 100.0) .OR. (IRRLOSS .LT. 0.0) ) IRRLOSS=4.0 ! In case if IRRLOSS is out of range
 
    ! Sprinkler water (m) for sprinkler fraction 
    IRSIRATE  = IRSIRATE * SIFAC
    IF(IRSIRATE .GE. IRAMTSI)THEN
       IRSIRATE = IRAMTSI
       IRAMTSI  = 0.0
    ELSE 
       IRAMTSI = IRAMTSI - IRSIRATE
    END IF
    IREVPLOS = IRSIRATE*IRRLOSS*(1.0/100.0) 
    IRSIRATE = IRSIRATE-IREVPLOS
  END SUBROUTINE SPRINKLER_IRRIGATION

  logical function isnan_lsm(arg1)
       real,intent(in) :: arg1
       isnan_lsm = (arg1 .ne. arg1)
       return
  end function isnan_lsm
  !============================================================================================================


  !============================================================================================================

  SUBROUTINE MICRO_IRRIGATION(parameters,NSOIL,DT,SH2O,SMC,SICE,MIFAC, & !in
                              IRAMTMI,IRMIRATE)                          !inout
  !---------------------------------------------------------------------------------------------
  ! This subroutine estimate irrigation water depth (m) based on Micro irrigation method defined 
  ! in chapter 7 of NRCS, Part 623 National Engineering Handbook. Irrigation water will be applied 
  ! under the canopy, within first layer (at ~5 cm depth) considering current soil moisture. 
  ! This subroutine will be called after CANWAT. 
  ! Author: Prasanth Valayamkunnath (NCAR) <prasanth@ucar.edu>
  ! Date  : 08/06/2020
  !---------------------------------------------------------------------------------------------
    IMPLICIT NONE
  ! --------------------------------------------------------------------------------------------
    ! inputs
    type (noahmp_parameters), intent(in)    :: parameters
    INTEGER,                  INTENT(IN)    :: NSOIL
    REAL,                     INTENT(IN)    :: DT
    REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: SH2O
    REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: SMC
    REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: SICE
    REAL,                     INTENT(IN)    :: MIFAC     ! micro irrigation fraction
    ! inout
    REAL,                     INTENT(INOUT) :: IRAMTMI   !irrigation water amount [m]
    REAL,                     INTENT(INOUT) :: IRMIRATE  !rate of irrigation by micro [m/time step]
    ! local
    REAL                                    :: FSUR      !infiltration rate [m/s]
    REAL                                    :: TEMP_RATE
  !-----------------------------------------------------------------------------------------------------
    ! estimate infiltration rate based on Philips Eq.
    CALL IRR_PHILIP_INFIL(parameters,SMC,SH2O,SICE,DT,NSOIL,FSUR)
    ! irrigation rate of micro irrigation
    TEMP_RATE = parameters%MICIR_RATE*(1.0/1000.0)*DT/3600.0        !NRCS rate/time step - calibratable
    IRMIRATE  = MIN(0.5*FSUR*DT,IRAMTMI,TEMP_RATE)               !Limit the application rate to minimum 
                                                                 !of 0.5*infiltration rate
                                                                 !and to the NRCS recommended rate, (m)
    IRMIRATE  = IRMIRATE * MIFAC
    IF(IRMIRATE .GE. IRAMTMI)THEN
       IRMIRATE  = IRAMTMI
       IRAMTMI = 0.0
    ELSE
       IRAMTMI = IRAMTMI - IRMIRATE
    END IF
  END SUBROUTINE MICRO_IRRIGATION
 !============================================================================================================

  SUBROUTINE FLOOD_IRRIGATION(parameters,NSOIL,DT,SH2O,SMC,SICE,FIFAC,& !in
                              IRAMTFI,IRFIRATE)                         !inout
  !---------------------------------------------------------------------------------------------
  ! This subroutine estimate irrigation water depth (m) based on surface flooding irrigation method 
  ! defined in chapter 4 of NRCS, Part 623 National Engineering Handbook. Irrigation water will 
  ! be applied on the surface based on present soil moisture and infiltration rate of the soil. 
  ! This subroutine will be called after CANWAT subroutine to estimate them. Flooding or overland
  ! flow is based on infiltration excess! 
  ! Author: Prasanth Valayamkunnath (NCAR) <prasanth@ucar.edu>
  ! Date  : 08/06/2020
  !---------------------------------------------------------------------------------------------
    IMPLICIT NONE
  ! --------------------------------------------------------------------------------------------
    ! inputs
    type (noahmp_parameters), intent(in)    :: parameters
    INTEGER,                  INTENT(IN)    :: NSOIL
    REAL,                     INTENT(IN)    :: DT
    REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: SH2O
    REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: SMC
    REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: SICE
    REAL,                     INTENT(IN)    :: FIFAC     !fraction of grid under micro irrigation(0 to 1)
    ! inout
    REAL,                     INTENT(INOUT) :: IRAMTFI   !irrigation water amount [m]
    REAL,                     INTENT(INOUT) :: IRFIRATE  !irrigation water rate by micro [m/timestep]
    ! local
    REAL                                    :: FSUR      !infiltration rate [m/s]
    REAL                                    :: TEMP_RATE
  !-----------------------------------------------------------------------------------------------------
    ! estimate infiltration rate based on Philips Eq.
    CALL IRR_PHILIP_INFIL(parameters,SMC,SH2O,SICE,DT,NSOIL,FSUR)
    ! irrigation rate of flood irrigation. It should be
    ! greater than infiltration rate to get infiltration
    ! excess runoff at the time of application
    IRFIRATE  = FSUR*DT*parameters%FIRTFAC                !Limit the application rate to  
                                                          !fac*infiltration rate 
    IRFIRATE  = IRFIRATE * FIFAC
    IF(IRFIRATE .GE. IRAMTFI)THEN
       IRFIRATE  = IRAMTFI
       IRAMTFI = 0.0
    ELSE
       IRAMTFI = IRAMTFI - IRFIRATE
    END IF

  END SUBROUTINE FLOOD_IRRIGATION

 !============================================================================================================
  SUBROUTINE IRR_PHILIP_INFIL(parameters,SMC,SH2O,SICE,DT,NSOIL, & ! in
                              FSUR)                                ! out
  !---------------------------------------------------------------------------------------------
  ! This function estimate infiltration rate based on Philip's two parameter equation (Eq. 2) 
  ! presented in Valiantzas (2010). New linearized two-parameter infiltration equation for direct
  ! determination of conductivity and sorptivity, J. Hydrology.
  ! Author: Prasanth Valayamkunnath (NCAR) <prasanth@ucar.edu>
  ! Date  : 08/06/2020
  !---------------------------------------------------------------------------------------------
   IMPLICIT NONE
  ! --------------------------------------------------------------------------------------------
   type (noahmp_parameters), intent(in) :: parameters
   INTEGER,                  INTENT(IN) :: NSOIL  !number of soil layers
   REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SMC    !soil moisture content [m3/m3]
   REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SH2O   !soil water content [m3/m3]
   REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SICE   !soil ice content [m3/m3]
   REAL,                     INTENT(IN) :: DT     !time-step (sec)

  ! outputs
   REAL,                     INTENT(OUT):: FSUR   !surface infiltration rate (m/s)
  ! local variables
   REAL                                 :: WDF    !soil water diffusivity (m2/s)
   REAL                                 :: WCND   !soil water conductivity[m/s]
   REAL                                 :: SP     !sorptivity (LT^-1/2)
   REAL                                 :: AP     !intial hydraulic conductivity (m/s,L/T)
   REAL                                 :: SICEMAX
   INTEGER                              :: ISOIL,K
  !---------------------------------------------------------------------------------
  ! maximum ice fraction
    SICEMAX = 0.0
    DO K = 1,NSOIL
       IF (SICE(K) > SICEMAX) SICEMAX = SICE(K)
    END DO

  ! estimate initial soil hydraulic conductivty and diffusivity (Ki, D(theta) in the equation)
   ISOIL = 1
   CALL WDFCND2 (parameters,WDF,WCND,SH2O(ISOIL),SICEMAX,ISOIL)

  ! sorptivity based on Eq. 10b from Kutilek, Miroslav, and Jana Valentova (1986) 
  ! sorptivity approximations. Transport in Porous Media 1.1, 57-62.
   SP = SQRT(2.0 * max(0.0,(parameters%SMCMAX(ISOIL) - SMC(ISOIL))) * (parameters%DWSAT(ISOIL) - WDF))

  ! parameter A in Eq. 9 of Valiantzas (2010) is given by
   AP = MIN(WCND,(2.0/3.0)*parameters%DKSAT(ISOIL))
   AP = MAX(AP,(1.0/3.0)*parameters%DKSAT(ISOIL))

  ! maximun infiltration rate, m
   FSUR = 0.5*SP*((DT)**(-0.5))+AP ! m/s
   FSUR = MAX(0.0,FSUR)
   !FSUR = MIN(WCND,FSUR)
  END SUBROUTINE IRR_PHILIP_INFIL

!=========end irrigation subroutines================================================================
!== begin groundwater ==============================================================================

  SUBROUTINE GROUNDWATER(parameters,NSNOW  ,NSOIL  ,DT     ,SICE   ,ZSOIL  , & !in
                         STC    ,WCND   ,FCRMAX ,ILOC   ,JLOC   , & !in
                         SH2O   ,ZWT    ,WA     ,WT     ,         & !inout
                         QIN    ,QDIS   )                           !out
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                         INTENT(IN) :: ILOC  !grid index
  INTEGER,                         INTENT(IN) :: JLOC  !grid index
  INTEGER,                         INTENT(IN) :: NSNOW !maximum no. of snow layers
  INTEGER,                         INTENT(IN) :: NSOIL !no. of soil layers
  REAL,                            INTENT(IN) :: DT    !timestep [sec]
  REAL,                            INTENT(IN) :: FCRMAX!maximum FCR (-)
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SICE  !soil ice content [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL !depth of soil layer-bottom [m]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: WCND  !hydraulic conductivity (m/s)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC   !snow/soil temperature (k)

! input and output
  REAL, DIMENSION(    1:NSOIL), INTENT(INOUT) :: SH2O  !liquid soil water [m3/m3]
  REAL,                         INTENT(INOUT) :: ZWT   !the depth to water table [m]
  REAL,                         INTENT(INOUT) :: WA    !water storage in aquifer [mm]
  REAL,                         INTENT(INOUT) :: WT    !water storage in aquifer 
                                                           !+ saturated soil [mm]
! output
  REAL,                           INTENT(OUT) :: QIN   !groundwater recharge [mm/s]
  REAL,                           INTENT(OUT) :: QDIS  !groundwater discharge [mm/s]

! local
  REAL                                        :: FFF   !runoff decay factor (m-1)
  REAL                                        :: RSBMX !baseflow coefficient [mm/s]
  INTEGER                                     :: IZ    !do-loop index
  INTEGER                                     :: IWT   !layer index above water table layer
  REAL,  DIMENSION(    1:NSOIL)               :: DZMM  !layer thickness [mm]
  REAL,  DIMENSION(    1:NSOIL)               :: ZNODE !node depth [m]
  REAL,  DIMENSION(    1:NSOIL)               :: MLIQ  !liquid water mass [kg/m2 or mm]
  REAL,  DIMENSION(    1:NSOIL)               :: EPORE !effective porosity [-]
  REAL,  DIMENSION(    1:NSOIL)               :: HK    !hydraulic conductivity [mm/s]
  REAL,  DIMENSION(    1:NSOIL)               :: SMC   !total soil water  content [m3/m3]
  REAL(KIND=8)                                :: S_NODE!degree of saturation of IWT layer
  REAL                                        :: DZSUM !cumulative depth above water table [m]
  REAL                                        :: SMPFZ !matric potential (frozen effects) [mm]
  REAL                                        :: KA    !aquifer hydraulic conductivity [mm/s]
  REAL                                        :: WH_ZWT!water head at water table [mm]
  REAL                                        :: WH    !water head at layer above ZWT [mm]
  REAL                                        :: WS    !water used to fill air pore [mm]
  REAL                                        :: WTSUB !sum of HK*DZMM
  REAL                                        :: WATMIN!minimum soil vol soil moisture [m3/m3]
  REAL                                        :: XS    !excessive water above saturation [mm]
  REAL, PARAMETER                             :: ROUS = 0.2    !specific yield [-]
  REAL, PARAMETER                             :: CMIC = 0.20   !microprore content (0.0-1.0)
                                                               !0.0-close to free drainage
! -------------------------------------------------------------
      QDIS      = 0.0
      QIN       = 0.0

! Derive layer-bottom depth in [mm]
!KWM:  Derive layer thickness in mm

      DZMM(1) = -ZSOIL(1)*1.E3
      DO IZ = 2, NSOIL
         DZMM(IZ)  = 1.E3 * (ZSOIL(IZ - 1) - ZSOIL(IZ))
      ENDDO

! Derive node (middle) depth in [m]
!KWM:  Positive number, depth below ground surface in m
      ZNODE(1) = -ZSOIL(1) / 2.0
      DO IZ = 2, NSOIL
         ZNODE(IZ)  = -ZSOIL(IZ-1) + 0.5 * (ZSOIL(IZ-1) - ZSOIL(IZ))
      ENDDO

! Convert volumetric soil moisture "sh2o" to mass

      DO IZ = 1, NSOIL
         SMC(IZ)      = SH2O(IZ) + SICE(IZ)
         MLIQ(IZ)     = SH2O(IZ) * DZMM(IZ)
         EPORE(IZ)    = MAX(0.01,parameters%SMCMAX(IZ) - SICE(IZ))
         HK(IZ)       = 1.E3*WCND(IZ)
      ENDDO

! The layer index of the first unsaturated layer,
! i.e., the layer right above the water table

      IWT = NSOIL
      DO IZ = 2,NSOIL
         IF(ZWT   .LE. -ZSOIL(IZ) ) THEN
            IWT = IZ-1
            EXIT
         END IF
      ENDDO

! Groundwater discharge [mm/s]

      FFF   = 6.0
      RSBMX = 5.0

      QDIS = (1.0-FCRMAX)*RSBMX*EXP(-parameters%TIMEAN)*EXP(-FFF*(ZWT-2.0))

! Matric potential at the layer above the water table

      S_NODE = MIN(1.0,SMC(IWT)/parameters%SMCMAX(IWT) )
      S_NODE = MAX(S_NODE,REAL(0.01,KIND=8))
      SMPFZ  = -parameters%PSISAT(IWT)*1000.0*S_NODE**(-parameters%BEXP(IWT))   ! m --> mm
      SMPFZ  = MAX(-120000.0,CMIC*SMPFZ)

! Recharge rate qin to groundwater

      KA  = HK(IWT)

      WH_ZWT  = - ZWT * 1.E3                          !(mm)
      WH      = SMPFZ  - ZNODE(IWT)*1.E3              !(mm)
      QIN     = - KA * (WH_ZWT-WH)  /((ZWT-ZNODE(IWT))*1.E3)
      QIN     = MAX(-10.0/DT,MIN(10.0/DT,QIN))

! Water storage in the aquifer + saturated soil

      WT  = WT + (QIN - QDIS) * DT     !(mm)

      IF(IWT.EQ.NSOIL) THEN
         WA          = WA + (QIN - QDIS) * DT     !(mm)
         WT          = WA
         ZWT         = (-ZSOIL(NSOIL) + 25.0) - WA/1000.0/ROUS      !(m)
         MLIQ(NSOIL) = MLIQ(NSOIL) - QIN * DT        ! [mm]

         MLIQ(NSOIL) = MLIQ(NSOIL) + MAX(0.,(WA - 5000.0))
         WA          = MIN(WA, 5000.0)
      ELSE

         IF (IWT.EQ.NSOIL-1) THEN
            ZWT = -ZSOIL(NSOIL)                   &
                 - (WT-ROUS*1000.0*25.0) / (EPORE(NSOIL))/1000.0
         ELSE
            WS = 0.   ! water used to fill soil air pores
            DO IZ = IWT+2,NSOIL
               WS = WS + EPORE(IZ) * DZMM(IZ)
            ENDDO
            ZWT = -ZSOIL(IWT+1)                  &
                  - (WT-ROUS*1000.0*25.0-WS) /(EPORE(IWT+1))/1000.0
         ENDIF

         WTSUB = 0.0
         DO IZ = 1, NSOIL
           WTSUB = WTSUB + HK(IZ)*DZMM(IZ)
         END DO

         DO IZ = 1, NSOIL           ! Removing subsurface runoff
         MLIQ(IZ) = MLIQ(IZ) - QDIS*DT*HK(IZ)*DZMM(IZ)/WTSUB
         END DO
      END IF

      ZWT = MAX(1.5,ZWT)

!
! Limit MLIQ to be greater than or equal to watmin.
! Get water needed to bring MLIQ equal WATMIN from lower layer.
!
      WATMIN = 0.01
      DO IZ = 1, NSOIL-1
          IF (MLIQ(IZ) .LT. 0.0) THEN
             XS = WATMIN-MLIQ(IZ)
          ELSE
             XS = 0.0
          END IF
          MLIQ(IZ  ) = MLIQ(IZ  ) + XS
          MLIQ(IZ+1) = MLIQ(IZ+1) - XS
      END DO

        IZ = NSOIL
        IF (MLIQ(IZ) .LT. WATMIN) THEN
           XS = WATMIN-MLIQ(IZ)
        ELSE
           XS = 0.0
        END IF
        MLIQ(IZ) = MLIQ(IZ) + XS
        WA       = WA - XS
        WT       = WT - XS

      DO IZ = 1, NSOIL
        SH2O(IZ)     = MLIQ(IZ) / DZMM(IZ)
      END DO

  END SUBROUTINE GROUNDWATER

!== begin shallowwatertable ========================================================================

  SUBROUTINE SHALLOWWATERTABLE (parameters,NSNOW  ,NSOIL  ,ZSOIL, DT    , & !in
                         DZSNSO ,SMCEQ ,ILOC   ,JLOC         , & !in
                         SMC    ,WTD   ,SMCWTD ,RECH, QDRAIN  )  !inout
! ----------------------------------------------------------------------
!Diagnoses water table depth and computes recharge when the water table is within the resolved soil layers,
!according to the Miguez-Macho&Fan scheme
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                         INTENT(IN) :: NSNOW !maximum no. of snow layers
  INTEGER,                         INTENT(IN) :: NSOIL !no. of soil layers
  INTEGER,                         INTENT(IN) :: ILOC,JLOC
  REAL,                            INTENT(IN) :: DT
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL !depth of soil layer-bottom [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO ! snow/soil layer thickness [m]
  REAL,  DIMENSION(      1:NSOIL), INTENT(IN) :: SMCEQ  !equilibrium soil water  content [m3/m3]

! input and output
  REAL,  DIMENSION(      1:NSOIL), INTENT(INOUT) :: SMC   !total soil water  content [m3/m3]
  REAL,                         INTENT(INOUT) :: WTD   !the depth to water table [m]
  REAL,                         INTENT(INOUT) :: SMCWTD   !soil moisture between bottom of the soil and the water table [m3/m3]
  REAL,                         INTENT(OUT) :: RECH ! groundwater recharge (net vertical flux across the water table), positive up
  REAL,                         INTENT(INOUT) :: QDRAIN

! local
  INTEGER                                     :: IZ    !do-loop index
  INTEGER                                     :: IWTD   !layer index above water table layer
  INTEGER                                     :: KWTD   !layer index where the water table layer is
  REAL                                        :: WTDOLD
  REAL                                        :: DZUP
  REAL                                        :: SMCEQDEEP
  REAL,  DIMENSION(       0:NSOIL)            :: ZSOIL0
! -------------------------------------------------------------


ZSOIL0(1:NSOIL) = ZSOIL(1:NSOIL)
ZSOIL0(0) = 0.0

!find the layer where the water table is
     DO IZ=NSOIL,1,-1
        IF(WTD + 1.E-6 < ZSOIL0(IZ)) EXIT
     ENDDO
        IWTD=IZ


        KWTD=IWTD+1  !layer where the water table is
        IF(KWTD.LE.NSOIL)THEN    !wtd in the resolved layers
           WTDOLD=WTD
           IF(SMC(KWTD).GT.SMCEQ(KWTD))THEN

               IF(SMC(KWTD).EQ.parameters%SMCMAX(KWTD))THEN !wtd went to the layer above
                      WTD=ZSOIL0(IWTD)
                      RECH=-(WTDOLD-WTD) * (parameters%SMCMAX(KWTD)-SMCEQ(KWTD))
                      IWTD=IWTD-1
                      KWTD=KWTD-1
                   IF(KWTD.GE.1)THEN
                      IF(SMC(KWTD).GT.SMCEQ(KWTD))THEN
                      WTDOLD=WTD
                      WTD = MIN( ( SMC(KWTD)*DZSNSO(KWTD) &
                        - SMCEQ(KWTD)*ZSOIL0(IWTD) + parameters%SMCMAX(KWTD)*ZSOIL0(KWTD) ) / &
                        ( parameters%SMCMAX(KWTD)-SMCEQ(KWTD) ), ZSOIL0(IWTD))
                      RECH=RECH-(WTDOLD-WTD) * (parameters%SMCMAX(KWTD)-SMCEQ(KWTD))
                      ENDIF
                   ENDIF
               ELSE  !wtd stays in the layer
                      WTD = MIN( ( SMC(KWTD)*DZSNSO(KWTD) &
                        - SMCEQ(KWTD)*ZSOIL0(IWTD) + parameters%SMCMAX(KWTD)*ZSOIL0(KWTD) ) / &
                        ( parameters%SMCMAX(KWTD)-SMCEQ(KWTD) ), ZSOIL0(IWTD))
                      RECH=-(WTDOLD-WTD) * (parameters%SMCMAX(KWTD)-SMCEQ(KWTD))
               ENDIF

           ELSE    !wtd has gone down to the layer below
               WTD=ZSOIL0(KWTD)
               RECH=-(WTDOLD-WTD) * (parameters%SMCMAX(KWTD)-SMCEQ(KWTD))
               KWTD=KWTD+1
               IWTD=IWTD+1
!wtd crossed to the layer below. Now adjust it there
               IF(KWTD.LE.NSOIL)THEN
                   WTDOLD=WTD
                   IF(SMC(KWTD).GT.SMCEQ(KWTD))THEN
                   WTD = MIN( ( SMC(KWTD)*DZSNSO(KWTD) &
                   - SMCEQ(KWTD)*ZSOIL0(IWTD) + parameters%SMCMAX(KWTD)*ZSOIL0(KWTD) ) / &
                       ( parameters%SMCMAX(KWTD)-SMCEQ(KWTD) ) , ZSOIL0(IWTD) )
                   ELSE
                   WTD=ZSOIL0(KWTD)
                   ENDIF
                   RECH = RECH - (WTDOLD-WTD) * &
                                 (parameters%SMCMAX(KWTD)-SMCEQ(KWTD))

                ELSE
                   WTDOLD=WTD
!restore smoi to equilibrium value with water from the ficticious layer below
!                   SMCWTD=SMCWTD-(SMCEQ(NSOIL)-SMC(NSOIL))
!                   QDRAIN = QDRAIN - 1000 * (SMCEQ(NSOIL)-SMC(NSOIL)) * DZSNSO(NSOIL) / DT
!                   SMC(NSOIL)=SMCEQ(NSOIL)
!adjust wtd in the ficticious layer below
                   SMCEQDEEP = parameters%SMCMAX(NSOIL) * ( -parameters%PSISAT(NSOIL) / ( -parameters%PSISAT(NSOIL) - DZSNSO(NSOIL) ) ) ** (1.0/parameters%BEXP(NSOIL))
                   WTD = MIN( ( SMCWTD*DZSNSO(NSOIL) &
                   - SMCEQDEEP*ZSOIL0(NSOIL) + parameters%SMCMAX(NSOIL)*(ZSOIL0(NSOIL)-DZSNSO(NSOIL)) ) / &
                       ( parameters%SMCMAX(NSOIL)-SMCEQDEEP ) , ZSOIL0(NSOIL) )
                   RECH = RECH - (WTDOLD-WTD) * &
                                 (parameters%SMCMAX(NSOIL)-SMCEQDEEP)
                ENDIF

            ENDIF
        ELSEIF(WTD.GE.ZSOIL0(NSOIL)-DZSNSO(NSOIL))THEN
!if wtd was already below the bottom of the resolved soil crust
           WTDOLD=WTD
           SMCEQDEEP = parameters%SMCMAX(NSOIL) * ( -parameters%PSISAT(NSOIL) / ( -parameters%PSISAT(NSOIL) - DZSNSO(NSOIL) ) ) ** (1.0/parameters%BEXP(NSOIL))
           IF(SMCWTD.GT.SMCEQDEEP)THEN
               WTD = MIN( ( SMCWTD*DZSNSO(NSOIL) &
                 - SMCEQDEEP*ZSOIL0(NSOIL) + parameters%SMCMAX(NSOIL)*(ZSOIL0(NSOIL)-DZSNSO(NSOIL)) ) / &
                     ( parameters%SMCMAX(NSOIL)-SMCEQDEEP ) , ZSOIL0(NSOIL) )
               RECH = -(WTDOLD-WTD) * (parameters%SMCMAX(NSOIL)-SMCEQDEEP)
           ELSE
               RECH = -(WTDOLD-(ZSOIL0(NSOIL)-DZSNSO(NSOIL))) * (parameters%SMCMAX(NSOIL)-SMCEQDEEP)
               WTDOLD=ZSOIL0(NSOIL)-DZSNSO(NSOIL)
!and now even further down
               DZUP=(SMCEQDEEP-SMCWTD)*DZSNSO(NSOIL)/(parameters%SMCMAX(NSOIL)-SMCEQDEEP)
               WTD=WTDOLD-DZUP
               RECH = RECH - (parameters%SMCMAX(NSOIL)-SMCEQDEEP)*DZUP
               SMCWTD=SMCEQDEEP
           ENDIF


         ENDIF

IF(IWTD.LT.NSOIL .AND. IWTD.GT.0) THEN
  SMCWTD=parameters%SMCMAX(IWTD)
ELSEIF(IWTD.LT.NSOIL .AND. IWTD.LE.0) THEN
  SMCWTD=parameters%SMCMAX(1)
END IF

END  SUBROUTINE SHALLOWWATERTABLE

! ==================================================================================================
! ********************* end of water subroutines ******************************************
! ==================================================================================================

!== begin noahmp_options ===========================================================================

  subroutine noahmp_options(idveg     ,iopt_crs  ,iopt_btr  ,iopt_run  ,iopt_sfc  ,iopt_frz , &
                             iopt_inf  ,iopt_rad  ,iopt_alb  ,iopt_snf  ,iopt_tbot, iopt_stc, &
                             iopt_rsf , iopt_soil, iopt_pedo, iopt_crop, iopt_irr, iopt_irrm, &
                             iopt_infdv,iopt_tdrn)

  implicit none

  INTEGER,  INTENT(IN) :: idveg     !dynamic vegetation (1 -> off ; 2 -> on) with opt_crs = 1
  INTEGER,  INTENT(IN) :: iopt_crs  !canopy stomatal resistance (1-> Ball-Berry; 2->Jarvis)
  INTEGER,  INTENT(IN) :: iopt_btr  !soil moisture factor for stomatal resistance (1-> Noah; 2-> CLM; 3-> SSiB)
  INTEGER,  INTENT(IN) :: iopt_run  !runoff and groundwater (1->SIMGM; 2->SIMTOP; 3->Schaake96; 4->BATS)
  INTEGER,  INTENT(IN) :: iopt_sfc  !surface layer drag coeff (CH & CM) (1->M-O; 2->Chen97)
  INTEGER,  INTENT(IN) :: iopt_frz  !supercooled liquid water (1-> NY06; 2->Koren99)
  INTEGER,  INTENT(IN) :: iopt_inf  !frozen soil permeability (1-> NY06; 2->Koren99)
  INTEGER,  INTENT(IN) :: iopt_rad  !radiation transfer (1->gap=F(3D,cosz); 2->gap=0; 3->gap=1-Fveg)
  INTEGER,  INTENT(IN) :: iopt_alb  !snow surface albedo (1->BATS; 2->CLASS)
  INTEGER,  INTENT(IN) :: iopt_snf  !rainfall & snowfall (1-Jordan91; 2->BATS; 3->Noah)
  INTEGER,  INTENT(IN) :: iopt_tbot !lower boundary of soil temperature (1->zero-flux; 2->Noah)

  INTEGER,  INTENT(IN) :: iopt_stc  !snow/soil temperature time scheme (only layer 1)
                                    ! 1 -> semi-implicit; 2 -> full implicit (original Noah)
  INTEGER,  INTENT(IN) :: iopt_rsf  !surface resistance (1->Sakaguchi/Zeng; 2->Seller; 3->mod Sellers; 4->1+snow)
  INTEGER,  INTENT(IN) :: iopt_soil !soil parameters set-up option
  INTEGER,  INTENT(IN) :: iopt_pedo !pedo-transfer function (1->Saxton and Rawls)
  INTEGER,  INTENT(IN) :: iopt_crop !crop model option (0->none; 1->Liu et al.)
  INTEGER,  INTENT(IN) :: iopt_irr  ! 0 -> No irrigation; 
                                    ! 1 -> Irrigation ON;
                                    ! 2 -> irrigation trigger based on crop season Planting and harvesting dates; 
                                    ! 3 -> irrigation trigger based on LAI threshold
  INTEGER,  INTENT(IN) :: iopt_irrm ! 0 -> all methods ON based on geo_em inputs
                                    ! 1 -> sprinkler ON
                                    ! 2 -> micro/drip ON
                                    ! 3 -> flood irrigation ON
  INTEGER,  INTENT(IN) :: iopt_infdv!infiltration options for dynamic VIC (1->Philip; 2-> Green-Ampt;3->Smith-Parlange)
  INTEGER,  INTENT(IN) :: iopt_tdrn !tile drainage (0->none; 1-> simple 2->Hooghoudt's)
! -------------------------------------------------------------------------------------------------

  dveg = idveg

  opt_crs  = iopt_crs
  opt_btr  = iopt_btr
  opt_run  = iopt_run
  opt_sfc  = iopt_sfc
  opt_frz  = iopt_frz
  opt_inf  = iopt_inf
  opt_rad  = iopt_rad
  opt_alb  = iopt_alb
  opt_snf  = iopt_snf
  opt_tbot = iopt_tbot
  opt_stc  = iopt_stc
  opt_rsf  = iopt_rsf
  opt_soil = iopt_soil
  opt_pedo = iopt_pedo
  opt_crop = iopt_crop
  opt_irr  = iopt_irr
  opt_irrm = iopt_irrm
  opt_infdv= iopt_infdv
  opt_tdrn = iopt_tdrn

  end subroutine noahmp_options

end module noahmp_routines

MODULE NOAHMP_TABLES

    IMPLICIT NONE

    INTEGER, PRIVATE, PARAMETER :: MVT   = 27
    INTEGER, PRIVATE, PARAMETER :: MBAND = 2
    INTEGER, PRIVATE, PARAMETER :: MSC   = 8
    INTEGER, PRIVATE, PARAMETER :: MAX_SOILTYP = 30
    INTEGER, PRIVATE, PARAMETER :: NCROP = 5
    INTEGER, PRIVATE, PARAMETER :: NSTAGE = 8

! MPTABLE.TBL vegetation parameters

    INTEGER :: ISURBAN_TABLE
    INTEGER :: ISWATER_TABLE
    INTEGER :: ISBARREN_TABLE
    INTEGER :: ISICE_TABLE
    INTEGER :: ISCROP_TABLE
    INTEGER :: EBLFOREST_TABLE
    INTEGER :: NATURAL_TABLE
    INTEGER :: LCZ_1_TABLE
    INTEGER :: LCZ_2_TABLE
    INTEGER :: LCZ_3_TABLE
    INTEGER :: LCZ_4_TABLE
    INTEGER :: LCZ_5_TABLE
    INTEGER :: LCZ_6_TABLE
    INTEGER :: LCZ_7_TABLE
    INTEGER :: LCZ_8_TABLE
    INTEGER :: LCZ_9_TABLE
    INTEGER :: LCZ_10_TABLE
    INTEGER :: LCZ_11_TABLE

    REAL :: CH2OP_TABLE(MVT)       !maximum intercepted h2o per unit lai+sai (mm)
    REAL :: DLEAF_TABLE(MVT)       !characteristic leaf dimension (m)
    REAL :: Z0MVT_TABLE(MVT)       !momentum roughness length (m)
    REAL :: HVT_TABLE(MVT)         !top of canopy (m)
    REAL :: HVB_TABLE(MVT)         !bottom of canopy (m)
    REAL :: DEN_TABLE(MVT)         !tree density (no. of trunks per m2)
    REAL :: RC_TABLE(MVT)          !tree crown radius (m)
    REAL :: MFSNO_TABLE(MVT)       !snowmelt curve parameter ()
    REAL :: SCFFAC_TABLE(MVT)      !snow cover factor (m) (replace original hard-coded 2.5*z0 in SCF formulation)
    REAL :: SAIM_TABLE(MVT,12)     !monthly stem area index, one-sided
    REAL :: LAIM_TABLE(MVT,12)     !monthly leaf area index, one-sided
    REAL :: SLA_TABLE(MVT)         !single-side leaf area per Kg [m2/kg]
    REAL :: DILEFC_TABLE(MVT)      !coeficient for leaf stress death [1/s]
    REAL :: DILEFW_TABLE(MVT)      !coeficient for leaf stress death [1/s]
    REAL :: FRAGR_TABLE(MVT)       !fraction of growth respiration  !original was 0.3 
    REAL :: LTOVRC_TABLE(MVT)      !leaf turnover [1/s]

    REAL :: C3PSN_TABLE(MVT)       !photosynthetic pathway: 0. = c4, 1. = c3
    REAL :: KC25_TABLE(MVT)        !co2 michaelis-menten constant at 25c (pa)
    REAL :: AKC_TABLE(MVT)         !q10 for kc25
    REAL :: KO25_TABLE(MVT)        !o2 michaelis-menten constant at 25c (pa)
    REAL :: AKO_TABLE(MVT)         !q10 for ko25
    REAL :: VCMX25_TABLE(MVT)      !maximum rate of carboxylation at 25c (umol co2/m**2/s)
    REAL :: AVCMX_TABLE(MVT)       !q10 for vcmx25
    REAL :: BP_TABLE(MVT)          !minimum leaf conductance (umol/m**2/s)
    REAL :: MP_TABLE(MVT)          !slope of conductance-to-photosynthesis relationship
    REAL :: QE25_TABLE(MVT)        !quantum efficiency at 25c (umol co2 / umol photon)
    REAL :: AQE_TABLE(MVT)         !q10 for qe25
    REAL :: RMF25_TABLE(MVT)       !leaf maintenance respiration at 25c (umol co2/m**2/s)
    REAL :: RMS25_TABLE(MVT)       !stem maintenance respiration at 25c (umol co2/kg bio/s)
    REAL :: RMR25_TABLE(MVT)       !root maintenance respiration at 25c (umol co2/kg bio/s)
    REAL :: ARM_TABLE(MVT)         !q10 for maintenance respiration
    REAL :: FOLNMX_TABLE(MVT)      !foliage nitrogen concentration when f(n)=1 (%)
    REAL :: TMIN_TABLE(MVT)        !minimum temperature for photosynthesis (k)

    REAL :: XL_TABLE(MVT)          !leaf/stem orientation index
    REAL :: RHOL_TABLE(MVT,MBAND)  !leaf reflectance: 1=vis, 2=nir
    REAL :: RHOS_TABLE(MVT,MBAND)  !stem reflectance: 1=vis, 2=nir
    REAL :: TAUL_TABLE(MVT,MBAND)  !leaf transmittance: 1=vis, 2=nir
    REAL :: TAUS_TABLE(MVT,MBAND)  !stem transmittance: 1=vis, 2=nir

    REAL :: MRP_TABLE(MVT)         !microbial respiration parameter (umol co2 /kg c/ s)
    REAL :: CWPVT_TABLE(MVT)       !empirical canopy wind parameter

    REAL :: WRRAT_TABLE(MVT)       !wood to non-wood ratio
    REAL :: WDPOOL_TABLE(MVT)      !wood pool (switch 1 or 0) depending on woody or not [-]
    REAL :: TDLEF_TABLE(MVT)       !characteristic T for leaf freezing [K]

    REAL :: NROOT_TABLE(MVT)       !number of soil layers with root present
    REAL :: RGL_TABLE(MVT)         !Parameter used in radiation stress function
    REAL :: RS_TABLE(MVT)          !Minimum stomatal resistance [s m-1]
    REAL :: HS_TABLE(MVT)          !Parameter used in vapor pressure deficit function
    REAL :: TOPT_TABLE(MVT)        !Optimum transpiration air temperature [K]
    REAL :: RSMAX_TABLE(MVT)       !Maximal stomatal resistance [s m-1]

! SOILPARM.TBL parameters

    INTEGER            :: SLCATS

    REAL :: BEXP_TABLE(MAX_SOILTYP)        !maximum intercepted h2o per unit lai+sai (mm)
    REAL :: SMCDRY_TABLE(MAX_SOILTYP)      !characteristic leaf dimension (m)
    REAL :: F1_TABLE(MAX_SOILTYP)          !momentum roughness length (m)
    REAL :: SMCMAX_TABLE(MAX_SOILTYP)      !top of canopy (m)
    REAL :: SMCREF_TABLE(MAX_SOILTYP)      !bottom of canopy (m)
    REAL :: PSISAT_TABLE(MAX_SOILTYP)      !tree density (no. of trunks per m2)
    REAL :: DKSAT_TABLE(MAX_SOILTYP)       !tree crown radius (m)
    REAL :: DWSAT_TABLE(MAX_SOILTYP)       !monthly stem area index, one-sided
    REAL :: SMCWLT_TABLE(MAX_SOILTYP)      !monthly leaf area index, one-sided
    REAL :: QUARTZ_TABLE(MAX_SOILTYP)      !single-side leaf area per Kg [m2/kg]
    REAL :: BVIC_TABLE(MAX_SOILTYP)        !VIC model infiltration parameter (-) for opt_run=6
    REAL :: AXAJ_TABLE(MAX_SOILTYP)        !Xinanjiang: Tension water distribution inflection parameter [-] for opt_run=7
    REAL :: BXAJ_TABLE(MAX_SOILTYP)        !Xinanjiang: Tension water distribution shape parameter [-] for opt_run=7
    REAL :: XXAJ_TABLE(MAX_SOILTYP)        !Xinanjiang: Free water distribution shape parameter [-] for opt_run=7
    REAL :: BDVIC_TABLE(MAX_SOILTYP)       !VIC model infiltration parameter (-)
    REAL :: GDVIC_TABLE(MAX_SOILTYP)       !mean capilary drive (m)
    REAL :: BBVIC_TABLE(MAX_SOILTYP)       !heterogeniety parameter for DVIC infiltration [-]

! GENPARM.TBL parameters

    REAL :: SLOPE_TABLE(9)    !slope factor for soil drainage

    REAL :: CSOIL_TABLE       !Soil heat capacity [J m-3 K-1]
    REAL :: REFDK_TABLE       !Parameter in the surface runoff parameterization
    REAL :: REFKDT_TABLE      !Parameter in the surface runoff parameterization
    REAL :: FRZK_TABLE        !Frozen ground parameter
    REAL :: ZBOT_TABLE        !Depth [m] of lower boundary soil temperature
    REAL :: CZIL_TABLE        !Parameter used in the calculation of the roughness length for heat

! MPTABLE.TBL radiation parameters

    REAL :: ALBSAT_TABLE(MSC,MBAND)   !saturated soil albedos: 1=vis, 2=nir
    REAL :: ALBDRY_TABLE(MSC,MBAND)   !dry soil albedos: 1=vis, 2=nir
    REAL :: ALBICE_TABLE(MBAND)       !albedo land ice: 1=vis, 2=nir
    REAL :: ALBLAK_TABLE(MBAND)       !albedo frozen lakes: 1=vis, 2=nir
    REAL :: OMEGAS_TABLE(MBAND)       !two-stream parameter omega for snow
    REAL :: BETADS_TABLE              !two-stream parameter betad for snow
    REAL :: BETAIS_TABLE              !two-stream parameter betad for snow
    REAL :: EG_TABLE(2)               !emissivity

! MPTABLE.TBL global parameters

    REAL :: CO2_TABLE      !co2 partial pressure
    REAL :: O2_TABLE       !o2 partial pressure
    REAL :: TIMEAN_TABLE   !gridcell mean topgraphic index (global mean)
    REAL :: FSATMX_TABLE   !maximum surface saturated fraction (global mean)
    REAL :: Z0SNO_TABLE    !snow surface roughness length (m) (0.002)
    REAL :: SSI_TABLE      !liquid water holding capacity for snowpack (m3/m3) (0.03)
    REAL :: SNOW_RET_FAC_TABLE  !snowpack water release timescale factor (1/s)
    REAL :: SNOW_EMIS_TABLE!snow emissivity
    REAL :: SWEMX_TABLE    !new snow mass to fully cover old snow (mm)
    REAL :: TAU0_TABLE          !tau0 from Yang97 eqn. 10a
    REAL :: GRAIN_GROWTH_TABLE  !growth from vapor diffusion Yang97 eqn. 10b
    REAL :: EXTRA_GROWTH_TABLE  !extra growth near freezing Yang97 eqn. 10c
    REAL :: DIRT_SOOT_TABLE     !dirt and soot term Yang97 eqn. 10d
    REAL :: BATS_COSZ_TABLE     !zenith angle snow albedo adjustment; b in Yang97 eqn. 15
    REAL :: BATS_VIS_NEW_TABLE  !new snow visible albedo
    REAL :: BATS_NIR_NEW_TABLE  !new snow NIR albedo
    REAL :: BATS_VIS_AGE_TABLE  !age factor for diffuse visible snow albedo Yang97 eqn. 17
    REAL :: BATS_NIR_AGE_TABLE  !age factor for diffuse NIR snow albedo Yang97 eqn. 18
    REAL :: BATS_VIS_DIR_TABLE  !cosz factor for direct visible snow albedo Yang97 eqn. 15
    REAL :: BATS_NIR_DIR_TABLE  !cosz factor for direct NIR snow albedo Yang97 eqn. 16
    REAL :: RSURF_SNOW_TABLE    !surface resistance for snow(s/m)
    REAL :: RSURF_EXP_TABLE    !exponent in the shape parameter for soil resistance option 1

! MPTABLE.TBL irrigation parameters

    REAL :: IRR_FRAC_TABLE              ! irrigation Fraction
 INTEGER :: IRR_HAR_TABLE               ! number of days before harvest date to stop irrigation 
    REAL :: IRR_LAI_TABLE               ! Minimum lai to trigger irrigation
    REAL :: IRR_MAD_TABLE               ! management allowable deficit (0-1)
    REAL :: FILOSS_TABLE                ! fraction of flood irrigation loss (0-1) 
    REAL :: SPRIR_RATE_TABLE            ! mm/h, sprinkler irrigation rate
    REAL :: MICIR_RATE_TABLE            ! mm/h, micro irrigation rate
    REAL :: FIRTFAC_TABLE               ! flood application rate factor
    REAL :: IR_RAIN_TABLE               ! maximum precipitation to stop irrigation trigger

! tile drainage parameters
    REAL    :: TDSMCFAC_TABLE(MAX_SOILTYP)
    REAL    :: TD_DC_TABLE(MAX_SOILTYP)
    INTEGER :: TD_DEPTH_TABLE(MAX_SOILTYP)
    INTEGER :: DRAIN_LAYER_OPT_TABLE
    REAL    :: TD_DCOEF_TABLE(MAX_SOILTYP)
    REAL    :: TD_D_TABLE(MAX_SOILTYP)
    REAL    :: TD_ADEPTH_TABLE(MAX_SOILTYP)
    REAL    :: TD_RADI_TABLE(MAX_SOILTYP)
    REAL    :: TD_SPAC_TABLE(MAX_SOILTYP)
    REAL    :: TD_DDRAIN_TABLE(MAX_SOILTYP)
    REAL    :: KLAT_FAC_TABLE(MAX_SOILTYP)

! MPTABLE.TBL optional parameters

    REAL :: sr2006_theta_1500t_a        ! sand coefficient
    REAL :: sr2006_theta_1500t_b        ! clay coefficient
    REAL :: sr2006_theta_1500t_c        ! orgm coefficient
    REAL :: sr2006_theta_1500t_d        ! sand*orgm coefficient
    REAL :: sr2006_theta_1500t_e        ! clay*orgm coefficient
    REAL :: sr2006_theta_1500t_f        ! sand*clay coefficient
    REAL :: sr2006_theta_1500t_g        ! constant adjustment

    REAL :: sr2006_theta_1500_a         ! theta_1500t coefficient
    REAL :: sr2006_theta_1500_b         ! constant adjustment

    REAL :: sr2006_theta_33t_a          ! sand coefficient
    REAL :: sr2006_theta_33t_b          ! clay coefficient
    REAL :: sr2006_theta_33t_c          ! orgm coefficient
    REAL :: sr2006_theta_33t_d          ! sand*orgm coefficient
    REAL :: sr2006_theta_33t_e          ! clay*orgm coefficient
    REAL :: sr2006_theta_33t_f          ! sand*clay coefficient
    REAL :: sr2006_theta_33t_g          ! constant adjustment

    REAL :: sr2006_theta_33_a           ! theta_33t*theta_33t coefficient
    REAL :: sr2006_theta_33_b           ! theta_33t coefficient
    REAL :: sr2006_theta_33_c           ! constant adjustment

    REAL :: sr2006_theta_s33t_a         ! sand coefficient
    REAL :: sr2006_theta_s33t_b         ! clay coefficient
    REAL :: sr2006_theta_s33t_c         ! orgm coefficient
    REAL :: sr2006_theta_s33t_d         ! sand*orgm coefficient
    REAL :: sr2006_theta_s33t_e         ! clay*orgm coefficient
    REAL :: sr2006_theta_s33t_f         ! sand*clay coefficient
    REAL :: sr2006_theta_s33t_g         ! constant adjustment

    REAL :: sr2006_theta_s33_a          ! theta_s33t coefficient
    REAL :: sr2006_theta_s33_b          ! constant adjustment

    REAL :: sr2006_psi_et_a             ! sand coefficient
    REAL :: sr2006_psi_et_b             ! clay coefficient
    REAL :: sr2006_psi_et_c             ! theta_s33 coefficient
    REAL :: sr2006_psi_et_d             ! sand*theta_s33 coefficient
    REAL :: sr2006_psi_et_e             ! clay*theta_s33 coefficient
    REAL :: sr2006_psi_et_f             ! sand*clay coefficient
    REAL :: sr2006_psi_et_g             ! constant adjustment

    REAL :: sr2006_psi_e_a              ! psi_et*psi_et coefficient
    REAL :: sr2006_psi_e_b              ! psi_et coefficient
    REAL :: sr2006_psi_e_c              ! constant adjustment

    REAL :: sr2006_smcmax_a             ! sand adjustment
    REAL :: sr2006_smcmax_b             ! constant adjustment

! MPTABLE.TBL crop parameters

 INTEGER :: DEFAULT_CROP_TABLE          ! Default crop index
 INTEGER :: PLTDAY_TABLE(NCROP)         ! Planting date
 INTEGER :: HSDAY_TABLE(NCROP)          ! Harvest date
    REAL :: PLANTPOP_TABLE(NCROP)       ! Plant density [per ha] - used?
    REAL :: IRRI_TABLE(NCROP)           ! Irrigation strategy 0= non-irrigation 1=irrigation (no water-stress)

    REAL :: GDDTBASE_TABLE(NCROP)       ! Base temperature for GDD accumulation [C]
    REAL :: GDDTCUT_TABLE(NCROP)        ! Upper temperature for GDD accumulation [C]
    REAL :: GDDS1_TABLE(NCROP)          ! GDD from seeding to emergence
    REAL :: GDDS2_TABLE(NCROP)          ! GDD from seeding to initial vegetative 
    REAL :: GDDS3_TABLE(NCROP)          ! GDD from seeding to post vegetative 
    REAL :: GDDS4_TABLE(NCROP)          ! GDD from seeding to intial reproductive
    REAL :: GDDS5_TABLE(NCROP)          ! GDD from seeding to pysical maturity 

    REAL :: C3PSNI_TABLE(NCROP)       !photosynthetic pathway: 0. = c4, 1. = c3 ! Zhe Zhang 2020-07-03
    REAL :: KC25I_TABLE(NCROP)        !co2 michaelis-menten constant at 25c (pa)
    REAL :: AKCI_TABLE(NCROP)         !q10 for kc25
    REAL :: KO25I_TABLE(NCROP)        !o2 michaelis-menten constant at 25c (pa)
    REAL :: AKOI_TABLE(NCROP)         !q10 for ko25
    REAL :: VCMX25I_TABLE(NCROP)      !maximum rate of carboxylation at 25c (umol co2/m**2/s)
    REAL :: AVCMXI_TABLE(NCROP)       !q10 for vcmx25
    REAL :: BPI_TABLE(NCROP)          !minimum leaf conductance (umol/m**2/s)
    REAL :: MPI_TABLE(NCROP)          !slope of conductance-to-photosynthesis relationship
    REAL :: QE25I_TABLE(NCROP)        !quantum efficiency at 25c (umol co2 / umol photon)
    REAL :: FOLNMXI_TABLE(NCROP)      !foliage nitrogen concentration when

 INTEGER :: C3C4_TABLE(NCROP)           ! photosynthetic pathway:  1. = c3 2. = c4
    REAL :: AREF_TABLE(NCROP)           ! reference maximum CO2 assimulation rate 
    REAL :: PSNRF_TABLE(NCROP)          ! CO2 assimulation reduction factor(0-1) (caused by non-modeling part,e.g.pest,weeds)
    REAL :: I2PAR_TABLE(NCROP)          ! Fraction of incoming solar radiation to photosynthetically active radiation
    REAL :: TASSIM0_TABLE(NCROP)        ! Minimum temperature for CO2 assimulation [C]
    REAL :: TASSIM1_TABLE(NCROP)        ! CO2 assimulation linearly increasing until temperature reaches T1 [C]
    REAL :: TASSIM2_TABLE(NCROP)        ! CO2 assmilation rate remain at Aref until temperature reaches T2 [C]
    REAL :: K_TABLE(NCROP)              ! light extinction coefficient
    REAL :: EPSI_TABLE(NCROP)           ! initial light use efficiency

    REAL :: Q10MR_TABLE(NCROP)          ! q10 for maintainance respiration
    REAL :: FOLN_MX_TABLE(NCROP)        ! foliage nitrogen concentration when f(n)=1 (%)
    REAL :: LEFREEZ_TABLE(NCROP)        ! characteristic T for leaf freezing [K]

    REAL :: DILE_FC_TABLE(NCROP,NSTAGE) ! coeficient for temperature leaf stress death [1/s]
    REAL :: DILE_FW_TABLE(NCROP,NSTAGE) ! coeficient for water leaf stress death [1/s]
    REAL :: FRA_GR_TABLE(NCROP)         ! fraction of growth respiration

    REAL :: LF_OVRC_TABLE(NCROP,NSTAGE) ! fraction of leaf turnover  [1/s]
    REAL :: ST_OVRC_TABLE(NCROP,NSTAGE) ! fraction of stem turnover  [1/s]
    REAL :: RT_OVRC_TABLE(NCROP,NSTAGE) ! fraction of root tunrover  [1/s]
    REAL :: LFMR25_TABLE(NCROP)         !  leaf maintenance respiration at 25C [umol CO2/m**2  /s]
    REAL :: STMR25_TABLE(NCROP)         !  stem maintenance respiration at 25C [umol CO2/kg bio/s]
    REAL :: RTMR25_TABLE(NCROP)         !  root maintenance respiration at 25C [umol CO2/kg bio/s]
    REAL :: GRAINMR25_TABLE(NCROP)      ! grain maintenance respiration at 25C [umol CO2/kg bio/s]

    REAL :: LFPT_TABLE(NCROP,NSTAGE)    ! fraction of carbohydrate flux to leaf
    REAL :: STPT_TABLE(NCROP,NSTAGE)    ! fraction of carbohydrate flux to stem
    REAL :: RTPT_TABLE(NCROP,NSTAGE)    ! fraction of carbohydrate flux to root
    REAL :: GRAINPT_TABLE(NCROP,NSTAGE) ! fraction of carbohydrate flux to grain
    REAL :: LFCT_TABLE(NCROP,NSTAGE)    ! fraction of carbohydrate translocation from leaf to grain ! Zhe Zhang 2020-07-13 
    REAL :: STCT_TABLE(NCROP,NSTAGE)    !                                             stem to grain
    REAL :: RTCT_TABLE(NCROP,NSTAGE)    !                                             root to grain
    REAL :: BIO2LAI_TABLE(NCROP)        ! leaf are per living leaf biomass [m^2/kg]


CONTAINS

  subroutine read_mp_veg_parameters(DATASET_IDENTIFIER)
    implicit none
    character(len=*), intent(in) :: DATASET_IDENTIFIER
    integer :: ierr
    INTEGER :: IK,IM
    logical :: file_named

    integer :: NVEG
    character(len=256) :: VEG_DATASET_DESCRIPTION

    INTEGER :: ISURBAN
    INTEGER :: ISWATER
    INTEGER :: ISBARREN
    INTEGER :: ISICE
    INTEGER :: ISCROP
    INTEGER :: EBLFOREST
    INTEGER :: NATURAL
    INTEGER :: LCZ_1
    INTEGER :: LCZ_2
    INTEGER :: LCZ_3
    INTEGER :: LCZ_4
    INTEGER :: LCZ_5
    INTEGER :: LCZ_6
    INTEGER :: LCZ_7
    INTEGER :: LCZ_8
    INTEGER :: LCZ_9
    INTEGER :: LCZ_10
    INTEGER :: LCZ_11

    REAL, DIMENSION(MVT) :: SAI_JAN,SAI_FEB,SAI_MAR,SAI_APR,SAI_MAY,SAI_JUN, &
                                     SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC
    REAL, DIMENSION(MVT) :: LAI_JAN,LAI_FEB,LAI_MAR,LAI_APR,LAI_MAY,LAI_JUN, &
                                     LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC
    REAL, DIMENSION(MVT) :: RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, &
                                     TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR
    REAL, DIMENSION(MVT) :: CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, &
                     AVCMX, AQE, LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  &
                     BP, MP, QE25, RMS25, RMR25, ARM, FOLNMX, WDPOOL, WRRAT, MRP, NROOT, RGL, RS, HS, TOPT, RSMAX, &
                     SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5
                        
    NAMELIST / noahmp_usgs_veg_categories / VEG_DATASET_DESCRIPTION, NVEG
    NAMELIST / noahmp_usgs_parameters / ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL, &
         LCZ_1,LCZ_2,LCZ_3,LCZ_4,LCZ_5,LCZ_6,LCZ_7,LCZ_8,LCZ_9,LCZ_10,LCZ_11,&
         CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, AVCMX, AQE, &
         LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  BP, MP, QE25, RMS25, RMR25, ARM, &
         FOLNMX, WDPOOL, WRRAT, MRP, NROOT, RGL, RS, HS, TOPT, RSMAX, &
         SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY, SAI_JUN,SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC, &
         LAI_JAN, LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN,LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC, &
         RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR, SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5
        
    NAMELIST / noahmp_modis_veg_categories / VEG_DATASET_DESCRIPTION, NVEG
    NAMELIST / noahmp_modis_parameters / ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL, &
         LCZ_1,LCZ_2,LCZ_3,LCZ_4,LCZ_5,LCZ_6,LCZ_7,LCZ_8,LCZ_9,LCZ_10,LCZ_11, &
         CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, AVCMX, AQE, &
         LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  BP, MP, QE25, RMS25, RMR25, ARM, &
         FOLNMX, WDPOOL, WRRAT, MRP, NROOT, RGL, RS, HS, TOPT, RSMAX, &
         SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY, SAI_JUN,SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC, &
         LAI_JAN, LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN,LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC, &
         RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR, SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5

    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screeching halt as soon as we try to use anything.
    CH2OP_TABLE  = -1.E36
    DLEAF_TABLE  = -1.E36
    Z0MVT_TABLE  = -1.E36
    HVT_TABLE    = -1.E36
    HVB_TABLE    = -1.E36
    DEN_TABLE    = -1.E36
    RC_TABLE     = -1.E36
    MFSNO_TABLE  = -1.E36
    SCFFAC_TABLE = -1.E36
    RHOL_TABLE   = -1.E36
    RHOS_TABLE   = -1.E36
    TAUL_TABLE   = -1.E36
    TAUS_TABLE   = -1.E36
    XL_TABLE     = -1.E36
    CWPVT_TABLE  = -1.E36
    C3PSN_TABLE  = -1.E36
    KC25_TABLE   = -1.E36
    AKC_TABLE    = -1.E36
    KO25_TABLE   = -1.E36
    AKO_TABLE    = -1.E36
    AVCMX_TABLE  = -1.E36
    AQE_TABLE    = -1.E36
    LTOVRC_TABLE = -1.E36
    DILEFC_TABLE = -1.E36
    DILEFW_TABLE = -1.E36
    RMF25_TABLE  = -1.E36
    SLA_TABLE    = -1.E36
    FRAGR_TABLE  = -1.E36
    TMIN_TABLE   = -1.E36
    VCMX25_TABLE = -1.E36
    TDLEF_TABLE  = -1.E36
    BP_TABLE     = -1.E36
    MP_TABLE     = -1.E36
    QE25_TABLE   = -1.E36
    RMS25_TABLE  = -1.E36
    RMR25_TABLE  = -1.E36
    ARM_TABLE    = -1.E36
    FOLNMX_TABLE = -1.E36
    WDPOOL_TABLE = -1.E36
    WRRAT_TABLE  = -1.E36
    MRP_TABLE    = -1.E36
    SAIM_TABLE   = -1.E36
    LAIM_TABLE   = -1.E36
    NROOT_TABLE  = -1.E36
    RGL_TABLE    = -1.E36
    RS_TABLE     = -1.E36
    HS_TABLE     = -1.E36
    TOPT_TABLE   = -1.E36
    RSMAX_TABLE  = -1.E36

    ISURBAN_TABLE      = -99999
    ISWATER_TABLE      = -99999
    ISBARREN_TABLE     = -99999
    ISICE_TABLE        = -99999
    ISCROP_TABLE       = -99999
    EBLFOREST_TABLE    = -99999
    NATURAL_TABLE      = -99999
    LCZ_1_TABLE   = -99999
    LCZ_2_TABLE   = -99999
    LCZ_3_TABLE   = -99999
    LCZ_4_TABLE   = -99999
    LCZ_5_TABLE   = -99999
    LCZ_6_TABLE   = -99999
    LCZ_7_TABLE   = -99999
    LCZ_8_TABLE   = -99999
    LCZ_9_TABLE   = -99999
    LCZ_10_TABLE   = -99999
    LCZ_11_TABLE   = -99999

    inquire( file='MPTABLE.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file MPTABLE.TBL")')
    endif

    if ( trim(DATASET_IDENTIFIER) == "USGS" ) then
       read(15,noahmp_usgs_veg_categories)
       read(15,noahmp_usgs_parameters)
    else if ( trim(DATASET_IDENTIFIER) == "MODIFIED_IGBP_MODIS_NOAH" ) then
       read(15,noahmp_modis_veg_categories)
       read(15,noahmp_modis_parameters)
    else
       write(*,'("WARNING: Unrecognized DATASET_IDENTIFIER in subroutine READ_MP_VEG_PARAMETERS")')
       write(*,'("WARNING: DATASET_IDENTIFIER = ''", A, "''")') trim(DATASET_IDENTIFIER)
    endif
    close(15)

                      ISURBAN_TABLE   = ISURBAN
                      ISWATER_TABLE   = ISWATER
                     ISBARREN_TABLE   = ISBARREN
                        ISICE_TABLE   = ISICE
                       ISCROP_TABLE   = ISCROP
                    EBLFOREST_TABLE   = EBLFOREST
                      NATURAL_TABLE   = NATURAL
                        LCZ_1_TABLE   = LCZ_1
                        LCZ_2_TABLE   = LCZ_2
                        LCZ_3_TABLE   = LCZ_3
                        LCZ_4_TABLE   = LCZ_4
                        LCZ_5_TABLE   = LCZ_5
                        LCZ_6_TABLE   = LCZ_6
                        LCZ_7_TABLE   = LCZ_7
                        LCZ_8_TABLE   = LCZ_8
                        LCZ_9_TABLE   = LCZ_9
                        LCZ_10_TABLE  = LCZ_10
                        LCZ_11_TABLE  = LCZ_11

     CH2OP_TABLE(1:NVEG)  = CH2OP(1:NVEG)
     DLEAF_TABLE(1:NVEG)  = DLEAF(1:NVEG)
     Z0MVT_TABLE(1:NVEG)  = Z0MVT(1:NVEG)
       HVT_TABLE(1:NVEG)  = HVT(1:NVEG)
       HVB_TABLE(1:NVEG)  = HVB(1:NVEG)
       DEN_TABLE(1:NVEG)  = DEN(1:NVEG)
        RC_TABLE(1:NVEG)  = RC(1:NVEG)
     MFSNO_TABLE(1:NVEG)  = MFSNO(1:NVEG)
    SCFFAC_TABLE(1:NVEG)  = SCFFAC(1:NVEG)
        XL_TABLE(1:NVEG)  = XL(1:NVEG)
     CWPVT_TABLE(1:NVEG)  = CWPVT(1:NVEG)
     C3PSN_TABLE(1:NVEG)  = C3PSN(1:NVEG)
      KC25_TABLE(1:NVEG)  = KC25(1:NVEG)
       AKC_TABLE(1:NVEG)  = AKC(1:NVEG)
      KO25_TABLE(1:NVEG)  = KO25(1:NVEG)
       AKO_TABLE(1:NVEG)  = AKO(1:NVEG)
     AVCMX_TABLE(1:NVEG)  = AVCMX(1:NVEG)
       AQE_TABLE(1:NVEG)  = AQE(1:NVEG)
    LTOVRC_TABLE(1:NVEG)  = LTOVRC(1:NVEG)
    DILEFC_TABLE(1:NVEG)  = DILEFC(1:NVEG)
    DILEFW_TABLE(1:NVEG)  = DILEFW(1:NVEG)
     RMF25_TABLE(1:NVEG)  = RMF25(1:NVEG)
       SLA_TABLE(1:NVEG)  = SLA(1:NVEG)
     FRAGR_TABLE(1:NVEG)  = FRAGR(1:NVEG)
      TMIN_TABLE(1:NVEG)  = TMIN(1:NVEG)
    VCMX25_TABLE(1:NVEG)  = VCMX25(1:NVEG)
     TDLEF_TABLE(1:NVEG)  = TDLEF(1:NVEG)
        BP_TABLE(1:NVEG)  = BP(1:NVEG)
        MP_TABLE(1:NVEG)  = MP(1:NVEG)
      QE25_TABLE(1:NVEG)  = QE25(1:NVEG)
     RMS25_TABLE(1:NVEG)  = RMS25(1:NVEG)
     RMR25_TABLE(1:NVEG)  = RMR25(1:NVEG)
       ARM_TABLE(1:NVEG)  = ARM(1:NVEG)
    FOLNMX_TABLE(1:NVEG)  = FOLNMX(1:NVEG)
    WDPOOL_TABLE(1:NVEG)  = WDPOOL(1:NVEG)
     WRRAT_TABLE(1:NVEG)  = WRRAT(1:NVEG)
       MRP_TABLE(1:NVEG)  = MRP(1:NVEG)
     NROOT_TABLE(1:NVEG)  = NROOT(1:NVEG)
       RGL_TABLE(1:NVEG)  = RGL(1:NVEG)
        RS_TABLE(1:NVEG)  = RS(1:NVEG)
        HS_TABLE(1:NVEG)  = HS(1:NVEG)
      TOPT_TABLE(1:NVEG)  = TOPT(1:NVEG)
     RSMAX_TABLE(1:NVEG)  = RSMAX(1:NVEG)

    ! Put LAI and SAI into 2d array from monthly lines in table; same for canopy radiation properties

    SAIM_TABLE(1:NVEG, 1) = SAI_JAN(1:NVEG)
    SAIM_TABLE(1:NVEG, 2) = SAI_FEB(1:NVEG)
    SAIM_TABLE(1:NVEG, 3) = SAI_MAR(1:NVEG)
    SAIM_TABLE(1:NVEG, 4) = SAI_APR(1:NVEG)
    SAIM_TABLE(1:NVEG, 5) = SAI_MAY(1:NVEG)
    SAIM_TABLE(1:NVEG, 6) = SAI_JUN(1:NVEG)
    SAIM_TABLE(1:NVEG, 7) = SAI_JUL(1:NVEG)
    SAIM_TABLE(1:NVEG, 8) = SAI_AUG(1:NVEG)
    SAIM_TABLE(1:NVEG, 9) = SAI_SEP(1:NVEG)
    SAIM_TABLE(1:NVEG,10) = SAI_OCT(1:NVEG)
    SAIM_TABLE(1:NVEG,11) = SAI_NOV(1:NVEG)
    SAIM_TABLE(1:NVEG,12) = SAI_DEC(1:NVEG)

    LAIM_TABLE(1:NVEG, 1) = LAI_JAN(1:NVEG)
    LAIM_TABLE(1:NVEG, 2) = LAI_FEB(1:NVEG)
    LAIM_TABLE(1:NVEG, 3) = LAI_MAR(1:NVEG)
    LAIM_TABLE(1:NVEG, 4) = LAI_APR(1:NVEG)
    LAIM_TABLE(1:NVEG, 5) = LAI_MAY(1:NVEG)
    LAIM_TABLE(1:NVEG, 6) = LAI_JUN(1:NVEG)
    LAIM_TABLE(1:NVEG, 7) = LAI_JUL(1:NVEG)
    LAIM_TABLE(1:NVEG, 8) = LAI_AUG(1:NVEG)
    LAIM_TABLE(1:NVEG, 9) = LAI_SEP(1:NVEG)
    LAIM_TABLE(1:NVEG,10) = LAI_OCT(1:NVEG)
    LAIM_TABLE(1:NVEG,11) = LAI_NOV(1:NVEG)
    LAIM_TABLE(1:NVEG,12) = LAI_DEC(1:NVEG)

    RHOL_TABLE(1:NVEG,1)  = RHOL_VIS(1:NVEG) !leaf reflectance: 1=vis, 2=nir
    RHOL_TABLE(1:NVEG,2)  = RHOL_NIR(1:NVEG) !leaf reflectance: 1=vis, 2=nir
    RHOS_TABLE(1:NVEG,1)  = RHOS_VIS(1:NVEG) !stem reflectance: 1=vis, 2=nir
    RHOS_TABLE(1:NVEG,2)  = RHOS_NIR(1:NVEG) !stem reflectance: 1=vis, 2=nir
    TAUL_TABLE(1:NVEG,1)  = TAUL_VIS(1:NVEG) !leaf transmittance: 1=vis, 2=nir
    TAUL_TABLE(1:NVEG,2)  = TAUL_NIR(1:NVEG) !leaf transmittance: 1=vis, 2=nir
    TAUS_TABLE(1:NVEG,1)  = TAUS_VIS(1:NVEG) !stem transmittance: 1=vis, 2=nir
    TAUS_TABLE(1:NVEG,2)  = TAUS_NIR(1:NVEG) !stem transmittance: 1=vis, 2=nir

  end subroutine read_mp_veg_parameters

  subroutine read_mp_soil_parameters()
    IMPLICIT NONE
    INTEGER :: IERR
    CHARACTER*4         :: SLTYPE
    INTEGER             :: ITMP, NUM_SLOPE, LC
    CHARACTER(len=256)  :: message
    logical             :: file_named


    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screeching halt as soon as we try to use anything.
       BEXP_TABLE = -1.E36
     SMCDRY_TABLE = -1.E36
         F1_TABLE = -1.E36
     SMCMAX_TABLE = -1.E36
     SMCREF_TABLE = -1.E36
     PSISAT_TABLE = -1.E36
      DKSAT_TABLE = -1.E36
      DWSAT_TABLE = -1.E36
     SMCWLT_TABLE = -1.E36
     QUARTZ_TABLE = -1.E36
      SLOPE_TABLE = -1.E36
      CSOIL_TABLE = -1.E36
      REFDK_TABLE = -1.E36
     REFKDT_TABLE = -1.E36
       FRZK_TABLE = -1.E36
       ZBOT_TABLE = -1.E36
       CZIL_TABLE = -1.E36
       BVIC_TABLE = -1.E36
       AXAJ_TABLE = -1.E36
       BXAJ_TABLE = -1.E36
       XXAJ_TABLE = -1.E36
      BDVIC_TABLE = -1.E36
      GDVIC_TABLE = -1.E36
      BBVIC_TABLE = -1.E36

!
!-----READ IN SOIL PROPERTIES FROM SOILPARM.TBL
!
    inquire( file='SOILPARM.TBL', exist=file_named )
    if ( file_named ) then
      open(21, file='SOILPARM.TBL',form='formatted',status='old',iostat=ierr)
    else
      open(21, form='formatted',status='old',iostat=ierr)
    end if

    IF(ierr .NE. 0 ) THEN
      WRITE(message,FMT='(A)') 'module_sf_noahmpdrv.F: read_mp_soil_parameters: failure opening SOILPARM.TBL'
    END IF

    READ (21,*)
    READ (21,*) SLTYPE
    READ (21,*) SLCATS
    WRITE( message , * ) 'SOIL TEXTURE CLASSIFICATION = ', TRIM ( SLTYPE ) , ' FOUND', &
               SLCATS,' CATEGORIES'

    DO LC=1,SLCATS
      READ (21,*) ITMP,BEXP_TABLE(LC),SMCDRY_TABLE(LC),F1_TABLE(LC),SMCMAX_TABLE(LC),   &
                  SMCREF_TABLE(LC),PSISAT_TABLE(LC),DKSAT_TABLE(LC), DWSAT_TABLE(LC),   &
                  SMCWLT_TABLE(LC),QUARTZ_TABLE(LC),BVIC_TABLE(LC), AXAJ_TABLE(LC),     &
                  BXAJ_TABLE(LC),XXAJ_TABLE(LC),BDVIC_TABLE(LC),BBVIC_TABLE(LC),GDVIC_TABLE(LC)
    ENDDO

    CLOSE (21)

!
!-----READ IN GENERAL PARAMETERS FROM GENPARM.TBL
!
    inquire( file='GENPARM.TBL', exist=file_named )
    if ( file_named ) then
      open(22, file='GENPARM.TBL',form='formatted',status='old',iostat=ierr)
    else
      open(22, form='formatted',status='old',iostat=ierr)
    end if

    IF(ierr .NE. 0 ) THEN
      WRITE(message,FMT='(A)') 'module_sf_noahlsm.F: read_mp_soil_parameters: failure opening GENPARM.TBL'
    END IF

    READ (22,*)
    READ (22,*)
    READ (22,*) NUM_SLOPE

    DO LC=1,NUM_SLOPE
        READ (22,*) SLOPE_TABLE(LC)
    ENDDO

    READ (22,*)
    READ (22,*)
    READ (22,*)
    READ (22,*)
    READ (22,*)
    READ (22,*) CSOIL_TABLE
    READ (22,*)
    READ (22,*)
    READ (22,*)
    READ (22,*) REFDK_TABLE
    READ (22,*)
    READ (22,*) REFKDT_TABLE
    READ (22,*)
    READ (22,*) FRZK_TABLE
    READ (22,*)
    READ (22,*) ZBOT_TABLE
    READ (22,*)
    READ (22,*) CZIL_TABLE
    READ (22,*)
    READ (22,*)
    READ (22,*)
    READ (22,*)

    CLOSE (22)

  end subroutine read_mp_soil_parameters

  subroutine read_mp_rad_parameters()
    implicit none
    integer :: ierr
    logical :: file_named

    REAL :: ALBICE(MBAND),ALBLAK(MBAND),OMEGAS(MBAND),BETADS,BETAIS,EG(2)
    REAL :: ALBSAT_VIS(MSC)
    REAL :: ALBSAT_NIR(MSC)
    REAL :: ALBDRY_VIS(MSC)
    REAL :: ALBDRY_NIR(MSC)

    NAMELIST / noahmp_rad_parameters / ALBSAT_VIS,ALBSAT_NIR,ALBDRY_VIS,ALBDRY_NIR,ALBICE,ALBLAK,OMEGAS,BETADS,BETAIS,EG


    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screeching halt as soon as we try to use anything.
    ALBSAT_TABLE     = -1.E36
    ALBDRY_TABLE     = -1.E36
    ALBICE_TABLE     = -1.E36
    ALBLAK_TABLE     = -1.E36
    OMEGAS_TABLE     = -1.E36
    BETADS_TABLE     = -1.E36
    BETAIS_TABLE     = -1.E36
    EG_TABLE         = -1.E36

    inquire( file='MPTABLE.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file MPTABLE.TBL")')
    endif

    read(15,noahmp_rad_parameters)
    close(15)

    ALBSAT_TABLE(:,1) = ALBSAT_VIS ! saturated soil albedos: 1=vis, 2=nir
    ALBSAT_TABLE(:,2) = ALBSAT_NIR ! saturated soil albedos: 1=vis, 2=nir
    ALBDRY_TABLE(:,1) = ALBDRY_VIS ! dry soil albedos: 1=vis, 2=nir
    ALBDRY_TABLE(:,2) = ALBDRY_NIR ! dry soil albedos: 1=vis, 2=nir
    ALBICE_TABLE      = ALBICE
    ALBLAK_TABLE      = ALBLAK
    OMEGAS_TABLE      = OMEGAS
    BETADS_TABLE      = BETADS
    BETAIS_TABLE      = BETAIS
    EG_TABLE          = EG

  end subroutine read_mp_rad_parameters


  subroutine read_mp_global_parameters()
    implicit none
    integer :: ierr
    logical :: file_named

    REAL :: CO2,O2,TIMEAN,FSATMX,Z0SNO,SSI,SNOW_RET_FAC,SNOW_EMIS,&
            SWEMX,TAU0,GRAIN_GROWTH,EXTRA_GROWTH,DIRT_SOOT,&
            BATS_COSZ,BATS_VIS_NEW,BATS_NIR_NEW,BATS_VIS_AGE,BATS_NIR_AGE,BATS_VIS_DIR,BATS_NIR_DIR,&
            RSURF_SNOW,RSURF_EXP

    NAMELIST / noahmp_global_parameters / CO2,O2,TIMEAN,FSATMX,Z0SNO,SSI,SNOW_RET_FAC,SNOW_EMIS,&
            SWEMX,TAU0,GRAIN_GROWTH,EXTRA_GROWTH,DIRT_SOOT,&
            BATS_COSZ,BATS_VIS_NEW,BATS_NIR_NEW,BATS_VIS_AGE,BATS_NIR_AGE,BATS_VIS_DIR,BATS_NIR_DIR,&
            RSURF_SNOW,RSURF_EXP


    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screeching halt as soon as we try to use anything.
       CO2_TABLE     = -1.E36
        O2_TABLE     = -1.E36
    TIMEAN_TABLE     = -1.E36
    FSATMX_TABLE     = -1.E36
     Z0SNO_TABLE     = -1.E36
       SSI_TABLE     = -1.E36
SNOW_RET_FAC_TABLE   = -1.E36
   SNOW_EMIS_TABLE   = -1.E36
       SWEMX_TABLE   = -1.E36
        TAU0_TABLE   = -1.E36
GRAIN_GROWTH_TABLE   = -1.E36
EXTRA_GROWTH_TABLE   = -1.E36
   DIRT_SOOT_TABLE   = -1.E36
   BATS_COSZ_TABLE   = -1.E36
BATS_VIS_NEW_TABLE   = -1.E36
BATS_NIR_NEW_TABLE   = -1.E36
BATS_VIS_AGE_TABLE   = -1.E36
BATS_NIR_AGE_TABLE   = -1.E36
BATS_VIS_DIR_TABLE   = -1.E36
BATS_NIR_DIR_TABLE   = -1.E36
RSURF_SNOW_TABLE     = -1.E36
 RSURF_EXP_TABLE     = -1.E36

    inquire( file='MPTABLE.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file MPTABLE.TBL")')
    endif

    read(15,noahmp_global_parameters)
    close(15)

       CO2_TABLE     = CO2
        O2_TABLE     = O2
    TIMEAN_TABLE     = TIMEAN
    FSATMX_TABLE     = FSATMX
     Z0SNO_TABLE     = Z0SNO
       SSI_TABLE     = SSI
SNOW_RET_FAC_TABLE   = SNOW_RET_FAC
   SNOW_EMIS_TABLE   = SNOW_EMIS
     SWEMX_TABLE     = SWEMX
        TAU0_TABLE   = TAU0
GRAIN_GROWTH_TABLE   = GRAIN_GROWTH
EXTRA_GROWTH_TABLE   = EXTRA_GROWTH
   DIRT_SOOT_TABLE   = DIRT_SOOT
   BATS_COSZ_TABLE   = BATS_COSZ
BATS_VIS_NEW_TABLE   = BATS_VIS_NEW
BATS_NIR_NEW_TABLE   = BATS_NIR_NEW
BATS_VIS_AGE_TABLE   = BATS_VIS_AGE
BATS_NIR_AGE_TABLE   = BATS_NIR_AGE
BATS_VIS_DIR_TABLE   = BATS_VIS_DIR
BATS_NIR_DIR_TABLE   = BATS_NIR_DIR
RSURF_SNOW_TABLE     = RSURF_SNOW
 RSURF_EXP_TABLE     = RSURF_EXP

  end subroutine read_mp_global_parameters

  subroutine read_mp_crop_parameters()
    implicit none
    integer :: ierr
    logical :: file_named

 INTEGER                   :: DEFAULT_CROP
 INTEGER, DIMENSION(NCROP) :: PLTDAY
 INTEGER, DIMENSION(NCROP) :: HSDAY
    REAL, DIMENSION(NCROP) :: PLANTPOP
    REAL, DIMENSION(NCROP) :: IRRI
    REAL, DIMENSION(NCROP) :: GDDTBASE
    REAL, DIMENSION(NCROP) :: GDDTCUT
    REAL, DIMENSION(NCROP) :: GDDS1
    REAL, DIMENSION(NCROP) :: GDDS2
    REAL, DIMENSION(NCROP) :: GDDS3
    REAL, DIMENSION(NCROP) :: GDDS4
    REAL, DIMENSION(NCROP) :: GDDS5
    REAL, DIMENSION(NCROP) :: C3PSN   ! this session copied from stomata parameters Zhe Zhang 2020-07-13
    REAL, DIMENSION(NCROP) :: KC25
    REAL, DIMENSION(NCROP) :: AKC
    REAL, DIMENSION(NCROP) :: KO25
    REAL, DIMENSION(NCROP) :: AKO
    REAL, DIMENSION(NCROP) :: AVCMX
    REAL, DIMENSION(NCROP) :: VCMX25
    REAL, DIMENSION(NCROP) :: BP
    REAL, DIMENSION(NCROP) :: MP
    REAL, DIMENSION(NCROP) :: FOLNMX
    REAL, DIMENSION(NCROP) :: QE25    ! until here
 INTEGER, DIMENSION(NCROP) :: C3C4
    REAL, DIMENSION(NCROP) :: AREF
    REAL, DIMENSION(NCROP) :: PSNRF
    REAL, DIMENSION(NCROP) :: I2PAR
    REAL, DIMENSION(NCROP) :: TASSIM0
    REAL, DIMENSION(NCROP) :: TASSIM1
    REAL, DIMENSION(NCROP) :: TASSIM2
    REAL, DIMENSION(NCROP) :: K
    REAL, DIMENSION(NCROP) :: EPSI
    REAL, DIMENSION(NCROP) :: Q10MR
    REAL, DIMENSION(NCROP) :: FOLN_MX
    REAL, DIMENSION(NCROP) :: LEFREEZ
    REAL, DIMENSION(NCROP) :: DILE_FC_S1,DILE_FC_S2,DILE_FC_S3,DILE_FC_S4,DILE_FC_S5,DILE_FC_S6,DILE_FC_S7,DILE_FC_S8
    REAL, DIMENSION(NCROP) :: DILE_FW_S1,DILE_FW_S2,DILE_FW_S3,DILE_FW_S4,DILE_FW_S5,DILE_FW_S6,DILE_FW_S7,DILE_FW_S8
    REAL, DIMENSION(NCROP) :: FRA_GR
    REAL, DIMENSION(NCROP) :: LF_OVRC_S1,LF_OVRC_S2,LF_OVRC_S3,LF_OVRC_S4,LF_OVRC_S5,LF_OVRC_S6,LF_OVRC_S7,LF_OVRC_S8
    REAL, DIMENSION(NCROP) :: ST_OVRC_S1,ST_OVRC_S2,ST_OVRC_S3,ST_OVRC_S4,ST_OVRC_S5,ST_OVRC_S6,ST_OVRC_S7,ST_OVRC_S8
    REAL, DIMENSION(NCROP) :: RT_OVRC_S1,RT_OVRC_S2,RT_OVRC_S3,RT_OVRC_S4,RT_OVRC_S5,RT_OVRC_S6,RT_OVRC_S7,RT_OVRC_S8
    REAL, DIMENSION(NCROP) :: LFMR25
    REAL, DIMENSION(NCROP) :: STMR25
    REAL, DIMENSION(NCROP) :: RTMR25
    REAL, DIMENSION(NCROP) :: GRAINMR25
    REAL, DIMENSION(NCROP) :: LFPT_S1,LFPT_S2,LFPT_S3,LFPT_S4,LFPT_S5,LFPT_S6,LFPT_S7,LFPT_S8
    REAL, DIMENSION(NCROP) :: STPT_S1,STPT_S2,STPT_S3,STPT_S4,STPT_S5,STPT_S6,STPT_S7,STPT_S8
    REAL, DIMENSION(NCROP) :: RTPT_S1,RTPT_S2,RTPT_S3,RTPT_S4,RTPT_S5,RTPT_S6,RTPT_S7,RTPT_S8
    REAL, DIMENSION(NCROP) :: GRAINPT_S1,GRAINPT_S2,GRAINPT_S3,GRAINPT_S4,GRAINPT_S5,GRAINPT_S6,GRAINPT_S7,GRAINPT_S8
    REAL, DIMENSION(NCROP) :: LFCT_S1,LFCT_S2,LFCT_S3,LFCT_S4,LFCT_S5,LFCT_S6,LFCT_S7,LFCT_S8
    REAL, DIMENSION(NCROP) :: STCT_S1,STCT_S2,STCT_S3,STCT_S4,STCT_S5,STCT_S6,STCT_S7,STCT_S8
    REAL, DIMENSION(NCROP) :: RTCT_S1,RTCT_S2,RTCT_S3,RTCT_S4,RTCT_S5,RTCT_S6,RTCT_S7,RTCT_S8
    REAL, DIMENSION(NCROP) :: BIO2LAI
!    NAMELIST / noahmp_crop_parameters /DEFAULT_CROP,   PLTDAY,     HSDAY,  PLANTPOP,      IRRI,  GDDTBASE,   GDDTCUT,     GDDS1,     GDDS2, &
!                                             GDDS3,     GDDS4,     GDDS5,      C3C4,      AREF,     PSNRF,     I2PAR,   TASSIM0, &
!                                           TASSIM1,   TASSIM2,         K,      EPSI,     Q10MR,   FOLN_MX,   LEFREEZ,            &
! Zhe Zhang 2020-07-13
    NAMELIST / noahmp_crop_parameters /DEFAULT_CROP,   PLTDAY,     HSDAY,  PLANTPOP,      IRRI,  GDDTBASE,   GDDTCUT,     GDDS1,  GDDS2,  GDDS3,     GDDS4,     GDDS5, & !
                                              C3PSN,     KC25,       AKC,      KO25,       AKO,     AVCMX,    VCMX25,        BP,     MP, FOLNMX,      QE25, &  ! parameters added from stomata
                                               C3C4,     AREF,     PSNRF,     I2PAR,   TASSIM0,                                               &
                                        TASSIM1,   TASSIM2,         K,      EPSI,     Q10MR,   FOLN_MX,   LEFREEZ,               &
                                        DILE_FC_S1,DILE_FC_S2,DILE_FC_S3,DILE_FC_S4,DILE_FC_S5,DILE_FC_S6,DILE_FC_S7,DILE_FC_S8, &
                                        DILE_FW_S1,DILE_FW_S2,DILE_FW_S3,DILE_FW_S4,DILE_FW_S5,DILE_FW_S6,DILE_FW_S7,DILE_FW_S8, &
                                            FRA_GR,                                                                              &
                                        LF_OVRC_S1,LF_OVRC_S2,LF_OVRC_S3,LF_OVRC_S4,LF_OVRC_S5,LF_OVRC_S6,LF_OVRC_S7,LF_OVRC_S8, &
                                        ST_OVRC_S1,ST_OVRC_S2,ST_OVRC_S3,ST_OVRC_S4,ST_OVRC_S5,ST_OVRC_S6,ST_OVRC_S7,ST_OVRC_S8, &
                                        RT_OVRC_S1,RT_OVRC_S2,RT_OVRC_S3,RT_OVRC_S4,RT_OVRC_S5,RT_OVRC_S6,RT_OVRC_S7,RT_OVRC_S8, &
                                            LFMR25,    STMR25,    RTMR25, GRAINMR25,                                             &
                                           LFPT_S1,   LFPT_S2,   LFPT_S3,   LFPT_S4,   LFPT_S5,   LFPT_S6,   LFPT_S7,   LFPT_S8, &
                                           STPT_S1,   STPT_S2,   STPT_S3,   STPT_S4,   STPT_S5,   STPT_S6,   STPT_S7,   STPT_S8, &
                                           RTPT_S1,   RTPT_S2,   RTPT_S3,   RTPT_S4,   RTPT_S5,   RTPT_S6,   RTPT_S7,   RTPT_S8, &
                                        GRAINPT_S1,GRAINPT_S2,GRAINPT_S3,GRAINPT_S4,GRAINPT_S5,GRAINPT_S6,GRAINPT_S7,GRAINPT_S8, &
                                           LFCT_S1,LFCT_S2,LFCT_S3,LFCT_S4,LFCT_S5,LFCT_S6,LFCT_S7,LFCT_S8,                      &
                                           STCT_S1,STCT_S2,STCT_S3,STCT_S4,STCT_S5,STCT_S6,STCT_S7,STCT_S8,                      &
                                           RTCT_S1,RTCT_S2,RTCT_S3,RTCT_S4,RTCT_S5,RTCT_S6,RTCT_S7,RTCT_S8,                      &
                                           BIO2LAI


    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screeching halt as soon as we try to use anything.
 DEFAULT_CROP_TABLE     = -99999
       PLTDAY_TABLE     = -99999
        HSDAY_TABLE     = -99999
     PLANTPOP_TABLE     = -1.E36
         IRRI_TABLE     = -1.E36
     GDDTBASE_TABLE     = -1.E36
      GDDTCUT_TABLE     = -1.E36
        GDDS1_TABLE     = -1.E36
        GDDS2_TABLE     = -1.E36
        GDDS3_TABLE     = -1.E36
        GDDS4_TABLE     = -1.E36
        GDDS5_TABLE     = -1.E36
       C3PSNI_TABLE     = -1.E36 ! parameter from PSN copied from stomata ! Zhe Zhang 2020-07-13
        KC25I_TABLE     = -1.E36
         AKCI_TABLE     = -1.E36
        KO25I_TABLE     = -1.E36
         AKOI_TABLE     = -1.E36
       AVCMXI_TABLE     = -1.E36
      VCMX25I_TABLE     = -1.E36
          BPI_TABLE     = -1.E36
          MPI_TABLE     = -1.E36
      FOLNMXI_TABLE     = -1.E36
        QE25I_TABLE     = -1.E36 ! ends here
         C3C4_TABLE     = -99999
         AREF_TABLE     = -1.E36
        PSNRF_TABLE     = -1.E36
        I2PAR_TABLE     = -1.E36
      TASSIM0_TABLE     = -1.E36
      TASSIM1_TABLE     = -1.E36
      TASSIM2_TABLE     = -1.E36
            K_TABLE     = -1.E36
         EPSI_TABLE     = -1.E36
        Q10MR_TABLE     = -1.E36
      FOLN_MX_TABLE     = -1.E36
      LEFREEZ_TABLE     = -1.E36
      DILE_FC_TABLE     = -1.E36
      DILE_FW_TABLE     = -1.E36
       FRA_GR_TABLE     = -1.E36
      LF_OVRC_TABLE     = -1.E36
      ST_OVRC_TABLE     = -1.E36
      RT_OVRC_TABLE     = -1.E36
       LFMR25_TABLE     = -1.E36
       STMR25_TABLE     = -1.E36
       RTMR25_TABLE     = -1.E36
    GRAINMR25_TABLE     = -1.E36
         LFPT_TABLE     = -1.E36
         STPT_TABLE     = -1.E36
         RTPT_TABLE     = -1.E36
      GRAINPT_TABLE     = -1.E36
         LFCT_TABLE     = -1.E36 ! convert start
         STCT_TABLE     = -1.E36
         RTCT_TABLE     = -1.E36 ! convert end
      BIO2LAI_TABLE     = -1.E36


    inquire( file='MPTABLE.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file MPTABLE.TBL")')
    endif

    read(15,noahmp_crop_parameters)
    close(15)

 DEFAULT_CROP_TABLE      = DEFAULT_CROP
       PLTDAY_TABLE      = PLTDAY
        HSDAY_TABLE      = HSDAY
     PLANTPOP_TABLE      = PLANTPOP
         IRRI_TABLE      = IRRI
     GDDTBASE_TABLE      = GDDTBASE
      GDDTCUT_TABLE      = GDDTCUT
        GDDS1_TABLE      = GDDS1
        GDDS2_TABLE      = GDDS2
        GDDS3_TABLE      = GDDS3
        GDDS4_TABLE      = GDDS4
        GDDS5_TABLE      = GDDS5
       C3PSNI_TABLE(1:5) = C3PSN(1:5)  ! parameters from stomata ! Zhe Zhang 2020-07-13
        KC25I_TABLE(1:5) = KC25(1:5)
         AKCI_TABLE(1:5) = AKC(1:5)
        KO25I_TABLE(1:5) = KO25(1:5)
         AKOI_TABLE(1:5) = AKO(1:5)
       AVCMXI_TABLE(1:5) = AVCMX(1:5)
      VCMX25I_TABLE(1:5) = VCMX25(1:5)
          BPI_TABLE(1:5) = BP(1:5)
          MPI_TABLE(1:5) = MP(1:5)
      FOLNMXI_TABLE(1:5) = FOLNMX(1:5)
        QE25I_TABLE(1:5) = QE25(1:5)   ! ends here
         C3C4_TABLE      = C3C4
         AREF_TABLE      = AREF
        PSNRF_TABLE      = PSNRF
        I2PAR_TABLE      = I2PAR
      TASSIM0_TABLE      = TASSIM0
      TASSIM1_TABLE      = TASSIM1
      TASSIM2_TABLE      = TASSIM2
            K_TABLE      = K
         EPSI_TABLE      = EPSI
        Q10MR_TABLE      = Q10MR
      FOLN_MX_TABLE      = FOLN_MX
      LEFREEZ_TABLE      = LEFREEZ
      DILE_FC_TABLE(:,1) = DILE_FC_S1
      DILE_FC_TABLE(:,2) = DILE_FC_S2
      DILE_FC_TABLE(:,3) = DILE_FC_S3
      DILE_FC_TABLE(:,4) = DILE_FC_S4
      DILE_FC_TABLE(:,5) = DILE_FC_S5
      DILE_FC_TABLE(:,6) = DILE_FC_S6
      DILE_FC_TABLE(:,7) = DILE_FC_S7
      DILE_FC_TABLE(:,8) = DILE_FC_S8
      DILE_FW_TABLE(:,1) = DILE_FW_S1
      DILE_FW_TABLE(:,2) = DILE_FW_S2
      DILE_FW_TABLE(:,3) = DILE_FW_S3
      DILE_FW_TABLE(:,4) = DILE_FW_S4
      DILE_FW_TABLE(:,5) = DILE_FW_S5
      DILE_FW_TABLE(:,6) = DILE_FW_S6
      DILE_FW_TABLE(:,7) = DILE_FW_S7
      DILE_FW_TABLE(:,8) = DILE_FW_S8
       FRA_GR_TABLE      = FRA_GR
      LF_OVRC_TABLE(:,1) = LF_OVRC_S1
      LF_OVRC_TABLE(:,2) = LF_OVRC_S2
      LF_OVRC_TABLE(:,3) = LF_OVRC_S3
      LF_OVRC_TABLE(:,4) = LF_OVRC_S4
      LF_OVRC_TABLE(:,5) = LF_OVRC_S5
      LF_OVRC_TABLE(:,6) = LF_OVRC_S6
      LF_OVRC_TABLE(:,7) = LF_OVRC_S7
      LF_OVRC_TABLE(:,8) = LF_OVRC_S8
      ST_OVRC_TABLE(:,1) = ST_OVRC_S1
      ST_OVRC_TABLE(:,2) = ST_OVRC_S2
      ST_OVRC_TABLE(:,3) = ST_OVRC_S3
      ST_OVRC_TABLE(:,4) = ST_OVRC_S4
      ST_OVRC_TABLE(:,5) = ST_OVRC_S5
      ST_OVRC_TABLE(:,6) = ST_OVRC_S6
      ST_OVRC_TABLE(:,7) = ST_OVRC_S7
      ST_OVRC_TABLE(:,8) = ST_OVRC_S8
      RT_OVRC_TABLE(:,1) = RT_OVRC_S1
      RT_OVRC_TABLE(:,2) = RT_OVRC_S2
      RT_OVRC_TABLE(:,3) = RT_OVRC_S3
      RT_OVRC_TABLE(:,4) = RT_OVRC_S4
      RT_OVRC_TABLE(:,5) = RT_OVRC_S5
      RT_OVRC_TABLE(:,6) = RT_OVRC_S6
      RT_OVRC_TABLE(:,7) = RT_OVRC_S7
      RT_OVRC_TABLE(:,8) = RT_OVRC_S8
       LFMR25_TABLE      = LFMR25
       STMR25_TABLE      = STMR25
       RTMR25_TABLE      = RTMR25
    GRAINMR25_TABLE      = GRAINMR25
         LFPT_TABLE(:,1) = LFPT_S1
         LFPT_TABLE(:,2) = LFPT_S2
         LFPT_TABLE(:,3) = LFPT_S3
         LFPT_TABLE(:,4) = LFPT_S4
         LFPT_TABLE(:,5) = LFPT_S5
         LFPT_TABLE(:,6) = LFPT_S6
         LFPT_TABLE(:,7) = LFPT_S7
         LFPT_TABLE(:,8) = LFPT_S8
         STPT_TABLE(:,1) = STPT_S1
         STPT_TABLE(:,2) = STPT_S2
         STPT_TABLE(:,3) = STPT_S3
         STPT_TABLE(:,4) = STPT_S4
         STPT_TABLE(:,5) = STPT_S5
         STPT_TABLE(:,6) = STPT_S6
         STPT_TABLE(:,7) = STPT_S7
         STPT_TABLE(:,8) = STPT_S8
         RTPT_TABLE(:,1) = RTPT_S1
         RTPT_TABLE(:,2) = RTPT_S2
         RTPT_TABLE(:,3) = RTPT_S3
         RTPT_TABLE(:,4) = RTPT_S4
         RTPT_TABLE(:,5) = RTPT_S5
         RTPT_TABLE(:,6) = RTPT_S6
         RTPT_TABLE(:,7) = RTPT_S7
         RTPT_TABLE(:,8) = RTPT_S8
      GRAINPT_TABLE(:,1) = GRAINPT_S1
      GRAINPT_TABLE(:,2) = GRAINPT_S2
      GRAINPT_TABLE(:,3) = GRAINPT_S3
      GRAINPT_TABLE(:,4) = GRAINPT_S4
      GRAINPT_TABLE(:,5) = GRAINPT_S5
      GRAINPT_TABLE(:,6) = GRAINPT_S6
      GRAINPT_TABLE(:,7) = GRAINPT_S7
      GRAINPT_TABLE(:,8) = GRAINPT_S8
         LFCT_TABLE(:,1) = LFCT_S1
         LFCT_TABLE(:,2) = LFCT_S2
         LFCT_TABLE(:,3) = LFCT_S3
         LFCT_TABLE(:,4) = LFCT_S4
         LFCT_TABLE(:,5) = LFCT_S5
         LFCT_TABLE(:,6) = LFCT_S6
         LFCT_TABLE(:,7) = LFCT_S7
         LFCT_TABLE(:,8) = LFCT_S8
         STCT_TABLE(:,1) = STCT_S1
         STCT_TABLE(:,2) = STCT_S2
         STCT_TABLE(:,3) = STCT_S3
         STCT_TABLE(:,4) = STCT_S4
         STCT_TABLE(:,5) = STCT_S5
         STCT_TABLE(:,6) = STCT_S6
         STCT_TABLE(:,7) = STCT_S7
         STCT_TABLE(:,8) = STCT_S8
         RTCT_TABLE(:,1) = RTCT_S1
         RTCT_TABLE(:,2) = RTCT_S2
         RTCT_TABLE(:,3) = RTCT_S3
         RTCT_TABLE(:,4) = RTCT_S4
         RTCT_TABLE(:,5) = RTCT_S5
         RTCT_TABLE(:,6) = RTCT_S6
         RTCT_TABLE(:,7) = RTCT_S7
         RTCT_TABLE(:,8) = RTCT_S8
      BIO2LAI_TABLE      = BIO2LAI

  end subroutine read_mp_crop_parameters

  subroutine read_mp_irrigation_parameters()
    implicit none
    integer :: ierr
    logical :: file_named

    REAL    :: IRR_FRAC              ! irrigation Fraction
    INTEGER :: IRR_HAR               ! number of days before harvest date to stop irrigation 
    REAL    :: IRR_LAI               ! Minimum lai to trigger irrigation
    REAL    :: IRR_MAD               ! management allowable deficit (0-1)
    REAL    :: FILOSS                ! fraction of flood irrigation loss (0-1) 
    REAL    :: SPRIR_RATE            ! mm/h, sprinkler irrigation rate
    REAL    :: MICIR_RATE            ! mm/h, micro irrigation rate
    REAL    :: FIRTFAC               ! flood application rate factor
    REAL    :: IR_RAIN               ! maximum precipitation to stop irrigation trigger

    NAMELIST / noahmp_irrigation_parameters / IRR_FRAC, IRR_HAR, IRR_LAI, IRR_MAD, FILOSS, &
                                              SPRIR_RATE, MICIR_RATE, FIRTFAC, IR_RAIN

    IRR_FRAC_TABLE   = -1.E36    ! irrigation Fraction
    IRR_HAR_TABLE    =  0        ! number of days before harvest date to stop irrigation 
    IRR_LAI_TABLE    = -1.E36    ! Minimum lai to trigger irrigation
    IRR_MAD_TABLE    = -1.E36    ! management allowable deficit (0-1)
    FILOSS_TABLE     = -1.E36    ! fraction of flood irrigation loss (0-1) 
    SPRIR_RATE_TABLE = -1.E36    ! mm/h, sprinkler irrigation rate
    MICIR_RATE_TABLE = -1.E36    ! mm/h, micro irrigation rate
    FIRTFAC_TABLE    = -1.E36    ! flood application rate factor
    IR_RAIN_TABLE    = -1.E36    ! maximum precipitation to stop irrigation trigger

    inquire( file='MPTABLE.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file MPTABLE.TBL")')
    endif

    read(15,noahmp_irrigation_parameters)
    close(15)

    IRR_FRAC_TABLE   = IRR_FRAC    ! irrigation Fraction
    IRR_HAR_TABLE    = IRR_HAR     ! number of days before harvest date to stop irrigation 
    IRR_LAI_TABLE    = IRR_LAI     ! Minimum lai to trigger irrigation
    IRR_MAD_TABLE    = IRR_MAD     ! management allowable deficit (0-1)
    FILOSS_TABLE     = FILOSS      ! fraction of flood irrigation loss (0-1) 
    SPRIR_RATE_TABLE = SPRIR_RATE  ! mm/h, sprinkler irrigation rate
    MICIR_RATE_TABLE = MICIR_RATE  ! mm/h, micro irrigation rate
    FIRTFAC_TABLE    = FIRTFAC     ! flood application rate factor
    IR_RAIN_TABLE    = IR_RAIN     ! maximum precipitation to stop irrigation trigger

  end subroutine read_mp_irrigation_parameters

  subroutine read_tiledrain_parameters()
    implicit none
    integer :: ierr
    logical :: file_named
    REAL, DIMENSION(MAX_SOILTYP)    :: TDSMC_FAC
    INTEGER, DIMENSION(MAX_SOILTYP) :: TD_DEPTH
    REAL, DIMENSION(MAX_SOILTYP)    :: TD_DC
    INTEGER                         :: DRAIN_LAYER_OPT
    REAL, DIMENSION(MAX_SOILTYP)    :: TD_DCOEF
    REAL, DIMENSION(MAX_SOILTYP)    :: TD_D
    REAL, DIMENSION(MAX_SOILTYP)    :: TD_ADEPTH
    REAL, DIMENSION(MAX_SOILTYP)    :: TD_RADI
    REAL, DIMENSION(MAX_SOILTYP)    :: TD_SPAC
    REAL, DIMENSION(MAX_SOILTYP)    :: TD_DDRAIN
    REAL, DIMENSION(MAX_SOILTYP)    :: KLAT_FAC
    NAMELIST / noahmp_tiledrain_parameters /DRAIN_LAYER_OPT,TDSMC_FAC,TD_DEPTH,TD_DC,&
                                           TD_DCOEF,TD_D,TD_ADEPTH,TD_RADI,TD_SPAC,TD_DDRAIN,&
                                           KLAT_FAC
    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screeching halt as soon as we try to use anything.
    TDSMCFAC_TABLE           = -99999
    TD_DEPTH_TABLE           = -99999
    TD_DC_TABLE              = -99999
    DRAIN_LAYER_OPT_TABLE    = -99999
    TD_DCOEF_TABLE           = -99999
    TD_D_TABLE               = -99999
    TD_ADEPTH_TABLE          = -99999
    TD_RADI_TABLE            = -99999
    TD_SPAC_TABLE            = -99999
    TD_DDRAIN_TABLE          = -99999
    KLAT_FAC_TABLE           = -99999

    inquire( file='MPTABLE.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file MPTABLE.TBL")')
    endif
    read(15,noahmp_tiledrain_parameters)
    close(15)
    TDSMCFAC_TABLE           = TDSMC_FAC
    TD_DEPTH_TABLE           = TD_DEPTH
    DRAIN_LAYER_OPT_TABLE    = DRAIN_LAYER_OPT
    TD_DC_TABLE              = TD_DC

    TD_DCOEF_TABLE           = TD_DCOEF
    TD_D_TABLE               = TD_D
    TD_ADEPTH_TABLE          = TD_ADEPTH
    TD_RADI_TABLE            = TD_RADI
    TD_SPAC_TABLE            = TD_SPAC
    TD_DDRAIN_TABLE          = TD_DDRAIN
    KLAT_FAC_TABLE           = KLAT_FAC

  end subroutine read_tiledrain_parameters

  subroutine read_mp_optional_parameters()
    implicit none
    integer :: ierr
    logical :: file_named

    NAMELIST / noahmp_optional_parameters /                                &
         sr2006_theta_1500t_a, sr2006_theta_1500t_b, sr2006_theta_1500t_c, &
         sr2006_theta_1500t_d, sr2006_theta_1500t_e, sr2006_theta_1500t_f, &
         sr2006_theta_1500t_g                                            , &
         sr2006_theta_1500_a , sr2006_theta_1500_b                       , &
         sr2006_theta_33t_a  , sr2006_theta_33t_b  , sr2006_theta_33t_c  , &
         sr2006_theta_33t_d  , sr2006_theta_33t_e  , sr2006_theta_33t_f  , &
         sr2006_theta_33t_g                                              , &
         sr2006_theta_33_a   , sr2006_theta_33_b   , sr2006_theta_33_c   , &
         sr2006_theta_s33t_a , sr2006_theta_s33t_b , sr2006_theta_s33t_c , &
         sr2006_theta_s33t_d , sr2006_theta_s33t_e , sr2006_theta_s33t_f , &
         sr2006_theta_s33t_g                                             , &
         sr2006_theta_s33_a  , sr2006_theta_s33_b                        , &
         sr2006_psi_et_a     , sr2006_psi_et_b     , sr2006_psi_et_c     , &
         sr2006_psi_et_d     , sr2006_psi_et_e     , sr2006_psi_et_f     , &
         sr2006_psi_et_g                                                 , &
         sr2006_psi_e_a      , sr2006_psi_e_b      , sr2006_psi_e_c      , &
         sr2006_smcmax_a     , sr2006_smcmax_b

    inquire( file='MPTABLE.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file MPTABLE.TBL")')
    endif

    read(15,noahmp_optional_parameters)
    close(15)


  end subroutine read_mp_optional_parameters


END MODULE NOAHMP_TABLES

