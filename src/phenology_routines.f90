module phenology_routines
  
  IMPLICIT NONE

  public  :: noahmp_options
  public  :: PHENOLOGY


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

!== begin phenology ================================================================================

  SUBROUTINE PHENOLOGY (parameters,VEGTYP ,croptype, SNOWH  , TV     , LAT   , YEARLEN , JULIAN , & !in
                        LAI    , SAI    , TROOT  , ELAI    , ESAI   , IGS, PGS)

! --------------------------------------------------------------------------------------------------
! vegetation phenology considering vegeation canopy being buries by snow and evolution in time
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER                , INTENT(IN   ) :: VEGTYP !vegetation type 
  INTEGER                , INTENT(IN   ) :: CROPTYPE !vegetation type 
  REAL                   , INTENT(IN   ) :: SNOWH  !snow height [m]
  REAL                   , INTENT(IN   ) :: TV     !vegetation temperature (k)
  REAL                   , INTENT(IN   ) :: LAT    !latitude (radians)
  INTEGER                , INTENT(IN   ) :: YEARLEN!Number of days in the particular year
  REAL                   , INTENT(IN   ) :: JULIAN !Julian day of year (fractional) ( 0 <= JULIAN < YEARLEN )
  real                   , INTENT(IN   ) :: TROOT  !root-zone averaged temperature (k)
  REAL                   , INTENT(INOUT) :: LAI    !LAI, unadjusted for burying by snow
  REAL                   , INTENT(INOUT) :: SAI    !SAI, unadjusted for burying by snow

! outputs
  REAL                   , INTENT(OUT  ) :: ELAI   !leaf area index, after burying by snow
  REAL                   , INTENT(OUT  ) :: ESAI   !stem area index, after burying by snow
  REAL                   , INTENT(OUT  ) :: IGS    !growing season index (0=off, 1=on)
  INTEGER                , INTENT(IN   ) :: PGS    !plant growing stage

! locals

  REAL                                   :: DB     !thickness of canopy buried by snow (m)
  REAL                                   :: FB     !fraction of canopy buried by snow
  REAL                                   :: SNOWHC !critical snow depth at which short vege
                                                   !is fully covered by snow

  INTEGER                                :: K       !index
  INTEGER                                :: IT1,IT2 !interpolation months
  REAL                                   :: DAY     !current day of year ( 0 <= DAY < YEARLEN )
  REAL                                   :: WT1,WT2 !interpolation weights
  REAL                                   :: T       !current month (1.00, ..., 12.00)
! --------------------------------------------------------------------------------------------------

IF (CROPTYPE == 0) THEN

  IF ( DVEG == 1 .or. DVEG == 3 .or. DVEG == 4 ) THEN

     IF (LAT >= 0.0) THEN
        ! Northern Hemisphere
        DAY = JULIAN
     ELSE
        ! Southern Hemisphere.  DAY is shifted by 1/2 year.
        DAY = MOD ( JULIAN + ( 0.5 * YEARLEN ) , REAL(YEARLEN) )
     ENDIF

     T = 12.0 * DAY / REAL(YEARLEN)
     IT1 = T + 0.5
     IT2 = IT1 + 1
     WT1 = (IT1+0.5) - T
     WT2 = 1.0-WT1
     IF (IT1 .LT.  1) IT1 = 12
     IF (IT2 .GT. 12) IT2 = 1
     
     LAI = WT1*parameters%LAIM(IT1) + WT2*parameters%LAIM(IT2)
     SAI = WT1*parameters%SAIM(IT1) + WT2*parameters%SAIM(IT2)
  ENDIF

  IF(DVEG == 7 .or. DVEG == 8 .or. DVEG == 9) THEN
    SAI = MAX(0.05,0.1 * LAI)  ! when reading LAI, set SAI to 10% LAI, but not below 0.05 MB: v3.8
    IF (LAI < 0.05) SAI = 0.0  ! if LAI below minimum, make sure SAI = 0
  ENDIF

  IF (SAI < 0.05) SAI = 0.0                    ! MB: SAI CHECK, change to 0.05 v3.6
  IF (LAI < 0.05 .OR. SAI == 0.0) LAI = 0.0  ! MB: LAI CHECK

  IF ( ( VEGTYP == parameters%iswater ) .OR. ( VEGTYP == parameters%ISBARREN ) .OR. &
       ( VEGTYP == parameters%ISICE   ) .or. ( parameters%urban_flag ) ) THEN
     LAI  = 0.0
     SAI  = 0.0
  ENDIF

ENDIF   ! CROPTYPE == 0

!buried by snow

     DB = MIN( MAX(SNOWH - parameters%HVB,0.0), parameters%HVT-parameters%HVB )
     FB = DB / MAX(1.E-06,parameters%HVT-parameters%HVB)

     IF(parameters%HVT> 0.0 .AND. parameters%HVT <= 1.0) THEN          !MB: change to 1.0 and 0.2 to reflect
       SNOWHC = parameters%HVT*EXP(-SNOWH/0.2)             !      changes to HVT in MPTABLE
       FB     = MIN(SNOWH,SNOWHC)/SNOWHC
     ENDIF

     ELAI =  LAI*(1.0-FB)
     ESAI =  SAI*(1.0-FB)
     IF (ESAI < 0.05 .and. CROPTYPE == 0) ESAI = 0.0                   ! MB: ESAI CHECK, change to 0.05 v3.6
     IF ((ELAI < 0.05 .OR. ESAI == 0.0) .and. CROPTYPE == 0) ELAI = 0.0  ! MB: LAI CHECK

! set growing season flag

     IF ((TV .GT. parameters%TMIN .and. CROPTYPE == 0).or.(PGS > 2 .and. PGS < 7 .and. CROPTYPE > 0)) THEN
         IGS = 1.
     ELSE
         IGS = 0.
     ENDIF

     !print*,'ELAI=',ELAI
     !print*,'ESAI=',ESAI
     !print*,'LAI=', LAI
     !print*,'SAI=', SAI

  END SUBROUTINE PHENOLOGY

! noah-mp options

  subroutine noahmp_options(idveg)
 
   implicit none

    INTEGER,  INTENT(IN) :: idveg  !rainfall & snowfall (1-Jordan91; 2->BATS; 3->Noah)

     dveg  = idveg  

  end subroutine noahmp_options

end module phenology_routines


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
