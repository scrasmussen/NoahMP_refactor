module water_routines

  TYPE noahmp_parameters
     real, allocatable, dimension(:) :: bexp   ! b parameter
     real, allocatable, dimension(:) :: smcmax ! porosity (volumetric)
     real, allocatable, dimension(:) :: smcwlt ! wilting point
     real, allocatable, dimension(:) :: smcref ! field capacity
     real, allocatable, dimension(:) :: dksat  ! saturated conductivity
     real, allocatable, dimension(:) :: dwsat  ! saturated diffusivity
     real, allocatable, dimension(:) :: psisat ! saturated matric potential
     real                            :: kdt    !
     real                            :: frzx   !
     real                            :: slope  ! drainage parameter
     real                            :: CH2OP  !maximum intercepted h2o per unit lai+sai (mm)    
     real                            :: SAIM   !monthly stem area index, one-sided
     real                            :: LAIM   !monthly leaf area index, one-sided
!addrunoff
     real                            :: bvic   !VIC model infiltration parameter
     real                            :: AXAJ          !Xinanjiang: Tension water distribution inflection parameter [-]
     real                            :: BXAJ          !Xinanjiang: Tension water distribution shape parameter [-]
     real                            :: XXAJ          !Xinanjiang: Free water distribution shape parameter [-]
     real                            :: G             !Mean Capillary Drive (m) for infiltration models
     real                            :: BBVIC         !DVIC heterogeniety parameter for infiltration 

     real :: timean = 10.5
     real :: fsatmx = 0.38
     logical :: urban_flag

  real :: GRAV   = 9.80616   !acceleration due to gravity (m/s2)
  real :: SB     = 5.67E-08  !Stefan-Boltzmann constant (w/m2/k4)
  real :: VKC    = 0.40      !von Karman constant
  real :: TFRZ   = 273.16    !freezing/melting point (k)
  real :: HSUB   = 2.8440E06 !latent heat of sublimation (j/kg)
  real :: HVAP   = 2.5104E06 !latent heat of vaporization (j/kg)
  real :: HFUS   = 0.3336E06 !latent heat of fusion (j/kg)
  real :: CWAT   = 4.188E06  !specific heat capacity of water (j/m3/k)
  real :: CICE   = 2.094E06  !specific heat capacity of ice (j/m3/k)
  real :: CPAIR  = 1004.64   !heat capacity dry air at const pres (j/kg/k)
  real :: TKWAT  = 0.6       !thermal conductivity of water (w/m/k)
  real :: TKICE  = 2.2       !thermal conductivity of ice (w/m/k)
  real :: TKAIR  = 0.023     !thermal conductivity of air (w/m/k) (not used MB: 20140718)
  real :: RAIR   = 287.04    !gas constant for dry air (j/kg/k)
  real :: RW     = 461.269   !gas constant for  water vapor (j/kg/k)
  real :: DENH2O = 1000.     !density of water (kg/m3)
  real :: DENICE = 917.      !density of ice (kg/m3)
  real :: SSI = 0.03         !liquid water holding capacity for snowpack (m3/m3)
  real :: SNOW_RET_FAC = 5.e-5 !snowpack water release timescale factor (1/s)
  integer :: NROOT = 3   !!!!!!!! Cenlin
  END TYPE noahmp_parameters
  
  integer :: OPT_RUN = 8
  integer :: OPT_INF = 1
  integer :: OPT_INFDV = 3 !addrunoff ! options for infiltration in dynamic VIC runoff scheme
                      ! **1 -> Philip scheme
                      !   2 -> Green-Ampt scheme
                      !   3 -> Smith-Parlange scheme

contains

!== begin precip_canopy_intercept  ==============================================================================
  SUBROUTINE canwater_intercept (parameters,ILOC   ,JLOC   ,VEGTYP ,DT     ,UU     ,VV     , & !in
                          ELAI   ,ESAI   ,FVEG   ,IST    ,                 & !in
                          BDFALL ,RAIN   ,SNOW   ,FP     ,                 & !in
                          CANLIQ ,CANICE ,TV     ,SFCTMP ,TG     ,         & !in
                          QINTR  ,QDRIPR ,QTHROR ,QINTS  ,QDRIPS ,QTHROS , & !out
			  QRAIN  ,QSNOW  ,SNOWHIN, FWET   ,CMC  )   !out
! ------------------------ code history ------------------------------
! Michael Barlage: Oct 2013 - split CANWATER to calculate precip movement for 
!                             tracking of advected heat
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! ------------------------ input/output variables --------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,INTENT(IN)  :: ILOC    !grid index
  INTEGER,INTENT(IN)  :: JLOC    !grid index
  INTEGER,INTENT(IN)  :: VEGTYP  !vegetation type
  INTEGER,INTENT(IN)  :: IST     !surface type 1-soil; 2-lake
  REAL,   INTENT(IN)  :: DT      !main time step (s)
  REAL,   INTENT(IN)  :: UU      !u-direction wind speed [m/s]
  REAL,   INTENT(IN)  :: VV      !v-direction wind speed [m/s]
  REAL,   INTENT(IN)  :: ELAI    !leaf area index, after burying by snow
  REAL,   INTENT(IN)  :: ESAI    !stem area index, after burying by snow
  REAL,   INTENT(IN)  :: FVEG    !greeness vegetation fraction (-)
  REAL,   INTENT(IN)  :: BDFALL  !bulk density of snowfall (kg/m3)
  REAL,   INTENT(IN)  :: RAIN    !rainfall (mm/s)
  REAL,   INTENT(IN)  :: SNOW    !snowfall (mm/s)
  REAL,   INTENT(IN)  :: FP      !fraction of the gridcell that receives precipitation
  REAL,   INTENT(IN)  :: TV      !vegetation temperature (k)
  REAL,   INTENT(IN)  :: SFCTMP  !model-level temperature (k)
  REAL,   INTENT(IN)  :: TG      !ground temperature (k)
! input & output
  REAL, INTENT(INOUT) :: CANLIQ  !intercepted liquid water (mm)
  REAL, INTENT(INOUT) :: CANICE  !intercepted ice mass (mm)
! output
  REAL, INTENT(OUT)   :: QINTR   !interception rate for rain (mm/s)
  REAL, INTENT(OUT)   :: QDRIPR  !drip rate for rain (mm/s)
  REAL, INTENT(OUT)   :: QTHROR  !throughfall for rain (mm/s)
  REAL, INTENT(OUT)   :: QINTS   !interception (loading) rate for snowfall (mm/s)
  REAL, INTENT(OUT)   :: QDRIPS  !drip (unloading) rate for intercepted snow (mm/s)
  REAL, INTENT(OUT)   :: QTHROS  !throughfall of snowfall (mm/s)
  REAL, INTENT(OUT)   :: QRAIN   !rain at ground srf (mm/s) [+]
  REAL, INTENT(OUT)   :: QSNOW   !snow at ground srf (mm/s) [+]
  REAL, INTENT(OUT)   :: SNOWHIN !snow depth increasing rate (m/s)
  REAL, INTENT(OUT)   :: FWET    !wetted or snowed fraction of the canopy (-)
  REAL, INTENT(OUT)   :: CMC     !intercepted water (mm)
! --------------------------------------------------------------------
! ------------------------ local variables ---------------------------
  REAL                :: MAXSNO  !canopy capacity for snow interception (mm)
  REAL                :: MAXLIQ  !canopy capacity for rain interception (mm)
  REAL                :: FT      !temperature factor for unloading rate
  REAL                :: FV      !wind factor for unloading rate
  REAL                :: ICEDRIP !canice unloading
! --------------------------------------------------------------------
! initialization
      QINTR   = 0.
      QDRIPR  = 0.
      QTHROR  = 0.
      QINTR   = 0.
      QINTS   = 0.
      QDRIPS  = 0.
      QTHROS  = 0.
      QRAIN   = 0.0
      QSNOW   = 0.0
      SNOWHIN = 0.0
      ICEDRIP = 0.0
! --------------------------- liquid water ------------------------------
! maximum canopy water
      MAXLIQ =  parameters%CH2OP * (ELAI+ ESAI)
! average interception and throughfall
      IF((ELAI+ ESAI).GT.0.) THEN
         QINTR  = FVEG * RAIN * FP  ! interception capability
         QINTR  = MIN(QINTR, (MAXLIQ - CANLIQ)/DT * (1.-EXP(-RAIN*DT/MAXLIQ)) )
         QINTR  = MAX(QINTR, 0.)
         QDRIPR = FVEG * RAIN - QINTR
         QTHROR = (1.-FVEG) * RAIN
         CANLIQ=MAX(0.,CANLIQ+QINTR*DT)
      ELSE
         QINTR  = 0.
         QDRIPR = 0.
         QTHROR = RAIN
	 IF(CANLIQ > 0.) THEN             ! FOR CASE OF CANOPY GETTING BURIED
	   QDRIPR = QDRIPR + CANLIQ/DT
	   CANLIQ = 0.0
	 END IF
      END IF
      
! --------------------------- canopy ice ------------------------------
! for canopy ice
      MAXSNO = 6.6*(0.27+46./BDFALL) * (ELAI+ ESAI)
      IF((ELAI+ ESAI).GT.0.) THEN
         QINTS = FVEG * SNOW * FP
         QINTS = MIN(QINTS, (MAXSNO - CANICE)/DT * (1.-EXP(-SNOW*DT/MAXSNO)) )
         QINTS = MAX(QINTS, 0.)
         FT = MAX(0.0,(TV - 270.15) / 1.87E5)
         FV = SQRT(UU*UU + VV*VV) / 1.56E5
	 ! MB: changed below to reflect the rain assumption that all precip gets intercepted 
	 ICEDRIP = MAX(0.,CANICE) * (FV+FT)    !MB: removed /DT
         QDRIPS = (FVEG * SNOW - QINTS) + ICEDRIP
         QTHROS = (1.0-FVEG) * SNOW
         CANICE= MAX(0.,CANICE + (QINTS - ICEDRIP)*DT)
      ELSE
         QINTS  = 0.
         QDRIPS = 0.
         QTHROS = SNOW
	 IF(CANICE > 0.) THEN             ! FOR CASE OF CANOPY GETTING BURIED
	   QDRIPS = QDRIPS + CANICE/DT
	   CANICE = 0.0
	 END IF
      ENDIF
! wetted fraction of canopy
      IF(CANICE.GT.0.) THEN
           FWET = MAX(0.,CANICE) / MAX(MAXSNO,1.E-06)
      ELSE
           FWET = MAX(0.,CANLIQ) / MAX(MAXLIQ,1.E-06)
      ENDIF
      FWET = MIN(FWET, 1.) ** 0.667
! total canopy water
      CMC = CANLIQ + CANICE
      
! rain or snow on the ground
      QRAIN   = QDRIPR + QTHROR
      QSNOW   = QDRIPS + QTHROS
      SNOWHIN = QSNOW/BDFALL
      IF (IST == 2 .AND. TG > parameters%TFRZ) THEN
         QSNOW   = 0.
         SNOWHIN = 0.
      END IF
      
  END SUBROUTINE canwater_intercept


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
  LOGICAL, INTENT(IN)   :: FROZEN_CANOPY ! used to define latent heat pathway
  REAL, INTENT(IN)    :: BDFALL   !bulk density of snowfall (kg/m3) ! MB/AN: v3.7
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
        ETRAN = MAX( FCTR/parameters%HVAP, 0. )
        QEVAC = MAX( FCEV/parameters%HVAP, 0. )
        QDEWC = ABS( MIN( FCEV/parameters%HVAP, 0. ) )
        QSUBC = 0.
        QFROC = 0.
      ELSE
        ETRAN = MAX( FCTR/parameters%HSUB, 0. )
        QEVAC = 0.
        QDEWC = 0.
        QSUBC = MAX( FCEV/parameters%HSUB, 0. )
        QFROC = ABS( MIN( FCEV/parameters%HSUB, 0. ) )
      ENDIF

! canopy water balance. for convenience allow dew to bring CANLIQ above
! maxh2o or else would have to re-adjust drip
       QEVAC = MIN(CANLIQ/DT,QEVAC)
       CANLIQ=MAX(0.,CANLIQ+(QDEWC-QEVAC)*DT)
       IF(CANLIQ <= 1.E-06) CANLIQ = 0.0

! --------------------------- canopy ice ------------------------------

! for canopy ice
      MAXSNO = 6.6*(0.27+46./BDFALL) * (ELAI+ ESAI)
      QSUBC = MIN(CANICE/DT,QSUBC) 
      CANICE= MAX(0.,CANICE + (QFROC-QSUBC)*DT)
      IF(CANICE.LE.1.E-6) CANICE = 0.
     
! wetted fraction of canopy
      IF(CANICE.GT.0.) THEN
           FWET = MAX(0.,CANICE) / MAX(MAXSNO,1.E-06)
      ELSE
           FWET = MAX(0.,CANLIQ) / MAX(MAXLIQ,1.E-06)
      ENDIF
      FWET = MIN(FWET, 1.) ** 0.667

! phase change
      QMELTC = 0.
      QFRZC = 0.

      IF(CANICE.GT.1.E-6.AND.TV.GT.parameters%TFRZ) THEN
         QMELTC = MIN(CANICE/DT,(TV-parameters%TFRZ)*parameters%CICE*CANICE/ &
                  parameters%DENICE/(DT*parameters%HFUS))
         CANICE = MAX(0.,CANICE - QMELTC*DT)
         CANLIQ = MAX(0.,CANLIQ + QMELTC*DT)
         TV     = FWET*parameters%TFRZ + (1.-FWET)*TV
      ENDIF

      IF(CANLIQ.GT.1.E-6.AND.TV.LT.parameters%TFRZ) THEN
         QFRZC  = MIN(CANLIQ/DT,(parameters%TFRZ-TV)*parameters%CWAT* &
                  CANLIQ/parameters%DENH2O/(DT*parameters%HFUS))
         CANLIQ = MAX(0.,CANLIQ - QFRZC*DT)
         CANICE = MAX(0.,CANICE + QFRZC*DT)
         TV     = FWET*parameters%TFRZ + (1.-FWET)*TV
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
        snice(iz) = 0.
        snliq(iz) = 0.
        stc(iz)   = 0.
        dzsnso(iz)= 0.
        zsnso(iz) = 0.
   enddo
!to obtain equilibrium state of snow in glacier region
       
   IF(SNEQV > 5000.) THEN   ! 5000 mm -> maximum water depth
      BDSNOW      = SNICE(0) / DZSNSO(0)
      SNOFLOW     = (SNEQV - 5000.)
      SNICE(0)    = SNICE(0)  - SNOFLOW 
      DZSNSO(0)   = DZSNSO(0) - SNOFLOW/BDSNOW
      SNOFLOW     = SNOFLOW / DT
   END IF

! sum up snow mass for layered snow
   IF(ISNOW < 0) THEN  ! MB: only do for multi-layer
       SNEQV = 0.
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
    IF(ISNOW == 0 .and. QSNOW > 0.)  THEN
      SNOWH = SNOWH + SNOWHIN * DT
      SNEQV = SNEQV + QSNOW * DT
    END IF
! creating a new layer
 
    IF(ISNOW == 0  .AND. QSNOW>0. .AND. SNOWH >= 0.025) THEN !MB: change limit
!    IF(ISNOW == 0  .AND. QSNOW>0. .AND. SNOWH >= 0.05) THEN
      ISNOW    = -1
      NEWNODE  =  1
      DZSNSO(0)= SNOWH
      SNOWH    = 0.
      STC(0)   = MIN(273.16, SFCTMP)   ! temporary setup
      SNICE(0) = SNEQV
      SNLIQ(0) = 0.
    END IF
! snow with layers
    IF(ISNOW <  0 .AND. NEWNODE == 0 .AND. QSNOW > 0.) then
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
          IF (SNICE(J) <= .1) THEN
             IF(J /= 0) THEN
                SNLIQ(J+1) = SNLIQ(J+1) + SNLIQ(J)
                SNICE(J+1) = SNICE(J+1) + SNICE(J)
                DZSNSO(J+1) = DZSNSO(J+1) + DZSNSO(J)   !!!!!!!! Cenlin
             ELSE
               IF (ISNOW_OLD < -1) THEN    ! MB/KM: change to ISNOW
                SNLIQ(J-1) = SNLIQ(J-1) + SNLIQ(J)
                SNICE(J-1) = SNICE(J-1) + SNICE(J)
                DZSNSO(J-1) = DZSNSO(J-1) + DZSNSO(J)  !!!!!!!! Cenlin
               ELSE
	         IF(SNICE(J) >= 0.) THEN
                  PONDING1 = SNLIQ(J)    ! ISNOW WILL GET SET TO ZERO BELOW; PONDING1 WILL GET 
                  SNEQV = SNICE(J)       ! ADDED TO PONDING FROM PHASECHANGE PONDING SHOULD BE
                  SNOWH = DZSNSO(J)      ! ZERO HERE BECAUSE IT WAS CALCULATED FOR THIN SNOW
		 ELSE   ! SNICE OVER-SUBLIMATED EARLIER
		  PONDING1 = SNLIQ(J) + SNICE(J)
		  IF(PONDING1 < 0.) THEN  ! IF SNICE AND SNLIQ SUBLIMATES REMOVE FROM SOIL
		   SICE(1) = MAX(0.0,SICE(1)+PONDING1/(DZSNSO(1)*1000.))
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
       IF(SICE(1) < 0.) THEN
          SH2O(1) = SH2O(1) + SICE(1)
          SICE(1) = 0.
       END IF
       IF(ISNOW ==0) RETURN   ! MB: get out if no longer multi-layer
       SNEQV  = 0.
       SNOWH  = 0.
       ZWICE  = 0.
       ZWLIQ  = 0.
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
          IF(SNEQV <= 0.) SNOWH = 0. ! SHOULD BE ZERO; SEE ABOVE
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
             DZ(1)    = DZ(1)/2.
             SWICE(1) = SWICE(1)/2.
             SWLIQ(1) = SWLIQ(1)/2.
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
                DTDZ = (TSNO(1) - TSNO(2))/((DZ(1)+DZ(2))/2.)
                DZ(2)    = DZ(2)/2.
                SWICE(2) = SWICE(2)/2.
                SWLIQ(2) = SWLIQ(2)/2.
                DZ(3)    = DZ(2)
                SWICE(3) = SWICE(2)
                SWLIQ(3) = SWLIQ(2)
                TSNO(3) = TSNO(2) - DTDZ*DZ(2)/2.
                IF (TSNO(3) >= parameters%TFRZ) THEN
                   TSNO(3)  = TSNO(2)
                ELSE
                   TSNO(2) = TSNO(2) + DTDZ*DZ(2)/2.
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
    H = (parameters%CICE*WICE+parameters%CWAT*WLIQ) * (T-parameters%TFRZ)+parameters%HFUS*WLIQ
    H2= (parameters%CICE*WICE2+parameters%CWAT*WLIQ2) * (T2-parameters%TFRZ)+parameters%HFUS*WLIQ2
    HC = H + H2
    IF(HC < 0.)THEN
       TC = parameters%TFRZ + HC/(parameters%CICE*WICEC + parameters%CWAT*WLIQC)
    ELSE IF (HC.LE.parameters%HFUS*WLIQC) THEN
       TC = parameters%TFRZ
    ELSE
       TC = parameters%TFRZ + (HC - parameters%HFUS*WLIQC) / (parameters%CICE*WICEC + parameters%CWAT*WLIQC)
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
        VOID    = 1. - (SNICE(J)/parameters%DENICE + SNLIQ(J)/parameters%DENH2O) / DZSNSO(J)
        ! Allow compaction only for non-saturated node and higher ice lens node.
        IF (VOID > 0.001 .AND. SNICE(J) > 0.1) THEN
           BI = SNICE(J) / DZSNSO(J)
           TD = MAX(0.,parameters%TFRZ-STC(J))
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
              DDZ3 = MAX(0.,(FICEOLD(J) - FICE(J))/MAX(1.E-6,FICEOLD(J)))
              DDZ3 = - DDZ3/DT           ! sometimes too large
           ELSE
              DDZ3 = 0.
           END IF
           ! Time rate of fractional change in DZ (units of s-1)
           PDZDTC = (DDZ1 + DDZ2 + DDZ3)*DT
           PDZDTC = MAX(-0.5,PDZDTC)
           ! The change in DZ due to compaction
           DZSNSO(J) = DZSNSO(J)*(1.+PDZDTC)
           DZSNSO(J) = max(DZSNSO(J),SNICE(J)/parameters%DENICE + SNLIQ(J)/parameters%DENH2O) !!!!!! Cenlin
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
   REAL, PARAMETER :: max_liq_mass_fraction = 0.4  !!!!!!!!! Cenlin
! ----------------------------------------------------------------------
!for the case when SNEQV becomes '0' after 'COMBINE'
   IF(SNEQV == 0.) THEN
      SICE(1) =  SICE(1) + (QSNFRO-QSNSUB)*DT/(DZSNSO(1)*1000.)  ! Barlage: SH2O->SICE v3.6
      IF(SICE(1) < 0.) THEN
         SH2O(1) = SH2O(1) + SICE(1)
         SICE(1) = 0.
      END IF
   END IF
! for shallow snow without a layer
! snow surface sublimation may be larger than existing snow mass. To conserve water,
! excessive sublimation is used to reduce soil water. Smaller time steps would tend 
! to aviod this problem.
   IF(ISNOW == 0 .and. SNEQV > 0.) THEN
      TEMP   = SNEQV
      SNEQV  = SNEQV - QSNSUB*DT + QSNFRO*DT
      PROPOR = SNEQV/TEMP
      SNOWH  = MAX(0.,PROPOR * SNOWH)
      SNOWH  = MIN(MAX(SNOWH,SNEQV/500.0),SNEQV/50.0)  ! limit adjustment to a reasonable density !!!!!! Cenlin
      IF(SNEQV < 0.) THEN
         SICE(1) = SICE(1) + SNEQV/(DZSNSO(1)*1000.)
         SNEQV   = 0.
         SNOWH   = 0.
      END IF
      IF(SICE(1) < 0.) THEN
         SH2O(1) = SH2O(1) + SICE(1)
         SICE(1) = 0.
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
         SNLIQ(ISNOW+1) = MAX(0., SNLIQ(ISNOW+1))
      ENDIF
      
   ENDIF !KWM  -- Can the ENDIF be moved toward the end of the subroutine (Just set QSNBOT=0)?
! Porosity and partial volume
   DO J = ISNOW+1, 0
     VOL_ICE(J)      = MIN(1., SNICE(J)/(DZSNSO(J)*parameters%DENICE))
     EPORE(J)        = 1. - VOL_ICE(J)
   END DO
   QIN = 0.
   QOUT = 0.
   DO J = ISNOW+1, 0
     SNLIQ(J) = SNLIQ(J) + QIN
     VOL_LIQ(J) = SNLIQ(J)/(DZSNSO(J)*parameters%DENH2O)
     QOUT = MAX(0.,(VOL_LIQ(J)-parameters%SSI*EPORE(J))*DZSNSO(J))
!!!!!!!! Cenlin: New snow water retention code
     IF(J == 0) THEN
       QOUT = MAX((VOL_LIQ(J)- EPORE(J))*DZSNSO(J) , parameters%SNOW_RET_FAC*DT*QOUT)
     END IF
!!!!!!!!
     QOUT = QOUT*parameters%DENH2O
     SNLIQ(J) = SNLIQ(J) - QOUT
!!!!!!!! Cenlin: New snow water retention code
     IF((SNLIQ(J)/(SNICE(J)+SNLIQ(J))) > max_liq_mass_fraction) THEN
       QOUT = QOUT + (SNLIQ(J) - max_liq_mass_fraction/(1.0 - max_liq_mass_fraction)*SNICE(J))
       SNLIQ(J) = max_liq_mass_fraction/(1.0 - max_liq_mass_fraction)*SNICE(J)
     ENDIF
!!!!!!!!
     QIN = QOUT
   END DO

!!!!!!!! Cenlin: New snow water retention code
   DO J = ISNOW+1, 0
     DZSNSO(J) = MAX(DZSNSO(J),SNLIQ(J)/parameters%DENH2O + SNICE(J)/parameters%DENICE)
   END DO
!!!!!!!!

! Liquid water from snow bottom to soil
   QSNBOT = QOUT / DT           ! mm/s

  END SUBROUTINE SNOWH2O


!== begin soilwater ================================================================================

  SUBROUTINE SOILWATER (parameters,NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & !in
                        QINSUR ,QSEVA  ,ETRANI ,SICE   ,ILOC   , JLOC, & !in
                        SH2O   ,SMC    ,ZWT    ,VEGTYP ,& !inout
                        SMCWTD, DEEPRECH                       ,& !inout
                        RUNSRF ,QDRAIN ,RUNSUB ,WCND   ,FCRMAX )   !out

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
  REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: SICE   !soil ice content [m3/m3]

  INTEGER,                     INTENT(IN) :: VEGTYP

! input & output
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SH2O   !soil liquid water content [m3/m3]
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SMC    !total soil water content [m3/m3]
  REAL, INTENT(INOUT)                     :: ZWT    !water table depth [m]
  REAL,                     INTENT(INOUT) :: SMCWTD !soil moisture between bottom of the soil and the water table [m3/m3]
  REAL                    , INTENT(INOUT) :: DEEPRECH

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
  REAL                                    :: FACC   !accumulated infiltration rate (m/s) !addrunoff
  REAL, PARAMETER :: A = 4.0
! ----------------------------------------------------------------------
    RUNSRF = 0.0
    RUNSUB = 0.0
    PDDUM  = 0.0
    RSAT   = 0.0

! for the case when snowmelt water is too large

    DO K = 1,NSOIL
       EPORE   = MAX ( 1.E-4 , ( parameters%SMCMAX(K) - SICE(K) ) )
       RSAT    = RSAT + MAX(0.,SH2O(K)-EPORE)*DZSNSO(K)  
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
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s 
       END IF
    END IF

    IF(OPT_RUN == 5) THEN
       FFF = 6.0
       FSAT   = parameters%FSATMX*EXP(-0.5*FFF*MAX(-2.0-ZWT,0.))
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s
       END IF
    END IF

    IF(OPT_RUN == 2) THEN
       FFF   = 2.0
       FSAT   = parameters%FSATMX*EXP(-0.5*FFF*ZWT)
       IF(QINSUR > 0.) THEN
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
       SMCTOT = 0.
       DZTOT  = 0.
       DO K = 1,NSOIL
          DZTOT   = DZTOT  + DZSNSO(K)  
          SMCTOT  = SMCTOT + SMC(K)/parameters%SMCMAX(K)*DZSNSO(K)
          IF(DZTOT >= 2.0) EXIT
       END DO
       SMCTOT = SMCTOT/DZTOT
       FSAT   = MAX(0.01,SMCTOT) ** 4.        !BATS

       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ((1.0-FCR(1))*FSAT+FCR(1))  
         PDDUM  = QINSUR - RUNSRF                       ! m/s
       END IF
    END IF

   !addrunoff
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
    FACC        = 1E-06 ! addrunoff
    QDRAIN_SAVE = 0.0
    RUNSRF_SAVE = 0.0
    DO ITER = 1, NITER
       IF(QINSUR > 0. .and. OPT_RUN == 3) THEN
          CALL INFIL (parameters,NSOIL  ,DTFINE     ,ZSOIL  ,SH2O   ,SICE   , & !in
                      SICEMAX,QINSUR ,                         & !in
                      PDDUM  ,RUNSRF )                           !out
       END IF

	!addrunoff
       IF(QINSUR > 0. .and. OPT_RUN == 6) THEN
          CALL COMPUTE_VIC_SURFRUNOFF(parameters,DTFINE,NSOIL,SMC,ZSOIL,QINSUR,& !in
                                      FSAT,RUNSRF,PDDUM)                         !out
       END IF

       IF (QINSUR > 0. .AND. OPT_RUN == 7) THEN
          CALL COMPUTE_XAJ_SURFRUNOFF(parameters,DTFINE,FCR,NSOIL,SMC,ZSOIL,QINSUR,& ! in
                                      RUNSRF,PDDUM)                                  ! out
       END IF

       IF(QINSUR > 0. .and. OPT_RUN == 8) THEN
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

    RUNSRF = RUNSRF * 1000. + RSAT * 1000./DT  ! m/s -> mm/s
    QDRAIN = QDRAIN * 1000.
    print*,'QDRAIN = ',QDRAIN

!WRF_HYDRO_DJG...
!yw    INFXSRT = RUNSRF * DT   !mm/s -> mm

! removal of soil water due to groundwater flow (option 2)

    IF(OPT_RUN == 2) THEN
         WTSUB = 0.
         DO K = 1, NSOIL
           WTSUB = WTSUB + WCND(K)*DZSNSO(K)
         END DO

         DO K = 1, NSOIL
           MH2O    = RUNSUB*DT*(WCND(K)*DZSNSO(K))/WTSUB       ! mm
           SH2O(K) = SH2O(K) - MH2O/(DZSNSO(K)*1000.)
         END DO
    END IF

! Limit MLIQ to be greater than or equal to watmin.
! Get water needed to bring MLIQ equal WATMIN from lower layer.

   IF(OPT_RUN /= 1) THEN
      DO IZ = 1, NSOIL
         MLIQ(IZ) = SH2O(IZ)*DZSNSO(IZ)*1000.
      END DO

      WATMIN = 0.01           ! mm
      DO IZ = 1, NSOIL-1
          IF (MLIQ(IZ) .LT. 0.) THEN
             XS = WATMIN-MLIQ(IZ)
          ELSE
             XS = 0.
          END IF
          MLIQ(IZ  ) = MLIQ(IZ  ) + XS
          MLIQ(IZ+1) = MLIQ(IZ+1) - XS
      END DO

        IZ = NSOIL
        IF (MLIQ(IZ) .LT. WATMIN) THEN
           XS = WATMIN-MLIQ(IZ)
        ELSE
           XS = 0.
        END IF
        MLIQ(IZ) = MLIQ(IZ) + XS
        RUNSUB   = RUNSUB - XS/DT
        IF(OPT_RUN == 5)DEEPRECH = DEEPRECH - XS*1.E-3

      DO IZ = 1, NSOIL
        SH2O(IZ)     = MLIQ(IZ) / (DZSNSO(IZ)*1000.)
      END DO
   END IF
  print*,'RUNSUB = ',RUNSUB
  print*,'DZSNSO = ',DZSNSO(1:4)
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

   WD1 = 0.
   DO K = 1,NSOIL
     WD1 = WD1 + (parameters%SMCMAX(1)-SH2O(K)) * DZSNSO(K) ! [m]
   ENDDO

   DZFINE = 3.0 * (-ZSOIL(NSOIL)) / NFINE  
   do K =1,NFINE
      ZFINE(K) = FLOAT(K) * DZFINE
   ENDDO

   ZWT = -3.*ZSOIL(NSOIL) - 0.001   ! initial value [m]

   WD2 = 0.
   DO K = 1,NFINE
     TEMP  = 1. + (ZWT-ZFINE(K))/parameters%PSISAT(1)
     WD2   = WD2 + parameters%SMCMAX(1)*(1.-TEMP**(-1./parameters%BEXP(1)))*DZFINE
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
       DT1 = DT /86400.
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

       VAL = (1. - EXP ( - parameters%KDT * DT1))
       DDT = DD * VAL
       PX  = MAX(0.,QINSUR * DT)
       INFMAX = (PX * (DDT / (PX + DDT)))/ DT

! impermeable fraction due to frozen soil

       FCR = 1.
       IF (DICE >  1.E-2) THEN
          ACRT = CVFRZ * parameters%FRZX / DICE
          SUM = 1.
          IALP1 = CVFRZ - 1
          DO J = 1,IALP1
             K = 1
             DO JJ = J +1,IALP1
                K = K * JJ
             END DO
             SUM = SUM + (ACRT ** (CVFRZ - J)) / FLOAT(K)
          END DO
          FCR = 1. - EXP (-ACRT) * SUM
       END IF

! correction of infiltration limitation

       INFMAX = INFMAX * FCR

! jref for urban areas
!       IF ( parameters%urban_flag ) INFMAX == INFMAX * 0.05

       CALL WDFCND2 (parameters,WDF,WCND,SH2O(1),SICEMAX,1)
       INFMAX = MAX (INFMAX,WCND)
       INFMAX = MIN (INFMAX,PX)

       RUNSRF= MAX(0., QINSUR - INFMAX)
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
             QDRAIN   = 0.
          END IF
          IF(OPT_RUN == 3  .OR. OPT_RUN == 6 .OR. OPT_RUN == 7 .OR. OPT_RUN == 8) THEN !addrunoff
             QDRAIN   = parameters%SLOPE*WCND(K)
          END IF
          IF(OPT_RUN == 4) THEN
             QDRAIN   = (1.0-FCRMAX)*WCND(K)
          END IF
          IF(OPT_RUN == 5) THEN   !gmm new m-m&f water table dynamics formulation
             TEMP1    = 2.0 * DENOM(K)
             IF(ZWT < ZSOIL(NSOIL)-DENOM(NSOIL))THEN
!gmm interpolate from below, midway to the water table, to the middle of the auxiliary layer below the soil bottom
                SMXBOT = SMX(K) - (SMX(K)-SMXWTD) *  DENOM(K) * 2./ (DENOM(K) + ZSOIL(K) - ZWT)
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
       BI (K)    = 1. + BI(K) * DT
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

!addrunoff
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
  REAL                    , INTENT(OUT):: RUNSRF
  REAL                    , INTENT(OUT):: PDDUM
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
    TOP_MOIST     = TOP_MOIST + (SMC(IZ)*-1*ZSOIL(IZ)) ! m
    TOP_MAX_MOIST = TOP_MAX_MOIST + (parameters%SMCMAX(IZ)*-1*ZSOIL(IZ)) ! m  
  END DO
  ! Saturated area from soil moisture
  EX    = parameters%BVIC/(1+parameters%BVIC)
  ASAT  = 1.0 - (( 1.0 - (TOP_MOIST/TOP_MAX_MOIST))**EX)  ! 
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
  IF (RUNSRF .GT. QINSUR)RUNSRF = QINSUR
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
    REAL                    , INTENT(OUT):: RUNSRF
    REAL                    , INTENT(OUT):: PDDUM
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
       IF ((SMC(IZ)-parameters%SMCREF(IZ)) .GT. 0.) THEN ! soil moisture greater than field capacity
          SM     = SM + (SMC(IZ) - parameters%SMCREF(IZ) )*-1*ZSOIL(IZ) !m
          WM     = WM + (parameters%SMCREF(IZ)*-1*ZSOIL(IZ))            !m  
       ELSE
          WM     = WM + (SMC(IZ)*-1*ZSOIL(IZ))
       END IF
       WM_MAX = WM_MAX + (parameters%SMCREF(IZ)*-1*ZSOIL(IZ))
       SM_MAX = SM_MAX + (parameters%SMCMAX(IZ) - parameters%SMCREF(IZ))*-1*ZSOIL(IZ)
    END DO
    WM = MIN(WM,WM_MAX) ! tension water (m) 
    SM = MIN(SM,SM_MAX) ! free water (m)
! impervious surface runoff R_IMP    
    IRUNOFF = FCR(1)*QINSUR*DT
! solve pervious surface runoff (m) based on Eq. (310)
    IF ((WM/WM_MAX) .LE. (0.5-parameters%AXAJ))THEN
       PRUNOFF = (1-FCR(1))*QINSUR*DT*((0.5-parameters%AXAJ)**(1-parameters%BXAJ))*((WM/WM_MAX)**parameters%BXAJ)
    ELSE
       PRUNOFF = (1-FCR(1))*QINSUR*DT*(1-(((0.5+parameters%AXAJ)**(1-parameters%BXAJ))*((1-(WM/WM_MAX))**parameters%BXAJ)))
    END IF
! estimate surface runoff based on Eq. (313)
    IF(QINSUR .EQ. 0.0) THEN
      RUNSRF  = 0.0
    ELSE
      RUNSRF = PRUNOFF*(1-((1-(SM/SM_MAX))**parameters%XXAJ))+IRUNOFF
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
    TOP_MOIST     = TOP_MOIST + (SMC(IZ)*-1*ZSOIL(IZ))                                   ! actual moisture in top layers, [m]
    TOP_MAX_MOIST = TOP_MAX_MOIST + (parameters%SMCMAX(IZ)*-1*ZSOIL(IZ))                 ! maximum moisture in top layers, [m]  
  END DO
  IF(TOP_MOIST .GT. TOP_MAX_MOIST) TOP_MOIST = TOP_MAX_MOIST  
  DP     = QINSUR * DT                                                                   ! precipitation depth, [m]
  I_MAX  = TOP_MAX_MOIST * (parameters%BVIC+1.0)                                         ! maximum infiltration capacity, im, [m], Eq. 14
  I_0    = I_MAX * (1-(1-(TOP_MOIST/TOP_MAX_MOIST)**(1.0/(1.0+parameters%BVIC))))        ! infiltration capacity, i [m] in the Eq. 1
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
      I_0 = I_MAX * (1-(1-(TOP_MOIST/TOP_MAX_MOIST)**(1.0/(1.0+parameters%BVIC))))     
      IF((DP+I_0) .GT. I_MAX)THEN
        IF((FMAX*DT) .GE. DP) THEN
          YD     = I_MAX - I_0
          TEMPR1 = 0.0
          CALL RR1(parameters,I_0,I_MAX,YD,TEMPR1) 
          TEMP1  = I_MAX-I_0-TEMPR1-((FSUR*DT) * (1 - (1-((DP-TEMPR1)/(FMAX*DT))**(BB+1.0))))
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
               YD     = TEMPR1 + ((FSUR*DT) * (1 - (1-((DP-TEMPR1)/(FMAX*DT))**(BB+1.0))))
               IF ((ABS(YD-YD_OLD) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                  GOTO 1001
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
                    GOTO 1001
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
                  GOTO 1001
               END IF 
            END DO
1001        IF(YD .LE. 0.0) YD = 0.0
            IF(YD .GE. DP) YD = DP
            CALL RR1(parameters,I_0,I_MAX,YD,R1)
            RUNOFFSAT = R1
            RUNOFFINF = DP - YD
            INFILTRTN = YD - RUNOFFSAT
            TOP_MOIST = TOP_MOIST + INFILTRTN
            YD        = I_0+YD
            IF (TOP_MOIST .LE. 0.0) TOP_MOIST=0.0
            IF (TOP_MOIST .GE. TOP_MAX_MOIST) TOP_MOIST = TOP_MAX_MOIST
            I_0       = I_MAX * (1-(1-(TOP_MOIST/TOP_MAX_MOIST)**(1.0/(1.0+parameters%BVIC))))
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
             YD = TEMPR1 + ((FSUR*DT) * (1 - (1-((DP-TEMPR1)/(FMAX*DT))**(BB+1.0))))
             !print*,'YD=',YD,'YD_OLD=',YD_OLD,'ERROR=',ABS(YD - YD_OLD),'IZ=',IZ,'ERROR2=',ERROR,'DT=',DT
             IF ((ABS(YD - YD_OLD) .LE. ERROR) .OR. (IZ .EQ. IZMAX)) THEN
                GOTO 1002
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
                    GOTO 1002
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
                   GOTO 1002
                 END IF
              END DO
          END IF
        END IF
1002    IF(YD .LE. 0.0) YD = 0.0
        IF(YD .GE. DP)  YD = DP
        R1 = 0.0
        CALL RR1(parameters,I_0,I_MAX,YD,R1)
        RUNOFFSAT = R1
        RUNOFFINF = DP - YD
        INFILTRTN = YD - RUNOFFSAT
        TOP_MOIST = TOP_MOIST + INFILTRTN
        IF (TOP_MOIST .LE. 0.0) TOP_MOIST=0.0
        IF (TOP_MOIST .GE. TOP_MAX_MOIST) TOP_MOIST = TOP_MAX_MOIST
        I_0       = I_MAX * (1-(1-(TOP_MOIST/TOP_MAX_MOIST)**(1.0/(1.0+parameters%BVIC))))
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
   R1 = YD - ( (I_MAX/(parameters%BVIC+1.0)) * ( ((1 - (I_0/I_MAX))**(parameters%BVIC+1.0)) &
                                               - ((1 - (TDEPTH/I_MAX))**(parameters%BVIC+1.0))))
   
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
     R2 = DP - R1 - (FMAX*DT* (1 - ((1 - (DP-R1)/(FMAX*DT))**(BB+1.0))))
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
   INTEGER,                  INTENT(IN) :: NSOIL  !no. of soil layers
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
      JJ   = parameters%G * (parameters%SMCMAX(ISOIL) - parameters%SMCWLT(ISOIL)) * -1 * ZSOIL(ISOIL)
      FSUR = parameters%DKSAT(ISOIL) + (GAM * (parameters%DKSAT(ISOIL) - WCND) / (EXP(GAM * 1E-05 / JJ) -1))
      ! infiltration rate at surface
      IF(parameters%DKSAT(ISOIL) .LT. QINSUR)THEN
        FSUR = MIN(QINSUR,FSUR)
      ELSE
        FSUR = QINSUR
      END IF
      IF(FSUR .LT. 0.0) FSUR = 0.0
   ELSE
      ! estimate initial soil hydraulic conductivty (Ki in the equation), WCND (m/s)
      CALL WDFCND2 (parameters,WDF,WCND,SMC(ISOIL),SICE(ISOIL),ISOIL)
      ! Maximum infiltrability based on the Eq. 6.25. (m/s)
      JJ   = parameters%G * (parameters%SMCMAX(ISOIL) - SMC(ISOIL)) * -1 * ZSOIL(ISOIL)
      FSUR = parameters%DKSAT(ISOIL) + (GAM * (parameters%DKSAT(ISOIL) - WCND) / (EXP(GAM * FACC / JJ) -1))
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
   INTEGER,                  INTENT(IN) :: NSOIL  !no. of soil layers
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
     JJ   = parameters%G * (parameters%SMCMAX(ISOIL) - parameters%SMCWLT(ISOIL)) * -1 * ZSOIL(ISOIL)
     FSUR = parameters%DKSAT(ISOIL) + ((JJ/1E-05) * (parameters%DKSAT(ISOIL) - WCND))
     !maximum infiltration rate at surface
     IF(FSUR .LT. 0.0) FSUR = 0.0
   ELSE 
     ! estimate initial soil hydraulic conductivty (Ki in the equation), WCND (m/s)
     CALL WDFCND2 (parameters,WDF,WCND,SMC(ISOIL),SICE(ISOIL),ISOIL)
     ! Maximum infiltrability based on the Eq. 6.25. (m/s)
     JJ   = parameters%G * (parameters%SMCMAX(ISOIL) - SMC(ISOIL)) * -1 * ZSOIL(ISOIL)
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
   INTEGER,                  INTENT(IN) :: NSOIL  !no. of soil layers
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
     SP = SQRT(2 * (parameters%SMCMAX(ISOIL) - parameters%SMCWLT(ISOIL)) * (parameters%DWSAT(ISOIL) - WDF))
     ! Parameter A in Eq. 9 of Valiantzas (2010) is given by
     AP = MIN(WCND, (2/3)*parameters%DKSAT(ISOIL))
     AP = MAX(AP,   (1/3)*parameters%DKSAT(ISOIL))
     ! Maximun infiltration rate, m
     FSUR = (1/2)*SP*(DT**(-1/2))+AP ! m/s
     IF(FSUR .LT. 0.0) FSUR = 0.0
   ELSE 
     ! estimate initial soil hydraulic conductivty and diffusivity (Ki, D(theta) in the equation)
     CALL WDFCND2 (parameters,WDF,WCND,SMC(ISOIL),SICE(ISOIL),ISOIL)
     ! Sorptivity based on Eq. 10b from Kutílek, Miroslav, and Jana Valentová (1986) 
     ! Sorptivity approximations. Transport in Porous Media 1.1, 57-62.
     SP = SQRT(2 * (parameters%SMCMAX(ISOIL) - SMC(ISOIL)) * (parameters%DWSAT(ISOIL) - WDF)) 
     ! Parameter A in Eq. 9 of Valiantzas (2010) is given by
     AP = MIN(WCND, (2/3)*parameters%DKSAT(ISOIL))
     AP = MAX(AP,   (1/3)*parameters%DKSAT(ISOIL))
     ! Maximun infiltration rate, m
     FSUR = (1/2)*SP*(DT**(-1/2))+AP ! m/s
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
    VKWGT = 1./ (1. + (500.* SICE)**3.)
    WDF   = VKWGT * WDF + (1.-VKWGT)*parameters%DWSAT(ISOIL)*(FACTR1)**EXPON
    END IF

! hydraulic conductivity

    EXPON = 2.0*parameters%BEXP(ISOIL) + 3.0
    WCND  = parameters%DKSAT(ISOIL) * FACTR2 ** EXPON

  END SUBROUTINE WDFCND2

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

end module water_routines
