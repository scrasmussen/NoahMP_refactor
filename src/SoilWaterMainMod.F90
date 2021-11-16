module SoilWaterMainMod



  subroutine SoilWaterMain(noahmp)



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







    end associate

  end subroutine SoilWaterMain

end module SoilWaterMainMod
