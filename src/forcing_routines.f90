module forcing_routines
  
  IMPLICIT NONE

  public  :: noahmp_options
  public  :: ATM

! =====================================options for different schemes================================

  INTEGER :: OPT_SNF  ! options for partitioning  precipitation into rainfall & snowfall
                      ! **1 -> Jordan (1991)
                      !   2 -> BATS: when SFCTMP<TFRZ+2.2 
                      !   3 -> SFCTMP < TFRZ
                      !   4 -> Use WRF microphysics
                      !   5 -> Use wetbulb temperature (Wang et al., 2019 GRL) C.He, 12/18/2020

! =====================================noahmp parameters============================================
  TYPE noahmp_parameters
       ! no params for ATM
  END TYPE noahmp_parameters

  REAL, PARAMETER :: RAIR   = 287.04    !gas constant for dry air (j/kg/k)
  REAL, PARAMETER :: CPAIR  = 1004.64   !heat capacity dry air at const pres (j/kg/k)
  REAL, PARAMETER :: TFRZ   = 273.16    !freezing/melting point (k) 
  REAL, PARAMETER :: HSUB   = 2.8440E06 !latent heat of sublimation (j/kg)
  REAL, PARAMETER :: HVAP   = 2.5104E06 !latent heat of vaporization (j/kg) 
contains
  

!== begin atm ======================================================================================

  SUBROUTINE ATM (parameters,SFCPRS  ,SFCTMP   ,Q2      ,                             &
                  PRCPCONV,PRCPNONC ,PRCPSHCV,PRCPSNOW,PRCPGRPL,PRCPHAIL , &
                  SOLDN   ,COSZ     ,THAIR   ,QAIR    ,                    & 
                  EAIR    ,RHOAIR   ,QPRECC  ,QPRECL  ,SOLAD   , SOLAI   , &
		  SWDOWN  ,BDFALL   ,RAIN    ,SNOW    ,FP      , FPICE   ,PRCP )     
! --------------------------------------------------------------------------------------------------
! re-process atmospheric forcing
! ----------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs

  type (noahmp_parameters), intent(in) :: parameters
  REAL                          , INTENT(IN)  :: SFCPRS !pressure (pa)
  REAL                          , INTENT(IN)  :: SFCTMP !surface air temperature [k]
  REAL                          , INTENT(IN)  :: Q2     !mixing ratio (kg/kg)
  REAL                          , INTENT(IN)  :: PRCPCONV ! convective precipitation entering  [mm/s]    ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPNONC ! non-convective precipitation entering [mm/s] ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPSHCV ! shallow convective precip entering  [mm/s]   ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPSNOW ! snow entering land model [mm/s]              ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPGRPL ! graupel entering land model [mm/s]           ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPHAIL ! hail entering land model [mm/s]              ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: SOLDN  !downward shortwave radiation (w/m2)
  REAL                          , INTENT(IN)  :: COSZ   !cosine solar zenith angle [0-1]

! outputs

  REAL                          , INTENT(OUT) :: THAIR  !potential temperature (k)
  REAL                          , INTENT(OUT) :: QAIR   !specific humidity (kg/kg) (q2/(1+q2))
  REAL                          , INTENT(OUT) :: EAIR   !vapor pressure air (pa)
  REAL                          , INTENT(OUT) :: RHOAIR !density air (kg/m3)
  REAL                          , INTENT(OUT) :: QPRECC !convective precipitation (mm/s)
  REAL                          , INTENT(OUT) :: QPRECL !large-scale precipitation (mm/s)
  REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAD  !incoming direct solar radiation (w/m2)
  REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAI  !incoming diffuse solar radiation (w/m2)
  REAL                          , INTENT(OUT) :: SWDOWN !downward solar filtered by sun angle [w/m2]
  REAL                          , INTENT(OUT) :: BDFALL  !!bulk density of snowfall (kg/m3) AJN
  REAL                          , INTENT(OUT) :: RAIN    !rainfall (mm/s) AJN
  REAL                          , INTENT(OUT) :: SNOW    !liquid equivalent snowfall (mm/s) AJN
  REAL                          , INTENT(OUT) :: FP      !fraction of area receiving precipitation  AJN
  REAL                          , INTENT(OUT) :: FPICE   !fraction of ice                AJN
  REAL                          , INTENT(OUT) :: PRCP    !total precipitation [mm/s]     ! MB/AN : v3.7

!locals

  REAL                                        :: PAIR   !atm bottom level pressure (pa)
  REAL                                        :: PRCP_FROZEN   !total frozen precipitation [mm/s] ! MB/AN : v3.7
  REAL, PARAMETER                             :: RHO_GRPL = 500.0  ! graupel bulk density [kg/m3] ! MB/AN : v3.7
  REAL, PARAMETER                             :: RHO_HAIL = 917.0  ! hail bulk density [kg/m3]    ! MB/AN : v3.7
! wet-bulb scheme Wang et al., 2019 GRL, C.He, 12/18/2020
  REAL               :: ESATAIR    ! saturated vapor pressure of air
  REAL               :: LATHEA     ! latent heat of vapor/sublimation
  REAL               :: GAMMA_b      ! (cp*p)/(eps*L)
  REAL               :: TDC        ! air temperature [C]
  REAL               :: TWET       ! wetbulb temperature
  INTEGER            :: ITER
  INTEGER, PARAMETER :: NITER = 10 ! iterations for Twet calculation

! --------------------------------------------------------------------------------------------------

!jref: seems like PAIR should be P1000mb??
       PAIR   = SFCPRS                   ! atm bottom level pressure (pa)
       THAIR  = SFCTMP * (SFCPRS/PAIR)**(RAIR/CPAIR) 

       QAIR   = Q2                       ! In WRF, driver converts to specific humidity

       EAIR   = QAIR*SFCPRS / (0.622+0.378*QAIR)
       RHOAIR = (SFCPRS-0.378*EAIR) / (RAIR*SFCTMP)

       IF(COSZ <= 0.) THEN 
          SWDOWN = 0.
       ELSE
          SWDOWN = SOLDN
       END IF 

       SOLAD(1) = SWDOWN*0.7*0.5     ! direct  vis
       SOLAD(2) = SWDOWN*0.7*0.5     ! direct  nir
       SOLAI(1) = SWDOWN*0.3*0.5     ! diffuse vis
       SOLAI(2) = SWDOWN*0.3*0.5     ! diffuse nir

       PRCP = PRCPCONV + PRCPNONC + PRCPSHCV

       IF(OPT_SNF == 4) THEN
         QPRECC = PRCPCONV + PRCPSHCV
	 QPRECL = PRCPNONC
       ELSE
         QPRECC = 0.10 * PRCP          ! should be from the atmospheric model
         QPRECL = 0.90 * PRCP          ! should be from the atmospheric model
       END IF

! fractional area that receives precipitation (see, Niu et al. 2005)
   
    FP = 0.0
    IF(QPRECC + QPRECL > 0.) & 
       FP = (QPRECC + QPRECL) / (10.*QPRECC + QPRECL)

! partition precipitation into rain and snow. Moved from CANWAT MB/AN: v3.7

! Jordan (1991)

     IF(OPT_SNF == 1) THEN
       IF(SFCTMP > TFRZ+2.5)THEN
           FPICE = 0.
       ELSE
         IF(SFCTMP <= TFRZ+0.5)THEN
           FPICE = 1.0
         ELSE IF(SFCTMP <= TFRZ+2.)THEN
           FPICE = 1.-(-54.632 + 0.2*SFCTMP)
         ELSE
           FPICE = 0.6
         ENDIF
       ENDIF
     ENDIF

     IF(OPT_SNF == 2) THEN
       IF(SFCTMP >= TFRZ+2.2) THEN
           FPICE = 0.
       ELSE
           FPICE = 1.0
       ENDIF
     ENDIF

     IF(OPT_SNF == 3) THEN
       IF(SFCTMP >= TFRZ) THEN
           FPICE = 0.
       ELSE
           FPICE = 1.0
       ENDIF
     ENDIF

! Hedstrom NR and JW Pomeroy (1998), Hydrol. Processes, 12, 1611-1625
! fresh snow density

     BDFALL = MIN(120.,67.92+51.25*EXP((SFCTMP-TFRZ)/2.59))       !MB/AN: change to MIN  
     IF(OPT_SNF == 4) THEN
        PRCP_FROZEN = PRCPSNOW + PRCPGRPL + PRCPHAIL
        IF(PRCPNONC > 0. .and. PRCP_FROZEN > 0.) THEN
	  FPICE = MIN(1.0,PRCP_FROZEN/PRCPNONC)
	  FPICE = MAX(0.0,FPICE)
	  BDFALL = BDFALL*(PRCPSNOW/PRCP_FROZEN) + RHO_GRPL*(PRCPGRPL/PRCP_FROZEN) + &
	             RHO_HAIL*(PRCPHAIL/PRCP_FROZEN)
	ELSE
	  FPICE = 0.0
        ENDIF
	
     ENDIF

! wet-bulb scheme (Wang et al., 2019 GRL), C.He, 12/18/2020
     IF(OPT_SNF == 5) THEN
       TDC   = MIN( 50., MAX(-50.,(SFCTMP-TFRZ)) ) !Kelvin to degree Celsius with limit -50 to +50
       IF (SFCTMP > TFRZ) THEN
          LATHEA = HVAP
       ELSE
          LATHEA = HSUB
       END IF
       GAMMA_b = CPAIR*SFCPRS/(0.622*LATHEA)
       TWET    = TDC - 5.  ! first guess wetbulb temperature
       DO ITER = 1, NITER
          ESATAIR = 610.8 * EXP((17.27*TWET)/(237.3+TWET))
          TWET    = TWET - (ESATAIR-EAIR)/ GAMMA_b  ! Wang et al., 2019 GRL Eq.2
       END DO
       FPICE = 1.0/(1.0+6.99E-5*exp(2.0*(TWET+3.97))) ! Wang et al., 2019 GRL Eq. 1
     ENDIF

     RAIN   = PRCP * (1.-FPICE)
     SNOW   = PRCP * FPICE


  END SUBROUTINE ATM

  subroutine noahmp_options(iopt_snf)
 
   implicit none

    INTEGER,  INTENT(IN) :: iopt_snf  !rainfall & snowfall (1-Jordan91; 2->BATS; 3->Noah)

    opt_snf  = iopt_snf  

  end subroutine noahmp_options

end module forcing_routines
