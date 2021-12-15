!!!! in the noahmplsm subroutine:
! compute energy budget (momentum & energy fluxes and phase changes) 

    CALL ENERGY (parameters,ICE    ,VEGTYP ,IST    ,NSNOW  ,NSOIL  , & !in
                 ISNOW  ,DT     ,RHOAIR ,SFCPRS ,QAIR   , & !in
                 SFCTMP ,THAIR  ,LWDN   ,UU     ,VV     ,ZLVL   , & !in
                 CO2AIR ,O2AIR  ,SOLAD  ,SOLAI  ,COSZ   ,IGS    , & !in
                 EAIR   ,TBOT   ,ZSNSO  ,ZSOIL  , & !in
                 ELAI   ,ESAI   ,FWET   ,FOLN   ,         & !in
                 FVEG   ,PAHV   ,PAHG   ,PAHB   ,                 & !in
                 QSNOW  ,DZSNSO ,LAT    ,CANLIQ ,CANICE ,iloc, jloc , & !in
		 Z0WRF  ,                                         &
                 IMELT  ,SNICEV ,SNLIQV ,EPORE  ,T2M    ,FSNO   , & !out
                 SAV    ,SAG    ,QMELT  ,FSA    ,FSR    ,TAUX   , & !out
                 TAUY   ,FIRA   ,FSH    ,FCEV   ,FGEV   ,FCTR   , & !out
                 TRAD   ,PSN    ,APAR   ,SSOIL  ,BTRANI ,BTRAN  , & !out
                 PONDING,TS     ,LATHEAV , LATHEAG , frozen_canopy,frozen_ground,& !out
                 TV     ,TG     ,STC    ,SNOWH  ,EAH    ,TAH    , & !inout
                 SNEQVO ,SNEQV  ,SH2O   ,SMC    ,SNICE  ,SNLIQ  , & !inout
                 ALBOLD ,CM     ,CH     ,DX     ,DZ8W   ,Q2     , & !inout
                 TAUSS  ,LAISUN ,LAISHA ,RB ,                     & !inout
!jref:start
                 QC     ,QSFC   ,PSFC   , & !in 
                 T2MV   ,T2MB  ,FSRV   , &
                 FSRG   ,RSSUN   ,RSSHA ,ALBSND  ,ALBSNI, BGAP   ,WGAP,TGV,TGB,&
                 Q1     ,Q2V    ,Q2B    ,Q2E    ,CHV   ,CHB     , & !out
                 EMISSI ,PAH    ,                                 &
		 SHG,SHC,SHB,EVG,EVB,GHV,GHB,IRG,IRC,IRB,TR,EVC,CHLEAF,CHUC,CHV2,CHB2 ) !out
!jref:end


!!!!! in the Energy subroutine:
!== begin energy ===================================================================================

  SUBROUTINE ENERGY (parameters,ICE    ,VEGTYP ,IST    ,NSNOW  ,NSOIL  , & !in
                     ISNOW  ,DT     ,RHOAIR ,SFCPRS ,QAIR   , & !in
                     SFCTMP ,THAIR  ,LWDN   ,UU     ,VV     ,ZREF   , & !in
                     CO2AIR ,O2AIR  ,SOLAD  ,SOLAI  ,COSZ   ,IGS    , & !in
                     EAIR   ,TBOT   ,ZSNSO  ,ZSOIL  , & !in
                     ELAI   ,ESAI   ,FWET   ,FOLN   ,         & !in
                     FVEG   ,PAHV   ,PAHG   ,PAHB   ,                 & !in
                     QSNOW  ,DZSNSO ,LAT    ,CANLIQ ,CANICE ,ILOC   , JLOC, & !in
		     Z0WRF  ,                                         &
                     IMELT  ,SNICEV ,SNLIQV ,EPORE  ,T2M    ,FSNO   , & !out
                     SAV    ,SAG    ,QMELT  ,FSA    ,FSR    ,TAUX   , & !out
                     TAUY   ,FIRA   ,FSH    ,FCEV   ,FGEV   ,FCTR   , & !out
                     TRAD   ,PSN    ,APAR   ,SSOIL  ,BTRANI ,BTRAN  , & !out
                     PONDING,TS     ,LATHEAV , LATHEAG , frozen_canopy,frozen_ground,                       & !out
                     TV     ,TG     ,STC    ,SNOWH  ,EAH    ,TAH    , & !inout
                     SNEQVO ,SNEQV  ,SH2O   ,SMC    ,SNICE  ,SNLIQ  , & !inout
                     ALBOLD ,CM     ,CH     ,DX     ,DZ8W   ,Q2     , &   !inout
                     TAUSS  ,LAISUN ,LAISHA ,RB ,                     & !inout
!jref:start
                     QC     ,QSFC   ,PSFC   , & !in 
                     T2MV   ,T2MB   ,FSRV   , &
                     FSRG   ,RSSUN  ,RSSHA  ,ALBSND  ,ALBSNI,BGAP   ,WGAP,TGV,TGB,&
                     Q1     ,Q2V    ,Q2B    ,Q2E    ,CHV  ,CHB, EMISSI,PAH  ,&
		     SHG,SHC,SHB,EVG,EVB,GHV,GHB,IRG,IRC,IRB,TR,EVC,CHLEAF,CHUC,CHV2,CHB2 )   !out 
!jref:end                            

! --------------------------------------------------------------------------------------------------
! we use different approaches to deal with subgrid features of radiation transfer and turbulent
! transfer. We use 'tile' approach to compute turbulent fluxes, while we use modified two-
! stream to compute radiation transfer. Tile approach, assemblying vegetation canopies together,
! may expose too much ground surfaces (either covered by snow or grass) to solar radiation. The
! modified two-stream assumes vegetation covers fully the gridcell but with gaps between tree
! crowns.
! --------------------------------------------------------------------------------------------------
! turbulence transfer : 'tile' approach to compute energy fluxes in vegetated fraction and
!                         bare fraction separately and then sum them up weighted by fraction
!                     --------------------------------------
!                    / O  O  O  O  O  O  O  O  /          / 
!                   /  |  |  |  |  |  |  |  | /          /
!                  / O  O  O  O  O  O  O  O  /          /
!                 /  |  |  |tile1|  |  |  | /  tile2   /
!                / O  O  O  O  O  O  O  O  /  bare    /
!               /  |  |  | vegetated |  | /          /
!              / O  O  O  O  O  O  O  O  /          /
!             /  |  |  |  |  |  |  |  | /          /
!            --------------------------------------
! --------------------------------------------------------------------------------------------------
! radiation transfer : modified two-stream (Yang and Friedl, 2003, JGR; Niu ang Yang, 2004, JGR)
!                     --------------------------------------  two-stream treats leaves as
!                    /   O   O   O   O   O   O   O   O    /  cloud over the entire grid-cell,
!                   /    |   |   |   |   |   |   |   |   / while the modified two-stream 
!                  /   O   O   O   O   O   O   O   O    / aggregates cloudy leaves into  
!                 /    |   |   |   |   |   |   |   |   / tree crowns with gaps (as shown in
!                /   O   O   O   O   O   O   O   O    / the left figure). We assume these
!               /    |   |   |   |   |   |   |   |   / tree crowns are evenly distributed
!              /   O   O   O   O   O   O   O   O    / within the gridcell with 100% veg
!             /    |   |   |   |   |   |   |   |   / fraction, but with gaps. The 'tile'
!            -------------------------------------- approach overlaps too much shadows.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs
  type (noahmp_parameters), intent(in) :: parameters
  integer                           , INTENT(IN)    :: ILOC
  integer                           , INTENT(IN)    :: JLOC
  INTEGER                           , INTENT(IN)    :: ICE    !ice (ice = 1)
  INTEGER                           , INTENT(IN)    :: VEGTYP !vegetation physiology type
  INTEGER                           , INTENT(IN)    :: IST    !surface type: 1->soil; 2->lake
  INTEGER                           , INTENT(IN)    :: NSNOW  !maximum no. of snow layers        
  INTEGER                           , INTENT(IN)    :: NSOIL  !number of soil layers
  INTEGER                           , INTENT(IN)    :: ISNOW  !actual no. of snow layers
  REAL                              , INTENT(IN)    :: DT     !time step [sec]
  REAL                              , INTENT(IN)    :: QSNOW  !snowfall on the ground (mm/s)
  REAL                              , INTENT(IN)    :: RHOAIR !density air (kg/m3)
  REAL                              , INTENT(IN)    :: EAIR   !vapor pressure air (pa)
  REAL                              , INTENT(IN)    :: SFCPRS !pressure (pa)
  REAL                              , INTENT(IN)    :: QAIR   !specific humidity (kg/kg)
  REAL                              , INTENT(IN)    :: SFCTMP !air temperature (k)
  REAL                              , INTENT(IN)    :: THAIR  !potential temperature (k)
  REAL                              , INTENT(IN)    :: LWDN   !downward longwave radiation (w/m2)
  REAL                              , INTENT(IN)    :: UU     !wind speed in e-w dir (m/s)
  REAL                              , INTENT(IN)    :: VV     !wind speed in n-s dir (m/s)
  REAL   , DIMENSION(       1:    2), INTENT(IN)    :: SOLAD  !incoming direct solar rad. (w/m2)
  REAL   , DIMENSION(       1:    2), INTENT(IN)    :: SOLAI  !incoming diffuse solar rad. (w/m2)
  REAL                              , INTENT(IN)    :: COSZ   !cosine solar zenith angle (0-1)
  REAL                              , INTENT(IN)    :: ELAI   !LAI adjusted for burying by snow
  REAL                              , INTENT(IN)    :: ESAI   !LAI adjusted for burying by snow
  REAL                              , INTENT(IN)    :: FWET   !fraction of canopy that is wet [-]
  REAL                              , INTENT(IN)    :: FVEG   !greeness vegetation fraction (-)
  REAL                              , INTENT(IN)    :: LAT    !latitude (radians)
  REAL                              , INTENT(IN)    :: CANLIQ !canopy-intercepted liquid water (mm)
  REAL                              , INTENT(IN)    :: CANICE !canopy-intercepted ice mass (mm)
  REAL                              , INTENT(IN)    :: FOLN   !foliage nitrogen (%)
  REAL                              , INTENT(IN)    :: CO2AIR !atmospheric co2 concentration (pa)
  REAL                              , INTENT(IN)    :: O2AIR  !atmospheric o2 concentration (pa)
  REAL                              , INTENT(IN)    :: IGS    !growing season index (0=off, 1=on)

  REAL                              , INTENT(IN)    :: ZREF   !reference height (m)
  REAL                              , INTENT(IN)    :: TBOT   !bottom condition for soil temp. (k) 
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: ZSNSO  !layer-bottom depth from snow surf [m]
  REAL   , DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !layer-bottom depth from soil surf [m]
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: DZSNSO !depth of snow & soil layer-bottom [m]
  REAL, INTENT(IN)   :: PAHV    !precipitation advected heat - vegetation net (W/m2)
  REAL, INTENT(IN)   :: PAHG    !precipitation advected heat - under canopy net (W/m2)
  REAL, INTENT(IN)   :: PAHB    !precipitation advected heat - bare ground net (W/m2)

!jref:start; in 
  REAL                              , INTENT(IN)    :: QC     !cloud water mixing ratio
  REAL                              , INTENT(INOUT) :: QSFC   !mixing ratio at lowest model layer
  REAL                              , INTENT(IN)    :: PSFC   !pressure at lowest model layer
  REAL                              , INTENT(IN)    :: DX     !horisontal resolution
  REAL                              , INTENT(IN)    :: DZ8W   !thickness of lowest layer
  REAL                              , INTENT(IN)    :: Q2     !mixing ratio (kg/kg)
!jref:end

! outputs
  REAL                              , INTENT(OUT)   :: Z0WRF  !combined z0 sent to coupled model
  INTEGER, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT)   :: IMELT  !phase change index [1-melt; 2-freeze]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: SNICEV !partial volume ice [m3/m3]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: SNLIQV !partial volume liq. water [m3/m3]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: EPORE  !effective porosity [m3/m3]
  REAL                              , INTENT(OUT)   :: FSNO   !snow cover fraction (-)
  REAL                              , INTENT(OUT)   :: QMELT  !snowmelt [mm/s]
  REAL                              , INTENT(OUT)   :: PONDING!pounding at ground [mm]
  REAL                              , INTENT(OUT)   :: SAV    !solar rad. absorbed by veg. (w/m2)
  REAL                              , INTENT(OUT)   :: SAG    !solar rad. absorbed by ground (w/m2)
  REAL                              , INTENT(OUT)   :: FSA    !tot. absorbed solar radiation (w/m2)
  REAL                              , INTENT(OUT)   :: FSR    !tot. reflected solar radiation (w/m2)
  REAL                              , INTENT(OUT)   :: TAUX   !wind stress: e-w (n/m2)
  REAL                              , INTENT(OUT)   :: TAUY   !wind stress: n-s (n/m2)
  REAL                              , INTENT(OUT)   :: FIRA   !total net LW. rad (w/m2)   [+ to atm]
  REAL                              , INTENT(OUT)   :: FSH    !total sensible heat (w/m2) [+ to atm]
  REAL                              , INTENT(OUT)   :: FCEV   !canopy evaporation (w/m2)  [+ to atm]
  REAL                              , INTENT(OUT)   :: FGEV   !ground evaporation (w/m2)  [+ to atm]
  REAL                              , INTENT(OUT)   :: FCTR   !transpiration (w/m2)       [+ to atm]
  REAL                              , INTENT(OUT)   :: TRAD   !radiative temperature (k)
  REAL                              , INTENT(OUT)   :: T2M    !2 m height air temperature (k)
  REAL                              , INTENT(OUT)   :: PSN    !total photosyn. (umolco2/m2/s) [+]
  REAL                              , INTENT(OUT)   :: APAR   !total photosyn. active energy (w/m2)
  REAL                              , INTENT(OUT)   :: SSOIL  !ground heat flux (w/m2)   [+ to soil]
  REAL   , DIMENSION(       1:NSOIL), INTENT(OUT)   :: BTRANI !soil water transpiration factor (0-1)
  REAL                              , INTENT(OUT)   :: BTRAN  !soil water transpiration factor (0-1)
!  REAL                              , INTENT(OUT)   :: LATHEA !latent heat vap./sublimation (j/kg)
  REAL                              , INTENT(OUT)   :: LATHEAV !latent heat vap./sublimation (j/kg)
  REAL                              , INTENT(OUT)   :: LATHEAG !latent heat vap./sublimation (j/kg)
  LOGICAL                           , INTENT(OUT)   :: FROZEN_GROUND ! used to define latent heat pathway
  LOGICAL                           , INTENT(OUT)   :: FROZEN_CANOPY ! used to define latent heat pathway

!jref:start  
  REAL                              , INTENT(OUT)   :: FSRV    !veg. reflected solar radiation (w/m2)
  REAL                              , INTENT(OUT)   :: FSRG    !ground reflected solar radiation (w/m2)
  REAL, INTENT(OUT) :: RSSUN        !sunlit leaf stomatal resistance (s/m)
  REAL, INTENT(OUT) :: RSSHA        !shaded leaf stomatal resistance (s/m)
!jref:end - out for debug  

!jref:start; output
  REAL                              , INTENT(OUT)   :: T2MV   !2-m air temperature over vegetated part [k]
  REAL                              , INTENT(OUT)   :: T2MB   !2-m air temperature over bare ground part [k]
  REAL                              , INTENT(OUT)   :: BGAP
  REAL                              , INTENT(OUT)   :: WGAP
  REAL, DIMENSION(1:2)              , INTENT(OUT)   :: ALBSND   !snow albedo (direct)
  REAL, DIMENSION(1:2)              , INTENT(OUT)   :: ALBSNI   !snow albedo (diffuse)
!jref:end

! input & output
  REAL                              , INTENT(INOUT) :: TS     !surface temperature (k)
  REAL                              , INTENT(INOUT) :: TV     !vegetation temperature (k)
  REAL                              , INTENT(INOUT) :: TG     !ground temperature (k)
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow/soil temperature [k]
  REAL                              , INTENT(INOUT) :: SNOWH  !snow height [m]
  REAL                              , INTENT(INOUT) :: SNEQV  !snow mass (mm)
  REAL                              , INTENT(INOUT) :: SNEQVO !snow mass at last time step (mm)
  REAL   , DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !liquid soil moisture [m3/m3]
  REAL   , DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC    !soil moisture (ice + liq.) [m3/m3]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow ice mass (kg/m2)
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow liq mass (kg/m2)
  REAL                              , INTENT(INOUT) :: EAH    !canopy air vapor pressure (pa)
  REAL                              , INTENT(INOUT) :: TAH    !canopy air temperature (k)
  REAL                              , INTENT(INOUT) :: ALBOLD !snow albedo at last time step(CLASS type)
  REAL                              , INTENT(INOUT) :: TAUSS  !non-dimensional snow age
  REAL                              , INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL                              , INTENT(INOUT) :: CH     !sensible heat exchange coefficient
  REAL                              , INTENT(INOUT) :: Q1
  REAL                              , INTENT(INOUT) :: RB     !leaf boundary layer resistance (s/m)
  REAL                              , INTENT(INOUT) :: LAISUN !sunlit leaf area index (m2/m2)
  REAL                              , INTENT(INOUT) :: LAISHA !shaded leaf area index (m2/m2)
!  REAL                                              :: Q2E
  REAL,                               INTENT(OUT)   :: EMISSI
  REAL,                               INTENT(OUT)   :: PAH    !precipitation advected heat - total (W/m2)

! local
  INTEGER                                           :: IZ     !do-loop index
  LOGICAL                                           :: VEG    !true if vegetated surface
  REAL                                              :: UR     !wind speed at height ZLVL (m/s)
  REAL                                              :: ZLVL   !reference height (m)
  REAL                                              :: FSUN   !sunlit fraction of canopy [-]
 ! REAL                                              :: RB     !leaf boundary layer resistance (s/m)
  REAL                                              :: RSURF  !ground surface resistance (s/m)
  REAL                                              :: L_RSURF!Dry-layer thickness for computing RSURF (Sakaguchi and Zeng, 2009)
  REAL                                              :: D_RSURF!Reduced vapor diffusivity in soil for computing RSURF (SZ09)
  REAL                                              :: BEVAP  !soil water evaporation factor (0- 1)
  REAL                                              :: MOL    !Monin-Obukhov length (m)
  REAL                                              :: VAI    !sum of LAI  + stem area index [m2/m2]
  REAL                                              :: CWP    !canopy wind extinction parameter
  REAL                                              :: ZPD    !zero plane displacement (m)
  REAL                                              :: Z0M    !z0 momentum (m)
  REAL                                              :: ZPDG   !zero plane displacement (m)
  REAL                                              :: Z0MG   !z0 momentum, ground (m)
  REAL                                              :: EMV    !vegetation emissivity
  REAL                                              :: EMG    !ground emissivity
  REAL                                              :: FIRE   !emitted IR (w/m2)

  REAL                                              :: PSNSUN !sunlit photosynthesis (umolco2/m2/s)
  REAL                                              :: PSNSHA !shaded photosynthesis (umolco2/m2/s)
!jref:start - for debug  
!  REAL                                              :: RSSUN  !sunlit stomatal resistance (s/m)
!  REAL                                              :: RSSHA  !shaded stomatal resistance (s/m)
!jref:end - for debug
  REAL                                              :: PARSUN !par absorbed per sunlit LAI (w/m2)
  REAL                                              :: PARSHA !par absorbed per shaded LAI (w/m2)

  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: FACT   !temporary used in phase change
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: DF     !thermal conductivity [w/m/k]
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: HCPCT  !heat capacity [j/m3/k]
  REAL                                              :: BDSNO  !bulk density of snow (kg/m3)
  REAL                                              :: FMELT  !melting factor for snow cover frac
  REAL                                              :: GX     !temporary variable
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: PHI    !light through water (w/m2)
!  REAL                                              :: GAMMA  !psychrometric constant (pa/k)
  REAL                                              :: GAMMAV  !psychrometric constant (pa/k)
  REAL                                              :: GAMMAG  !psychrometric constant (pa/k)
  REAL                                              :: PSI    !surface layer soil matrix potential (m)
  REAL                                              :: RHSUR  !raltive humidity in surface soil/snow air space (-)

! temperature and fluxes over vegetated fraction

  REAL                                              :: TAUXV  !wind stress: e-w dir [n/m2]
  REAL                                              :: TAUYV  !wind stress: n-s dir [n/m2]
  REAL,INTENT(OUT)                                              :: IRC    !canopy net LW rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: IRG    !ground net LW rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: SHC    !canopy sen. heat [w/m2]   [+ to atm]
  REAL,INTENT(OUT)                                              :: SHG    !ground sen. heat [w/m2]   [+ to atm]
!jref:start  
  REAL,INTENT(OUT)                                  :: Q2V
  REAL,INTENT(OUT)                                  :: Q2B
  REAL,INTENT(OUT)                                  :: Q2E
!jref:end  
  REAL,INTENT(OUT)                                              :: EVC    !canopy evap. heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: EVG    !ground evap. heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: TR     !transpiration heat [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: GHV    !ground heat flux [w/m2]  [+ to soil]
  REAL,INTENT(OUT)                                  :: TGV    !ground surface temp. [k]
  REAL                                              :: CMV    !momentum drag coefficient
  REAL,INTENT(OUT)                                  :: CHV    !sensible heat exchange coefficient

! temperature and fluxes over bare soil fraction

  REAL                                              :: TAUXB  !wind stress: e-w dir [n/m2]
  REAL                                              :: TAUYB  !wind stress: n-s dir [n/m2]
  REAL,INTENT(OUT)                                              :: IRB    !net longwave rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: SHB    !sensible heat [w/m2]     [+ to atm]
  REAL,INTENT(OUT)                                              :: EVB    !evaporation heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: GHB    !ground heat flux [w/m2] [+ to soil]
  REAL,INTENT(OUT)                                  :: TGB    !ground surface temp. [k]
  REAL                                              :: CMB    !momentum drag coefficient
  REAL,INTENT(OUT)                                  :: CHB    !sensible heat exchange coefficient
  REAL,INTENT(OUT)                                  :: CHLEAF !leaf exchange coefficient
  REAL,INTENT(OUT)                                  :: CHUC   !under canopy exchange coefficient
!jref:start  
  REAL,INTENT(OUT)                                  :: CHV2    !sensible heat conductance, canopy air to ZLVL air (m/s)
  REAL,INTENT(OUT)                                  :: CHB2    !sensible heat conductance, canopy air to ZLVL air (m/s)
  REAL                                  :: noahmpres

!jref:end  

  REAL, PARAMETER                   :: MPE    = 1.0E-6
  REAL, PARAMETER                   :: PSIWLT = -150.0  !metric potential for wilting point (m)
  REAL, PARAMETER                   :: Z0     = 0.002  ! Bare-soil roughness length (m) (i.e., under the canopy)

! ---------------------------------------------------------------------------------------------------
! initialize fluxes from veg. fraction

    TAUXV     = 0.0    
    TAUYV     = 0.0
    IRC       = 0.0
    SHC       = 0.0
    IRG       = 0.0
    SHG       = 0.0
    EVG       = 0.0      
    EVC       = 0.0
    TR        = 0.0
    GHV       = 0.0      
    PSNSUN    = 0.0
    PSNSHA    = 0.0
    T2MV      = 0.0
    Q2V       = 0.0
    CHV       = 0.0
    CHLEAF    = 0.0
    CHUC      = 0.0
    CHV2      = 0.0
    RB        = 0.0

! wind speed at reference height: ur >= 1

    UR = MAX( SQRT(UU**2.0 + VV**2.0), 1.0 )

! vegetated or non-vegetated

    VAI = ELAI + ESAI
    VEG = .FALSE.
    IF(VAI > 0.0) VEG = .TRUE.

! ground snow cover fraction [Niu and Yang, 2007, JGR]

     FSNO = 0.0
     IF(SNOWH .GT. 0.0)  THEN
         BDSNO    = SNEQV / SNOWH
         FMELT    = (BDSNO/100.0)**parameters%MFSNO
         !FSNO     = TANH( SNOWH /(2.5* Z0 * FMELT))
         FSNO     = TANH( SNOWH /(parameters%SCFFAC * FMELT)) ! C.He: bring hard-coded 2.5*z0 to MPTABLE tunable parameter SCFFAC
     ENDIF

! ground roughness length

     IF(IST == 2) THEN
       IF(TG .LE. TFRZ) THEN
         Z0MG = 0.01 * (1.0-FSNO) + FSNO * parameters%Z0SNO
       ELSE
         Z0MG = 0.01  
       END IF
     ELSE
       Z0MG = Z0 * (1.0-FSNO) + FSNO * parameters%Z0SNO
     END IF

! roughness length and displacement height

     ZPDG  = SNOWH
     IF(VEG) THEN
        Z0M  = parameters%Z0MVT
        ZPD  = 0.65 * parameters%HVT
        IF(SNOWH.GT.ZPD) ZPD  = SNOWH
     ELSE
        Z0M  = Z0MG
        ZPD  = ZPDG
     END IF

! special case for urban

     IF (parameters%urban_flag) THEN
       Z0MG = parameters%Z0MVT
       ZPDG  = 0.65 * parameters%HVT
       Z0M  = Z0MG
       ZPD  = ZPDG
     END IF

     ZLVL = MAX(ZPD,parameters%HVT) + ZREF
     IF(ZPDG >= ZLVL) ZLVL = ZPDG + ZREF
!     UR   = UR*LOG(ZLVL/Z0M)/LOG(10./Z0M)       !input UR is at 10m

! canopy wind absorption coeffcient

     CWP = parameters%CWPVT

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

! vegetation and ground emissivity

     EMV = 1.0 - EXP(-(ELAI+ESAI)/1.0)
     IF (ICE == 1) THEN
       EMG = 0.98*(1.0-FSNO) + parameters%SNOW_EMIS*FSNO  ! move hard-coded snow emissivity as a global parameter to MPTABLE
     ELSE
       EMG = parameters%EG(IST)*(1.0-FSNO) + parameters%SNOW_EMIS*FSNO
     END IF

! soil moisture factor controlling stomatal resistance
   
     BTRAN = 0.0

     IF(IST ==1 ) THEN
       DO IZ = 1, parameters%NROOT
          IF(OPT_BTR == 1) then                  ! Noah
            GX    = (SH2O(IZ)-parameters%SMCWLT(IZ)) / (parameters%SMCREF(IZ)-parameters%SMCWLT(IZ))
          END IF
          IF(OPT_BTR == 2) then                  ! CLM
            PSI   = MAX(PSIWLT,-parameters%PSISAT(IZ)*(MAX(0.01,SH2O(IZ))/parameters%SMCMAX(IZ))**(-parameters%BEXP(IZ)) )
            GX    = (1.0-PSI/PSIWLT)/(1.0+parameters%PSISAT(IZ)/PSIWLT)
          END IF
          IF(OPT_BTR == 3) then                  ! SSiB
            PSI   = MAX(PSIWLT,-parameters%PSISAT(IZ)*(MAX(0.01,SH2O(IZ))/parameters%SMCMAX(IZ))**(-parameters%BEXP(IZ)) )
            GX    = 1.0-EXP(-5.8*(LOG(PSIWLT/PSI))) 
          END IF
       
          GX = MIN(1.0,MAX(0.0,GX))
          BTRANI(IZ) = MAX(MPE,DZSNSO(IZ) / (-ZSOIL(parameters%NROOT)) * GX)
          BTRAN      = BTRAN + BTRANI(IZ)
       END DO
       BTRAN = MAX(MPE,BTRAN)

       BTRANI(1:parameters%NROOT) = BTRANI(1:parameters%NROOT)/BTRAN
     END IF

! soil surface resistance for ground evap.

     BEVAP = MAX(0.0,SH2O(1)/parameters%SMCMAX(1))
     IF(IST == 2) THEN
       RSURF = 1.0          ! avoid being divided by 0
       RHSUR = 1.0
     ELSE

       IF(OPT_RSF == 1 .OR. OPT_RSF == 4) THEN
         ! RSURF based on Sakaguchi and Zeng, 2009
         ! taking the "residual water content" to be the wilting point, 
         ! and correcting the exponent on the D term (typo in SZ09 ?)
         L_RSURF = (-ZSOIL(1)) * ( exp ( (1.0 - MIN(1.0,SH2O(1)/parameters%SMCMAX(1))) ** parameters%RSURF_EXP ) - 1.0 ) / ( 2.71828 - 1.0 ) 
         D_RSURF = 2.2E-5 * parameters%SMCMAX(1) * parameters%SMCMAX(1) * ( 1.0 - parameters%SMCWLT(1) / parameters%SMCMAX(1) ) ** (2.0+3.0/parameters%BEXP(1))
         RSURF = L_RSURF / D_RSURF
       ELSEIF(OPT_RSF == 2) THEN
         RSURF = FSNO * 1.0 + (1.0-FSNO)* EXP(8.25-4.225*BEVAP) !Sellers (1992) ! Older RSURF computations
       ELSEIF(OPT_RSF == 3) THEN
         RSURF = FSNO * 1.0 + (1.0-FSNO)* EXP(8.25-6.0  *BEVAP) !adjusted to decrease RSURF for wet soil
       ENDIF

       IF(OPT_RSF == 4) THEN  ! AD: FSNO weighted; snow RSURF set in MPTABLE v3.8
         RSURF = 1.0 / (FSNO * (1.0/parameters%RSURF_SNOW) + (1.0-FSNO) * (1.0/max(RSURF, 0.001)))
       ENDIF

       IF(SH2O(1) < 0.01 .and. SNOWH == 0.0) RSURF = 1.0E6
       PSI   = -parameters%PSISAT(1)*(MAX(0.01,SH2O(1))/parameters%SMCMAX(1))**(-parameters%BEXP(1))   
       RHSUR = FSNO + (1.0-FSNO) * EXP(PSI*GRAV/(RW*TG)) 
     END IF

! urban - jref 
     IF (parameters%urban_flag .and. SNOWH == 0.0 ) THEN
        RSURF = 1.0E6
     ENDIF

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

!     IF (SFCTMP .GT. TFRZ) THEN
!        LATHEA = HVAP
!     ELSE
!        LATHEA = HSUB
!     END IF
!     GAMMA = CPAIR*SFCPRS/(0.622*LATHEA)

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
!jref:start
                    QC      ,QSFC    ,PSFC    , & !in
                    Q2V     ,CHV2, CHLEAF, CHUC)               !inout 
!jref:end                            
    END IF

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
!jref:start
                    QC      ,QSFC    ,PSFC    , & !in
                    SFCPRS  ,Q2B,   CHB2)                          !in 
!jref:end                            

!energy balance at vege canopy: SAV          =(IRC+SHC+EVC+TR)     *FVEG  at   FVEG 
!energy balance at vege ground: SAG*    FVEG =(IRG+SHG+EVG+GHV)    *FVEG  at   FVEG
!energy balance at bare ground: SAG*(1.-FVEG)=(IRB+SHB+EVB+GHB)*(1.-FVEG) at 1-FVEG

    IF (VEG .AND. FVEG > 0) THEN 
        TAUX  = FVEG * TAUXV     + (1.0 - FVEG) * TAUXB
        TAUY  = FVEG * TAUYV     + (1.0 - FVEG) * TAUYB
        FIRA  = FVEG * IRG       + (1.0 - FVEG) * IRB       + IRC
        FSH   = FVEG * SHG       + (1.0 - FVEG) * SHB       + SHC
        FGEV  = FVEG * EVG       + (1.0 - FVEG) * EVB
        SSOIL = FVEG * GHV       + (1.0 - FVEG) * GHB
        FCEV  = EVC
        FCTR  = TR
	PAH   = FVEG * PAHG      + (1.0 - FVEG) * PAHB   + PAHV
        TG    = FVEG * TGV       + (1.0 - FVEG) * TGB
        T2M   = FVEG * T2MV      + (1.0 - FVEG) * T2MB
        TS    = FVEG * TV        + (1.0 - FVEG) * TGB
        CM    = FVEG * CMV       + (1.0 - FVEG) * CMB      ! better way to average?
        CH    = FVEG * CHV       + (1.0 - FVEG) * CHB
        Q1    = FVEG * (EAH*0.622/(SFCPRS - 0.378*EAH)) + (1.0 - FVEG)*QSFC
        Q2E   = FVEG * Q2V       + (1.0 - FVEG) * Q2B
	Z0WRF = Z0M
    ELSE
        TAUX  = TAUXB
        TAUY  = TAUYB
        FIRA  = IRB
        FSH   = SHB
        FGEV  = EVB
        SSOIL = GHB
        TG    = TGB
        T2M   = T2MB
        FCEV  = 0.0
        FCTR  = 0.0
	PAH   = PAHB
        TS    = TG
        CM    = CMB
        CH    = CHB
        Q1    = QSFC
        Q2E   = Q2B
        RSSUN = 0.0
        RSSHA = 0.0
        TGV   = TGB
        CHV   = CHB
	Z0WRF = Z0MG
    END IF

    FIRE = LWDN + FIRA

    IF(FIRE <=0.0) THEN
       WRITE(6,*) 'emitted longwave <0; skin T may be wrong due to inconsistent'
       WRITE(6,*) 'input of SHDFAC with LAI'
       WRITE(6,*) ILOC, JLOC, 'SHDFAC=',FVEG,'VAI=',VAI,'TV=',TV,'TG=',TG
       WRITE(6,*) 'LWDN=',LWDN,'FIRA=',FIRA,'SNOWH=',SNOWH
       call wrf_error_fatal("STOP in Noah-MP")
    END IF

    ! Compute a net emissivity
    EMISSI = FVEG * ( EMG*(1-EMV) + EMV + EMV*(1-EMV)*(1-EMG) ) + &
         (1-FVEG) * EMG

    ! When we're computing a TRAD, subtract from the emitted IR the
    ! reflected portion of the incoming LWDN, so we're just
    ! considering the IR originating in the canopy/ground system.
    
    TRAD = ( ( FIRE - (1-EMISSI)*LWDN ) / (EMISSI*SB) ) ** 0.25

    ! Old TRAD calculation not taking into account Emissivity:
    ! TRAD = (FIRE/SB)**0.25

    APAR = PARSUN*LAISUN + PARSHA*LAISHA
    PSN  = PSNSUN*LAISUN + PSNSHA*LAISHA

! 3L snow & 4L soil temperatures

    CALL TSNOSOI (parameters,ICE     ,NSOIL   ,NSNOW   ,ISNOW   ,IST     , & !in
                  TBOT    ,ZSNSO   ,SSOIL   ,DF      ,HCPCT   , & !in
                  SAG     ,DT      ,SNOWH   ,DZSNSO  , & !in
                  TG      ,ILOC    ,JLOC    ,                   & !in
                  STC     )                                       !inout

! adjusting snow surface temperature
     IF(OPT_STC == 2) THEN
      IF (SNOWH > 0.05 .AND. TG > TFRZ) THEN
        TGV = TFRZ
        TGB = TFRZ
          IF (VEG .AND. FVEG > 0) THEN
             TG    = FVEG * TGV       + (1.0 - FVEG) * TGB
             TS    = FVEG * TV        + (1.0 - FVEG) * TGB
          ELSE
             TG    = TGB
             TS    = TGB
          END IF
      END IF
     END IF

! Energy released or consumed by snow & frozen soil

 CALL PHASECHANGE (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,FACT    , & !in
                   DZSNSO  ,HCPCT   ,IST     ,ILOC    ,JLOC    , & !in
                   STC     ,SNICE   ,SNLIQ   ,SNEQV   ,SNOWH   , & !inout
                   SMC     ,SH2O    ,                            & !inout
                   QMELT   ,IMELT   ,PONDING )                     !out


  END SUBROUTINE ENERGY




!== begin radiation ================================================================================

  SUBROUTINE RADIATION (parameters,VEGTYP  ,IST     ,ICE     ,NSOIL   , & !in
                        SNEQVO  ,SNEQV   ,DT      ,COSZ    ,SNOWH   , & !in
                        TG      ,TV      ,FSNO    ,QSNOW   ,FWET    , & !in
                        ELAI    ,ESAI    ,SMC     ,SOLAD   ,SOLAI   , & !in
                        FVEG    ,ILOC    ,JLOC    ,                   & !in
                        ALBOLD  ,TAUSS   ,                            & !inout
                        FSUN    ,LAISUN  ,LAISHA  ,PARSUN  ,PARSHA  , & !out
                        SAV     ,SAG     ,FSR     ,FSA     ,FSRV    , &
                        FSRG    ,ALBSND  ,ALBSNI  ,BGAP    ,WGAP    )   !out
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER, INTENT(IN)                  :: ILOC
  INTEGER, INTENT(IN)                  :: JLOC
  INTEGER, INTENT(IN)                  :: VEGTYP !vegetation type
  INTEGER, INTENT(IN)                  :: IST    !surface type
  INTEGER, INTENT(IN)                  :: ICE    !ice (ice = 1)
  INTEGER, INTENT(IN)                  :: NSOIL  !number of soil layers

  REAL, INTENT(IN)                     :: DT     !time step [s]
  REAL, INTENT(IN)                     :: QSNOW  !snowfall (mm/s)
  REAL, INTENT(IN)                     :: SNEQVO !snow mass at last time step(mm)
  REAL, INTENT(IN)                     :: SNEQV  !snow mass (mm)
  REAL, INTENT(IN)                     :: SNOWH  !snow height (mm)
  REAL, INTENT(IN)                     :: COSZ   !cosine solar zenith angle (0-1)
  REAL, INTENT(IN)                     :: TG     !ground temperature (k)
  REAL, INTENT(IN)                     :: TV     !vegetation temperature (k)
  REAL, INTENT(IN)                     :: ELAI   !LAI, one-sided, adjusted for burying by snow
  REAL, INTENT(IN)                     :: ESAI   !SAI, one-sided, adjusted for burying by snow
  REAL, INTENT(IN)                     :: FWET   !fraction of canopy that is wet
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SMC    !volumetric soil water [m3/m3]
  REAL, DIMENSION(1:2)    , INTENT(IN) :: SOLAD  !incoming direct solar radiation (w/m2)
  REAL, DIMENSION(1:2)    , INTENT(IN) :: SOLAI  !incoming diffuse solar radiation (w/m2)
  REAL, INTENT(IN)                     :: FSNO   !snow cover fraction (-)
  REAL, INTENT(IN)                     :: FVEG   !green vegetation fraction [0.0-1.0]

! inout
  REAL,                  INTENT(INOUT) :: ALBOLD !snow albedo at last time step (CLASS type)
  REAL,                  INTENT(INOUT) :: TAUSS  !non-dimensional snow age.

! output
  REAL, INTENT(OUT)                    :: FSUN   !sunlit fraction of canopy (-)
  REAL, INTENT(OUT)                    :: LAISUN !sunlit leaf area (-)
  REAL, INTENT(OUT)                    :: LAISHA !shaded leaf area (-)
  REAL, INTENT(OUT)                    :: PARSUN !average absorbed par for sunlit leaves (w/m2)
  REAL, INTENT(OUT)                    :: PARSHA !average absorbed par for shaded leaves (w/m2)
  REAL, INTENT(OUT)                    :: SAV    !solar radiation absorbed by vegetation (w/m2)
  REAL, INTENT(OUT)                    :: SAG    !solar radiation absorbed by ground (w/m2)
  REAL, INTENT(OUT)                    :: FSA    !total absorbed solar radiation (w/m2)
  REAL, INTENT(OUT)                    :: FSR    !total reflected solar radiation (w/m2)

!jref:start  
  REAL, INTENT(OUT)                    :: FSRV    !veg. reflected solar radiation (w/m2)
  REAL, INTENT(OUT)                    :: FSRG    !ground reflected solar radiation (w/m2)
  REAL, INTENT(OUT)                    :: BGAP
  REAL, INTENT(OUT)                    :: WGAP
  REAL, DIMENSION(1:2), INTENT(OUT)    :: ALBSND   !snow albedo (direct)
  REAL, DIMENSION(1:2), INTENT(OUT)    :: ALBSNI   !snow albedo (diffuse)
!jref:end  

! local
  REAL                                 :: FAGE   !snow age function (0 - new snow)
  REAL, DIMENSION(1:2)                 :: ALBGRD !ground albedo (direct)
  REAL, DIMENSION(1:2)                 :: ALBGRI !ground albedo (diffuse)
  REAL, DIMENSION(1:2)                 :: ALBD   !surface albedo (direct)
  REAL, DIMENSION(1:2)                 :: ALBI   !surface albedo (diffuse)
  REAL, DIMENSION(1:2)                 :: FABD   !flux abs by veg (per unit direct flux)
  REAL, DIMENSION(1:2)                 :: FABI   !flux abs by veg (per unit diffuse flux)
  REAL, DIMENSION(1:2)                 :: FTDD   !down direct flux below veg (per unit dir flux)
  REAL, DIMENSION(1:2)                 :: FTID   !down diffuse flux below veg (per unit dir flux)
  REAL, DIMENSION(1:2)                 :: FTII   !down diffuse flux below veg (per unit dif flux)
!jref:start  
  REAL, DIMENSION(1:2)                 :: FREVI
  REAL, DIMENSION(1:2)                 :: FREVD
  REAL, DIMENSION(1:2)                 :: FREGI
  REAL, DIMENSION(1:2)                 :: FREGD
!jref:end

  REAL                                 :: FSHA   !shaded fraction of canopy
  REAL                                 :: VAI    !total LAI + stem area index, one sided

  REAL,PARAMETER :: MPE = 1.0E-6
  LOGICAL VEG  !true: vegetated for surface temperature calculation

! --------------------------------------------------------------------------------------------------

! surface abeldo

   CALL ALBEDO (parameters,VEGTYP ,IST    ,ICE    ,NSOIL  , & !in
                DT     ,COSZ   ,FAGE   ,ELAI   ,ESAI   , & !in
                TG     ,TV     ,SNOWH  ,FSNO   ,FWET   , & !in
                SMC    ,SNEQVO ,SNEQV  ,QSNOW  ,FVEG   , & !in
                ILOC   ,JLOC   ,                         & !in
                ALBOLD ,TAUSS                          , & !inout
                ALBGRD ,ALBGRI ,ALBD   ,ALBI   ,FABD   , & !out
                FABI   ,FTDD   ,FTID   ,FTII   ,FSUN   , & !)   !out
                FREVI  ,FREVD   ,FREGD ,FREGI  ,BGAP   , & !inout
                WGAP   ,ALBSND ,ALBSNI )

! surface radiation

     FSHA = 1.0-FSUN
     LAISUN = ELAI*FSUN
     LAISHA = ELAI*FSHA
     VAI = ELAI+ ESAI
     IF (VAI .GT. 0.0) THEN
        VEG = .TRUE.
     ELSE
        VEG = .FALSE.
     END IF

   CALL SURRAD (parameters,MPE    ,FSUN   ,FSHA   ,ELAI   ,VAI    , & !in
                LAISUN ,LAISHA ,SOLAD  ,SOLAI  ,FABD   , & !in
                FABI   ,FTDD   ,FTID   ,FTII   ,ALBGRD , & !in
                ALBGRI ,ALBD   ,ALBI   ,ILOC   ,JLOC   , & !in
                PARSUN ,PARSHA ,SAV    ,SAG    ,FSA    , & !out
                FSR    ,                                 & !out
                FREVI  ,FREVD  ,FREGD  ,FREGI  ,FSRV   , & !inout
                FSRG)

  END SUBROUTINE RADIATION

!== begin albedo ===================================================================================

  SUBROUTINE ALBEDO (parameters,VEGTYP ,IST    ,ICE    ,NSOIL  , & !in
                     DT     ,COSZ   ,FAGE   ,ELAI   ,ESAI   , & !in
                     TG     ,TV     ,SNOWH  ,FSNO   ,FWET   , & !in
                     SMC    ,SNEQVO ,SNEQV  ,QSNOW  ,FVEG   , & !in
                     ILOC   ,JLOC   ,                         & !in
                     ALBOLD ,TAUSS                          , & !inout
                     ALBGRD ,ALBGRI ,ALBD   ,ALBI   ,FABD   , & !out
                     FABI   ,FTDD   ,FTID   ,FTII   ,FSUN   , & !out
                     FREVI  ,FREVD  ,FREGD  ,FREGI  ,BGAP   , & !out
                     WGAP   ,ALBSND ,ALBSNI )

! --------------------------------------------------------------------------------------------------
! surface albedos. also fluxes (per unit incoming direct and diffuse
! radiation) reflected, transmitted, and absorbed by vegetation.
! also sunlit fraction of the canopy.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                  INTENT(IN)  :: ILOC
  INTEGER,                  INTENT(IN)  :: JLOC
  INTEGER,                  INTENT(IN)  :: NSOIL  !number of soil layers
  INTEGER,                  INTENT(IN)  :: VEGTYP !vegetation type
  INTEGER,                  INTENT(IN)  :: IST    !surface type
  INTEGER,                  INTENT(IN)  :: ICE    !ice (ice = 1)

  REAL,                     INTENT(IN)  :: DT     !time step [sec]
  REAL,                     INTENT(IN)  :: QSNOW  !snowfall
  REAL,                     INTENT(IN)  :: COSZ   !cosine solar zenith angle for next time step
  REAL,                     INTENT(IN)  :: SNOWH  !snow height (mm)
  REAL,                     INTENT(IN)  :: TG     !ground temperature (k)
  REAL,                     INTENT(IN)  :: TV     !vegetation temperature (k)
  REAL,                     INTENT(IN)  :: ELAI   !LAI, one-sided, adjusted for burying by snow
  REAL,                     INTENT(IN)  :: ESAI   !SAI, one-sided, adjusted for burying by snow
  REAL,                     INTENT(IN)  :: FSNO   !fraction of grid covered by snow
  REAL,                     INTENT(IN)  :: FWET   !fraction of canopy that is wet
  REAL,                     INTENT(IN)  :: SNEQVO !snow mass at last time step(mm)
  REAL,                     INTENT(IN)  :: SNEQV  !snow mass (mm)
  REAL,                     INTENT(IN)  :: FVEG   !green vegetation fraction [0.0-1.0]
  REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SMC    !volumetric soil water (m3/m3)

! inout
  REAL,                  INTENT(INOUT)  :: ALBOLD !snow albedo at last time step (CLASS type)
  REAL,                  INTENT(INOUT)  :: TAUSS  !non-dimensional snow age

! output
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRD !ground albedo (direct)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRI !ground albedo (diffuse)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBD   !surface albedo (direct)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBI   !surface albedo (diffuse)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FABD   !flux abs by veg (per unit direct flux)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FABI   !flux abs by veg (per unit diffuse flux)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FTDD   !down direct flux below veg (per unit dir flux)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FTID   !down diffuse flux below veg (per unit dir flux)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FTII   !down diffuse flux below veg (per unit dif flux)
  REAL,                     INTENT(OUT) :: FSUN   !sunlit fraction of canopy (-)
!jref:start
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREVD
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREVI
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREGD
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREGI
  REAL, INTENT(OUT) :: BGAP
  REAL, INTENT(OUT) :: WGAP
!jref:end

! ------------------------------------------------------------------------
! ------------------------ local variables -------------------------------
! local
  REAL                 :: FAGE     !snow age function
  REAL                 :: ALB
  INTEGER              :: IB       !indices
  INTEGER              :: NBAND    !number of solar radiation wave bands
  INTEGER              :: IC       !direct beam: ic=0; diffuse: ic=1

  REAL                 :: WL       !fraction of LAI+SAI that is LAI
  REAL                 :: WS       !fraction of LAI+SAI that is SAI
  REAL                 :: MPE      !prevents overflow for division by zero

  REAL, DIMENSION(1:2) :: RHO      !leaf/stem reflectance weighted by fraction LAI and SAI
  REAL, DIMENSION(1:2) :: TAU      !leaf/stem transmittance weighted by fraction LAI and SAI
  REAL, DIMENSION(1:2) :: FTDI     !down direct flux below veg per unit dif flux = 0
  REAL, DIMENSION(1:2), INTENT(OUT) :: ALBSND   !snow albedo (direct)
  REAL, DIMENSION(1:2), INTENT(OUT) :: ALBSNI   !snow albedo (diffuse)

  REAL                 :: VAI      !ELAI+ESAI
  REAL                 :: GDIR     !average projected leaf/stem area in solar direction
  REAL                 :: EXT      !optical depth direct beam per unit leaf + stem area

! --------------------------------------------------------------------------------------------------

  NBAND = 2
  MPE = 1.0E-06
  BGAP = 0.0
  WGAP = 0.0

! initialize output because solar radiation only done if COSZ > 0

  DO IB = 1, NBAND
    ALBD(IB) = 0.0
    ALBI(IB) = 0.0
    ALBGRD(IB) = 0.0
    ALBGRI(IB) = 0.0
    ALBSND(IB) = 0.0
    ALBSNI(IB) = 0.0
    FABD(IB) = 0.0
    FABI(IB) = 0.0
    FTDD(IB) = 0.0
    FTID(IB) = 0.0
    FTII(IB) = 0.0
    IF (IB .EQ. 1) FSUN = 0.0
  END DO

  IF(COSZ <= 0) GOTO 100

! weight reflectance/transmittance by LAI and SAI

  DO IB = 1, NBAND
    VAI = ELAI + ESAI
    WL  = ELAI / MAX(VAI,MPE)
    WS  = ESAI / MAX(VAI,MPE)
    RHO(IB) = MAX(parameters%RHOL(IB)*WL+parameters%RHOS(IB)*WS, MPE)
    TAU(IB) = MAX(parameters%TAUL(IB)*WL+parameters%TAUS(IB)*WS, MPE)
  END DO

! snow age

   CALL SNOW_AGE (parameters,DT,TG,SNEQVO,SNEQV,TAUSS,FAGE)

! snow albedos: only if COSZ > 0 and FSNO > 0

  IF(OPT_ALB == 1) &
     CALL SNOWALB_BATS (parameters,NBAND, FSNO,COSZ,FAGE,ALBSND,ALBSNI)
  IF(OPT_ALB == 2) THEN
     CALL SNOWALB_CLASS (parameters,NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI,ILOC,JLOC)
     ALBOLD = ALB
  END IF

! ground surface albedo

  CALL GROUNDALB (parameters,NSOIL   ,NBAND   ,ICE     ,IST     , & !in
                  FSNO    ,SMC     ,ALBSND  ,ALBSNI  ,COSZ    , & !in
                  TG      ,ILOC    ,JLOC    ,                   & !in
                  ALBGRD  ,ALBGRI  )                              !out

! loop over NBAND wavebands to calculate surface albedos and solar
! fluxes for unit incoming direct (IC=0) and diffuse flux (IC=1)

  DO IB = 1, NBAND
      IC = 0      ! direct
      CALL TWOSTREAM (parameters,IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & !in
                      FWET   ,TV      ,ALBGRD  ,ALBGRI  ,RHO    , & !in
                      TAU    ,FVEG    ,IST     ,ILOC    ,JLOC   , & !in
                      FABD   ,ALBD    ,FTDD    ,FTID    ,GDIR   , &!)   !out
                      FREVD  ,FREGD   ,BGAP    ,WGAP)

      IC = 1      ! diffuse
      CALL TWOSTREAM (parameters,IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & !in
                      FWET   ,TV      ,ALBGRD  ,ALBGRI  ,RHO    , & !in
                      TAU    ,FVEG    ,IST     ,ILOC    ,JLOC   , & !in
                      FABI   ,ALBI    ,FTDI    ,FTII    ,GDIR   , & !)   !out
                      FREVI  ,FREGI   ,BGAP    ,WGAP)

  END DO

! sunlit fraction of canopy. set FSUN = 0 if FSUN < 0.01.

  EXT = GDIR/COSZ * SQRT(1.0-RHO(1)-TAU(1))
  FSUN = (1.0-EXP(-EXT*VAI)) / MAX(EXT*VAI,MPE)
  EXT = FSUN

  IF (EXT .LT. 0.01) THEN
     WL = 0.0
  ELSE
     WL = EXT 
  END IF
  FSUN = WL

100 CONTINUE

  END SUBROUTINE ALBEDO

!== begin surrad ===================================================================================

  SUBROUTINE SURRAD (parameters,MPE     ,FSUN    ,FSHA    ,ELAI    ,VAI     , & !in
                     LAISUN  ,LAISHA  ,SOLAD   ,SOLAI   ,FABD    , & !in
                     FABI    ,FTDD    ,FTID    ,FTII    ,ALBGRD  , & !in
                     ALBGRI  ,ALBD    ,ALBI    ,ILOC    ,JLOC    , & !in
                     PARSUN  ,PARSHA  ,SAV     ,SAG     ,FSA     , & !out
                     FSR     , & !)                                       !out
                     FREVI   ,FREVD   ,FREGD   ,FREGI   ,FSRV    , &
                     FSRG) !inout

! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER, INTENT(IN)              :: ILOC
  INTEGER, INTENT(IN)              :: JLOC
  REAL, INTENT(IN)                 :: MPE     !prevents underflow errors if division by zero

  REAL, INTENT(IN)                 :: FSUN    !sunlit fraction of canopy
  REAL, INTENT(IN)                 :: FSHA    !shaded fraction of canopy
  REAL, INTENT(IN)                 :: ELAI    !leaf area, one-sided
  REAL, INTENT(IN)                 :: VAI     !leaf + stem area, one-sided
  REAL, INTENT(IN)                 :: LAISUN  !sunlit leaf area index, one-sided
  REAL, INTENT(IN)                 :: LAISHA  !shaded leaf area index, one-sided

  REAL, DIMENSION(1:2), INTENT(IN) :: SOLAD   !incoming direct solar radiation (w/m2)
  REAL, DIMENSION(1:2), INTENT(IN) :: SOLAI   !incoming diffuse solar radiation (w/m2)
  REAL, DIMENSION(1:2), INTENT(IN) :: FABD    !flux abs by veg (per unit incoming direct flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: FABI    !flux abs by veg (per unit incoming diffuse flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: FTDD    !down dir flux below veg (per incoming dir flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: FTID    !down dif flux below veg (per incoming dir flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: FTII    !down dif flux below veg (per incoming dif flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBGRD  !ground albedo (direct)
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBGRI  !ground albedo (diffuse)
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBD    !overall surface albedo (direct)
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBI    !overall surface albedo (diffuse)

  REAL, DIMENSION(1:2), INTENT(IN) :: FREVD    !overall surface albedo veg (direct)
  REAL, DIMENSION(1:2), INTENT(IN) :: FREVI    !overall surface albedo veg (diffuse)
  REAL, DIMENSION(1:2), INTENT(IN) :: FREGD    !overall surface albedo grd (direct)
  REAL, DIMENSION(1:2), INTENT(IN) :: FREGI    !overall surface albedo grd (diffuse)

! output

  REAL, INTENT(OUT)                :: PARSUN  !average absorbed par for sunlit leaves (w/m2)
  REAL, INTENT(OUT)                :: PARSHA  !average absorbed par for shaded leaves (w/m2)
  REAL, INTENT(OUT)                :: SAV     !solar radiation absorbed by vegetation (w/m2)
  REAL, INTENT(OUT)                :: SAG     !solar radiation absorbed by ground (w/m2)
  REAL, INTENT(OUT)                :: FSA     !total absorbed solar radiation (w/m2)
  REAL, INTENT(OUT)                :: FSR     !total reflected solar radiation (w/m2)
  REAL, INTENT(OUT)                :: FSRV    !reflected solar radiation by vegetation
  REAL, INTENT(OUT)                :: FSRG    !reflected solar radiation by ground

! ------------------------ local variables ----------------------------------------------------
  INTEGER                          :: IB      !waveband number (1=vis, 2=nir)
  INTEGER                          :: NBAND   !number of solar radiation waveband classes

  REAL                             :: ABS     !absorbed solar radiation (w/m2)
  REAL                             :: RNIR    !reflected solar radiation [nir] (w/m2)
  REAL                             :: RVIS    !reflected solar radiation [vis] (w/m2)
  REAL                             :: LAIFRA  !leaf area fraction of canopy
  REAL                             :: TRD     !transmitted solar radiation: direct (w/m2)
  REAL                             :: TRI     !transmitted solar radiation: diffuse (w/m2)
  REAL, DIMENSION(1:2)             :: CAD     !direct beam absorbed by canopy (w/m2)
  REAL, DIMENSION(1:2)             :: CAI     !diffuse radiation absorbed by canopy (w/m2)
! ---------------------------------------------------------------------------------------------
   NBAND = 2

! zero summed solar fluxes

    SAG = 0.0
    SAV = 0.0
    FSA = 0.0

! loop over nband wavebands

  DO IB = 1, NBAND

! absorbed by canopy

    CAD(IB) = SOLAD(IB)*FABD(IB)    
    CAI(IB) = SOLAI(IB)*FABI(IB)
    SAV     = SAV + CAD(IB) + CAI(IB)
    FSA     = FSA + CAD(IB) + CAI(IB)
 
! transmitted solar fluxes incident on ground

    TRD = SOLAD(IB)*FTDD(IB)
    TRI = SOLAD(IB)*FTID(IB) + SOLAI(IB)*FTII(IB)

! solar radiation absorbed by ground surface

    ABS = TRD*(1.0-ALBGRD(IB)) + TRI*(1.0-ALBGRI(IB))
    SAG = SAG + ABS
    FSA = FSA + ABS
  END DO

! partition visible canopy absorption to sunlit and shaded fractions
! to get average absorbed par for sunlit and shaded leaves

     LAIFRA = ELAI / MAX(VAI,MPE)
     IF (FSUN .GT. 0.0) THEN
        PARSUN = (CAD(1)+FSUN*CAI(1)) * LAIFRA / MAX(LAISUN,MPE)
        PARSHA = (FSHA*CAI(1))*LAIFRA / MAX(LAISHA,MPE)
     ELSE
        PARSUN = 0.0
        PARSHA = (CAD(1)+CAI(1))*LAIFRA /MAX(LAISHA,MPE)
     ENDIF

! reflected solar radiation

     RVIS = ALBD(1)*SOLAD(1) + ALBI(1)*SOLAI(1)
     RNIR = ALBD(2)*SOLAD(2) + ALBI(2)*SOLAI(2)
     FSR  = RVIS + RNIR

! reflected solar radiation of veg. and ground (combined ground)
     FSRV = FREVD(1)*SOLAD(1)+FREVI(1)*SOLAI(1)+FREVD(2)*SOLAD(2)+FREVI(2)*SOLAI(2)
     FSRG = FREGD(1)*SOLAD(1)+FREGI(1)*SOLAI(1)+FREGD(2)*SOLAD(2)+FREGI(2)*SOLAI(2)


  END SUBROUTINE SURRAD

!== begin snow_age =================================================================================

  SUBROUTINE SNOW_AGE (parameters,DT,TG,SNEQVO,SNEQV,TAUSS,FAGE)
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ------------------------ code history ------------------------------------------------------------
! from BATS
! ------------------------ input/output variables --------------------------------------------------
!input
  type (noahmp_parameters), intent(in) :: parameters
   REAL, INTENT(IN) :: DT        !main time step (s)
   REAL, INTENT(IN) :: TG        !ground temperature (k)
   REAL, INTENT(IN) :: SNEQVO    !snow mass at last time step(mm)
   REAL, INTENT(IN) :: SNEQV     !snow water per unit ground area (mm)

!output
   REAL, INTENT(OUT) :: FAGE     !snow age

!input/output
   REAL, INTENT(INOUT) :: TAUSS      !non-dimensional snow age
!local
   REAL            :: TAGE       !total aging effects
   REAL            :: AGE1       !effects of grain growth due to vapor diffusion
   REAL            :: AGE2       !effects of grain growth at freezing of melt water
   REAL            :: AGE3       !effects of soot
   REAL            :: DELA       !temporary variable
   REAL            :: SGE        !temporary variable
   REAL            :: DELS       !temporary variable
   REAL            :: DELA0      !temporary variable
   REAL            :: ARG        !temporary variable
! See Yang et al. (1997) J.of Climate for detail.
!---------------------------------------------------------------------------------------------------

   IF(SNEQV.LE.0.0) THEN
          TAUSS = 0.0
   ELSE
          DELA0 = DT/parameters%TAU0
          ARG   = parameters%GRAIN_GROWTH*(1.0/TFRZ-1.0/TG)
          AGE1  = EXP(ARG)
          AGE2  = EXP(AMIN1(0.0,parameters%EXTRA_GROWTH*ARG))
          AGE3  = parameters%DIRT_SOOT
          TAGE  = AGE1+AGE2+AGE3
          DELA  = DELA0*TAGE
          DELS  = AMAX1(0.0,SNEQV-SNEQVO) / parameters%SWEMX
          SGE   = (TAUSS+DELA)*(1.0-DELS)
          TAUSS = AMAX1(0.0,SGE)
   ENDIF

   FAGE= TAUSS/(TAUSS+1.0)

  END SUBROUTINE SNOW_AGE

!== begin snowalb_bats =============================================================================

  SUBROUTINE SNOWALB_BATS (parameters,NBAND,FSNO,COSZ,FAGE,ALBSND,ALBSNI)
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,INTENT(IN) :: NBAND  !number of waveband classes

  REAL,INTENT(IN) :: COSZ    !cosine solar zenith angle
  REAL,INTENT(IN) :: FSNO    !snow cover fraction (-)
  REAL,INTENT(IN) :: FAGE    !snow age correction

! output

  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSND !snow albedo for direct(1=vis, 2=nir)
  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSNI !snow albedo for diffuse
! ---------------------------------------------------------------------------------------------

! ------------------------ local variables ----------------------------------------------------
  INTEGER :: IB          !waveband class

  REAL :: FZEN                 !zenith angle correction
  REAL :: CF1                  !temperary variable
  REAL :: SL2                  !2.*SL
  REAL :: SL1                  !1/SL
  REAL :: SL                   !adjustable parameter
!  REAL, PARAMETER :: C1 = 0.2  !default in BATS 
!  REAL, PARAMETER :: C2 = 0.5  !default in BATS
!  REAL, PARAMETER :: C1 = 0.2 * 2. ! double the default to match Sleepers River's
!  REAL, PARAMETER :: C2 = 0.5 * 2. ! snow surface albedo (double aging effects)
! ---------------------------------------------------------------------------------------------
! zero albedos for all points
        ALBSND(1: NBAND) = 0.0
        ALBSNI(1: NBAND) = 0.0
! when cosz > 0
        SL=parameters%BATS_COSZ
        SL1=1.0/SL
        SL2=2.0*SL
        CF1=((1.0+SL1)/(1.0+SL2*COSZ)-SL1)
        FZEN=AMAX1(CF1,0.0)
        ALBSNI(1)=parameters%BATS_VIS_NEW*(1.0-parameters%BATS_VIS_AGE*FAGE)         
        ALBSNI(2)=parameters%BATS_NIR_NEW*(1.0-parameters%BATS_NIR_AGE*FAGE)        
        ALBSND(1)=ALBSNI(1)+parameters%BATS_VIS_DIR*FZEN*(1.0-ALBSNI(1))    !  vis direct
        ALBSND(2)=ALBSNI(2)+parameters%BATS_NIR_DIR*FZEN*(1.0-ALBSNI(2))    !  nir direct
  END SUBROUTINE SNOWALB_BATS
!== begin snowalb_class ============================================================================
  SUBROUTINE SNOWALB_CLASS (parameters,NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI,ILOC,JLOC)
! ----------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,INTENT(IN) :: ILOC !grid index
  INTEGER,INTENT(IN) :: JLOC !grid index
  INTEGER,INTENT(IN) :: NBAND  !number of waveband classes
  REAL,INTENT(IN) :: QSNOW     !snowfall (mm/s)
  REAL,INTENT(IN) :: DT        !time step (sec)
  REAL,INTENT(IN) :: ALBOLD    !snow albedo at last time step
! in & out
  REAL,                INTENT(INOUT) :: ALB        ! 
! output
  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSND !snow albedo for direct(1=vis, 2=nir)
  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSNI !snow albedo for diffuse
! ---------------------------------------------------------------------------------------------
! ------------------------ local variables ----------------------------------------------------
  INTEGER :: IB          !waveband class
! ---------------------------------------------------------------------------------------------
! zero albedos for all points
        ALBSND(1: NBAND) = 0.0
        ALBSNI(1: NBAND) = 0.0
! when cosz > 0
         ALB = 0.55 + (ALBOLD-0.55) * EXP(-0.01*DT/3600.0)
! 1 mm fresh snow(SWE) -- 10mm snow depth, assumed the fresh snow density 100kg/m3
! here assume 1cm snow depth will fully cover the old snow
         IF (QSNOW > 0.0) then
           ALB = ALB + MIN(QSNOW,parameters%SWEMX/DT) * (0.84-ALB)/(parameters%SWEMX/DT)
         ENDIF
         ALBSNI(1)= ALB         ! vis diffuse
         ALBSNI(2)= ALB         ! nir diffuse
         ALBSND(1)= ALB         ! vis direct
         ALBSND(2)= ALB         ! nir direct
  END SUBROUTINE SNOWALB_CLASS
!== begin groundalb ================================================================================
  SUBROUTINE GROUNDALB (parameters,NSOIL   ,NBAND   ,ICE     ,IST     , & !in
                        FSNO    ,SMC     ,ALBSND  ,ALBSNI  ,COSZ    , & !in
                        TG      ,ILOC    ,JLOC    ,                   & !in
                        ALBGRD  ,ALBGRI  )                              !out
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
!input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                  INTENT(IN)  :: ILOC   !grid index
  INTEGER,                  INTENT(IN)  :: JLOC   !grid index
  INTEGER,                  INTENT(IN)  :: NSOIL  !number of soil layers
  INTEGER,                  INTENT(IN)  :: NBAND  !number of solar radiation waveband classes
  INTEGER,                  INTENT(IN)  :: ICE    !value of ist for land ice
  INTEGER,                  INTENT(IN)  :: IST    !surface type
  REAL,                     INTENT(IN)  :: FSNO   !fraction of surface covered with snow (-)
  REAL,                     INTENT(IN)  :: TG     !ground temperature (k)
  REAL,                     INTENT(IN)  :: COSZ   !cosine solar zenith angle (0-1)
  REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SMC    !volumetric soil water content (m3/m3)
  REAL, DIMENSION(1:    2), INTENT(IN)  :: ALBSND !direct beam snow albedo (vis, nir)
  REAL, DIMENSION(1:    2), INTENT(IN)  :: ALBSNI !diffuse snow albedo (vis, nir)
!output
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRD !ground albedo (direct beam: vis, nir)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRI !ground albedo (diffuse: vis, nir)
!local 
  INTEGER                               :: IB     !waveband number (1=vis, 2=nir)
  REAL                                  :: INC    !soil water correction factor for soil albedo
  REAL                                  :: ALBSOD !soil albedo (direct)
  REAL                                  :: ALBSOI !soil albedo (diffuse)
! --------------------------------------------------------------------------------------------------
  DO IB = 1, NBAND
        INC = MAX(0.11-0.40*SMC(1), 0.0)
        IF (IST .EQ. 1)  THEN                     !soil
           ALBSOD = MIN(parameters%ALBSAT(IB)+INC,parameters%ALBDRY(IB))
           ALBSOI = ALBSOD
        ELSE IF (TG .GT. TFRZ) THEN               !unfrozen lake, wetland
           ALBSOD = 0.06/(MAX(0.01,COSZ)**1.7 + 0.15)
           ALBSOI = 0.06
        ELSE                                      !frozen lake, wetland
           ALBSOD = parameters%ALBLAK(IB)
           ALBSOI = ALBSOD
        END IF
        ALBGRD(IB) = ALBSOD*(1.0-FSNO) + ALBSND(IB)*FSNO
        ALBGRI(IB) = ALBSOI*(1.0-FSNO) + ALBSNI(IB)*FSNO
  END DO
  END SUBROUTINE GROUNDALB

!== begin twostream ================================================================================
  SUBROUTINE TWOSTREAM (parameters,IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & !in
                        FWET   ,T       ,ALBGRD  ,ALBGRI  ,RHO    , & !in
                        TAU    ,FVEG    ,IST     ,ILOC    ,JLOC   , & !in
                        FAB    ,FRE     ,FTD     ,FTI     ,GDIR   , & !)   !out
                        FREV   ,FREG    ,BGAP    ,WGAP)
! --------------------------------------------------------------------------------------------------
! use two-stream approximation of Dickinson (1983) Adv Geophysics
! 25:305-353 and Sellers (1985) Int J Remote Sensing 6:1335-1372
! to calculate fluxes absorbed by vegetation, reflected by vegetation,
! and transmitted through vegetation for unit incoming direct or diffuse
! flux given an underlying surface with known albedo.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
   INTEGER,              INTENT(IN)  :: ILOC    !grid index
   INTEGER,              INTENT(IN)  :: JLOC    !grid index
   INTEGER,              INTENT(IN)  :: IST     !surface type
   INTEGER,              INTENT(IN)  :: IB      !waveband number
   INTEGER,              INTENT(IN)  :: IC      !0=unit incoming direct; 1=unit incoming diffuse
   INTEGER,              INTENT(IN)  :: VEGTYP  !vegetation type
   REAL,                 INTENT(IN)  :: COSZ    !cosine of direct zenith angle (0-1)
   REAL,                 INTENT(IN)  :: VAI     !one-sided leaf+stem area index (m2/m2)
   REAL,                 INTENT(IN)  :: FWET    !fraction of lai, sai that is wetted (-)
   REAL,                 INTENT(IN)  :: T       !surface temperature (k)
   REAL, DIMENSION(1:2), INTENT(IN)  :: ALBGRD  !direct  albedo of underlying surface (-)
   REAL, DIMENSION(1:2), INTENT(IN)  :: ALBGRI  !diffuse albedo of underlying surface (-)
   REAL, DIMENSION(1:2), INTENT(IN)  :: RHO     !leaf+stem reflectance
   REAL, DIMENSION(1:2), INTENT(IN)  :: TAU     !leaf+stem transmittance
   REAL,                 INTENT(IN)  :: FVEG    !green vegetation fraction [0.0-1.0]
! output
   REAL, DIMENSION(1:2), INTENT(OUT) :: FAB     !flux abs by veg layer (per unit incoming flux)
   REAL, DIMENSION(1:2), INTENT(OUT) :: FRE     !flux refl above veg layer (per unit incoming flux)
   REAL, DIMENSION(1:2), INTENT(OUT) :: FTD     !down dir flux below veg layer (per unit in flux)
   REAL, DIMENSION(1:2), INTENT(OUT) :: FTI     !down dif flux below veg layer (per unit in flux)
   REAL,                 INTENT(OUT) :: GDIR    !projected leaf+stem area in solar direction
   REAL, DIMENSION(1:2), INTENT(OUT) :: FREV    !flux reflected by veg layer   (per unit incoming flux) 
   REAL, DIMENSION(1:2), INTENT(OUT) :: FREG    !flux reflected by ground (per unit incoming flux)
! local
   REAL                              :: OMEGA   !fraction of intercepted radiation that is scattered
   REAL                              :: OMEGAL  !omega for leaves
   REAL                              :: BETAI   !upscatter parameter for diffuse radiation
   REAL                              :: BETAIL  !betai for leaves
   REAL                              :: BETAD   !upscatter parameter for direct beam radiation
   REAL                              :: BETADL  !betad for leaves
   REAL                              :: EXT     !optical depth of direct beam per unit leaf area
   REAL                              :: AVMU    !average diffuse optical depth
   REAL                              :: COSZI   !0.001 <= cosz <= 1.000
   REAL                              :: ASU     !single scattering albedo
   REAL                              :: CHIL    ! -0.4 <= xl <= 0.6
   REAL                              :: TMP0,TMP1,TMP2,TMP3,TMP4,TMP5,TMP6,TMP7,TMP8,TMP9
   REAL                              :: P1,P2,P3,P4,S1,S2,U1,U2,U3
   REAL                              :: B,C,D,D1,D2,F,H,H1,H2,H3,H4,H5,H6,H7,H8,H9,H10
   REAL                              :: PHI1,PHI2,SIGMA
   REAL                              :: FTDS,FTIS,FRES
   REAL                              :: DENFVEG
   REAL                              :: VAI_SPREAD
!jref:start
   REAL                              :: FREVEG,FREBAR,FTDVEG,FTIVEG,FTDBAR,FTIBAR
   REAL                              :: THETAZ
!jref:end   
!  variables for the modified two-stream scheme
!  Niu and Yang (2004), JGR
   REAL, PARAMETER :: PAI = 3.14159265 
   REAL :: HD       !crown depth (m)
   REAL :: BB       !vertical crown radius (m)
   REAL :: THETAP   !angle conversion from SZA 
   REAL :: FA       !foliage volume density (m-1)
   REAL :: NEWVAI   !effective LSAI (-)
   REAL,INTENT(INOUT) :: BGAP     !between canopy gap fraction for beam (-)
   REAL,INTENT(INOUT) :: WGAP     !within canopy gap fraction for beam (-)
   REAL :: KOPEN    !gap fraction for diffue light (-)
   REAL :: GAP      !total gap fraction for beam ( <=1-shafac )
! -----------------------------------------------------------------
! compute within and between gaps
     VAI_SPREAD = VAI
     if(VAI == 0.0) THEN
         GAP     = 1.0
         KOPEN   = 1.0
     ELSE
         IF(OPT_RAD == 1) THEN
	   DENFVEG = -LOG(MAX(1.0-FVEG,0.01))/(PAI*parameters%RC**2)
           HD      = parameters%HVT - parameters%HVB
           BB      = 0.5 * HD           
           THETAP  = ATAN(BB/parameters%RC * TAN(ACOS(MAX(0.01,COSZ))) )
           ! BGAP    = EXP(-parameters%DEN * PAI * parameters%RC**2/COS(THETAP) )
           BGAP    = EXP(-DENFVEG * PAI * parameters%RC**2/COS(THETAP) )
           FA      = VAI/(1.33 * PAI * parameters%RC**3.0 *(BB/parameters%RC)*DENFVEG)
           NEWVAI  = HD*FA
           WGAP    = (1.0-BGAP) * EXP(-0.5*NEWVAI/COSZ)
           GAP     = MIN(1.0-FVEG, BGAP+WGAP)
           KOPEN   = 0.05
         END IF
         IF(OPT_RAD == 2) THEN
           GAP     = 0.0
           KOPEN   = 0.0
         END IF
         IF(OPT_RAD == 3) THEN
           GAP     = 1.0-FVEG
           KOPEN   = 1.0-FVEG
         END IF
     end if
! calculate two-stream parameters OMEGA, BETAD, BETAI, AVMU, GDIR, EXT.
! OMEGA, BETAD, BETAI are adjusted for snow. values for OMEGA*BETAD
! and OMEGA*BETAI are calculated and then divided by the new OMEGA
! because the product OMEGA*BETAI, OMEGA*BETAD is used in solution.
! also, the transmittances and reflectances (TAU, RHO) are linear
! weights of leaf and stem values.
     COSZI  = MAX(0.001, COSZ)
     CHIL   = MIN( MAX(parameters%XL, -0.4), 0.6)
     IF (ABS(CHIL) .LE. 0.01) CHIL = 0.01
     PHI1   = 0.5 - 0.633*CHIL - 0.330*CHIL*CHIL
     PHI2   = 0.877 * (1.0-2.0*PHI1)
     GDIR   = PHI1 + PHI2*COSZI
     EXT    = GDIR/COSZI
     AVMU   = ( 1.0 - PHI1/PHI2 * LOG((PHI1+PHI2)/PHI1) ) / PHI2
     OMEGAL = RHO(IB) + TAU(IB)
     TMP0   = GDIR + PHI2*COSZI
     TMP1   = PHI1*COSZI
     ASU    = 0.5*OMEGAL*GDIR/TMP0 * ( 1.0-TMP1/TMP0*LOG((TMP1+TMP0)/TMP1) )
     BETADL = (1.0+AVMU*EXT)/(OMEGAL*AVMU*EXT)*ASU
     BETAIL = 0.5 * ( RHO(IB)+TAU(IB) + (RHO(IB)-TAU(IB))   &
            * ((1.0+CHIL)/2.0)**2 ) / OMEGAL
! adjust omega, betad, and betai for intercepted snow
     IF (T .GT. TFRZ) THEN                                !no snow
        TMP0 = OMEGAL
        TMP1 = BETADL
        TMP2 = BETAIL
     ELSE
        TMP0 =   (1.0-FWET)*OMEGAL        + FWET*parameters%OMEGAS(IB)
        TMP1 = ( (1.0-FWET)*OMEGAL*BETADL + FWET*parameters%OMEGAS(IB)*parameters%BETADS ) / TMP0
        TMP2 = ( (1.0-FWET)*OMEGAL*BETAIL + FWET*parameters%OMEGAS(IB)*parameters%BETAIS ) / TMP0
     END IF
     OMEGA = TMP0
     BETAD = TMP1
     BETAI = TMP2
! absorbed, reflected, transmitted fluxes per unit incoming radiation
     B = 1.0 - OMEGA + OMEGA*BETAI
     C = OMEGA*BETAI
     TMP0 = AVMU*EXT
     D = TMP0 * OMEGA*BETAD
     F = TMP0 * OMEGA*(1.0-BETAD)
     TMP1 = B*B - C*C
     H = SQRT(TMP1) / AVMU
     SIGMA = TMP0*TMP0 - TMP1
     if ( ABS (SIGMA) < 1.0e-6 ) SIGMA = SIGN(1.0e-6,SIGMA)
     P1 = B + AVMU*H
     P2 = B - AVMU*H
     P3 = B + TMP0
     P4 = B - TMP0
     S1 = EXP(-H*VAI)
     S2 = EXP(-EXT*VAI)
     IF (IC .EQ. 0) THEN
        U1 = B - C/ALBGRD(IB)
        U2 = B - C*ALBGRD(IB)
        U3 = F + C*ALBGRD(IB)
     ELSE
        U1 = B - C/ALBGRI(IB)
        U2 = B - C*ALBGRI(IB)
        U3 = F + C*ALBGRI(IB)
     END IF
     TMP2 = U1 - AVMU*H
     TMP3 = U1 + AVMU*H
     D1 = P1*TMP2/S1 - P2*TMP3*S1
     TMP4 = U2 + AVMU*H
     TMP5 = U2 - AVMU*H
     D2 = TMP4/S1 - TMP5*S1
     H1 = -D*P4 - C*F
     TMP6 = D - H1*P3/SIGMA
     TMP7 = ( D - C - H1/SIGMA*(U1+TMP0) ) * S2
     H2 = ( TMP6*TMP2/S1 - P2*TMP7 ) / D1
     H3 = - ( TMP6*TMP3*S1 - P1*TMP7 ) / D1
     H4 = -F*P3 - C*D
     TMP8 = H4/SIGMA
     TMP9 = ( U3 - TMP8*(U2-TMP0) ) * S2
     H5 = - ( TMP8*TMP4/S1 + TMP9 ) / D2
     H6 = ( TMP8*TMP5*S1 + TMP9 ) / D2
     H7 = (C*TMP2) / (D1*S1)
     H8 = (-C*TMP3*S1) / D1
     H9 = TMP4 / (D2*S1)
     H10 = (-TMP5*S1) / D2
! downward direct and diffuse fluxes below vegetation
! Niu and Yang (2004), JGR.
     IF (IC .EQ. 0) THEN
        FTDS = S2                           *(1.0-GAP) + GAP
        FTIS = (H4*S2/SIGMA + H5*S1 + H6/S1)*(1.0-GAP)
     ELSE
        FTDS = 0.0
        FTIS = (H9*S1 + H10/S1)*(1.0-KOPEN) + KOPEN
     END IF
     FTD(IB) = FTDS
     FTI(IB) = FTIS
! flux reflected by the surface (veg. and ground)
     IF (IC .EQ. 0) THEN
        FRES   = (H1/SIGMA + H2 + H3)*(1.0-GAP  ) + ALBGRD(IB)*GAP        
        FREVEG = (H1/SIGMA + H2 + H3)*(1.0-GAP  ) 
        FREBAR = ALBGRD(IB)*GAP                   !jref - separate veg. and ground reflection
     ELSE
        FRES   = (H7 + H8) *(1.0-KOPEN) + ALBGRI(IB)*KOPEN        
        FREVEG = (H7 + H8) *(1.0-KOPEN) + ALBGRI(IB)*KOPEN
        FREBAR = 0                                !jref - separate veg. and ground reflection
     END IF
     FRE(IB) = FRES
     FREV(IB) = FREVEG 
     FREG(IB) = FREBAR 
! flux absorbed by vegetation
     FAB(IB) = 1.0 - FRE(IB) - (1.0-ALBGRD(IB))*FTD(IB) &
                             - (1.0-ALBGRI(IB))*FTI(IB)
  END SUBROUTINE TWOSTREAM


!== begin vege_flux ================================================================================
  SUBROUTINE VEGE_FLUX(parameters,NSNOW   ,NSOIL   ,ISNOW   ,VEGTYP  ,VEG     , & !in
                       DT      ,SAV     ,SAG     ,LWDN    ,UR      , & !in
                       UU      ,VV      ,SFCTMP  ,THAIR   ,QAIR    , & !in
                       EAIR    ,RHOAIR  ,SNOWH   ,VAI     ,GAMMAV   ,GAMMAG,  & !in
                       FWET    ,LAISUN  ,LAISHA  ,CWP     ,DZSNSO  , & !in
                       ZLVL    ,ZPD     ,Z0M     ,FVEG    , & !in
                       Z0MG    ,EMV     ,EMG     ,CANLIQ  ,FSNO,          & !in
                       CANICE  ,STC     ,DF      ,RSSUN   ,RSSHA   , & !in
                       RSURF   ,LATHEAV ,LATHEAG  ,PARSUN  ,PARSHA  ,IGS     , & !in
                       FOLN    ,CO2AIR  ,O2AIR   ,BTRAN   ,SFCPRS  , & !in
                       RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHV    ,PAHG     , & !in
                       EAH     ,TAH     ,TV      ,TG      ,CM      , & !inout
                       CH      ,DX      ,DZ8W    ,                   & !
                       TAUXV   ,TAUYV   ,IRG     ,IRC     ,SHG     , & !out
                       SHC     ,EVG     ,EVC     ,TR      ,GH      , & !out
                       T2MV    ,PSNSUN  ,PSNSHA  ,                   & !out
                       QC      ,QSFC    ,PSFC    ,                   & !in
                       Q2V     ,CAH2    ,CHLEAF  ,CHUC    )            !inout 
! --------------------------------------------------------------------------------------------------
! use newton-raphson iteration to solve for vegetation (tv) and
! ground (tg) temperatures that balance the surface energy budgets
! vegetated:
! -SAV + IRC[TV] + SHC[TV] + EVC[TV] + TR[TV] = 0
! -SAG + IRG[TG] + SHG[TG] + EVG[TG] + GH[TG] = 0
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                         INTENT(IN) :: ILOC   !grid index
  INTEGER,                         INTENT(IN) :: JLOC   !grid index
  LOGICAL,                         INTENT(IN) :: VEG    !true if vegetated surface
  INTEGER,                         INTENT(IN) :: NSNOW  !maximum no. of snow layers        
  INTEGER,                         INTENT(IN) :: NSOIL  !number of soil layers
  INTEGER,                         INTENT(IN) :: ISNOW  !actual no. of snow layers
  INTEGER,                         INTENT(IN) :: VEGTYP !vegetation physiology type
  REAL,                            INTENT(IN) :: FVEG   !greeness vegetation fraction (-)
  REAL,                            INTENT(IN) :: SAV    !solar rad absorbed by veg (w/m2)
  REAL,                            INTENT(IN) :: SAG    !solar rad absorbed by ground (w/m2)
  REAL,                            INTENT(IN) :: LWDN   !atmospheric longwave radiation (w/m2)
  REAL,                            INTENT(IN) :: UR     !wind speed at height zlvl (m/s)
  REAL,                            INTENT(IN) :: UU     !wind speed in eastward dir (m/s)
  REAL,                            INTENT(IN) :: VV     !wind speed in northward dir (m/s)
  REAL,                            INTENT(IN) :: SFCTMP !air temperature at reference height (k)
  REAL,                            INTENT(IN) :: THAIR  !potential temp at reference height (k)
  REAL,                            INTENT(IN) :: EAIR   !vapor pressure air at zlvl (pa)
  REAL,                            INTENT(IN) :: QAIR   !specific humidity at zlvl (kg/kg)
  REAL,                            INTENT(IN) :: RHOAIR !density air (kg/m**3)
  REAL,                            INTENT(IN) :: DT     !time step (s)
  REAL,                            INTENT(IN) :: FSNO     !snow fraction
  REAL,                            INTENT(IN) :: SNOWH  !actual snow depth [m]
  REAL,                            INTENT(IN) :: FWET   !wetted fraction of canopy
  REAL,                            INTENT(IN) :: CWP    !canopy wind parameter
  REAL,                            INTENT(IN) :: VAI    !total leaf area index + stem area index
  REAL,                            INTENT(IN) :: LAISUN !sunlit leaf area index, one-sided (m2/m2)
  REAL,                            INTENT(IN) :: LAISHA !shaded leaf area index, one-sided (m2/m2)
  REAL,                            INTENT(IN) :: ZLVL   !reference height (m)
  REAL,                            INTENT(IN) :: ZPD    !zero plane displacement (m)
  REAL,                            INTENT(IN) :: Z0M    !roughness length, momentum (m)
  REAL,                            INTENT(IN) :: Z0MG   !roughness length, momentum, ground (m)
  REAL,                            INTENT(IN) :: EMV    !vegetation emissivity
  REAL,                            INTENT(IN) :: EMG    !ground emissivity
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !soil/snow temperature (k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DF     !thermal conductivity of snow/soil (w/m/k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !thinkness of snow/soil layers (m)
  REAL,                            INTENT(IN) :: CANLIQ !intercepted liquid water (mm)
  REAL,                            INTENT(IN) :: CANICE !intercepted ice mass (mm)
  REAL,                            INTENT(IN) :: RSURF  !ground surface resistance (s/m)
!  REAL,                            INTENT(IN) :: GAMMA  !psychrometric constant (pa/K)
!  REAL,                            INTENT(IN) :: LATHEA !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: GAMMAV  !psychrometric constant (pa/K)
  REAL,                            INTENT(IN) :: LATHEAV !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: GAMMAG  !psychrometric constant (pa/K)
  REAL,                            INTENT(IN) :: LATHEAG !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: PARSUN !par absorbed per unit sunlit lai (w/m2)
  REAL,                            INTENT(IN) :: PARSHA !par absorbed per unit shaded lai (w/m2)
  REAL,                            INTENT(IN) :: FOLN   !foliage nitrogen (%)
  REAL,                            INTENT(IN) :: CO2AIR !atmospheric co2 concentration (pa)
  REAL,                            INTENT(IN) :: O2AIR  !atmospheric o2 concentration (pa)
  REAL,                            INTENT(IN) :: IGS    !growing season index (0=off, 1=on)
  REAL,                            INTENT(IN) :: SFCPRS !pressure (pa)
  REAL,                            INTENT(IN) :: BTRAN  !soil water transpiration factor (0 to 1)
  REAL,                            INTENT(IN) :: RHSUR  !raltive humidity in surface soil/snow air space (-)
  REAL                           , INTENT(IN) :: QC     !cloud water mixing ratio
  REAL                           , INTENT(IN) :: PSFC   !pressure at lowest model layer
  REAL                           , INTENT(IN) :: DX     !grid spacing
  REAL                           , INTENT(IN) :: Q2     !mixing ratio (kg/kg)
  REAL                           , INTENT(IN) :: DZ8W   !thickness of lowest layer
  REAL                           , INTENT(INOUT) :: QSFC   !mixing ratio at lowest model layer
  REAL, INTENT(IN)   :: PAHV  !precipitation advected heat - canopy net IN (W/m2)
  REAL, INTENT(IN)   :: PAHG  !precipitation advected heat - ground net IN (W/m2)
! input/output
  REAL,                         INTENT(INOUT) :: EAH    !canopy air vapor pressure (pa)
  REAL,                         INTENT(INOUT) :: TAH    !canopy air temperature (k)
  REAL,                         INTENT(INOUT) :: TV     !vegetation temperature (k)
  REAL,                         INTENT(INOUT) :: TG     !ground temperature (k)
  REAL,                         INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL,                         INTENT(INOUT) :: CH     !sensible heat exchange coefficient
! output
! -FSA + FIRA + FSH + (FCEV + FCTR + FGEV) + FCST + SSOIL = 0
  REAL,                           INTENT(OUT) :: TAUXV  !wind stress: e-w (n/m2)
  REAL,                           INTENT(OUT) :: TAUYV  !wind stress: n-s (n/m2)
  REAL,                           INTENT(OUT) :: IRC    !net longwave radiation (w/m2) [+= to atm]
  REAL,                           INTENT(OUT) :: SHC    !sensible heat flux (w/m2)     [+= to atm]
  REAL,                           INTENT(OUT) :: EVC    !evaporation heat flux (w/m2)  [+= to atm]
  REAL,                           INTENT(OUT) :: IRG    !net longwave radiation (w/m2) [+= to atm]
  REAL,                           INTENT(OUT) :: SHG    !sensible heat flux (w/m2)     [+= to atm]
  REAL,                           INTENT(OUT) :: EVG    !evaporation heat flux (w/m2)  [+= to atm]
  REAL,                           INTENT(OUT) :: TR     !transpiration heat flux (w/m2)[+= to atm]
  REAL,                           INTENT(OUT) :: GH     !ground heat (w/m2) [+ = to soil]
  REAL,                           INTENT(OUT) :: T2MV   !2 m height air temperature (k)
  REAL,                           INTENT(OUT) :: PSNSUN !sunlit leaf photosynthesis (umolco2/m2/s)
  REAL,                           INTENT(OUT) :: PSNSHA !shaded leaf photosynthesis (umolco2/m2/s)
  REAL,                           INTENT(OUT) :: CHLEAF !leaf exchange coefficient
  REAL,                           INTENT(OUT) :: CHUC   !under canopy exchange coefficient
  REAL,                           INTENT(OUT) :: Q2V
  REAL :: CAH    !sensible heat conductance, canopy air to ZLVL air (m/s)
  REAL :: U10V    !10 m wind speed in eastward dir (m/s) 
  REAL :: V10V    !10 m wind speed in eastward dir (m/s) 
  REAL :: WSPD
! ------------------------ local variables ----------------------------------------------------
  REAL :: CW           !water vapor exchange coefficient
  REAL :: FV           !friction velocity (m/s)
  REAL :: WSTAR        !friction velocity n vertical direction (m/s) (only for SFCDIF2)
  REAL :: Z0H          !roughness length, sensible heat (m)
  REAL :: Z0HG         !roughness length, sensible heat (m)
  REAL :: RB           !bulk leaf boundary layer resistance (s/m)
  REAL :: RAMC         !aerodynamic resistance for momentum (s/m)
  REAL :: RAHC         !aerodynamic resistance for sensible heat (s/m)
  REAL :: RAWC         !aerodynamic resistance for water vapor (s/m)
  REAL :: RAMG         !aerodynamic resistance for momentum (s/m)
  REAL :: RAHG         !aerodynamic resistance for sensible heat (s/m)
  REAL :: RAWG         !aerodynamic resistance for water vapor (s/m)
  REAL, INTENT(OUT) :: RSSUN        !sunlit leaf stomatal resistance (s/m)
  REAL, INTENT(OUT) :: RSSHA        !shaded leaf stomatal resistance (s/m)
  REAL :: MOL          !Monin-Obukhov length (m)
  REAL :: DTV          !change in tv, last iteration (k)
  REAL :: DTG          !change in tg, last iteration (k)
  REAL :: AIR,CIR      !coefficients for ir as function of ts**4
  REAL :: CSH          !coefficients for sh as function of ts
  REAL :: CEV          !coefficients for ev as function of esat[ts]
  REAL :: CGH          !coefficients for st as function of ts
  REAL :: ATR,CTR      !coefficients for tr as function of esat[ts]
  REAL :: ATA,BTA      !coefficients for tah as function of ts
  REAL :: AEA,BEA      !coefficients for eah as function of esat[ts]
  REAL :: ESTV         !saturation vapor pressure at tv (pa)
  REAL :: ESTG         !saturation vapor pressure at tg (pa)
  REAL :: DESTV        !d(es)/dt at ts (pa/k)
  REAL :: DESTG        !d(es)/dt at tg (pa/k)
  REAL :: ESATW        !es for water
  REAL :: ESATI        !es for ice
  REAL :: DSATW        !d(es)/dt at tg (pa/k) for water
  REAL :: DSATI        !d(es)/dt at tg (pa/k) for ice
  REAL :: FM           !momentum stability correction, weighted by prior iters
  REAL :: FH           !sen heat stability correction, weighted by prior iters
  REAL :: FHG          !sen heat stability correction, ground
  REAL :: HCAN         !canopy height (m) [note: hcan >= z0mg]
  REAL :: A            !temporary calculation
  REAL :: B            !temporary calculation
  REAL :: CVH          !sensible heat conductance, leaf surface to canopy air (m/s)
  REAL :: CAW          !latent heat conductance, canopy air ZLVL air (m/s)
  REAL :: CTW          !transpiration conductance, leaf to canopy air (m/s)
  REAL :: CEW          !evaporation conductance, leaf to canopy air (m/s)
  REAL :: CGW          !latent heat conductance, ground to canopy air (m/s)
  REAL :: COND         !sum of conductances (s/m)
  REAL :: UC           !wind speed at top of canopy (m/s)
  REAL :: KH           !turbulent transfer coefficient, sensible heat, (m2/s)
  REAL :: H            !temporary sensible heat flux (w/m2)
  REAL :: HG           !temporary sensible heat flux (w/m2)
  REAL :: MOZ          !Monin-Obukhov stability parameter
  REAL :: MOZG         !Monin-Obukhov stability parameter
  REAL :: MOZOLD       !Monin-Obukhov stability parameter from prior iteration
  REAL :: FM2          !Monin-Obukhov momentum adjustment at 2m
  REAL :: FH2          !Monin-Obukhov heat adjustment at 2m
  REAL :: CH2          !Surface exchange at 2m
  REAL :: THSTAR          !Surface exchange at 2m
  REAL :: THVAIR
  REAL :: THAH 
  REAL :: RAHC2        !aerodynamic resistance for sensible heat (s/m)
  REAL :: RAWC2        !aerodynamic resistance for water vapor (s/m)
  REAL, INTENT(OUT):: CAH2         !sensible heat conductance for diagnostics
  REAL :: CH2V         !exchange coefficient for 2m over vegetation. 
  REAL :: CQ2V         !exchange coefficient for 2m over vegetation. 
  REAL :: EAH2         !2m vapor pressure over canopy
  REAL :: QFX        !moisture flux
  REAL :: E1           
  REAL :: VAIE         !total leaf area index + stem area index,effective
  REAL :: LAISUNE      !sunlit leaf area index, one-sided (m2/m2),effective
  REAL :: LAISHAE      !shaded leaf area index, one-sided (m2/m2),effective
  INTEGER :: K         !index
  INTEGER :: ITER      !iteration index
!jref - NITERC test from 5 to 20  
  INTEGER, PARAMETER :: NITERC = 20   !number of iterations for surface temperature
!jref - NITERG test from 3-5
  INTEGER, PARAMETER :: NITERG = 5   !number of iterations for ground temperature
  INTEGER :: MOZSGN    !number of times MOZ changes sign
  REAL    :: MPE       !prevents overflow error if division by zero
  INTEGER :: LITER     !Last iteration
  REAL :: T, TDC       !Kelvin to degree Celsius with limit -50 to +50
  character(len=80) ::  message
  TDC(T)   = MIN( 50.0, MAX(-50.0,(T-TFRZ)) )
! ---------------------------------------------------------------------------------------------
        MPE = 1E-6
        LITER = 0
        FV = 0.1
! ---------------------------------------------------------------------------------------------
! initialization variables that do not depend on stability iteration
! ---------------------------------------------------------------------------------------------
        DTV = 0.0
        DTG = 0.0
        MOZ    = 0.0
        MOZSGN = 0
        MOZOLD = 0.0
        FH2    = 0.0
        HG     = 0.0
        H      = 0.0
        QFX    = 0.0
! limit LAI
        VAIE    = MIN(6.0,VAI   )
        LAISUNE = MIN(6.0,LAISUN)
        LAISHAE = MIN(6.0,LAISHA)
! saturation vapor pressure at ground temperature
        T = TDC(TG)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.0) THEN
           ESTG = ESATW
        ELSE
           ESTG = ESATI
        END IF
!jref - consistent surface specific humidity for sfcdif3 and sfcdif4
        QSFC = 0.622*EAIR/(PSFC-0.378*EAIR)  
! canopy height
        HCAN = parameters%HVT
        UC = UR*LOG(HCAN/Z0M)/LOG(ZLVL/Z0M)
        UC = UR*LOG((HCAN-ZPD+Z0M)/Z0M)/LOG(ZLVL/Z0M)   ! MB: add ZPD v3.7
        IF((HCAN-ZPD) <= 0.) THEN
          WRITE(message,*) "CRITICAL PROBLEM: HCAN <= ZPD"
          call wrf_message ( message )
          WRITE(message,*) 'i,j point=',ILOC, JLOC
          call wrf_message ( message )
          WRITE(message,*) 'HCAN  =',HCAN
          call wrf_message ( message )
          WRITE(message,*) 'ZPD   =',ZPD
          call wrf_message ( message )
          write (message, *) 'SNOWH =',SNOWH
          call wrf_message ( message )
          call wrf_error_fatal ( "CRITICAL PROBLEM IN MODULE_SF_NOAHMPLSM:VEGEFLUX" )
        END IF
! prepare for longwave rad.
        AIR = -EMV*(1.0+(1.0-EMV)*(1.0-EMG))*LWDN - EMV*EMG*SB*TG**4  
        CIR = (2.0-EMV*(1.0-EMG))*EMV*SB
! ---------------------------------------------------------------------------------------------
      loop1: DO ITER = 1, NITERC    !  begin stability iteration
       IF(ITER == 1) THEN
            Z0H  = Z0M  
            Z0HG = Z0MG
       ELSE
            Z0H  = Z0M    !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0M))
            Z0HG = Z0MG   !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0MG))
       END IF
! aerodyn resistances between heights zlvl and d+z0v
       IF(OPT_SFC == 1) THEN
          CALL SFCDIF1(parameters,ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !in
                       ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !in
                       MPE    ,ILOC   ,JLOC   ,                 & !in
                       MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !inout
                       CM     ,CH     ,FV     ,CH2     )          !out
       ENDIF
     
       IF(OPT_SFC == 2) THEN
          CALL SFCDIF2(parameters,ITER   ,Z0M    ,TAH    ,THAIR  ,UR     , & !in
                       ZLVL   ,ILOC   ,JLOC   ,         & !in
                       CM     ,CH     ,MOZ    ,WSTAR  ,         & !in
                       FV     )                                   !out
          ! Undo the multiplication by windspeed that SFCDIF2 
          ! applies to exchange coefficients CH and CM:
          CH = CH / UR
          CM = CM / UR
       ENDIF
       RAMC = MAX(1.0,1.0/(CM*UR))
       RAHC = MAX(1.0,1.0/(CH*UR))
       RAWC = RAHC
! aerodyn resistance between heights z0g and d+z0v, RAG, and leaf
! boundary layer resistance, RB
       
       CALL RAGRB(parameters,ITER   ,VAIE   ,RHOAIR ,HG     ,TAH    , & !in
                  ZPD    ,Z0MG   ,Z0HG   ,HCAN   ,UC     , & !in
                  Z0H    ,FV     ,CWP    ,VEGTYP ,MPE    , & !in
                  TV     ,MOZG   ,FHG    ,ILOC   ,JLOC   , & !inout
                  RAMG   ,RAHG   ,RAWG   ,RB     )           !out
! es and d(es)/dt evaluated at tv
       T = TDC(TV)
       CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
       IF (T .GT. 0.0) THEN
          ESTV  = ESATW
          DESTV = DSATW
       ELSE
          ESTV  = ESATI
          DESTV = DSATI
       END IF
! stomatal resistance
        
     IF(ITER == 1) THEN
        IF (OPT_CRS == 1) then  ! Ball-Berry
         CALL STOMATA (parameters,VEGTYP,MPE   ,PARSUN ,FOLN  ,ILOC  , JLOC , & !in       
                       TV    ,ESTV  ,EAH    ,SFCTMP,SFCPRS, & !in
                       O2AIR ,CO2AIR,IGS    ,BTRAN ,RB    , & !in
                       RSSUN ,PSNSUN)                         !out
         CALL STOMATA (parameters,VEGTYP,MPE   ,PARSHA ,FOLN  ,ILOC  , JLOC , & !in
                       TV    ,ESTV  ,EAH    ,SFCTMP,SFCPRS, & !in
                       O2AIR ,CO2AIR,IGS    ,BTRAN ,RB    , & !in
                       RSSHA ,PSNSHA)                         !out
        END IF
        IF (OPT_CRS == 2) then  ! Jarvis
         CALL  CANRES (parameters,PARSUN,TV    ,BTRAN ,EAH    ,SFCPRS, & !in
                       RSSUN ,PSNSUN,ILOC  ,JLOC   )          !out
         CALL  CANRES (parameters,PARSHA,TV    ,BTRAN ,EAH    ,SFCPRS, & !in
                       RSSHA ,PSNSHA,ILOC  ,JLOC   )          !out
        END IF
     END IF
! prepare for sensible heat flux above veg.
        CAH  = 1.0/RAHC
        CVH  = 2.0*VAIE/RB
        CGH  = 1.0/RAHG
        COND = CAH + CVH + CGH
        ATA  = (SFCTMP*CAH + TG*CGH) / COND
        BTA  = CVH/COND
        CSH  = (1.0-BTA)*RHOAIR*CPAIR*CVH
! prepare for latent heat flux above veg.
        CAW  = 1.0/RAWC
        CEW  = FWET*VAIE/RB
        CTW  = (1.0-FWET)*(LAISUNE/(RB+RSSUN) + LAISHAE/(RB+RSSHA))
        CGW  = 1.0/(RAWG+RSURF)
        COND = CAW + CEW + CTW + CGW
        AEA  = (EAIR*CAW + ESTG*CGW) / COND
        BEA  = (CEW+CTW)/COND
        CEV  = (1.0-BEA)*CEW*RHOAIR*CPAIR/GAMMAV   ! Barlage: change to vegetation v3.6
        CTR  = (1.0-BEA)*CTW*RHOAIR*CPAIR/GAMMAV
! evaluate surface fluxes with current temperature and solve for dts
        TAH = ATA + BTA*TV               ! canopy air T.
        EAH = AEA + BEA*ESTV             ! canopy air e
        IRC = FVEG*(AIR + CIR*TV**4)
        SHC = FVEG*RHOAIR*CPAIR*CVH * (  TV-TAH)
        EVC = FVEG*RHOAIR*CPAIR*CEW * (ESTV-EAH) / GAMMAV ! Barlage: change to v in v3.6
        TR  = FVEG*RHOAIR*CPAIR*CTW * (ESTV-EAH) / GAMMAV
	IF (TV > TFRZ) THEN
          EVC = MIN(CANLIQ*LATHEAV/DT,EVC)    ! Barlage: add if block for canice in v3.6
	ELSE
          EVC = MIN(CANICE*LATHEAV/DT,EVC)
	END IF
        B   = SAV-IRC-SHC-EVC-TR+PAHV                          !additional w/m2
        A   = FVEG*(4.0*CIR*TV**3 + CSH + (CEV+CTR)*DESTV) !volumetric heat capacity
        DTV = B/A
        IRC = IRC + FVEG*4.0*CIR*TV**3*DTV
        SHC = SHC + FVEG*CSH*DTV
        EVC = EVC + FVEG*CEV*DESTV*DTV
        TR  = TR  + FVEG*CTR*DESTV*DTV                               
! update vegetation surface temperature
        TV  = TV + DTV
!        TAH = ATA + BTA*TV               ! canopy air T; update here for consistency
! for computing M-O length in the next iteration
        H  = RHOAIR*CPAIR*(TAH - SFCTMP) /RAHC        
        HG = RHOAIR*CPAIR*(TG  - TAH)   /RAHG
! consistent specific humidity from canopy air vapor pressure
        QSFC = (0.622*EAH)/(SFCPRS-0.378*EAH)
        IF (LITER == 1) THEN
           exit loop1 
        ENDIF
        IF (ITER >= 5 .AND. ABS(DTV) <= 0.01 .AND. LITER == 0) THEN
           LITER = 1
        ENDIF
     END DO loop1 ! end stability iteration
! under-canopy fluxes and tg
        AIR = - EMG*(1.-EMV)*LWDN - EMG*EMV*SB*TV**4
        CIR = EMG*SB
        CSH = RHOAIR*CPAIR/RAHG
        CEV = RHOAIR*CPAIR / (GAMMAG*(RAWG+RSURF))  ! Barlage: change to ground v3.6
        CGH = 2.0*DF(ISNOW+1)/DZSNSO(ISNOW+1)
     loop2: DO ITER = 1, NITERG
        T = TDC(TG)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.0) THEN
            ESTG  = ESATW
            DESTG = DSATW
        ELSE
            ESTG  = ESATI
            DESTG = DSATI
        END IF
        IRG = CIR*TG**4 + AIR
        SHG = CSH * (TG         - TAH         )
        EVG = CEV * (ESTG*RHSUR - EAH         )
        GH  = CGH * (TG         - STC(ISNOW+1))
        B = SAG-IRG-SHG-EVG-GH+PAHG
        A = 4.0*CIR*TG**3+CSH+CEV*DESTG+CGH
        DTG = B/A
        IRG = IRG + 4.0*CIR*TG**3*DTG
        SHG = SHG + CSH*DTG
        EVG = EVG + CEV*DESTG*DTG
        GH  = GH  + CGH*DTG
        TG  = TG  + DTG
     END DO loop2
     
!     TAH = (CAH*SFCTMP + CVH*TV + CGH*TG)/(CAH + CVH + CGH)
! if snow on ground and TG > TFRZ: reset TG = TFRZ. reevaluate ground fluxes.
     IF(OPT_STC == 1 .OR. OPT_STC == 3) THEN
     IF (SNOWH > 0.05 .AND. TG > TFRZ) THEN
        IF(OPT_STC == 1) TG  = TFRZ
        IF(OPT_STC == 3) TG  = (1.0-FSNO)*TG + FSNO*TFRZ   ! MB: allow TG>0C during melt v3.7
        IRG = CIR*TG**4 - EMG*(1.0-EMV)*LWDN - EMG*EMV*SB*TV**4
        SHG = CSH * (TG         - TAH)
        EVG = CEV * (ESTG*RHSUR - EAH)
        GH  = SAG+PAHG - (IRG+SHG+EVG)
     END IF
     END IF
! wind stresses
     TAUXV = -RHOAIR*CM*UR*UU
     TAUYV = -RHOAIR*CM*UR*VV
! consistent vegetation air temperature and vapor pressure since TG is not consistent with the TAH/EAH
! calculation.
!     TAH = SFCTMP + (SHG+SHC)/(RHOAIR*CPAIR*CAH) 
!     TAH = SFCTMP + (SHG*FVEG+SHC)/(RHOAIR*CPAIR*CAH) ! ground flux need fveg
!     EAH = EAIR + (EVC+FVEG*(TR+EVG))/(RHOAIR*CAW*CPAIR/GAMMAG )
!     QFX = (QSFC-QAIR)*RHOAIR*CAW !*CPAIR/GAMMAG
! 2m temperature over vegetation ( corrected for low CQ2V values )
   IF (OPT_SFC == 1 .OR. OPT_SFC == 2) THEN
!      CAH2 = FV*1./VKC*LOG((2.+Z0H)/Z0H)
      CAH2 = FV*VKC/LOG((2.0+Z0H)/Z0H)
      CAH2 = FV*VKC/(LOG((2.0+Z0H)/Z0H)-FH2)
      CQ2V = CAH2
      IF (CAH2 .LT. 1.0E-5 ) THEN
         T2MV = TAH
!         Q2V  = (EAH*0.622/(SFCPRS - 0.378*EAH))
         Q2V  = QSFC
      ELSE
         T2MV = TAH - (SHG+SHC/FVEG)/(RHOAIR*CPAIR) * 1.0/CAH2
!         Q2V = (EAH*0.622/(SFCPRS - 0.378*EAH))- QFX/(RHOAIR*FV)* 1./VKC * LOG((2.+Z0H)/Z0H)
         Q2V = QSFC - ((EVC+TR)/FVEG+EVG)/(LATHEAV*RHOAIR) * 1.0/CQ2V
      ENDIF
   ENDIF
! update CH for output
     CH = CAH
     CHLEAF = CVH
     CHUC = 1.0/RAHG
  END SUBROUTINE VEGE_FLUX
!== begin bare_flux ================================================================================
  SUBROUTINE BARE_FLUX (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,SAG     , & !in
                        LWDN    ,UR      ,UU      ,VV      ,SFCTMP  , & !in
                        THAIR   ,QAIR    ,EAIR    ,RHOAIR  ,SNOWH   , & !in
                        DZSNSO  ,ZLVL    ,ZPD     ,Z0M     ,FSNO    , & !in
                        EMG     ,STC     ,DF      ,RSURF   ,LATHEA  , & !in
                        GAMMA   ,RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHB  , & !in
                        TGB     ,CM      ,CH      ,          & !inout
                        TAUXB   ,TAUYB   ,IRB     ,SHB     ,EVB     , & !out
                        GHB     ,T2MB    ,DX      ,DZ8W    ,IVGTYP  , & !out
                        QC      ,QSFC    ,PSFC    ,                   & !in
                        SFCPRS  ,Q2B     ,EHB2    )                     !in 
! --------------------------------------------------------------------------------------------------
! use newton-raphson iteration to solve ground (tg) temperature
! that balances the surface energy budgets for bare soil fraction.
! bare soil:
! -SAB + IRB[TG] + SHB[TG] + EVB[TG] + GHB[TG] = 0
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  integer                        , INTENT(IN) :: ILOC   !grid index
  integer                        , INTENT(IN) :: JLOC   !grid index
  INTEGER,                         INTENT(IN) :: NSNOW  !maximum no. of snow layers
  INTEGER,                         INTENT(IN) :: NSOIL  !number of soil layers
  INTEGER,                         INTENT(IN) :: ISNOW  !actual no. of snow layers
  REAL,                            INTENT(IN) :: DT     !time step (s)
  REAL,                            INTENT(IN) :: SAG    !solar radiation absorbed by ground (w/m2)
  REAL,                            INTENT(IN) :: LWDN   !atmospheric longwave radiation (w/m2)
  REAL,                            INTENT(IN) :: UR     !wind speed at height zlvl (m/s)
  REAL,                            INTENT(IN) :: UU     !wind speed in eastward dir (m/s)
  REAL,                            INTENT(IN) :: VV     !wind speed in northward dir (m/s)
  REAL,                            INTENT(IN) :: SFCTMP !air temperature at reference height (k)
  REAL,                            INTENT(IN) :: THAIR  !potential temperature at height zlvl (k)
  REAL,                            INTENT(IN) :: QAIR   !specific humidity at height zlvl (kg/kg)
  REAL,                            INTENT(IN) :: EAIR   !vapor pressure air at height (pa)
  REAL,                            INTENT(IN) :: RHOAIR !density air (kg/m3)
  REAL,                            INTENT(IN) :: SNOWH  !actual snow depth [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !thickness of snow/soil layers (m)
  REAL,                            INTENT(IN) :: ZLVL   !reference height (m)
  REAL,                            INTENT(IN) :: ZPD    !zero plane displacement (m)
  REAL,                            INTENT(IN) :: Z0M    !roughness length, momentum, ground (m)
  REAL,                            INTENT(IN) :: EMG    !ground emissivity
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !soil/snow temperature (k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DF     !thermal conductivity of snow/soil (w/m/k)
  REAL,                            INTENT(IN) :: RSURF  !ground surface resistance (s/m)
  REAL,                            INTENT(IN) :: LATHEA !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: GAMMA  !psychrometric constant (pa/k)
  REAL,                            INTENT(IN) :: RHSUR  !raltive humidity in surface soil/snow air space (-)
  REAL,                            INTENT(IN) :: FSNO     !snow fraction
!jref:start; in 
  INTEGER                        , INTENT(IN) :: IVGTYP
  REAL                           , INTENT(IN) :: QC     !cloud water mixing ratio
  REAL                           , INTENT(INOUT) :: QSFC   !mixing ratio at lowest model layer
  REAL                           , INTENT(IN) :: PSFC   !pressure at lowest model layer
  REAL                           , INTENT(IN) :: SFCPRS !pressure at lowest model layer
  REAL                           , INTENT(IN) :: DX     !horisontal grid spacing
  REAL                           , INTENT(IN) :: Q2     !mixing ratio (kg/kg)
  REAL                           , INTENT(IN) :: DZ8W   !thickness of lowest layer
!jref:end
  REAL, INTENT(IN)   :: PAHB  !precipitation advected heat - ground net IN (W/m2)
! input/output
  REAL,                         INTENT(INOUT) :: TGB    !ground temperature (k)
  REAL,                         INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL,                         INTENT(INOUT) :: CH     !sensible heat exchange coefficient
! output
! -SAB + IRB[TG] + SHB[TG] + EVB[TG] + GHB[TG] = 0
  REAL,                           INTENT(OUT) :: TAUXB  !wind stress: e-w (n/m2)
  REAL,                           INTENT(OUT) :: TAUYB  !wind stress: n-s (n/m2)
  REAL,                           INTENT(OUT) :: IRB    !net longwave rad (w/m2)   [+ to atm]
  REAL,                           INTENT(OUT) :: SHB    !sensible heat flux (w/m2) [+ to atm]
  REAL,                           INTENT(OUT) :: EVB    !latent heat flux (w/m2)   [+ to atm]
  REAL,                           INTENT(OUT) :: GHB    !ground heat flux (w/m2)  [+ to soil]
  REAL,                           INTENT(OUT) :: T2MB   !2 m height air temperature (k)
!jref:start
  REAL,                           INTENT(OUT) :: Q2B    !bare ground heat conductance
  REAL :: EHB    !bare ground heat conductance
  REAL :: U10B    !10 m wind speed in eastward dir (m/s)
  REAL :: V10B    !10 m wind speed in eastward dir (m/s)
  REAL :: WSPD
!jref:end
! local variables 
  REAL :: TAUX       !wind stress: e-w (n/m2)
  REAL :: TAUY       !wind stress: n-s (n/m2)
  REAL :: FIRA       !total net longwave rad (w/m2)      [+ to atm]
  REAL :: FSH        !total sensible heat flux (w/m2)    [+ to atm]
  REAL :: FGEV       !ground evaporation heat flux (w/m2)[+ to atm]
  REAL :: SSOIL      !soil heat flux (w/m2)             [+ to soil]
  REAL :: FIRE       !emitted ir (w/m2)
  REAL :: TRAD       !radiative temperature (k)
  REAL :: TAH        !"surface" temperature at height z0h+zpd (k)
  REAL :: CW         !water vapor exchange coefficient
  REAL :: FV         !friction velocity (m/s)
  REAL :: WSTAR      !friction velocity n vertical direction (m/s) (only for SFCDIF2)
  REAL :: Z0H        !roughness length, sensible heat, ground (m)
  REAL :: RB         !bulk leaf boundary layer resistance (s/m)
  REAL :: RAMB       !aerodynamic resistance for momentum (s/m)
  REAL :: RAHB       !aerodynamic resistance for sensible heat (s/m)
  REAL :: RAWB       !aerodynamic resistance for water vapor (s/m)
  REAL :: MOL        !Monin-Obukhov length (m)
  REAL :: DTG        !change in tg, last iteration (k)
  REAL :: CIR        !coefficients for ir as function of ts**4
  REAL :: CSH        !coefficients for sh as function of ts
  REAL :: CEV        !coefficients for ev as function of esat[ts]
  REAL :: CGH        !coefficients for st as function of ts
!jref:start
  REAL :: RAHB2      !aerodynamic resistance for sensible heat 2m (s/m)
  REAL :: RAWB2      !aerodynamic resistance for water vapor 2m (s/m)
  REAL,INTENT(OUT) :: EHB2       !sensible heat conductance for diagnostics
  REAL :: CH2B       !exchange coefficient for 2m temp.
  REAL :: CQ2B       !exchange coefficient for 2m temp.
  REAL :: THVAIR     !virtual potential air temp
  REAL :: THGH       !potential ground temp
  REAL :: EMB        !momentum conductance
  REAL :: QFX        !moisture flux
  REAL :: ESTG2      !saturation vapor pressure at 2m (pa)
  INTEGER :: VEGTYP     !vegetation type set to isbarren
  REAL :: E1
!jref:end
  REAL :: ESTG       !saturation vapor pressure at tg (pa)
  REAL :: DESTG      !d(es)/dt at tg (pa/K)
  REAL :: ESATW      !es for water
  REAL :: ESATI      !es for ice
  REAL :: DSATW      !d(es)/dt at tg (pa/K) for water
  REAL :: DSATI      !d(es)/dt at tg (pa/K) for ice
  REAL :: A          !temporary calculation
  REAL :: B          !temporary calculation
  REAL :: H          !temporary sensible heat flux (w/m2)
  REAL :: MOZ        !Monin-Obukhov stability parameter
  REAL :: MOZOLD     !Monin-Obukhov stability parameter from prior iteration
  REAL :: FM         !momentum stability correction, weighted by prior iters
  REAL :: FH         !sen heat stability correction, weighted by prior iters
  INTEGER :: MOZSGN  !number of times MOZ changes sign
  REAL :: FM2          !Monin-Obukhov momentum adjustment at 2m
  REAL :: FH2          !Monin-Obukhov heat adjustment at 2m
  REAL :: CH2          !Surface exchange at 2m
  INTEGER :: ITER    !iteration index
  INTEGER :: NITERB  !number of iterations for surface temperature
  REAL    :: MPE     !prevents overflow error if division by zero
!jref:start
!  DATA NITERB /3/
  DATA NITERB /5/
  SAVE NITERB
  REAL :: T, TDC     !Kelvin to degree Celsius with limit -50 to +50
  TDC(T)   = MIN( 50.0, MAX(-50.0,(T-TFRZ)) )
! -----------------------------------------------------------------
! initialization variables that do not depend on stability iteration
! -----------------------------------------------------------------
        MPE = 1E-6
        DTG = 0.0
        MOZ    = 0.0
        MOZSGN = 0
        MOZOLD = 0.0
        FH2    = 0.0
        H      = 0.0
        QFX    = 0.0
        FV     = 0.1
        CIR = EMG*SB
        CGH = 2.0*DF(ISNOW+1)/DZSNSO(ISNOW+1)
! -----------------------------------------------------------------
      loop3: DO ITER = 1, NITERB  ! begin stability iteration
        IF(ITER == 1) THEN
            Z0H = Z0M 
        ELSE
            Z0H = Z0M !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0M))
        END IF
        IF(OPT_SFC == 1) THEN
          CALL SFCDIF1(parameters,ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !in
                       ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !in
                       MPE    ,ILOC   ,JLOC   ,                 & !in
                       MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !inout
                       CM     ,CH     ,FV     ,CH2     )          !out
        ENDIF
        IF(OPT_SFC == 2) THEN
          CALL SFCDIF2(parameters,ITER   ,Z0M    ,TGB    ,THAIR  ,UR     , & !in
                       ZLVL   ,ILOC   ,JLOC   ,         & !in
                       CM     ,CH     ,MOZ    ,WSTAR  ,         & !in
                       FV     )                                   !out
          ! Undo the multiplication by windspeed that SFCDIF2 
          ! applies to exchange coefficients CH and CM:
          CH = CH / UR
          CM = CM / UR
          IF(SNOWH > 0.0) THEN
             CM = MIN(0.01,CM)   ! CM & CH are too large, causing
             CH = MIN(0.01,CH)   ! computational instability
          END IF
        ENDIF
        RAMB = MAX(1.0,1.0/(CM*UR))
        RAHB = MAX(1.0,1.0/(CH*UR))
        RAWB = RAHB
!jref - variables for diagnostics         
        EMB = 1.0/RAMB
        EHB = 1.0/RAHB
! es and d(es)/dt evaluated at tg
        T = TDC(TGB)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.0) THEN
            ESTG  = ESATW
            DESTG = DSATW
        ELSE
            ESTG  = ESATI
            DESTG = DSATI
        END IF
        CSH = RHOAIR*CPAIR/RAHB
        CEV = RHOAIR*CPAIR/GAMMA/(RSURF+RAWB)
! surface fluxes and dtg
        IRB   = CIR * TGB**4 - EMG*LWDN
        SHB   = CSH * (TGB        - SFCTMP      )
        EVB   = CEV * (ESTG*RHSUR - EAIR        )
        GHB   = CGH * (TGB        - STC(ISNOW+1))
        B     = SAG-IRB-SHB-EVB-GHB+PAHB
        A     = 4.0*CIR*TGB**3 + CSH + CEV*DESTG + CGH
        DTG   = B/A
        IRB = IRB + 4.0*CIR*TGB**3*DTG
        SHB = SHB + CSH*DTG
        EVB = EVB + CEV*DESTG*DTG
        GHB = GHB + CGH*DTG
! update ground surface temperature
        TGB = TGB + DTG
! for M-O length
        H = CSH * (TGB - SFCTMP)
        T = TDC(TGB)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.0) THEN
            ESTG  = ESATW
        ELSE
            ESTG  = ESATI
        END IF
        QSFC = 0.622*(ESTG*RHSUR)/(PSFC-0.378*(ESTG*RHSUR))
        QFX = (QSFC-QAIR)*CEV*GAMMA/CPAIR
     END DO loop3 ! end stability iteration
! -----------------------------------------------------------------
! if snow on ground and TG > TFRZ: reset TG = TFRZ. reevaluate ground fluxes.
     IF(OPT_STC == 1 .OR. OPT_STC == 3) THEN
     IF (SNOWH > 0.05 .AND. TGB > TFRZ) THEN
          IF(OPT_STC == 1) TGB = TFRZ
          IF(OPT_STC == 3) TGB  = (1.0-FSNO)*TGB + FSNO*TFRZ  ! MB: allow TG>0C during melt v3.7
          IRB = CIR * TGB**4 - EMG*LWDN
          SHB = CSH * (TGB        - SFCTMP)
          EVB = CEV * (ESTG*RHSUR - EAIR )          !ESTG reevaluate ?
          GHB = SAG+PAHB - (IRB+SHB+EVB)
     END IF
     END IF
! wind stresses
         
     TAUXB = -RHOAIR*CM*UR*UU
     TAUYB = -RHOAIR*CM*UR*VV
!jref:start; errors in original equation corrected.
! 2m air temperature
     IF(OPT_SFC == 1 .OR. OPT_SFC ==2) THEN
       EHB2  = FV*VKC/LOG((2.0+Z0H)/Z0H)
       EHB2  = FV*VKC/(LOG((2.0+Z0H)/Z0H)-FH2)
       CQ2B  = EHB2
       IF (EHB2 .lt. 1.0E-5 ) THEN
         T2MB  = TGB
         Q2B   = QSFC
       ELSE
         T2MB  = TGB - SHB/(RHOAIR*CPAIR) * 1.0/EHB2
         Q2B   = QSFC - EVB/(LATHEA*RHOAIR)*(1.0/CQ2B + RSURF)
       ENDIF
       IF (parameters%urban_flag) Q2B = QSFC
     END IF
! update CH 
     CH = EHB
  END SUBROUTINE BARE_FLUX


!== begin ragrb ====================================================================================
  SUBROUTINE RAGRB(parameters,ITER   ,VAI    ,RHOAIR ,HG     ,TAH    , & !in
                   ZPD    ,Z0MG   ,Z0HG   ,HCAN   ,UC     , & !in
                   Z0H    ,FV     ,CWP    ,VEGTYP ,MPE    , & !in
                   TV     ,MOZG   ,FHG    ,ILOC   ,JLOC   , & !inout
                   RAMG   ,RAHG   ,RAWG   ,RB     )           !out
! --------------------------------------------------------------------------------------------------
! compute under-canopy aerodynamic resistance RAG and leaf boundary layer
! resistance RB
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,              INTENT(IN) :: ILOC   !grid index
  INTEGER,              INTENT(IN) :: JLOC   !grid index
  INTEGER,              INTENT(IN) :: ITER   !iteration index
  INTEGER,              INTENT(IN) :: VEGTYP !vegetation physiology type
  REAL,                 INTENT(IN) :: VAI    !total LAI + stem area index, one sided
  REAL,                 INTENT(IN) :: RHOAIR !density air (kg/m3)
  REAL,                 INTENT(IN) :: HG     !ground sensible heat flux (w/m2)
  REAL,                 INTENT(IN) :: TV     !vegetation temperature (k)
  REAL,                 INTENT(IN) :: TAH    !air temperature at height z0h+zpd (k)
  REAL,                 INTENT(IN) :: ZPD    !zero plane displacement (m)
  REAL,                 INTENT(IN) :: Z0MG   !roughness length, momentum, ground (m)
  REAL,                 INTENT(IN) :: HCAN   !canopy height (m) [note: hcan >= z0mg]
  REAL,                 INTENT(IN) :: UC     !wind speed at top of canopy (m/s)
  REAL,                 INTENT(IN) :: Z0H    !roughness length, sensible heat (m)
  REAL,                 INTENT(IN) :: Z0HG   !roughness length, sensible heat, ground (m)
  REAL,                 INTENT(IN) :: FV     !friction velocity (m/s)
  REAL,                 INTENT(IN) :: CWP    !canopy wind parameter
  REAL,                 INTENT(IN) :: MPE    !prevents overflow error if division by zero
! in & out
  REAL,              INTENT(INOUT) :: MOZG   !Monin-Obukhov stability parameter
  REAL,              INTENT(INOUT) :: FHG    !stability correction
! outputs
  REAL                             :: RAMG   !aerodynamic resistance for momentum (s/m)
  REAL                             :: RAHG   !aerodynamic resistance for sensible heat (s/m)
  REAL                             :: RAWG   !aerodynamic resistance for water vapor (s/m)
  REAL                             :: RB     !bulk leaf boundary layer resistance (s/m)
  REAL :: KH           !turbulent transfer coefficient, sensible heat, (m2/s)
  REAL :: TMP1         !temporary calculation
  REAL :: TMP2         !temporary calculation
  REAL :: TMPRAH2      !temporary calculation for aerodynamic resistances
  REAL :: TMPRB        !temporary calculation for rb
  real :: MOLG,FHGNEW,CWPC
! --------------------------------------------------------------------------------------------------
! stability correction to below canopy resistance
       MOZG = 0.0
       MOLG = 0.0
       IF(ITER > 1) THEN
        TMP1 = VKC * (GRAV/TAH) * HG/(RHOAIR*CPAIR)
        IF (ABS(TMP1) .LE. MPE) TMP1 = MPE
        MOLG = -1.0 * FV**3 / TMP1
        MOZG = MIN( (ZPD-Z0MG)/MOLG, 1.0)
       END IF
       IF (MOZG < 0.0) THEN
          FHGNEW  = (1.0 - 15.0*MOZG)**(-0.25)
       ELSE
          FHGNEW  = 1.0+ 4.7*MOZG
       ENDIF
       IF (ITER == 1) THEN
          FHG = FHGNEW
       ELSE
          FHG = 0.5 * (FHG+FHGNEW)
       ENDIF
       CWPC = (CWP * VAI * HCAN * FHG)**0.5
!       CWPC = (CWP*FHG)**0.5
       TMP1 = EXP( -CWPC*Z0HG/HCAN )
       TMP2 = EXP( -CWPC*(Z0H+ZPD)/HCAN )
       TMPRAH2 = HCAN*EXP(CWPC) / CWPC * (TMP1-TMP2)
! aerodynamic resistances raw and rah between heights zpd+z0h and z0hg.
       KH  = MAX ( VKC*FV*(HCAN-ZPD), MPE )
       RAMG = 0.0
       RAHG = TMPRAH2 / KH
       RAWG = RAHG
! leaf boundary layer resistance
       TMPRB  = CWPC*50.0 / (1.0 - EXP(-CWPC/2.0))
       RB     = TMPRB * SQRT(parameters%DLEAF/UC)
       RB     = MIN(MAX(RB, 5.0),50.0) ! limit RB to 5-50, typically RB<50
  END SUBROUTINE RAGRB
!== begin sfcdif1 ==================================================================================
  SUBROUTINE SFCDIF1(parameters,ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !in
       &             ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !in
       &             MPE    ,ILOC   ,JLOC   ,                 & !in
       &             MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !inout
       &             CM     ,CH     ,FV     ,CH2     )          !out
! -------------------------------------------------------------------------------------------------
! computing surface drag coefficient CM for momentum and CH for heat
! -------------------------------------------------------------------------------------------------
    IMPLICIT NONE
! -------------------------------------------------------------------------------------------------
! inputs
    
  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,              INTENT(IN) :: ILOC   !grid index
    INTEGER,              INTENT(IN) :: JLOC   !grid index
    INTEGER,              INTENT(IN) :: ITER   !iteration index
    REAL,                 INTENT(IN) :: SFCTMP !temperature at reference height (k)
    REAL,                 INTENT(IN) :: RHOAIR !density air (kg/m**3)
    REAL,                 INTENT(IN) :: H      !sensible heat flux (w/m2) [+ to atm]
    REAL,                 INTENT(IN) :: QAIR   !specific humidity at reference height (kg/kg)
    REAL,                 INTENT(IN) :: ZLVL   !reference height  (m)
    REAL,                 INTENT(IN) :: ZPD    !zero plane displacement (m)
    REAL,                 INTENT(IN) :: Z0H    !roughness length, sensible heat, ground (m)
    REAL,                 INTENT(IN) :: Z0M    !roughness length, momentum, ground (m)
    REAL,                 INTENT(IN) :: UR     !wind speed (m/s)
    REAL,                 INTENT(IN) :: MPE    !prevents overflow error if division by zero
! in & out
    INTEGER,           INTENT(INOUT) :: MOZSGN !number of times moz changes sign
    REAL,              INTENT(INOUT) :: MOZ    !Monin-Obukhov stability (z/L)
    REAL,              INTENT(INOUT) :: FM     !momentum stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FH     !sen heat stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FM2    !sen heat stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FH2    !sen heat stability correction, weighted by prior iters
! outputs
    REAL,                INTENT(OUT) :: CM     !drag coefficient for momentum
    REAL,                INTENT(OUT) :: CH     !drag coefficient for heat
    REAL,                INTENT(OUT) :: FV     !friction velocity (m/s)
    REAL,                INTENT(OUT) :: CH2    !drag coefficient for heat
! locals
    REAL    :: MOL                      !Monin-Obukhov length (m)
    REAL    :: TMPCM                    !temporary calculation for CM
    REAL    :: TMPCH                    !temporary calculation for CH
    REAL    :: FMNEW                    !stability correction factor, momentum, for current moz
    REAL    :: FHNEW                    !stability correction factor, sen heat, for current moz
    REAL    :: MOZOLD                   !Monin-Obukhov stability parameter from prior iteration
    REAL    :: TMP1,TMP2,TMP3,TMP4,TMP5 !temporary calculation
    REAL    :: TVIR                     !temporary virtual temperature (k)
    REAL    :: MOZ2                     !2/L
    REAL    :: TMPCM2                   !temporary calculation for CM2
    REAL    :: TMPCH2                   !temporary calculation for CH2
    REAL    :: FM2NEW                   !stability correction factor, momentum, for current moz
    REAL    :: FH2NEW                   !stability correction factor, sen heat, for current moz
    REAL    :: TMP12,TMP22,TMP32        !temporary calculation
    REAL    :: CMFM, CHFH, CM2FM2, CH2FH2
! -------------------------------------------------------------------------------------------------
! Monin-Obukhov stability parameter moz for next iteration
    MOZOLD = MOZ
  
    IF(ZLVL <= ZPD) THEN
       write(*,*) 'WARNING: critical problem: ZLVL <= ZPD; model stops'
       call wrf_error_fatal("STOP in Noah-MP")
    ENDIF
    TMPCM = LOG((ZLVL-ZPD) / Z0M)
    TMPCH = LOG((ZLVL-ZPD) / Z0H)
    TMPCM2 = LOG((2.0 + Z0M) / Z0M)
    TMPCH2 = LOG((2.0 + Z0H) / Z0H)
    IF(ITER == 1) THEN
       FV   = 0.0
       MOZ  = 0.0
       MOL  = 0.0
       MOZ2 = 0.0
    ELSE
       TVIR = (1.0 + 0.61*QAIR) * SFCTMP
       TMP1 = VKC * (GRAV/TVIR) * H/(RHOAIR*CPAIR)
       IF (ABS(TMP1) .LE. MPE) TMP1 = MPE
       MOL  = -1.0 * FV**3 / TMP1
       MOZ  = MIN( (ZLVL-ZPD)/MOL, 1.0)
       MOZ2  = MIN( (2.0 + Z0H)/MOL, 1.0)
    ENDIF
! accumulate number of times moz changes sign.
    IF (MOZOLD*MOZ .LT. 0.0) MOZSGN = MOZSGN+1
    IF (MOZSGN .GE. 2) THEN
       MOZ = 0.0
       FM = 0.0
       FH = 0.0
       MOZ2 = 0.0
       FM2 = 0.0
       FH2 = 0.0
    ENDIF
! evaluate stability-dependent variables using moz from prior iteration
    IF (MOZ .LT. 0.0) THEN
       TMP1 = (1.0 - 16.0*MOZ)**0.25
       TMP2 = LOG((1.0+TMP1*TMP1)/2.0)
       TMP3 = LOG((1.0+TMP1)/2.0)
       FMNEW = 2.0*TMP3 + TMP2 - 2.0*ATAN(TMP1) + 1.5707963
       FHNEW = 2*TMP2
! 2-meter
       TMP12 = (1.0 - 16.0*MOZ2)**0.25
       TMP22 = LOG((1.0+TMP12*TMP12)/2.0)
       TMP32 = LOG((1.0+TMP12)/2.0)
       FM2NEW = 2.0*TMP32 + TMP22 - 2.0*ATAN(TMP12) + 1.5707963
       FH2NEW = 2*TMP22
    ELSE
       FMNEW = -5.0*MOZ
       FHNEW = FMNEW
       FM2NEW = -5.0*MOZ2
       FH2NEW = FM2NEW
    ENDIF
! except for first iteration, weight stability factors for previous
! iteration to help avoid flip-flops from one iteration to the next
    IF (ITER == 1) THEN
       FM = FMNEW
       FH = FHNEW
       FM2 = FM2NEW
       FH2 = FH2NEW
    ELSE
       FM = 0.5 * (FM+FMNEW)
       FH = 0.5 * (FH+FHNEW)
       FM2 = 0.5 * (FM2+FM2NEW)
       FH2 = 0.5 * (FH2+FH2NEW)
    ENDIF
! exchange coefficients
    FH = MIN(FH,0.9*TMPCH)
    FM = MIN(FM,0.9*TMPCM)
    FH2 = MIN(FH2,0.9*TMPCH2)
    FM2 = MIN(FM2,0.9*TMPCM2)
    CMFM = TMPCM-FM
    CHFH = TMPCH-FH
    CM2FM2 = TMPCM2-FM2
    CH2FH2 = TMPCH2-FH2
    IF(ABS(CMFM) <= MPE) CMFM = MPE
    IF(ABS(CHFH) <= MPE) CHFH = MPE
    IF(ABS(CM2FM2) <= MPE) CM2FM2 = MPE
    IF(ABS(CH2FH2) <= MPE) CH2FH2 = MPE
    CM  = VKC*VKC/(CMFM*CMFM)
    CH  = VKC*VKC/(CMFM*CHFH)
    CH2  = VKC*VKC/(CM2FM2*CH2FH2)
        
! friction velocity
    FV = UR * SQRT(CM)
    CH2  = VKC*FV/CH2FH2
  END SUBROUTINE SFCDIF1
!== begin sfcdif2 ==================================================================================
  SUBROUTINE SFCDIF2(parameters,ITER   ,Z0     ,THZ0   ,THLM   ,SFCSPD , & !in
                     ZLM    ,ILOC   ,JLOC   ,         & !in
                     AKMS   ,AKHS   ,RLMO   ,WSTAR2 ,         & !in
                     USTAR  )                                   !out
! -------------------------------------------------------------------------------------------------
! SUBROUTINE SFCDIF (renamed SFCDIF_off to avoid clash with Eta PBL)
! -------------------------------------------------------------------------------------------------
! CALCULATE SURFACE LAYER EXCHANGE COEFFICIENTS VIA ITERATIVE PROCESS.
! SEE CHEN ET AL (1997, BLM)
! -------------------------------------------------------------------------------------------------
    IMPLICIT NONE
  type (noahmp_parameters), intent(in) :: parameters
    INTEGER, INTENT(IN) :: ILOC
    INTEGER, INTENT(IN) :: JLOC
    INTEGER, INTENT(IN) :: ITER
    REAL,    INTENT(IN) :: ZLM, Z0, THZ0, THLM, SFCSPD
    REAL, intent(INOUT) :: AKMS
    REAL, intent(INOUT) :: AKHS
    REAL, intent(INOUT) :: RLMO
    REAL, intent(INOUT) :: WSTAR2
    REAL,   intent(OUT) :: USTAR
    REAL     ZZ, PSLMU, PSLMS, PSLHU, PSLHS
    REAL     XX, PSPMU, YY, PSPMS, PSPHU, PSPHS
    REAL     ZILFC, ZU, ZT, RDZ, CXCH
    REAL     DTHV, DU2, BTGH, ZSLU, ZSLT, RLOGU, RLOGT
    REAL     ZETALT, ZETALU, ZETAU, ZETAT, XLU4, XLT4, XU4, XT4
    REAL     XLU, XLT, XU, XT, PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN,  &
         &         RLMA
    INTEGER  ILECH, ITR
    INTEGER, PARAMETER :: ITRMX  = 5
    REAL,    PARAMETER :: WWST   = 1.2
    REAL,    PARAMETER :: WWST2  = WWST * WWST
    REAL,    PARAMETER :: VKRM   = 0.40
    REAL,    PARAMETER :: EXCM   = 0.001
    REAL,    PARAMETER :: BETA   = 1.0 / 270.0
    REAL,    PARAMETER :: BTG    = BETA * GRAV
    REAL,    PARAMETER :: ELFC   = VKRM * BTG
    REAL,    PARAMETER :: WOLD   = 0.15
    REAL,    PARAMETER :: WNEW   = 1.0 - WOLD
    REAL,    PARAMETER :: PIHF   = 3.14159265 / 2.0
    REAL,    PARAMETER :: EPSU2  = 1.E-4
    REAL,    PARAMETER :: EPSUST = 0.07
    REAL,    PARAMETER :: EPSIT  = 1.E-4
    REAL,    PARAMETER :: EPSA   = 1.E-8
    REAL,    PARAMETER :: ZTMIN  = -5.0
    REAL,    PARAMETER :: ZTMAX  = 1.0
    REAL,    PARAMETER :: HPBL   = 1000.0
    REAL,    PARAMETER :: SQVISC = 258.2
    REAL,    PARAMETER :: RIC    = 0.183
    REAL,    PARAMETER :: RRIC   = 1.0 / RIC
    REAL,    PARAMETER :: FHNEU  = 0.8
    REAL,    PARAMETER :: RFC    = 0.191
    REAL,    PARAMETER :: RFAC   = RIC / ( FHNEU * RFC * RFC )
! ----------------------------------------------------------------------
! NOTE: THE TWO CODE BLOCKS BELOW DEFINE FUNCTIONS
! ----------------------------------------------------------------------
! LECH'S SURFACE FUNCTIONS
    PSLMU (ZZ)= -0.96* log (1.0-4.5* ZZ)
    PSLMS (ZZ)= ZZ * RRIC -2.076* (1.0 -1.0/ (ZZ +1.0))
    PSLHU (ZZ)= -0.96* log (1.0-4.5* ZZ)
    PSLHS (ZZ)= ZZ * RFAC -2.076* (1.0 -1.0/ (ZZ +1.0))
! PAULSON'S SURFACE FUNCTIONS
    PSPMU (XX)= -2.0* log ( (XX +1.)*0.5) - log ( (XX * XX +1.0)*0.5)   &
         &        +2.0* ATAN (XX)                                            &
         &- PIHF
    PSPMS (YY)= 5.0* YY
    PSPHU (XX)= -2.0* log ( (XX * XX +1.0)*0.5)
    PSPHS (YY)= 5.0* YY
! THIS ROUTINE SFCDIF CAN HANDLE BOTH OVER OPEN WATER (SEA, OCEAN) AND
! OVER SOLID SURFACE (LAND, SEA-ICE).
! ----------------------------------------------------------------------
!     ZTFC: RATIO OF ZOH/ZOM  LESS OR EQUAL THAN 1
!     C......ZTFC=0.1
!     CZIL: CONSTANT C IN Zilitinkevich, S. S.1995,:NOTE ABOUT ZT
! ----------------------------------------------------------------------
    ILECH = 0
! ----------------------------------------------------------------------
    ZILFC = - parameters%CZIL * VKRM * SQVISC
    ZU = Z0
    RDZ = 1.0/ ZLM
    CXCH = EXCM * RDZ
    DTHV = THLM - THZ0
! BELJARS CORRECTION OF USTAR
    DU2 = MAX (SFCSPD * SFCSPD,EPSU2)
    BTGH = BTG * HPBL
    IF(ITER == 1) THEN
        IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
           WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2.0/3.0)
        ELSE
           WSTAR2 = 0.0
        END IF
        USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)
        RLMO = ELFC * AKHS * DTHV / USTAR **3
    END IF
 
! ZILITINKEVITCH APPROACH FOR ZT
    ZT = MAX(1.0E-6,EXP (ZILFC * SQRT (USTAR * Z0))* Z0)
    ZSLU = ZLM + ZU
    ZSLT = ZLM + ZT
    RLOGU = log (ZSLU / ZU)
    RLOGT = log (ZSLT / ZT)
! ----------------------------------------------------------------------
! 1./MONIN-OBUKKHOV LENGTH-SCALE
! ----------------------------------------------------------------------
    ZETALT = MAX (ZSLT * RLMO,ZTMIN)
    RLMO = ZETALT / ZSLT
    ZETALU = ZSLU * RLMO
    ZETAU = ZU * RLMO
    ZETAT = ZT * RLMO
    IF (ILECH .eq. 0) THEN
       IF (RLMO .lt. 0.0)THEN
          XLU4 = 1.0 -16.0* ZETALU
          XLT4 = 1.0 -16.0* ZETALT
          XU4  = 1.0 -16.0* ZETAU
          XT4  = 1.0 -16.0* ZETAT
          XLU  = SQRT (SQRT (XLU4))
          XLT  = SQRT (SQRT (XLT4))
          XU   = SQRT (SQRT (XU4))
          XT = SQRT (SQRT (XT4))
          PSMZ = PSPMU (XU)
          SIMM = PSPMU (XLU) - PSMZ + RLOGU
          PSHZ = PSPHU (XT)
          SIMH = PSPHU (XLT) - PSHZ + RLOGT
       ELSE
          ZETALU = MIN (ZETALU,ZTMAX)
          ZETALT = MIN (ZETALT,ZTMAX)
          ZETAU  = MIN (ZETAU,ZTMAX/(ZSLU/ZU))   ! Barlage: add limit on ZETAU/ZETAT
          ZETAT  = MIN (ZETAT,ZTMAX/(ZSLT/ZT))   ! Barlage: prevent SIMM/SIMH < 0
          PSMZ = PSPMS (ZETAU)
          SIMM = PSPMS (ZETALU) - PSMZ + RLOGU
          PSHZ = PSPHS (ZETAT)
          SIMH = PSPHS (ZETALT) - PSHZ + RLOGT
       END IF
! ----------------------------------------------------------------------
! LECH'S FUNCTIONS
! ----------------------------------------------------------------------
    ELSE
       IF (RLMO .lt. 0.0)THEN
          PSMZ = PSLMU (ZETAU)
          SIMM = PSLMU (ZETALU) - PSMZ + RLOGU
          PSHZ = PSLHU (ZETAT)
          SIMH = PSLHU (ZETALT) - PSHZ + RLOGT
       ELSE
          ZETALU = MIN (ZETALU,ZTMAX)
          ZETALT = MIN (ZETALT,ZTMAX)
          PSMZ = PSLMS (ZETAU)
          SIMM = PSLMS (ZETALU) - PSMZ + RLOGU
          PSHZ = PSLHS (ZETAT)
          SIMH = PSLHS (ZETALT) - PSHZ + RLOGT
       END IF
! ----------------------------------------------------------------------
       END IF

! ----------------------------------------------------------------------
! BELJAARS CORRECTION FOR USTAR
! ----------------------------------------------------------------------
       USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)

! ZILITINKEVITCH FIX FOR ZT
       ZT = MAX(1.0E-6,EXP (ZILFC * SQRT (USTAR * Z0))* Z0)
       ZSLT = ZLM + ZT
!-----------------------------------------------------------------------
       RLOGT = log (ZSLT / ZT)
       USTARK = USTAR * VKRM
       IF(SIMM < 1.0e-6) SIMM = 1.0e-6        ! Limit stability function
       AKMS = MAX (USTARK / SIMM,CXCH)
!-----------------------------------------------------------------------
! IF STATEMENTS TO AVOID TANGENT LINEAR PROBLEMS NEAR ZERO
!-----------------------------------------------------------------------
       IF(SIMH < 1.0e-6) SIMH = 1.0e-6        ! Limit stability function
       AKHS = MAX (USTARK / SIMH,CXCH)

       IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
          WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2.0/3.0)
       ELSE
          WSTAR2 = 0.0
       END IF
!-----------------------------------------------------------------------
       RLMN = ELFC * AKHS * DTHV / USTAR **3
!-----------------------------------------------------------------------
!     IF(ABS((RLMN-RLMO)/RLMA).LT.EPSIT)    GO TO 110
!-----------------------------------------------------------------------
       RLMA = RLMO * WOLD+ RLMN * WNEW
!-----------------------------------------------------------------------
       RLMO = RLMA

!       write(*,'(a20,10f15.6)')'SFCDIF: RLMO=',RLMO,RLMN,ELFC , AKHS , DTHV , USTAR
!    END DO
! ----------------------------------------------------------------------
  END SUBROUTINE SFCDIF2

!== begin esat =====================================================================================

  SUBROUTINE ESAT(T, ESW, ESI, DESW, DESI)
!---------------------------------------------------------------------------------------------------
! use polynomials to calculate saturation vapor pressure and derivative with
! respect to temperature: over water when t > 0 c and over ice when t <= 0 c
  IMPLICIT NONE
!---------------------------------------------------------------------------------------------------
! in

  REAL, intent(in)  :: T              !temperature

!out

  REAL, intent(out) :: ESW            !saturation vapor pressure over water (pa)
  REAL, intent(out) :: ESI            !saturation vapor pressure over ice (pa)
  REAL, intent(out) :: DESW           !d(esat)/dt over water (pa/K)
  REAL, intent(out) :: DESI           !d(esat)/dt over ice (pa/K)

! local

  REAL :: A0,A1,A2,A3,A4,A5,A6  !coefficients for esat over water
  REAL :: B0,B1,B2,B3,B4,B5,B6  !coefficients for esat over ice
  REAL :: C0,C1,C2,C3,C4,C5,C6  !coefficients for dsat over water
  REAL :: D0,D1,D2,D3,D4,D5,D6  !coefficients for dsat over ice

  PARAMETER (A0=6.107799961    , A1=4.436518521E-01,  &
             A2=1.428945805E-02, A3=2.650648471E-04,  &
             A4=3.031240396E-06, A5=2.034080948E-08,  &
             A6=6.136820929E-11)

  PARAMETER (B0=6.109177956    , B1=5.034698970E-01,  &
             B2=1.886013408E-02, B3=4.176223716E-04,  &
             B4=5.824720280E-06, B5=4.838803174E-08,  &
             B6=1.838826904E-10)

  PARAMETER (C0= 4.438099984E-01, C1=2.857002636E-02,  &
             C2= 7.938054040E-04, C3=1.215215065E-05,  &
             C4= 1.036561403E-07, C5=3.532421810e-10,  &
             C6=-7.090244804E-13)

  PARAMETER (D0=5.030305237E-01, D1=3.773255020E-02,  &
             D2=1.267995369E-03, D3=2.477563108E-05,  &
             D4=3.005693132E-07, D5=2.158542548E-09,  &
             D6=7.131097725E-12)

  ESW  = 100.0*(A0+T*(A1+T*(A2+T*(A3+T*(A4+T*(A5+T*A6))))))
  ESI  = 100.0*(B0+T*(B1+T*(B2+T*(B3+T*(B4+T*(B5+T*B6))))))
  DESW = 100.0*(C0+T*(C1+T*(C2+T*(C3+T*(C4+T*(C5+T*C6))))))
  DESI = 100.0*(D0+T*(D1+T*(D2+T*(D3+T*(D4+T*(D5+T*D6))))))

  END SUBROUTINE ESAT


!== begin stomata ==================================================================================

  SUBROUTINE STOMATA (parameters,VEGTYP  ,MPE     ,APAR    ,FOLN    ,ILOC    , JLOC, & !in
                      TV      ,EI      ,EA      ,SFCTMP  ,SFCPRS  , & !in
                      O2      ,CO2     ,IGS     ,BTRAN   ,RB      , & !in
                      RS      ,PSN     )                              !out
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
      INTEGER,INTENT(IN)  :: ILOC   !grid index
      INTEGER,INTENT(IN)  :: JLOC   !grid index
      INTEGER,INTENT(IN)  :: VEGTYP !vegetation physiology type

      REAL, INTENT(IN)    :: IGS    !growing season index (0=off, 1=on)
      REAL, INTENT(IN)    :: MPE    !prevents division by zero errors

      REAL, INTENT(IN)    :: TV     !foliage temperature (k)
      REAL, INTENT(IN)    :: EI     !vapor pressure inside leaf (sat vapor press at tv) (pa)
      REAL, INTENT(IN)    :: EA     !vapor pressure of canopy air (pa)
      REAL, INTENT(IN)    :: APAR   !par absorbed per unit lai (w/m2)
      REAL, INTENT(IN)    :: O2     !atmospheric o2 concentration (pa)
      REAL, INTENT(IN)    :: CO2    !atmospheric co2 concentration (pa)
      REAL, INTENT(IN)    :: SFCPRS !air pressure at reference height (pa)
      REAL, INTENT(IN)    :: SFCTMP !air temperature at reference height (k)
      REAL, INTENT(IN)    :: BTRAN  !soil water transpiration factor (0 to 1)
      REAL, INTENT(IN)    :: FOLN   !foliage nitrogen concentration (%)
      REAL, INTENT(IN)    :: RB     !boundary layer resistance (s/m)

! output
      REAL, INTENT(OUT)   :: RS     !leaf stomatal resistance (s/m)
      REAL, INTENT(OUT)   :: PSN    !foliage photosynthesis (umol co2 /m2/ s) [always +]

! in&out
      REAL                :: RLB    !boundary layer resistance (s m2 / umol)
! ---------------------------------------------------------------------------------------------

! ------------------------ local variables ----------------------------------------------------
      INTEGER :: ITER     !iteration index
      INTEGER :: NITER    !number of iterations

      DATA NITER /3/
      SAVE NITER

      REAL :: AB          !used in statement functions
      REAL :: BC          !used in statement functions
      REAL :: F1          !generic temperature response (statement function)
      REAL :: F2          !generic temperature inhibition (statement function)
      REAL :: TC          !foliage temperature (degree Celsius)
      REAL :: CS          !co2 concentration at leaf surface (pa)
      REAL :: KC          !co2 Michaelis-Menten constant (pa)
      REAL :: KO          !o2 Michaelis-Menten constant (pa)
      REAL :: A,B,C,Q     !intermediate calculations for RS
      REAL :: R1,R2       !roots for RS
      REAL :: FNF         !foliage nitrogen adjustment factor (0 to 1)
      REAL :: PPF         !absorb photosynthetic photon flux (umol photons/m2/s)
      REAL :: WC          !Rubisco limited photosynthesis (umol co2/m2/s)
      REAL :: WJ          !light limited photosynthesis (umol co2/m2/s)
      REAL :: WE          !export limited photosynthesis (umol co2/m2/s)
      REAL :: CP          !co2 compensation point (pa)
      REAL :: CI          !internal co2 (pa)
      REAL :: AWC         !intermediate calculation for wc
      REAL :: VCMX        !maximum rate of carbonylation (umol co2/m2/s)
      REAL :: J           !electron transport (umol co2/m2/s)
      REAL :: CEA         !constrain ea or else model blows up
      REAL :: CF          !s m2/umol -> s/m

      F1(AB,BC) = AB**((BC-25.0)/10.0)
      F2(AB) = 1. + EXP((-2.2E05+710.*(AB+273.16))/(8.314*(AB+273.16)))
      REAL :: T
! ---------------------------------------------------------------------------------------------

! initialize RS=RSMAX and PSN=0 because will only do calculations
! for APAR > 0, in which case RS <= RSMAX and PSN >= 0

         CF = SFCPRS/(8.314*SFCTMP)*1.0e06
         RS = 1.0/parameters%BP * CF
         PSN = 0.0

         IF (APAR .LE. 0.0) RETURN

         FNF = MIN( FOLN/MAX(MPE,parameters%FOLNMX), 1.0 )
         TC  = TV-TFRZ
         PPF = 4.6*APAR
         J   = PPF*parameters%QE25
         KC  = parameters%KC25 * F1(parameters%AKC,TC)
         KO  = parameters%KO25 * F1(parameters%AKO,TC)
         AWC = KC * (1.0+O2/KO)
         CP  = 0.5*KC/KO*O2*0.21
         VCMX = parameters%VCMX25 / F2(TC) * FNF * BTRAN * F1(parameters%AVCMX,TC)

! first guess ci

         CI = 0.7*CO2*parameters%C3PSN + 0.4*CO2*(1.0-parameters%C3PSN)

! rb: s/m -> s m**2 / umol

         RLB = RB/CF

! constrain ea

         CEA = MAX(0.25*EI*parameters%C3PSN+0.40*EI*(1.-parameters%C3PSN), MIN(EA,EI) )

! ci iteration
!jref: C3PSN is equal to 1 for all veg types.
       DO ITER = 1, NITER
            WJ = MAX(CI-CP,0.0)*J/(CI+2.0*CP)*parameters%C3PSN  + J*(1.0-parameters%C3PSN)
            WC = MAX(CI-CP,0.0)*VCMX/(CI+AWC)*parameters%C3PSN + VCMX*(1.0-parameters%C3PSN)
            WE = 0.5*VCMX*parameters%C3PSN + 4000.0*VCMX*CI/SFCPRS*(1.0-parameters%C3PSN)
            PSN = MIN(WJ,WC,WE) * IGS

            CS = MAX( CO2-1.37*RLB*SFCPRS*PSN, MPE )
            A = parameters%MP*PSN*SFCPRS*CEA / (CS*EI) + parameters%BP
            B = ( parameters%MP*PSN*SFCPRS/CS + parameters%BP ) * RLB - 1.0
            C = -RLB
            IF (B .GE. 0.0) THEN
               Q = -0.5*( B + SQRT(B*B-4.0*A*C) )
            ELSE
               Q = -0.5*( B - SQRT(B*B-4.0*A*C) )
            END IF
            R1 = Q/A
            R2 = C/Q
            RS = MAX(R1,R2)
            CI = MAX( CS-PSN*SFCPRS*1.65*RS, 0.0 )
       END DO 

! rs, rb:  s m**2 / umol -> s/m

         RS = RS*CF

  END SUBROUTINE STOMATA

!== begin canres ===================================================================================

  SUBROUTINE CANRES (parameters,PAR   ,SFCTMP,RCSOIL ,EAH   ,SFCPRS , & !in
                     RC    ,PSN   ,ILOC   ,JLOC  )           !out

! --------------------------------------------------------------------------------------------------
! calculate canopy resistance which depends on incoming solar radiation,
! air temperature, atmospheric water vapor pressure deficit at the
! lowest model level, and soil moisture (preferably unfrozen soil
! moisture rather than total)
! --------------------------------------------------------------------------------------------------
! source:  Jarvis (1976), Noilhan and Planton (1989, MWR), Jacquemin and
! Noilhan (1990, BLM). Chen et al (1996, JGR, Vol 101(D3), 7251-7268), 
! eqns 12-14 and table 2 of sec. 3.1.2
! --------------------------------------------------------------------------------------------------
!niu    USE module_Noahlsm_utility
! --------------------------------------------------------------------------------------------------
    IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                  INTENT(IN)  :: ILOC   !grid index
    INTEGER,                  INTENT(IN)  :: JLOC   !grid index
    REAL,                     INTENT(IN)  :: PAR    !par absorbed per unit sunlit lai (w/m2)
    REAL,                     INTENT(IN)  :: SFCTMP !canopy air temperature
    REAL,                     INTENT(IN)  :: SFCPRS !surface pressure (pa)
    REAL,                     INTENT(IN)  :: EAH    !water vapor pressure (pa)
    REAL,                     INTENT(IN)  :: RCSOIL !soil moisture stress factor

!outputs

    REAL,                     INTENT(OUT) :: RC     !canopy resistance per unit LAI
    REAL,                     INTENT(OUT) :: PSN    !foliage photosynthesis (umolco2/m2/s)

!local

    REAL                                  :: RCQ
    REAL                                  :: RCS
    REAL                                  :: RCT
    REAL                                  :: FF
    REAL                                  :: Q2     !water vapor mixing ratio (kg/kg)
    REAL                                  :: Q2SAT  !saturation Q2
    REAL                                  :: DQSDT2 !d(Q2SAT)/d(T)

! RSMIN, RSMAX, TOPT, RGL, HS are canopy stress parameters set in REDPRM
! ----------------------------------------------------------------------
! initialize canopy resistance multiplier terms.
! ----------------------------------------------------------------------
    RC     = 0.0
    RCS    = 0.0
    RCT    = 0.0
    RCQ    = 0.0

!  compute Q2 and Q2SAT

    Q2 = 0.622 *  EAH  / (SFCPRS - 0.378 * EAH) !specific humidity [kg/kg]
    Q2 = Q2 / (1.0 + Q2)                        !mixing ratio [kg/kg]

    CALL CALHUM(parameters,SFCTMP, SFCPRS, Q2SAT, DQSDT2)

! contribution due to incoming solar radiation

    FF  = 2.0 * PAR / parameters%RGL                
    RCS = (FF + parameters%RSMIN / parameters%RSMAX) / (1.0+ FF)
    RCS = MAX (RCS,0.0001)

! contribution due to air temperature

    RCT = 1.0- 0.0016* ( (parameters%TOPT - SFCTMP)**2.0)
    RCT = MAX (RCT,0.0001)

! contribution due to vapor pressure deficit

    RCQ = 1.0/ (1.0+ parameters%HS * MAX(0.0,Q2SAT-Q2))
    RCQ = MAX (RCQ,0.01)

! determine canopy resistance due to all factors

    RC  = parameters%RSMIN / (RCS * RCT * RCQ * RCSOIL)
    PSN = -999.99       ! PSN not applied for dynamic carbon

  END SUBROUTINE CANRES

!== begin calhum ===================================================================================

  SUBROUTINE CALHUM(parameters,SFCTMP, SFCPRS, Q2SAT, DQSDT2)

        IMPLICIT NONE

  type (noahmp_parameters), intent(in) :: parameters
        REAL, INTENT(IN)       :: SFCTMP, SFCPRS
        REAL, INTENT(OUT)      :: Q2SAT, DQSDT2
        REAL, PARAMETER        :: A2=17.67,A3=273.15,A4=29.65, ELWV=2.501E6,         &
                                  A23M4=A2*(A3-A4), E0=0.611, RV=461.0,             &
                                  EPSILON=0.622
        REAL                   :: ES, SFCPRSX

! Q2SAT: saturated mixing ratio
        ES = E0 * EXP ( ELWV/RV*(1.0/A3 - 1.0/SFCTMP) )
! convert SFCPRS from Pa to KPa
        SFCPRSX = SFCPRS*1.0E-3
        Q2SAT = EPSILON * ES / (SFCPRSX-ES)
! convert from  g/g to g/kg
        Q2SAT = Q2SAT * 1.0E3
! Q2SAT is currently a 'mixing ratio'

! DQSDT2 is calculated assuming Q2SAT is a specific humidity
        DQSDT2=(Q2SAT/(1+Q2SAT))*A23M4/(SFCTMP-A4)**2

! DG Q2SAT needs to be in g/g when returned for SFLX
        Q2SAT = Q2SAT / 1.0E3

  END SUBROUTINE CALHUM

!== begin tsnosoi ==================================================================================

  SUBROUTINE TSNOSOI (parameters,ICE     ,NSOIL   ,NSNOW   ,ISNOW   ,IST     , & !in
                      TBOT    ,ZSNSO   ,SSOIL   ,DF      ,HCPCT   , & !in
                      SAG     ,DT      ,SNOWH   ,DZSNSO  , & !in
                      TG      ,ILOC    ,JLOC    ,                   & !in
                      STC     )                                       !inout
! --------------------------------------------------------------------------------------------------
! Compute snow (up to 3L) and soil (4L) temperature. Note that snow temperatures
! during melting season may exceed melting point (TFRZ) but later in PHASECHANGE
! subroutine the snow temperatures are reset to TFRZ for melting snow.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
!input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                         INTENT(IN)  :: ILOC
    INTEGER,                         INTENT(IN)  :: JLOC
    INTEGER,                         INTENT(IN)  :: ICE    !
    INTEGER,                         INTENT(IN)  :: NSOIL  !no of soil layers (4)
    INTEGER,                         INTENT(IN)  :: NSNOW  !maximum no of snow layers (3)
    INTEGER,                         INTENT(IN)  :: ISNOW  !actual no of snow layers
    INTEGER,                         INTENT(IN)  :: IST    !surface type

    REAL,                            INTENT(IN)  :: DT     !time step (s)
    REAL,                            INTENT(IN)  :: TBOT   !
    REAL,                            INTENT(IN)  :: SSOIL  !ground heat flux (w/m2)
    REAL,                            INTENT(IN)  :: SAG    !solar rad. absorbed by ground (w/m2)
    REAL,                            INTENT(IN)  :: SNOWH  !snow depth (m)
    REAL,                            INTENT(IN)  :: TG     !ground temperature (k)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: ZSNSO  !layer-bot. depth from snow surf.(m)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DZSNSO !snow/soil layer thickness (m)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DF     !thermal conductivity
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: HCPCT  !heat capacity (J/m3/k)

!input and output

    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC

!local

    INTEGER                                      :: IZ
    REAL                                         :: ZBOTSNO   !ZBOT from snow surface
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: AI, BI, CI, RHSTS
    REAL                                         :: EFLXB !energy influx from soil bottom (w/m2)
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: PHI   !light through water (w/m2)

    REAL, DIMENSION(-NSNOW+1:NSOIL) :: TBEG
    REAL                            :: ERR_EST !heat storage error  (w/m2)
    REAL                            :: SSOIL2  !ground heat flux (w/m2) (for energy check)
    REAL                            :: EFLXB2  !heat flux from the bottom (w/m2) (for energy check)
    character(len=256)              :: message
! ----------------------------------------------------------------------
! compute solar penetration through water, needs more work

    PHI(ISNOW+1:NSOIL) = 0.0

! adjust ZBOT from soil surface to ZBOTSNO from snow surface

    ZBOTSNO = parameters%ZBOT - SNOWH    !from snow surface

! snow/soil heat storage for energy balance check

    DO IZ = ISNOW+1, NSOIL
       TBEG(IZ) = STC(IZ)
    ENDDO

! compute soil temperatures

      CALL HRT   (parameters,NSNOW     ,NSOIL     ,ISNOW     ,ZSNSO     , &
                  STC       ,TBOT      ,ZBOTSNO   ,DT        , &
                  DF        ,HCPCT     ,SSOIL     ,PHI       , &
                  AI        ,BI        ,CI        ,RHSTS     , &
                  EFLXB     )

      CALL HSTEP (parameters,NSNOW     ,NSOIL     ,ISNOW     ,DT        , &
                  AI        ,BI        ,CI        ,RHSTS     , &
                  STC       ) 

! update ground heat flux just for energy check, but not for final output
! otherwise, it would break the surface energy balance

    IF(OPT_TBOT == 1) THEN
       EFLXB2  = 0.0
    ELSE IF(OPT_TBOT == 2) THEN
       EFLXB2  = DF(NSOIL)*(TBOT-STC(NSOIL)) / &
            (0.5*(ZSNSO(NSOIL-1)+ZSNSO(NSOIL)) - ZBOTSNO)
    END IF

    ! Skip the energy balance check for now, until we can make it work
    ! right for small time steps.
    return

! energy balance check

    ERR_EST = 0.0
    DO IZ = ISNOW+1, NSOIL
       ERR_EST = ERR_EST + (STC(IZ)-TBEG(IZ)) * DZSNSO(IZ) * HCPCT(IZ) / DT
    ENDDO

    if (OPT_STC == 1 .OR. OPT_STC == 3) THEN   ! semi-implicit
       ERR_EST = ERR_EST - (SSOIL +EFLXB)
    ELSE                     ! full-implicit
       SSOIL2 = DF(ISNOW+1)*(TG-STC(ISNOW+1))/(0.5*DZSNSO(ISNOW+1))   !M. Barlage
       ERR_EST = ERR_EST - (SSOIL2+EFLXB2)
    ENDIF

    IF (ABS(ERR_EST) > 1.0) THEN    ! W/m2
       WRITE(message,*) 'TSNOSOI is losing(-)/gaining(+) false energy',ERR_EST,' W/m2'
       call wrf_message(trim(message))
       WRITE(message,'(i6,1x,i6,1x,i3,F18.13,5F20.12)') &
            ILOC, JLOC, IST,ERR_EST,SSOIL,SNOWH,TG,STC(ISNOW+1),EFLXB
       call wrf_message(trim(message))
       !niu      STOP
    END IF

  END SUBROUTINE TSNOSOI

!== begin hrt ======================================================================================

  SUBROUTINE HRT (parameters,NSNOW     ,NSOIL     ,ISNOW     ,ZSNSO     , &
                  STC       ,TBOT      ,ZBOT      ,DT        , &
                  DF        ,HCPCT     ,SSOIL     ,PHI       , &
                  AI        ,BI        ,CI        ,RHSTS     , &
                  BOTFLX    )
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! calculate the right hand side of the time tendency term of the soil
! thermal diffusion equation.  also to compute ( prepare ) the matrix
! coefficients for the tri-diagonal matrix of the implicit time scheme.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                         INTENT(IN)  :: NSOIL  !no of soil layers (4)
    INTEGER,                         INTENT(IN)  :: NSNOW  !maximum no of snow layers (3)
    INTEGER,                         INTENT(IN)  :: ISNOW  !actual no of snow layers
    REAL,                            INTENT(IN)  :: TBOT   !bottom soil temp. at ZBOT (k)
    REAL,                            INTENT(IN)  :: ZBOT   !depth of lower boundary condition (m)
                                                           !from soil surface not snow surface
    REAL,                            INTENT(IN)  :: DT     !time step (s)
    REAL,                            INTENT(IN)  :: SSOIL  !ground heat flux (w/m2)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: ZSNSO  !depth of layer-bottom of snow/soil (m)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: STC    !snow/soil temperature (k)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DF     !thermal conductivity [w/m/k]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: HCPCT  !heat capacity [j/m3/k]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: PHI    !light through water (w/m2)

! output

    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: RHSTS  !right-hand side of the matrix
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: AI     !left-hand side coefficient
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: BI     !left-hand side coefficient
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: CI     !left-hand side coefficient
    REAL,                            INTENT(OUT) :: BOTFLX !energy influx from soil bottom (w/m2)

! local

    INTEGER                                      :: K
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DDZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DENOM
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DTSDZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: EFLUX
    REAL                                         :: TEMP1
! ----------------------------------------------------------------------

    DO K = ISNOW+1, NSOIL
        IF (K == ISNOW+1) THEN
           DENOM(K)  = - ZSNSO(K) * HCPCT(K)
           TEMP1     = - ZSNSO(K+1)
           DDZ(K)    = 2.0 / TEMP1
           DTSDZ(K)  = 2.0 * (STC(K) - STC(K+1)) / TEMP1
           EFLUX(K)  = DF(K) * DTSDZ(K) - SSOIL - PHI(K)
        ELSE IF (K < NSOIL) THEN
           DENOM(K)  = (ZSNSO(K-1) - ZSNSO(K)) * HCPCT(K)
           TEMP1     = ZSNSO(K-1) - ZSNSO(K+1)
           DDZ(K)    = 2.0 / TEMP1
           DTSDZ(K)  = 2.0 * (STC(K) - STC(K+1)) / TEMP1
           EFLUX(K)  = (DF(K)*DTSDZ(K) - DF(K-1)*DTSDZ(K-1)) - PHI(K)
        ELSE IF (K == NSOIL) THEN
           DENOM(K)  = (ZSNSO(K-1) - ZSNSO(K)) * HCPCT(K)
           TEMP1     =  ZSNSO(K-1) - ZSNSO(K)
           IF(OPT_TBOT == 1) THEN
               BOTFLX     = 0.0 
           END IF
           IF(OPT_TBOT == 2) THEN
               DTSDZ(K)  = (STC(K) - TBOT) / ( 0.5*(ZSNSO(K-1)+ZSNSO(K)) - ZBOT)
               BOTFLX    = -DF(K) * DTSDZ(K)
           END IF
           EFLUX(K)  = (-BOTFLX - DF(K-1)*DTSDZ(K-1) ) - PHI(K)
        END IF
    END DO

    DO K = ISNOW+1, NSOIL
        IF (K == ISNOW+1) THEN
           AI(K)    =   0.0
           CI(K)    = - DF(K)   * DDZ(K) / DENOM(K)
           IF (OPT_STC == 1 .OR. OPT_STC == 3 ) THEN
              BI(K) = - CI(K)
           END IF                                        
           IF (OPT_STC == 2) THEN
              BI(K) = - CI(K) + DF(K)/(0.5*ZSNSO(K)*ZSNSO(K)*HCPCT(K))
           END IF
        ELSE IF (K < NSOIL) THEN
           AI(K)    = - DF(K-1) * DDZ(K-1) / DENOM(K) 
           CI(K)    = - DF(K  ) * DDZ(K  ) / DENOM(K) 
           BI(K)    = - (AI(K) + CI (K))
        ELSE IF (K == NSOIL) THEN
           AI(K)    = - DF(K-1) * DDZ(K-1) / DENOM(K) 
           CI(K)    = 0.0
           BI(K)    = - (AI(K) + CI(K))
        END IF
           RHSTS(K)  = EFLUX(K)/ (-DENOM(K))
    END DO

  END SUBROUTINE HRT

!== begin hstep ====================================================================================

  SUBROUTINE HSTEP (parameters,NSNOW     ,NSOIL     ,ISNOW     ,DT        ,  &
                    AI        ,BI        ,CI        ,RHSTS     ,  &
                    STC       )  
! ----------------------------------------------------------------------
! CALCULATE/UPDATE THE SOIL TEMPERATURE FIELD.
! ----------------------------------------------------------------------
    implicit none
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                         INTENT(IN)    :: NSOIL
    INTEGER,                         INTENT(IN)    :: NSNOW
    INTEGER,                         INTENT(IN)    :: ISNOW
    REAL,                            INTENT(IN)    :: DT

! output & input
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: RHSTS
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: AI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: BI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: CI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC

! local
    INTEGER                                        :: K
    REAL, DIMENSION(-NSNOW+1:NSOIL)                :: RHSTSIN
    REAL, DIMENSION(-NSNOW+1:NSOIL)                :: CIIN
! ----------------------------------------------------------------------

    DO K = ISNOW+1,NSOIL
       RHSTS(K) =   RHSTS(K) * DT
       AI(K)    =      AI(K) * DT
       BI(K)    = 1.0 + BI(K) * DT
       CI(K)    =      CI(K) * DT
    END DO

! copy values for input variables before call to rosr12

    DO K = ISNOW+1,NSOIL
       RHSTSIN(K) = RHSTS(K)
       CIIN(K)    = CI(K)
    END DO

! solve the tri-diagonal matrix equation

    CALL ROSR12 (CI,AI,BI,CIIN,RHSTSIN,RHSTS,ISNOW+1,NSOIL,NSNOW)

! update snow & soil temperature

    DO K = ISNOW+1,NSOIL
       STC (K) = STC (K) + CI (K)
    END DO

  END SUBROUTINE HSTEP

!== begin phasechange ==============================================================================

  SUBROUTINE PHASECHANGE (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,FACT    , & !in
                          DZSNSO  ,HCPCT   ,IST     ,ILOC    ,JLOC    , & !in
                          STC     ,SNICE   ,SNLIQ   ,SNEQV   ,SNOWH   , & !inout
                          SMC     ,SH2O    ,                            & !inout
                          QMELT   ,IMELT   ,PONDING )                     !out
! ----------------------------------------------------------------------
! melting/freezing of snow water and soil water
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! inputs

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER, INTENT(IN)                             :: ILOC   !grid index
  INTEGER, INTENT(IN)                             :: JLOC   !grid index
  INTEGER, INTENT(IN)                             :: NSNOW  !maximum no. of snow layers [=3]
  INTEGER, INTENT(IN)                             :: NSOIL  !No. of soil layers [=4]
  INTEGER, INTENT(IN)                             :: ISNOW  !actual no. of snow layers [<=3]
  INTEGER, INTENT(IN)                             :: IST    !surface type: 1->soil; 2->lake
  REAL, INTENT(IN)                                :: DT     !land model time step (sec)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: FACT   !temporary
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: DZSNSO !snow/soil layer thickness [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: HCPCT  !heat capacity (J/m3/k)

! outputs
  INTEGER, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: IMELT  !phase change index
  REAL,                               INTENT(OUT) :: QMELT  !snowmelt rate [mm/s]
  REAL,                               INTENT(OUT) :: PONDING!snowmelt when snow has no layer [mm]

! inputs and outputs

  REAL, INTENT(INOUT) :: SNEQV
  REAL, INTENT(INOUT) :: SNOWH
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT)  :: STC    !snow/soil layer temperature [k]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT)  :: SH2O   !soil liquid water [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT)  :: SMC    !total soil water [m3/m3]
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT)  :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT)  :: SNLIQ  !snow layer liquid water [mm]

! local

  INTEGER                         :: J         !do loop index
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: HM        !energy residual [w/m2]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: XM        !melting or freezing water [kg/m2]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WMASS0
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WICE0 
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WLIQ0 
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: MICE      !soil/snow ice mass [mm]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: MLIQ      !soil/snow liquid water mass [mm]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: SUPERCOOL !supercooled water in soil (kg/m2)
  REAL                            :: HEATR     !energy residual or loss after melting/freezing
  REAL                            :: TEMP1     !temporary variables [kg/m2]
  REAL                            :: PROPOR
  REAL                            :: SMP       !frozen water potential (mm)
  REAL                            :: XMF       !total latent heat of phase change

! ----------------------------------------------------------------------
! Initialization

    QMELT   = 0.0
    PONDING = 0.0
    XMF     = 0.0

    DO J = -NSNOW+1, NSOIL
         SUPERCOOL(J) = 0.0
    END DO

    DO J = ISNOW+1,0       ! all layers
         MICE(J) = SNICE(J)
         MLIQ(J) = SNLIQ(J)
    END DO

    DO J = 1, NSOIL               ! soil
         MLIQ(J) =  SH2O(J)            * DZSNSO(J) * 1000.0
         MICE(J) = (SMC(J) - SH2O(J))  * DZSNSO(J) * 1000.0
    END DO

    DO J = ISNOW+1,NSOIL       ! all layers
         IMELT(J)    = 0
         HM(J)       = 0.0
         XM(J)       = 0.0
         WICE0(J)    = MICE(J)
         WLIQ0(J)    = MLIQ(J)
         WMASS0(J)   = MICE(J) + MLIQ(J)
    ENDDO

    if(ist == 1) then
      DO J = 1,NSOIL
         IF (OPT_FRZ == 1) THEN
            IF(STC(J) < TFRZ) THEN
               SMP = HFUS*(TFRZ-STC(J))/(GRAV*STC(J))             !(m)
               SUPERCOOL(J) = parameters%SMCMAX(J)*(SMP/parameters%PSISAT(J))**(-1.0/parameters%BEXP(J))
               SUPERCOOL(J) = SUPERCOOL(J)*DZSNSO(J)*1000.0        !(mm)
            END IF
         END IF
         IF (OPT_FRZ == 2) THEN
               CALL FRH2O (parameters,J,SUPERCOOL(J),STC(J),SMC(J),SH2O(J))
               SUPERCOOL(J) = SUPERCOOL(J)*DZSNSO(J)*1000.0        !(mm)
         END IF
      ENDDO
    end if

    DO J = ISNOW+1,NSOIL
         IF (MICE(J) > 0.0 .AND. STC(J) >= TFRZ) THEN  !melting 
             IMELT(J) = 1
         ENDIF
         IF (MLIQ(J) > SUPERCOOL(J) .AND. STC(J) < TFRZ) THEN
             IMELT(J) = 2
         ENDIF

         ! If snow exists, but its thickness is not enough to create a layer
         IF (ISNOW == 0 .AND. SNEQV > 0.0 .AND. J == 1) THEN
             IF (STC(J) >= TFRZ) THEN
                IMELT(J) = 1
             ENDIF
         ENDIF
    ENDDO

! Calculate the energy surplus and loss for melting and freezing

    DO J = ISNOW+1,NSOIL
         IF (IMELT(J) > 0) THEN
             HM(J) = (STC(J)-TFRZ)/FACT(J)
             STC(J) = TFRZ
         ENDIF

         IF (IMELT(J) == 1 .AND. HM(J) < 0.0) THEN
            HM(J) = 0.0
            IMELT(J) = 0
         ENDIF
         IF (IMELT(J) == 2 .AND. HM(J) > 0.0) THEN
            HM(J) = 0.0
            IMELT(J) = 0
         ENDIF
         XM(J) = HM(J)*DT/HFUS                           
    ENDDO

! The rate of melting and freezing for snow without a layer, needs more work.

    IF (ISNOW == 0 .AND. SNEQV > 0.0 .AND. XM(1) > 0.0) THEN  
        TEMP1  = SNEQV
        SNEQV  = MAX(0.0,TEMP1-XM(1))  
        PROPOR = SNEQV/TEMP1
        SNOWH  = MAX(0.0,PROPOR * SNOWH)
        SNOWH  = MIN(MAX(SNOWH,SNEQV/500.0),SNEQV/50.0)  ! limit adjustment to a reasonable density
        HEATR  = HM(1) - HFUS*(TEMP1-SNEQV)/DT  
        IF (HEATR > 0.0) THEN
              XM(1) = HEATR*DT/HFUS             
              HM(1) = HEATR                    
        ELSE
              XM(1) = 0.0
              HM(1) = 0.0
        ENDIF
        QMELT   = MAX(0.0,(TEMP1-SNEQV))/DT
        XMF     = HFUS*QMELT
        PONDING = TEMP1-SNEQV
    ENDIF

! The rate of melting and freezing for snow and soil

    DO J = ISNOW+1,NSOIL
      IF (IMELT(J) > 0 .AND. ABS(HM(J)) > 0.0) THEN

         HEATR = 0.0
         IF (XM(J) > 0.0) THEN                            
            MICE(J) = MAX(0.0, WICE0(J)-XM(J))
            HEATR = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ELSE IF (XM(J) < 0.0) THEN                      
            IF (J <= 0) THEN                             ! snow
               MICE(J) = MIN(WMASS0(J), WICE0(J)-XM(J))  
            ELSE                                         ! soil
               IF (WMASS0(J) < SUPERCOOL(J)) THEN
                  MICE(J) = 0.0
               ELSE
                  MICE(J) = MIN(WMASS0(J) - SUPERCOOL(J),WICE0(J)-XM(J))
                  MICE(J) = MAX(MICE(J),0.0)
               ENDIF
            ENDIF
            HEATR = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ENDIF

         MLIQ(J) = MAX(0.0,WMASS0(J)-MICE(J))

         IF (ABS(HEATR) > 0.0) THEN
            STC(J) = STC(J) + FACT(J)*HEATR
            IF (J <= 0) THEN                             ! snow
               IF (MLIQ(J)*MICE(J)>0.0) STC(J) = TFRZ
               IF (MICE(J) == 0.0) THEN         ! BARLAGE
                  STC(J) = TFRZ                ! BARLAGE
                  HM(J+1) = HM(J+1) + HEATR    ! BARLAGE
                  XM(J+1) = HM(J+1)*DT/HFUS    ! BARLAGE
               ENDIF 
            END IF
         ENDIF

         XMF = XMF + HFUS * (WICE0(J)-MICE(J))/DT

         IF (J < 1) THEN
            QMELT = QMELT + MAX(0.0,(WICE0(J)-MICE(J)))/DT
         ENDIF
      ENDIF
    ENDDO

    DO J = ISNOW+1,0             ! snow
       SNLIQ(J) = MLIQ(J)
       SNICE(J) = MICE(J)
    END DO

    DO J = 1, NSOIL              ! soil
       SH2O(J) =  MLIQ(J)            / (1000.0 * DZSNSO(J))
       SMC(J)  = (MLIQ(J) + MICE(J)) / (1000.0 * DZSNSO(J))
    END DO
   
  END SUBROUTINE PHASECHANGE

!== begin frh2o ====================================================================================

  SUBROUTINE FRH2O (parameters,ISOIL,FREE,TKELV,SMC,SH2O)

! ----------------------------------------------------------------------
! SUBROUTINE FRH2O
! ----------------------------------------------------------------------
! CALCULATE AMOUNT OF SUPERCOOLED LIQUID SOIL WATER CONTENT IF
! TEMPERATURE IS BELOW 273.15K (TFRZ).  REQUIRES NEWTON-TYPE ITERATION
! TO SOLVE THE NONLINEAR IMPLICIT EQUATION GIVEN IN EQN 17 OF KOREN ET AL
! (1999, JGR, VOL 104(D16), 19569-19585).
! ----------------------------------------------------------------------
! NEW VERSION (JUNE 2001): MUCH FASTER AND MORE ACCURATE NEWTON
! ITERATION ACHIEVED BY FIRST TAKING LOG OF EQN CITED ABOVE -- LESS THAN
! 4 (TYPICALLY 1 OR 2) ITERATIONS ACHIEVES CONVERGENCE.  ALSO, EXPLICIT
! 1-STEP SOLUTION OPTION FOR SPECIAL CASE OF PARAMETER CK=0, WHICH
! REDUCES THE ORIGINAL IMPLICIT EQUATION TO A SIMPLER EXPLICIT FORM,
! KNOWN AS THE "FLERCHINGER EQN". IMPROVED HANDLING OF SOLUTION IN THE
! LIMIT OF FREEZING POINT TEMPERATURE TFRZ.
! ----------------------------------------------------------------------
! INPUT:

!   TKELV.........TEMPERATURE (Kelvin)
!   SMC...........TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC)
!   SH2O..........LIQUID SOIL MOISTURE CONTENT (VOLUMETRIC)
!   B.............SOIL TYPE "B" PARAMETER (FROM REDPRM)
!   PSISAT........SATURATED SOIL MATRIC POTENTIAL (FROM REDPRM)

! OUTPUT:
!   FREE..........SUPERCOOLED LIQUID WATER CONTENT [m3/m3]
! ----------------------------------------------------------------------
    IMPLICIT NONE
  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,INTENT(IN)   :: ISOIL
    REAL, INTENT(IN)     :: SH2O,SMC,TKELV
    REAL, INTENT(OUT)    :: FREE
    REAL                 :: BX,DENOM,DF,DSWL,FK,SWL,SWLK
    INTEGER              :: NLOG,KCOUNT
!      PARAMETER(CK = 0.0)
    REAL, PARAMETER      :: CK = 8.0, BLIM = 5.5, ERROR = 0.005,       &
         DICE = 920.0
    CHARACTER(LEN=80)    :: message

! ----------------------------------------------------------------------
! LIMITS ON PARAMETER B: B < 5.5  (use parameter BLIM)
! SIMULATIONS SHOWED IF B > 5.5 UNFROZEN WATER CONTENT IS
! NON-REALISTICALLY HIGH AT VERY LOW TEMPERATURES.
! ----------------------------------------------------------------------
    BX = parameters%BEXP(ISOIL)
! ----------------------------------------------------------------------
! INITIALIZING ITERATIONS COUNTER AND ITERATIVE SOLUTION FLAG.
! ----------------------------------------------------------------------

    IF (parameters%BEXP(ISOIL) >  BLIM) BX = BLIM
    NLOG = 0

! ----------------------------------------------------------------------
!  IF TEMPERATURE NOT SIGNIFICANTLY BELOW FREEZING (TFRZ), SH2O = SMC
! ----------------------------------------------------------------------
    KCOUNT = 0
    IF (TKELV > (TFRZ- 1.0E-3)) THEN
       FREE = SMC
    ELSE

! ----------------------------------------------------------------------
! OPTION 1: ITERATED SOLUTION IN KOREN ET AL, JGR, 1999, EQN 17
! ----------------------------------------------------------------------
! INITIAL GUESS FOR SWL (frozen content)
! ----------------------------------------------------------------------
       IF (CK /= 0.0) THEN
          SWL = SMC - SH2O
! ----------------------------------------------------------------------
! KEEP WITHIN BOUNDS.
! ----------------------------------------------------------------------
          IF (SWL > (SMC -0.02)) SWL = SMC -0.02
! ----------------------------------------------------------------------
!  START OF ITERATIONS
! ----------------------------------------------------------------------
          IF (SWL < 0.0) SWL = 0.0
1001      Continue
          IF (.NOT.( (NLOG < 10) .AND. (KCOUNT == 0)))   goto 1002
          NLOG = NLOG +1
          DF = ALOG ( ( parameters%PSISAT(ISOIL) * GRAV / HFUS ) * ( ( 1.0 + CK * SWL )**2.0) * &
               ( parameters%SMCMAX(ISOIL) / (SMC - SWL) )** BX) - ALOG ( - (               &
               TKELV - TFRZ)/ TKELV)
          DENOM = 2.0 * CK / ( 1.0 + CK * SWL ) + BX / ( SMC - SWL )
          SWLK = SWL - DF / DENOM
! ----------------------------------------------------------------------
! BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.
! ----------------------------------------------------------------------
          IF (SWLK > (SMC -0.02)) SWLK = SMC - 0.02
          IF (SWLK < 0.0) SWLK = 0.0

! ----------------------------------------------------------------------
! MATHEMATICAL SOLUTION BOUNDS APPLIED.
! ----------------------------------------------------------------------
          DSWL = ABS (SWLK - SWL)
! IF MORE THAN 10 ITERATIONS, USE EXPLICIT METHOD (CK=0 APPROX.)
! WHEN DSWL LESS OR EQ. ERROR, NO MORE ITERATIONS REQUIRED.
! ----------------------------------------------------------------------
          SWL = SWLK
          IF ( DSWL <= ERROR ) THEN
             KCOUNT = KCOUNT +1
          END IF
! ----------------------------------------------------------------------
!  END OF ITERATIONS
! ----------------------------------------------------------------------
! BOUNDS APPLIED WITHIN DO-BLOCK ARE VALID FOR PHYSICAL SOLUTION.
! ----------------------------------------------------------------------
          goto 1001
1002      continue
          FREE = SMC - SWL
       END IF
! ----------------------------------------------------------------------
! END OPTION 1
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! OPTION 2: EXPLICIT SOLUTION FOR FLERCHINGER EQ. i.e. CK=0
! IN KOREN ET AL., JGR, 1999, EQN 17
! APPLY PHYSICAL BOUNDS TO FLERCHINGER SOLUTION
! ----------------------------------------------------------------------
       IF (KCOUNT == 0) THEN
          write(message, '("Flerchinger used in NEW version. Iterations=", I6)') NLOG
          call wrf_message(trim(message))
          FK = ( ( (HFUS / (GRAV * ( - parameters%PSISAT(ISOIL))))*                    &
               ( (TKELV - TFRZ)/ TKELV))** ( -1.0/ BX))* parameters%SMCMAX(ISOIL)
          IF (FK < 0.02) FK = 0.02
          FREE = MIN (FK, SMC)
! ----------------------------------------------------------------------
! END OPTION 2
! ----------------------------------------------------------------------
       END IF
    END IF
! ----------------------------------------------------------------------
  END SUBROUTINE FRH2O
! ----------------------------------------------------------------------
! ==================================================================================================
! **********************End of energy subroutines***********************
! ==================================================================================================









