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









