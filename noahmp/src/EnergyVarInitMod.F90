module EnergyVarInitMod

!!! Initialize column (1-D) Noah-MP energy variables
!!! Energy variables should be first defined in EnergyVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (July 2022)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use NoahmpVarType

  implicit none

contains

!=== initialize with default values
  subroutine EnergyVarInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    associate(                                                         &
              NumSnowLayerMax => noahmp%config%domain%NumSnowLayerMax ,&
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer    ,&
              NumSWRadBand    => noahmp%config%domain%NumSWRadBand     &
             )
    
    ! energy state variables
    noahmp%energy%state%FROZEN_CANOPY   = .false.
    noahmp%energy%state%FROZEN_GROUND   = .false.
    noahmp%energy%state%ELAI            = undefined_real 
    noahmp%energy%state%ESAI            = undefined_real
    noahmp%energy%state%LAI             = undefined_real
    noahmp%energy%state%SAI             = undefined_real
    noahmp%energy%state%VAI             = undefined_real
    noahmp%energy%state%FVEG            = undefined_real
    noahmp%energy%state%EAIR            = undefined_real
    noahmp%energy%state%FAGE            = undefined_real
    noahmp%energy%state%TAUSS           = undefined_real
    noahmp%energy%state%ALBOLD          = undefined_real
    noahmp%energy%state%GDIR            = undefined_real
    noahmp%energy%state%BGAP            = undefined_real
    noahmp%energy%state%WGAP            = undefined_real
    noahmp%energy%state%KOPEN           = undefined_real
    noahmp%energy%state%GAP             = undefined_real
    noahmp%energy%state%FSUN            = undefined_real
    noahmp%energy%state%FSHA            = undefined_real
    noahmp%energy%state%LAISUN          = undefined_real
    noahmp%energy%state%LAISHA          = undefined_real
    noahmp%energy%state%ESTV            = undefined_real
    noahmp%energy%state%ESTG            = undefined_real
    noahmp%energy%state%ESTB            = undefined_real
    noahmp%energy%state%DESTV           = undefined_real
    noahmp%energy%state%DESTG           = undefined_real
    noahmp%energy%state%DESTB           = undefined_real
    noahmp%energy%state%EAH             = undefined_real
    noahmp%energy%state%CO2AIR          = undefined_real
    noahmp%energy%state%O2AIR           = undefined_real
    noahmp%energy%state%RSSUN           = undefined_real
    noahmp%energy%state%RSSHA           = undefined_real
    noahmp%energy%state%RHOAIR          = undefined_real
    noahmp%energy%state%TAH             = undefined_real
    noahmp%energy%state%ZPD             = undefined_real
    noahmp%energy%state%ZPDG            = undefined_real
    noahmp%energy%state%Z0MG            = undefined_real
    noahmp%energy%state%Z0M             = undefined_real
    noahmp%energy%state%HCAN            = undefined_real
    noahmp%energy%state%UC              = undefined_real
    noahmp%energy%state%Z0HV            = undefined_real
    noahmp%energy%state%Z0HG            = undefined_real
    noahmp%energy%state%Z0HB            = undefined_real
    noahmp%energy%state%FVV             = undefined_real
    noahmp%energy%state%FVB             = undefined_real
    noahmp%energy%state%CWPC            = undefined_real
    noahmp%energy%state%MOZG            = undefined_real
    noahmp%energy%state%MOZV            = undefined_real
    noahmp%energy%state%MOZB            = undefined_real
    noahmp%energy%state%MOZ2V           = undefined_real
    noahmp%energy%state%MOZ2B           = undefined_real
    noahmp%energy%state%MOLG            = undefined_real
    noahmp%energy%state%MOLV            = undefined_real
    noahmp%energy%state%MOLB            = undefined_real
    noahmp%energy%state%FHG             = undefined_real
    noahmp%energy%state%FMV             = undefined_real
    noahmp%energy%state%FHV             = undefined_real
    noahmp%energy%state%FM2V            = undefined_real
    noahmp%energy%state%FH2V            = undefined_real
    noahmp%energy%state%FMB             = undefined_real
    noahmp%energy%state%FHB             = undefined_real
    noahmp%energy%state%FM2B            = undefined_real
    noahmp%energy%state%FH2B            = undefined_real
    noahmp%energy%state%CM              = undefined_real
    noahmp%energy%state%CMV             = undefined_real
    noahmp%energy%state%CMB             = undefined_real
    noahmp%energy%state%CH              = undefined_real
    noahmp%energy%state%CHB             = undefined_real
    noahmp%energy%state%CHV             = undefined_real
    noahmp%energy%state%CHLEAF          = undefined_real
    noahmp%energy%state%CHUC            = undefined_real
    noahmp%energy%state%CH2V            = undefined_real
    noahmp%energy%state%CH2B            = undefined_real
    noahmp%energy%state%CHV2            = undefined_real
    noahmp%energy%state%EHB             = undefined_real
    noahmp%energy%state%EHB2            = undefined_real
    noahmp%energy%state%EMB             = undefined_real
    noahmp%energy%state%CAW             = undefined_real
    noahmp%energy%state%CTW             = undefined_real
    noahmp%energy%state%CEW             = undefined_real
    noahmp%energy%state%CGW             = undefined_real
    noahmp%energy%state%RAMG            = undefined_real
    noahmp%energy%state%RAHG            = undefined_real
    noahmp%energy%state%RAWG            = undefined_real
    noahmp%energy%state%RAMC            = undefined_real
    noahmp%energy%state%RAHC            = undefined_real
    noahmp%energy%state%RAWC            = undefined_real
    noahmp%energy%state%RAMB            = undefined_real
    noahmp%energy%state%RAHB            = undefined_real
    noahmp%energy%state%RAWB            = undefined_real
    noahmp%energy%state%RB              = undefined_real
    noahmp%energy%state%THAIR           = undefined_real
    noahmp%energy%state%UR              = undefined_real
    noahmp%energy%state%WSTARV          = undefined_real
    noahmp%energy%state%WSTARB          = undefined_real
    noahmp%energy%state%EMV             = undefined_real
    noahmp%energy%state%EMG             = undefined_real
    noahmp%energy%state%RSURF           = undefined_real
    noahmp%energy%state%GAMMAV          = undefined_real
    noahmp%energy%state%LATHEAV         = undefined_real
    noahmp%energy%state%GAMMAG          = undefined_real
    noahmp%energy%state%LATHEAG         = undefined_real
    noahmp%energy%state%RHSUR           = undefined_real
    noahmp%energy%state%QSFC            = undefined_real
    noahmp%energy%state%Q1              = undefined_real
    noahmp%energy%state%Q2V             = undefined_real
    noahmp%energy%state%Q2B             = undefined_real
    noahmp%energy%state%Q2E             = undefined_real
    noahmp%energy%state%TS              = undefined_real
    noahmp%energy%state%TG              = undefined_real
    noahmp%energy%state%TV              = undefined_real
    noahmp%energy%state%TGV             = undefined_real
    noahmp%energy%state%TGB             = undefined_real
    noahmp%energy%state%TROOT           = undefined_real
    noahmp%energy%state%TAUXV           = undefined_real
    noahmp%energy%state%TAUYV           = undefined_real
    noahmp%energy%state%TAUXB           = undefined_real
    noahmp%energy%state%TAUYB           = undefined_real
    noahmp%energy%state%TAUX            = undefined_real
    noahmp%energy%state%TAUY            = undefined_real
    noahmp%energy%state%T2MV            = undefined_real
    noahmp%energy%state%T2MB            = undefined_real
    noahmp%energy%state%T2M             = undefined_real
    noahmp%energy%state%FB_snow         = undefined_real
    noahmp%energy%state%ZBOTSNO         = undefined_real
    noahmp%energy%state%Z0WRF           = undefined_real
    noahmp%energy%state%TRAD            = undefined_real
    noahmp%energy%state%EMISSI          = undefined_real
    noahmp%energy%state%ALBEDO          = undefined_real
    noahmp%energy%state%ERRENG          = undefined_real
    noahmp%energy%state%ERRSW           = undefined_real
    noahmp%energy%state%RefHeightAboveGround            = undefined_real
 
    if( .not. allocated( noahmp%energy%state%STC      ) ) allocate( noahmp%energy%state%STC      (-NumSnowLayerMax+1:NumSoilLayer) )
    if( .not. allocated( noahmp%energy%state%DF       ) ) allocate( noahmp%energy%state%DF       (-NumSnowLayerMax+1:NumSoilLayer) )
    if( .not. allocated( noahmp%energy%state%HCPCT    ) ) allocate( noahmp%energy%state%HCPCT    (-NumSnowLayerMax+1:NumSoilLayer) )
    if( .not. allocated( noahmp%energy%state%FACT     ) ) allocate( noahmp%energy%state%FACT     (-NumSnowLayerMax+1:NumSoilLayer) )
    if( .not. allocated( noahmp%energy%state%CVSNO    ) ) allocate( noahmp%energy%state%CVSNO    (-NumSnowLayerMax+1:0    ) )
    if( .not. allocated( noahmp%energy%state%TKSNO    ) ) allocate( noahmp%energy%state%TKSNO    (-NumSnowLayerMax+1:0    ) )
    if( .not. allocated( noahmp%energy%state%CVSOIL   ) ) allocate( noahmp%energy%state%CVSOIL   (       1:NumSoilLayer) )
    if( .not. allocated( noahmp%energy%state%TKSOIL   ) ) allocate( noahmp%energy%state%TKSOIL   (       1:NumSoilLayer) )
    if( .not. allocated( noahmp%energy%state%CVGLAICE ) ) allocate( noahmp%energy%state%CVGLAICE (       1:NumSoilLayer) )
    if( .not. allocated( noahmp%energy%state%TKGLAICE ) ) allocate( noahmp%energy%state%TKGLAICE (       1:NumSoilLayer) )
    if( .not. allocated( noahmp%energy%state%ALBSND   ) ) allocate( noahmp%energy%state%ALBSND   (       1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%state%ALBSNI   ) ) allocate( noahmp%energy%state%ALBSNI   (       1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%state%ALBSOD   ) ) allocate( noahmp%energy%state%ALBSOD   (       1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%state%ALBSOI   ) ) allocate( noahmp%energy%state%ALBSOI   (       1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%state%ALBGRD   ) ) allocate( noahmp%energy%state%ALBGRD   (       1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%state%ALBGRI   ) ) allocate( noahmp%energy%state%ALBGRI   (       1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%state%RHO      ) ) allocate( noahmp%energy%state%RHO      (       1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%state%TAU      ) ) allocate( noahmp%energy%state%TAU      (       1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%state%ALBD     ) ) allocate( noahmp%energy%state%ALBD     (       1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%state%ALBI     ) ) allocate( noahmp%energy%state%ALBI     (       1:NumSWRadBand) )
    
    noahmp%energy%state%STC(:)          = undefined_real
    noahmp%energy%state%DF(:)           = undefined_real
    noahmp%energy%state%HCPCT(:)        = undefined_real
    noahmp%energy%state%FACT(:)         = undefined_real
    noahmp%energy%state%CVSNO(:)        = undefined_real
    noahmp%energy%state%TKSNO(:)        = undefined_real
    noahmp%energy%state%CVSOIL(:)       = undefined_real
    noahmp%energy%state%TKSOIL(:)       = undefined_real
    noahmp%energy%state%CVGLAICE(:)     = undefined_real
    noahmp%energy%state%TKGLAICE(:)     = undefined_real
    noahmp%energy%state%ALBSND(:)       = undefined_real
    noahmp%energy%state%ALBSNI(:)       = undefined_real
    noahmp%energy%state%ALBSOD(:)       = undefined_real
    noahmp%energy%state%ALBSOI(:)       = undefined_real
    noahmp%energy%state%ALBGRD(:)       = undefined_real
    noahmp%energy%state%ALBGRI(:)       = undefined_real
    noahmp%energy%state%RHO(:)          = undefined_real
    noahmp%energy%state%TAU(:)          = undefined_real
    noahmp%energy%state%ALBD(:)         = undefined_real
    noahmp%energy%state%ALBI(:)         = undefined_real
    
    ! energy flux variables
    noahmp%energy%flux%HeatLatentCanopy             = undefined_real
    noahmp%energy%flux%HeatLatentTransp             = undefined_real
    noahmp%energy%flux%HeatLatentGrdTot             = undefined_real
    noahmp%energy%flux%HeatPrecipAdvCanopy          = undefined_real
    noahmp%energy%flux%HeatPrecipAdvVegGrd          = undefined_real
    noahmp%energy%flux%HeatPrecipAdvBareGrd         = undefined_real
    noahmp%energy%flux%HeatPrecipAdvTot             = undefined_real
    noahmp%energy%flux%RadPhotoActAbsSunlit         = undefined_real
    noahmp%energy%flux%RadPhotoActAbsShade          = undefined_real
    noahmp%energy%flux%RadSwAbsVeg                  = undefined_real
    noahmp%energy%flux%RadSwAbsGrd                  = undefined_real
    noahmp%energy%flux%RadSwAbsTot                  = undefined_real
    noahmp%energy%flux%RadSwReflTot                 = undefined_real
    noahmp%energy%flux%RadSwReflVeg                 = undefined_real
    noahmp%energy%flux%RadSwReflGrd                 = undefined_real
    noahmp%energy%flux%RadLwNetCanopy               = undefined_real
    noahmp%energy%flux%HeatSensibleCanopy           = undefined_real
    noahmp%energy%flux%HeatLatentCanEvap            = undefined_real
    noahmp%energy%flux%RadLwNetVegGrd               = undefined_real
    noahmp%energy%flux%HeatSensibleVegGrd           = undefined_real
    noahmp%energy%flux%HeatLatentVegGrd             = undefined_real
    noahmp%energy%flux%HeatLatentCanTransp          = undefined_real
    noahmp%energy%flux%HeatGroundVegGrd             = undefined_real
    noahmp%energy%flux%RadLwNetBareGrd              = undefined_real
    noahmp%energy%flux%HeatSensibleBareGrd          = undefined_real
    noahmp%energy%flux%HeatLatentBareGrd            = undefined_real
    noahmp%energy%flux%HeatGroundBareGrd            = undefined_real
    noahmp%energy%flux%HeatGroundTot                = undefined_real
    noahmp%energy%flux%HeatFromSoilBot              = undefined_real
    noahmp%energy%flux%RadLwNetTot                  = undefined_real
    noahmp%energy%flux%HeatSensibleTot              = undefined_real
    noahmp%energy%flux%RadPhotoActAbsCan            = undefined_real
    noahmp%energy%flux%RadLwEmitTot                 = undefined_real
    noahmp%energy%flux%HeatLatentIrriEvap           = 0.0
 
    if ( .not. allocated(noahmp%energy%flux%RadSwAbsVegDir) )      &
       allocate( noahmp%energy%flux%RadSwAbsVegDir(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%flux%RadSwAbsVegDif) )      &
       allocate( noahmp%energy%flux%RadSwAbsVegDif(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%flux%RadSwDirTranGrdDir) )  &
       allocate( noahmp%energy%flux%RadSwDirTranGrdDir(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%flux%RadSwDirTranGrdDif) )  &
       allocate( noahmp%energy%flux%RadSwDirTranGrdDif(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%flux%RadSwDifTranGrdDir) )  &
       allocate( noahmp%energy%flux%RadSwDifTranGrdDir(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%flux%RadSwDifTranGrdDif) )  &
       allocate( noahmp%energy%flux%RadSwDifTranGrdDif(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%flux%RadSwReflVegDir) )     &
       allocate( noahmp%energy%flux%RadSwReflVegDir(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%flux%RadSwReflVegDif) )     &
       allocate( noahmp%energy%flux%RadSwReflVegDif(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%flux%RadSwReflGrdDir) )     &
       allocate( noahmp%energy%flux%RadSwReflGrdDir(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%flux%RadSwReflGrdDif) )     &
       allocate( noahmp%energy%flux%RadSwReflGrdDif(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%flux%RadSwDownDir) )        &
       allocate( noahmp%energy%flux%RadSwDownDir(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%flux%RadSwDownDif) )        &
       allocate( noahmp%energy%flux%RadSwDownDif(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%flux%RadSwPenetrateGrd) )   &
       allocate( noahmp%energy%flux%RadSwPenetrateGrd(-NumSnowLayerMax+1:NumSoilLayer) )
    
    noahmp%energy%flux%RadSwAbsVegDir    (:)        = undefined_real
    noahmp%energy%flux%RadSwAbsVegDif    (:)        = undefined_real
    noahmp%energy%flux%RadSwDirTranGrdDir(:)        = undefined_real
    noahmp%energy%flux%RadSwDirTranGrdDif(:)        = undefined_real
    noahmp%energy%flux%RadSwDifTranGrdDir(:)        = undefined_real
    noahmp%energy%flux%RadSwDifTranGrdDif(:)        = undefined_real
    noahmp%energy%flux%RadSwReflVegDir   (:)        = undefined_real
    noahmp%energy%flux%RadSwReflVegDif   (:)        = undefined_real
    noahmp%energy%flux%RadSwReflGrdDir   (:)        = undefined_real
    noahmp%energy%flux%RadSwReflGrdDif   (:)        = undefined_real
    noahmp%energy%flux%RadSwDownDir      (:)        = undefined_real
    noahmp%energy%flux%RadSwDownDif      (:)        = undefined_real
    noahmp%energy%flux%RadSwPenetrateGrd (:)        = undefined_real
    
    ! energy parameter variables
    noahmp%energy%param%TreeCrownRadius             = undefined_real
    noahmp%energy%param%HeightCanopyTop             = undefined_real
    noahmp%energy%param%HeightCanopyBot             = undefined_real
    noahmp%energy%param%RoughLenMomVeg              = undefined_real
    noahmp%energy%param%TreeDensity                 = undefined_real
    noahmp%energy%param%CanopyOrientIndex           = undefined_real
    noahmp%energy%param%UpscatterCoeffSnowDir       = undefined_real
    noahmp%energy%param%UpscatterCoeffSnowDif       = undefined_real
    noahmp%energy%param%SoilHeatCapacity            = undefined_real
    noahmp%energy%param%SnowAgeFacBats              = undefined_real
    noahmp%energy%param%SnowGrowVapFacBats          = undefined_real
    noahmp%energy%param%SnowSootFacBats             = undefined_real
    noahmp%energy%param%SnowGrowFrzFacBats          = undefined_real
    noahmp%energy%param%SolarZenithAdjBats          = undefined_real
    noahmp%energy%param%FreshSnoAlbVisBats          = undefined_real
    noahmp%energy%param%FreshSnoAlbNirBats          = undefined_real
    noahmp%energy%param%SnoAgeFacDifVisBats         = undefined_real
    noahmp%energy%param%SnoAgeFacDifNirBats         = undefined_real
    noahmp%energy%param%SzaFacDirVisBats            = undefined_real
    noahmp%energy%param%SzaFacDirNirBats            = undefined_real
    noahmp%energy%param%SnowAlbRefClass             = undefined_real
    noahmp%energy%param%SnowAgeFacClass             = undefined_real
    noahmp%energy%param%SnowAlbFreshClass           = undefined_real
    noahmp%energy%param%ConductanceLeafMin          = undefined_real
    noahmp%energy%param%Co2MmConst25C               = undefined_real
    noahmp%energy%param%O2MmConst25C                = undefined_real
    noahmp%energy%param%Co2MmConstQ10               = undefined_real
    noahmp%energy%param%O2MmConstQ10                = undefined_real
    noahmp%energy%param%RadiationStressFac          = undefined_real
    noahmp%energy%param%ResistanceStomataMin        = undefined_real
    noahmp%energy%param%ResistanceStomataMax        = undefined_real
    noahmp%energy%param%AirTempOptimTransp          = undefined_real
    noahmp%energy%param%VaporPresDeficitFac         = undefined_real
    noahmp%energy%param%LeafDimLength               = undefined_real
    noahmp%energy%param%ZilitinkevichCoeff          = undefined_real
    noahmp%energy%param%EmissivitySnow              = undefined_real
    noahmp%energy%param%CanopyWindExtFac            = undefined_real
    noahmp%energy%param%RoughLenMomSno              = undefined_real
    noahmp%energy%param%RoughLenMomSoil             = undefined_real
    noahmp%energy%param%RoughLenMomLake             = undefined_real
    noahmp%energy%param%EmissivityIceSfc            = undefined_real
    noahmp%energy%param%ResistanceSoilExp           = undefined_real
    noahmp%energy%param%ResistanceSnowSfc           = undefined_real
    noahmp%energy%param%VegFracAnnMax               = undefined_real
    noahmp%energy%param%VegFracGreen                = undefined_real
    
    if ( .not. allocated(noahmp%energy%param%LeafAreaIndexMon) )   &
       allocate( noahmp%energy%param%LeafAreaIndexMon(1:12) )
    if ( .not. allocated(noahmp%energy%param%StemAreaIndexMon) )   &
       allocate( noahmp%energy%param%StemAreaIndexMon(1:12) )      
    if ( .not. allocated(noahmp%energy%param%SoilQuartzFrac) )     &
       allocate( noahmp%energy%param%SoilQuartzFrac(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%energy%param%AlbedoSoilSat) )      &
       allocate( noahmp%energy%param%AlbedoSoilSat(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%param%AlbedoSoilDry) )      &
       allocate( noahmp%energy%param%AlbedoSoilDry(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%param%AlbedoLakeFrz) )      &
       allocate( noahmp%energy%param%AlbedoLakeFrz(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%param%ScatterCoeffSnow) )   &
       allocate( noahmp%energy%param%ScatterCoeffSnow(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%param%ReflectanceLeaf) )    &
       allocate( noahmp%energy%param%ReflectanceLeaf(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%param%ReflectanceStem) )    &
       allocate( noahmp%energy%param%ReflectanceStem(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%param%TransmittanceLeaf) )  &
       allocate( noahmp%energy%param%TransmittanceLeaf(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%param%TransmittanceStem) )  &
       allocate( noahmp%energy%param%TransmittanceStem(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%param%EmissivitySoilLake) ) &
       allocate( noahmp%energy%param%EmissivitySoilLake(1:2) )
    if ( .not. allocated(noahmp%energy%param%AlbedoLandIce) )      &
       allocate( noahmp%energy%param%AlbedoLandIce(1:NumSWRadBand) )
    
    noahmp%energy%param%LeafAreaIndexMon  (:)       = undefined_real
    noahmp%energy%param%StemAreaIndexMon  (:)       = undefined_real
    noahmp%energy%param%SoilQuartzFrac    (:)       = undefined_real
    noahmp%energy%param%AlbedoSoilSat     (:)       = undefined_real
    noahmp%energy%param%AlbedoSoilDry     (:)       = undefined_real
    noahmp%energy%param%AlbedoLakeFrz     (:)       = undefined_real
    noahmp%energy%param%ScatterCoeffSnow  (:)       = undefined_real
    noahmp%energy%param%ReflectanceLeaf   (:)       = undefined_real
    noahmp%energy%param%ReflectanceStem   (:)       = undefined_real
    noahmp%energy%param%TransmittanceLeaf (:)       = undefined_real
    noahmp%energy%param%TransmittanceStem (:)       = undefined_real
    noahmp%energy%param%EmissivitySoilLake(:)       = undefined_real
    noahmp%energy%param%AlbedoLandIce     (:)       = undefined_real
    
    end associate

  end subroutine EnergyVarInitDefault

!=== initialize with input data or table values

  subroutine EnergyVarInitTransfer(noahmp, NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout) :: NoahmpIO
    type(noahmp_type),   intent(inout) :: noahmp

    ! local loop index
    integer                          :: SoilLayerIndex

    associate(                                                         &
              I               => noahmp%config%domain%GridIndexI      ,&
              J               => noahmp%config%domain%GridIndexJ      ,&
              VegType         => noahmp%config%domain%VegType         ,&
              SoilType        => noahmp%config%domain%SoilType        ,&
              CropType        => noahmp%config%domain%CropType        ,&
              SoilColor       => noahmp%config%domain%SoilColor       ,&
              FlagUrban       => noahmp%config%domain%FlagUrban       ,&
              NumSnowLayerMax => noahmp%config%domain%NumSnowLayerMax ,&
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer    ,&
              NumSWRadBand    => noahmp%config%domain%NumSWRadBand     &
             )

    ! energy state variables
    noahmp%energy%state%LAI               = NoahmpIO%LAI    (I,J)
    noahmp%energy%state%SAI               = NoahmpIO%XSAIXY (I,J)
    noahmp%energy%state%QSFC              = NoahmpIO%QSFC   (I,J)
    noahmp%energy%state%TG                = NoahmpIO%TGXY   (I,J)
    noahmp%energy%state%TV                = NoahmpIO%TVXY   (I,J)
    noahmp%energy%state%STC(-NumSnowLayerMax+1:0)   = NoahmpIO%TSNOXY (I,-NumSnowLayerMax+1:0,J)
    noahmp%energy%state%STC(1:NumSoilLayer)      = NoahmpIO%TSLB   (I,1:NumSoilLayer,J)
    noahmp%energy%state%TAUSS             = NoahmpIO%TAUSSXY(I,J)
    noahmp%energy%state%ALBOLD            = NoahmpIO%ALBOLDXY(I,J)
    noahmp%energy%state%EAH               = NoahmpIO%EAHXY (I,J)
    noahmp%energy%state%TAH               = NoahmpIO%TAHXY (I,J)
    noahmp%energy%state%CH                = NoahmpIO%CHXY  (I,J) 
    noahmp%energy%state%CM                = NoahmpIO%CMXY  (I,J)
    noahmp%energy%state%CO2AIR            = NoahmpIO%CO2_TABLE * noahmp%forcing%PressureAirRefHeight
    noahmp%energy%state%O2AIR             = NoahmpIO%O2_TABLE * noahmp%forcing%PressureAirRefHeight

    ! energy parameter variables
    noahmp%energy%param%SoilHeatCapacity                  = NoahmpIO%CSOIL_TABLE
    noahmp%energy%param%SnowAgeFacBats                    = NoahmpIO%TAU0_TABLE
    noahmp%energy%param%SnowGrowVapFacBats                = NoahmpIO%GRAIN_GROWTH_TABLE
    noahmp%energy%param%SnowSootFacBats                   = NoahmpIO%DIRT_SOOT_TABLE
    noahmp%energy%param%SnowGrowFrzFacBats                = NoahmpIO%EXTRA_GROWTH_TABLE
    noahmp%energy%param%SolarZenithAdjBats                = NoahmpIO%BATS_COSZ_TABLE
    noahmp%energy%param%FreshSnoAlbVisBats                = NoahmpIO%BATS_VIS_NEW_TABLE
    noahmp%energy%param%FreshSnoAlbNirBats                = NoahmpIO%BATS_NIR_NEW_TABLE
    noahmp%energy%param%SnoAgeFacDifVisBats               = NoahmpIO%BATS_VIS_AGE_TABLE
    noahmp%energy%param%SnoAgeFacDifNirBats               = NoahmpIO%BATS_NIR_AGE_TABLE
    noahmp%energy%param%SzaFacDirVisBats                  = NoahmpIO%BATS_VIS_DIR_TABLE
    noahmp%energy%param%SzaFacDirNirBats                  = NoahmpIO%BATS_NIR_DIR_TABLE
    noahmp%energy%param%SnowAlbRefClass                   = NoahmpIO%CLASS_ALB_REF_TABLE
    noahmp%energy%param%SnowAgeFacClass                   = NoahmpIO%CLASS_SNO_AGE_TABLE
    noahmp%energy%param%SnowAlbFreshClass                 = NoahmpIO%CLASS_ALB_NEW_TABLE
    noahmp%energy%param%UpscatterCoeffSnowDir             = NoahmpIO%BETADS_TABLE
    noahmp%energy%param%UpscatterCoeffSnowDif             = NoahmpIO%BETAIS_TABLE
    noahmp%energy%param%ZilitinkevichCoeff                = NoahmpIO%CZIL_TABLE
    noahmp%energy%param%EmissivitySnow                    = NoahmpIO%SNOW_EMIS_TABLE
    noahmp%energy%param%EmissivitySoilLake                = NoahmpIO%EG_TABLE
    noahmp%energy%param%AlbedoLandIce                     = NoahmpIO%ALBICE_TABLE
    noahmp%energy%param%RoughLenMomSno                    = NoahmpIO%Z0SNO_TABLE
    noahmp%energy%param%RoughLenMomSoil                   = NoahmpIO%Z0SOIL_TABLE
    noahmp%energy%param%RoughLenMomLake                   = NoahmpIO%Z0LAKE_TABLE
    noahmp%energy%param%EmissivityIceSfc                  = NoahmpIO%EICE_TABLE
    noahmp%energy%param%ResistanceSoilExp                 = NoahmpIO%RSURF_EXP_TABLE
    noahmp%energy%param%ResistanceSnowSfc                 = NoahmpIO%RSURF_SNOW_TABLE
    noahmp%energy%param%VegFracAnnMax                     = NoahmpIO%GVFMAX(I,J) / 100.0
    noahmp%energy%param%VegFracGreen                      = NoahmpIO%VEGFRA(I,J) / 100.0
    noahmp%energy%param%TreeCrownRadius                   = NoahmpIO%RC_TABLE    (VegType)
    noahmp%energy%param%HeightCanopyTop                   = NoahmpIO%HVT_TABLE   (VegType)
    noahmp%energy%param%HeightCanopyBot                   = NoahmpIO%HVB_TABLE   (VegType)
    noahmp%energy%param%RoughLenMomVeg                    = NoahmpIO%Z0MVT_TABLE (VegType)
    noahmp%energy%param%CanopyWindExtFac                  = NoahmpIO%CWPVT_TABLE (VegType)
    noahmp%energy%param%TreeDensity                       = NoahmpIO%DEN_TABLE   (VegType)
    noahmp%energy%param%CanopyOrientIndex                 = NoahmpIO%XL_TABLE    (VegType)
    noahmp%energy%param%ConductanceLeafMin                = NoahmpIO%BP_TABLE    (VegType)
    noahmp%energy%param%Co2MmConst25C                     = NoahmpIO%KC25_TABLE  (VegType)
    noahmp%energy%param%O2MmConst25C                      = NoahmpIO%KO25_TABLE  (VegType)
    noahmp%energy%param%Co2MmConstQ10                     = NoahmpIO%AKC_TABLE   (VegType)
    noahmp%energy%param%O2MmConstQ10                      = NoahmpIO%AKO_TABLE   (VegType)
    noahmp%energy%param%RadiationStressFac                = NoahmpIO%RGL_TABLE   (VegType)
    noahmp%energy%param%ResistanceStomataMin              = NoahmpIO%RS_TABLE    (VegType)
    noahmp%energy%param%ResistanceStomataMax              = NoahmpIO%RSMAX_TABLE (VegType)
    noahmp%energy%param%AirTempOptimTransp                = NoahmpIO%TOPT_TABLE  (VegType)
    noahmp%energy%param%VaporPresDeficitFac               = NoahmpIO%HS_TABLE    (VegType)
    noahmp%energy%param%LeafDimLength                     = NoahmpIO%DLEAF_TABLE (VegType)
    noahmp%energy%param%LeafAreaIndexMon (1:12)           = NoahmpIO%LAIM_TABLE  (VegType,1:12)
    noahmp%energy%param%StemAreaIndexMon (1:12)           = NoahmpIO%SAIM_TABLE  (VegType,1:12)
    noahmp%energy%param%ReflectanceLeaf  (1:NumSWRadBand) = NoahmpIO%RHOL_TABLE  (VegType,1:NumSWRadBand)
    noahmp%energy%param%ReflectanceStem  (1:NumSWRadBand) = NoahmpIO%RHOS_TABLE  (VegType,1:NumSWRadBand)
    noahmp%energy%param%TransmittanceLeaf(1:NumSWRadBand) = NoahmpIO%TAUL_TABLE  (VegType,1:NumSWRadBand)
    noahmp%energy%param%TransmittanceStem(1:NumSWRadBand) = NoahmpIO%TAUS_TABLE  (VegType,1:NumSWRadBand)
    noahmp%energy%param%AlbedoSoilSat    (1:NumSWRadBand) = NoahmpIO%ALBSAT_TABLE(SoilColor,1:NumSWRadBand)
    noahmp%energy%param%AlbedoSoilDry    (1:NumSWRadBand) = NoahmpIO%ALBDRY_TABLE(SoilColor,1:NumSWRadBand)
    noahmp%energy%param%AlbedoLakeFrz    (1:NumSWRadBand) = NoahmpIO%ALBLAK_TABLE(1:NumSWRadBand)
    noahmp%energy%param%ScatterCoeffSnow (1:NumSWRadBand) = NoahmpIO%OMEGAS_TABLE(1:NumSWRadBand)

    do SoilLayerIndex = 1, size(SoilType)
       noahmp%energy%param%SoilQuartzFrac(SoilLayerIndex) = NoahmpIO%QUARTZ_TABLE(SoilType(SoilLayerIndex))
    enddo

    if ( FlagUrban .eqv. .true. ) noahmp%energy%param%SoilHeatCapacity = 3.0e6

    if ( CropType > 0 ) then
       noahmp%energy%param%ConductanceLeafMin             = NoahmpIO%BPI_TABLE  (CropType)
       noahmp%energy%param%Co2MmConst25C                  = NoahmpIO%KC25I_TABLE(CropType)
       noahmp%energy%param%O2MmConst25C                   = NoahmpIO%KO25I_TABLE(CropType)
       noahmp%energy%param%Co2MmConstQ10                  = NoahmpIO%AKCI_TABLE (CropType)
       noahmp%energy%param%O2MmConstQ10                   = NoahmpIO%AKOI_TABLE (CropType)
    endif

    end associate

  end subroutine EnergyVarInitTransfer

end module EnergyVarInitMod
