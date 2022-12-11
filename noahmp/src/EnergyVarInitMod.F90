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
    noahmp%energy%state%FlagFrozenCanopy            = .false.
    noahmp%energy%state%FlagFrozenGround            = .false.
    noahmp%energy%state%LeafAreaIndEff              = undefined_real 
    noahmp%energy%state%StemAreaIndEff              = undefined_real
    noahmp%energy%state%LeafAreaIndex               = undefined_real
    noahmp%energy%state%StemAreaIndex               = undefined_real
    noahmp%energy%state%VegAreaIndEff               = undefined_real
    noahmp%energy%state%VegFrac                     = undefined_real
    noahmp%energy%state%PressureVaporRefHeight      = undefined_real
    noahmp%energy%state%SnowAgeFac                  = undefined_real
    noahmp%energy%state%SnowAgeNondim               = undefined_real
    noahmp%energy%state%AlbedoSnowPrev              = undefined_real
    noahmp%energy%state%VegAreaProjDir              = undefined_real
    noahmp%energy%state%GapBtwCanopy                = undefined_real
    noahmp%energy%state%GapInCanopy                 = undefined_real
    noahmp%energy%state%GapCanopyDif                = undefined_real
    noahmp%energy%state%GapCanopyDir                = undefined_real
    noahmp%energy%state%CanopySunlitFrac            = undefined_real
    noahmp%energy%state%CanopyShadeFrac             = undefined_real
    noahmp%energy%state%LeafAreaIndSunlit           = undefined_real
    noahmp%energy%state%LeafAreaIndShade            = undefined_real
    noahmp%energy%state%VapPresSatCanopy            = undefined_real
    noahmp%energy%state%VapPresSatGrdVeg            = undefined_real
    noahmp%energy%state%VapPresSatGrdBare           = undefined_real
    noahmp%energy%state%VapPresSatCanTempD          = undefined_real
    noahmp%energy%state%VapPresSatGrdVegTempD       = undefined_real
    noahmp%energy%state%VapPresSatGrdBareTempD      = undefined_real
    noahmp%energy%state%PressureVaporCanAir         = undefined_real
    noahmp%energy%state%PressureAtmosCO2            = undefined_real
    noahmp%energy%state%PressureAtmosO2             = undefined_real
    noahmp%energy%state%ResistanceStomataSunlit     = undefined_real
    noahmp%energy%state%ResistanceStomataShade      = undefined_real
    noahmp%energy%state%DensityAirRefHeight         = undefined_real
    noahmp%energy%state%TemperatureCanopyAir        = undefined_real
    noahmp%energy%state%ZeroPlaneDispSfc            = undefined_real
    noahmp%energy%state%ZeroPlaneDispGrd            = undefined_real
    noahmp%energy%state%RoughLenMomGrd              = undefined_real
    noahmp%energy%state%RoughLenMomSfc              = undefined_real
    noahmp%energy%state%CanopyHeight                = undefined_real
    noahmp%energy%state%WindSpdCanopyTop            = undefined_real
    noahmp%energy%state%RoughLenShCanopy            = undefined_real
    noahmp%energy%state%RoughLenShVegGrd            = undefined_real
    noahmp%energy%state%RoughLenShBareGrd           = undefined_real
    noahmp%energy%state%FrictionVelVeg              = undefined_real
    noahmp%energy%state%FrictionVelBare             = undefined_real
    noahmp%energy%state%WindExtCoeffCanopy          = undefined_real
    noahmp%energy%state%MoStabParaUndCan            = undefined_real
    noahmp%energy%state%MoStabParaAbvCan            = undefined_real
    noahmp%energy%state%MoStabParaBare              = undefined_real
    noahmp%energy%state%MoStabParaVeg2m             = undefined_real
    noahmp%energy%state%MoStabParaBare2m            = undefined_real
    noahmp%energy%state%MoLengthUndCan              = undefined_real
    noahmp%energy%state%MoLengthAbvCan              = undefined_real
    noahmp%energy%state%MoLengthBare                = undefined_real
    noahmp%energy%state%MoStabCorrShUndCan          = undefined_real
    noahmp%energy%state%MoStabCorrMomAbvCan         = undefined_real
    noahmp%energy%state%MoStabCorrShAbvCan          = undefined_real
    noahmp%energy%state%MoStabCorrMomVeg2m          = undefined_real
    noahmp%energy%state%MoStabCorrShVeg2m           = undefined_real
    noahmp%energy%state%MoStabCorrShBare            = undefined_real
    noahmp%energy%state%MoStabCorrMomBare           = undefined_real
    noahmp%energy%state%MoStabCorrMomBare2m         = undefined_real
    noahmp%energy%state%MoStabCorrShBare2m          = undefined_real
    noahmp%energy%state%ExchCoeffMomSfc             = undefined_real
    noahmp%energy%state%ExchCoeffMomAbvCan          = undefined_real
    noahmp%energy%state%ExchCoeffMomBare            = undefined_real
    noahmp%energy%state%ExchCoeffShSfc              = undefined_real
    noahmp%energy%state%ExchCoeffShBare             = undefined_real
    noahmp%energy%state%ExchCoeffShAbvCan           = undefined_real
    noahmp%energy%state%ExchCoeffShLeaf             = undefined_real
    noahmp%energy%state%ExchCoeffShUndCan           = undefined_real
    noahmp%energy%state%ExchCoeffSh2mVegMo          = undefined_real
    noahmp%energy%state%ExchCoeffSh2mBareMo         = undefined_real
    noahmp%energy%state%ExchCoeffSh2mVeg            = undefined_real
    noahmp%energy%state%ExchCoeffSh2mBare           = undefined_real
    noahmp%energy%state%ExchCoeffLhAbvCan           = undefined_real
    noahmp%energy%state%ExchCoeffLhTransp           = undefined_real
    noahmp%energy%state%ExchCoeffLhEvap             = undefined_real
    noahmp%energy%state%ExchCoeffLhUndCan           = undefined_real
    noahmp%energy%state%ResistanceMomUndCan         = undefined_real
    noahmp%energy%state%ResistanceShUndCan          = undefined_real
    noahmp%energy%state%ResistanceLhUndCan          = undefined_real
    noahmp%energy%state%ResistanceMomAbvCan         = undefined_real
    noahmp%energy%state%ResistanceShAbvCan          = undefined_real
    noahmp%energy%state%ResistanceLhAbvCan          = undefined_real
    noahmp%energy%state%ResistanceMomBareGrd        = undefined_real
    noahmp%energy%state%ResistanceShBareGrd         = undefined_real
    noahmp%energy%state%ResistanceLhBareGrd         = undefined_real
    noahmp%energy%state%ResistanceLeafBoundary      = undefined_real
    noahmp%energy%state%TemperaturePotRefHeight     = undefined_real
    noahmp%energy%state%WindSpdRefHeight            = undefined_real
    noahmp%energy%state%FrictionVelVertVeg          = undefined_real
    noahmp%energy%state%FrictionVelVertBare         = undefined_real
    noahmp%energy%state%EmissivityVeg               = undefined_real
    noahmp%energy%state%EmissivityGrd               = undefined_real
    noahmp%energy%state%ResistanceGrdEvap           = undefined_real
    noahmp%energy%state%PsychConstCanopy            = undefined_real
    noahmp%energy%state%LatHeatVapCanopy            = undefined_real
    noahmp%energy%state%PsychConstGrd               = undefined_real
    noahmp%energy%state%LatHeatVapGrd               = undefined_real
    noahmp%energy%state%RelHumidityGrd              = undefined_real
    noahmp%energy%state%SpecHumiditySfcBare         = undefined_real
    noahmp%energy%state%SpecHumiditySfc             = undefined_real
    noahmp%energy%state%SpecHumidity2mVeg           = undefined_real
    noahmp%energy%state%SpecHumidity2mBare          = undefined_real
    noahmp%energy%state%SpecHumidity2m              = undefined_real
    noahmp%energy%state%TemperatureSfc              = undefined_real
    noahmp%energy%state%TemperatureGrd              = undefined_real
    noahmp%energy%state%TemperatureCanopy           = undefined_real
    noahmp%energy%state%TemperatureGrdVeg           = undefined_real
    noahmp%energy%state%TemperatureGrdBare          = undefined_real
    noahmp%energy%state%TemperatureRootZone         = undefined_real
    noahmp%energy%state%WindStressEwVeg             = undefined_real
    noahmp%energy%state%WindStressNsVeg             = undefined_real
    noahmp%energy%state%WindStressEwBare            = undefined_real
    noahmp%energy%state%WindStressNsBare            = undefined_real
    noahmp%energy%state%WindStressEwSfc             = undefined_real
    noahmp%energy%state%WindStressNsSfc             = undefined_real
    noahmp%energy%state%TemperatureAir2mVeg         = undefined_real
    noahmp%energy%state%TemperatureAir2mBare        = undefined_real
    noahmp%energy%state%TemperatureAir2m            = undefined_real
    noahmp%energy%state%CanopyFracSnowBury          = undefined_real
    noahmp%energy%state%DepthSoilTempBotToSno       = undefined_real
    noahmp%energy%state%RoughLenMomSfcToAtm         = undefined_real
    noahmp%energy%state%TemperatureRadSfc           = undefined_real
    noahmp%energy%state%EmissivitySfc               = undefined_real
    noahmp%energy%state%AlbedoSfc                   = undefined_real
    noahmp%energy%state%EnergyBalanceError          = undefined_real
    noahmp%energy%state%RadSwBalanceError           = undefined_real
    noahmp%energy%state%RefHeightAboveGrd           = undefined_real
 
    if ( .not. allocated(noahmp%energy%state%TemperatureSoilSnow) )  &
       allocate( noahmp%energy%state%TemperatureSoilSnow(-NumSnowLayerMax+1:NumSoilLayer) )
    if ( .not. allocated(noahmp%energy%state%ThermConductSoilSnow) ) &
       allocate( noahmp%energy%state%ThermConductSoilSnow(-NumSnowLayerMax+1:NumSoilLayer) )
    if ( .not. allocated(noahmp%energy%state%HeatCapacSoilSnow) )    &
       allocate( noahmp%energy%state%HeatCapacSoilSnow(-NumSnowLayerMax+1:NumSoilLayer) )
    if ( .not. allocated(noahmp%energy%state%PhaseChgFacSoilSnow) )  &
       allocate( noahmp%energy%state%PhaseChgFacSoilSnow(-NumSnowLayerMax+1:NumSoilLayer) )
    if ( .not. allocated(noahmp%energy%state%HeatCapacVolSnow) )     &
       allocate( noahmp%energy%state%HeatCapacVolSnow(-NumSnowLayerMax+1:0) )
    if ( .not. allocated(noahmp%energy%state%ThermConductSnow) )     &
       allocate( noahmp%energy%state%ThermConductSnow(-NumSnowLayerMax+1:0) )
    if ( .not. allocated(noahmp%energy%state%HeatCapacVolSoil) )     &
       allocate( noahmp%energy%state%HeatCapacVolSoil(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%energy%state%ThermConductSoil) )     &
       allocate( noahmp%energy%state%ThermConductSoil(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%energy%state%HeatCapacGlaIce) )      &
       allocate( noahmp%energy%state%HeatCapacGlaIce(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%energy%state%ThermConductGlaIce) )   &
       allocate( noahmp%energy%state%ThermConductGlaIce(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%energy%state%AlbedoSnowDir) )        &
       allocate( noahmp%energy%state%AlbedoSnowDir(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%state%AlbedoSnowDif) )        &
       allocate( noahmp%energy%state%AlbedoSnowDif(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%state%AlbedoSoilDir) )        &
       allocate( noahmp%energy%state%AlbedoSoilDir(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%state%AlbedoSoilDif) )        &
       allocate( noahmp%energy%state%AlbedoSoilDif(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%state%AlbedoGrdDir) )         &
       allocate( noahmp%energy%state%AlbedoGrdDir(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%state%AlbedoGrdDif) )         &
       allocate( noahmp%energy%state%AlbedoGrdDif(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%state%ReflectanceVeg) )       &
       allocate( noahmp%energy%state%ReflectanceVeg(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%state%TransmittanceVeg) )     &
       allocate( noahmp%energy%state%TransmittanceVeg(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%state%AlbedoSfcDir) )         &
       allocate( noahmp%energy%state%AlbedoSfcDir(1:NumSWRadBand) )
    if ( .not. allocated(noahmp%energy%state%AlbedoSfcDif) )         &
       allocate( noahmp%energy%state%AlbedoSfcDif(1:NumSWRadBand) )
 
    noahmp%energy%state%TemperatureSoilSnow (:)     = undefined_real
    noahmp%energy%state%ThermConductSoilSnow(:)     = undefined_real
    noahmp%energy%state%HeatCapacSoilSnow   (:)     = undefined_real
    noahmp%energy%state%PhaseChgFacSoilSnow (:)     = undefined_real
    noahmp%energy%state%HeatCapacVolSnow    (:)     = undefined_real
    noahmp%energy%state%ThermConductSnow    (:)     = undefined_real
    noahmp%energy%state%HeatCapacVolSoil    (:)     = undefined_real
    noahmp%energy%state%ThermConductSoil    (:)     = undefined_real
    noahmp%energy%state%HeatCapacGlaIce     (:)     = undefined_real
    noahmp%energy%state%ThermConductGlaIce  (:)     = undefined_real
    noahmp%energy%state%AlbedoSnowDir       (:)     = undefined_real
    noahmp%energy%state%AlbedoSnowDif       (:)     = undefined_real
    noahmp%energy%state%AlbedoSoilDir       (:)     = undefined_real
    noahmp%energy%state%AlbedoSoilDif       (:)     = undefined_real
    noahmp%energy%state%AlbedoGrdDir        (:)     = undefined_real
    noahmp%energy%state%AlbedoGrdDif        (:)     = undefined_real
    noahmp%energy%state%ReflectanceVeg      (:)     = undefined_real
    noahmp%energy%state%TransmittanceVeg    (:)     = undefined_real
    noahmp%energy%state%AlbedoSfcDir        (:)     = undefined_real
    noahmp%energy%state%AlbedoSfcDif        (:)     = undefined_real
    
    ! energy flux variables
    noahmp%energy%flux%HeatLatentCanopy             = undefined_real
    noahmp%energy%flux%HeatLatentTransp             = undefined_real
    noahmp%energy%flux%HeatLatentGrd                = undefined_real
    noahmp%energy%flux%HeatPrecipAdvCanopy          = undefined_real
    noahmp%energy%flux%HeatPrecipAdvVegGrd          = undefined_real
    noahmp%energy%flux%HeatPrecipAdvBareGrd         = undefined_real
    noahmp%energy%flux%HeatPrecipAdvSfc             = undefined_real
    noahmp%energy%flux%RadPhotoActAbsSunlit         = undefined_real
    noahmp%energy%flux%RadPhotoActAbsShade          = undefined_real
    noahmp%energy%flux%RadSwAbsVeg                  = undefined_real
    noahmp%energy%flux%RadSwAbsGrd                  = undefined_real
    noahmp%energy%flux%RadSwAbsSfc                  = undefined_real
    noahmp%energy%flux%RadSwReflSfc                 = undefined_real
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
    noahmp%energy%flux%RadLwNetSfc                  = undefined_real
    noahmp%energy%flux%HeatSensibleSfc              = undefined_real
    noahmp%energy%flux%RadPhotoActAbsCan            = undefined_real
    noahmp%energy%flux%RadLwEmitSfc                 = undefined_real
    noahmp%energy%flux%HeatCanStorageChg            = undefined_real
    noahmp%energy%flux%HeatGroundTotAcc             = undefined_real
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
    noahmp%energy%param%RoughLenMomSnow             = undefined_real
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
    noahmp%energy%state%LeafAreaIndex                             = NoahmpIO%LAI     (I,J)
    noahmp%energy%state%StemAreaIndex                             = NoahmpIO%XSAIXY  (I,J)
    noahmp%energy%state%SpecHumiditySfcBare                       = NoahmpIO%QSFC    (I,J)
    noahmp%energy%state%TemperatureGrd                            = NoahmpIO%TGXY    (I,J)
    noahmp%energy%state%TemperatureCanopy                         = NoahmpIO%TVXY    (I,J)
    noahmp%energy%state%SnowAgeNondim                             = NoahmpIO%TAUSSXY (I,J)
    noahmp%energy%state%AlbedoSnowPrev                            = NoahmpIO%ALBOLDXY(I,J)
    noahmp%energy%state%PressureVaporCanAir                       = NoahmpIO%EAHXY   (I,J)
    noahmp%energy%state%TemperatureCanopyAir                      = NoahmpIO%TAHXY   (I,J)
    noahmp%energy%state%ExchCoeffShSfc                            = NoahmpIO%CHXY    (I,J) 
    noahmp%energy%state%ExchCoeffMomSfc                           = NoahmpIO%CMXY    (I,J)
    noahmp%energy%state%TemperatureSoilSnow(-NumSnowLayerMax+1:0) = NoahmpIO%TSNOXY  (I,-NumSnowLayerMax+1:0,J)
    noahmp%energy%state%TemperatureSoilSnow(1:NumSoilLayer)       = NoahmpIO%TSLB    (I,1:NumSoilLayer,J)
    noahmp%energy%state%PressureAtmosCO2                          = NoahmpIO%CO2_TABLE * noahmp%forcing%PressureAirRefHeight
    noahmp%energy%state%PressureAtmosO2                           = NoahmpIO%O2_TABLE  * noahmp%forcing%PressureAirRefHeight
    ! vegetation treatment for USGS land types (playa, lava, sand to bare)
    if ( (VegType == 25) .or. (VegType == 26) .or. (VegType == 27) ) then
       noahmp%energy%state%VegFrac       = 0.0
       noahmp%energy%state%LeafAreaIndex = 0.0
    endif

    ! energy flux variables
    noahmp%energy%flux%HeatGroundTotAcc                           = NoahmpIO%ACC_SSOILXY(I,J)

    ! energy parameter variables
    noahmp%energy%param%SoilHeatCapacity                          = NoahmpIO%CSOIL_TABLE
    noahmp%energy%param%SnowAgeFacBats                            = NoahmpIO%TAU0_TABLE
    noahmp%energy%param%SnowGrowVapFacBats                        = NoahmpIO%GRAIN_GROWTH_TABLE
    noahmp%energy%param%SnowSootFacBats                           = NoahmpIO%DIRT_SOOT_TABLE
    noahmp%energy%param%SnowGrowFrzFacBats                        = NoahmpIO%EXTRA_GROWTH_TABLE
    noahmp%energy%param%SolarZenithAdjBats                        = NoahmpIO%BATS_COSZ_TABLE
    noahmp%energy%param%FreshSnoAlbVisBats                        = NoahmpIO%BATS_VIS_NEW_TABLE
    noahmp%energy%param%FreshSnoAlbNirBats                        = NoahmpIO%BATS_NIR_NEW_TABLE
    noahmp%energy%param%SnoAgeFacDifVisBats                       = NoahmpIO%BATS_VIS_AGE_TABLE
    noahmp%energy%param%SnoAgeFacDifNirBats                       = NoahmpIO%BATS_NIR_AGE_TABLE
    noahmp%energy%param%SzaFacDirVisBats                          = NoahmpIO%BATS_VIS_DIR_TABLE
    noahmp%energy%param%SzaFacDirNirBats                          = NoahmpIO%BATS_NIR_DIR_TABLE
    noahmp%energy%param%SnowAlbRefClass                           = NoahmpIO%CLASS_ALB_REF_TABLE
    noahmp%energy%param%SnowAgeFacClass                           = NoahmpIO%CLASS_SNO_AGE_TABLE
    noahmp%energy%param%SnowAlbFreshClass                         = NoahmpIO%CLASS_ALB_NEW_TABLE
    noahmp%energy%param%UpscatterCoeffSnowDir                     = NoahmpIO%BETADS_TABLE
    noahmp%energy%param%UpscatterCoeffSnowDif                     = NoahmpIO%BETAIS_TABLE
    noahmp%energy%param%ZilitinkevichCoeff                        = NoahmpIO%CZIL_TABLE
    noahmp%energy%param%EmissivitySnow                            = NoahmpIO%SNOW_EMIS_TABLE
    noahmp%energy%param%EmissivitySoilLake                        = NoahmpIO%EG_TABLE
    noahmp%energy%param%AlbedoLandIce                             = NoahmpIO%ALBICE_TABLE
    noahmp%energy%param%RoughLenMomSnow                           = NoahmpIO%Z0SNO_TABLE
    noahmp%energy%param%RoughLenMomSoil                           = NoahmpIO%Z0SOIL_TABLE
    noahmp%energy%param%RoughLenMomLake                           = NoahmpIO%Z0LAKE_TABLE
    noahmp%energy%param%EmissivityIceSfc                          = NoahmpIO%EICE_TABLE
    noahmp%energy%param%ResistanceSoilExp                         = NoahmpIO%RSURF_EXP_TABLE
    noahmp%energy%param%ResistanceSnowSfc                         = NoahmpIO%RSURF_SNOW_TABLE
    noahmp%energy%param%VegFracAnnMax                             = NoahmpIO%GVFMAX(I,J) / 100.0
    noahmp%energy%param%VegFracGreen                              = NoahmpIO%VEGFRA(I,J) / 100.0
    noahmp%energy%param%TreeCrownRadius                           = NoahmpIO%RC_TABLE    (VegType)
    noahmp%energy%param%HeightCanopyTop                           = NoahmpIO%HVT_TABLE   (VegType)
    noahmp%energy%param%HeightCanopyBot                           = NoahmpIO%HVB_TABLE   (VegType)
    noahmp%energy%param%RoughLenMomVeg                            = NoahmpIO%Z0MVT_TABLE (VegType)
    noahmp%energy%param%CanopyWindExtFac                          = NoahmpIO%CWPVT_TABLE (VegType)
    noahmp%energy%param%TreeDensity                               = NoahmpIO%DEN_TABLE   (VegType)
    noahmp%energy%param%CanopyOrientIndex                         = NoahmpIO%XL_TABLE    (VegType)
    noahmp%energy%param%ConductanceLeafMin                        = NoahmpIO%BP_TABLE    (VegType)
    noahmp%energy%param%Co2MmConst25C                             = NoahmpIO%KC25_TABLE  (VegType)
    noahmp%energy%param%O2MmConst25C                              = NoahmpIO%KO25_TABLE  (VegType)
    noahmp%energy%param%Co2MmConstQ10                             = NoahmpIO%AKC_TABLE   (VegType)
    noahmp%energy%param%O2MmConstQ10                              = NoahmpIO%AKO_TABLE   (VegType)
    noahmp%energy%param%RadiationStressFac                        = NoahmpIO%RGL_TABLE   (VegType)
    noahmp%energy%param%ResistanceStomataMin                      = NoahmpIO%RS_TABLE    (VegType)
    noahmp%energy%param%ResistanceStomataMax                      = NoahmpIO%RSMAX_TABLE (VegType)
    noahmp%energy%param%AirTempOptimTransp                        = NoahmpIO%TOPT_TABLE  (VegType)
    noahmp%energy%param%VaporPresDeficitFac                       = NoahmpIO%HS_TABLE    (VegType)
    noahmp%energy%param%LeafDimLength                             = NoahmpIO%DLEAF_TABLE (VegType)
    noahmp%energy%param%LeafAreaIndexMon (1:12)                   = NoahmpIO%LAIM_TABLE  (VegType,1:12)
    noahmp%energy%param%StemAreaIndexMon (1:12)                   = NoahmpIO%SAIM_TABLE  (VegType,1:12)
    noahmp%energy%param%ReflectanceLeaf  (1:NumSWRadBand)         = NoahmpIO%RHOL_TABLE  (VegType,1:NumSWRadBand)
    noahmp%energy%param%ReflectanceStem  (1:NumSWRadBand)         = NoahmpIO%RHOS_TABLE  (VegType,1:NumSWRadBand)
    noahmp%energy%param%TransmittanceLeaf(1:NumSWRadBand)         = NoahmpIO%TAUL_TABLE  (VegType,1:NumSWRadBand)
    noahmp%energy%param%TransmittanceStem(1:NumSWRadBand)         = NoahmpIO%TAUS_TABLE  (VegType,1:NumSWRadBand)
    noahmp%energy%param%AlbedoSoilSat    (1:NumSWRadBand)         = NoahmpIO%ALBSAT_TABLE(SoilColor,1:NumSWRadBand)
    noahmp%energy%param%AlbedoSoilDry    (1:NumSWRadBand)         = NoahmpIO%ALBDRY_TABLE(SoilColor,1:NumSWRadBand)
    noahmp%energy%param%AlbedoLakeFrz    (1:NumSWRadBand)         = NoahmpIO%ALBLAK_TABLE(1:NumSWRadBand)
    noahmp%energy%param%ScatterCoeffSnow (1:NumSWRadBand)         = NoahmpIO%OMEGAS_TABLE(1:NumSWRadBand)

    do SoilLayerIndex = 1, size(SoilType)
       noahmp%energy%param%SoilQuartzFrac(SoilLayerIndex)         = NoahmpIO%QUARTZ_TABLE(SoilType(SoilLayerIndex))
    enddo

    if ( FlagUrban .eqv. .true. ) noahmp%energy%param%SoilHeatCapacity = 3.0e6

    if ( CropType > 0 ) then
       noahmp%energy%param%ConductanceLeafMin                     = NoahmpIO%BPI_TABLE  (CropType)
       noahmp%energy%param%Co2MmConst25C                          = NoahmpIO%KC25I_TABLE(CropType)
       noahmp%energy%param%O2MmConst25C                           = NoahmpIO%KO25I_TABLE(CropType)
       noahmp%energy%param%Co2MmConstQ10                          = NoahmpIO%AKCI_TABLE (CropType)
       noahmp%energy%param%O2MmConstQ10                           = NoahmpIO%AKOI_TABLE (CropType)
    endif

    end associate

  end subroutine EnergyVarInitTransfer

end module EnergyVarInitMod
