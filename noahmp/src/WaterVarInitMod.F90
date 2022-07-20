module WaterVarInitMod

!!! Initialize column (1-D) Noah-MP water variables
!!! Water variables should be first defined in WaterVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (July 2022)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use NoahmpVarType
  use PedoTransferSR2006

  implicit none

contains

!=== initialize with default values
  subroutine WaterVarInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    associate(                                                          &
              NumSnowLayerMax  => noahmp%config%domain%NumSnowLayerMax ,&
              NumSoilLayer     => noahmp%config%domain%NumSoilLayer     &
             )

    ! water state variables
    noahmp%water%state%IrrigationCntSprinkler      = undefined_int
    noahmp%water%state%IrrigationCntMicro          = undefined_int
    noahmp%water%state%IrrigationCntFlood          = undefined_int
    noahmp%water%state%IrrigationFracFlood         = undefined_real
    noahmp%water%state%IrrigationAmtFlood          = undefined_real
    noahmp%water%state%IrrigationFracMicro         = undefined_real
    noahmp%water%state%IrrigationAmtMicro          = undefined_real
    noahmp%water%state%IrrigationFracSprinkler     = undefined_real
    noahmp%water%state%IrrigationAmtSprinkler      = undefined_real
    noahmp%water%state%IrrigationFracGrid          = undefined_real
    noahmp%water%state%CanopyLiqWater              = undefined_real
    noahmp%water%state%CanopyIce                   = undefined_real
    noahmp%water%state%CanopyTotalWater            = undefined_real
    noahmp%water%state%CanopyWetFrac               = undefined_real
    noahmp%water%state%CanopyIceMax                = undefined_real
    noahmp%water%state%CanopyLiqWaterMax           = undefined_real
    noahmp%water%state%SnowfallDensity             = undefined_real
    noahmp%water%state%SnowDepth                   = undefined_real
    noahmp%water%state%SnowWaterEquiv              = undefined_real
    noahmp%water%state%SnowWaterEquivPrev          = undefined_real
    noahmp%water%state%SnowCoverFrac               = undefined_real
    noahmp%water%state%PondSfcThinSnwMelt          = undefined_real
    noahmp%water%state%PondSfcThinSnwComb          = undefined_real
    noahmp%water%state%PondSfcThinSnwTrans         = undefined_real
    noahmp%water%state%SoilIceMax                  = undefined_real
    noahmp%water%state%SoilLiqWaterMin             = undefined_real
    noahmp%water%state%SoilSaturateFrac            = undefined_real
    noahmp%water%state%SoilImpervFracMax           = undefined_real
    noahmp%water%state%SoilMoistureToWT            = undefined_real
    noahmp%water%state%SoilTranspFacAcc            = undefined_real
    noahmp%water%state%SoilWaterRootZone           = undefined_real
    noahmp%water%state%SoilWaterStress             = undefined_real
    noahmp%water%state%SoilSaturationExcess        = undefined_real
    noahmp%water%state%RechargeGwDeepWT            = undefined_real
    noahmp%water%state%RechargeGwShallowWT         = undefined_real
    noahmp%water%state%WaterTableHydro             = undefined_real
    noahmp%water%state%WaterTableDepth             = undefined_real
    noahmp%water%state%WaterStorageAquifer         = undefined_real
    noahmp%water%state%WaterStorageSoilAqf         = undefined_real
    noahmp%water%state%WaterStorageLake            = undefined_real
    noahmp%water%state%WaterStorageTotBeg          = undefined_real
    noahmp%water%state%WaterBalanceError           = undefined_real
    noahmp%water%state%WaterStorageTotEnd          = undefined_real
    noahmp%water%state%WaterHeadSfc                = undefined_real
    noahmp%water%state%PrecipAreaFrac              = undefined_real
    noahmp%water%state%TileDrainFrac               = undefined_real
    noahmp%water%state%FrozenPrecipFrac            = undefined_real

    if ( .not. allocated(noahmp%water%state%IndexPhaseChange) )     &
       allocate( noahmp%water%state%IndexPhaseChange(-NumSnowLayerMax+1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%state%SoilSupercoolWater) )   &
       allocate( noahmp%water%state%SoilSupercoolWater(-NumSnowLayerMax+1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%state%SnowIce) )              &
       allocate( noahmp%water%state%SnowIce(-NumSnowLayerMax+1:0) )
    if ( .not. allocated(noahmp%water%state%SnowLiqWater) )         &
       allocate( noahmp%water%state%SnowLiqWater(-NumSnowLayerMax+1:0) )
    if ( .not. allocated(noahmp%water%state%SnowIceVol) )           &
       allocate( noahmp%water%state%SnowIceVol(-NumSnowLayerMax+1:0) )
    if ( .not. allocated(noahmp%water%state%SnowLiqWaterVol) )      &
       allocate( noahmp%water%state%SnowLiqWaterVol(-NumSnowLayerMax+1:0) )
    if ( .not. allocated(noahmp%water%state%SnowIceFracPrev) )      &
       allocate( noahmp%water%state%SnowIceFracPrev(-NumSnowLayerMax+1:0) )
    if ( .not. allocated(noahmp%water%state%SnowIceFrac) )          &
       allocate( noahmp%water%state%SnowIceFrac(-NumSnowLayerMax+1:0) )
    if ( .not. allocated(noahmp%water%state%SnowEffPorosity) )      &
       allocate( noahmp%water%state%SnowEffPorosity(-NumSnowLayerMax+1:0) )
    if ( .not. allocated(noahmp%water%state%SoilLiqWater) )         &
       allocate( noahmp%water%state%SoilLiqWater(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%state%SoilIce) )              &
       allocate( noahmp%water%state%SoilIce(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%state%SoilMoisture) )         &
       allocate( noahmp%water%state%SoilMoisture(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%state%SoilImpervFrac) )       &
       allocate( noahmp%water%state%SoilImpervFrac(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%state%SoilWatConductivity) )  &
       allocate( noahmp%water%state%SoilWatConductivity(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%state%SoilWatDiffusivity) )   &
       allocate( noahmp%water%state%SoilWatDiffusivity(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%state%SoilEffPorosity) )      &
       allocate( noahmp%water%state%SoilEffPorosity(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%state%SoilIceFrac) )          &
       allocate( noahmp%water%state%SoilIceFrac(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%state%SoilMoistureEqui) )     &
       allocate( noahmp%water%state%SoilMoistureEqui(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%state%SoilTranspFac) )        &
       allocate( noahmp%water%state%SoilTranspFac(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%state%SoilMatPotential) )     &
       allocate( noahmp%water%state%SoilMatPotential(1:NumSoilLayer) )

    noahmp%water%state%IndexPhaseChange   (:)      = undefined_int
    noahmp%water%state%SoilSupercoolWater (:)      = undefined_real
    noahmp%water%state%SnowIce            (:)      = undefined_real
    noahmp%water%state%SnowLiqWater       (:)      = undefined_real
    noahmp%water%state%SnowIceVol         (:)      = undefined_real
    noahmp%water%state%SnowLiqWaterVol    (:)      = undefined_real
    noahmp%water%state%SnowIceFracPrev    (:)      = undefined_real
    noahmp%water%state%SnowIceFrac        (:)      = undefined_real
    noahmp%water%state%SoilIceFrac        (:)      = undefined_real
    noahmp%water%state%SnowEffPorosity    (:)      = undefined_real
    noahmp%water%state%SoilLiqWater       (:)      = undefined_real
    noahmp%water%state%SoilIce            (:)      = undefined_real
    noahmp%water%state%SoilMoisture       (:)      = undefined_real
    noahmp%water%state%SoilImpervFrac     (:)      = undefined_real
    noahmp%water%state%SoilWatConductivity(:)      = undefined_real
    noahmp%water%state%SoilWatDiffusivity (:)      = undefined_real
    noahmp%water%state%SoilEffPorosity    (:)      = undefined_real
    noahmp%water%state%SoilMoistureEqui   (:)      = undefined_real
    noahmp%water%state%SoilTranspFac      (:)      = undefined_real
    noahmp%water%state%SoilMatPotential   (:)      = undefined_real

    ! water flux variables
    noahmp%water%flux%PrecipTotRefHeight           = undefined_real
    noahmp%water%flux%RainfallRefHeight            = undefined_real
    noahmp%water%flux%SnowfallRefHeight            = undefined_real
    noahmp%water%flux%PrecipConvTotRefHeight       = undefined_real
    noahmp%water%flux%PrecipLargeSclRefHeight      = undefined_real
    noahmp%water%flux%EvapCanopyNet                = undefined_real
    noahmp%water%flux%Transpiration                = undefined_real
    noahmp%water%flux%EvapCanopyLiq                = undefined_real
    noahmp%water%flux%DewCanopyLiq                 = undefined_real
    noahmp%water%flux%FrostCanopyIce               = undefined_real
    noahmp%water%flux%SublimCanopyIce              = undefined_real
    noahmp%water%flux%MeltCanopyIce                = undefined_real
    noahmp%water%flux%RefrzCanopyLiq               = undefined_real
    noahmp%water%flux%SnowfallGround               = undefined_real
    noahmp%water%flux%SnowDepthIncr                = undefined_real
    noahmp%water%flux%FrostSnowSfcIce              = undefined_real
    noahmp%water%flux%SublimSnowSfcIce             = undefined_real
    noahmp%water%flux%RainfallGround               = undefined_real
    noahmp%water%flux%SnowBotOutflow               = undefined_real
    noahmp%water%flux%GlacierExcessFlow            = undefined_real
    noahmp%water%flux%SoilSfcInflow                = undefined_real
    noahmp%water%flux%RunoffSurface                = undefined_real
    noahmp%water%flux%RunoffSubsurface             = undefined_real
    noahmp%water%flux%InfilRateSfc                 = undefined_real
    noahmp%water%flux%EvapSoilSfcLiq               = undefined_real
    noahmp%water%flux%DrainSoilBot                 = undefined_real
    noahmp%water%flux%RechargeGw                   = undefined_real
    noahmp%water%flux%DischargeGw                  = undefined_real
    noahmp%water%flux%VaporizeGrd                  = undefined_real
    noahmp%water%flux%CondenseVapGrd               = undefined_real
    noahmp%water%flux%DewSoilSfcLiq                = undefined_real
    noahmp%water%flux%InterceptCanopyRain          = undefined_real
    noahmp%water%flux%DripCanopyRain               = undefined_real
    noahmp%water%flux%ThroughfallRain              = undefined_real
    noahmp%water%flux%InterceptCanopySnow          = undefined_real
    noahmp%water%flux%DripCanopySnow               = undefined_real
    noahmp%water%flux%ThroughfallSnow              = undefined_real
    noahmp%water%flux%EvapSoilNet                  = undefined_real
    noahmp%water%flux%MeltGroundSnow               = undefined_real
    noahmp%water%flux%WaterToAtmosTotal            = undefined_real
    noahmp%water%flux%IrrigationRateFlood          = 0.0
    noahmp%water%flux%IrrigationRateMicro          = 0.0
    noahmp%water%flux%IrrigationRateSprinkler      = 0.0
    noahmp%water%flux%IrriEvapLossSprinkler        = 0.0
    noahmp%water%flux%EvapIrriSprinkler            = 0.0
    noahmp%water%flux%TileDrain                    = 0.0

    if ( .not. allocated(noahmp%water%flux%CompactionSnowAging) )   &
       allocate( noahmp%water%flux%CompactionSnowAging(-NumSnowLayerMax+1:0) )
    if ( .not. allocated(noahmp%water%flux%CompactionSnowBurden) )  &
       allocate( noahmp%water%flux%CompactionSnowBurden(-NumSnowLayerMax+1:0) )
    if ( .not. allocated(noahmp%water%flux%CompactionSnowMelt) )    &
       allocate( noahmp%water%flux%CompactionSnowMelt(-NumSnowLayerMax+1:0) )
    if ( .not. allocated(noahmp%water%flux%CompactionSnowTot) )     &
       allocate( noahmp%water%flux%CompactionSnowTot(-NumSnowLayerMax+1:0) )
    if ( .not. allocated(noahmp%water%flux%TranspWatLossSoil) )     &
       allocate( noahmp%water%flux%TranspWatLossSoil(1:NumSoilLayer) )

    noahmp%water%flux%CompactionSnowAging (:)      = undefined_real
    noahmp%water%flux%CompactionSnowBurden(:)      = undefined_real
    noahmp%water%flux%CompactionSnowMelt  (:)      = undefined_real
    noahmp%water%flux%CompactionSnowTot   (:)      = undefined_real
    noahmp%water%flux%TranspWatLossSoil   (:)      = undefined_real

    ! water parameter variables
    noahmp%water%param%DrainSoilLayerInd           = undefined_int
    noahmp%water%param%TileDrainTubeDepth          = undefined_int
    noahmp%water%param%NumSoilLayerRoot            = undefined_int
    noahmp%water%param%IrriStopDayBfHarvest        = undefined_int
    noahmp%water%param%CanopyLiqHoldCap            = undefined_real
    noahmp%water%param%SnowCompactBurdenFac        = undefined_real
    noahmp%water%param%SnowCompactAgingFac1        = undefined_real
    noahmp%water%param%SnowCompactAgingFac2        = undefined_real
    noahmp%water%param%SnowCompactAgingFac3        = undefined_real
    noahmp%water%param%SnowCompactAgingMax         = undefined_real
    noahmp%water%param%SnowViscosityCoeff          = undefined_real
    noahmp%water%param%SnowLiqFracMax              = undefined_real
    noahmp%water%param%SnowLiqHoldCap              = undefined_real
    noahmp%water%param%SnowLiqReleaseFac           = undefined_real
    noahmp%water%param%IrriFloodRateFac            = undefined_real
    noahmp%water%param%IrriMicroRate               = undefined_real
    noahmp%water%param%SoilInfilMaxCoeff           = undefined_real
    noahmp%water%param%SoilImpervFracCoeff         = undefined_real
    noahmp%water%param%InfilFacVic                 = undefined_real
    noahmp%water%param%TensionWatDistrInfl         = undefined_real
    noahmp%water%param%TensionWatDistrShp          = undefined_real
    noahmp%water%param%FreeWatDistrShp             = undefined_real
    noahmp%water%param%InfilHeteroDynVic           = undefined_real
    noahmp%water%param%InfilCapillaryDynVic        = undefined_real
    noahmp%water%param%InfilFacDynVic              = undefined_real
    noahmp%water%param%SoilDrainSlope              = undefined_real
    noahmp%water%param%TileDrainCoeffSp            = undefined_real
    noahmp%water%param%DrainFacSoilWat             = undefined_real
    noahmp%water%param%TileDrainCoeff              = undefined_real
    noahmp%water%param%DrainDepthToImperv          = undefined_real
    noahmp%water%param%LateralWatCondFac           = undefined_real
    noahmp%water%param%TileDrainDepth              = undefined_real
    noahmp%water%param%DrainTubeDist               = undefined_real
    noahmp%water%param%DrainTubeRadius             = undefined_real
    noahmp%water%param%DrainWatDepToImperv         = undefined_real
    noahmp%water%param%RunoffDecayFac              = undefined_real
    noahmp%water%param%BaseflowCoeff               = undefined_real
    noahmp%water%param%GridTopoIndex               = undefined_real
    noahmp%water%param%SoilSfcSatFracMax           = undefined_real
    noahmp%water%param%SpecYieldGw                 = undefined_real
    noahmp%water%param%MicroPoreContent            = undefined_real
    noahmp%water%param%WaterStorageLakeMax         = undefined_real
    noahmp%water%param%SnoWatEqvMaxGlacier         = undefined_real
    noahmp%water%param%SoilConductivityRef         = undefined_real
    noahmp%water%param%SoilInfilFacRef             = undefined_real
    noahmp%water%param%GroundFrzCoeff              = undefined_real
    noahmp%water%param%IrriTriggerLaiMin           = undefined_real
    noahmp%water%param%SoilWatDeficitAllow         = undefined_real
    noahmp%water%param%IrriFloodLossFrac           = undefined_real
    noahmp%water%param%IrriSprinklerRate           = undefined_real
    noahmp%water%param%IrriFracThreshold           = undefined_real
    noahmp%water%param%IrriStopPrecipThr           = undefined_real
    noahmp%water%param%SnowfallDensityMax          = undefined_real
    noahmp%water%param%SnowMassFullCoverOld        = undefined_real
    noahmp%water%param%SoilMatPotentialWilt        = undefined_real
    noahmp%water%param%SnowMeltFac                 = undefined_real
    noahmp%water%param%SnowCoverFac                = undefined_real

    if ( .not. allocated(noahmp%water%param%SoilMoistureSat) )        &
       allocate( noahmp%water%param%SoilMoistureSat(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%param%SoilMoistureWilt) )       &
       allocate( noahmp%water%param%SoilMoistureWilt(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%param%SoilMoistureFieldCap) )   &
       allocate( noahmp%water%param%SoilMoistureFieldCap(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%param%SoilMoistureDry) )        &
       allocate( noahmp%water%param%SoilMoistureDry(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%param%SoilWatDiffusivitySat) )  &
       allocate( noahmp%water%param%SoilWatDiffusivitySat(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%param%SoilWatConductivitySat) ) &
       allocate( noahmp%water%param%SoilWatConductivitySat(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%param%SoilExpCoeffB) )          &
       allocate( noahmp%water%param%SoilExpCoeffB(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%water%param%SoilMatPotentialSat) )    &
       allocate( noahmp%water%param%SoilMatPotentialSat(1:NumSoilLayer) )

    noahmp%water%param%SoilMoistureSat       (:)   = undefined_real
    noahmp%water%param%SoilMoistureWilt      (:)   = undefined_real
    noahmp%water%param%SoilMoistureFieldCap  (:)   = undefined_real
    noahmp%water%param%SoilMoistureDry       (:)   = undefined_real
    noahmp%water%param%SoilWatDiffusivitySat (:)   = undefined_real
    noahmp%water%param%SoilWatConductivitySat(:)   = undefined_real
    noahmp%water%param%SoilExpCoeffB         (:)   = undefined_real
    noahmp%water%param%SoilMatPotentialSat   (:)   = undefined_real

    end associate

  end subroutine WaterVarInitDefault


!=== initialize with input data or table values
  subroutine WaterVarInitTransfer(noahmp, NoahmpIO)

    implicit none

    type(noahmp_type),   intent(inout) :: noahmp
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

    ! local variables 
    integer                            :: IndexSoilLayer
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilSand
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilClay
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilOrg

    associate(                                                           &
              I                => noahmp%config%domain%GridIndexI       ,&
              J                => noahmp%config%domain%GridIndexJ       ,&
              NumSnowLayerMax  => noahmp%config%domain%NumSnowLayerMax  ,&
              NumSoilLayer     => noahmp%config%domain%NumSoilLayer     ,&
              VegType          => noahmp%config%domain%VegType          ,&
              SoilType         => noahmp%config%domain%SoilType         ,&
              FlagUrban        => noahmp%config%domain%FlagUrban        ,&
              RunoffSlopeType  => noahmp%config%domain%RunoffSlopeType  ,&
              NumSnowLayerNeg  => noahmp%config%domain%NumSnowLayerNeg   &
             )

    ! water state variables
    noahmp%water%state%CanopyLiqWater                     = NoahmpIO%CANLIQXY   (I,J)
    noahmp%water%state%CanopyIce                          = NoahmpIO%CANICEXY   (I,J)
    noahmp%water%state%CanopyWetFrac                      = NoahmpIO%FWETXY     (I,J)
    noahmp%water%state%SnowWaterEquiv                     = NoahmpIO%SNOW       (I,J)
    noahmp%water%state%SnowWaterEquivPrev                 = NoahmpIO%SNEQVOXY   (I,J) 
    noahmp%water%state%SnowDepth                          = NoahmpIO%SNOWH      (I,J)
    noahmp%water%state%IrrigationFracFlood                = NoahmpIO%FIFRACT    (I,J)
    noahmp%water%state%IrrigationAmtFlood                 = NoahmpIO%IRWATFI    (I,J)
    noahmp%water%state%IrrigationFracMicro                = NoahmpIO%MIFRACT    (I,J)
    noahmp%water%state%IrrigationAmtMicro                 = NoahmpIO%IRWATMI    (I,J) 
    noahmp%water%state%IrrigationFracSprinkler            = NoahmpIO%SIFRACT    (I,J)
    noahmp%water%state%IrrigationAmtSprinkler             = NoahmpIO%IRWATSI    (I,J)  
    noahmp%water%state%WaterTableDepth                    = NoahmpIO%ZWTXY      (I,J) 
    noahmp%water%state%SoilMoistureToWT                   = NoahmpIO%SMCWTDXY   (I,J)
    noahmp%water%state%TileDrainFrac                      = NoahmpIO%TD_FRACTION(I,J)
    noahmp%water%state%WaterStorageAquifer                = NoahmpIO%WAXY       (I,J)
    noahmp%water%state%WaterStorageSoilAqf                = NoahmpIO%WTXY       (I,J)
    noahmp%water%state%WaterStorageLake                   = NoahmpIO%WSLAKEXY   (I,J)
    noahmp%water%state%IrrigationFracGrid                 = NoahmpIO%IRFRACT    (I,J)
    noahmp%water%state%IrrigationCntSprinkler             = NoahmpIO%IRNUMSI    (I,J)     
    noahmp%water%state%IrrigationCntMicro                 = NoahmpIO%IRNUMMI    (I,J)
    noahmp%water%state%IrrigationCntFlood                 = NoahmpIO%IRNUMFI    (I,J)
    noahmp%water%state%SnowIce     (-NumSnowLayerMax+1:0) = NoahmpIO%SNICEXY    (I,-NumSnowLayerMax+1:0,J)
    noahmp%water%state%SnowLiqWater(-NumSnowLayerMax+1:0) = NoahmpIO%SNLIQXY    (I,-NumSnowLayerMax+1:0,J)
    noahmp%water%state%SoilLiqWater      (1:NumSoilLayer) = NoahmpIO%SH2O       (I,1:NumSoilLayer,J)
    noahmp%water%state%SoilMoisture      (1:NumSoilLayer) = NoahmpIO%SMOIS      (I,1:NumSoilLayer,J)    
    noahmp%water%state%SoilMoistureEqui  (1:NumSoilLayer) = NoahmpIO%SMOISEQ    (I,1:NumSoilLayer,J)
    noahmp%water%state%RechargeGwDeepWT                   = 0.0
    noahmp%water%state%RechargeGwShallowWT                = 0.0
#ifdef WRF_HYDRO
    noahmp%water%state%WaterTableHydro                    = NoahmpIO%ZWATBLE2D  (I,J)
    noahmp%water%state%WaterHeadSfc                       = NoahmpIO%sfcheadrt  (I,J)
#endif

    ! water parameter variables
    noahmp%water%param%DrainSoilLayerInd                  = NoahmpIO%DRAIN_LAYER_OPT_TABLE
    noahmp%water%param%CanopyLiqHoldCap                   = NoahmpIO%CH2OP_TABLE(VegType)
    noahmp%water%param%SnowCompactBurdenFac               = NoahmpIO%C2_SNOWCOMPACT_TABLE
    noahmp%water%param%SnowCompactAgingFac1               = NoahmpIO%C3_SNOWCOMPACT_TABLE
    noahmp%water%param%SnowCompactAgingFac2               = NoahmpIO%C4_SNOWCOMPACT_TABLE
    noahmp%water%param%SnowCompactAgingFac3               = NoahmpIO%C5_SNOWCOMPACT_TABLE
    noahmp%water%param%SnowCompactAgingMax                = NoahmpIO%DM_SNOWCOMPACT_TABLE
    noahmp%water%param%SnowViscosityCoeff                 = NoahmpIO%ETA0_SNOWCOMPACT_TABLE
    noahmp%water%param%SnowLiqFracMax                     = NoahmpIO%SNLIQMAXFRAC_TABLE
    noahmp%water%param%SnowLiqHoldCap                     = NoahmpIO%SSI_TABLE
    noahmp%water%param%SnowLiqReleaseFac                  = NoahmpIO%SNOW_RET_FAC_TABLE
    noahmp%water%param%IrriFloodRateFac                   = NoahmpIO%FIRTFAC_TABLE
    noahmp%water%param%IrriMicroRate                      = NoahmpIO%MICIR_RATE_TABLE
    noahmp%water%param%SoilConductivityRef                = NoahmpIO%REFDK_TABLE
    noahmp%water%param%SoilInfilFacRef                    = NoahmpIO%REFKDT_TABLE
    noahmp%water%param%GroundFrzCoeff                     = NoahmpIO%FRZK_TABLE
    noahmp%water%param%GridTopoIndex                      = NoahmpIO%TIMEAN_TABLE
    noahmp%water%param%SoilSfcSatFracMax                  = NoahmpIO%FSATMX_TABLE
    noahmp%water%param%SpecYieldGw                        = NoahmpIO%ROUS_TABLE
    noahmp%water%param%MicroPoreContent                   = NoahmpIO%CMIC_TABLE
    noahmp%water%param%WaterStorageLakeMax                = NoahmpIO%WSLMAX_TABLE
    noahmp%water%param%SnoWatEqvMaxGlacier                = NoahmpIO%SWEMAXGLA_TABLE
    noahmp%water%param%IrriStopDayBfHarvest               = NoahmpIO%IRR_HAR_TABLE
    noahmp%water%param%IrriTriggerLaiMin                  = NoahmpIO%IRR_LAI_TABLE
    noahmp%water%param%SoilWatDeficitAllow                = NoahmpIO%IRR_MAD_TABLE
    noahmp%water%param%IrriFloodLossFrac                  = NoahmpIO%FILOSS_TABLE
    noahmp%water%param%IrriSprinklerRate                  = NoahmpIO%SPRIR_RATE_TABLE
    noahmp%water%param%IrriFracThreshold                  = NoahmpIO%IRR_FRAC_TABLE
    noahmp%water%param%IrriStopPrecipThr                  = NoahmpIO%IR_RAIN_TABLE
    noahmp%water%param%SnowfallDensityMax                 = NoahmpIO%SNOWDEN_MAX_TABLE
    noahmp%water%param%SnowMassFullCoverOld               = NoahmpIO%SWEMX_TABLE
    noahmp%water%param%SoilMatPotentialWilt               = NoahmpIO%PSIWLT_TABLE
    noahmp%water%param%SnowMeltFac                        = NoahmpIO%MFSNO_TABLE(VegType)
    noahmp%water%param%SnowCoverFac                       = NoahmpIO%SCFFAC_TABLE(VegType)
    noahmp%water%param%InfilFacVic                        = NoahmpIO%BVIC_TABLE(SoilType(1))
    noahmp%water%param%TensionWatDistrInfl                = NoahmpIO%AXAJ_TABLE(SoilType(1))
    noahmp%water%param%TensionWatDistrShp                 = NoahmpIO%BXAJ_TABLE(SoilType(1))
    noahmp%water%param%FreeWatDistrShp                    = NoahmpIO%XXAJ_TABLE(SoilType(1))
    noahmp%water%param%InfilHeteroDynVic                  = NoahmpIO%BBVIC_TABLE(SoilType(1))
    noahmp%water%param%InfilCapillaryDynVic               = NoahmpIO%GDVIC_TABLE(SoilType(1))
    noahmp%water%param%InfilFacDynVic                     = NoahmpIO%BDVIC_TABLE(SoilType(1))
    noahmp%water%param%TileDrainCoeffSp                   = NoahmpIO%TD_DC_TABLE(SoilType(1))
    noahmp%water%param%TileDrainTubeDepth                 = NoahmpIO%TD_DEPTH_TABLE(SoilType(1))
    noahmp%water%param%DrainFacSoilWat                    = NoahmpIO%TDSMC_FAC_TABLE(SoilType(1))
    noahmp%water%param%TileDrainCoeff                     = NoahmpIO%TD_DCOEF_TABLE(SoilType(1))
    noahmp%water%param%DrainDepthToImperv                 = NoahmpIO%TD_ADEPTH_TABLE(SoilType(1))
    noahmp%water%param%LateralWatCondFac                  = NoahmpIO%KLAT_FAC_TABLE(SoilType(1))
    noahmp%water%param%TileDrainDepth                     = NoahmpIO%TD_DDRAIN_TABLE(SoilType(1))
    noahmp%water%param%DrainTubeDist                      = NoahmpIO%TD_SPAC_TABLE(SoilType(1))
    noahmp%water%param%DrainTubeRadius                    = NoahmpIO%TD_RADI_TABLE(SoilType(1))
    noahmp%water%param%DrainWatDepToImperv                = NoahmpIO%TD_D_TABLE(SoilType(1))
    noahmp%water%param%NumSoilLayerRoot                   = NoahmpIO%NROOT_TABLE(VegType)
    noahmp%water%param%SoilDrainSlope                     = NoahmpIO%SLOPE_TABLE(RunoffSlopeType)

    do IndexSoilLayer = 1, size(SoilType)
       noahmp%water%param%SoilMoistureSat       (IndexSoilLayer)  = NoahmpIO%SMCMAX_TABLE(SoilType(IndexSoilLayer))
       noahmp%water%param%SoilMoistureWilt      (IndexSoilLayer)  = NoahmpIO%SMCWLT_TABLE(SoilType(IndexSoilLayer))
       noahmp%water%param%SoilMoistureFieldCap  (IndexSoilLayer)  = NoahmpIO%SMCREF_TABLE(SoilType(IndexSoilLayer))
       noahmp%water%param%SoilMoistureDry       (IndexSoilLayer)  = NoahmpIO%SMCDRY_TABLE(SoilType(IndexSoilLayer))
       noahmp%water%param%SoilWatDiffusivitySat (IndexSoilLayer)  = NoahmpIO%DWSAT_TABLE (SoilType(IndexSoilLayer))
       noahmp%water%param%SoilWatConductivitySat(IndexSoilLayer)  = NoahmpIO%DKSAT_TABLE (SoilType(IndexSoilLayer))
       noahmp%water%param%SoilExpCoeffB         (IndexSoilLayer)  = NoahmpIO%BEXP_TABLE  (SoilType(IndexSoilLayer))
       noahmp%water%param%SoilMatPotentialSat   (IndexSoilLayer)  = NoahmpIO%PSISAT_TABLE(SoilType(IndexSoilLayer))
    enddo

    ! derived water parameters
    noahmp%water%param%SoilInfilMaxCoeff  = noahmp%water%param%SoilInfilFacRef * &
                              noahmp%water%param%SoilWatConductivitySat(1) / noahmp%water%param%SoilConductivityRef
    if ( FlagUrban .eqv. .true. ) then
       noahmp%water%param%SoilMoistureSat = 0.45
       noahmp%water%param%SoilMoistureFieldCap = 0.42
       noahmp%water%param%SoilMoistureWilt = 0.40
       noahmp%water%param%SoilMoistureDry = 0.40
    endif

    if ( SoilType(1) /= 14 ) then
       noahmp%water%param%SoilImpervFracCoeff = noahmp%water%param%GroundFrzCoeff * &
             ((noahmp%water%param%SoilMoistureSat(1) / noahmp%water%param%SoilMoistureFieldCap(1)) * (0.412/0.468))
    endif

    noahmp%water%state%SnowIceFracPrev = 0.0
    noahmp%water%state%SnowIceFracPrev(NumSnowLayerNeg+1:0) = NoahmpIO%SNICEXY(I,NumSnowLayerNeg+1:0,J) / & 
                            (NoahmpIO%SNICEXY(I,NumSnowLayerNeg+1:0,J) + NoahmpIO%SNLIQXY(I,NumSnowLayerNeg+1:0,J))

    if ( (noahmp%config%nmlist%OptSoilProperty == 3) .and. (.not. noahmp%config%domain%FlagUrban) ) then
       allocate( SoilSand(1:NumSoilLayer) )
       allocate( SoilClay(1:NumSoilLayer) )
       allocate( SoilOrg (1:NumSoilLayer) )
       SoilSand(1:4) = 0.01 * NoahmpIO%soilcomp(I,1:4,J)
       SoilClay(1:4) = 0.01 * NoahmpIO%soilcomp(I,5:8,J)
       SoilOrg(1:4)  = 0.0
       if (noahmp%config%nmlist%OptPedotransfer == 1) &
          call PedoTransfer_SR2006(NoahmpIO,noahmp,SoilSand,SoilClay,SoilOrg)
    endif

    end associate

  end subroutine WaterVarInitTransfer

end module WaterVarInitMod
