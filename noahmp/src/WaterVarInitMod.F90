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

    ! water state variable
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

    noahmp%water%state%IndexPhaseChange   (:) = undefined_int
    noahmp%water%state%SoilSupercoolWater (:) = undefined_real
    noahmp%water%state%SnowIce            (:) = undefined_real
    noahmp%water%state%SnowLiqWater       (:) = undefined_real
    noahmp%water%state%SnowIceVol         (:) = undefined_real
    noahmp%water%state%SnowLiqWaterVol    (:) = undefined_real
    noahmp%water%state%SnowIceFracPrev    (:) = undefined_real
    noahmp%water%state%SnowIceFrac        (:) = undefined_real
    noahmp%water%state%SoilIceFrac        (:) = undefined_real
    noahmp%water%state%SnowEffPorosity    (:) = undefined_real
    noahmp%water%state%SoilLiqWater       (:) = undefined_real
    noahmp%water%state%SoilIce            (:) = undefined_real
    noahmp%water%state%SoilMoisture       (:) = undefined_real
    noahmp%water%state%SoilImpervFrac     (:) = undefined_real
    noahmp%water%state%SoilWatConductivity(:) = undefined_real
    noahmp%water%state%SoilWatDiffusivity (:) = undefined_real
    noahmp%water%state%SoilEffPorosity    (:) = undefined_real
    noahmp%water%state%SoilMoistureEqui   (:) = undefined_real
    noahmp%water%state%SoilTranspFac      (:) = undefined_real
    noahmp%water%state%SoilMatPotential   (:) = undefined_real

    ! water flux variable
    noahmp%water%flux%PRCP            = undefined_real
    noahmp%water%flux%RAIN            = undefined_real
    noahmp%water%flux%SNOW            = undefined_real
    noahmp%water%flux%QPRECC          = undefined_real
    noahmp%water%flux%QPRECL          = undefined_real
    noahmp%water%flux%ECAN            = undefined_real
    noahmp%water%flux%ETRAN           = undefined_real
    noahmp%water%flux%QEVAC           = undefined_real
    noahmp%water%flux%QDEWC           = undefined_real
    noahmp%water%flux%QFROC           = undefined_real
    noahmp%water%flux%QSUBC           = undefined_real
    noahmp%water%flux%QMELTC          = undefined_real
    noahmp%water%flux%QFRZC           = undefined_real
    noahmp%water%flux%QSNOW           = undefined_real
    noahmp%water%flux%SNOWHIN         = undefined_real
    noahmp%water%flux%QSNFRO          = undefined_real
    noahmp%water%flux%QSNSUB          = undefined_real
    noahmp%water%flux%QRAIN           = undefined_real
    noahmp%water%flux%QSNBOT          = undefined_real
    noahmp%water%flux%SNOFLOW         = undefined_real
    noahmp%water%flux%QINSUR          = undefined_real
    noahmp%water%flux%RUNSRF          = undefined_real
    noahmp%water%flux%RUNSUB          = undefined_real
    noahmp%water%flux%PDDUM           = undefined_real
    noahmp%water%flux%QSEVA           = undefined_real
    noahmp%water%flux%QDRAIN          = undefined_real
    noahmp%water%flux%QIN             = undefined_real
    noahmp%water%flux%QDIS            = undefined_real
    noahmp%water%flux%QVAP            = undefined_real
    noahmp%water%flux%QDEW            = undefined_real
    noahmp%water%flux%QSDEW           = undefined_real
    noahmp%water%flux%QINTR           = undefined_real
    noahmp%water%flux%QDRIPR          = undefined_real
    noahmp%water%flux%QTHROR          = undefined_real
    noahmp%water%flux%QINTS           = undefined_real
    noahmp%water%flux%QDRIPS          = undefined_real
    noahmp%water%flux%QTHROS          = undefined_real
    noahmp%water%flux%EDIR            = undefined_real
    noahmp%water%flux%QMELT           = undefined_real
    noahmp%water%flux%QFX             = undefined_real
    noahmp%water%flux%IRFIRATE        = 0.0
    noahmp%water%flux%IRMIRATE        = 0.0
    noahmp%water%flux%IRSIRATE        = 0.0
    noahmp%water%flux%IREVPLOS        = 0.0
    noahmp%water%flux%EIRR            = 0.0
    noahmp%water%flux%QTLDRN          = 0.0

    if( .not. allocated( noahmp%water%flux%DDZ1   ) ) allocate( noahmp%water%flux%DDZ1   (-NumSnowLayerMax+1:0) )
    if( .not. allocated( noahmp%water%flux%DDZ2   ) ) allocate( noahmp%water%flux%DDZ2   (-NumSnowLayerMax+1:0) )
    if( .not. allocated( noahmp%water%flux%DDZ3   ) ) allocate( noahmp%water%flux%DDZ3   (-NumSnowLayerMax+1:0) )
    if( .not. allocated( noahmp%water%flux%PDZDTC ) ) allocate( noahmp%water%flux%PDZDTC (-NumSnowLayerMax+1:0) )
    if( .not. allocated( noahmp%water%flux%ETRANI ) ) allocate( noahmp%water%flux%ETRANI ( 1:NumSoilLayer) )

    noahmp%water%flux%DDZ1(:)           = undefined_real
    noahmp%water%flux%DDZ2(:)           = undefined_real
    noahmp%water%flux%DDZ3(:)           = undefined_real
    noahmp%water%flux%PDZDTC(:)         = undefined_real
    noahmp%water%flux%ETRANI(:)         = undefined_real

    ! water parameter variable
    noahmp%water%param%DRAIN_LAYER_OPT  = undefined_int
    noahmp%water%param%TD_DEPTH         = undefined_int
    noahmp%water%param%NROOT            = undefined_int
    noahmp%water%param%IRR_HAR          = undefined_int
    noahmp%water%param%CH2OP            = undefined_real
    noahmp%water%param%C2_SnowCompact   = undefined_real
    noahmp%water%param%C3_SnowCompact   = undefined_real
    noahmp%water%param%C4_SnowCompact   = undefined_real
    noahmp%water%param%C5_SnowCompact   = undefined_real
    noahmp%water%param%DM_SnowCompact   = undefined_real
    noahmp%water%param%ETA0_SnowCompact = undefined_real
    noahmp%water%param%SNLIQMAXFRAC     = undefined_real
    noahmp%water%param%SSI              = undefined_real
    noahmp%water%param%SNOW_RET_FAC     = undefined_real
    noahmp%water%param%FIRTFAC          = undefined_real
    noahmp%water%param%MICIR_RATE       = undefined_real
    noahmp%water%param%KDT              = undefined_real
    noahmp%water%param%FRZX             = undefined_real
    noahmp%water%param%BVIC             = undefined_real
    noahmp%water%param%AXAJ             = undefined_real
    noahmp%water%param%BXAJ             = undefined_real
    noahmp%water%param%XXAJ             = undefined_real
    noahmp%water%param%BBVIC            = undefined_real
    noahmp%water%param%GDVIC            = undefined_real
    noahmp%water%param%BDVIC            = undefined_real
    noahmp%water%param%SLOPE            = undefined_real
    noahmp%water%param%TD_DC            = undefined_real
    noahmp%water%param%TDSMC_FAC        = undefined_real
    noahmp%water%param%TD_DCOEF         = undefined_real
    noahmp%water%param%TD_ADEPTH        = undefined_real
    noahmp%water%param%KLAT_FAC         = undefined_real
    noahmp%water%param%TD_DDRAIN        = undefined_real
    noahmp%water%param%TD_SPAC          = undefined_real
    noahmp%water%param%TD_RADI          = undefined_real
    noahmp%water%param%TD_D             = undefined_real
    noahmp%water%param%FFF              = undefined_real
    noahmp%water%param%RSBMX            = undefined_real
    noahmp%water%param%TIMEAN           = undefined_real
    noahmp%water%param%FSATMX           = undefined_real
    noahmp%water%param%ROUS             = undefined_real
    noahmp%water%param%CMIC             = undefined_real
    noahmp%water%param%WSLMAX           = undefined_real
    noahmp%water%param%SWEMAXGLA        = undefined_real
    noahmp%water%param%REFDK            = undefined_real
    noahmp%water%param%REFKDT           = undefined_real
    noahmp%water%param%FRZK             = undefined_real
    noahmp%water%param%IRR_LAI          = undefined_real
    noahmp%water%param%IRR_MAD          = undefined_real
    noahmp%water%param%FILOSS           = undefined_real
    noahmp%water%param%SPRIR_RATE       = undefined_real
    noahmp%water%param%IRR_FRAC         = undefined_real
    noahmp%water%param%IR_RAIN          = undefined_real
    noahmp%water%param%SNOWDEN_MIN      = undefined_real
    noahmp%water%param%SWEMX            = undefined_real
    noahmp%water%param%PSIWLT           = undefined_real
    noahmp%water%param%MFSNO            = undefined_real
    noahmp%water%param%SCFFAC           = undefined_real

    if( .not. allocated( noahmp%water%param%SMCMAX ) ) allocate( noahmp%water%param%SMCMAX (1:NumSoilLayer) )
    if( .not. allocated( noahmp%water%param%SMCWLT ) ) allocate( noahmp%water%param%SMCWLT (1:NumSoilLayer) )
    if( .not. allocated( noahmp%water%param%SMCREF ) ) allocate( noahmp%water%param%SMCREF (1:NumSoilLayer) )
    if( .not. allocated( noahmp%water%param%SMCDRY ) ) allocate( noahmp%water%param%SMCDRY (1:NumSoilLayer) )
    if( .not. allocated( noahmp%water%param%DWSAT  ) ) allocate( noahmp%water%param%DWSAT  (1:NumSoilLayer) )
    if( .not. allocated( noahmp%water%param%DKSAT  ) ) allocate( noahmp%water%param%DKSAT  (1:NumSoilLayer) )
    if( .not. allocated( noahmp%water%param%BEXP   ) ) allocate( noahmp%water%param%BEXP   (1:NumSoilLayer) )
    if( .not. allocated( noahmp%water%param%PSISAT ) ) allocate( noahmp%water%param%PSISAT (1:NumSoilLayer) )
    if( .not. allocated( noahmp%water%param%QUARTZ ) ) allocate( noahmp%water%param%QUARTZ (1:NumSoilLayer) )

    noahmp%water%param%SMCMAX(:)        = undefined_real
    noahmp%water%param%SMCWLT(:)        = undefined_real
    noahmp%water%param%SMCREF(:)        = undefined_real
    noahmp%water%param%SMCDRY(:)        = undefined_real
    noahmp%water%param%DWSAT (:)        = undefined_real
    noahmp%water%param%DKSAT (:)        = undefined_real
    noahmp%water%param%BEXP  (:)        = undefined_real
    noahmp%water%param%PSISAT(:)        = undefined_real
    noahmp%water%param%QUARTZ(:)        = undefined_real

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

    ! water state variable
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

    ! water parameter variable
    noahmp%water%param%DRAIN_LAYER_OPT   = NoahmpIO%DRAIN_LAYER_OPT_TABLE
    noahmp%water%param%CH2OP             = NoahmpIO%CH2OP_TABLE(VegType)
    noahmp%water%param%C2_SnowCompact    = NoahmpIO%C2_SNOWCOMPACT_TABLE
    noahmp%water%param%C3_SnowCompact    = NoahmpIO%C3_SNOWCOMPACT_TABLE
    noahmp%water%param%C4_SnowCompact    = NoahmpIO%C4_SNOWCOMPACT_TABLE
    noahmp%water%param%C5_SnowCompact    = NoahmpIO%C5_SNOWCOMPACT_TABLE
    noahmp%water%param%DM_SnowCompact    = NoahmpIO%DM_SNOWCOMPACT_TABLE
    noahmp%water%param%ETA0_SnowCompact  = NoahmpIO%ETA0_SNOWCOMPACT_TABLE
    noahmp%water%param%SNLIQMAXFRAC      = NoahmpIO%SNLIQMAXFRAC_TABLE
    noahmp%water%param%SSI               = NoahmpIO%SSI_TABLE
    noahmp%water%param%SNOW_RET_FAC      = NoahmpIO%SNOW_RET_FAC_TABLE
    noahmp%water%param%FIRTFAC           = NoahmpIO%FIRTFAC_TABLE
    noahmp%water%param%MICIR_RATE        = NoahmpIO%MICIR_RATE_TABLE
    noahmp%water%param%REFDK             = NoahmpIO%REFDK_TABLE
    noahmp%water%param%REFKDT            = NoahmpIO%REFKDT_TABLE
    noahmp%water%param%FRZK              = NoahmpIO%FRZK_TABLE
    noahmp%water%param%TIMEAN            = NoahmpIO%TIMEAN_TABLE
    noahmp%water%param%FSATMX            = NoahmpIO%FSATMX_TABLE
    noahmp%water%param%ROUS              = NoahmpIO%ROUS_TABLE
    noahmp%water%param%CMIC              = NoahmpIO%CMIC_TABLE
    noahmp%water%param%WSLMAX            = NoahmpIO%WSLMAX_TABLE
    noahmp%water%param%SWEMAXGLA         = NoahmpIO%SWEMAXGLA_TABLE
    noahmp%water%param%IRR_HAR           = NoahmpIO%IRR_HAR_TABLE
    noahmp%water%param%IRR_LAI           = NoahmpIO%IRR_LAI_TABLE
    noahmp%water%param%IRR_MAD           = NoahmpIO%IRR_MAD_TABLE
    noahmp%water%param%FILOSS            = NoahmpIO%FILOSS_TABLE
    noahmp%water%param%SPRIR_RATE        = NoahmpIO%SPRIR_RATE_TABLE
    noahmp%water%param%IRR_FRAC          = NoahmpIO%IRR_FRAC_TABLE
    noahmp%water%param%IR_RAIN           = NoahmpIO%IR_RAIN_TABLE
    noahmp%water%param%SNOWDEN_MIN       = NoahmpIO%SNOWDEN_MIN_TABLE
    noahmp%water%param%SWEMX             = NoahmpIO%SWEMX_TABLE
    noahmp%water%param%PSIWLT            = NoahmpIO%PSIWLT_TABLE
    noahmp%water%param%MFSNO             = NoahmpIO%MFSNO_TABLE(VegType)
    noahmp%water%param%SCFFAC            = NoahmpIO%SCFFAC_TABLE(VegType)
    noahmp%water%param%BVIC              = NoahmpIO%BVIC_TABLE(SoilType(1))
    noahmp%water%param%AXAJ              = NoahmpIO%AXAJ_TABLE(SoilType(1))
    noahmp%water%param%BXAJ              = NoahmpIO%BXAJ_TABLE(SoilType(1))
    noahmp%water%param%XXAJ              = NoahmpIO%XXAJ_TABLE(SoilType(1))
    noahmp%water%param%BBVIC             = NoahmpIO%BBVIC_TABLE(SoilType(1))
    noahmp%water%param%GDVIC             = NoahmpIO%GDVIC_TABLE(SoilType(1))
    noahmp%water%param%BDVIC             = NoahmpIO%BDVIC_TABLE(SoilType(1))
    noahmp%water%param%TD_DC             = NoahmpIO%TD_DC_TABLE(SoilType(1))
    noahmp%water%param%TD_DEPTH          = NoahmpIO%TD_DEPTH_TABLE(SoilType(1))
    noahmp%water%param%TDSMC_FAC         = NoahmpIO%TDSMC_FAC_TABLE(SoilType(1))
    noahmp%water%param%TD_DCOEF          = NoahmpIO%TD_DCOEF_TABLE(SoilType(1))
    noahmp%water%param%TD_ADEPTH         = NoahmpIO%TD_ADEPTH_TABLE(SoilType(1))
    noahmp%water%param%KLAT_FAC          = NoahmpIO%KLAT_FAC_TABLE(SoilType(1))
    noahmp%water%param%TD_DDRAIN         = NoahmpIO%TD_DDRAIN_TABLE(SoilType(1))
    noahmp%water%param%TD_SPAC           = NoahmpIO%TD_SPAC_TABLE(SoilType(1))
    noahmp%water%param%TD_RADI           = NoahmpIO%TD_RADI_TABLE(SoilType(1))
    noahmp%water%param%TD_D              = NoahmpIO%TD_D_TABLE(SoilType(1))
    noahmp%water%param%NROOT             = NoahmpIO%NROOT_TABLE(VegType)
    noahmp%water%param%SLOPE             = NoahmpIO%SLOPE_TABLE(RunoffSlopeType)

    do IndexSoilLayer = 1, size(SoilType)
       noahmp%water%param%SMCMAX(IndexSoilLayer)   = NoahmpIO%SMCMAX_TABLE(SoilType(IndexSoilLayer))
       noahmp%water%param%SMCWLT(IndexSoilLayer)   = NoahmpIO%SMCWLT_TABLE(SoilType(IndexSoilLayer))
       noahmp%water%param%SMCREF(IndexSoilLayer)   = NoahmpIO%SMCREF_TABLE(SoilType(IndexSoilLayer))
       noahmp%water%param%SMCDRY(IndexSoilLayer)   = NoahmpIO%SMCDRY_TABLE(SoilType(IndexSoilLayer))
       noahmp%water%param%DWSAT(IndexSoilLayer)    = NoahmpIO%DWSAT_TABLE(SoilType(IndexSoilLayer))
       noahmp%water%param%DKSAT(IndexSoilLayer)    = NoahmpIO%DKSAT_TABLE(SoilType(IndexSoilLayer))
       noahmp%water%param%BEXP(IndexSoilLayer)     = NoahmpIO%BEXP_TABLE(SoilType(IndexSoilLayer))
       noahmp%water%param%PSISAT(IndexSoilLayer)   = NoahmpIO%PSISAT_TABLE(SoilType(IndexSoilLayer))
       noahmp%water%param%QUARTZ(IndexSoilLayer)   = NoahmpIO%QUARTZ_TABLE(SoilType(IndexSoilLayer))
    enddo

    ! derived water parameters
    noahmp%water%param%KDT  = noahmp%water%param%REFKDT * noahmp%water%param%DKSAT(1) / &
                              noahmp%water%param%REFDK
    if ( FlagUrban .eqv. .true. ) then
       noahmp%water%param%SMCMAX = 0.45
       noahmp%water%param%SMCREF = 0.42
       noahmp%water%param%SMCWLT = 0.40
       noahmp%water%param%SMCDRY = 0.40
    endif
    if ( SoilType(1) /= 14 ) then
       noahmp%water%param%FRZX = noahmp%water%param%FRZK * &
            ((noahmp%water%param%SMCMAX(1) / noahmp%water%param%SMCREF(1)) * (0.412/0.468))
    endif

    noahmp%water%state%SnowIceFracPrev            = 0.0
    noahmp%water%state%SnowIceFracPrev(NumSnowLayerNeg+1:0) = NoahmpIO%SNICEXY(I,NumSnowLayerNeg+1:0,J)   &  ! snow ice fraction  
                                                          / (NoahmpIO%SNICEXY(I,NumSnowLayerNeg+1:0,J) &
                                                             + NoahmpIO%SNLIQXY(I,NumSnowLayerNeg+1:0,J))
    if ( (noahmp%config%nmlist%OptSoilProperty == 3) .and. (.not. noahmp%config%domain%FlagUrban) ) then
       allocate( SoilSand(1:NumSoilLayer) )
       allocate( SoilClay(1:NumSoilLayer) )
       allocate( SoilOrg(1:NumSoilLayer) )
       SoilSand(1:4) = 0.01 * NoahmpIO%soilcomp(I,1:4,J)
       SoilClay(1:4) = 0.01 * NoahmpIO%soilcomp(I,5:8,J)
       SoilOrg(1:4) = 0.0
       if (noahmp%config%nmlist%OptPedotransfer == 1) call PedoTransfer_SR2006(NoahmpIO,noahmp,SoilSand,SoilClay,SoilOrg)
    endif

    end associate

  end subroutine WaterVarInitTransfer

end module WaterVarInitMod
