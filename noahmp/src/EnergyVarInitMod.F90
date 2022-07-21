module EnergyVarInitMod

!!! Initialize column (1-D) Noah-MP energy variables
!!! Energy variables should be first defined in EnergyType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
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
    noahmp%energy%param%RC              = undefined_real
    noahmp%energy%param%HVT             = undefined_real
    noahmp%energy%param%HVB             = undefined_real
    noahmp%energy%param%Z0MVT           = undefined_real
    noahmp%energy%param%DEN             = undefined_real
    noahmp%energy%param%XL              = undefined_real
    noahmp%energy%param%BETADS          = undefined_real
    noahmp%energy%param%BETAIS          = undefined_real
    noahmp%energy%param%CSOIL           = undefined_real
    noahmp%energy%param%TAU0            = undefined_real
    noahmp%energy%param%GRAIN_GROWTH    = undefined_real
    noahmp%energy%param%DIRT_SOOT       = undefined_real
    noahmp%energy%param%EXTRA_GROWTH    = undefined_real
    noahmp%energy%param%BATS_COSZ       = undefined_real
    noahmp%energy%param%BATS_VIS_NEW    = undefined_real
    noahmp%energy%param%BATS_NIR_NEW    = undefined_real
    noahmp%energy%param%BATS_VIS_AGE    = undefined_real
    noahmp%energy%param%BATS_NIR_AGE    = undefined_real
    noahmp%energy%param%BATS_VIS_DIR    = undefined_real
    noahmp%energy%param%BATS_NIR_DIR    = undefined_real
    noahmp%energy%param%CLASS_ALB_REF   = undefined_real
    noahmp%energy%param%CLASS_SNO_AGE   = undefined_real
    noahmp%energy%param%CLASS_ALB_NEW   = undefined_real
    noahmp%energy%param%BP              = undefined_real
    noahmp%energy%param%KC25            = undefined_real
    noahmp%energy%param%KO25            = undefined_real
    noahmp%energy%param%AKC             = undefined_real
    noahmp%energy%param%AKO             = undefined_real
    noahmp%energy%param%RGL             = undefined_real
    noahmp%energy%param%RSMIN           = undefined_real
    noahmp%energy%param%RSMAX           = undefined_real
    noahmp%energy%param%TOPT            = undefined_real
    noahmp%energy%param%HS              = undefined_real
    noahmp%energy%param%DLEAF           = undefined_real
    noahmp%energy%param%CZIL            = undefined_real
    noahmp%energy%param%SNOW_EMIS       = undefined_real
    noahmp%energy%param%CWPVT           = undefined_real
    noahmp%energy%param%Z0SNO           = undefined_real
    noahmp%energy%param%Z0SOIL          = undefined_real
    noahmp%energy%param%Z0LAKE          = undefined_real
    noahmp%energy%param%EICE            = undefined_real
    noahmp%energy%param%RSURF_EXP       = undefined_real
    noahmp%energy%param%RSURF_SNOW      = undefined_real
    noahmp%energy%param%SHDMAX          = undefined_real
    noahmp%energy%param%SHDFAC          = undefined_real
    
    if( .not. allocated( noahmp%energy%param%LAIM   ) ) allocate( noahmp%energy%param%LAIM   (1:12   ) )
    if( .not. allocated( noahmp%energy%param%SAIM   ) ) allocate( noahmp%energy%param%SAIM   (1:12   ) )
    if( .not. allocated( noahmp%energy%param%SoilQuartzFrac ) ) allocate( noahmp%energy%param%SoilQuartzFrac (1:NumSoilLayer) )
    if( .not. allocated( noahmp%energy%param%ALBSAT ) ) allocate( noahmp%energy%param%ALBSAT (1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%param%ALBDRY ) ) allocate( noahmp%energy%param%ALBDRY (1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%param%ALBLAK ) ) allocate( noahmp%energy%param%ALBLAK (1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%param%OMEGAS ) ) allocate( noahmp%energy%param%OMEGAS (1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%param%RHOL   ) ) allocate( noahmp%energy%param%RHOL   (1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%param%RHOS   ) ) allocate( noahmp%energy%param%RHOS   (1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%param%TAUL   ) ) allocate( noahmp%energy%param%TAUL   (1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%param%TAUS   ) ) allocate( noahmp%energy%param%TAUS   (1:NumSWRadBand) )
    if( .not. allocated( noahmp%energy%param%EG     ) ) allocate( noahmp%energy%param%EG     (1:2    ) )
    if( .not. allocated( noahmp%energy%param%ALBICE ) ) allocate( noahmp%energy%param%ALBICE (1:NumSWRadBand) )
    
    noahmp%energy%param%LAIM(:)         = undefined_real
    noahmp%energy%param%SAIM(:)         = undefined_real
    noahmp%energy%param%SoilQuartzFrac(:)       = undefined_real
    noahmp%energy%param%ALBSAT(:)       = undefined_real
    noahmp%energy%param%ALBDRY(:)       = undefined_real
    noahmp%energy%param%ALBLAK(:)       = undefined_real
    noahmp%energy%param%OMEGAS(:)       = undefined_real
    noahmp%energy%param%RHOL(:)         = undefined_real
    noahmp%energy%param%RHOS(:)         = undefined_real
    noahmp%energy%param%TAUL(:)         = undefined_real
    noahmp%energy%param%TAUS(:)         = undefined_real
    noahmp%energy%param%EG(:)           = undefined_real
    noahmp%energy%param%ALBICE          = undefined_real
    
    end associate

  end subroutine EnergyVarInitDefault

!=== initialize with input data or table values

  subroutine EnergyVarInitTransfer(noahmp, NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout) :: NoahmpIO
    type(noahmp_type),   intent(inout) :: noahmp

    ! local loop index
    integer                          :: ISOIL

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
    noahmp%energy%param%RC                 = NoahmpIO%RC_TABLE(VegType)
    noahmp%energy%param%HVT                = NoahmpIO%HVT_TABLE(VegType)
    noahmp%energy%param%HVB                = NoahmpIO%HVB_TABLE(VegType)
    noahmp%energy%param%Z0MVT              = NoahmpIO%Z0MVT_TABLE(VegType)
    noahmp%energy%param%CWPVT              = NoahmpIO%CWPVT_TABLE(VegType)
    noahmp%energy%param%DEN                = NoahmpIO%DEN_TABLE(VegType)
    noahmp%energy%param%XL                 = NoahmpIO%XL_TABLE(VegType)
    noahmp%energy%param%BP                 = NoahmpIO%BP_TABLE(VegType)
    noahmp%energy%param%KC25               = NoahmpIO%KC25_TABLE(VegType)
    noahmp%energy%param%KO25               = NoahmpIO%KO25_TABLE(VegType)
    noahmp%energy%param%AKC                = NoahmpIO%AKC_TABLE(VegType)
    noahmp%energy%param%AKO                = NoahmpIO%AKO_TABLE(VegType)
    noahmp%energy%param%RGL                = NoahmpIO%RGL_TABLE(VegType)
    noahmp%energy%param%RSMIN              = NoahmpIO%RS_TABLE(VegType)
    noahmp%energy%param%RSMAX              = NoahmpIO%RSMAX_TABLE(VegType)
    noahmp%energy%param%TOPT               = NoahmpIO%TOPT_TABLE(VegType)
    noahmp%energy%param%HS                 = NoahmpIO%HS_TABLE(VegType)
    noahmp%energy%param%DLEAF              = NoahmpIO%DLEAF_TABLE(VegType)
    noahmp%energy%param%CSOIL              = NoahmpIO%CSOIL_TABLE
    noahmp%energy%param%TAU0               = NoahmpIO%TAU0_TABLE
    noahmp%energy%param%GRAIN_GROWTH       = NoahmpIO%GRAIN_GROWTH_TABLE
    noahmp%energy%param%DIRT_SOOT          = NoahmpIO%DIRT_SOOT_TABLE
    noahmp%energy%param%EXTRA_GROWTH       = NoahmpIO%EXTRA_GROWTH_TABLE
    noahmp%energy%param%BATS_COSZ          = NoahmpIO%BATS_COSZ_TABLE
    noahmp%energy%param%BATS_VIS_NEW       = NoahmpIO%BATS_VIS_NEW_TABLE
    noahmp%energy%param%BATS_NIR_NEW       = NoahmpIO%BATS_NIR_NEW_TABLE
    noahmp%energy%param%BATS_VIS_AGE       = NoahmpIO%BATS_VIS_AGE_TABLE
    noahmp%energy%param%BATS_NIR_AGE       = NoahmpIO%BATS_NIR_AGE_TABLE
    noahmp%energy%param%BATS_VIS_DIR       = NoahmpIO%BATS_VIS_DIR_TABLE
    noahmp%energy%param%BATS_NIR_DIR       = NoahmpIO%BATS_NIR_DIR_TABLE
    noahmp%energy%param%CLASS_ALB_REF      = NoahmpIO%CLASS_ALB_REF_TABLE
    noahmp%energy%param%CLASS_SNO_AGE      = NoahmpIO%CLASS_SNO_AGE_TABLE
    noahmp%energy%param%CLASS_ALB_NEW      = NoahmpIO%CLASS_ALB_NEW_TABLE
    noahmp%energy%param%BETADS             = NoahmpIO%BETADS_TABLE
    noahmp%energy%param%BETAIS             = NoahmpIO%BETAIS_TABLE
    noahmp%energy%param%CZIL               = NoahmpIO%CZIL_TABLE
    noahmp%energy%param%SNOW_EMIS          = NoahmpIO%SNOW_EMIS_TABLE
    noahmp%energy%param%EG                 = NoahmpIO%EG_TABLE
    noahmp%energy%param%ALBICE             = NoahmpIO%ALBICE_TABLE
    noahmp%energy%param%Z0SNO              = NoahmpIO%Z0SNO_TABLE
    noahmp%energy%param%Z0SOIL             = NoahmpIO%Z0SOIL_TABLE
    noahmp%energy%param%Z0LAKE             = NoahmpIO%Z0LAKE_TABLE
    noahmp%energy%param%EICE               = NoahmpIO%EICE_TABLE
    noahmp%energy%param%RSURF_EXP          = NoahmpIO%RSURF_EXP_TABLE
    noahmp%energy%param%RSURF_SNOW         = NoahmpIO%RSURF_SNOW_TABLE
    noahmp%energy%param%SHDMAX             = NoahmpIO%GVFMAX(I,J) / 100.0
    noahmp%energy%param%SHDFAC             = NoahmpIO%VEGFRA(I,J) / 100.0
    noahmp%energy%param%LAIM(1:12)         = NoahmpIO%LAIM_TABLE(VegType,1:12)
    noahmp%energy%param%SAIM(1:12)         = NoahmpIO%SAIM_TABLE(VegType,1:12)
    noahmp%energy%param%RHOL(1:NumSWRadBand)      = NoahmpIO%RHOL_TABLE(VegType,1:NumSWRadBand)
    noahmp%energy%param%RHOS(1:NumSWRadBand)      = NoahmpIO%RHOS_TABLE(VegType,1:NumSWRadBand)
    noahmp%energy%param%TAUL(1:NumSWRadBand)      = NoahmpIO%TAUL_TABLE(VegType,1:NumSWRadBand)
    noahmp%energy%param%TAUS(1:NumSWRadBand)      = NoahmpIO%TAUS_TABLE(VegType,1:NumSWRadBand)
    noahmp%energy%param%ALBSAT(1:NumSWRadBand)    = NoahmpIO%ALBSAT_TABLE(SoilColor,1:NumSWRadBand)
    noahmp%energy%param%ALBDRY(1:NumSWRadBand)    = NoahmpIO%ALBDRY_TABLE(SoilColor,1:NumSWRadBand)
    noahmp%energy%param%ALBLAK(1:NumSWRadBand)    = NoahmpIO%ALBLAK_TABLE(1:NumSWRadBand)
    noahmp%energy%param%OMEGAS(1:NumSWRadBand)    = NoahmpIO%OMEGAS_TABLE(1:NumSWRadBand)

    do ISOIL = 1, size(SoilType)
       noahmp%energy%param%SoilQuartzFrac(ISOIL)   = NoahmpIO%QUARTZ_TABLE(SoilType(ISOIL))
    enddo

    if ( FlagUrban .eqv. .true. ) then
       noahmp%energy%param%CSOIL = 3.0e6
    endif

    if ( CropType > 0 ) then
       noahmp%energy%param%BP              = NoahmpIO%BPI_TABLE(CropType)
       noahmp%energy%param%KC25            = NoahmpIO%KC25I_TABLE(CropType)
       noahmp%energy%param%KO25            = NoahmpIO%KO25I_TABLE(CropType)
       noahmp%energy%param%AKC             = NoahmpIO%AKCI_TABLE(CropType)
       noahmp%energy%param%AKO             = NoahmpIO%AKOI_TABLE(CropType)
    endif


    end associate

  end subroutine EnergyVarInitTransfer

end module EnergyVarInitMod
