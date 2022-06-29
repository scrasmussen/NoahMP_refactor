module WaterVarInitMod

!!! Initialize column (1-D) Noah-MP water variables
!!! Water variables should be first defined in WaterType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
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

    associate(                                      &
              NSNOW => noahmp%config%domain%NSNOW  ,&
              NSOIL => noahmp%config%domain%NSOIL   &
             )

    ! water state variable
    noahmp%water%state%IRCNTSI        = undefined_int
    noahmp%water%state%IRCNTMI        = undefined_int
    noahmp%water%state%IRCNTFI        = undefined_int
    noahmp%water%state%BDFALL         = undefined_real
    noahmp%water%state%CANLIQ         = undefined_real
    noahmp%water%state%CANICE         = undefined_real
    noahmp%water%state%CMC            = undefined_real
    noahmp%water%state%FWET           = undefined_real
    noahmp%water%state%MAXSNO         = undefined_real
    noahmp%water%state%MAXLIQ         = undefined_real
    noahmp%water%state%SNOWH          = undefined_real
    noahmp%water%state%SNEQV          = undefined_real
    noahmp%water%state%SNEQVO         = undefined_real
    noahmp%water%state%PONDING        = undefined_real
    noahmp%water%state%PONDING1       = undefined_real
    noahmp%water%state%PONDING2       = undefined_real
    noahmp%water%state%FIFAC          = undefined_real
    noahmp%water%state%IRAMTFI        = undefined_real
    noahmp%water%state%MIFAC          = undefined_real
    noahmp%water%state%IRAMTMI        = undefined_real
    noahmp%water%state%SIFAC          = undefined_real
    noahmp%water%state%IRAMTSI        = undefined_real
    noahmp%water%state%ZWT            = undefined_real
    noahmp%water%state%SICEMAX        = undefined_real
    noahmp%water%state%SH2OMIN        = undefined_real
    noahmp%water%state%FSAT           = undefined_real
    noahmp%water%state%FCRMAX         = undefined_real
    noahmp%water%state%SMCWTD         = undefined_real
    noahmp%water%state%DEEPRECH       = undefined_real
    noahmp%water%state%RECH           = undefined_real
    noahmp%water%state%WPLUS          = undefined_real
    noahmp%water%state%WATBLED        = undefined_real
    noahmp%water%state%TDFRACMP       = undefined_real
    noahmp%water%state%WA             = undefined_real
    noahmp%water%state%WT             = undefined_real
    noahmp%water%state%WSLAKE         = undefined_real
    noahmp%water%state%sfcheadrt      = undefined_real
    noahmp%water%state%IRRFRA         = undefined_real
    noahmp%water%state%SIFRA          = undefined_real
    noahmp%water%state%MIFRA          = undefined_real
    noahmp%water%state%FIFRA          = undefined_real
    noahmp%water%state%FP             = undefined_real
    noahmp%water%state%FSNO           = undefined_real
    noahmp%water%state%BTRAN          = undefined_real
    noahmp%water%state%FPICE          = undefined_real
    noahmp%water%state%WROOT          = undefined_real
    noahmp%water%state%WSTRES         = undefined_real
    noahmp%water%state%BEG_WB         = undefined_real
    noahmp%water%state%ERRWAT         = undefined_real
    noahmp%water%state%END_WB         = undefined_real

    if( .not. allocated( noahmp%water%state%IMELT        ) ) allocate( noahmp%water%state%IMELT        (-NSNOW+1:NSOIL) )
    if( .not. allocated( noahmp%water%state%SUPERCOOL    ) ) allocate( noahmp%water%state%SUPERCOOL    (-NSNOW+1:NSOIL) )
    if( .not. allocated( noahmp%water%state%SNICE        ) ) allocate( noahmp%water%state%SNICE        (-NSNOW+1:0)     )
    if( .not. allocated( noahmp%water%state%SNLIQ        ) ) allocate( noahmp%water%state%SNLIQ        (-NSNOW+1:0)     )
    if( .not. allocated( noahmp%water%state%SNICEV       ) ) allocate( noahmp%water%state%SNICEV       (-NSNOW+1:0)     )
    if( .not. allocated( noahmp%water%state%SNLIQV       ) ) allocate( noahmp%water%state%SNLIQV       (-NSNOW+1:0)     )
    if( .not. allocated( noahmp%water%state%FICEOLD_SNOW ) ) allocate( noahmp%water%state%FICEOLD_SNOW (-NSNOW+1:0)     )
    if( .not. allocated( noahmp%water%state%FICE_SNOW    ) ) allocate( noahmp%water%state%FICE_SNOW    (-NSNOW+1:0)     )
    if( .not. allocated( noahmp%water%state%EPORE_SNOW   ) ) allocate( noahmp%water%state%EPORE_SNOW   (-NSNOW+1:0)     )
    if( .not. allocated( noahmp%water%state%EPORE_SNOW2  ) ) allocate( noahmp%water%state%EPORE_SNOW2  (-NSNOW+1:0)     )
    if( .not. allocated( noahmp%water%state%SH2O         ) ) allocate( noahmp%water%state%SH2O         (       1:NSOIL) )
    if( .not. allocated( noahmp%water%state%SICE         ) ) allocate( noahmp%water%state%SICE         (       1:NSOIL) )
    if( .not. allocated( noahmp%water%state%SMC          ) ) allocate( noahmp%water%state%SMC          (       1:NSOIL) )
    if( .not. allocated( noahmp%water%state%FCR          ) ) allocate( noahmp%water%state%FCR          (       1:NSOIL) )
    if( .not. allocated( noahmp%water%state%WCND         ) ) allocate( noahmp%water%state%WCND         (       1:NSOIL) )
    if( .not. allocated( noahmp%water%state%WDF          ) ) allocate( noahmp%water%state%WDF          (       1:NSOIL) )
    if( .not. allocated( noahmp%water%state%EPORE_SOIL   ) ) allocate( noahmp%water%state%EPORE_SOIL   (       1:NSOIL) )
    if( .not. allocated( noahmp%water%state%FICE_SOIL    ) ) allocate( noahmp%water%state%FICE_SOIL    (       1:NSOIL) )
    if( .not. allocated( noahmp%water%state%SMCEQ        ) ) allocate( noahmp%water%state%SMCEQ        (       1:NSOIL) )
    if( .not. allocated( noahmp%water%state%BTRANI       ) ) allocate( noahmp%water%state%BTRANI       (       1:NSOIL) )
    if( .not. allocated( noahmp%water%state%PSI          ) ) allocate( noahmp%water%state%PSI          (       1:NSOIL) )

    noahmp%water%state%IMELT(:)       = undefined_int
    noahmp%water%state%SUPERCOOL(:)   = undefined_real
    noahmp%water%state%SNICE(:)       = undefined_real
    noahmp%water%state%SNLIQ(:)       = undefined_real
    noahmp%water%state%SNICEV(:)      = undefined_real
    noahmp%water%state%SNLIQV(:)      = undefined_real
    noahmp%water%state%FICEOLD_SNOW(:)= undefined_real
    noahmp%water%state%FICE_SNOW(:)   = undefined_real
    noahmp%water%state%FICE_SOIL(:)   = undefined_real
    noahmp%water%state%EPORE_SNOW(:)  = undefined_real
    noahmp%water%state%EPORE_SNOW2(:) = undefined_real
    noahmp%water%state%SH2O(:)        = undefined_real
    noahmp%water%state%SICE(:)        = undefined_real
    noahmp%water%state%SMC(:)         = undefined_real
    noahmp%water%state%FCR(:)         = undefined_real
    noahmp%water%state%WCND(:)        = undefined_real
    noahmp%water%state%WDF(:)         = undefined_real
    noahmp%water%state%EPORE_SOIL(:)  = undefined_real
    noahmp%water%state%SMCEQ(:)       = undefined_real
    noahmp%water%state%BTRANI(:)      = undefined_real
    noahmp%water%state%PSI(:)         = undefined_real

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
    noahmp%water%flux%IRFIRATE        = 0.0
    noahmp%water%flux%IRMIRATE        = 0.0
    noahmp%water%flux%IRSIRATE        = 0.0
    noahmp%water%flux%IREVPLOS        = 0.0
    noahmp%water%flux%QINSUR          = undefined_real
    noahmp%water%flux%RUNSRF          = undefined_real
    noahmp%water%flux%RUNSUB          = undefined_real
    noahmp%water%flux%PDDUM           = undefined_real
    noahmp%water%flux%QSEVA           = undefined_real
    noahmp%water%flux%QDRAIN          = undefined_real
    noahmp%water%flux%QTLDRN          = undefined_real
    noahmp%water%flux%QIN             = undefined_real
    noahmp%water%flux%QDIS            = undefined_real
    noahmp%water%flux%QVAP            = undefined_real
    noahmp%water%flux%QDEW            = undefined_real
    noahmp%water%flux%QSDEW           = undefined_real
    noahmp%water%flux%EIRR            = 0.0
    noahmp%water%flux%QINTR           = undefined_real
    noahmp%water%flux%QDRIPR          = undefined_real
    noahmp%water%flux%QTHROR          = undefined_real
    noahmp%water%flux%QINTS           = undefined_real
    noahmp%water%flux%QDRIPS          = undefined_real
    noahmp%water%flux%QTHROS          = undefined_real
    noahmp%water%flux%EDIR            = undefined_real
    noahmp%water%flux%QMELT           = undefined_real
    noahmp%water%flux%QFX             = undefined_real

    if( .not. allocated( noahmp%water%flux%DDZ1   ) ) allocate( noahmp%water%flux%DDZ1   (-NSNOW+1:0) )
    if( .not. allocated( noahmp%water%flux%DDZ2   ) ) allocate( noahmp%water%flux%DDZ2   (-NSNOW+1:0) )
    if( .not. allocated( noahmp%water%flux%DDZ3   ) ) allocate( noahmp%water%flux%DDZ3   (-NSNOW+1:0) )
    if( .not. allocated( noahmp%water%flux%PDZDTC ) ) allocate( noahmp%water%flux%PDZDTC (-NSNOW+1:0) )
    if( .not. allocated( noahmp%water%flux%ETRANI ) ) allocate( noahmp%water%flux%ETRANI ( 1:NSOIL  ) )

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

    if( .not. allocated( noahmp%water%param%SMCMAX ) ) allocate( noahmp%water%param%SMCMAX (1:NSOIL) )
    if( .not. allocated( noahmp%water%param%SMCWLT ) ) allocate( noahmp%water%param%SMCWLT (1:NSOIL) )
    if( .not. allocated( noahmp%water%param%SMCREF ) ) allocate( noahmp%water%param%SMCREF (1:NSOIL) )
    if( .not. allocated( noahmp%water%param%SMCDRY ) ) allocate( noahmp%water%param%SMCDRY (1:NSOIL) )
    if( .not. allocated( noahmp%water%param%DWSAT  ) ) allocate( noahmp%water%param%DWSAT  (1:NSOIL) )
    if( .not. allocated( noahmp%water%param%DKSAT  ) ) allocate( noahmp%water%param%DKSAT  (1:NSOIL) )
    if( .not. allocated( noahmp%water%param%BEXP   ) ) allocate( noahmp%water%param%BEXP   (1:NSOIL) )
    if( .not. allocated( noahmp%water%param%PSISAT ) ) allocate( noahmp%water%param%PSISAT (1:NSOIL) )
    if( .not. allocated( noahmp%water%param%QUARTZ ) ) allocate( noahmp%water%param%QUARTZ (1:NSOIL) )

    noahmp%water%param%SMCMAX(:)        = undefined_real
    noahmp%water%param%SMCWLT(:)        = undefined_real
    noahmp%water%param%SMCREF(:)        = undefined_real
    noahmp%water%param%SMCDRY(:)        = undefined_real
    noahmp%water%param%DWSAT (:)        = undefined_real
    noahmp%water%param%DKSAT (:)        = undefined_real
    noahmp%water%param%BEXP  (:)        = undefined_real
    noahmp%water%param%PSISAT(:)        = undefined_real
    noahmp%water%param%QUARTZ(:)        = undefined_real
    ! water diagnostic variable


    end associate

  end subroutine WaterVarInitDefault

!=== initialize with input data or table values

  subroutine WaterVarInitTransfer(noahmp, NoahmpIO)

    implicit none

    type(noahmp_type),   intent(inout) :: noahmp
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

    ! local loop 
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil ) :: SAND
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil ) :: CLAY
    real(kind=kind_noahmp), dimension( 1:NoahmpIO%nsoil ) :: ORGM
    integer                                               :: ISOIL

    associate(                                                  &
              I           => noahmp%config%domain%ILOC         ,&
              J           => noahmp%config%domain%JLOC         ,&
              NSNOW       => noahmp%config%domain%NSNOW        ,&
              NSOIL       => noahmp%config%domain%NSOIL        ,&
              VEGTYP      => noahmp%config%domain%VEGTYP       ,&
              SOILTYP     => noahmp%config%domain%SOILTYP      ,&
              URBAN_FLAG  => noahmp%config%domain%URBAN_FLAG   ,&
              ISNOW       => noahmp%config%domain%ISNOW         &
             )

    ! water state variable
    noahmp%water%state%CANLIQ                   = NoahmpIO%CANLIQXY (I,J)
    noahmp%water%state%CANICE                   = NoahmpIO%CANICEXY (I,J)
    noahmp%water%state%FWET                     = NoahmpIO%FWETXY (I,J)
    noahmp%water%state%SNEQV                    = NoahmpIO%SNOW (I,J)
    noahmp%water%state%SNEQVO                   = NoahmpIO%SNEQVOXY(I,J) 
    noahmp%water%state%SNOWH                    = NoahmpIO%SNOWH (I,J)
    noahmp%water%state%SNICE(-NSNOW+1:0)        = NoahmpIO%SNICEXY (I,-NSNOW+1:    0,J)
    noahmp%water%state%SNLIQ(-NSNOW+1:0)        = NoahmpIO%SNLIQXY (I,-NSNOW+1:    0,J)
    noahmp%water%state%SH2O (1:NSOIL)           = NoahmpIO%SH2O    (I,1:NSOIL,J)
    noahmp%water%state%SMC  (1:NSOIL)           = NoahmpIO%SMOIS   (I,1:NSOIL,J) 
    noahmp%water%state%SMCEQ(1:NSOIL)           = NoahmpIO%SMOISEQ (I,1:NSOIL,J)
    noahmp%water%state%FIFRA                    = NoahmpIO%FIFRACT (I,J)
    noahmp%water%state%IRAMTFI                  = NoahmpIO%IRWATFI (I,J)
    noahmp%water%state%MIFRA                    = NoahmpIO%MIFRACT (I,J)
    noahmp%water%state%IRAMTMI                  = NoahmpIO%IRWATMI (I,J) 
    noahmp%water%state%SIFRA                    = NoahmpIO%SIFRACT (I,J)
    noahmp%water%state%IRAMTSI                  = NoahmpIO%IRWATSI (I,J)  
    noahmp%water%state%ZWT                      = NoahmpIO%ZWTXY   (I,J) 
    noahmp%water%state%SMCWTD                   = NoahmpIO%SMCWTDXY(I,J)
    noahmp%water%state%DEEPRECH                 = 0.0
    noahmp%water%state%RECH                     = 0.0
#ifdef WRF_HYDRO
    noahmp%water%state%WATBLED                  = NoahmpIO%ZWATBLE2D (I,J)
    noahmp%water%state%sfcheadrt                = NoahmpIO%sfcheadrt (I,J)
#endif
    noahmp%water%state%TDFRACMP                 = NoahmpIO%TD_FRACTION (I,J)
    noahmp%water%state%WA                       = NoahmpIO%WAXY    (I,J)
    noahmp%water%state%WT                       = NoahmpIO%WTXY    (I,J)
    noahmp%water%state%WSLAKE                   = NoahmpIO%WSLAKEXY(I,J) 
    noahmp%water%state%IRRFRA                   = NoahmpIO%IRFRACT (I,J)
    noahmp%water%state%IRCNTSI                  = NoahmpIO%IRNUMSI (I,J)  
    noahmp%water%state%IRCNTMI                  = NoahmpIO%IRNUMMI (I,J)
    noahmp%water%state%IRCNTFI                  = NoahmpIO%IRNUMFI (I,J)

    ! water state variable
    noahmp%water%flux%QSNOW                     = NoahmpIO%QSNOWXY (I,J) 
    noahmp%water%flux%QRAIN                     = NoahmpIO%QRAINXY (I,J)
    noahmp%water%flux%QTLDRN                    = 0.0                     
    
    ! water parameter variable
    noahmp%water%param%DRAIN_LAYER_OPT   = NoahmpIO%DRAIN_LAYER_OPT_TABLE
    noahmp%water%param%CH2OP             = NoahmpIO%CH2OP_TABLE(VEGTYP)
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
    noahmp%water%param%MFSNO             = NoahmpIO%MFSNO_TABLE(VEGTYP)
    noahmp%water%param%SCFFAC            = NoahmpIO%SCFFAC_TABLE(VEGTYP)

    noahmp%water%param%BVIC              = NoahmpIO%BVIC_TABLE(SOILTYP(1))
    noahmp%water%param%AXAJ              = NoahmpIO%AXAJ_TABLE(SOILTYP(1))
    noahmp%water%param%BXAJ              = NoahmpIO%BXAJ_TABLE(SOILTYP(1))
    noahmp%water%param%XXAJ              = NoahmpIO%XXAJ_TABLE(SOILTYP(1))
    noahmp%water%param%BBVIC             = NoahmpIO%BBVIC_TABLE(SOILTYP(1))
    noahmp%water%param%GDVIC             = NoahmpIO%GDVIC_TABLE(SOILTYP(1))
    noahmp%water%param%BDVIC             = NoahmpIO%BDVIC_TABLE(SOILTYP(1))
    noahmp%water%param%SLOPE             = NoahmpIO%SLOPE_TABLE(NoahmpIO%SLOPETYP)
    noahmp%water%param%TD_DC             = NoahmpIO%TD_DC_TABLE(SOILTYP(1))
    noahmp%water%param%TD_DEPTH          = NoahmpIO%TD_DEPTH_TABLE(SOILTYP(1))
    noahmp%water%param%TDSMC_FAC         = NoahmpIO%TDSMC_FAC_TABLE(SOILTYP(1))
    noahmp%water%param%TD_DCOEF          = NoahmpIO%TD_DCOEF_TABLE(SOILTYP(1))
    noahmp%water%param%TD_ADEPTH         = NoahmpIO%TD_ADEPTH_TABLE(SOILTYP(1))
    noahmp%water%param%KLAT_FAC          = NoahmpIO%KLAT_FAC_TABLE(SOILTYP(1))
    noahmp%water%param%TD_DDRAIN         = NoahmpIO%TD_DDRAIN_TABLE(SOILTYP(1))
    noahmp%water%param%TD_SPAC           = NoahmpIO%TD_SPAC_TABLE(SOILTYP(1))
    noahmp%water%param%TD_RADI           = NoahmpIO%TD_RADI_TABLE(SOILTYP(1))
    noahmp%water%param%TD_D              = NoahmpIO%TD_D_TABLE(SOILTYP(1))
    noahmp%water%param%NROOT             = NoahmpIO%NROOT_TABLE(VEGTYP)

    do ISOIL = 1, size(SOILTYP)
       noahmp%water%param%SMCMAX(ISOIL)   = NoahmpIO%SMCMAX_TABLE(SOILTYP(ISOIL))
       noahmp%water%param%SMCWLT(ISOIL)   = NoahmpIO%SMCWLT_TABLE(SOILTYP(ISOIL))
       noahmp%water%param%SMCREF(ISOIL)   = NoahmpIO%SMCREF_TABLE(SOILTYP(ISOIL))
       noahmp%water%param%SMCDRY(ISOIL)   = NoahmpIO%SMCDRY_TABLE(SOILTYP(ISOIL))
       noahmp%water%param%DWSAT(ISOIL)    = NoahmpIO%DWSAT_TABLE(SOILTYP(ISOIL))
       noahmp%water%param%DKSAT(ISOIL)    = NoahmpIO%DKSAT_TABLE(SOILTYP(ISOIL))
       noahmp%water%param%BEXP(ISOIL)     = NoahmpIO%BEXP_TABLE(SOILTYP(ISOIL))
       noahmp%water%param%PSISAT(ISOIL)   = NoahmpIO%PSISAT_TABLE(SOILTYP(ISOIL))
       noahmp%water%param%QUARTZ(ISOIL)   = NoahmpIO%QUARTZ_TABLE(SOILTYP(ISOIL))
    enddo

    ! derived parameters
    noahmp%water%param%KDT  = noahmp%water%param%REFKDT * noahmp%water%param%DKSAT(1) / &
                              noahmp%water%param%REFDK
    if ( URBAN_FLAG .eqv. .true. ) then
       noahmp%water%param%SMCMAX = 0.45
       noahmp%water%param%SMCREF = 0.42
       noahmp%water%param%SMCWLT = 0.40
       noahmp%water%param%SMCDRY = 0.40
    endif
    if ( SOILTYP(1) /= 14 ) then
       noahmp%water%param%FRZX = noahmp%water%param%FRZK * &
            ((noahmp%water%param%SMCMAX(1) / noahmp%water%param%SMCREF(1)) * (0.412/0.468))
    endif

    noahmp%water%state%FICEOLD_SNOW            = 0.0
    noahmp%water%state%FICEOLD_SNOW(ISNOW+1:0) = NoahmpIO%SNICEXY(I,ISNOW+1:0,J)   &  ! snow ice fraction  
                                                 /(NoahmpIO%SNICEXY(I,ISNOW+1:0,J) &
                                                 + NoahmpIO%SNLIQXY(I,ISNOW+1:0,J))

    if( (noahmp%config%nmlist%OptSoilProperty == 3) .and. (.not. noahmp%config%domain%urban_flag) ) then
       sand = 0.01 * NoahmpIO%soilcomp(I,1:4,J)
       clay = 0.01 * NoahmpIO%soilcomp(I,5:8,J)
       orgm = 0.0
       if (noahmp%config%nmlist%OptPedotransfer == 1) &
         call pedotransfer_sr2006(NoahmpIO,noahmp,noahmp%config%domain%SOILTYP,sand,clay,orgm)
    endif

    end associate

  end subroutine WaterVarInitTransfer

end module WaterVarInitMod
