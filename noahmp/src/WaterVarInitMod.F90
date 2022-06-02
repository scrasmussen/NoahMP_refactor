module WaterVarInitMod

!!! Initialize column (1-D) Noah-MP water variables
!!! Water variables should be first defined in WaterType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use NoahmpIOVarType
  use NoahmpVarType
  use PedoTransferSR2006
  use Machine, only : kind_noahmp

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
    noahmp%water%state%IRCNTSI        = huge(1  )
    noahmp%water%state%IRCNTMI        = huge(1  )
    noahmp%water%state%IRCNTFI        = huge(1  )
    noahmp%water%state%BDFALL         = huge(1.0)
    noahmp%water%state%CANLIQ         = huge(1.0)
    noahmp%water%state%CANICE         = huge(1.0)
    noahmp%water%state%CMC            = huge(1.0)
    noahmp%water%state%FWET           = huge(1.0)
    noahmp%water%state%MAXSNO         = huge(1.0)
    noahmp%water%state%MAXLIQ         = huge(1.0)
    noahmp%water%state%SNOWH          = huge(1.0)
    noahmp%water%state%SNEQV          = huge(1.0)
    noahmp%water%state%SNEQVO         = huge(1.0)
    noahmp%water%state%PONDING        = huge(1.0)
    noahmp%water%state%PONDING1       = huge(1.0)
    noahmp%water%state%PONDING2       = huge(1.0)
    noahmp%water%state%FIFAC          = huge(1.0)
    noahmp%water%state%IRAMTFI        = huge(1.0)
    noahmp%water%state%MIFAC          = huge(1.0)
    noahmp%water%state%IRAMTMI        = huge(1.0)
    noahmp%water%state%SIFAC          = huge(1.0)
    noahmp%water%state%IRAMTSI        = huge(1.0)
    noahmp%water%state%ZWT            = huge(1.0)
    noahmp%water%state%SICEMAX        = huge(1.0)
    noahmp%water%state%SH2OMIN        = huge(1.0)
    noahmp%water%state%FSAT           = huge(1.0)
    noahmp%water%state%FCRMAX         = huge(1.0)
    noahmp%water%state%SMCWTD         = huge(1.0)
    noahmp%water%state%DEEPRECH       = huge(1.0)
    noahmp%water%state%RECH           = huge(1.0)
    noahmp%water%state%WPLUS          = huge(1.0)
    noahmp%water%state%WATBLED        = huge(1.0)
    noahmp%water%state%TDFRACMP       = huge(1.0)
    noahmp%water%state%WA             = huge(1.0)
    noahmp%water%state%WT             = huge(1.0)
    noahmp%water%state%WSLAKE         = huge(1.0)
    noahmp%water%state%sfcheadrt      = huge(1.0)
    noahmp%water%state%IRRFRA         = huge(1.0)
    noahmp%water%state%SIFRA          = huge(1.0)
    noahmp%water%state%MIFRA          = huge(1.0)
    noahmp%water%state%FIFRA          = huge(1.0)
    noahmp%water%state%FP             = huge(1.0)
    noahmp%water%state%FSNO           = huge(1.0)
    noahmp%water%state%BTRAN          = huge(1.0)
    noahmp%water%state%FPICE          = huge(1.0)
    noahmp%water%state%WROOT          = huge(1.0)
    noahmp%water%state%WSTRES         = huge(1.0)
    noahmp%water%state%BEG_WB         = huge(1.0)
    noahmp%water%state%ERRWAT         = huge(1.0)
    noahmp%water%state%END_WB         = huge(1.0)

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

    noahmp%water%state%IMELT(:)       = huge(1)
    noahmp%water%state%SUPERCOOL(:)   = huge(1.0)
    noahmp%water%state%SNICE(:)       = huge(1.0)
    noahmp%water%state%SNLIQ(:)       = huge(1.0)
    noahmp%water%state%SNICEV(:)      = huge(1.0)
    noahmp%water%state%SNLIQV(:)      = huge(1.0)
    noahmp%water%state%FICEOLD_SNOW(:)= huge(1.0)
    noahmp%water%state%FICE_SNOW(:)   = huge(1.0)
    noahmp%water%state%FICE_SOIL(:)   = huge(1.0)
    noahmp%water%state%EPORE_SNOW(:)  = huge(1.0)
    noahmp%water%state%EPORE_SNOW2(:) = huge(1.0)
    noahmp%water%state%SH2O(:)        = huge(1.0)
    noahmp%water%state%SICE(:)        = huge(1.0)
    noahmp%water%state%SMC(:)         = huge(1.0)
    noahmp%water%state%FCR(:)         = huge(1.0)
    noahmp%water%state%WCND(:)        = huge(1.0)
    noahmp%water%state%WDF(:)         = huge(1.0)
    noahmp%water%state%EPORE_SOIL(:)  = huge(1.0)
    noahmp%water%state%SMCEQ(:)       = huge(1.0)
    noahmp%water%state%BTRANI(:)      = huge(1.0)
    noahmp%water%state%PSI(:)         = huge(1.0)

    ! water flux variable
    noahmp%water%flux%PRCP            = huge(1.0)
    noahmp%water%flux%RAIN            = huge(1.0)
    noahmp%water%flux%SNOW            = huge(1.0)
    noahmp%water%flux%QPRECC          = huge(1.0)
    noahmp%water%flux%QPRECL          = huge(1.0)
    noahmp%water%flux%ECAN            = huge(1.0)
    noahmp%water%flux%ETRAN           = huge(1.0)
    noahmp%water%flux%QEVAC           = huge(1.0)
    noahmp%water%flux%QDEWC           = huge(1.0)
    noahmp%water%flux%QFROC           = huge(1.0)
    noahmp%water%flux%QSUBC           = huge(1.0)
    noahmp%water%flux%QMELTC          = huge(1.0)
    noahmp%water%flux%QFRZC           = huge(1.0)
    noahmp%water%flux%QSNOW           = huge(1.0)
    noahmp%water%flux%SNOWHIN         = huge(1.0)
    noahmp%water%flux%QSNFRO          = huge(1.0)
    noahmp%water%flux%QSNSUB          = huge(1.0)
    noahmp%water%flux%QRAIN           = huge(1.0)
    noahmp%water%flux%QSNBOT          = huge(1.0)
    noahmp%water%flux%SNOFLOW         = huge(1.0)
    noahmp%water%flux%IRFIRATE        = 0.0
    noahmp%water%flux%IRMIRATE        = 0.0
    noahmp%water%flux%IRSIRATE        = 0.0
    noahmp%water%flux%IREVPLOS        = 0.0
    noahmp%water%flux%QINSUR          = huge(1.0)
    noahmp%water%flux%RUNSRF          = huge(1.0)
    noahmp%water%flux%RUNSUB          = huge(1.0)
    noahmp%water%flux%PDDUM           = huge(1.0)
    noahmp%water%flux%QSEVA           = huge(1.0)
    noahmp%water%flux%QDRAIN          = huge(1.0)
    noahmp%water%flux%QTLDRN          = huge(1.0)
    noahmp%water%flux%QIN             = huge(1.0)
    noahmp%water%flux%QDIS            = huge(1.0)
    noahmp%water%flux%QVAP            = huge(1.0)
    noahmp%water%flux%QDEW            = huge(1.0)
    noahmp%water%flux%QSDEW           = huge(1.0)
    noahmp%water%flux%EIRR            = 0.0
    noahmp%water%flux%QINTR           = huge(1.0)
    noahmp%water%flux%QDRIPR          = huge(1.0)
    noahmp%water%flux%QTHROR          = huge(1.0)
    noahmp%water%flux%QINTS           = huge(1.0)
    noahmp%water%flux%QDRIPS          = huge(1.0)
    noahmp%water%flux%QTHROS          = huge(1.0)
    noahmp%water%flux%EDIR            = huge(1.0)
    noahmp%water%flux%QMELT           = huge(1.0)
    noahmp%water%flux%QFX             = huge(1.0)

    if( .not. allocated( noahmp%water%flux%DDZ1   ) ) allocate( noahmp%water%flux%DDZ1   (-NSNOW+1:0) )
    if( .not. allocated( noahmp%water%flux%DDZ2   ) ) allocate( noahmp%water%flux%DDZ2   (-NSNOW+1:0) )
    if( .not. allocated( noahmp%water%flux%DDZ3   ) ) allocate( noahmp%water%flux%DDZ3   (-NSNOW+1:0) )
    if( .not. allocated( noahmp%water%flux%PDZDTC ) ) allocate( noahmp%water%flux%PDZDTC (-NSNOW+1:0) )
    if( .not. allocated( noahmp%water%flux%ETRANI ) ) allocate( noahmp%water%flux%ETRANI ( 1:NSOIL  ) )

    noahmp%water%flux%DDZ1(:)           = huge(1.0)
    noahmp%water%flux%DDZ2(:)           = huge(1.0)
    noahmp%water%flux%DDZ3(:)           = huge(1.0)
    noahmp%water%flux%PDZDTC(:)         = huge(1.0)
    noahmp%water%flux%ETRANI(:)         = huge(1.0)

    ! water parameter variable
    noahmp%water%param%DRAIN_LAYER_OPT  = huge(1  )
    noahmp%water%param%TD_DEPTH         = huge(1  )
    noahmp%water%param%NROOT            = huge(1  )
    noahmp%water%param%IRR_HAR          = huge(1  )
    noahmp%water%param%CH2OP            = huge(1.0)
    noahmp%water%param%C2_SnowCompact   = huge(1.0)
    noahmp%water%param%C3_SnowCompact   = huge(1.0)
    noahmp%water%param%C4_SnowCompact   = huge(1.0)
    noahmp%water%param%C5_SnowCompact   = huge(1.0)
    noahmp%water%param%DM_SnowCompact   = huge(1.0)
    noahmp%water%param%ETA0_SnowCompact = huge(1.0)
    noahmp%water%param%SNLIQMAXFRAC     = huge(1.0)
    noahmp%water%param%SSI              = huge(1.0)
    noahmp%water%param%SNOW_RET_FAC     = huge(1.0)
    noahmp%water%param%FIRTFAC          = huge(1.0)
    noahmp%water%param%MICIR_RATE       = huge(1.0)
    noahmp%water%param%KDT              = huge(1.0)
    noahmp%water%param%FRZX             = huge(1.0)
    noahmp%water%param%BVIC             = huge(1.0)
    noahmp%water%param%AXAJ             = huge(1.0)
    noahmp%water%param%BXAJ             = huge(1.0)
    noahmp%water%param%XXAJ             = huge(1.0)
    noahmp%water%param%BBVIC            = huge(1.0)
    noahmp%water%param%GDVIC            = huge(1.0)
    noahmp%water%param%BDVIC            = huge(1.0)
    noahmp%water%param%SLOPE            = huge(1.0)
    noahmp%water%param%TD_DC            = huge(1.0)
    noahmp%water%param%TDSMC_FAC        = huge(1.0)
    noahmp%water%param%TD_DCOEF         = huge(1.0)
    noahmp%water%param%TD_ADEPTH        = huge(1.0)
    noahmp%water%param%KLAT_FAC         = huge(1.0)
    noahmp%water%param%TD_DDRAIN        = huge(1.0)
    noahmp%water%param%TD_SPAC          = huge(1.0)
    noahmp%water%param%TD_RADI          = huge(1.0)
    noahmp%water%param%TD_D             = huge(1.0)
    noahmp%water%param%FFF              = huge(1.0)
    noahmp%water%param%RSBMX            = huge(1.0)
    noahmp%water%param%TIMEAN           = huge(1.0)
    noahmp%water%param%FSATMX           = huge(1.0)
    noahmp%water%param%ROUS             = huge(1.0)
    noahmp%water%param%CMIC             = huge(1.0)
    noahmp%water%param%WSLMAX           = huge(1.0)
    noahmp%water%param%SWEMAXGLA        = huge(1.0)
    noahmp%water%param%REFDK            = huge(1.0)
    noahmp%water%param%REFKDT           = huge(1.0)
    noahmp%water%param%FRZK             = huge(1.0)
    noahmp%water%param%IRR_LAI          = huge(1.0)
    noahmp%water%param%IRR_MAD          = huge(1.0)
    noahmp%water%param%FILOSS           = huge(1.0)
    noahmp%water%param%SPRIR_RATE       = huge(1.0)
    noahmp%water%param%IRR_FRAC         = huge(1.0)
    noahmp%water%param%IR_RAIN          = huge(1.0)
    noahmp%water%param%SNOWDEN_MIN      = huge(1.0)
    noahmp%water%param%SWEMX            = huge(1.0)
    noahmp%water%param%PSIWLT           = huge(1.0)
    noahmp%water%param%MFSNO            = huge(1.0)
    noahmp%water%param%SCFFAC           = huge(1.0)

    if( .not. allocated( noahmp%water%param%SMCMAX ) ) allocate( noahmp%water%param%SMCMAX (1:NSOIL) )
    if( .not. allocated( noahmp%water%param%SMCWLT ) ) allocate( noahmp%water%param%SMCWLT (1:NSOIL) )
    if( .not. allocated( noahmp%water%param%SMCREF ) ) allocate( noahmp%water%param%SMCREF (1:NSOIL) )
    if( .not. allocated( noahmp%water%param%SMCDRY ) ) allocate( noahmp%water%param%SMCDRY (1:NSOIL) )
    if( .not. allocated( noahmp%water%param%DWSAT  ) ) allocate( noahmp%water%param%DWSAT  (1:NSOIL) )
    if( .not. allocated( noahmp%water%param%DKSAT  ) ) allocate( noahmp%water%param%DKSAT  (1:NSOIL) )
    if( .not. allocated( noahmp%water%param%BEXP   ) ) allocate( noahmp%water%param%BEXP   (1:NSOIL) )
    if( .not. allocated( noahmp%water%param%PSISAT ) ) allocate( noahmp%water%param%PSISAT (1:NSOIL) )
    if( .not. allocated( noahmp%water%param%QUARTZ ) ) allocate( noahmp%water%param%QUARTZ (1:NSOIL) )

    noahmp%water%param%SMCMAX(:)        = huge(1.0)
    noahmp%water%param%SMCWLT(:)        = huge(1.0)
    noahmp%water%param%SMCREF(:)        = huge(1.0)
    noahmp%water%param%SMCDRY(:)        = huge(1.0)
    noahmp%water%param%DWSAT (:)        = huge(1.0)
    noahmp%water%param%DKSAT (:)        = huge(1.0)
    noahmp%water%param%BEXP  (:)        = huge(1.0)
    noahmp%water%param%PSISAT(:)        = huge(1.0)
    noahmp%water%param%QUARTZ(:)        = huge(1.0)
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

    if(NoahmpIO%iopt_soil == 3 .and. .not. noahmp%config%domain%urban_flag) then
       sand = 0.01 * NoahmpIO%soilcomp(I,1:4,J)
       clay = 0.01 * NoahmpIO%soilcomp(I,5:8,J)
       orgm = 0.0

       if(NoahmpIO%iopt_pedo == 1) call pedotransfer_sr2006(NoahmpIO,noahmp,              &
                                                            noahmp%config%domain%SOILTYP, &
                                                            sand,clay,orgm)
    end if

    end associate

  end subroutine WaterVarInitTransfer

end module WaterVarInitMod
