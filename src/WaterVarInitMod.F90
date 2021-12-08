module WaterVarInitMod

!!! Initialize column (1-D) Noah-MP water variables
!!! Water variables should be first defined in WaterType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use InputVarType
  use NoahmpVarType

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
    noahmp%water%state%BDFALL         = huge(1.0)
    noahmp%water%state%CANLIQ         = huge(1.0)
    noahmp%water%state%CANICE         = huge(1.0)
    noahmp%water%state%CMC            = huge(1.0)
    noahmp%water%state%FWET           = huge(1.0)
    noahmp%water%state%MAXSNO         = huge(1.0)
    noahmp%water%state%MAXLIQ         = huge(1.0)
    noahmp%water%state%SNOWH          = huge(1.0)
    noahmp%water%state%SNEQV          = huge(1.0)
    noahmp%water%state%PONDING        = huge(1.0)
    noahmp%water%state%PONDING1       = huge(1.0)
    noahmp%water%state%PONDING2       = huge(1.0)
    noahmp%water%state%FIFAC          = huge(1.0)
    noahmp%water%state%IRAMTFI        = huge(1.0)
    noahmp%water%state%MIFAC          = huge(1.0)
    noahmp%water%state%IRAMTMI        = huge(1.0)
    noahmp%water%state%SIFAC          = huge(1.0)
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

    allocate( noahmp%water%state%IMELT        (-NSNOW+1:NSOIL) )
    allocate( noahmp%water%state%SNICE        (-NSNOW+1:0)     )
    allocate( noahmp%water%state%SNLIQ        (-NSNOW+1:0)     )
    allocate( noahmp%water%state%FICEOLD_SNOW (-NSNOW+1:0)     )
    allocate( noahmp%water%state%FICE_SNOW    (-NSNOW+1:0)     )
    allocate( noahmp%water%state%EPORE_SNOW   (-NSNOW+1:0)     )
    allocate( noahmp%water%state%SH2O         (       1:NSOIL) )
    allocate( noahmp%water%state%SICE         (       1:NSOIL) )
    allocate( noahmp%water%state%SMC          (       1:NSOIL) )
    allocate( noahmp%water%state%FCR          (       1:NSOIL) )
    allocate( noahmp%water%state%WCND         (       1:NSOIL) )
    allocate( noahmp%water%state%WDF          (       1:NSOIL) )
    allocate( noahmp%water%state%EPORE_SOIL   (       1:NSOIL) )
    allocate( noahmp%water%state%FICE_SOIL    (       1:NSOIL) )
    allocate( noahmp%water%state%SMCEQ        (       1:NSOIL) )
    allocate( noahmp%water%state%BTRANI       (       1:NSOIL) )

    noahmp%water%state%IMELT(:)       = huge(1)
    noahmp%water%state%SNICE(:)       = huge(1.0)
    noahmp%water%state%SNLIQ(:)       = huge(1.0)
    noahmp%water%state%FICEOLD_SNOW(:)= huge(1.0)
    noahmp%water%state%FICE_SNOW(:)   = huge(1.0)
    noahmp%water%state%FICE_SOIL(:)   = huge(1.0)
    noahmp%water%state%EPORE_SNOW(:)  = huge(1.0)
    noahmp%water%state%SH2O(:)        = huge(1.0)
    noahmp%water%state%SICE(:)        = huge(1.0)
    noahmp%water%state%SMC(:)         = huge(1.0)
    noahmp%water%state%FCR(:)         = huge(1.0)
    noahmp%water%state%WCND(:)        = huge(1.0)
    noahmp%water%state%WDF(:)         = huge(1.0)
    noahmp%water%state%EPORE_SOIL(:)  = huge(1.0)
    noahmp%water%state%SMCEQ(:)       = huge(1.0)
    noahmp%water%state%BTRANI(:)      = huge(1.0)

    ! water flux variable
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
    noahmp%water%flux%IRFIRATE        = huge(1.0)
    noahmp%water%flux%IRMIRATE        = huge(1.0)
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

    allocate( noahmp%water%flux%DDZ1     (-NSNOW+1:0)     )
    allocate( noahmp%water%flux%DDZ2     (-NSNOW+1:0)     )
    allocate( noahmp%water%flux%DDZ3     (-NSNOW+1:0)     )
    allocate( noahmp%water%flux%PDZDTC   (-NSNOW+1:0)     )
    allocate( noahmp%water%flux%ETRANI   (       1:NSOIL) )

    noahmp%water%flux%DDZ1(:)           = huge(1.0)
    noahmp%water%flux%DDZ2(:)           = huge(1.0)
    noahmp%water%flux%DDZ3(:)           = huge(1.0)
    noahmp%water%flux%PDZDTC(:)         = huge(1.0)
    noahmp%water%flux%ETRANI(:)         = huge(1.0)

    ! water parameter variable
    noahmp%water%param%DRAIN_LAYER_OPT  = huge(1  )
    noahmp%water%param%TD_DEPTH         = huge(1  )
    noahmp%water%param%NROOT            = huge(1  )
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

    allocate( noahmp%water%param%SMCMAX   (       1:NSOIL) )
    allocate( noahmp%water%param%SMCWLT   (       1:NSOIL) )
    allocate( noahmp%water%param%SMCREF   (       1:NSOIL) )
    allocate( noahmp%water%param%DWSAT    (       1:NSOIL) )
    allocate( noahmp%water%param%DKSAT    (       1:NSOIL) )
    allocate( noahmp%water%param%BEXP     (       1:NSOIL) )
    allocate( noahmp%water%param%PSISAT   (       1:NSOIL) )

    noahmp%water%param%SMCMAX(:)        = huge(1.0)
    noahmp%water%param%SMCWLT(:)        = huge(1.0)
    noahmp%water%param%SMCREF(:)        = huge(1.0)
    noahmp%water%param%DWSAT (:)        = huge(1.0)
    noahmp%water%param%DKSAT (:)        = huge(1.0)
    noahmp%water%param%BEXP  (:)        = huge(1.0)
    noahmp%water%param%PSISAT(:)        = huge(1.0)

    ! water diagnostic variable


    end associate

  end subroutine WaterVarInitDefault

!=== initialize with input data or table values
  subroutine WaterVarInitTransfer(noahmp, input)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp
    type(input_type) , intent(inout) :: input

    ! local loop index
    integer                          :: ISOIL

    associate(                                                  &
              ILOC        => noahmp%config%domain%ILOC         ,&
              JLOC        => noahmp%config%domain%JLOC         ,&
              NSNOW       => noahmp%config%domain%NSNOW        ,&
              NSOIL       => noahmp%config%domain%NSOIL        ,&
              VEGTYP      => noahmp%config%domain%VEGTYP       ,&
              SOILTYP     => noahmp%config%domain%SOILTYP      ,&
              URBAN_FLAG  => noahmp%config%domain%URBAN_FLAG    &
             )

    ! water state variable
    !noahmp%water%state%CANLIQ                   = input%CANLIQIn
    !noahmp%water%state%CANICE                   = input%CANICEIn
    !noahmp%water%state%SNEQV                    = input%SNEQVIn
    !noahmp%water%state%SNOWH                    = input%SNOWHIn
    !noahmp%water%state%SNICE(-NSNOW+1:0)        = input%SNICEIn(-NSNOW+1:0)
    !noahmp%water%state%SNLIQ(-NSNOW+1:0)        = input%SNLIQIn(-NSNOW+1:0)
    !noahmp%water%state%FICEOLD_SNOW(-NSNOW+1:0) = input%FICEOLDIn(-NSNOW+1:0)
    !noahmp%water%state%SH2O(1:NSOIL)            = input%SH2OIn(1:NSOIL)
    !noahmp%water%state%SICE(1:NSOIL)            = input%SICEIn(1:NSOIL)
    !noahmp%water%state%SMC(1:NSOIL)             = input%SMCIn(1:NSOIL)
    !noahmp%water%state%SMCEQ(1:NSOIL)           = input%SMCEQIn(1:NSOIL)
    !noahmp%water%state%FIFAC                    = input%FIFACIn
    !noahmp%water%state%IRAMTFI                  = input%IRAMTFIIn
    !noahmp%water%state%MIFAC                    = input%MIFACIn
    !noahmp%water%state%IRAMTMI                  = input%IRAMTMIIn
    !noahmp%water%state%SIFAC                    = input%SIFACIn
    !noahmp%water%state%ZWT                      = input%ZWTIn
    !noahmp%water%state%SMCWTD                   = input%SMCWTDIn
    !noahmp%water%state%DEEPRECH                 = input%DEEPRECHIn
    !noahmp%water%state%RECH                     = input%RECHIn
    !noahmp%water%state%WATBLED                  = input%WATBLEDIn
    !noahmp%water%state%TDFRACMP                 = input%TDFRACMPIn
    !noahmp%water%state%WA                       = input%WAIn
    !noahmp%water%state%WT                       = input%WTIn
    !noahmp%water%state%WSLAKE                   = input%WSLAKEIn
    !noahmp%water%state%PONDING                  = input%PONDINGIn
    !noahmp%water%state%sfcheadrt                = input%sfcheadrtIn
    !noahmp%water%state%IRRFRA                   = input%IRRFRAIn

    ! water parameter variable
    noahmp%water%param%DRAIN_LAYER_OPT   = input%DRAIN_LAYER_OPT_TABLE
    noahmp%water%param%CH2OP             = input%CH2OP_TABLE(VEGTYP)
    noahmp%water%param%C2_SnowCompact    = input%C2_SNOWCOMPACT_TABLE
    noahmp%water%param%C3_SnowCompact    = input%C3_SNOWCOMPACT_TABLE
    noahmp%water%param%C4_SnowCompact    = input%C4_SNOWCOMPACT_TABLE
    noahmp%water%param%C5_SnowCompact    = input%C5_SNOWCOMPACT_TABLE
    noahmp%water%param%DM_SnowCompact    = input%DM_SNOWCOMPACT_TABLE
    noahmp%water%param%ETA0_SnowCompact  = input%ETA0_SNOWCOMPACT_TABLE
    noahmp%water%param%SNLIQMAXFRAC      = input%SNLIQMAXFRAC_TABLE
    noahmp%water%param%SSI               = input%SSI_TABLE
    noahmp%water%param%SNOW_RET_FAC      = input%SNOW_RET_FAC_TABLE
    noahmp%water%param%FIRTFAC           = input%FIRTFAC_TABLE
    noahmp%water%param%MICIR_RATE        = input%MICIR_RATE_TABLE
    noahmp%water%param%REFDK             = input%REFDK_TABLE
    noahmp%water%param%REFKDT            = input%REFKDT_TABLE
    noahmp%water%param%FRZK              = input%FRZK_TABLE
    noahmp%water%param%TIMEAN            = input%TIMEAN_TABLE
    noahmp%water%param%FSATMX            = input%FSATMX_TABLE
    noahmp%water%param%ROUS              = input%ROUS_TABLE
    noahmp%water%param%CMIC              = input%CMIC_TABLE
    noahmp%water%param%WSLMAX            = input%WSLMAX_TABLE
    noahmp%water%param%SWEMAXGLA         = input%SWEMAXGLA_TABLE
    noahmp%water%param%BVIC              = input%BVIC_TABLE(SOILTYP(1))
    noahmp%water%param%AXAJ              = input%AXAJ_TABLE(SOILTYP(1))
    noahmp%water%param%BXAJ              = input%BXAJ_TABLE(SOILTYP(1))
    noahmp%water%param%XXAJ              = input%XXAJ_TABLE(SOILTYP(1))
    noahmp%water%param%BBVIC             = input%BBVIC_TABLE(SOILTYP(1))
    noahmp%water%param%GDVIC             = input%GDVIC_TABLE(SOILTYP(1))
    noahmp%water%param%BDVIC             = input%BDVIC_TABLE(SOILTYP(1))
    noahmp%water%param%SLOPE             = input%SLOPE_TABLE(input%SLOPETYPEIn)
    noahmp%water%param%TD_DC             = input%TD_DC_TABLE(SOILTYP(1))
    noahmp%water%param%TD_DEPTH          = input%TD_DEPTH_TABLE(SOILTYP(1))
    noahmp%water%param%TDSMC_FAC         = input%TDSMC_FAC_TABLE(SOILTYP(1))
    noahmp%water%param%TD_DCOEF          = input%TD_DCOEF_TABLE(SOILTYP(1))
    noahmp%water%param%TD_ADEPTH         = input%TD_ADEPTH_TABLE(SOILTYP(1))
    noahmp%water%param%KLAT_FAC          = input%KLAT_FAC_TABLE(SOILTYP(1))
    noahmp%water%param%TD_DDRAIN         = input%TD_DDRAIN_TABLE(SOILTYP(1))
    noahmp%water%param%TD_SPAC           = input%TD_SPAC_TABLE(SOILTYP(1))
    noahmp%water%param%TD_RADI           = input%TD_RADI_TABLE(SOILTYP(1))
    noahmp%water%param%TD_D              = input%TD_D_TABLE(SOILTYP(1))
    noahmp%water%param%NROOT             = input%NROOT_TABLE(VEGTYP)

    do ISOIL = 1, size(SOILTYP)
       noahmp%water%param%SMCMAX(ISOIL)   = input%SMCMAX_TABLE(SOILTYP(ISOIL))
       noahmp%water%param%SMCWLT(ISOIL)   = input%SMCWLT_TABLE(SOILTYP(ISOIL))
       noahmp%water%param%SMCREF(ISOIL)   = input%SMCREF_TABLE(SOILTYP(ISOIL))
       noahmp%water%param%DWSAT(ISOIL)    = input%DWSAT_TABLE(SOILTYP(ISOIL))
       noahmp%water%param%DKSAT(ISOIL)    = input%DKSAT_TABLE(SOILTYP(ISOIL))
       noahmp%water%param%BEXP(ISOIL)     = input%BEXP_TABLE(SOILTYP(ISOIL))
       noahmp%water%param%PSISAT(ISOIL)   = input%PSISAT_TABLE(SOILTYP(ISOIL))
    enddo

    ! derived parameters
    noahmp%water%param%KDT  = noahmp%water%param%REFKDT * noahmp%water%param%DKSAT(1) / &
                              noahmp%water%param%REFDK
    if ( URBAN_FLAG .eqv. .true. ) then
       noahmp%water%param%SMCMAX = 0.45
       noahmp%water%param%SMCREF = 0.42
       noahmp%water%param%SMCWLT = 0.40
    endif
    if ( SOILTYP(1) /= 14 ) then
       noahmp%water%param%FRZX = noahmp%water%param%FRZK * &
            ((noahmp%water%param%SMCMAX(1) / noahmp%water%param%SMCREF(1)) * (0.412/0.468))
    endif


    end associate

  end subroutine WaterVarInitTransfer

end module WaterVarInitMod
