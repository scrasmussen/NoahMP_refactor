module WaterInitMod

!!! Initialize column (1-D) Noah-MP water variables
!!! Water variables should be first defined in WaterType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use InputType
  use NoahmpType

  implicit none

contains

!=== initialize with default values
  subroutine WaterInitDefault(noahmp)

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
    noahmp%water%state%PONDING1       = huge(1.0)
    noahmp%water%state%PONDING2       = huge(1.0)

    allocate( noahmp%water%state%SNICE   (-NSNOW+1:0)     )
    allocate( noahmp%water%state%SNLIQ   (-NSNOW+1:0)     )
    allocate( noahmp%water%state%FICEOLD (-NSNOW+1:0)     )
    allocate( noahmp%water%state%FICE    (-NSNOW+1:0)     )
    allocate( noahmp%water%state%SH2O    (       1:NSOIL) )
    allocate( noahmp%water%state%SICE    (       1:NSOIL) )
    allocate( noahmp%water%state%IMELT   (-NSNOW+1:NSOIL) )

    noahmp%water%state%SNICE(:)       = huge(1.0)
    noahmp%water%state%SNLIQ(:)       = huge(1.0)
    noahmp%water%state%FICEOLD(:)     = huge(1.0)
    noahmp%water%state%FICE(:)        = huge(1.0)
    noahmp%water%state%SH2O(:)        = huge(1.0)
    noahmp%water%state%SICE(:)        = huge(1.0)
    noahmp%water%state%IMELT(:)       = huge(1)


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

    allocate( noahmp%water%flux%DDZ1     (-NSNOW+1:0)  )
    allocate( noahmp%water%flux%DDZ2     (-NSNOW+1:0)  )
    allocate( noahmp%water%flux%DDZ3     (-NSNOW+1:0)  )
    allocate( noahmp%water%flux%PDZDTC   (-NSNOW+1:0)  )

    noahmp%water%flux%DDZ1(:)           = huge(1.0)
    noahmp%water%flux%DDZ2(:)           = huge(1.0)
    noahmp%water%flux%DDZ3(:)           = huge(1.0)
    noahmp%water%flux%PDZDTC(:)         = huge(1.0)

    ! water parameter variable
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

    ! water diagnostic variable
    noahmp%water%diag%DZ2_COMBO       = huge(1.0)
    noahmp%water%diag%WLIQ2_COMBO     = huge(1.0)
    noahmp%water%diag%WICE2_COMBO     = huge(1.0)
    noahmp%water%diag%T2_COMBO        = huge(1.0)
    noahmp%water%diag%DZ_COMBO        = huge(1.0)
    noahmp%water%diag%WLIQ_COMBO      = huge(1.0)
    noahmp%water%diag%WICE_COMBO      = huge(1.0)
    noahmp%water%diag%T_COMBO         = huge(1.0)

    end associate

  end subroutine WaterInitDefault

!=== initialize with input data or table values
  subroutine WaterInitTransfer(noahmp, input)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp
    type(input_type) , intent(in)    :: input

    associate(                                      &
              ILOC  => noahmp%config%domain%ILOC   ,&
              JLOC  => noahmp%config%domain%JLOC   ,&
              NSNOW => noahmp%config%domain%NSNOW  ,&
              NSOIL => noahmp%config%domain%NSOIL   &
             )

    ! water state variable
    noahmp%water%state%CANLIQ              = input%CANLIQIn(ILOC,JLOC)
    noahmp%water%state%CANICE              = input%CANICEIn(ILOC,JLOC)
    noahmp%water%state%SNEQV               = input%SNEQVIn(ILOC,JLOC)
    noahmp%water%state%SNOWH               = input%SNOWHIn(ILOC,JLOC)
    noahmp%water%state%SNICE(-NSNOW+1:0)   = input%SNICEIn(ILOC,-NSNOW+1:0,JLOC)
    noahmp%water%state%SNLIQ(-NSNOW+1:0)   = input%SNLIQIn(ILOC,-NSNOW+1:0,JLOC)
    noahmp%water%state%FICEOLD(-NSNOW+1:0) = input%FICEOLDIn(ILOC,-NSNOW+1:0,JLOC)
    noahmp%water%state%SH2O(1:NSOIL)       = input%SH2OIn(ILOC,1:NSOIL,JLOC)
    noahmp%water%state%SICE(1:NSOIL)       = input%SICEIn(ILOC,1:NSOIL,JLOC)

    ! water parameter variable
    noahmp%water%param%CH2OP             = input%CH2OPIn
    noahmp%water%param%C2_SnowCompact    = input%C2_SnowCompactIn
    noahmp%water%param%C3_SnowCompact    = input%C3_SnowCompactIn
    noahmp%water%param%C4_SnowCompact    = input%C4_SnowCompactIn
    noahmp%water%param%C5_SnowCompact    = input%C5_SnowCompactIn
    noahmp%water%param%DM_SnowCompact    = input%DM_SnowCompactIn
    noahmp%water%param%ETA0_SnowCompact  = input%ETA0_SnowCompactIn
    noahmp%water%param%SNLIQMAXFRAC      = input%SNLIQMAXFRACIn
    noahmp%water%param%SSI               = input%SSIIn
    noahmp%water%param%SNOW_RET_FAC      = input%SNOW_RET_FACIn

    end associate

  end subroutine WaterInitTransfer

end module WaterInitMod
