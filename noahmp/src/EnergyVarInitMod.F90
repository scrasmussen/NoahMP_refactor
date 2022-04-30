module EnergyVarInitMod

!!! Initialize column (1-D) Noah-MP energy variables
!!! Energy variables should be first defined in EnergyType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use NoahmpIOVarType
  use NoahmpVarType
  use Machine, only : kind_noahmp

  implicit none

contains

!=== initialize with default values
  subroutine EnergyVarInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    associate(                                      &
              NSNOW => noahmp%config%domain%NSNOW  ,&
              NSOIL => noahmp%config%domain%NSOIL  ,&
              NBAND => noahmp%config%domain%NBAND   &
             )
    
    ! energy state variable
    noahmp%energy%state%FROZEN_CANOPY   = .false.
    noahmp%energy%state%FROZEN_GROUND   = .false.
    noahmp%energy%state%ELAI            = huge(1.0)
    noahmp%energy%state%ESAI            = huge(1.0)
    noahmp%energy%state%LAI             = huge(1.0)
    noahmp%energy%state%SAI             = huge(1.0)
    noahmp%energy%state%VAI             = huge(1.0)
    noahmp%energy%state%FVEG            = huge(1.0)
    noahmp%energy%state%EAIR            = huge(1.0)
    noahmp%energy%state%FAGE            = huge(1.0)
    noahmp%energy%state%TAUSS           = huge(1.0)
    noahmp%energy%state%ALBOLD          = huge(1.0)
    noahmp%energy%state%GDIR            = huge(1.0)
    noahmp%energy%state%BGAP            = huge(1.0)
    noahmp%energy%state%WGAP            = huge(1.0)
    noahmp%energy%state%KOPEN           = huge(1.0)
    noahmp%energy%state%GAP             = huge(1.0)
    noahmp%energy%state%FSUN            = huge(1.0)
    noahmp%energy%state%FSHA            = huge(1.0)
    noahmp%energy%state%LAISUN          = huge(1.0)
    noahmp%energy%state%LAISHA          = huge(1.0)
    noahmp%energy%state%ESTV            = huge(1.0)
    noahmp%energy%state%ESTG            = huge(1.0)
    noahmp%energy%state%ESTB            = huge(1.0)
    noahmp%energy%state%DESTV           = huge(1.0)
    noahmp%energy%state%DESTG           = huge(1.0)
    noahmp%energy%state%DESTB           = huge(1.0)
    noahmp%energy%state%EAH             = huge(1.0)
    noahmp%energy%state%CO2AIR          = huge(1.0)
    noahmp%energy%state%O2AIR           = huge(1.0)
    noahmp%energy%state%RSSUN           = huge(1.0)
    noahmp%energy%state%RSSHA           = huge(1.0)
    noahmp%energy%state%RHOAIR          = huge(1.0)
    noahmp%energy%state%TAH             = huge(1.0)
    noahmp%energy%state%ZPD             = huge(1.0)
    noahmp%energy%state%ZPDG            = huge(1.0)
    noahmp%energy%state%Z0MG            = huge(1.0)
    noahmp%energy%state%Z0M             = huge(1.0)
    noahmp%energy%state%HCAN            = huge(1.0)
    noahmp%energy%state%UC              = huge(1.0)
    noahmp%energy%state%Z0HV            = huge(1.0)
    noahmp%energy%state%Z0HG            = huge(1.0)
    noahmp%energy%state%Z0HB            = huge(1.0)
    noahmp%energy%state%FVV             = huge(1.0)
    noahmp%energy%state%FVB             = huge(1.0)
    noahmp%energy%state%CWPC            = huge(1.0)
    noahmp%energy%state%MOZG            = huge(1.0)
    noahmp%energy%state%MOZV            = huge(1.0)
    noahmp%energy%state%MOZB            = huge(1.0)
    noahmp%energy%state%MOZ2V           = huge(1.0)
    noahmp%energy%state%MOZ2B           = huge(1.0)
    noahmp%energy%state%MOLG            = huge(1.0)
    noahmp%energy%state%MOLV            = huge(1.0)
    noahmp%energy%state%MOLB            = huge(1.0)
    noahmp%energy%state%FHG             = huge(1.0)
    noahmp%energy%state%FMV             = huge(1.0)
    noahmp%energy%state%FHV             = huge(1.0)
    noahmp%energy%state%FM2V            = huge(1.0)
    noahmp%energy%state%FH2V            = huge(1.0)
    noahmp%energy%state%FMB             = huge(1.0)
    noahmp%energy%state%FHB             = huge(1.0)
    noahmp%energy%state%FM2B            = huge(1.0)
    noahmp%energy%state%FH2B            = huge(1.0)
    noahmp%energy%state%CM              = huge(1.0)
    noahmp%energy%state%CMV             = huge(1.0)
    noahmp%energy%state%CMB             = huge(1.0)
    noahmp%energy%state%CH              = huge(1.0)
    noahmp%energy%state%CHB             = huge(1.0)
    noahmp%energy%state%CHV             = huge(1.0)
    noahmp%energy%state%CHLEAF          = huge(1.0)
    noahmp%energy%state%CHUC            = huge(1.0)
    noahmp%energy%state%CH2V            = huge(1.0)
    noahmp%energy%state%CH2B            = huge(1.0)
    noahmp%energy%state%CHV2            = huge(1.0)
    noahmp%energy%state%EHB             = huge(1.0)
    noahmp%energy%state%EHB2            = huge(1.0)
    noahmp%energy%state%EMB             = huge(1.0)
    noahmp%energy%state%CAW             = huge(1.0)
    noahmp%energy%state%CTW             = huge(1.0)
    noahmp%energy%state%CEW             = huge(1.0)
    noahmp%energy%state%CGW             = huge(1.0)
    noahmp%energy%state%RAMG            = huge(1.0)
    noahmp%energy%state%RAHG            = huge(1.0)
    noahmp%energy%state%RAWG            = huge(1.0)
    noahmp%energy%state%RAMC            = huge(1.0)
    noahmp%energy%state%RAHC            = huge(1.0)
    noahmp%energy%state%RAWC            = huge(1.0)
    noahmp%energy%state%RAMB            = huge(1.0)
    noahmp%energy%state%RAHB            = huge(1.0)
    noahmp%energy%state%RAWB            = huge(1.0)
    noahmp%energy%state%RB              = huge(1.0)
    noahmp%energy%state%QAIR            = huge(1.0)
    noahmp%energy%state%THAIR           = huge(1.0)
    noahmp%energy%state%UR              = huge(1.0)
    noahmp%energy%state%WSTARV          = huge(1.0)
    noahmp%energy%state%WSTARB          = huge(1.0)
    noahmp%energy%state%EMV             = huge(1.0)
    noahmp%energy%state%EMG             = huge(1.0)
    noahmp%energy%state%RSURF           = huge(1.0)
    noahmp%energy%state%GAMMAV          = huge(1.0)
    noahmp%energy%state%LATHEAV         = huge(1.0)
    noahmp%energy%state%GAMMAG          = huge(1.0)
    noahmp%energy%state%LATHEAG         = huge(1.0)
    noahmp%energy%state%RHSUR           = huge(1.0)
    noahmp%energy%state%QSFC            = huge(1.0)
    noahmp%energy%state%Q1              = huge(1.0)
    noahmp%energy%state%Q2V             = huge(1.0)
    noahmp%energy%state%Q2B             = huge(1.0)
    noahmp%energy%state%Q2E             = huge(1.0)
    noahmp%energy%state%TS              = huge(1.0)
    noahmp%energy%state%TG              = huge(1.0)
    noahmp%energy%state%TV              = huge(1.0)
    noahmp%energy%state%TGV             = huge(1.0)
    noahmp%energy%state%TGB             = huge(1.0)
    noahmp%energy%state%TROOT           = huge(1.0)
    noahmp%energy%state%TAUXV           = huge(1.0)
    noahmp%energy%state%TAUYV           = huge(1.0)
    noahmp%energy%state%TAUXB           = huge(1.0)
    noahmp%energy%state%TAUYB           = huge(1.0)
    noahmp%energy%state%TAUX            = huge(1.0)
    noahmp%energy%state%TAUY            = huge(1.0)
    noahmp%energy%state%T2MV            = huge(1.0)
    noahmp%energy%state%T2MB            = huge(1.0)
    noahmp%energy%state%T2M             = huge(1.0)
    noahmp%energy%state%ZLVL            = huge(1.0)
    noahmp%energy%state%FB_snow         = huge(1.0)
    noahmp%energy%state%ZBOTSNO         = huge(1.0)
    noahmp%energy%state%Z0WRF           = huge(1.0)
    noahmp%energy%state%TRAD            = huge(1.0)
    noahmp%energy%state%EMISSI          = huge(1.0)
    noahmp%energy%state%ALBEDO          = huge(1.0)
    noahmp%energy%state%ERRENG          = huge(1.0)
    noahmp%energy%state%ERRSW           = huge(1.0)
    
    if( .not. allocated( noahmp%energy%state%STC      ) ) allocate( noahmp%energy%state%STC      (-NSNOW+1:NSOIL) )
    if( .not. allocated( noahmp%energy%state%DF       ) ) allocate( noahmp%energy%state%DF       (-NSNOW+1:NSOIL) )
    if( .not. allocated( noahmp%energy%state%HCPCT    ) ) allocate( noahmp%energy%state%HCPCT    (-NSNOW+1:NSOIL) )
    if( .not. allocated( noahmp%energy%state%FACT     ) ) allocate( noahmp%energy%state%FACT     (-NSNOW+1:NSOIL) )
    if( .not. allocated( noahmp%energy%state%CVSNO    ) ) allocate( noahmp%energy%state%CVSNO    (-NSNOW+1:0    ) )
    if( .not. allocated( noahmp%energy%state%TKSNO    ) ) allocate( noahmp%energy%state%TKSNO    (-NSNOW+1:0    ) )
    if( .not. allocated( noahmp%energy%state%CVSOIL   ) ) allocate( noahmp%energy%state%CVSOIL   (       1:NSOIL) )
    if( .not. allocated( noahmp%energy%state%TKSOIL   ) ) allocate( noahmp%energy%state%TKSOIL   (       1:NSOIL) )
    if( .not. allocated( noahmp%energy%state%CVGLAICE ) ) allocate( noahmp%energy%state%CVGLAICE (       1:NSOIL) )
    if( .not. allocated( noahmp%energy%state%TKGLAICE ) ) allocate( noahmp%energy%state%TKGLAICE (       1:NSOIL) )
    if( .not. allocated( noahmp%energy%state%ALBSND   ) ) allocate( noahmp%energy%state%ALBSND   (       1:NBAND) )
    if( .not. allocated( noahmp%energy%state%ALBSNI   ) ) allocate( noahmp%energy%state%ALBSNI   (       1:NBAND) )
    if( .not. allocated( noahmp%energy%state%ALBSOD   ) ) allocate( noahmp%energy%state%ALBSOD   (       1:NBAND) )
    if( .not. allocated( noahmp%energy%state%ALBSOI   ) ) allocate( noahmp%energy%state%ALBSOI   (       1:NBAND) )
    if( .not. allocated( noahmp%energy%state%ALBGRD   ) ) allocate( noahmp%energy%state%ALBGRD   (       1:NBAND) )
    if( .not. allocated( noahmp%energy%state%ALBGRI   ) ) allocate( noahmp%energy%state%ALBGRI   (       1:NBAND) )
    if( .not. allocated( noahmp%energy%state%RHO      ) ) allocate( noahmp%energy%state%RHO      (       1:NBAND) )
    if( .not. allocated( noahmp%energy%state%TAU      ) ) allocate( noahmp%energy%state%TAU      (       1:NBAND) )
    if( .not. allocated( noahmp%energy%state%ALBD     ) ) allocate( noahmp%energy%state%ALBD     (       1:NBAND) )
    if( .not. allocated( noahmp%energy%state%ALBI     ) ) allocate( noahmp%energy%state%ALBI     (       1:NBAND) )
    
    noahmp%energy%state%STC(:)          = huge(1.0)
    noahmp%energy%state%DF(:)           = huge(1.0)
    noahmp%energy%state%HCPCT(:)        = huge(1.0)
    noahmp%energy%state%FACT(:)         = huge(1.0)
    noahmp%energy%state%CVSNO(:)        = huge(1.0)
    noahmp%energy%state%TKSNO(:)        = huge(1.0)
    noahmp%energy%state%CVSOIL(:)       = huge(1.0)
    noahmp%energy%state%TKSOIL(:)       = huge(1.0)
    noahmp%energy%state%CVGLAICE(:)     = huge(1.0)
    noahmp%energy%state%TKGLAICE(:)     = huge(1.0)
    noahmp%energy%state%ALBSND(:)       = huge(1.0)
    noahmp%energy%state%ALBSNI(:)       = huge(1.0)
    noahmp%energy%state%ALBSOD(:)       = huge(1.0)
    noahmp%energy%state%ALBSOI(:)       = huge(1.0)
    noahmp%energy%state%ALBGRD(:)       = huge(1.0)
    noahmp%energy%state%ALBGRI(:)       = huge(1.0)
    noahmp%energy%state%RHO(:)          = huge(1.0)
    noahmp%energy%state%TAU(:)          = huge(1.0)
    noahmp%energy%state%ALBD(:)         = huge(1.0)
    noahmp%energy%state%ALBI(:)         = huge(1.0)
    
    ! energy flux variable
    noahmp%energy%flux%FCEV             = huge(1.0)
    noahmp%energy%flux%FCTR             = huge(1.0)
    noahmp%energy%flux%FGEV             = huge(1.0)
    noahmp%energy%flux%FIRR             = 0.0
    noahmp%energy%flux%PAHV             = huge(1.0)
    noahmp%energy%flux%PAHG             = huge(1.0)
    noahmp%energy%flux%PAHB             = huge(1.0)
    noahmp%energy%flux%PAH              = huge(1.0)
    noahmp%energy%flux%PARSUN           = huge(1.0)
    noahmp%energy%flux%PARSHA           = huge(1.0)
    noahmp%energy%flux%SAV              = huge(1.0)
    noahmp%energy%flux%SAG              = huge(1.0)
    noahmp%energy%flux%FSA              = huge(1.0)
    noahmp%energy%flux%FSR              = huge(1.0)
    noahmp%energy%flux%FSRV             = huge(1.0)
    noahmp%energy%flux%FSRG             = huge(1.0)
    noahmp%energy%flux%SWDOWN           = huge(1.0)
    noahmp%energy%flux%IRC              = huge(1.0)
    noahmp%energy%flux%SHC              = huge(1.0)
    noahmp%energy%flux%EVC              = huge(1.0)
    noahmp%energy%flux%IRG              = huge(1.0)
    noahmp%energy%flux%SHG              = huge(1.0)
    noahmp%energy%flux%EVG              = huge(1.0)
    noahmp%energy%flux%TR               = huge(1.0)
    noahmp%energy%flux%GHV              = huge(1.0)
    noahmp%energy%flux%IRB              = huge(1.0)
    noahmp%energy%flux%SHB              = huge(1.0)
    noahmp%energy%flux%EVB              = huge(1.0)
    noahmp%energy%flux%GHB              = huge(1.0)
    noahmp%energy%flux%SSOIL            = huge(1.0)
    noahmp%energy%flux%EFLXB            = huge(1.0)
    noahmp%energy%flux%FIRA             = huge(1.0)
    noahmp%energy%flux%FSH              = huge(1.0)
    noahmp%energy%flux%APAR             = huge(1.0)
    noahmp%energy%flux%FIRE             = huge(1.0)
    
    if( .not. allocated( noahmp%energy%flux%FABD  ) ) allocate( noahmp%energy%flux%FABD  (1:NBAND) )
    if( .not. allocated( noahmp%energy%flux%FABI  ) ) allocate( noahmp%energy%flux%FABI  (1:NBAND) )
    if( .not. allocated( noahmp%energy%flux%FTDD  ) ) allocate( noahmp%energy%flux%FTDD  (1:NBAND) )
    if( .not. allocated( noahmp%energy%flux%FTDI  ) ) allocate( noahmp%energy%flux%FTDI  (1:NBAND) )
    if( .not. allocated( noahmp%energy%flux%FTID  ) ) allocate( noahmp%energy%flux%FTID  (1:NBAND) )
    if( .not. allocated( noahmp%energy%flux%FTII  ) ) allocate( noahmp%energy%flux%FTII  (1:NBAND) )
    if( .not. allocated( noahmp%energy%flux%FREVD ) ) allocate( noahmp%energy%flux%FREVD (1:NBAND) )
    if( .not. allocated( noahmp%energy%flux%FREVI ) ) allocate( noahmp%energy%flux%FREVI (1:NBAND) )
    if( .not. allocated( noahmp%energy%flux%FREGD ) ) allocate( noahmp%energy%flux%FREGD (1:NBAND) )
    if( .not. allocated( noahmp%energy%flux%FREGI ) ) allocate( noahmp%energy%flux%FREGI (1:NBAND) )
    if( .not. allocated( noahmp%energy%flux%SOLAD ) ) allocate( noahmp%energy%flux%SOLAD (1:NBAND) )
    if( .not. allocated( noahmp%energy%flux%SOLAI ) ) allocate( noahmp%energy%flux%SOLAI (1:NBAND) )
    if( .not. allocated( noahmp%energy%flux%PHI   ) ) allocate( noahmp%energy%flux%PHI   (-NSNOW+1:NSOIL) )
    
    noahmp%energy%flux%FABD(:)          = huge(1.0)
    noahmp%energy%flux%FABI(:)          = huge(1.0)
    noahmp%energy%flux%FTDD(:)          = huge(1.0)
    noahmp%energy%flux%FTDI(:)          = huge(1.0)
    noahmp%energy%flux%FTID(:)          = huge(1.0)
    noahmp%energy%flux%FTII(:)          = huge(1.0)
    noahmp%energy%flux%FREVD(:)         = huge(1.0)
    noahmp%energy%flux%FREVI(:)         = huge(1.0)
    noahmp%energy%flux%FREGD(:)         = huge(1.0)
    noahmp%energy%flux%FREGI(:)         = huge(1.0)
    noahmp%energy%flux%SOLAD(:)         = huge(1.0)
    noahmp%energy%flux%SOLAI(:)         = huge(1.0)
    noahmp%energy%flux%PHI(:)           = huge(1.0)
    
    ! energy parameter variable
    noahmp%energy%param%RC              = huge(1.0)
    noahmp%energy%param%HVT             = huge(1.0)
    noahmp%energy%param%HVB             = huge(1.0)
    noahmp%energy%param%Z0MVT           = huge(1.0)
    noahmp%energy%param%DEN             = huge(1.0)
    noahmp%energy%param%XL              = huge(1.0)
    noahmp%energy%param%BETADS          = huge(1.0)
    noahmp%energy%param%BETAIS          = huge(1.0)
    noahmp%energy%param%CSOIL           = huge(1.0)
    noahmp%energy%param%TAU0            = huge(1.0)
    noahmp%energy%param%GRAIN_GROWTH    = huge(1.0)
    noahmp%energy%param%DIRT_SOOT       = huge(1.0)
    noahmp%energy%param%EXTRA_GROWTH    = huge(1.0)
    noahmp%energy%param%BATS_COSZ       = huge(1.0)
    noahmp%energy%param%BATS_VIS_NEW    = huge(1.0)
    noahmp%energy%param%BATS_NIR_NEW    = huge(1.0)
    noahmp%energy%param%BATS_VIS_AGE    = huge(1.0)
    noahmp%energy%param%BATS_NIR_AGE    = huge(1.0)
    noahmp%energy%param%BATS_VIS_DIR    = huge(1.0)
    noahmp%energy%param%BATS_NIR_DIR    = huge(1.0)
    noahmp%energy%param%CLASS_ALB_REF   = huge(1.0)
    noahmp%energy%param%CLASS_SNO_AGE   = huge(1.0)
    noahmp%energy%param%CLASS_ALB_NEW   = huge(1.0)
    noahmp%energy%param%BP              = huge(1.0)
    noahmp%energy%param%KC25            = huge(1.0)
    noahmp%energy%param%KO25            = huge(1.0)
    noahmp%energy%param%AKC             = huge(1.0)
    noahmp%energy%param%AKO             = huge(1.0)
    noahmp%energy%param%RGL             = huge(1.0)
    noahmp%energy%param%RSMIN           = huge(1.0)
    noahmp%energy%param%RSMAX           = huge(1.0)
    noahmp%energy%param%TOPT            = huge(1.0)
    noahmp%energy%param%HS              = huge(1.0)
    noahmp%energy%param%DLEAF           = huge(1.0)
    noahmp%energy%param%CZIL            = huge(1.0)
    noahmp%energy%param%SNOW_EMIS       = huge(1.0)
    noahmp%energy%param%CWPVT           = huge(1.0)
    noahmp%energy%param%Z0SNO           = huge(1.0)
    noahmp%energy%param%ZBOT            = huge(1.0)
    noahmp%energy%param%Z0SOIL          = huge(1.0)
    noahmp%energy%param%Z0LAKE          = huge(1.0)
    noahmp%energy%param%EICE            = huge(1.0)
    noahmp%energy%param%RSURF_EXP       = huge(1.0)
    noahmp%energy%param%RSURF_SNOW      = huge(1.0)
    noahmp%energy%param%SHDMAX          = huge(1.0)
    noahmp%energy%param%SHDFAC          = huge(1.0)
    
    if( .not. allocated( noahmp%energy%param%LAIM   ) ) allocate( noahmp%energy%param%LAIM   (1:12   ) )
    if( .not. allocated( noahmp%energy%param%SAIM   ) ) allocate( noahmp%energy%param%SAIM   (1:12   ) )
    if( .not. allocated( noahmp%energy%param%QUARTZ ) ) allocate( noahmp%energy%param%QUARTZ (1:NSOIL) )
    if( .not. allocated( noahmp%energy%param%ALBSAT ) ) allocate( noahmp%energy%param%ALBSAT (1:NBAND) )
    if( .not. allocated( noahmp%energy%param%ALBDRY ) ) allocate( noahmp%energy%param%ALBDRY (1:NBAND) )
    if( .not. allocated( noahmp%energy%param%ALBLAK ) ) allocate( noahmp%energy%param%ALBLAK (1:NBAND) )
    if( .not. allocated( noahmp%energy%param%OMEGAS ) ) allocate( noahmp%energy%param%OMEGAS (1:NBAND) )
    if( .not. allocated( noahmp%energy%param%RHOL   ) ) allocate( noahmp%energy%param%RHOL   (1:NBAND) )
    if( .not. allocated( noahmp%energy%param%RHOS   ) ) allocate( noahmp%energy%param%RHOS   (1:NBAND) )
    if( .not. allocated( noahmp%energy%param%TAUL   ) ) allocate( noahmp%energy%param%TAUL   (1:NBAND) )
    if( .not. allocated( noahmp%energy%param%TAUS   ) ) allocate( noahmp%energy%param%TAUS   (1:NBAND) )
    if( .not. allocated( noahmp%energy%param%EG     ) ) allocate( noahmp%energy%param%EG     (1:2    ) )
    if( .not. allocated( noahmp%energy%param%ALBICE ) ) allocate( noahmp%energy%param%ALBICE (1:NBAND) )
    
    noahmp%energy%param%LAIM(:)         = huge(1.0)
    noahmp%energy%param%SAIM(:)         = huge(1.0)
    noahmp%energy%param%QUARTZ(:)       = huge(1.0)
    noahmp%energy%param%ALBSAT(:)       = huge(1.0)
    noahmp%energy%param%ALBDRY(:)       = huge(1.0)
    noahmp%energy%param%ALBLAK(:)       = huge(1.0)
    noahmp%energy%param%OMEGAS(:)       = huge(1.0)
    noahmp%energy%param%RHOL(:)         = huge(1.0)
    noahmp%energy%param%RHOS(:)         = huge(1.0)
    noahmp%energy%param%TAUL(:)         = huge(1.0)
    noahmp%energy%param%TAUS(:)         = huge(1.0)
    noahmp%energy%param%EG(:)           = huge(1.0)
    noahmp%energy%param%ALBICE          = huge(1.0)
    
    end associate

  end subroutine EnergyVarInitDefault

!=== initialize with input data or table values

  subroutine EnergyVarInitTransfer(noahmp, NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout) :: NoahmpIO
    type(noahmp_type),   intent(inout) :: noahmp

    ! local loop index
    integer                          :: ISOIL

    associate(                                                  &
              I           => noahmp%config%domain%ILOC         ,&
              J           => noahmp%config%domain%JLOC         ,&
              KTS         => NoahmpIO%KTS                      ,&
              VEGTYP      => noahmp%config%domain%VEGTYP       ,&
              SOILTYP     => noahmp%config%domain%SOILTYP      ,&
              CROPTYP     => noahmp%config%domain%CROPTYP      ,&
              SOILCOLOR   => noahmp%config%domain%SOILCOLOR    ,&
              URBAN_FLAG  => noahmp%config%domain%URBAN_FLAG   ,&
              NSNOW       => noahmp%config%domain%NSNOW        ,&
              NSOIL       => noahmp%config%domain%NSOIL        ,&
              NBAND       => noahmp%config%domain%NBAND         &
             )

    !energy state variable
    
    noahmp%energy%state%LAI               = NoahmpIO%LAI    (I,J)
    noahmp%energy%state%SAI               = NoahmpIO%XSAIXY (I,J)
    noahmp%energy%state%QSFC              = NoahmpIO%QSFC   (I,J)
    noahmp%energy%state%TG                = NoahmpIO%TGXY   (I,J)
    noahmp%energy%state%TV                = NoahmpIO%TVXY   (I,J)
    noahmp%energy%state%STC(-NSNOW+1:0)   = NoahmpIO%TSNOXY (I,-NSNOW+1:0,J)
    noahmp%energy%state%STC(1:NSOIL)      = NoahmpIO%TSLB   (I,1:NSOIL,J)
    noahmp%energy%state%TAUSS             = NoahmpIO%TAUSSXY(I,J)
    noahmp%energy%state%ALBOLD            = NoahmpIO%ALBOLDXY(I,J)
    noahmp%energy%state%EAH               = NoahmpIO%EAHXY (I,J)
    noahmp%energy%state%TAH               = NoahmpIO%TAHXY (I,J)
    noahmp%energy%state%CH                = NoahmpIO%CHXY  (I,J) 
    noahmp%energy%state%CM                = NoahmpIO%CMXY  (I,J)
    noahmp%energy%state%CO2AIR            = NoahmpIO%CO2_TABLE * &
                                            (NoahmpIO%P8W(I,KTS+1,J)+NoahmpIO%P8W(I,KTS,J))*0.5
    noahmp%energy%state%O2AIR             = NoahmpIO%O2_TABLE *  &
                                            (NoahmpIO%P8W(I,KTS+1,J)+NoahmpIO%P8W(I,KTS,J))*0.5
    ! energy parameter variable
    noahmp%energy%param%RC                 = NoahmpIO%RC_TABLE(VEGTYP)
    noahmp%energy%param%HVT                = NoahmpIO%HVT_TABLE(VEGTYP)
    noahmp%energy%param%HVB                = NoahmpIO%HVB_TABLE(VEGTYP)
    noahmp%energy%param%Z0MVT              = NoahmpIO%Z0MVT_TABLE(VEGTYP)
    noahmp%energy%param%CWPVT              = NoahmpIO%CWPVT_TABLE(VEGTYP)
    noahmp%energy%param%DEN                = NoahmpIO%DEN_TABLE(VEGTYP)
    noahmp%energy%param%XL                 = NoahmpIO%XL_TABLE(VEGTYP)
    noahmp%energy%param%BP                 = NoahmpIO%BP_TABLE(VEGTYP)
    noahmp%energy%param%KC25               = NoahmpIO%KC25_TABLE(VEGTYP)
    noahmp%energy%param%KO25               = NoahmpIO%KO25_TABLE(VEGTYP)
    noahmp%energy%param%AKC                = NoahmpIO%AKC_TABLE(VEGTYP)
    noahmp%energy%param%AKO                = NoahmpIO%AKO_TABLE(VEGTYP)
    noahmp%energy%param%RGL                = NoahmpIO%RGL_TABLE(VEGTYP)
    noahmp%energy%param%RSMIN              = NoahmpIO%RS_TABLE(VEGTYP)
    noahmp%energy%param%RSMAX              = NoahmpIO%RSMAX_TABLE(VEGTYP)
    noahmp%energy%param%TOPT               = NoahmpIO%TOPT_TABLE(VEGTYP)
    noahmp%energy%param%HS                 = NoahmpIO%HS_TABLE(VEGTYP)
    noahmp%energy%param%DLEAF              = NoahmpIO%DLEAF_TABLE(VEGTYP)
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
    noahmp%energy%param%ZBOT               = NoahmpIO%ZBOT_TABLE
    noahmp%energy%param%Z0SOIL             = NoahmpIO%Z0SOIL_TABLE
    noahmp%energy%param%Z0LAKE             = NoahmpIO%Z0LAKE_TABLE
    noahmp%energy%param%EICE               = NoahmpIO%EICE_TABLE
    noahmp%energy%param%RSURF_EXP          = NoahmpIO%RSURF_EXP_TABLE
    noahmp%energy%param%RSURF_SNOW         = NoahmpIO%RSURF_SNOW_TABLE
    noahmp%energy%param%SHDMAX             = NoahmpIO%GVFMAX(I,J) / 100.0
    noahmp%energy%param%SHDFAC             = NoahmpIO%VEGFRA(I,J) / 100.0

    noahmp%energy%param%LAIM(1:12)         = NoahmpIO%LAIM_TABLE(VEGTYP,1:12)
    noahmp%energy%param%SAIM(1:12)         = NoahmpIO%SAIM_TABLE(VEGTYP,1:12)
    noahmp%energy%param%RHOL(1:NBAND)      = NoahmpIO%RHOL_TABLE(VEGTYP,1:NBAND)
    noahmp%energy%param%RHOS(1:NBAND)      = NoahmpIO%RHOS_TABLE(VEGTYP,1:NBAND)
    noahmp%energy%param%TAUL(1:NBAND)      = NoahmpIO%TAUL_TABLE(VEGTYP,1:NBAND)
    noahmp%energy%param%TAUS(1:NBAND)      = NoahmpIO%TAUS_TABLE(VEGTYP,1:NBAND)
    noahmp%energy%param%ALBSAT(1:NBAND)    = NoahmpIO%ALBSAT_TABLE(SOILCOLOR,1:NBAND)
    noahmp%energy%param%ALBDRY(1:NBAND)    = NoahmpIO%ALBDRY_TABLE(SOILCOLOR,1:NBAND)
    noahmp%energy%param%ALBLAK(1:NBAND)    = NoahmpIO%ALBLAK_TABLE(1:NBAND)
    noahmp%energy%param%OMEGAS(1:NBAND)    = NoahmpIO%OMEGAS_TABLE(1:NBAND)

    do ISOIL = 1, size(SOILTYP)
       noahmp%energy%param%QUARTZ(ISOIL)   = NoahmpIO%QUARTZ_TABLE(SOILTYP(ISOIL))
    enddo

    if ( URBAN_FLAG .eqv. .true. ) then
       noahmp%energy%param%CSOIL = 3.0e6
    endif

    if ( CROPTYP > 0 ) then
       noahmp%energy%param%BP              = NoahmpIO%BPI_TABLE(CROPTYP)
       noahmp%energy%param%KC25            = NoahmpIO%KC25I_TABLE(CROPTYP)
       noahmp%energy%param%KO25            = NoahmpIO%KO25I_TABLE(CROPTYP)
       noahmp%energy%param%AKC             = NoahmpIO%AKCI_TABLE(CROPTYP)
       noahmp%energy%param%AKO             = NoahmpIO%AKOI_TABLE(CROPTYP)
    endif


    end associate

  end subroutine EnergyVarInitTransfer

!=== Transfer model states to output=====

  subroutine EnergyVarOutTransfer(noahmp, NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout) :: NoahmpIO
    type(noahmp_type),   intent(inout) :: noahmp

    ! local loop index
    real(kind=kind_noahmp)           :: LAISUN       ! sunlit leaf area index (m2/m2)
    real(kind=kind_noahmp)           :: LAISHA       ! shaded leaf area index (m2/m2)
    real(kind=kind_noahmp)           :: RB           ! leaf boundary layer resistance (s/m)  
    
    associate(                                                  &
              I           => noahmp%config%domain%ILOC         ,&
              J           => noahmp%config%domain%JLOC         ,&
              NSOIL       => noahmp%config%domain%NSOIL         &
             )

             NoahmpIO%TSK      (I,J)         = noahmp%energy%state%TRAD
             NoahmpIO%HFX      (I,J)         = noahmp%energy%flux%FSH
             NoahmpIO%GRDFLX   (I,J)         = noahmp%energy%flux%SSOIL  
  
             if ( noahmp%energy%state%ALBEDO > -999 ) then
                NoahmpIO%ALBEDO(I,J)         = noahmp%energy%state%ALBEDO
             endif  

             NoahmpIO%TSLB     (I,1:NSOIL,J) = noahmp%energy%state%STC(1:NSOIL)
             NoahmpIO%EMISS    (I,J)         = noahmp%energy%state%EMISSI
             NoahmpIO%QSFC     (I,J)         = noahmp%energy%state%QSFC
             NoahmpIO%TVXY     (I,J)         = noahmp%energy%state%TV 
             NoahmpIO%TGXY     (I,J)         = noahmp%energy%state%TG
             NoahmpIO%EAHXY    (I,J)         = noahmp%energy%state%EAH
             NoahmpIO%TAHXY    (I,J)         = noahmp%energy%state%TAH
             NoahmpIO%CMXY     (I,J)         = noahmp%energy%state%CM
             NoahmpIO%CHXY     (I,J)         = noahmp%energy%state%CH
             NoahmpIO%ALBOLDXY (I,J)         = noahmp%energy%state%ALBOLD
             NoahmpIO%LAI      (I,J)         = noahmp%energy%state%LAI
             NoahmpIO%XSAIXY   (I,J)         = noahmp%energy%state%SAI
             NoahmpIO%TAUSSXY  (I,J)         = noahmp%energy%state%TAUSS
             NoahmpIO%Z0       (I,J)         = noahmp%energy%state%Z0WRF
             NoahmpIO%ZNT      (I,J)         = noahmp%energy%state%Z0WRF
             NoahmpIO%T2MVXY   (I,J)         = noahmp%energy%state%T2MV 
             NoahmpIO%T2MBXY   (I,J)         = noahmp%energy%state%T2MB
             NoahmpIO%Q2MVXY   (I,J)         = noahmp%energy%state%Q2V/(1.0 - noahmp%energy%state%Q2V)  ! specific humidity to mixing ratio   
             NoahmpIO%Q2MBXY   (I,J)         = noahmp%energy%state%Q2B/(1.0 - noahmp%energy%state%Q2B)  ! consistent with registry def of Q2  
             NoahmpIO%TRADXY   (I,J)         = noahmp%energy%state%TRAD
             NoahmpIO%FVEGXY   (I,J)         = noahmp%energy%state%FVEG
             NoahmpIO%FSAXY    (I,J)         = noahmp%energy%flux%FSA 
             NoahmpIO%FIRAXY   (I,J)         = noahmp%energy%flux%FIRA
             NoahmpIO%APARXY   (I,J)         = noahmp%energy%flux%APAR
             NoahmpIO%SAVXY    (I,J)         = noahmp%energy%flux%SAV
             NoahmpIO%SAGXY    (I,J)         = noahmp%energy%flux%SAG 
             NoahmpIO%RSSUNXY  (I,J)         = noahmp%energy%state%RSSUN
             NoahmpIO%RSSHAXY  (I,J)         = noahmp%energy%state%RSSHA
             LAISUN                          = max(noahmp%energy%state%LAISUN, 0.0) 
             LAISHA                          = max(noahmp%energy%state%LAISHA, 0.0) 
             RB                              = max(noahmp%energy%state%RB,     0.0)  

! -- New Calculation of total Canopy/Stomatal Conductance Based on Bonan et al. (2011) 
! -- Inverse of Canopy Resistance (below)

             if(noahmp%energy%state%RSSUN  .le. 0.0 .or. noahmp%energy%state%RSSHA  .le. 0.0 &
                .or. LAISUN .eq. 0.0 .or. LAISHA .eq. 0.0) then
                NoahmpIO%RS    (I,J)   = 0.0
             else
                NoahmpIO%RS    (I,J)   = ((1.0/(noahmp%energy%state%RSSUN+RB)*noahmp%energy%state%LAISUN) + &
                                         ((1.0/(noahmp%energy%state%RSSHA+RB))*noahmp%energy%state%LAISHA))
                NoahmpIO%RS    (I,J)   = 1.0/NoahmpIO%RS (I,J) !Resistance
             endif
             
             NoahmpIO%BGAPXY   (I,J)   = noahmp%energy%state%BGAP
             NoahmpIO%WGAPXY   (I,J)   = noahmp%energy%state%WGAP
             NoahmpIO%TGVXY    (I,J)   = noahmp%energy%state%TGV
             NoahmpIO%TGBXY    (I,J)   = noahmp%energy%state%TGB
             NoahmpIO%CHVXY    (I,J)   = noahmp%energy%state%CHV
             NoahmpIO%CHBXY    (I,J)   = noahmp%energy%state%CHB
             NoahmpIO%IRCXY    (I,J)   = noahmp%energy%flux%IRC
             NoahmpIO%IRGXY    (I,J)   = noahmp%energy%flux%IRG
             NoahmpIO%SHCXY    (I,J)   = noahmp%energy%flux%SHC
             NoahmpIO%SHGXY    (I,J)   = noahmp%energy%flux%SHG
             NoahmpIO%EVGXY    (I,J)   = noahmp%energy%flux%EVG
             NoahmpIO%GHVXY    (I,J)   = noahmp%energy%flux%GHV
             NoahmpIO%IRBXY    (I,J)   = noahmp%energy%flux%IRB
             NoahmpIO%SHBXY    (I,J)   = noahmp%energy%flux%SHB
             NoahmpIO%EVBXY    (I,J)   = noahmp%energy%flux%EVB
             NoahmpIO%GHBXY    (I,J)   = noahmp%energy%flux%GHB
             NoahmpIO%TRXY     (I,J)   = noahmp%energy%flux%TR
             NoahmpIO%EVCXY    (I,J)   = noahmp%energy%flux%EVC
             NoahmpIO%CHLEAFXY (I,J)   = noahmp%energy%state%CHLEAF
             NoahmpIO%CHUCXY   (I,J)   = noahmp%energy%state%CHUC
             NoahmpIO%CHV2XY   (I,J)   = noahmp%energy%state%CHV2
             NoahmpIO%CHB2XY   (I,J)   = noahmp%energy%state%EHB2             
             NoahmpIO%IRRSPLH  (I,J)   = NoahmpIO%IRRSPLH(I,J) + &
                                         (noahmp%energy%flux%FIRR*noahmp%config%domain%DT) ! Joules/m^2               
  
    end associate
    
  end subroutine EnergyVarOutTransfer
  
end module EnergyVarInitMod
