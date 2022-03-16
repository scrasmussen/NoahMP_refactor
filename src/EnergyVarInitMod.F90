module EnergyVarInitMod

!!! Initialize column (1-D) Noah-MP energy variables
!!! Energy variables should be first defined in EnergyType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use InputVarType
  use NoahmpVarType

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

    allocate( noahmp%energy%state%STC     (-NSNOW+1:NSOIL) )
    allocate( noahmp%energy%state%DF      (-NSNOW+1:NSOIL) )
    allocate( noahmp%energy%state%HCPCT   (-NSNOW+1:NSOIL) )
    allocate( noahmp%energy%state%FACT    (-NSNOW+1:NSOIL) )
    allocate( noahmp%energy%state%CVSNO   (-NSNOW+1:0    ) )
    allocate( noahmp%energy%state%TKSNO   (-NSNOW+1:0    ) )
    allocate( noahmp%energy%state%CVSOIL  (       1:NSOIL) )
    allocate( noahmp%energy%state%TKSOIL  (       1:NSOIL) )
    allocate( noahmp%energy%state%ALBSND  (       1:NBAND) )
    allocate( noahmp%energy%state%ALBSNI  (       1:NBAND) )
    allocate( noahmp%energy%state%ALBSOD  (       1:NBAND) )
    allocate( noahmp%energy%state%ALBSOI  (       1:NBAND) )
    allocate( noahmp%energy%state%ALBGRD  (       1:NBAND) )
    allocate( noahmp%energy%state%ALBGRI  (       1:NBAND) )
    allocate( noahmp%energy%state%RHO     (       1:NBAND) )
    allocate( noahmp%energy%state%TAU     (       1:NBAND) )
    allocate( noahmp%energy%state%ALBD    (       1:NBAND) )
    allocate( noahmp%energy%state%ALBI    (       1:NBAND) )

    noahmp%energy%state%STC(:)          = huge(1.0)
    noahmp%energy%state%DF(:)           = huge(1.0)
    noahmp%energy%state%HCPCT(:)        = huge(1.0)
    noahmp%energy%state%FACT(:)         = huge(1.0)
    noahmp%energy%state%CVSNO(:)        = huge(1.0)
    noahmp%energy%state%TKSNO(:)        = huge(1.0)
    noahmp%energy%state%CVSOIL(:)       = huge(1.0)
    noahmp%energy%state%TKSOIL(:)       = huge(1.0)
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
    noahmp%energy%flux%FIRR             = huge(1.0)
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

    allocate( noahmp%energy%flux%FABD    (1:NBAND) )
    allocate( noahmp%energy%flux%FABI    (1:NBAND) )
    allocate( noahmp%energy%flux%FTDD    (1:NBAND) )
    allocate( noahmp%energy%flux%FTDI    (1:NBAND) )
    allocate( noahmp%energy%flux%FTID    (1:NBAND) )
    allocate( noahmp%energy%flux%FTII    (1:NBAND) )
    allocate( noahmp%energy%flux%FREVD   (1:NBAND) )
    allocate( noahmp%energy%flux%FREVI   (1:NBAND) )
    allocate( noahmp%energy%flux%FREGD   (1:NBAND) )
    allocate( noahmp%energy%flux%FREGI   (1:NBAND) )
    allocate( noahmp%energy%flux%SOLAD   (1:NBAND) )
    allocate( noahmp%energy%flux%SOLAI   (1:NBAND) )
    allocate( noahmp%energy%flux%PHI     (-NSNOW+1:NSOIL) )

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

    allocate( noahmp%energy%param%LAIM   (1:12   ) )
    allocate( noahmp%energy%param%SAIM   (1:12   ) )
    allocate( noahmp%energy%param%QUARTZ (1:NSOIL) )
    allocate( noahmp%energy%param%ALBSAT (1:NBAND) )
    allocate( noahmp%energy%param%ALBDRY (1:NBAND) )
    allocate( noahmp%energy%param%ALBLAK (1:NBAND) )
    allocate( noahmp%energy%param%OMEGAS (1:NBAND) )
    allocate( noahmp%energy%param%RHOL   (1:NBAND) )
    allocate( noahmp%energy%param%RHOS   (1:NBAND) )
    allocate( noahmp%energy%param%TAUL   (1:NBAND) )
    allocate( noahmp%energy%param%TAUS   (1:NBAND) )
    allocate( noahmp%energy%param%EG     (1:2    ) )

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

    end associate

  end subroutine EnergyVarInitDefault

!=== initialize with input data or table values
  subroutine EnergyVarInitTransfer(noahmp, input)

    implicit none

    type(input_type) , intent(inout) :: input
    type(noahmp_type), intent(inout) :: noahmp

    ! local loop index
    integer                          :: ISOIL

    associate(                                                  &
              ILOC        => noahmp%config%domain%ILOC         ,&
              JLOC        => noahmp%config%domain%JLOC         ,&
              VEGTYP      => noahmp%config%domain%VEGTYP       ,&
              SOILTYP     => noahmp%config%domain%SOILTYP      ,&
              CROPTYP     => noahmp%config%domain%CROPTYP      ,&
              SOILCOLOR   => noahmp%config%domain%SOILCOLOR    ,&
              NSNOW       => noahmp%config%domain%NSNOW        ,&
              NSOIL       => noahmp%config%domain%NSOIL        ,&
              NBAND       => noahmp%config%domain%NBAND         &
             )

    ! energy state variable
    !noahmp%energy%state%LAI               = input%LAIIn
    !noahmp%energy%state%SAI               = input%SAIIn
    !noahmp%energy%state%TG                = input%TGIn
    !noahmp%energy%state%TV                = input%TVIn
    !noahmp%energy%state%STC               = input%STCIn
    !noahmp%energy%state%TAUSS             = input%TAUSSIn
    !noahmp%energy%state%ALBOLD            = input%ALBOLDIn
    !noahmp%energy%state%EAH               = input%EAHIn
    !noahmp%energy%state%TAH               = input%TAHIn
    !noahmp%energy%state%CH                = input%CHIn
    !noahmp%energy%state%CM                = input%CMIn
    noahmp%energy%state%CO2AIR             = input%CO2_TABLE * input%SFCPRSIn
    noahmp%energy%state%O2AIR              = input%O2_TABLE * input%SFCPRSIn

    ! energy parameter variable
    noahmp%energy%param%RC                 = input%RC_TABLE(VEGTYP)
    noahmp%energy%param%HVT                = input%HVT_TABLE(VEGTYP)
    noahmp%energy%param%HVB                = input%HVB_TABLE(VEGTYP)
    noahmp%energy%param%Z0MVT              = input%Z0MVT_TABLE(VEGTYP)
    noahmp%energy%param%CWPVT              = input%CWPVT_TABLE(VEGTYP)
    noahmp%energy%param%DEN                = input%DEN_TABLE(VEGTYP)
    noahmp%energy%param%XL                 = input%XL_TABLE(VEGTYP)
    noahmp%energy%param%BP                 = input%BP_TABLE(VEGTYP)
    noahmp%energy%param%KC25               = input%KC25_TABLE(VEGTYP)
    noahmp%energy%param%KO25               = input%KO25_TABLE(VEGTYP)
    noahmp%energy%param%AKC                = input%AKC_TABLE(VEGTYP)
    noahmp%energy%param%AKO                = input%AKO_TABLE(VEGTYP)
    noahmp%energy%param%RGL                = input%RGL_TABLE(VEGTYP)
    noahmp%energy%param%RSMIN              = input%RS_TABLE(VEGTYP)
    noahmp%energy%param%RSMAX              = input%RSMAX_TABLE(VEGTYP)
    noahmp%energy%param%TOPT               = input%TOPT_TABLE(VEGTYP)
    noahmp%energy%param%HS                 = input%HS_TABLE(VEGTYP)
    noahmp%energy%param%DLEAF              = input%DLEAF_TABLE(VEGTYP)
    noahmp%energy%param%CSOIL              = input%CSOIL_TABLE
    noahmp%energy%param%TAU0               = input%TAU0_TABLE
    noahmp%energy%param%GRAIN_GROWTH       = input%GRAIN_GROWTH_TABLE
    noahmp%energy%param%DIRT_SOOT          = input%DIRT_SOOT_TABLE
    noahmp%energy%param%EXTRA_GROWTH       = input%EXTRA_GROWTH_TABLE
    noahmp%energy%param%BATS_COSZ          = input%BATS_COSZ_TABLE
    noahmp%energy%param%BATS_VIS_NEW       = input%BATS_VIS_NEW_TABLE
    noahmp%energy%param%BATS_NIR_NEW       = input%BATS_NIR_NEW_TABLE
    noahmp%energy%param%BATS_VIS_AGE       = input%BATS_VIS_AGE_TABLE
    noahmp%energy%param%BATS_NIR_AGE       = input%BATS_NIR_AGE_TABLE
    noahmp%energy%param%BATS_VIS_DIR       = input%BATS_VIS_DIR_TABLE
    noahmp%energy%param%BATS_NIR_DIR       = input%BATS_NIR_DIR_TABLE
    noahmp%energy%param%CLASS_ALB_REF      = input%CLASS_ALB_REF_TABLE
    noahmp%energy%param%CLASS_SNO_AGE      = input%CLASS_SNO_AGE_TABLE
    noahmp%energy%param%CLASS_ALB_NEW      = input%CLASS_ALB_NEW_TABLE
    noahmp%energy%param%BETADS             = input%BETADS_TABLE
    noahmp%energy%param%BETAIS             = input%BETAIS_TABLE
    noahmp%energy%param%CZIL               = input%CZIL_TABLE
    noahmp%energy%param%SNOW_EMIS          = input%SNOW_EMIS_TABLE
    noahmp%energy%param%EG                 = input%EG_TABLE
    noahmp%energy%param%Z0SNO              = input%Z0SNO_TABLE
    noahmp%energy%param%ZBOT               = input%ZBOT_TABLE
    noahmp%energy%param%Z0SOIL             = input%Z0SOIL_TABLE
    noahmp%energy%param%Z0LAKE             = input%Z0LAKE_TABLE
    noahmp%energy%param%EICE               = input%EICE_TABLE
    noahmp%energy%param%RSURF_EXP          = input%RSURF_EXP_TABLE
    noahmp%energy%param%RSURF_SNOW         = input%RSURF_SNOW_TABLE
    noahmp%energy%param%SHDMAX             = input%SHDMAXIn
    noahmp%energy%param%SHDFAC             = input%SHDFACIn

    noahmp%energy%param%LAIM(1:12)         = input%LAIM_TABLE(VEGTYP,1:12)
    noahmp%energy%param%SAIM(1:12)         = input%SAIM_TABLE(VEGTYP,1:12)
    noahmp%energy%param%RHOL(1:NBAND)      = input%RHOL_TABLE(VEGTYP,1:NBAND)
    noahmp%energy%param%RHOS(1:NBAND)      = input%RHOS_TABLE(VEGTYP,1:NBAND)
    noahmp%energy%param%TAUL(1:NBAND)      = input%TAUL_TABLE(VEGTYP,1:NBAND)
    noahmp%energy%param%TAUS(1:NBAND)      = input%TAUS_TABLE(VEGTYP,1:NBAND)
    noahmp%energy%param%ALBSAT(1:NBAND)    = input%ALBSAT_TABLE(SOILCOLOR,1:NBAND)
    noahmp%energy%param%ALBDRY(1:NBAND)    = input%ALBDRY_TABLE(SOILCOLOR,1:NBAND)
    noahmp%energy%param%ALBLAK(1:NBAND)    = input%ALBLAK_TABLE(1:NBAND)
    noahmp%energy%param%OMEGAS(1:NBAND)    = input%OMEGAS_TABLE(1:NBAND)

    do ISOIL = 1, size(SOILTYP)
       noahmp%energy%param%QUARTZ(ISOIL)   = input%QUARTZ_TABLE(SOILTYP(ISOIL))
    enddo

    if ( CROPTYP > 0 ) then
       noahmp%energy%param%BP              = input%BPI_TABLE(CROPTYP)
       noahmp%energy%param%KC25            = input%KC25I_TABLE(CROPTYP)
       noahmp%energy%param%KO25            = input%KO25I_TABLE(CROPTYP)
       noahmp%energy%param%AKC             = input%AKCI_TABLE(CROPTYP)
       noahmp%energy%param%AKO             = input%AKOI_TABLE(CROPTYP)

    endif


    end associate

  end subroutine EnergyVarInitTransfer

end module EnergyVarInitMod
