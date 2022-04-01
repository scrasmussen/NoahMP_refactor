module InputVarInitMod

!!! Initialize Noah-MP input variables (2D forcing, namelist, table, static)
!!! Input variables should be first defined in InputType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use Machine, only : kind_noahmp
!  use ErrorHandleMod
  use InputVarType

  implicit none

contains

!=== read Noahmp Table values
  subroutine ReadNoahmpTable(input)

    implicit none

    type(input_type), intent(inout)  :: input

    !-------------------------------------------------------
    !=== define key dimensional variables
    !-------------------------------------------------------
    integer, parameter :: MVT         = 27   ! number of vegetation types
    integer, parameter :: MBAND       = 2    ! number of radiation bands
    integer, parameter :: MSC         = 8    ! number of soil texture
    integer, parameter :: MAX_SOILTYP = 30   ! max number of soil types
    integer, parameter :: NCROP       = 5    ! number of crop types
    integer, parameter :: NSTAGE      = 8    ! number of crop growth stages
    integer, parameter :: NUM_SLOPE   = 9    ! number of slope

    !-------------------------------------------------------
    !=== define local variables to store NoahmpTable values
    !-------------------------------------------------------
    
    ! MPTABLE.TBL vegetation parameters
    character(len=256)                     :: DATASET_IDENTIFIER
    character(len=256)                     :: VEG_DATASET_DESCRIPTION
    logical                                :: file_named
    integer                                :: ierr, IK, IM
    integer                                :: NVEG, ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL
    integer                                :: LCZ_1, LCZ_2, LCZ_3, LCZ_4, LCZ_5, LCZ_6, LCZ_7, LCZ_8, LCZ_9, LCZ_10, LCZ_11
    real(kind=kind_noahmp), dimension(MVT) :: SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY, SAI_JUN, SAI_JUL, SAI_AUG,        &
                                              SAI_SEP, SAI_OCT, SAI_NOV, SAI_DEC, LAI_JAN, LAI_FEB, LAI_MAR, LAI_APR,        &
                                              LAI_MAY, LAI_JUN, LAI_JUL, LAI_AUG, LAI_SEP, LAI_OCT, LAI_NOV, LAI_DEC,        &
                                              RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR,&
                                              CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, &
                                              AKC, KO25, AKO, AVCMX, AQE, LTOVRC, DILEFC, DILEFW, RMF25, SLA, FRAGR, TMIN,   &
                                              VCMX25, TDLEF, BP, MP, QE25, RMS25, RMR25, ARM, FOLNMX, WDPOOL, WRRAT, MRP,    &
                                              NROOT, RGL, RS, HS, TOPT, RSMAX, RTOVRC, RSDRYC, RSWOODC, BF, WSTRC, LAIMIN,   &
                                              XSAMIN, SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5
    namelist / noahmp_usgs_veg_categories /   VEG_DATASET_DESCRIPTION, NVEG
    namelist / noahmp_usgs_parameters     /   ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL,                 &
                                              LCZ_1, LCZ_2, LCZ_3, LCZ_4, LCZ_5, LCZ_6, LCZ_7, LCZ_8, LCZ_9, LCZ_10, LCZ_11, &
                                              CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, &
                                              AKC, KO25, AKO, AVCMX, AQE, LTOVRC, DILEFC, DILEFW, RMF25, SLA, FRAGR, TMIN,   &
                                              VCMX25, TDLEF, BP, MP, QE25, RMS25, RMR25, ARM, FOLNMX, WDPOOL, WRRAT, MRP,    &
                                              NROOT, RGL, RS, HS, TOPT, RSMAX, RTOVRC, RSDRYC, RSWOODC, BF, WSTRC, LAIMIN,   &
                                              XSAMIN, SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY,                           &
                                              SAI_JUN, SAI_JUL, SAI_AUG, SAI_SEP, SAI_OCT, SAI_NOV, SAI_DEC, LAI_JAN,        &
                                              LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN, LAI_JUL, LAI_AUG, LAI_SEP,        &
                                              LAI_OCT, LAI_NOV, LAI_DEC, RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS,   &
                                              TAUL_NIR, TAUS_VIS, TAUS_NIR, SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5
    namelist / noahmp_modis_veg_categories /  VEG_DATASET_DESCRIPTION, NVEG
    namelist / noahmp_modis_parameters     /  ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL,                 &
                                              LCZ_1, LCZ_2, LCZ_3, LCZ_4, LCZ_5, LCZ_6, LCZ_7, LCZ_8, LCZ_9, LCZ_10, LCZ_11, &
                                              CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, &
                                              AKC, KO25, AKO, AVCMX, AQE, LTOVRC, DILEFC, DILEFW, RMF25, SLA, FRAGR, TMIN,   &
                                              VCMX25, TDLEF, BP, MP, QE25, RMS25, RMR25, ARM, FOLNMX, WDPOOL, WRRAT, MRP,    &
                                              NROOT, RGL, RS, HS, TOPT, RSMAX, RTOVRC, RSDRYC, RSWOODC, BF, WSTRC, LAIMIN,   &
                                              XSAMIN, SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY,                           &
                                              SAI_JUN, SAI_JUL, SAI_AUG, SAI_SEP, SAI_OCT, SAI_NOV, SAI_DEC, LAI_JAN,        &
                                              LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN, LAI_JUL, LAI_AUG, LAI_SEP,        &
                                              LAI_OCT, LAI_NOV, LAI_DEC, RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS,   &
                                              TAUL_NIR, TAUS_VIS, TAUS_NIR, SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5

    ! SOIPARM.TBL soil parameters
    character(len=256)                             :: message
    character(len=10)                              :: SLTYPE
    integer                                        :: SLCATS
    real(kind=kind_noahmp), dimension(MAX_SOILTYP) :: BB, DRYSMC, F11, MAXSMC, REFSMC, SATPSI, SATDK, SATDW, WLTSMC, QTZ,    &
                                                      BVIC, AXAJ, BXAJ, XXAJ, BDVIC, BBVIC, GDVIC
    namelist / noahmp_stas_soil_categories /          SLTYPE, SLCATS
    namelist / noahmp_soil_stas_parameters /          BB, DRYSMC, F11, MAXSMC, REFSMC, SATPSI, SATDK, SATDW, WLTSMC, QTZ,    &
                                                      BVIC, AXAJ, BXAJ, XXAJ, BDVIC, BBVIC, GDVIC
    namelist / noahmp_soil_stas_ruc_parameters /      BB, DRYSMC, F11, MAXSMC, REFSMC, SATPSI, SATDK, SATDW, WLTSMC, QTZ,    &
                                                      BVIC, AXAJ, BXAJ, XXAJ, BDVIC, BBVIC, GDVIC

    ! GENPARM.TBL general parameters
    real(kind=kind_noahmp)                       :: CSOIL_DATA, REFDK_DATA, REFKDT_DATA, FRZK_DATA, ZBOT_DATA, CZIL_DATA
    real(kind=kind_noahmp), dimension(NUM_SLOPE) :: SLOPE_DATA
    namelist / noahmp_general_parameters /          SLOPE_DATA, CSOIL_DATA, REFDK_DATA, REFKDT_DATA, FRZK_DATA, ZBOT_DATA,   &
                                                    CZIL_DATA

    ! MPTABLE.TBL radiation parameters
    real(kind=kind_noahmp)                   :: BETADS, BETAIS, EICE
    real(kind=kind_noahmp), dimension(MBAND) :: ALBICE, ALBLAK, OMEGAS 
    real(kind=kind_noahmp), dimension(2)     :: EG
    real(kind=kind_noahmp), dimension(MSC)   :: ALBSAT_VIS, ALBSAT_NIR, ALBDRY_VIS, ALBDRY_NIR
    namelist / noahmp_rad_parameters /          ALBSAT_VIS, ALBSAT_NIR, ALBDRY_VIS, ALBDRY_NIR, ALBICE, ALBLAK, OMEGAS,      &
                                                BETADS, BETAIS, EG, EICE

    ! MPTABLE.TBL global parameters
    real(kind=kind_noahmp)                   :: CO2, O2, TIMEAN, FSATMX, Z0SNO, SSI, SNOW_RET_FAC ,SNOW_EMIS, SWEMX, TAU0,   &
                                                GRAIN_GROWTH, EXTRA_GROWTH, DIRT_SOOT, BATS_COSZ, BATS_VIS_NEW,              &
                                                BATS_NIR_NEW, BATS_VIS_AGE, BATS_NIR_AGE, BATS_VIS_DIR, BATS_NIR_DIR,        &
                                                RSURF_SNOW, RSURF_EXP, C2_SNOWCOMPACT, C3_SNOWCOMPACT, C4_SNOWCOMPACT,       &
                                                C5_SNOWCOMPACT, DM_SNOWCOMPACT, ETA0_SNOWCOMPACT, SNLIQMAXFRAC, SWEMAXGLA,   &
                                                WSLMAX, ROUS, CMIC, SNOWDEN_MIN, CLASS_ALB_REF, CLASS_SNO_AGE, CLASS_ALB_NEW,&
                                                PSIWLT, Z0SOIL, Z0LAKE
    namelist / noahmp_global_parameters /       CO2, O2, TIMEAN, FSATMX, Z0SNO, SSI, SNOW_RET_FAC ,SNOW_EMIS, SWEMX, TAU0,   &
                                                GRAIN_GROWTH, EXTRA_GROWTH, DIRT_SOOT, BATS_COSZ, BATS_VIS_NEW,              &
                                                BATS_NIR_NEW, BATS_VIS_AGE, BATS_NIR_AGE, BATS_VIS_DIR, BATS_NIR_DIR,        &
                                                RSURF_SNOW, RSURF_EXP, C2_SNOWCOMPACT, C3_SNOWCOMPACT, C4_SNOWCOMPACT,       &
                                                C5_SNOWCOMPACT, DM_SNOWCOMPACT, ETA0_SNOWCOMPACT, SNLIQMAXFRAC, SWEMAXGLA,   &
                                                WSLMAX, ROUS, CMIC, SNOWDEN_MIN, CLASS_ALB_REF, CLASS_SNO_AGE, CLASS_ALB_NEW,&
                                                PSIWLT, Z0SOIL, Z0LAKE

    ! MPTABLE.TBL irrigation parameters
    integer                                  :: IRR_HAR
    real(kind=kind_noahmp)                   :: IRR_FRAC, IRR_LAI, IRR_MAD, FILOSS, SPRIR_RATE, MICIR_RATE, FIRTFAC, IR_RAIN
    namelist / noahmp_irrigation_parameters /   IRR_FRAC, IRR_HAR, IRR_LAI, IRR_MAD, FILOSS, SPRIR_RATE, MICIR_RATE, FIRTFAC,&
                                                IR_RAIN

    ! MPTABLE.TBL crop parameters
    integer                                  :: DEFAULT_CROP
    integer               , dimension(NCROP) :: PLTDAY, HSDAY, C3C4
    real(kind=kind_noahmp), dimension(NCROP) :: PLANTPOP, IRRI, GDDTBASE, GDDTCUT, GDDS1, GDDS2, GDDS3, GDDS4, GDDS5, C3PSNI,&
                                                KC25I, AKCI, KO25I, AKOI, AVCMXI, VCMX25I, BPI, MPI, FOLNMXI, QE25I, AREF,   &
                                                PSNRF, I2PAR, TASSIM0, TASSIM1, TASSIM2, K, EPSI, Q10MR, FOLN_MX, LEFREEZ,   &
                                                DILE_FC_S1, DILE_FC_S2, DILE_FC_S3, DILE_FC_S4, DILE_FC_S5, DILE_FC_S6,      &
                                                DILE_FC_S7, DILE_FC_S8, DILE_FW_S1, DILE_FW_S2, DILE_FW_S3, DILE_FW_S4,      &
                                                DILE_FW_S5, DILE_FW_S6, DILE_FW_S7, DILE_FW_S8, FRA_GR, LF_OVRC_S1,          &
                                                LF_OVRC_S2, LF_OVRC_S3, LF_OVRC_S4, LF_OVRC_S5, LF_OVRC_S6, LF_OVRC_S7,      &
                                                LF_OVRC_S8, ST_OVRC_S1, ST_OVRC_S2, ST_OVRC_S3, ST_OVRC_S4, ST_OVRC_S5,      &
                                                ST_OVRC_S6, ST_OVRC_S7, ST_OVRC_S8, RT_OVRC_S1, RT_OVRC_S2, RT_OVRC_S3,      &
                                                RT_OVRC_S4, RT_OVRC_S5, RT_OVRC_S6, RT_OVRC_S7, RT_OVRC_S8, LFMR25, STMR25,  &
                                                RTMR25, GRAINMR25, LFPT_S1, LFPT_S2, LFPT_S3, LFPT_S4, LFPT_S5, LFPT_S6,     &
                                                LFPT_S7, LFPT_S8, STPT_S1, STPT_S2, STPT_S3, STPT_S4, STPT_S5, STPT_S6,      &
                                                STPT_S7, STPT_S8, RTPT_S1, RTPT_S2, RTPT_S3, RTPT_S4, RTPT_S5, RTPT_S6,      &
                                                RTPT_S7, RTPT_S8, GRAINPT_S1, GRAINPT_S2, GRAINPT_S3, GRAINPT_S4, GRAINPT_S5,&
                                                GRAINPT_S6, GRAINPT_S7, GRAINPT_S8, LFCT_S1, LFCT_S2, LFCT_S3, LFCT_S4,      &
                                                LFCT_S5, LFCT_S6, LFCT_S7, LFCT_S8, STCT_S1, STCT_S2, STCT_S3, STCT_S4,      &
                                                STCT_S5, STCT_S6, STCT_S7, STCT_S8, RTCT_S1, RTCT_S2, RTCT_S3, RTCT_S4,      &
                                                RTCT_S5, RTCT_S6, RTCT_S7, RTCT_S8, BIO2LAI
    namelist / noahmp_crop_parameters /         DEFAULT_CROP, PLTDAY, HSDAY, PLANTPOP, IRRI, GDDTBASE, GDDTCUT, GDDS1, GDDS2,&
                                                GDDS3, GDDS4, GDDS5, C3PSNI, KC25I, AKCI, KO25I, AKOI, AVCMXI, VCMX25I, BPI, &
                                                MPI, FOLNMXI, QE25I, C3C4, AREF, PSNRF, I2PAR, TASSIM0, TASSIM1, TASSIM2, K, &
                                                EPSI,Q10MR, FOLN_MX, LEFREEZ, DILE_FC_S1, DILE_FC_S2, DILE_FC_S3, DILE_FC_S4,&
                                                DILE_FC_S5, DILE_FC_S6, DILE_FC_S7, DILE_FC_S8, DILE_FW_S1, DILE_FW_S2,      &
                                                DILE_FW_S3, DILE_FW_S4, DILE_FW_S5, DILE_FW_S6, DILE_FW_S7, DILE_FW_S8,      &
                                                FRA_GR, LF_OVRC_S1, LF_OVRC_S2, LF_OVRC_S3, LF_OVRC_S4, LF_OVRC_S5,          &
                                                LF_OVRC_S6, LF_OVRC_S7, LF_OVRC_S8, ST_OVRC_S1, ST_OVRC_S2, ST_OVRC_S3,      &
                                                ST_OVRC_S4, ST_OVRC_S5, ST_OVRC_S6, ST_OVRC_S7, ST_OVRC_S8, RT_OVRC_S1,      &
                                                RT_OVRC_S2, RT_OVRC_S3, RT_OVRC_S4, RT_OVRC_S5, RT_OVRC_S6, RT_OVRC_S7,      &
                                                RT_OVRC_S8, LFMR25, STMR25, RTMR25, GRAINMR25, LFPT_S1, LFPT_S2, LFPT_S3,    &
                                                LFPT_S4, LFPT_S5, LFPT_S6, LFPT_S7, LFPT_S8, STPT_S1, STPT_S2, STPT_S3,      &
                                                STPT_S4, STPT_S5, STPT_S6, STPT_S7, STPT_S8, RTPT_S1, RTPT_S2, RTPT_S3,      &
                                                RTPT_S4, RTPT_S5, RTPT_S6, RTPT_S7, RTPT_S8, GRAINPT_S1, GRAINPT_S2,         &
                                                GRAINPT_S3, GRAINPT_S4, GRAINPT_S5, GRAINPT_S6, GRAINPT_S7, GRAINPT_S8,      &
                                                LFCT_S1, LFCT_S2, LFCT_S3, LFCT_S4, LFCT_S5, LFCT_S6, LFCT_S7, LFCT_S8,      &
                                                STCT_S1, STCT_S2, STCT_S3, STCT_S4, STCT_S5, STCT_S6, STCT_S7, STCT_S8,      &
                                                RTCT_S1, RTCT_S2, RTCT_S3, RTCT_S4, RTCT_S5, RTCT_S6, RTCT_S7, RTCT_S8,      &
                                                BIO2LAI

    ! MPTABLE.TBL tile drainage parameters
    integer                                        :: NSOILTYPE, DRAIN_LAYER_OPT
    integer               , dimension(MAX_SOILTYP) :: TD_DEPTH
    real(kind=kind_noahmp), dimension(MAX_SOILTYP) :: TDSMC_FAC, TD_DC, TD_DCOEF, TD_D, TD_ADEPTH, TD_RADI, TD_SPAC,         &
                                                      TD_DDRAIN, KLAT_FAC
    namelist / noahmp_tiledrain_parameters /          NSOILTYPE, DRAIN_LAYER_OPT, TDSMC_FAC, TD_DEPTH, TD_DC, TD_DCOEF, TD_D,&
                                                      TD_ADEPTH, TD_RADI, TD_SPAC, TD_DDRAIN, KLAT_FAC

    ! MPTABLE.TBL optional parameters
    real(kind=kind_noahmp)                         :: sr2006_theta_1500t_a, sr2006_theta_1500t_b, sr2006_theta_1500t_c,      &
                                                      sr2006_theta_1500t_d, sr2006_theta_1500t_e, sr2006_theta_1500t_f,      &
                                                      sr2006_theta_1500t_g, sr2006_theta_1500_a , sr2006_theta_1500_b,       &
                                                      sr2006_theta_33t_a, sr2006_theta_33t_b, sr2006_theta_33t_c,            &
                                                      sr2006_theta_33t_d, sr2006_theta_33t_e, sr2006_theta_33t_f,            &
                                                      sr2006_theta_33t_g, sr2006_theta_33_a, sr2006_theta_33_b,              &
                                                      sr2006_theta_33_c, sr2006_theta_s33t_a, sr2006_theta_s33t_b,           &
                                                      sr2006_theta_s33t_c, sr2006_theta_s33t_d, sr2006_theta_s33t_e,         &
                                                      sr2006_theta_s33t_f, sr2006_theta_s33t_g, sr2006_theta_s33_a,          &
                                                      sr2006_theta_s33_b, sr2006_psi_et_a, sr2006_psi_et_b, sr2006_psi_et_c, &
                                                      sr2006_psi_et_d, sr2006_psi_et_e, sr2006_psi_et_f, sr2006_psi_et_g,    &
                                                      sr2006_psi_e_a, sr2006_psi_e_b, sr2006_psi_e_c, sr2006_smcmax_a,       &
                                                      sr2006_smcmax_b
    namelist / noahmp_optional_parameters /           sr2006_theta_1500t_a, sr2006_theta_1500t_b, sr2006_theta_1500t_c,      &
                                                      sr2006_theta_1500t_d, sr2006_theta_1500t_e, sr2006_theta_1500t_f,      &
                                                      sr2006_theta_1500t_g, sr2006_theta_1500_a, sr2006_theta_1500_b,        &
                                                      sr2006_theta_33t_a, sr2006_theta_33t_b, sr2006_theta_33t_c,            &
                                                      sr2006_theta_33t_d, sr2006_theta_33t_e, sr2006_theta_33t_f,            &
                                                      sr2006_theta_33t_g, sr2006_theta_33_a, sr2006_theta_33_b,              &
                                                      sr2006_theta_33_c, sr2006_theta_s33t_a, sr2006_theta_s33t_b,           &
                                                      sr2006_theta_s33t_c, sr2006_theta_s33t_d, sr2006_theta_s33t_e,         &
                                                      sr2006_theta_s33t_f, sr2006_theta_s33t_g, sr2006_theta_s33_a,          &
                                                      sr2006_theta_s33_b, sr2006_psi_et_a, sr2006_psi_et_b, sr2006_psi_et_c, &
                                                      sr2006_psi_et_d, sr2006_psi_et_e, sr2006_psi_et_f, sr2006_psi_et_g,    &
                                                      sr2006_psi_e_a, sr2006_psi_e_b, sr2006_psi_e_c, sr2006_smcmax_a,       &
                                                      sr2006_smcmax_b

    !--------------------------------------------------
    !=== allocate multi-dim input table variables
    !--------------------------------------------------

    ! MPTABLE.TBL vegetation parameters
    allocate( input%CH2OP_TABLE(MVT) )
    allocate( input%DLEAF_TABLE(MVT) )
    allocate( input%Z0MVT_TABLE(MVT) )
    allocate( input%HVT_TABLE  (MVT) )
    allocate( input%HVB_TABLE  (MVT) )
    allocate( input%DEN_TABLE  (MVT) )
    allocate( input%RC_TABLE   (MVT) )
    allocate( input%MFSNO_TABLE(MVT) )
    allocate( input%SCFFAC_TABLE(MVT) )
    allocate( input%SAIM_TABLE(MVT,12) )
    allocate( input%LAIM_TABLE(MVT,12) )
    allocate( input%SLA_TABLE(MVT )   )
    allocate( input%DILEFC_TABLE(MVT) )
    allocate( input%DILEFW_TABLE(MVT) )
    allocate( input%FRAGR_TABLE(MVT) )
    allocate( input%LTOVRC_TABLE(MVT) )
    allocate( input%C3PSN_TABLE(MVT) )
    allocate( input%KC25_TABLE(MVT) )
    allocate( input%AKC_TABLE(MVT) )
    allocate( input%KO25_TABLE(MVT) )
    allocate( input%AKO_TABLE(MVT) )
    allocate( input%VCMX25_TABLE(MVT) )
    allocate( input%AVCMX_TABLE(MVT) )
    allocate( input%BP_TABLE(MVT) )
    allocate( input%MP_TABLE(MVT) )
    allocate( input%QE25_TABLE(MVT) )
    allocate( input%AQE_TABLE(MVT) )
    allocate( input%RMF25_TABLE(MVT) )
    allocate( input%RMS25_TABLE(MVT) )
    allocate( input%RMR25_TABLE(MVT) )
    allocate( input%ARM_TABLE(MVT) )
    allocate( input%FOLNMX_TABLE(MVT) )
    allocate( input%TMIN_TABLE(MVT) )
    allocate( input%XL_TABLE(MVT) )
    allocate( input%RHOL_TABLE(MVT,MBAND) )
    allocate( input%RHOS_TABLE(MVT,MBAND) )
    allocate( input%TAUL_TABLE(MVT,MBAND) )
    allocate( input%TAUS_TABLE(MVT,MBAND) )
    allocate( input%MRP_TABLE(MVT) )
    allocate( input%CWPVT_TABLE(MVT) )
    allocate( input%WRRAT_TABLE(MVT) )
    allocate( input%WDPOOL_TABLE(MVT) )
    allocate( input%TDLEF_TABLE(MVT) )
    allocate( input%NROOT_TABLE(MVT) )
    allocate( input%RGL_TABLE(MVT) )
    allocate( input%RS_TABLE(MVT) )
    allocate( input%HS_TABLE(MVT) )
    allocate( input%TOPT_TABLE(MVT) )
    allocate( input%RSMAX_TABLE(MVT) )
    allocate( input%RTOVRC_TABLE(MVT) )
    allocate( input%RSDRYC_TABLE(MVT) )
    allocate( input%RSWOODC_TABLE(MVT) )
    allocate( input%BF_TABLE(MVT) )
    allocate( input%WSTRC_TABLE(MVT) )
    allocate( input%LAIMIN_TABLE(MVT) )
    allocate( input%XSAMIN_TABLE(MVT) )

    ! SOILPARM.TBL parameters
    allocate( input%BEXP_TABLE(MAX_SOILTYP) )
    allocate( input%SMCDRY_TABLE(MAX_SOILTYP) )
    allocate( input%F1_TABLE(MAX_SOILTYP) )
    allocate( input%SMCMAX_TABLE(MAX_SOILTYP) )
    allocate( input%SMCREF_TABLE(MAX_SOILTYP) )
    allocate( input%PSISAT_TABLE(MAX_SOILTYP) )
    allocate( input%DKSAT_TABLE(MAX_SOILTYP) )
    allocate( input%DWSAT_TABLE(MAX_SOILTYP) )
    allocate( input%SMCWLT_TABLE(MAX_SOILTYP) )
    allocate( input%QUARTZ_TABLE(MAX_SOILTYP) )
    allocate( input%BVIC_TABLE(MAX_SOILTYP) )
    allocate( input%AXAJ_TABLE(MAX_SOILTYP) )
    allocate( input%BXAJ_TABLE(MAX_SOILTYP) )
    allocate( input%XXAJ_TABLE(MAX_SOILTYP) )
    allocate( input%BDVIC_TABLE(MAX_SOILTYP) )
    allocate( input%GDVIC_TABLE(MAX_SOILTYP) )
    allocate( input%BBVIC_TABLE(MAX_SOILTYP) )

    ! GENPARM.TBL parameters
    allocate( input%SLOPE_TABLE(NUM_SLOPE) )

    ! MPTABLE.TBL radiation parameters
    allocate( input%ALBSAT_TABLE(MSC,MBAND) )
    allocate( input%ALBDRY_TABLE(MSC,MBAND) )
    allocate( input%ALBICE_TABLE(MBAND) )
    allocate( input%ALBLAK_TABLE(MBAND) )
    allocate( input%OMEGAS_TABLE(MBAND) )
    allocate( input%EG_TABLE(2) )

    ! MPTABLE.TBL tile drainage parameters
    allocate( input%TDSMC_FAC_TABLE(MAX_SOILTYP) )
    allocate( input%TD_DC_TABLE(MAX_SOILTYP) )
    allocate( input%TD_DEPTH_TABLE(MAX_SOILTYP) )
    allocate( input%TD_DCOEF_TABLE(MAX_SOILTYP) )
    allocate( input%TD_D_TABLE(MAX_SOILTYP) )
    allocate( input%TD_ADEPTH_TABLE(MAX_SOILTYP) )
    allocate( input%TD_RADI_TABLE(MAX_SOILTYP) )
    allocate( input%TD_SPAC_TABLE(MAX_SOILTYP) )
    allocate( input%TD_DDRAIN_TABLE(MAX_SOILTYP) )
    allocate( input%KLAT_FAC_TABLE(MAX_SOILTYP) )

    ! MPTABLE.TBL crop parameters
    allocate( input%PLTDAY_TABLE(NCROP) )
    allocate( input%HSDAY_TABLE(NCROP) )
    allocate( input%C3C4_TABLE(NCROP) )
    allocate( input%PLANTPOP_TABLE(NCROP) )
    allocate( input%IRRI_TABLE(NCROP) )
    allocate( input%GDDTBASE_TABLE(NCROP) )
    allocate( input%GDDTCUT_TABLE(NCROP) )
    allocate( input%GDDS1_TABLE(NCROP) )
    allocate( input%GDDS2_TABLE(NCROP) )
    allocate( input%GDDS3_TABLE(NCROP) )
    allocate( input%GDDS4_TABLE(NCROP) )
    allocate( input%GDDS5_TABLE(NCROP) )
    allocate( input%C3PSNI_TABLE(NCROP) )
    allocate( input%KC25I_TABLE(NCROP) )
    allocate( input%AKCI_TABLE(NCROP) )
    allocate( input%KO25I_TABLE(NCROP) )
    allocate( input%AKOI_TABLE(NCROP) )
    allocate( input%VCMX25I_TABLE(NCROP) )
    allocate( input%AVCMXI_TABLE(NCROP) )
    allocate( input%BPI_TABLE(NCROP) )
    allocate( input%MPI_TABLE(NCROP) )
    allocate( input%QE25I_TABLE(NCROP) )
    allocate( input%FOLNMXI_TABLE(NCROP) )
    allocate( input%AREF_TABLE(NCROP) )
    allocate( input%PSNRF_TABLE(NCROP) )
    allocate( input%I2PAR_TABLE(NCROP) )
    allocate( input%TASSIM0_TABLE(NCROP) )
    allocate( input%TASSIM1_TABLE(NCROP) )
    allocate( input%TASSIM2_TABLE(NCROP) )
    allocate( input%K_TABLE(NCROP) )
    allocate( input%EPSI_TABLE(NCROP) )
    allocate( input%Q10MR_TABLE(NCROP) )
    allocate( input%FOLN_MX_TABLE(NCROP) )
    allocate( input%LEFREEZ_TABLE(NCROP) )
    allocate( input%DILE_FC_TABLE(NCROP,NSTAGE) )
    allocate( input%DILE_FW_TABLE(NCROP,NSTAGE) )
    allocate( input%FRA_GR_TABLE(NCROP) )
    allocate( input%LF_OVRC_TABLE(NCROP,NSTAGE) )
    allocate( input%ST_OVRC_TABLE(NCROP,NSTAGE) )
    allocate( input%RT_OVRC_TABLE(NCROP,NSTAGE) )
    allocate( input%LFMR25_TABLE(NCROP) )
    allocate( input%STMR25_TABLE(NCROP) )
    allocate( input%RTMR25_TABLE(NCROP) )
    allocate( input%GRAINMR25_TABLE(NCROP) )
    allocate( input%LFPT_TABLE(NCROP,NSTAGE) )
    allocate( input%STPT_TABLE(NCROP,NSTAGE) )
    allocate( input%RTPT_TABLE(NCROP,NSTAGE) )
    allocate( input%GRAINPT_TABLE(NCROP,NSTAGE) )
    allocate( input%LFCT_TABLE(NCROP,NSTAGE) )
    allocate( input%STCT_TABLE(NCROP,NSTAGE) )
    allocate( input%RTCT_TABLE(NCROP,NSTAGE) )
    allocate( input%BIO2LAI_TABLE(NCROP) )

    !---------------------------------------------------------------
    ! intialization to bad value, so that if the namelist read fails,
    ! we come to a screeching halt as soon as we try to use anything
    !---------------------------------------------------------------

    ! MPTABLE.TBL vegetation parameters
    input%ISURBAN_TABLE      = -99999
    input%ISWATER_TABLE      = -99999
    input%ISBARREN_TABLE     = -99999
    input%ISICE_TABLE        = -99999
    input%ISCROP_TABLE       = -99999
    input%EBLFOREST_TABLE    = -99999
    input%NATURAL_TABLE      = -99999
    input%LCZ_1_TABLE        = -99999
    input%LCZ_2_TABLE        = -99999
    input%LCZ_3_TABLE        = -99999
    input%LCZ_4_TABLE        = -99999
    input%LCZ_5_TABLE        = -99999
    input%LCZ_6_TABLE        = -99999
    input%LCZ_7_TABLE        = -99999
    input%LCZ_8_TABLE        = -99999
    input%LCZ_9_TABLE        = -99999
    input%LCZ_10_TABLE       = -99999
    input%LCZ_11_TABLE       = -99999
    input%CH2OP_TABLE        = -1.0e36
    input%DLEAF_TABLE        = -1.0e36
    input%Z0MVT_TABLE        = -1.0e36
    input%HVT_TABLE          = -1.0e36
    input%HVB_TABLE          = -1.0e36
    input%DEN_TABLE          = -1.0e36
    input%RC_TABLE           = -1.0e36
    input%MFSNO_TABLE        = -1.0e36
    input%SCFFAC_TABLE       = -1.0e36
    input%RHOL_TABLE         = -1.0e36
    input%RHOS_TABLE         = -1.0e36
    input%TAUL_TABLE         = -1.0e36
    input%TAUS_TABLE         = -1.0e36
    input%XL_TABLE           = -1.0e36
    input%CWPVT_TABLE        = -1.0e36
    input%C3PSN_TABLE        = -1.0e36
    input%KC25_TABLE         = -1.0e36
    input%AKC_TABLE          = -1.0e36
    input%KO25_TABLE         = -1.0e36
    input%AKO_TABLE          = -1.0e36
    input%AVCMX_TABLE        = -1.0e36
    input%AQE_TABLE          = -1.0e36
    input%LTOVRC_TABLE       = -1.0e36
    input%DILEFC_TABLE       = -1.0e36
    input%DILEFW_TABLE       = -1.0e36
    input%RMF25_TABLE        = -1.0e36
    input%SLA_TABLE          = -1.0e36
    input%FRAGR_TABLE        = -1.0e36
    input%TMIN_TABLE         = -1.0e36
    input%VCMX25_TABLE       = -1.0e36
    input%TDLEF_TABLE        = -1.0e36
    input%BP_TABLE           = -1.0e36
    input%MP_TABLE           = -1.0e36
    input%QE25_TABLE         = -1.0e36
    input%RMS25_TABLE        = -1.0e36
    input%RMR25_TABLE        = -1.0e36
    input%ARM_TABLE          = -1.0e36
    input%FOLNMX_TABLE       = -1.0e36
    input%WDPOOL_TABLE       = -1.0e36
    input%WRRAT_TABLE        = -1.0e36
    input%MRP_TABLE          = -1.0e36
    input%SAIM_TABLE         = -1.0e36
    input%LAIM_TABLE         = -1.0e36
    input%NROOT_TABLE        = -1.0e36
    input%RGL_TABLE          = -1.0e36
    input%RS_TABLE           = -1.0e36
    input%HS_TABLE           = -1.0e36
    input%TOPT_TABLE         = -1.0e36
    input%RSMAX_TABLE        = -1.0e36
    input%RTOVRC_TABLE       = -1.0e36
    input%RSDRYC_TABLE       = -1.0e36
    input%RSWOODC_TABLE      = -1.0e36
    input%BF_TABLE           = -1.0e36
    input%WSTRC_TABLE        = -1.0e36
    input%LAIMIN_TABLE       = -1.0e36
    input%XSAMIN_TABLE       = -1.0e36

    ! SOILPARM.TBL soil parameters
    input%SLCATS_TABLE       = -99999
    input%BEXP_TABLE         = -1.0e36
    input%SMCDRY_TABLE       = -1.0e36
    input%F1_TABLE           = -1.0e36
    input%SMCMAX_TABLE       = -1.0e36
    input%SMCREF_TABLE       = -1.0e36
    input%PSISAT_TABLE       = -1.0e36
    input%DKSAT_TABLE        = -1.0e36
    input%DWSAT_TABLE        = -1.0e36
    input%SMCWLT_TABLE       = -1.0e36
    input%QUARTZ_TABLE       = -1.0e36
    input%BVIC_TABLE         = -1.0e36
    input%AXAJ_TABLE         = -1.0e36
    input%BXAJ_TABLE         = -1.0e36
    input%XXAJ_TABLE         = -1.0e36
    input%BDVIC_TABLE        = -1.0e36
    input%GDVIC_TABLE        = -1.0e36
    input%BBVIC_TABLE        = -1.0e36

    ! GENPARM.TBL general parameters
    input%SLOPE_TABLE        = -1.0e36
    input%CSOIL_TABLE        = -1.0e36
    input%REFDK_TABLE        = -1.0e36
    input%REFKDT_TABLE       = -1.0e36
    input%FRZK_TABLE         = -1.0e36
    input%ZBOT_TABLE         = -1.0e36
    input%CZIL_TABLE         = -1.0e36

    ! MPTABLE.TBL radiation parameters
    input%ALBSAT_TABLE       = -1.0e36
    input%ALBDRY_TABLE       = -1.0e36
    input%ALBICE_TABLE       = -1.0e36
    input%ALBLAK_TABLE       = -1.0e36
    input%OMEGAS_TABLE       = -1.0e36
    input%BETADS_TABLE       = -1.0e36
    input%BETAIS_TABLE       = -1.0e36
    input%EG_TABLE           = -1.0e36
    input%EICE_TABLE         = -1.0e36

    ! MPTABLE.TBL global parameters
    input%CO2_TABLE              = -1.0e36
    input%O2_TABLE               = -1.0e36
    input%TIMEAN_TABLE           = -1.0e36
    input%FSATMX_TABLE           = -1.0e36
    input%Z0SNO_TABLE            = -1.0e36
    input%SSI_TABLE              = -1.0e36
    input%SNOW_RET_FAC_TABLE     = -1.0e36
    input%SNOW_EMIS_TABLE        = -1.0e36
    input%SWEMX_TABLE            = -1.0e36
    input%TAU0_TABLE             = -1.0e36
    input%GRAIN_GROWTH_TABLE     = -1.0e36
    input%EXTRA_GROWTH_TABLE     = -1.0e36
    input%DIRT_SOOT_TABLE        = -1.0e36
    input%BATS_COSZ_TABLE        = -1.0e36
    input%BATS_VIS_NEW_TABLE     = -1.0e36
    input%BATS_NIR_NEW_TABLE     = -1.0e36
    input%BATS_VIS_AGE_TABLE     = -1.0e36
    input%BATS_NIR_AGE_TABLE     = -1.0e36
    input%BATS_VIS_DIR_TABLE     = -1.0e36
    input%BATS_NIR_DIR_TABLE     = -1.0e36
    input%RSURF_SNOW_TABLE       = -1.0e36
    input%RSURF_EXP_TABLE        = -1.0e36
    input%C2_SNOWCOMPACT_TABLE   = -1.0e36
    input%C3_SNOWCOMPACT_TABLE   = -1.0e36
    input%C4_SNOWCOMPACT_TABLE   = -1.0e36
    input%C5_SNOWCOMPACT_TABLE   = -1.0e36
    input%DM_SNOWCOMPACT_TABLE   = -1.0e36
    input%ETA0_SNOWCOMPACT_TABLE = -1.0e36
    input%SNLIQMAXFRAC_TABLE     = -1.0e36
    input%SWEMAXGLA_TABLE        = -1.0e36
    input%WSLMAX_TABLE           = -1.0e36
    input%ROUS_TABLE             = -1.0e36
    input%CMIC_TABLE             = -1.0e36
    input%SNOWDEN_MIN_TABLE      = -1.0e36
    input%CLASS_ALB_REF_TABLE    = -1.0e36
    input%CLASS_SNO_AGE_TABLE    = -1.0e36
    input%CLASS_ALB_NEW_TABLE    = -1.0e36
    input%PSIWLT_TABLE           = -1.0e36
    input%Z0SOIL_TABLE           = -1.0e36
    input%Z0LAKE_TABLE           = -1.0e36

    ! MPTABLE.TBL irrigation parameters
    input%IRR_HAR_TABLE          = -99999
    input%IRR_FRAC_TABLE         = -1.0e36
    input%IRR_LAI_TABLE          = -1.0e36
    input%IRR_MAD_TABLE          = -1.0e36
    input%FILOSS_TABLE           = -1.0e36
    input%SPRIR_RATE_TABLE       = -1.0e36
    input%MICIR_RATE_TABLE       = -1.0e36
    input%FIRTFAC_TABLE          = -1.0e36
    input%IR_RAIN_TABLE          = -1.0e36

    ! MPTABLE.TBL crop parameters
    input%DEFAULT_CROP_TABLE     = -99999
    input%PLTDAY_TABLE           = -99999
    input%HSDAY_TABLE            = -99999
    input%PLANTPOP_TABLE         = -1.0e36
    input%IRRI_TABLE             = -1.0e36
    input%GDDTBASE_TABLE         = -1.0e36
    input%GDDTCUT_TABLE          = -1.0e36
    input%GDDS1_TABLE            = -1.0e36
    input%GDDS2_TABLE            = -1.0e36
    input%GDDS3_TABLE            = -1.0e36
    input%GDDS4_TABLE            = -1.0e36
    input%GDDS5_TABLE            = -1.0e36
    input%C3PSNI_TABLE           = -1.0e36
    input%KC25I_TABLE            = -1.0e36
    input%AKCI_TABLE             = -1.0e36
    input%KO25I_TABLE            = -1.0e36
    input%AKOI_TABLE             = -1.0e36
    input%AVCMXI_TABLE           = -1.0e36
    input%VCMX25I_TABLE          = -1.0e36
    input%BPI_TABLE              = -1.0e36
    input%MPI_TABLE              = -1.0e36
    input%FOLNMXI_TABLE          = -1.0e36
    input%QE25I_TABLE            = -1.0e36
    input%C3C4_TABLE             = -99999
    input%AREF_TABLE             = -1.0e36
    input%PSNRF_TABLE            = -1.0e36
    input%I2PAR_TABLE            = -1.0e36
    input%TASSIM0_TABLE          = -1.0e36
    input%TASSIM1_TABLE          = -1.0e36
    input%TASSIM2_TABLE          = -1.0e36
    input%K_TABLE                = -1.0e36
    input%EPSI_TABLE             = -1.0e36
    input%Q10MR_TABLE            = -1.0e36
    input%FOLN_MX_TABLE          = -1.0e36
    input%LEFREEZ_TABLE          = -1.0e36
    input%DILE_FC_TABLE          = -1.0e36
    input%DILE_FW_TABLE          = -1.0e36
    input%FRA_GR_TABLE           = -1.0e36
    input%LF_OVRC_TABLE          = -1.0e36
    input%ST_OVRC_TABLE          = -1.0e36
    input%RT_OVRC_TABLE          = -1.0e36
    input%LFMR25_TABLE           = -1.0e36
    input%STMR25_TABLE           = -1.0e36
    input%RTMR25_TABLE           = -1.0e36
    input%GRAINMR25_TABLE        = -1.0e36
    input%LFPT_TABLE             = -1.0e36
    input%STPT_TABLE             = -1.0e36
    input%RTPT_TABLE             = -1.0e36
    input%GRAINPT_TABLE          = -1.0e36
    input%LFCT_TABLE             = -1.0e36
    input%STCT_TABLE             = -1.0e36
    input%RTCT_TABLE             = -1.0e36
    input%BIO2LAI_TABLE          = -1.0e36

    ! MPTABLE.TBL tile drainage parameters
    input%DRAIN_LAYER_OPT_TABLE  = -99999
    input%TD_DEPTH_TABLE         = -99999
    input%TDSMC_FAC_TABLE        = -1.0e36
    input%TD_DC_TABLE            = -1.0e36
    input%TD_DCOEF_TABLE         = -1.0e36
    input%TD_D_TABLE             = -1.0e36
    input%TD_ADEPTH_TABLE        = -1.0e36
    input%TD_RADI_TABLE          = -1.0e36
    input%TD_SPAC_TABLE          = -1.0e36
    input%TD_DDRAIN_TABLE        = -1.0e36
    input%KLAT_FAC_TABLE         = -1.0e36

    ! MPTABLE.TBL optional parameters
    input%sr2006_theta_1500t_a_TABLE = -1.0e36
    input%sr2006_theta_1500t_b_TABLE = -1.0e36
    input%sr2006_theta_1500t_c_TABLE = -1.0e36
    input%sr2006_theta_1500t_d_TABLE = -1.0e36
    input%sr2006_theta_1500t_e_TABLE = -1.0e36
    input%sr2006_theta_1500t_f_TABLE = -1.0e36
    input%sr2006_theta_1500t_g_TABLE = -1.0e36
    input%sr2006_theta_1500_a_TABLE  = -1.0e36
    input%sr2006_theta_1500_b_TABLE  = -1.0e36
    input%sr2006_theta_33t_a_TABLE   = -1.0e36
    input%sr2006_theta_33t_b_TABLE   = -1.0e36
    input%sr2006_theta_33t_c_TABLE   = -1.0e36
    input%sr2006_theta_33t_d_TABLE   = -1.0e36
    input%sr2006_theta_33t_e_TABLE   = -1.0e36
    input%sr2006_theta_33t_f_TABLE   = -1.0e36
    input%sr2006_theta_33t_g_TABLE   = -1.0e36
    input%sr2006_theta_33_a_TABLE    = -1.0e36
    input%sr2006_theta_33_b_TABLE    = -1.0e36
    input%sr2006_theta_33_c_TABLE    = -1.0e36
    input%sr2006_theta_s33t_a_TABLE  = -1.0e36
    input%sr2006_theta_s33t_b_TABLE  = -1.0e36
    input%sr2006_theta_s33t_c_TABLE  = -1.0e36
    input%sr2006_theta_s33t_d_TABLE  = -1.0e36
    input%sr2006_theta_s33t_e_TABLE  = -1.0e36
    input%sr2006_theta_s33t_f_TABLE  = -1.0e36
    input%sr2006_theta_s33t_g_TABLE  = -1.0e36
    input%sr2006_theta_s33_a_TABLE   = -1.0e36
    input%sr2006_theta_s33_b_TABLE   = -1.0e36
    input%sr2006_psi_et_a_TABLE      = -1.0e36
    input%sr2006_psi_et_b_TABLE      = -1.0e36
    input%sr2006_psi_et_c_TABLE      = -1.0e36
    input%sr2006_psi_et_d_TABLE      = -1.0e36
    input%sr2006_psi_et_e_TABLE      = -1.0e36
    input%sr2006_psi_et_f_TABLE      = -1.0e36
    input%sr2006_psi_et_g_TABLE      = -1.0e36
    input%sr2006_psi_e_a_TABLE       = -1.0e36
    input%sr2006_psi_e_b_TABLE       = -1.0e36
    input%sr2006_psi_e_c_TABLE       = -1.0e36
    input%sr2006_smcmax_a_TABLE      = -1.0e36
    input%sr2006_smcmax_b_TABLE      = -1.0e36

    !---------------------------------------------------------------
    ! transfer values from table to input variables
    !---------------------------------------------------------------

    !---------------- MPTABLE.TBL vegetation parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
       open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
       open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if ( ierr /= 0 ) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    DATASET_IDENTIFIER = "MODIFIED_IGBP_MODIS_NOAH" ! set to MODIS type for now for testing
    if ( trim(DATASET_IDENTIFIER) == "USGS" ) then
       read(15, noahmp_usgs_veg_categories)
       read(15, noahmp_usgs_parameters)
    elseif ( trim(DATASET_IDENTIFIER) == "MODIFIED_IGBP_MODIS_NOAH" ) then
       read(15,noahmp_modis_veg_categories)
       read(15,noahmp_modis_parameters)
    else
       write(*,'("WARNING: Unrecognized DATASET_IDENTIFIER in subroutine ReadNoahmpTable")')
       write(*,'("WARNING: DATASET_IDENTIFIER = ''", A, "''")') trim(DATASET_IDENTIFIER)
    endif
    close(15)
    ! assign values
    input%ISURBAN_TABLE         = ISURBAN
    input%ISWATER_TABLE         = ISWATER
    input%ISBARREN_TABLE        = ISBARREN
    input%ISICE_TABLE           = ISICE
    input%ISCROP_TABLE          = ISCROP
    input%EBLFOREST_TABLE       = EBLFOREST
    input%NATURAL_TABLE         = NATURAL
    input%LCZ_1_TABLE           = LCZ_1
    input%LCZ_2_TABLE           = LCZ_2
    input%LCZ_3_TABLE           = LCZ_3
    input%LCZ_4_TABLE           = LCZ_4
    input%LCZ_5_TABLE           = LCZ_5
    input%LCZ_6_TABLE           = LCZ_6
    input%LCZ_7_TABLE           = LCZ_7
    input%LCZ_8_TABLE           = LCZ_8
    input%LCZ_9_TABLE           = LCZ_9
    input%LCZ_10_TABLE          = LCZ_10
    input%LCZ_11_TABLE          = LCZ_11
    input%CH2OP_TABLE(1:NVEG)   = CH2OP(1:NVEG)
    input%DLEAF_TABLE(1:NVEG)   = DLEAF(1:NVEG)
    input%Z0MVT_TABLE(1:NVEG)   = Z0MVT(1:NVEG)
    input%HVT_TABLE(1:NVEG)     = HVT(1:NVEG)
    input%HVB_TABLE(1:NVEG)     = HVB(1:NVEG)
    input%DEN_TABLE(1:NVEG)     = DEN(1:NVEG)
    input%RC_TABLE(1:NVEG)      = RC(1:NVEG)
    input%MFSNO_TABLE(1:NVEG)   = MFSNO(1:NVEG)
    input%SCFFAC_TABLE(1:NVEG)  = SCFFAC(1:NVEG)
    input%XL_TABLE(1:NVEG)      = XL(1:NVEG)
    input%CWPVT_TABLE(1:NVEG)   = CWPVT(1:NVEG)
    input%C3PSN_TABLE(1:NVEG)   = C3PSN(1:NVEG)
    input%KC25_TABLE(1:NVEG)    = KC25(1:NVEG)
    input%AKC_TABLE(1:NVEG)     = AKC(1:NVEG)
    input%KO25_TABLE(1:NVEG)    = KO25(1:NVEG)
    input%AKO_TABLE(1:NVEG)     = AKO(1:NVEG)
    input%AVCMX_TABLE(1:NVEG)   = AVCMX(1:NVEG)
    input%AQE_TABLE(1:NVEG)     = AQE(1:NVEG)
    input%LTOVRC_TABLE(1:NVEG)  = LTOVRC(1:NVEG)
    input%DILEFC_TABLE(1:NVEG)  = DILEFC(1:NVEG)
    input%DILEFW_TABLE(1:NVEG)  = DILEFW(1:NVEG)
    input%RMF25_TABLE(1:NVEG)   = RMF25(1:NVEG)
    input%SLA_TABLE(1:NVEG)     = SLA(1:NVEG)
    input%FRAGR_TABLE(1:NVEG)   = FRAGR(1:NVEG)
    input%TMIN_TABLE(1:NVEG)    = TMIN(1:NVEG)
    input%VCMX25_TABLE(1:NVEG)  = VCMX25(1:NVEG)
    input%TDLEF_TABLE(1:NVEG)   = TDLEF(1:NVEG)
    input%BP_TABLE(1:NVEG)      = BP(1:NVEG)
    input%MP_TABLE(1:NVEG)      = MP(1:NVEG)
    input%QE25_TABLE(1:NVEG)    = QE25(1:NVEG)
    input%RMS25_TABLE(1:NVEG)   = RMS25(1:NVEG)
    input%RMR25_TABLE(1:NVEG)   = RMR25(1:NVEG)
    input%ARM_TABLE(1:NVEG)     = ARM(1:NVEG)
    input%FOLNMX_TABLE(1:NVEG)  = FOLNMX(1:NVEG)
    input%WDPOOL_TABLE(1:NVEG)  = WDPOOL(1:NVEG)
    input%WRRAT_TABLE(1:NVEG)   = WRRAT(1:NVEG)
    input%MRP_TABLE(1:NVEG)     = MRP(1:NVEG)
    input%NROOT_TABLE(1:NVEG)   = NROOT(1:NVEG)
    input%RGL_TABLE(1:NVEG)     = RGL(1:NVEG)
    input%RS_TABLE(1:NVEG)      = RS(1:NVEG)
    input%HS_TABLE(1:NVEG)      = HS(1:NVEG)
    input%TOPT_TABLE(1:NVEG)    = TOPT(1:NVEG)
    input%RSMAX_TABLE(1:NVEG)   = RSMAX(1:NVEG)
    input%RTOVRC_TABLE(1:NVEG)  = RTOVRC(1:NVEG)
    input%RSDRYC_TABLE(1:NVEG)  = RSDRYC(1:NVEG)
    input%RSWOODC_TABLE(1:NVEG) = RSWOODC(1:NVEG)
    input%BF_TABLE(1:NVEG)      = BF(1:NVEG)
    input%WSTRC_TABLE(1:NVEG)   = WSTRC(1:NVEG)
    input%LAIMIN_TABLE(1:NVEG)  = LAIMIN(1:NVEG)
    input%XSAMIN_TABLE(1:NVEG)  = XSAMIN(1:NVEG)

    input%SAIM_TABLE(1:NVEG, 1) = SAI_JAN(1:NVEG)
    input%SAIM_TABLE(1:NVEG, 2) = SAI_FEB(1:NVEG)
    input%SAIM_TABLE(1:NVEG, 3) = SAI_MAR(1:NVEG)
    input%SAIM_TABLE(1:NVEG, 4) = SAI_APR(1:NVEG)
    input%SAIM_TABLE(1:NVEG, 5) = SAI_MAY(1:NVEG)
    input%SAIM_TABLE(1:NVEG, 6) = SAI_JUN(1:NVEG)
    input%SAIM_TABLE(1:NVEG, 7) = SAI_JUL(1:NVEG)
    input%SAIM_TABLE(1:NVEG, 8) = SAI_AUG(1:NVEG)
    input%SAIM_TABLE(1:NVEG, 9) = SAI_SEP(1:NVEG)
    input%SAIM_TABLE(1:NVEG,10) = SAI_OCT(1:NVEG)
    input%SAIM_TABLE(1:NVEG,11) = SAI_NOV(1:NVEG)
    input%SAIM_TABLE(1:NVEG,12) = SAI_DEC(1:NVEG)
    input%LAIM_TABLE(1:NVEG, 1) = LAI_JAN(1:NVEG)
    input%LAIM_TABLE(1:NVEG, 2) = LAI_FEB(1:NVEG)
    input%LAIM_TABLE(1:NVEG, 3) = LAI_MAR(1:NVEG)
    input%LAIM_TABLE(1:NVEG, 4) = LAI_APR(1:NVEG)
    input%LAIM_TABLE(1:NVEG, 5) = LAI_MAY(1:NVEG)
    input%LAIM_TABLE(1:NVEG, 6) = LAI_JUN(1:NVEG)
    input%LAIM_TABLE(1:NVEG, 7) = LAI_JUL(1:NVEG)
    input%LAIM_TABLE(1:NVEG, 8) = LAI_AUG(1:NVEG)
    input%LAIM_TABLE(1:NVEG, 9) = LAI_SEP(1:NVEG)
    input%LAIM_TABLE(1:NVEG,10) = LAI_OCT(1:NVEG)
    input%LAIM_TABLE(1:NVEG,11) = LAI_NOV(1:NVEG)
    input%LAIM_TABLE(1:NVEG,12) = LAI_DEC(1:NVEG)
    input%RHOL_TABLE(1:NVEG,1)  = RHOL_VIS(1:NVEG) !leaf reflectance: 1=vis, 2=nir
    input%RHOL_TABLE(1:NVEG,2)  = RHOL_NIR(1:NVEG) !leaf reflectance: 1=vis, 2=nir
    input%RHOS_TABLE(1:NVEG,1)  = RHOS_VIS(1:NVEG) !stem reflectance: 1=vis, 2=nir
    input%RHOS_TABLE(1:NVEG,2)  = RHOS_NIR(1:NVEG) !stem reflectance: 1=vis, 2=nir
    input%TAUL_TABLE(1:NVEG,1)  = TAUL_VIS(1:NVEG) !leaf transmittance: 1=vis, 2=nir
    input%TAUL_TABLE(1:NVEG,2)  = TAUL_NIR(1:NVEG) !leaf transmittance: 1=vis, 2=nir
    input%TAUS_TABLE(1:NVEG,1)  = TAUS_VIS(1:NVEG) !stem transmittance: 1=vis, 2=nir
    input%TAUS_TABLE(1:NVEG,2)  = TAUS_NIR(1:NVEG) !stem transmittance: 1=vis, 2=nir

    !---------------- SOILPARM.TBL soil parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
       open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
       open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if ( ierr /= 0 ) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    read(15, noahmp_stas_soil_categories)
    if ( trim(SLTYPE) == "STAS" ) then
       read(15, noahmp_soil_stas_parameters)
    elseif ( trim(SLTYPE) == "STAS_RUC" ) then
       read(15, noahmp_soil_stas_ruc_parameters)
    else
       write(*,'("WARNING: Unrecognized SOILTYPE in subroutine ReadNoahmpTable")')
       write(*,'("WARNING: DATASET_IDENTIFIER = ''", A, "''")') trim(SLTYPE)
    endif
    close(15)
    ! assign values
    input%SLCATS_TABLE             = SLCATS
    input%BEXP_TABLE(1:SLCATS)     = BB(1:SLCATS)
    input%SMCDRY_TABLE(1:SLCATS)   = DRYSMC(1:SLCATS)
    input%F1_TABLE(1:SLCATS)       = F11(1:SLCATS)
    input%SMCMAX_TABLE(1:SLCATS)   = MAXSMC(1:SLCATS)
    input%SMCREF_TABLE(1:SLCATS)   = REFSMC(1:SLCATS)
    input%PSISAT_TABLE(1:SLCATS)   = SATPSI(1:SLCATS)
    input%DKSAT_TABLE(1:SLCATS)    = SATDK(1:SLCATS)
    input%DWSAT_TABLE(1:SLCATS)    = SATDW(1:SLCATS)
    input%SMCWLT_TABLE(1:SLCATS)   = WLTSMC(1:SLCATS)
    input%QUARTZ_TABLE(1:SLCATS)   = QTZ(1:SLCATS)
    input%BVIC_TABLE(1:SLCATS)     = BVIC(1:SLCATS)
    input%AXAJ_TABLE(1:SLCATS)     = AXAJ(1:SLCATS)
    input%BXAJ_TABLE(1:SLCATS)     = BXAJ(1:SLCATS)
    input%XXAJ_TABLE(1:SLCATS)     = XXAJ(1:SLCATS)
    input%BDVIC_TABLE(1:SLCATS)    = BDVIC(1:SLCATS)
    input%GDVIC_TABLE(1:SLCATS)    = GDVIC(1:SLCATS)
    input%BBVIC_TABLE(1:SLCATS)    = BBVIC(1:SLCATS)

    !---------------- GENPARM.TBL general parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
       open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
       open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if ( ierr /= 0 ) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    read(15, noahmp_general_parameters)
    close(15)
    ! assign values
    input%SLOPE_TABLE(1:NUM_SLOPE) = SLOPE_DATA(1:NUM_SLOPE)
    input%CSOIL_TABLE              = CSOIL_DATA
    input%REFDK_TABLE              = REFDK_DATA
    input%REFKDT_TABLE             = REFKDT_DATA
    input%FRZK_TABLE               = FRZK_DATA
    input%ZBOT_TABLE               = ZBOT_DATA
    input%CZIL_TABLE               = CZIL_DATA

    !---------------- MPTABLE.TBL radiation parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    read(15,noahmp_rad_parameters)
    close(15)
    ! assign values
    input%ALBSAT_TABLE(:,1) = ALBSAT_VIS ! saturated soil albedos: 1=vis, 2=nir
    input%ALBSAT_TABLE(:,2) = ALBSAT_NIR ! saturated soil albedos: 1=vis, 2=nir
    input%ALBDRY_TABLE(:,1) = ALBDRY_VIS ! dry soil albedos: 1=vis, 2=nir
    input%ALBDRY_TABLE(:,2) = ALBDRY_NIR ! dry soil albedos: 1=vis, 2=nir
    input%ALBICE_TABLE      = ALBICE
    input%ALBLAK_TABLE      = ALBLAK
    input%OMEGAS_TABLE      = OMEGAS
    input%BETADS_TABLE      = BETADS
    input%BETAIS_TABLE      = BETAIS
    input%EG_TABLE          = EG
    input%EICE_TABLE        = EICE

    !---------------- MPTABLE.TBL global parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    read(15,noahmp_global_parameters)
    close(15)
    ! assign values
    input%CO2_TABLE              = CO2
    input%O2_TABLE               = O2
    input%TIMEAN_TABLE           = TIMEAN
    input%FSATMX_TABLE           = FSATMX
    input%Z0SNO_TABLE            = Z0SNO
    input%SSI_TABLE              = SSI
    input%SNOW_RET_FAC_TABLE     = SNOW_RET_FAC
    input%SNOW_EMIS_TABLE        = SNOW_EMIS
    input%SWEMX_TABLE            = SWEMX
    input%TAU0_TABLE             = TAU0
    input%GRAIN_GROWTH_TABLE     = GRAIN_GROWTH
    input%EXTRA_GROWTH_TABLE     = EXTRA_GROWTH
    input%DIRT_SOOT_TABLE        = DIRT_SOOT
    input%BATS_COSZ_TABLE        = BATS_COSZ
    input%BATS_VIS_NEW_TABLE     = BATS_VIS_NEW
    input%BATS_NIR_NEW_TABLE     = BATS_NIR_NEW
    input%BATS_VIS_AGE_TABLE     = BATS_VIS_AGE
    input%BATS_NIR_AGE_TABLE     = BATS_NIR_AGE
    input%BATS_VIS_DIR_TABLE     = BATS_VIS_DIR
    input%BATS_NIR_DIR_TABLE     = BATS_NIR_DIR
    input%RSURF_SNOW_TABLE       = RSURF_SNOW
    input%RSURF_EXP_TABLE        = RSURF_EXP
    input%C2_SNOWCOMPACT_TABLE   = C2_SNOWCOMPACT
    input%C3_SNOWCOMPACT_TABLE   = C3_SNOWCOMPACT
    input%C4_SNOWCOMPACT_TABLE   = C4_SNOWCOMPACT
    input%C5_SNOWCOMPACT_TABLE   = C5_SNOWCOMPACT
    input%DM_SNOWCOMPACT_TABLE   = DM_SNOWCOMPACT
    input%ETA0_SNOWCOMPACT_TABLE = ETA0_SNOWCOMPACT
    input%SNLIQMAXFRAC_TABLE     = SNLIQMAXFRAC
    input%SWEMAXGLA_TABLE        = SWEMAXGLA
    input%WSLMAX_TABLE           = WSLMAX
    input%ROUS_TABLE             = ROUS
    input%CMIC_TABLE             = CMIC
    input%SNOWDEN_MIN_TABLE      = SNOWDEN_MIN
    input%CLASS_ALB_REF_TABLE    = CLASS_ALB_REF
    input%CLASS_SNO_AGE_TABLE    = CLASS_SNO_AGE
    input%CLASS_ALB_NEW_TABLE    = CLASS_ALB_NEW
    input%PSIWLT_TABLE           = PSIWLT
    input%Z0SOIL_TABLE           = Z0SOIL
    input%Z0LAKE_TABLE           = Z0LAKE

    !---------------- MPTABLE.TBL irrigation parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    read(15,noahmp_irrigation_parameters)
    close(15)
    ! assign values
    input%IRR_FRAC_TABLE         = IRR_FRAC
    input%IRR_HAR_TABLE          = IRR_HAR
    input%IRR_LAI_TABLE          = IRR_LAI
    input%IRR_MAD_TABLE          = IRR_MAD
    input%FILOSS_TABLE           = FILOSS  
    input%SPRIR_RATE_TABLE       = SPRIR_RATE
    input%MICIR_RATE_TABLE       = MICIR_RATE
    input%FIRTFAC_TABLE          = FIRTFAC
    input%IR_RAIN_TABLE          = IR_RAIN 

    !---------------- MPTABLE.TBL crop parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    read(15,noahmp_crop_parameters)
    close(15)
    ! assign values
    input%DEFAULT_CROP_TABLE     = DEFAULT_CROP
    input%PLTDAY_TABLE           = PLTDAY
    input%HSDAY_TABLE            = HSDAY
    input%PLANTPOP_TABLE         = PLANTPOP
    input%IRRI_TABLE             = IRRI
    input%GDDTBASE_TABLE         = GDDTBASE
    input%GDDTCUT_TABLE          = GDDTCUT
    input%GDDS1_TABLE            = GDDS1
    input%GDDS2_TABLE            = GDDS2
    input%GDDS3_TABLE            = GDDS3
    input%GDDS4_TABLE            = GDDS4
    input%GDDS5_TABLE            = GDDS5
    input%C3PSNI_TABLE(1:5)      = C3PSNI(1:5)
    input%KC25I_TABLE(1:5)       = KC25I(1:5)
    input%AKCI_TABLE(1:5)        = AKCI(1:5)
    input%KO25I_TABLE(1:5)       = KO25I(1:5)
    input%AKOI_TABLE(1:5)        = AKOI(1:5)
    input%AVCMXI_TABLE(1:5)      = AVCMXI(1:5)
    input%VCMX25I_TABLE(1:5)     = VCMX25I(1:5)
    input%BPI_TABLE(1:5)         = BPI(1:5)
    input%MPI_TABLE(1:5)         = MPI(1:5)
    input%FOLNMXI_TABLE(1:5)     = FOLNMXI(1:5)
    input%QE25I_TABLE(1:5)       = QE25I(1:5)
    input%C3C4_TABLE             = C3C4
    input%AREF_TABLE             = AREF
    input%PSNRF_TABLE            = PSNRF
    input%I2PAR_TABLE            = I2PAR
    input%TASSIM0_TABLE          = TASSIM0
    input%TASSIM1_TABLE          = TASSIM1
    input%TASSIM2_TABLE          = TASSIM2
    input%K_TABLE                = K
    input%EPSI_TABLE             = EPSI
    input%Q10MR_TABLE            = Q10MR
    input%FOLN_MX_TABLE          = FOLN_MX
    input%LEFREEZ_TABLE          = LEFREEZ
    input%DILE_FC_TABLE(:,1)     = DILE_FC_S1
    input%DILE_FC_TABLE(:,2)     = DILE_FC_S2
    input%DILE_FC_TABLE(:,3)     = DILE_FC_S3
    input%DILE_FC_TABLE(:,4)     = DILE_FC_S4
    input%DILE_FC_TABLE(:,5)     = DILE_FC_S5
    input%DILE_FC_TABLE(:,6)     = DILE_FC_S6
    input%DILE_FC_TABLE(:,7)     = DILE_FC_S7
    input%DILE_FC_TABLE(:,8)     = DILE_FC_S8
    input%DILE_FW_TABLE(:,1)     = DILE_FW_S1
    input%DILE_FW_TABLE(:,2)     = DILE_FW_S2
    input%DILE_FW_TABLE(:,3)     = DILE_FW_S3
    input%DILE_FW_TABLE(:,4)     = DILE_FW_S4
    input%DILE_FW_TABLE(:,5)     = DILE_FW_S5
    input%DILE_FW_TABLE(:,6)     = DILE_FW_S6
    input%DILE_FW_TABLE(:,7)     = DILE_FW_S7
    input%DILE_FW_TABLE(:,8)     = DILE_FW_S8
    input%FRA_GR_TABLE           = FRA_GR
    input%LF_OVRC_TABLE(:,1)     = LF_OVRC_S1
    input%LF_OVRC_TABLE(:,2)     = LF_OVRC_S2
    input%LF_OVRC_TABLE(:,3)     = LF_OVRC_S3
    input%LF_OVRC_TABLE(:,4)     = LF_OVRC_S4
    input%LF_OVRC_TABLE(:,5)     = LF_OVRC_S5
    input%LF_OVRC_TABLE(:,6)     = LF_OVRC_S6
    input%LF_OVRC_TABLE(:,7)     = LF_OVRC_S7
    input%LF_OVRC_TABLE(:,8)     = LF_OVRC_S8
    input%ST_OVRC_TABLE(:,1)     = ST_OVRC_S1
    input%ST_OVRC_TABLE(:,2)     = ST_OVRC_S2
    input%ST_OVRC_TABLE(:,3)     = ST_OVRC_S3
    input%ST_OVRC_TABLE(:,4)     = ST_OVRC_S4
    input%ST_OVRC_TABLE(:,5)     = ST_OVRC_S5
    input%ST_OVRC_TABLE(:,6)     = ST_OVRC_S6
    input%ST_OVRC_TABLE(:,7)     = ST_OVRC_S7
    input%ST_OVRC_TABLE(:,8)     = ST_OVRC_S8
    input%RT_OVRC_TABLE(:,1)     = RT_OVRC_S1
    input%RT_OVRC_TABLE(:,2)     = RT_OVRC_S2
    input%RT_OVRC_TABLE(:,3)     = RT_OVRC_S3
    input%RT_OVRC_TABLE(:,4)     = RT_OVRC_S4
    input%RT_OVRC_TABLE(:,5)     = RT_OVRC_S5
    input%RT_OVRC_TABLE(:,6)     = RT_OVRC_S6
    input%RT_OVRC_TABLE(:,7)     = RT_OVRC_S7
    input%RT_OVRC_TABLE(:,8)     = RT_OVRC_S8
    input%LFMR25_TABLE           = LFMR25
    input%STMR25_TABLE           = STMR25
    input%RTMR25_TABLE           = RTMR25
    input%GRAINMR25_TABLE        = GRAINMR25
    input%LFPT_TABLE(:,1)        = LFPT_S1
    input%LFPT_TABLE(:,2)        = LFPT_S2
    input%LFPT_TABLE(:,3)        = LFPT_S3
    input%LFPT_TABLE(:,4)        = LFPT_S4
    input%LFPT_TABLE(:,5)        = LFPT_S5
    input%LFPT_TABLE(:,6)        = LFPT_S6
    input%LFPT_TABLE(:,7)        = LFPT_S7
    input%LFPT_TABLE(:,8)        = LFPT_S8
    input%STPT_TABLE(:,1)        = STPT_S1
    input%STPT_TABLE(:,2)        = STPT_S2
    input%STPT_TABLE(:,3)        = STPT_S3
    input%STPT_TABLE(:,4)        = STPT_S4
    input%STPT_TABLE(:,5)        = STPT_S5
    input%STPT_TABLE(:,6)        = STPT_S6
    input%STPT_TABLE(:,7)        = STPT_S7
    input%STPT_TABLE(:,8)        = STPT_S8
    input%RTPT_TABLE(:,1)        = RTPT_S1
    input%RTPT_TABLE(:,2)        = RTPT_S2
    input%RTPT_TABLE(:,3)        = RTPT_S3
    input%RTPT_TABLE(:,4)        = RTPT_S4
    input%RTPT_TABLE(:,5)        = RTPT_S5
    input%RTPT_TABLE(:,6)        = RTPT_S6
    input%RTPT_TABLE(:,7)        = RTPT_S7
    input%RTPT_TABLE(:,8)        = RTPT_S8
    input%GRAINPT_TABLE(:,1)     = GRAINPT_S1
    input%GRAINPT_TABLE(:,2)     = GRAINPT_S2
    input%GRAINPT_TABLE(:,3)     = GRAINPT_S3
    input%GRAINPT_TABLE(:,4)     = GRAINPT_S4
    input%GRAINPT_TABLE(:,5)     = GRAINPT_S5
    input%GRAINPT_TABLE(:,6)     = GRAINPT_S6
    input%GRAINPT_TABLE(:,7)     = GRAINPT_S7
    input%GRAINPT_TABLE(:,8)     = GRAINPT_S8
    input%LFCT_TABLE(:,1)        = LFCT_S1
    input%LFCT_TABLE(:,2)        = LFCT_S2
    input%LFCT_TABLE(:,3)        = LFCT_S3
    input%LFCT_TABLE(:,4)        = LFCT_S4
    input%LFCT_TABLE(:,5)        = LFCT_S5
    input%LFCT_TABLE(:,6)        = LFCT_S6
    input%LFCT_TABLE(:,7)        = LFCT_S7
    input%LFCT_TABLE(:,8)        = LFCT_S8
    input%STCT_TABLE(:,1)        = STCT_S1
    input%STCT_TABLE(:,2)        = STCT_S2
    input%STCT_TABLE(:,3)        = STCT_S3
    input%STCT_TABLE(:,4)        = STCT_S4
    input%STCT_TABLE(:,5)        = STCT_S5
    input%STCT_TABLE(:,6)        = STCT_S6
    input%STCT_TABLE(:,7)        = STCT_S7
    input%STCT_TABLE(:,8)        = STCT_S8
    input%RTCT_TABLE(:,1)        = RTCT_S1
    input%RTCT_TABLE(:,2)        = RTCT_S2
    input%RTCT_TABLE(:,3)        = RTCT_S3
    input%RTCT_TABLE(:,4)        = RTCT_S4
    input%RTCT_TABLE(:,5)        = RTCT_S5
    input%RTCT_TABLE(:,6)        = RTCT_S6
    input%RTCT_TABLE(:,7)        = RTCT_S7
    input%RTCT_TABLE(:,8)        = RTCT_S8
    input%BIO2LAI_TABLE          = BIO2LAI

    !---------------- MPTABLE.TBL tile drainage parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    read(15,noahmp_tiledrain_parameters)
    close(15)
    ! assign values
    input%TDSMC_FAC_TABLE(1:NSOILTYPE)    = TDSMC_FAC(1:NSOILTYPE)
    input%TD_DEPTH_TABLE(1:NSOILTYPE)    = TD_DEPTH(1:NSOILTYPE)
    input%DRAIN_LAYER_OPT_TABLE          = DRAIN_LAYER_OPT
    input%TD_DC_TABLE(1:NSOILTYPE)       = TD_DC(1:NSOILTYPE)
    input%TD_DCOEF_TABLE(1:NSOILTYPE)    = TD_DCOEF(1:NSOILTYPE)
    input%TD_D_TABLE(1:NSOILTYPE)        = TD_D(1:NSOILTYPE)
    input%TD_ADEPTH_TABLE(1:NSOILTYPE)   = TD_ADEPTH(1:NSOILTYPE)
    input%TD_RADI_TABLE(1:NSOILTYPE)     = TD_RADI(1:NSOILTYPE)
    input%TD_SPAC_TABLE(1:NSOILTYPE)     = TD_SPAC(1:NSOILTYPE)
    input%TD_DDRAIN_TABLE(1:NSOILTYPE)   = TD_DDRAIN(1:NSOILTYPE)
    input%KLAT_FAC_TABLE(1:NSOILTYPE)    = KLAT_FAC(1:NSOILTYPE)

    !---------------- MPTABLE.TBL optional parameters
    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
      open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif
    read(15,noahmp_optional_parameters)
    close(15)
    ! assign values
    input%sr2006_theta_1500t_a_TABLE = sr2006_theta_1500t_a
    input%sr2006_theta_1500t_b_TABLE = sr2006_theta_1500t_b
    input%sr2006_theta_1500t_c_TABLE = sr2006_theta_1500t_c
    input%sr2006_theta_1500t_d_TABLE = sr2006_theta_1500t_d
    input%sr2006_theta_1500t_e_TABLE = sr2006_theta_1500t_e
    input%sr2006_theta_1500t_f_TABLE = sr2006_theta_1500t_f
    input%sr2006_theta_1500t_g_TABLE = sr2006_theta_1500t_g
    input%sr2006_theta_1500_a_TABLE  = sr2006_theta_1500_a
    input%sr2006_theta_1500_b_TABLE  = sr2006_theta_1500_b
    input%sr2006_theta_33t_a_TABLE   = sr2006_theta_33t_a
    input%sr2006_theta_33t_b_TABLE   = sr2006_theta_33t_b
    input%sr2006_theta_33t_c_TABLE   = sr2006_theta_33t_c
    input%sr2006_theta_33t_d_TABLE   = sr2006_theta_33t_d
    input%sr2006_theta_33t_e_TABLE   = sr2006_theta_33t_e
    input%sr2006_theta_33t_f_TABLE   = sr2006_theta_33t_f
    input%sr2006_theta_33t_g_TABLE   = sr2006_theta_33t_g
    input%sr2006_theta_33_a_TABLE    = sr2006_theta_33_a
    input%sr2006_theta_33_b_TABLE    = sr2006_theta_33_b
    input%sr2006_theta_33_c_TABLE    = sr2006_theta_33_c
    input%sr2006_theta_s33t_a_TABLE  = sr2006_theta_s33t_a
    input%sr2006_theta_s33t_b_TABLE  = sr2006_theta_s33t_b
    input%sr2006_theta_s33t_c_TABLE  = sr2006_theta_s33t_c
    input%sr2006_theta_s33t_d_TABLE  = sr2006_theta_s33t_d
    input%sr2006_theta_s33t_e_TABLE  = sr2006_theta_s33t_e
    input%sr2006_theta_s33t_f_TABLE  = sr2006_theta_s33t_f
    input%sr2006_theta_s33t_g_TABLE  = sr2006_theta_s33t_g
    input%sr2006_theta_s33_a_TABLE   = sr2006_theta_s33_a
    input%sr2006_theta_s33_b_TABLE   = sr2006_theta_s33_b
    input%sr2006_psi_et_a_TABLE      = sr2006_psi_et_a
    input%sr2006_psi_et_b_TABLE      = sr2006_psi_et_b
    input%sr2006_psi_et_c_TABLE      = sr2006_psi_et_c
    input%sr2006_psi_et_d_TABLE      = sr2006_psi_et_d
    input%sr2006_psi_et_e_TABLE      = sr2006_psi_et_e
    input%sr2006_psi_et_f_TABLE      = sr2006_psi_et_f
    input%sr2006_psi_et_g_TABLE      = sr2006_psi_et_g
    input%sr2006_psi_e_a_TABLE       = sr2006_psi_e_a
    input%sr2006_psi_e_b_TABLE       = sr2006_psi_e_b
    input%sr2006_psi_e_c_TABLE       = sr2006_psi_e_c
    input%sr2006_smcmax_a_TABLE      = sr2006_smcmax_a
    input%sr2006_smcmax_b_TABLE      = sr2006_smcmax_b


  end subroutine ReadNoahmpTable

!=== read namelist values
  subroutine ReadNamelist(input)

    implicit none

    type(input_type), intent(inout)  :: input

    !=== declare local variable to store namelist values
    ! timing
    real(kind=kind_noahmp) :: dt
    integer                :: maxtime
    character(len=256)     :: output_filename
    logical                :: runsnow
    real(kind=kind_noahmp) :: JULIAN
    logical                :: runglacier
    ! forcing
    real(kind=kind_noahmp) :: rainrate
    integer                :: rain_duration
    integer                :: dry_duration
    logical                :: raining
    real(kind=kind_noahmp) :: uwind
    real(kind=kind_noahmp) :: vwind
    real(kind=kind_noahmp) :: sfcpres
    real(kind=kind_noahmp) :: Q2
    real(kind=kind_noahmp) :: SWDOWN
    real(kind=kind_noahmp) :: LWDOWN
    ! structure
    integer                :: isltyp
    integer                :: vegtype
    integer                :: soilcolor
    integer                :: slopetype
    integer                :: croptype
    integer                :: nsoil
    integer                :: nsnow
    integer                :: structure_option
    real(kind=kind_noahmp) :: soil_depth
    real(kind=kind_noahmp) :: vegfra
    real(kind=kind_noahmp) :: vegmax
    real(kind=kind_noahmp) :: shdmax
    real(kind=kind_noahmp) :: zlvl
    ! fixed_initial
    real(kind=kind_noahmp), allocatable, dimension(:) :: zsoil   ! depth of layer-bottom from soil surface
    ! uniform_initial
    logical                :: initial_uniform                 ! initial all levels the same
    real(kind=kind_noahmp) :: initial_sh2o_value              ! constant sh2o value
    ! options
    integer                :: idveg,iopt_crs,iopt_btr,iopt_runsrf,iopt_runsub,iopt_sfc,iopt_frz,&
                              iopt_inf,iopt_rad,iopt_alb,iopt_snf,iopt_tbot,iopt_stc,iopt_rsf,iopt_soil,&
                              iopt_pedo,iopt_crop,iopt_irr,iopt_irrm,iopt_infdv,iopt_tdrn,iopt_tksno,iopt_gla

    !=== arrange structures for reading namelist.input
    namelist / timing          / dt,maxtime,output_filename,runsnow,JULIAN,runglacier
    namelist / forcing         / rainrate,rain_duration,dry_duration,&
                                 raining,uwind,vwind,sfcpres,Q2,SWDOWN,LWDOWN
    namelist / structure       / isltyp,vegtype,soilcolor,slopetype,croptype,nsoil,&
                                 nsnow,structure_option,soil_depth,vegfra,vegmax,shdmax,zlvl
    namelist / fixed_initial   / zsoil
    namelist / uniform_initial / initial_uniform,initial_sh2o_value
    namelist / options         / idveg,iopt_crs,iopt_btr,iopt_runsrf,iopt_runsub,iopt_sfc,iopt_frz,&
                                 iopt_inf,iopt_rad,iopt_alb,iopt_snf,iopt_tbot,iopt_stc,iopt_rsf,iopt_soil,&
                                 iopt_pedo,iopt_crop,iopt_irr,iopt_irrm,iopt_infdv,iopt_tdrn,iopt_tksno,iopt_gla

    !---------------------------------------------------------------
    ! read namelist.input
    !---------------------------------------------------------------
    open(30, file="namelist.input", form="formatted")
       read(30, timing)
       read(30, forcing)
       read(30, structure)
       read(30, uniform_initial)
       read(30, options)
    close(30)

    allocate (zsoil (       1:nsoil))

    if ( structure_option == 1 ) then       ! user-defined levels
       open(30, file="namelist.input", form="formatted")
       read(30, fixed_initial)
       close(30)
    endif

    !---------------------------------------------------------------
    ! transfer table values to input variables
    !---------------------------------------------------------------
    input%OPT_DVEGIn       = idveg
    input%OPT_CRSIn        = iopt_crs
    input%OPT_BTRIn        = iopt_btr
    input%OPT_RUNSRFIn     = iopt_runsrf
    input%OPT_RUNSUBIn     = iopt_runsub
    input%OPT_SFCIn        = iopt_sfc
    input%OPT_FRZIn        = iopt_frz
    input%OPT_INFIn        = iopt_inf
    input%OPT_RADIn        = iopt_rad 
    input%OPT_ALBIn        = iopt_alb
    input%OPT_SNFIn        = iopt_snf
    input%OPT_TBOTIn       = iopt_tbot
    input%OPT_STCIn        = iopt_stc
    input%OPT_RSFIn        = iopt_rsf
    input%OPT_SOILIn       = iopt_soil
    input%OPT_PEDOIn       = iopt_pedo
    input%OPT_CROPIn       = iopt_crop
    input%OPT_IRRIn        = iopt_irr
    input%OPT_IRRMIn       = iopt_irrm
    input%OPT_INFDVIn      = iopt_infdv
    input%OPT_TDRNIn       = iopt_tdrn
    input%OPT_TKSNOIn      = iopt_tksno
    input%OPT_GLAIn        = iopt_gla
    input%output_filename  = output_filename
    input%DTIn             = dt
    input%maxtime          = maxtime
    input%runsnow          = runsnow
    input%runglacier       = runglacier
    input%rainrate         = rainrate
    input%rain_duration    = rain_duration
    input%dry_duration     = dry_duration
    input%raining          = raining
    input%UUIn             = uwind
    input%VVIn             = vwind
    input%SFCPRSIn         = sfcpres
    input%VEGTYPEIn        = vegtype
    input%SOILCOLORIn      = soilcolor
    input%SLOPETYPEIn      = slopetype
    input%CROPTYPEIn       = croptype
    input%NSOILIn          = nsoil
    input%NSNOWIn          = nsnow
    input%VEGFRAIn         = vegfra
    input%VEGMAXIn         = vegmax
    input%SHDMAXIn         = shdmax / 100.0
    input%SHDFACIn         = vegfra / 100.0
    input%JULIANIn         = JULIAN
    input%Q2In             = Q2
    input%SOLDNIn          = SWDOWN
    input%LWDNIn           = LWDOWN
    input%PSFCIn           = sfcpres
    input%ZLVLIn           = zlvl

    allocate( input%SOILTYPEIn(       1:nsoil))
    allocate( input%ZSOILIn   (       1:nsoil))
    allocate( input%SH2OIn    (       1:nsoil))
    allocate( input%ZSNSOIn   (-nsnow+1:nsoil))
    input%ZSOILIn(1:nsoil)         = zsoil(1:nsoil)
    input%ZSNSOIn(-nsnow+1:0)      = 0.0
    input%ZSNSOIn(1:nsoil)         = zsoil(1:nsoil)
    input%SOILTYPEIn(1:nsoil)      = isltyp    
    if ( initial_uniform .eqv. .true. ) then
       input%SH2OIn(1:nsoil) = initial_sh2o_value
    endif


  end subroutine ReadNamelist

!=== initialize with default values
  subroutine InputVarInitDefault(input)

    implicit none

    type(input_type), intent(inout) :: input

    input%ILOCIn           = 1
    input%JLOCIn           = 1
    input%DXIn             = 4000.0
    input%NSOILIn          = 4
    input%NSNOWIn          = 3
    input%NBANDIn          = 2
    input%SFCTMPIn         = huge(1.0)
    input%PRCPCONVIn       = huge(1.0)
    input%PRCPNONCIn       = huge(1.0)
    input%PRCPSHCVIn       = huge(1.0)
    input%PRCPSNOWIn       = huge(1.0)
    input%PRCPGRPLIn       = huge(1.0)
    input%PRCPHAILIn       = huge(1.0)
    input%TBOTIn           = huge(1.0)
    input%COSZIn           = 0.5
    input%YEARLENIn        = 365
    input%LATIn            = 40.0 * 3.1415 / 180.0
    input%PGSIn            = 0
    input%FOLNIn           = 1.0
    input%LLANDUSEIn       = "MODIFIED_IGBP_MODIS_NOAH"
    input%IRRFRAIn         = 0.0
    input%SIFRAIn          = 0.0
    input%MIFRAIn          = 0.0
    input%FIFRAIn          = 0.0
    input%DZ8WIn           = 20.0
    input%ICEIn            = 0
    input%ISTIn            = 1
    input%URBAN_FLAGIn     = .false.
    input%NSTAGEIn         = 8

  end subroutine InputVarInitDefault



!=== read input forcing data
!  subroutine ReadInputForcing(input)
!
!    use netcdf
!    use error_handling, only : handle_err
!
!    implicit none
!
!    type(input_type), intent(inout)  :: input
!
!    integer :: ncid, dimid, varid, status
!
!  !=== read in forcing netcdf files
!    status = nf90_open(input%forcfilename, NF90_NOWRITE, ncid)
!    if (status /= nf90_noerr) call handle_err(status)
!
!    status = nf90_inq_varid(ncid, "time", varid)
!    status = nf90_get_var(ncid, varid , now_time)
!    if (status /= nf90_noerr) call handle_err(status)
!
!    status = nf90_inq_varid(ncid, "west_east", varid)
!    status = nf90_get_var(ncid, varid , input%nx)
!    if (status /= nf90_noerr) call handle_err(status)
!
!    status = nf90_inq_varid(ncid, "south_north", varid)
!    status = nf90_get_var(ncid, varid , input%ny)
!    if (status /= nf90_noerr) call handle_err(status)
!
!    allocate( input%U2D(input%nx,input%ny) )
!    status = nf90_inq_varid(ncid, "U2D", varid)
!    status = nf90_get_var(ncid, varid , input%U2D, start=(/1,1/), count=(/input%nx,input%ny/))
!    if (status /= nf90_noerr) call handle_err(status)
!
!    status = nf90_close(ncid)
!    if (status /= nf90_noerr) call handle_err(status)
!
!  end subroutine ReadInputForcing

!=== read input setup data (static & initial)
!  subroutine ReadInputSetup(input)
!
!    use netcdf
!    use error_handling, only : handle_err
!
!    implicit none
!
!    type(input_type), intent(inout)  :: input
!
!    integer :: ncid, dimid, varid, status
!
!  !=== read in setup netcdf files (including static & initial data)
!    status = nf90_open(input%setupfilename, NF90_NOWRITE, ncid)
!    if (status /= nf90_noerr) call handle_err(status)
!    
!    status = nf90_inq_varid(ncid, "time", varid)
!    status = nf90_get_var(ncid, varid , now_time)
!    if (status /= nf90_noerr) call handle_err(status)
!
!    status = nf90_inq_varid(ncid, "west_east", varid)
!    status = nf90_get_var(ncid, varid , input%nx)
!    if (status /= nf90_noerr) call handle_err(status)
!
!    status = nf90_inq_varid(ncid, "south_north", varid)
!    status = nf90_get_var(ncid, varid , input%ny)
!    if (status /= nf90_noerr) call handle_err(status)
!
!    allocate( input%SHDMAX(input%nx,input%ny) )
!    status = nf90_inq_varid(ncid, "SHDMAX", varid)
!    status = nf90_get_var(ncid, varid , input%SHDMAX, start=(/1,1/), count=(/input%nx,input%ny/))
!    if (status /= nf90_noerr) call handle_err(status)
!
!    status = nf90_close(ncid)
!    if (status /= nf90_noerr) call handle_err(status)
!
!  end subroutine ReadInputSetup

!=== read input restart data
!  subroutine ReadInputRestart(input)
!
!    use netcdf
!    use error_handling, only : handle_err
!
!    implicit none
!
!    type(input_type), intent(inout)  :: input
!
!    integer :: ncid, dimid, varid, status
!
!  !=== read in restart netcdf files
!    status = nf90_open(input%restartfilename, NF90_NOWRITE, ncid)
!    if (status /= nf90_noerr) call handle_err(status)
!
!    status = nf90_inq_varid(ncid, "time", varid)
!    status = nf90_get_var(ncid, varid , now_time)
!    if (status /= nf90_noerr) call handle_err(status)
!
!    status = nf90_inq_varid(ncid, "west_east", varid)
!    status = nf90_get_var(ncid, varid , input%nx)
!    if (status /= nf90_noerr) call handle_err(status)
!
!    status = nf90_inq_varid(ncid, "south_north", varid)
!    status = nf90_get_var(ncid, varid , input%ny)
!    if (status /= nf90_noerr) call handle_err(status)
!
!    allocate( input%SWE(input%nx,input%ny) )
!    status = nf90_inq_varid(ncid, "SWE", varid)
!    status = nf90_get_var(ncid, varid , input%SWE, start=(/1,1/), count=(/input%nx,input%ny/))
!    if (status /= nf90_noerr) call handle_err(status)
!
!    status = nf90_close(ncid)
!    if (status /= nf90_noerr) call handle_err(status)
!
!  end subroutine ReadInputRestart


end module InputVarInitMod
