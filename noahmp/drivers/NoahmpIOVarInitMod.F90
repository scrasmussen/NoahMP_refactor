module NoahmpIOVarInitMod

!!! Initialize Noah-MP input/output variables (2D forcing, namelist, table, static)
!!! Input/Output variables should be first defined in NoahmpIOType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (July 2022)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType

  implicit none

contains

!=== read Noahmp Table values
  subroutine ReadNoahmpTable(NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout)  :: NoahmpIO

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
    
    ! vegetation parameters
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
                                              NROOT, RGL, RS, HS, TOPT, RSMAX, RTOVRC, RSWOODC, BF, WSTRC, LAIMIN,           &
                                              XSAMIN, EPS1, EPS2, EPS3, EPS4, EPS5
    namelist / noahmp_usgs_veg_categories /   VEG_DATASET_DESCRIPTION, NVEG
    namelist / noahmp_usgs_parameters     /   ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL,                 &
                                              LCZ_1, LCZ_2, LCZ_3, LCZ_4, LCZ_5, LCZ_6, LCZ_7, LCZ_8, LCZ_9, LCZ_10, LCZ_11, &
                                              CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, &
                                              AKC, KO25, AKO, AVCMX, AQE, LTOVRC, DILEFC, DILEFW, RMF25, SLA, FRAGR, TMIN,   &
                                              VCMX25, TDLEF, BP, MP, QE25, RMS25, RMR25, ARM, FOLNMX, WDPOOL, WRRAT, MRP,    &
                                              NROOT, RGL, RS, HS, TOPT, RSMAX, RTOVRC, RSWOODC, BF, WSTRC, LAIMIN,           &
                                              XSAMIN, SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY,                           &
                                              SAI_JUN, SAI_JUL, SAI_AUG, SAI_SEP, SAI_OCT, SAI_NOV, SAI_DEC, LAI_JAN,        &
                                              LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN, LAI_JUL, LAI_AUG, LAI_SEP,        &
                                              LAI_OCT, LAI_NOV, LAI_DEC, RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS,   &
                                              TAUL_NIR, TAUS_VIS, TAUS_NIR, EPS1, EPS2, EPS3, EPS4, EPS5
    namelist / noahmp_modis_veg_categories /  VEG_DATASET_DESCRIPTION, NVEG
    namelist / noahmp_modis_parameters     /  ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL,                 &
                                              LCZ_1, LCZ_2, LCZ_3, LCZ_4, LCZ_5, LCZ_6, LCZ_7, LCZ_8, LCZ_9, LCZ_10, LCZ_11, &
                                              CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, &
                                              AKC, KO25, AKO, AVCMX, AQE, LTOVRC, DILEFC, DILEFW, RMF25, SLA, FRAGR, TMIN,   &
                                              VCMX25, TDLEF, BP, MP, QE25, RMS25, RMR25, ARM, FOLNMX, WDPOOL, WRRAT, MRP,    &
                                              NROOT, RGL, RS, HS, TOPT, RSMAX, RTOVRC, RSWOODC, BF, WSTRC, LAIMIN,           &
                                              XSAMIN, SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY,                           &
                                              SAI_JUN, SAI_JUL, SAI_AUG, SAI_SEP, SAI_OCT, SAI_NOV, SAI_DEC, LAI_JAN,        &
                                              LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN, LAI_JUL, LAI_AUG, LAI_SEP,        &
                                              LAI_OCT, LAI_NOV, LAI_DEC, RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS,   &
                                              TAUL_NIR, TAUS_VIS, TAUS_NIR, EPS1, EPS2, EPS3, EPS4, EPS5

    ! soil parameters
    character(len=256)                             :: message
    character(len=10)                              :: SLTYPE
    integer                                        :: SLCATS
    real(kind=kind_noahmp), dimension(MAX_SOILTYP) :: BB, DRYSMC, MAXSMC, REFSMC, SATPSI, SATDK, SATDW, WLTSMC, QTZ,    &
                                                      BVIC, AXAJ, BXAJ, XXAJ, BDVIC, BBVIC, GDVIC, HC
    namelist / noahmp_stas_soil_categories /          SLTYPE, SLCATS
    namelist / noahmp_soil_stas_parameters /          BB, DRYSMC, MAXSMC, REFSMC, SATPSI, SATDK, SATDW, WLTSMC, QTZ,    &
                                                      BVIC, AXAJ, BXAJ, XXAJ, BDVIC, BBVIC, GDVIC
    namelist / noahmp_soil_stas_ruc_parameters /      BB, DRYSMC, HC, MAXSMC, REFSMC, SATPSI, SATDK, SATDW, WLTSMC, QTZ,    &
                                                      BVIC, AXAJ, BXAJ, XXAJ, BDVIC, BBVIC, GDVIC

    ! general parameters
    real(kind=kind_noahmp)                       :: CSOIL_DATA, REFDK_DATA, REFKDT_DATA, FRZK_DATA, ZBOT_DATA, CZIL_DATA
    real(kind=kind_noahmp), dimension(NUM_SLOPE) :: SLOPE_DATA
    namelist / noahmp_general_parameters /          SLOPE_DATA, CSOIL_DATA, REFDK_DATA, REFKDT_DATA, FRZK_DATA, ZBOT_DATA,   &
                                                    CZIL_DATA

    ! radiation parameters
    real(kind=kind_noahmp)                   :: BETADS, BETAIS, EICE
    real(kind=kind_noahmp), dimension(MBAND) :: ALBICE, ALBLAK, OMEGAS 
    real(kind=kind_noahmp), dimension(2)     :: EG
    real(kind=kind_noahmp), dimension(MSC)   :: ALBSAT_VIS, ALBSAT_NIR, ALBDRY_VIS, ALBDRY_NIR
    namelist / noahmp_rad_parameters /          ALBSAT_VIS, ALBSAT_NIR, ALBDRY_VIS, ALBDRY_NIR, ALBICE, ALBLAK, OMEGAS,      &
                                                BETADS, BETAIS, EG, EICE

    ! global parameters
    real(kind=kind_noahmp)                   :: CO2, O2, TIMEAN, FSATMX, Z0SNO, SSI, SNOW_RET_FAC ,SNOW_EMIS, SWEMX, TAU0,   &
                                                GRAIN_GROWTH, EXTRA_GROWTH, DIRT_SOOT, BATS_COSZ, BATS_VIS_NEW,              &
                                                BATS_NIR_NEW, BATS_VIS_AGE, BATS_NIR_AGE, BATS_VIS_DIR, BATS_NIR_DIR,        &
                                                RSURF_SNOW, RSURF_EXP, C2_SNOWCOMPACT, C3_SNOWCOMPACT, C4_SNOWCOMPACT,       &
                                                C5_SNOWCOMPACT, DM_SNOWCOMPACT, ETA0_SNOWCOMPACT, SNLIQMAXFRAC, SWEMAXGLA,   &
                                                WSLMAX, ROUS, CMIC, SNOWDEN_MAX, CLASS_ALB_REF, CLASS_SNO_AGE, CLASS_ALB_NEW,&
                                                PSIWLT, Z0SOIL, Z0LAKE
    namelist / noahmp_global_parameters /       CO2, O2, TIMEAN, FSATMX, Z0SNO, SSI, SNOW_RET_FAC ,SNOW_EMIS, SWEMX, TAU0,   &
                                                GRAIN_GROWTH, EXTRA_GROWTH, DIRT_SOOT, BATS_COSZ, BATS_VIS_NEW,              &
                                                BATS_NIR_NEW, BATS_VIS_AGE, BATS_NIR_AGE, BATS_VIS_DIR, BATS_NIR_DIR,        &
                                                RSURF_SNOW, RSURF_EXP, C2_SNOWCOMPACT, C3_SNOWCOMPACT, C4_SNOWCOMPACT,       &
                                                C5_SNOWCOMPACT, DM_SNOWCOMPACT, ETA0_SNOWCOMPACT, SNLIQMAXFRAC, SWEMAXGLA,   &
                                                WSLMAX, ROUS, CMIC, SNOWDEN_MAX, CLASS_ALB_REF, CLASS_SNO_AGE, CLASS_ALB_NEW,&
                                                PSIWLT, Z0SOIL, Z0LAKE

    ! irrigation parameters
    integer                                  :: IRR_HAR
    real(kind=kind_noahmp)                   :: IRR_FRAC, IRR_LAI, IRR_MAD, FILOSS, SPRIR_RATE, MICIR_RATE, FIRTFAC, IR_RAIN
    namelist / noahmp_irrigation_parameters /   IRR_FRAC, IRR_HAR, IRR_LAI, IRR_MAD, FILOSS, SPRIR_RATE, MICIR_RATE, FIRTFAC,&
                                                IR_RAIN

    ! crop parameters
    integer                                  :: DEFAULT_CROP
    integer               , dimension(NCROP) :: PLTDAY, HSDAY
    real(kind=kind_noahmp), dimension(NCROP) :: PLANTPOP, IRRI, GDDTBASE, GDDTCUT, GDDS1, GDDS2, GDDS3, GDDS4, GDDS5, C3PSNI,&
                                                KC25I, AKCI, KO25I, AKOI, AVCMXI, VCMX25I, BPI, MPI, FOLNMXI, QE25I, AREF,   &
                                                PSNRF, I2PAR, TASSIM0, TASSIM1, TASSIM2, K, EPSI, Q10MR, LEFREEZ,            &
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
                                                MPI, FOLNMXI, QE25I, AREF, PSNRF, I2PAR, TASSIM0, TASSIM1, TASSIM2, K,       &
                                                EPSI,Q10MR, LEFREEZ, DILE_FC_S1, DILE_FC_S2, DILE_FC_S3, DILE_FC_S4,         &
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

    ! tile drainage parameters
    integer                                        :: NSOILTYPE, DRAIN_LAYER_OPT
    integer               , dimension(MAX_SOILTYP) :: TD_DEPTH
    real(kind=kind_noahmp), dimension(MAX_SOILTYP) :: TDSMC_FAC, TD_DC, TD_DCOEF, TD_D, TD_ADEPTH, TD_RADI, TD_SPAC,         &
                                                      TD_DDRAIN, KLAT_FAC
    namelist / noahmp_tiledrain_parameters /          NSOILTYPE, DRAIN_LAYER_OPT, TDSMC_FAC, TD_DEPTH, TD_DC, TD_DCOEF, TD_D,&
                                                      TD_ADEPTH, TD_RADI, TD_SPAC, TD_DDRAIN, KLAT_FAC

    ! optional parameters
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

    ! vegetation parameters
    allocate( NoahmpIO%CH2OP_TABLE (MVT) )
    allocate( NoahmpIO%DLEAF_TABLE (MVT) )
    allocate( NoahmpIO%Z0MVT_TABLE (MVT) )
    allocate( NoahmpIO%HVT_TABLE   (MVT) )
    allocate( NoahmpIO%HVB_TABLE   (MVT) )
    allocate( NoahmpIO%DEN_TABLE   (MVT) )
    allocate( NoahmpIO%RC_TABLE    (MVT) )
    allocate( NoahmpIO%MFSNO_TABLE (MVT) )
    allocate( NoahmpIO%SCFFAC_TABLE(MVT) )
    allocate( NoahmpIO%SAIM_TABLE  (MVT,12) )
    allocate( NoahmpIO%LAIM_TABLE  (MVT,12) )
    allocate( NoahmpIO%SLA_TABLE   (MVT) )
    allocate( NoahmpIO%DILEFC_TABLE(MVT) )
    allocate( NoahmpIO%DILEFW_TABLE(MVT) )
    allocate( NoahmpIO%FRAGR_TABLE (MVT) )
    allocate( NoahmpIO%LTOVRC_TABLE(MVT) )
    allocate( NoahmpIO%C3PSN_TABLE (MVT) )
    allocate( NoahmpIO%KC25_TABLE  (MVT) )
    allocate( NoahmpIO%AKC_TABLE   (MVT) )
    allocate( NoahmpIO%KO25_TABLE  (MVT) )
    allocate( NoahmpIO%AKO_TABLE   (MVT) )
    allocate( NoahmpIO%VCMX25_TABLE(MVT) )
    allocate( NoahmpIO%AVCMX_TABLE (MVT) )
    allocate( NoahmpIO%BP_TABLE    (MVT) )
    allocate( NoahmpIO%MP_TABLE    (MVT) )
    allocate( NoahmpIO%QE25_TABLE  (MVT) )
    allocate( NoahmpIO%AQE_TABLE   (MVT) )
    allocate( NoahmpIO%RMF25_TABLE (MVT) )
    allocate( NoahmpIO%RMS25_TABLE (MVT) )
    allocate( NoahmpIO%RMR25_TABLE (MVT) )
    allocate( NoahmpIO%ARM_TABLE   (MVT) )
    allocate( NoahmpIO%FOLNMX_TABLE(MVT) )
    allocate( NoahmpIO%TMIN_TABLE  (MVT) )
    allocate( NoahmpIO%XL_TABLE    (MVT) )
    allocate( NoahmpIO%RHOL_TABLE  (MVT,MBAND) )
    allocate( NoahmpIO%RHOS_TABLE  (MVT,MBAND) )
    allocate( NoahmpIO%TAUL_TABLE  (MVT,MBAND) )
    allocate( NoahmpIO%TAUS_TABLE  (MVT,MBAND) )
    allocate( NoahmpIO%MRP_TABLE   (MVT) )
    allocate( NoahmpIO%CWPVT_TABLE (MVT) )
    allocate( NoahmpIO%WRRAT_TABLE (MVT) )
    allocate( NoahmpIO%WDPOOL_TABLE(MVT) )
    allocate( NoahmpIO%TDLEF_TABLE (MVT) )
    allocate( NoahmpIO%NROOT_TABLE (MVT) )
    allocate( NoahmpIO%RGL_TABLE   (MVT) )
    allocate( NoahmpIO%RS_TABLE    (MVT) )
    allocate( NoahmpIO%HS_TABLE    (MVT) )
    allocate( NoahmpIO%TOPT_TABLE  (MVT) )
    allocate( NoahmpIO%RSMAX_TABLE (MVT) )
    allocate( NoahmpIO%RTOVRC_TABLE(MVT) )
    allocate( NoahmpIO%RSWOODC_TABLE(MVT) )
    allocate( NoahmpIO%BF_TABLE    (MVT) )
    allocate( NoahmpIO%WSTRC_TABLE (MVT) )
    allocate( NoahmpIO%LAIMIN_TABLE(MVT) )
    allocate( NoahmpIO%XSAMIN_TABLE(MVT) )

    ! soil parameters
    allocate( NoahmpIO%BEXP_TABLE  (MAX_SOILTYP) )
    allocate( NoahmpIO%SMCDRY_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%SMCMAX_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%SMCREF_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%PSISAT_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%DKSAT_TABLE (MAX_SOILTYP) )
    allocate( NoahmpIO%DWSAT_TABLE (MAX_SOILTYP) )
    allocate( NoahmpIO%SMCWLT_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%QUARTZ_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%BVIC_TABLE  (MAX_SOILTYP) )
    allocate( NoahmpIO%AXAJ_TABLE  (MAX_SOILTYP) )
    allocate( NoahmpIO%BXAJ_TABLE  (MAX_SOILTYP) )
    allocate( NoahmpIO%XXAJ_TABLE  (MAX_SOILTYP) )
    allocate( NoahmpIO%BDVIC_TABLE (MAX_SOILTYP) )
    allocate( NoahmpIO%GDVIC_TABLE (MAX_SOILTYP) )
    allocate( NoahmpIO%BBVIC_TABLE (MAX_SOILTYP) )

    ! general parameters
    allocate( NoahmpIO%SLOPE_TABLE(NUM_SLOPE) )

    ! radiation parameters
    allocate( NoahmpIO%ALBSAT_TABLE(MSC,MBAND) )
    allocate( NoahmpIO%ALBDRY_TABLE(MSC,MBAND) )
    allocate( NoahmpIO%ALBICE_TABLE(MBAND) )
    allocate( NoahmpIO%ALBLAK_TABLE(MBAND) )
    allocate( NoahmpIO%OMEGAS_TABLE(MBAND) )
    allocate( NoahmpIO%EG_TABLE(2) )

    ! tile drainage parameters
    allocate( NoahmpIO%TDSMC_FAC_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%TD_DC_TABLE    (MAX_SOILTYP) )
    allocate( NoahmpIO%TD_DEPTH_TABLE (MAX_SOILTYP) )
    allocate( NoahmpIO%TD_DCOEF_TABLE (MAX_SOILTYP) )
    allocate( NoahmpIO%TD_D_TABLE     (MAX_SOILTYP) )
    allocate( NoahmpIO%TD_ADEPTH_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%TD_RADI_TABLE  (MAX_SOILTYP) )
    allocate( NoahmpIO%TD_SPAC_TABLE  (MAX_SOILTYP) )
    allocate( NoahmpIO%TD_DDRAIN_TABLE(MAX_SOILTYP) )
    allocate( NoahmpIO%KLAT_FAC_TABLE (MAX_SOILTYP) )

    ! crop parameters
    allocate( NoahmpIO%PLTDAY_TABLE   (NCROP) )
    allocate( NoahmpIO%HSDAY_TABLE    (NCROP) )
    allocate( NoahmpIO%PLANTPOP_TABLE (NCROP) )
    allocate( NoahmpIO%IRRI_TABLE     (NCROP) )
    allocate( NoahmpIO%GDDTBASE_TABLE (NCROP) )
    allocate( NoahmpIO%GDDTCUT_TABLE  (NCROP) )
    allocate( NoahmpIO%GDDS1_TABLE    (NCROP) )
    allocate( NoahmpIO%GDDS2_TABLE    (NCROP) )
    allocate( NoahmpIO%GDDS3_TABLE    (NCROP) )
    allocate( NoahmpIO%GDDS4_TABLE    (NCROP) )
    allocate( NoahmpIO%GDDS5_TABLE    (NCROP) )
    allocate( NoahmpIO%C3PSNI_TABLE   (NCROP) )
    allocate( NoahmpIO%KC25I_TABLE    (NCROP) )
    allocate( NoahmpIO%AKCI_TABLE     (NCROP) )
    allocate( NoahmpIO%KO25I_TABLE    (NCROP) )
    allocate( NoahmpIO%AKOI_TABLE     (NCROP) )
    allocate( NoahmpIO%VCMX25I_TABLE  (NCROP) )
    allocate( NoahmpIO%AVCMXI_TABLE   (NCROP) )
    allocate( NoahmpIO%BPI_TABLE      (NCROP) )
    allocate( NoahmpIO%MPI_TABLE      (NCROP) )
    allocate( NoahmpIO%QE25I_TABLE    (NCROP) )
    allocate( NoahmpIO%FOLNMXI_TABLE  (NCROP) )
    allocate( NoahmpIO%AREF_TABLE     (NCROP) )
    allocate( NoahmpIO%PSNRF_TABLE    (NCROP) )
    allocate( NoahmpIO%I2PAR_TABLE    (NCROP) )
    allocate( NoahmpIO%TASSIM0_TABLE  (NCROP) )
    allocate( NoahmpIO%TASSIM1_TABLE  (NCROP) )
    allocate( NoahmpIO%TASSIM2_TABLE  (NCROP) )
    allocate( NoahmpIO%K_TABLE        (NCROP) )
    allocate( NoahmpIO%EPSI_TABLE     (NCROP) )
    allocate( NoahmpIO%Q10MR_TABLE    (NCROP) )
    allocate( NoahmpIO%LEFREEZ_TABLE  (NCROP) )
    allocate( NoahmpIO%DILE_FC_TABLE  (NCROP,NSTAGE) )
    allocate( NoahmpIO%DILE_FW_TABLE  (NCROP,NSTAGE) )
    allocate( NoahmpIO%FRA_GR_TABLE   (NCROP) )
    allocate( NoahmpIO%LF_OVRC_TABLE  (NCROP,NSTAGE) )
    allocate( NoahmpIO%ST_OVRC_TABLE  (NCROP,NSTAGE) )
    allocate( NoahmpIO%RT_OVRC_TABLE  (NCROP,NSTAGE) )
    allocate( NoahmpIO%LFMR25_TABLE   (NCROP) )
    allocate( NoahmpIO%STMR25_TABLE   (NCROP) )
    allocate( NoahmpIO%RTMR25_TABLE   (NCROP) )
    allocate( NoahmpIO%GRAINMR25_TABLE(NCROP) )
    allocate( NoahmpIO%LFPT_TABLE     (NCROP,NSTAGE) )
    allocate( NoahmpIO%STPT_TABLE     (NCROP,NSTAGE) )
    allocate( NoahmpIO%RTPT_TABLE     (NCROP,NSTAGE) )
    allocate( NoahmpIO%GRAINPT_TABLE  (NCROP,NSTAGE) )
    allocate( NoahmpIO%LFCT_TABLE     (NCROP,NSTAGE) )
    allocate( NoahmpIO%STCT_TABLE     (NCROP,NSTAGE) )
    allocate( NoahmpIO%RTCT_TABLE     (NCROP,NSTAGE) )
    allocate( NoahmpIO%BIO2LAI_TABLE  (NCROP) )

    !---------------------------------------------------------------
    ! intialization to bad value, so that if the namelist read fails,
    ! we come to a screeching halt as soon as we try to use anything
    !---------------------------------------------------------------

    ! vegetation parameters
    NoahmpIO%ISURBAN_TABLE      = -99999
    NoahmpIO%ISWATER_TABLE      = -99999
    NoahmpIO%ISBARREN_TABLE     = -99999
    NoahmpIO%ISICE_TABLE        = -99999
    NoahmpIO%ISCROP_TABLE       = -99999
    NoahmpIO%EBLFOREST_TABLE    = -99999
    NoahmpIO%NATURAL_TABLE      = -99999
    NoahmpIO%LCZ_1_TABLE        = -99999
    NoahmpIO%LCZ_2_TABLE        = -99999
    NoahmpIO%LCZ_3_TABLE        = -99999
    NoahmpIO%LCZ_4_TABLE        = -99999
    NoahmpIO%LCZ_5_TABLE        = -99999
    NoahmpIO%LCZ_6_TABLE        = -99999
    NoahmpIO%LCZ_7_TABLE        = -99999
    NoahmpIO%LCZ_8_TABLE        = -99999
    NoahmpIO%LCZ_9_TABLE        = -99999
    NoahmpIO%LCZ_10_TABLE       = -99999
    NoahmpIO%LCZ_11_TABLE       = -99999
    NoahmpIO%CH2OP_TABLE        = -1.0e36
    NoahmpIO%DLEAF_TABLE        = -1.0e36
    NoahmpIO%Z0MVT_TABLE        = -1.0e36
    NoahmpIO%HVT_TABLE          = -1.0e36
    NoahmpIO%HVB_TABLE          = -1.0e36
    NoahmpIO%DEN_TABLE          = -1.0e36
    NoahmpIO%RC_TABLE           = -1.0e36
    NoahmpIO%MFSNO_TABLE        = -1.0e36
    NoahmpIO%SCFFAC_TABLE       = -1.0e36
    NoahmpIO%RHOL_TABLE         = -1.0e36
    NoahmpIO%RHOS_TABLE         = -1.0e36
    NoahmpIO%TAUL_TABLE         = -1.0e36
    NoahmpIO%TAUS_TABLE         = -1.0e36
    NoahmpIO%XL_TABLE           = -1.0e36
    NoahmpIO%CWPVT_TABLE        = -1.0e36
    NoahmpIO%C3PSN_TABLE        = -1.0e36
    NoahmpIO%KC25_TABLE         = -1.0e36
    NoahmpIO%AKC_TABLE          = -1.0e36
    NoahmpIO%KO25_TABLE         = -1.0e36
    NoahmpIO%AKO_TABLE          = -1.0e36
    NoahmpIO%AVCMX_TABLE        = -1.0e36
    NoahmpIO%AQE_TABLE          = -1.0e36
    NoahmpIO%LTOVRC_TABLE       = -1.0e36
    NoahmpIO%DILEFC_TABLE       = -1.0e36
    NoahmpIO%DILEFW_TABLE       = -1.0e36
    NoahmpIO%RMF25_TABLE        = -1.0e36
    NoahmpIO%SLA_TABLE          = -1.0e36
    NoahmpIO%FRAGR_TABLE        = -1.0e36
    NoahmpIO%TMIN_TABLE         = -1.0e36
    NoahmpIO%VCMX25_TABLE       = -1.0e36
    NoahmpIO%TDLEF_TABLE        = -1.0e36
    NoahmpIO%BP_TABLE           = -1.0e36
    NoahmpIO%MP_TABLE           = -1.0e36
    NoahmpIO%QE25_TABLE         = -1.0e36
    NoahmpIO%RMS25_TABLE        = -1.0e36
    NoahmpIO%RMR25_TABLE        = -1.0e36
    NoahmpIO%ARM_TABLE          = -1.0e36
    NoahmpIO%FOLNMX_TABLE       = -1.0e36
    NoahmpIO%WDPOOL_TABLE       = -1.0e36
    NoahmpIO%WRRAT_TABLE        = -1.0e36
    NoahmpIO%MRP_TABLE          = -1.0e36
    NoahmpIO%SAIM_TABLE         = -1.0e36
    NoahmpIO%LAIM_TABLE         = -1.0e36
    NoahmpIO%NROOT_TABLE        = -1.0e36
    NoahmpIO%RGL_TABLE          = -1.0e36
    NoahmpIO%RS_TABLE           = -1.0e36
    NoahmpIO%HS_TABLE           = -1.0e36
    NoahmpIO%TOPT_TABLE         = -1.0e36
    NoahmpIO%RSMAX_TABLE        = -1.0e36
    NoahmpIO%RTOVRC_TABLE       = -1.0e36
    NoahmpIO%RSWOODC_TABLE      = -1.0e36
    NoahmpIO%BF_TABLE           = -1.0e36
    NoahmpIO%WSTRC_TABLE        = -1.0e36
    NoahmpIO%LAIMIN_TABLE       = -1.0e36
    NoahmpIO%XSAMIN_TABLE       = -1.0e36

    ! soil parameters
    NoahmpIO%SLCATS_TABLE       = -99999
    NoahmpIO%BEXP_TABLE         = -1.0e36
    NoahmpIO%SMCDRY_TABLE       = -1.0e36
    NoahmpIO%SMCMAX_TABLE       = -1.0e36
    NoahmpIO%SMCREF_TABLE       = -1.0e36
    NoahmpIO%PSISAT_TABLE       = -1.0e36
    NoahmpIO%DKSAT_TABLE        = -1.0e36
    NoahmpIO%DWSAT_TABLE        = -1.0e36
    NoahmpIO%SMCWLT_TABLE       = -1.0e36
    NoahmpIO%QUARTZ_TABLE       = -1.0e36
    NoahmpIO%BVIC_TABLE         = -1.0e36
    NoahmpIO%AXAJ_TABLE         = -1.0e36
    NoahmpIO%BXAJ_TABLE         = -1.0e36
    NoahmpIO%XXAJ_TABLE         = -1.0e36
    NoahmpIO%BDVIC_TABLE        = -1.0e36
    NoahmpIO%GDVIC_TABLE        = -1.0e36
    NoahmpIO%BBVIC_TABLE        = -1.0e36

    ! general parameters
    NoahmpIO%SLOPE_TABLE        = -1.0e36
    NoahmpIO%CSOIL_TABLE        = -1.0e36
    NoahmpIO%REFDK_TABLE        = -1.0e36
    NoahmpIO%REFKDT_TABLE       = -1.0e36
    NoahmpIO%FRZK_TABLE         = -1.0e36
    NoahmpIO%ZBOT_TABLE         = -1.0e36
    NoahmpIO%CZIL_TABLE         = -1.0e36

    ! radiation parameters
    NoahmpIO%ALBSAT_TABLE       = -1.0e36
    NoahmpIO%ALBDRY_TABLE       = -1.0e36
    NoahmpIO%ALBICE_TABLE       = -1.0e36
    NoahmpIO%ALBLAK_TABLE       = -1.0e36
    NoahmpIO%OMEGAS_TABLE       = -1.0e36
    NoahmpIO%BETADS_TABLE       = -1.0e36
    NoahmpIO%BETAIS_TABLE       = -1.0e36
    NoahmpIO%EG_TABLE           = -1.0e36
    NoahmpIO%EICE_TABLE         = -1.0e36

    ! global parameters
    NoahmpIO%CO2_TABLE              = -1.0e36
    NoahmpIO%O2_TABLE               = -1.0e36
    NoahmpIO%TIMEAN_TABLE           = -1.0e36
    NoahmpIO%FSATMX_TABLE           = -1.0e36
    NoahmpIO%Z0SNO_TABLE            = -1.0e36
    NoahmpIO%SSI_TABLE              = -1.0e36
    NoahmpIO%SNOW_RET_FAC_TABLE     = -1.0e36
    NoahmpIO%SNOW_EMIS_TABLE        = -1.0e36
    NoahmpIO%SWEMX_TABLE            = -1.0e36
    NoahmpIO%TAU0_TABLE             = -1.0e36
    NoahmpIO%GRAIN_GROWTH_TABLE     = -1.0e36
    NoahmpIO%EXTRA_GROWTH_TABLE     = -1.0e36
    NoahmpIO%DIRT_SOOT_TABLE        = -1.0e36
    NoahmpIO%BATS_COSZ_TABLE        = -1.0e36
    NoahmpIO%BATS_VIS_NEW_TABLE     = -1.0e36
    NoahmpIO%BATS_NIR_NEW_TABLE     = -1.0e36
    NoahmpIO%BATS_VIS_AGE_TABLE     = -1.0e36
    NoahmpIO%BATS_NIR_AGE_TABLE     = -1.0e36
    NoahmpIO%BATS_VIS_DIR_TABLE     = -1.0e36
    NoahmpIO%BATS_NIR_DIR_TABLE     = -1.0e36
    NoahmpIO%RSURF_SNOW_TABLE       = -1.0e36
    NoahmpIO%RSURF_EXP_TABLE        = -1.0e36
    NoahmpIO%C2_SNOWCOMPACT_TABLE   = -1.0e36
    NoahmpIO%C3_SNOWCOMPACT_TABLE   = -1.0e36
    NoahmpIO%C4_SNOWCOMPACT_TABLE   = -1.0e36
    NoahmpIO%C5_SNOWCOMPACT_TABLE   = -1.0e36
    NoahmpIO%DM_SNOWCOMPACT_TABLE   = -1.0e36
    NoahmpIO%ETA0_SNOWCOMPACT_TABLE = -1.0e36
    NoahmpIO%SNLIQMAXFRAC_TABLE     = -1.0e36
    NoahmpIO%SWEMAXGLA_TABLE        = -1.0e36
    NoahmpIO%WSLMAX_TABLE           = -1.0e36
    NoahmpIO%ROUS_TABLE             = -1.0e36
    NoahmpIO%CMIC_TABLE             = -1.0e36
    NoahmpIO%SNOWDEN_MAX_TABLE      = -1.0e36
    NoahmpIO%CLASS_ALB_REF_TABLE    = -1.0e36
    NoahmpIO%CLASS_SNO_AGE_TABLE    = -1.0e36
    NoahmpIO%CLASS_ALB_NEW_TABLE    = -1.0e36
    NoahmpIO%PSIWLT_TABLE           = -1.0e36
    NoahmpIO%Z0SOIL_TABLE           = -1.0e36
    NoahmpIO%Z0LAKE_TABLE           = -1.0e36

    ! irrigation parameters
    NoahmpIO%IRR_HAR_TABLE          = -99999
    NoahmpIO%IRR_FRAC_TABLE         = -1.0e36
    NoahmpIO%IRR_LAI_TABLE          = -1.0e36
    NoahmpIO%IRR_MAD_TABLE          = -1.0e36
    NoahmpIO%FILOSS_TABLE           = -1.0e36
    NoahmpIO%SPRIR_RATE_TABLE       = -1.0e36
    NoahmpIO%MICIR_RATE_TABLE       = -1.0e36
    NoahmpIO%FIRTFAC_TABLE          = -1.0e36
    NoahmpIO%IR_RAIN_TABLE          = -1.0e36

    ! crop parameters
    NoahmpIO%DEFAULT_CROP_TABLE     = -99999
    NoahmpIO%PLTDAY_TABLE           = -99999
    NoahmpIO%HSDAY_TABLE            = -99999
    NoahmpIO%PLANTPOP_TABLE         = -1.0e36
    NoahmpIO%IRRI_TABLE             = -1.0e36
    NoahmpIO%GDDTBASE_TABLE         = -1.0e36
    NoahmpIO%GDDTCUT_TABLE          = -1.0e36
    NoahmpIO%GDDS1_TABLE            = -1.0e36
    NoahmpIO%GDDS2_TABLE            = -1.0e36
    NoahmpIO%GDDS3_TABLE            = -1.0e36
    NoahmpIO%GDDS4_TABLE            = -1.0e36
    NoahmpIO%GDDS5_TABLE            = -1.0e36
    NoahmpIO%C3PSNI_TABLE           = -1.0e36
    NoahmpIO%KC25I_TABLE            = -1.0e36
    NoahmpIO%AKCI_TABLE             = -1.0e36
    NoahmpIO%KO25I_TABLE            = -1.0e36
    NoahmpIO%AKOI_TABLE             = -1.0e36
    NoahmpIO%AVCMXI_TABLE           = -1.0e36
    NoahmpIO%VCMX25I_TABLE          = -1.0e36
    NoahmpIO%BPI_TABLE              = -1.0e36
    NoahmpIO%MPI_TABLE              = -1.0e36
    NoahmpIO%FOLNMXI_TABLE          = -1.0e36
    NoahmpIO%QE25I_TABLE            = -1.0e36
    NoahmpIO%AREF_TABLE             = -1.0e36
    NoahmpIO%PSNRF_TABLE            = -1.0e36
    NoahmpIO%I2PAR_TABLE            = -1.0e36
    NoahmpIO%TASSIM0_TABLE          = -1.0e36
    NoahmpIO%TASSIM1_TABLE          = -1.0e36
    NoahmpIO%TASSIM2_TABLE          = -1.0e36
    NoahmpIO%K_TABLE                = -1.0e36
    NoahmpIO%EPSI_TABLE             = -1.0e36
    NoahmpIO%Q10MR_TABLE            = -1.0e36
    NoahmpIO%LEFREEZ_TABLE          = -1.0e36
    NoahmpIO%DILE_FC_TABLE          = -1.0e36
    NoahmpIO%DILE_FW_TABLE          = -1.0e36
    NoahmpIO%FRA_GR_TABLE           = -1.0e36
    NoahmpIO%LF_OVRC_TABLE          = -1.0e36
    NoahmpIO%ST_OVRC_TABLE          = -1.0e36
    NoahmpIO%RT_OVRC_TABLE          = -1.0e36
    NoahmpIO%LFMR25_TABLE           = -1.0e36
    NoahmpIO%STMR25_TABLE           = -1.0e36
    NoahmpIO%RTMR25_TABLE           = -1.0e36
    NoahmpIO%GRAINMR25_TABLE        = -1.0e36
    NoahmpIO%LFPT_TABLE             = -1.0e36
    NoahmpIO%STPT_TABLE             = -1.0e36
    NoahmpIO%RTPT_TABLE             = -1.0e36
    NoahmpIO%GRAINPT_TABLE          = -1.0e36
    NoahmpIO%LFCT_TABLE             = -1.0e36
    NoahmpIO%STCT_TABLE             = -1.0e36
    NoahmpIO%RTCT_TABLE             = -1.0e36
    NoahmpIO%BIO2LAI_TABLE          = -1.0e36

    ! tile drainage parameters
    NoahmpIO%DRAIN_LAYER_OPT_TABLE  = -99999
    NoahmpIO%TD_DEPTH_TABLE         = -99999
    NoahmpIO%TDSMC_FAC_TABLE        = -1.0e36
    NoahmpIO%TD_DC_TABLE            = -1.0e36
    NoahmpIO%TD_DCOEF_TABLE         = -1.0e36
    NoahmpIO%TD_D_TABLE             = -1.0e36
    NoahmpIO%TD_ADEPTH_TABLE        = -1.0e36
    NoahmpIO%TD_RADI_TABLE          = -1.0e36
    NoahmpIO%TD_SPAC_TABLE          = -1.0e36
    NoahmpIO%TD_DDRAIN_TABLE        = -1.0e36
    NoahmpIO%KLAT_FAC_TABLE         = -1.0e36

    ! optional parameters
    NoahmpIO%sr2006_theta_1500t_a_TABLE = -1.0e36
    NoahmpIO%sr2006_theta_1500t_b_TABLE = -1.0e36
    NoahmpIO%sr2006_theta_1500t_c_TABLE = -1.0e36
    NoahmpIO%sr2006_theta_1500t_d_TABLE = -1.0e36
    NoahmpIO%sr2006_theta_1500t_e_TABLE = -1.0e36
    NoahmpIO%sr2006_theta_1500t_f_TABLE = -1.0e36
    NoahmpIO%sr2006_theta_1500t_g_TABLE = -1.0e36
    NoahmpIO%sr2006_theta_1500_a_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_1500_b_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_33t_a_TABLE   = -1.0e36
    NoahmpIO%sr2006_theta_33t_b_TABLE   = -1.0e36
    NoahmpIO%sr2006_theta_33t_c_TABLE   = -1.0e36
    NoahmpIO%sr2006_theta_33t_d_TABLE   = -1.0e36
    NoahmpIO%sr2006_theta_33t_e_TABLE   = -1.0e36
    NoahmpIO%sr2006_theta_33t_f_TABLE   = -1.0e36
    NoahmpIO%sr2006_theta_33t_g_TABLE   = -1.0e36
    NoahmpIO%sr2006_theta_33_a_TABLE    = -1.0e36
    NoahmpIO%sr2006_theta_33_b_TABLE    = -1.0e36
    NoahmpIO%sr2006_theta_33_c_TABLE    = -1.0e36
    NoahmpIO%sr2006_theta_s33t_a_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_s33t_b_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_s33t_c_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_s33t_d_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_s33t_e_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_s33t_f_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_s33t_g_TABLE  = -1.0e36
    NoahmpIO%sr2006_theta_s33_a_TABLE   = -1.0e36
    NoahmpIO%sr2006_theta_s33_b_TABLE   = -1.0e36
    NoahmpIO%sr2006_psi_et_a_TABLE      = -1.0e36
    NoahmpIO%sr2006_psi_et_b_TABLE      = -1.0e36
    NoahmpIO%sr2006_psi_et_c_TABLE      = -1.0e36
    NoahmpIO%sr2006_psi_et_d_TABLE      = -1.0e36
    NoahmpIO%sr2006_psi_et_e_TABLE      = -1.0e36
    NoahmpIO%sr2006_psi_et_f_TABLE      = -1.0e36
    NoahmpIO%sr2006_psi_et_g_TABLE      = -1.0e36
    NoahmpIO%sr2006_psi_e_a_TABLE       = -1.0e36
    NoahmpIO%sr2006_psi_e_b_TABLE       = -1.0e36
    NoahmpIO%sr2006_psi_e_c_TABLE       = -1.0e36
    NoahmpIO%sr2006_smcmax_a_TABLE      = -1.0e36
    NoahmpIO%sr2006_smcmax_b_TABLE      = -1.0e36

    !---------------------------------------------------------------
    ! transfer values from table to input variables
    !---------------------------------------------------------------

    !---------------- NoahmpTable.TBL vegetation parameters

    DATASET_IDENTIFIER = NoahmpIO%LLANDUSE

    inquire( file='NoahmpTable.TBL', exist=file_named )
    if ( file_named ) then
       open(15, file="NoahmpTable.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
       open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if ( ierr /= 0 ) then
       write(*,'("WARNING: Cannot find file NoahmpTable.TBL")')
    endif

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
    NoahmpIO%ISURBAN_TABLE         = ISURBAN
    NoahmpIO%ISWATER_TABLE         = ISWATER
    NoahmpIO%ISBARREN_TABLE        = ISBARREN
    NoahmpIO%ISICE_TABLE           = ISICE
    NoahmpIO%ISCROP_TABLE          = ISCROP
    NoahmpIO%EBLFOREST_TABLE       = EBLFOREST
    NoahmpIO%NATURAL_TABLE         = NATURAL
    NoahmpIO%LCZ_1_TABLE           = LCZ_1
    NoahmpIO%LCZ_2_TABLE           = LCZ_2
    NoahmpIO%LCZ_3_TABLE           = LCZ_3
    NoahmpIO%LCZ_4_TABLE           = LCZ_4
    NoahmpIO%LCZ_5_TABLE           = LCZ_5
    NoahmpIO%LCZ_6_TABLE           = LCZ_6
    NoahmpIO%LCZ_7_TABLE           = LCZ_7
    NoahmpIO%LCZ_8_TABLE           = LCZ_8
    NoahmpIO%LCZ_9_TABLE           = LCZ_9
    NoahmpIO%LCZ_10_TABLE          = LCZ_10
    NoahmpIO%LCZ_11_TABLE          = LCZ_11
    NoahmpIO%CH2OP_TABLE  (1:NVEG) = CH2OP  (1:NVEG)
    NoahmpIO%DLEAF_TABLE  (1:NVEG) = DLEAF  (1:NVEG)
    NoahmpIO%Z0MVT_TABLE  (1:NVEG) = Z0MVT  (1:NVEG)
    NoahmpIO%HVT_TABLE    (1:NVEG) = HVT    (1:NVEG)
    NoahmpIO%HVB_TABLE    (1:NVEG) = HVB    (1:NVEG)
    NoahmpIO%DEN_TABLE    (1:NVEG) = DEN    (1:NVEG)
    NoahmpIO%RC_TABLE     (1:NVEG) = RC     (1:NVEG)
    NoahmpIO%MFSNO_TABLE  (1:NVEG) = MFSNO  (1:NVEG)
    NoahmpIO%SCFFAC_TABLE (1:NVEG) = SCFFAC (1:NVEG)
    NoahmpIO%XL_TABLE     (1:NVEG) = XL     (1:NVEG)
    NoahmpIO%CWPVT_TABLE  (1:NVEG) = CWPVT  (1:NVEG)
    NoahmpIO%C3PSN_TABLE  (1:NVEG) = C3PSN  (1:NVEG)
    NoahmpIO%KC25_TABLE   (1:NVEG) = KC25   (1:NVEG)
    NoahmpIO%AKC_TABLE    (1:NVEG) = AKC    (1:NVEG)
    NoahmpIO%KO25_TABLE   (1:NVEG) = KO25   (1:NVEG)
    NoahmpIO%AKO_TABLE    (1:NVEG) = AKO    (1:NVEG)
    NoahmpIO%AVCMX_TABLE  (1:NVEG) = AVCMX  (1:NVEG)
    NoahmpIO%AQE_TABLE    (1:NVEG) = AQE    (1:NVEG)
    NoahmpIO%LTOVRC_TABLE (1:NVEG) = LTOVRC (1:NVEG)
    NoahmpIO%DILEFC_TABLE (1:NVEG) = DILEFC (1:NVEG)
    NoahmpIO%DILEFW_TABLE (1:NVEG) = DILEFW (1:NVEG)
    NoahmpIO%RMF25_TABLE  (1:NVEG) = RMF25  (1:NVEG)
    NoahmpIO%SLA_TABLE    (1:NVEG) = SLA    (1:NVEG)
    NoahmpIO%FRAGR_TABLE  (1:NVEG) = FRAGR  (1:NVEG)
    NoahmpIO%TMIN_TABLE   (1:NVEG) = TMIN   (1:NVEG)
    NoahmpIO%VCMX25_TABLE (1:NVEG) = VCMX25 (1:NVEG)
    NoahmpIO%TDLEF_TABLE  (1:NVEG) = TDLEF  (1:NVEG)
    NoahmpIO%BP_TABLE     (1:NVEG) = BP     (1:NVEG)
    NoahmpIO%MP_TABLE     (1:NVEG) = MP     (1:NVEG)
    NoahmpIO%QE25_TABLE   (1:NVEG) = QE25   (1:NVEG)
    NoahmpIO%RMS25_TABLE  (1:NVEG) = RMS25  (1:NVEG)
    NoahmpIO%RMR25_TABLE  (1:NVEG) = RMR25  (1:NVEG)
    NoahmpIO%ARM_TABLE    (1:NVEG) = ARM    (1:NVEG)
    NoahmpIO%FOLNMX_TABLE (1:NVEG) = FOLNMX (1:NVEG)
    NoahmpIO%WDPOOL_TABLE (1:NVEG) = WDPOOL (1:NVEG)
    NoahmpIO%WRRAT_TABLE  (1:NVEG) = WRRAT  (1:NVEG)
    NoahmpIO%MRP_TABLE    (1:NVEG) = MRP    (1:NVEG)
    NoahmpIO%NROOT_TABLE  (1:NVEG) = NROOT  (1:NVEG)
    NoahmpIO%RGL_TABLE    (1:NVEG) = RGL    (1:NVEG)
    NoahmpIO%RS_TABLE     (1:NVEG) = RS     (1:NVEG)
    NoahmpIO%HS_TABLE     (1:NVEG) = HS     (1:NVEG)
    NoahmpIO%TOPT_TABLE   (1:NVEG) = TOPT   (1:NVEG)
    NoahmpIO%RSMAX_TABLE  (1:NVEG) = RSMAX  (1:NVEG)
    NoahmpIO%RTOVRC_TABLE (1:NVEG) = RTOVRC (1:NVEG)
    NoahmpIO%RSWOODC_TABLE(1:NVEG) = RSWOODC(1:NVEG)
    NoahmpIO%BF_TABLE     (1:NVEG) = BF     (1:NVEG)
    NoahmpIO%WSTRC_TABLE  (1:NVEG) = WSTRC  (1:NVEG)
    NoahmpIO%LAIMIN_TABLE (1:NVEG) = LAIMIN (1:NVEG)
    NoahmpIO%XSAMIN_TABLE (1:NVEG) = XSAMIN (1:NVEG)

    NoahmpIO%SAIM_TABLE(1:NVEG, 1) = SAI_JAN(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG, 2) = SAI_FEB(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG, 3) = SAI_MAR(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG, 4) = SAI_APR(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG, 5) = SAI_MAY(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG, 6) = SAI_JUN(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG, 7) = SAI_JUL(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG, 8) = SAI_AUG(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG, 9) = SAI_SEP(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG,10) = SAI_OCT(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG,11) = SAI_NOV(1:NVEG)
    NoahmpIO%SAIM_TABLE(1:NVEG,12) = SAI_DEC(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 1) = LAI_JAN(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 2) = LAI_FEB(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 3) = LAI_MAR(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 4) = LAI_APR(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 5) = LAI_MAY(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 6) = LAI_JUN(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 7) = LAI_JUL(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 8) = LAI_AUG(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG, 9) = LAI_SEP(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG,10) = LAI_OCT(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG,11) = LAI_NOV(1:NVEG)
    NoahmpIO%LAIM_TABLE(1:NVEG,12) = LAI_DEC(1:NVEG)
    NoahmpIO%RHOL_TABLE(1:NVEG,1)  = RHOL_VIS(1:NVEG) !leaf reflectance: 1=vis, 2=nir
    NoahmpIO%RHOL_TABLE(1:NVEG,2)  = RHOL_NIR(1:NVEG) !leaf reflectance: 1=vis, 2=nir
    NoahmpIO%RHOS_TABLE(1:NVEG,1)  = RHOS_VIS(1:NVEG) !stem reflectance: 1=vis, 2=nir
    NoahmpIO%RHOS_TABLE(1:NVEG,2)  = RHOS_NIR(1:NVEG) !stem reflectance: 1=vis, 2=nir
    NoahmpIO%TAUL_TABLE(1:NVEG,1)  = TAUL_VIS(1:NVEG) !leaf transmittance: 1=vis, 2=nir
    NoahmpIO%TAUL_TABLE(1:NVEG,2)  = TAUL_NIR(1:NVEG) !leaf transmittance: 1=vis, 2=nir
    NoahmpIO%TAUS_TABLE(1:NVEG,1)  = TAUS_VIS(1:NVEG) !stem transmittance: 1=vis, 2=nir
    NoahmpIO%TAUS_TABLE(1:NVEG,2)  = TAUS_NIR(1:NVEG) !stem transmittance: 1=vis, 2=nir

    !---------------- NoahmpTable.TBL soil parameters
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
    NoahmpIO%SLCATS_TABLE           = SLCATS
    NoahmpIO%BEXP_TABLE  (1:SLCATS) = BB    (1:SLCATS)
    NoahmpIO%SMCDRY_TABLE(1:SLCATS) = DRYSMC(1:SLCATS)
    NoahmpIO%SMCMAX_TABLE(1:SLCATS) = MAXSMC(1:SLCATS)
    NoahmpIO%SMCREF_TABLE(1:SLCATS) = REFSMC(1:SLCATS)
    NoahmpIO%PSISAT_TABLE(1:SLCATS) = SATPSI(1:SLCATS)
    NoahmpIO%DKSAT_TABLE (1:SLCATS) = SATDK (1:SLCATS)
    NoahmpIO%DWSAT_TABLE (1:SLCATS) = SATDW (1:SLCATS)
    NoahmpIO%SMCWLT_TABLE(1:SLCATS) = WLTSMC(1:SLCATS)
    NoahmpIO%QUARTZ_TABLE(1:SLCATS) = QTZ   (1:SLCATS)
    NoahmpIO%BVIC_TABLE  (1:SLCATS) = BVIC  (1:SLCATS)
    NoahmpIO%AXAJ_TABLE  (1:SLCATS) = AXAJ  (1:SLCATS)
    NoahmpIO%BXAJ_TABLE  (1:SLCATS) = BXAJ  (1:SLCATS)
    NoahmpIO%XXAJ_TABLE  (1:SLCATS) = XXAJ  (1:SLCATS)
    NoahmpIO%BDVIC_TABLE (1:SLCATS) = BDVIC (1:SLCATS)
    NoahmpIO%GDVIC_TABLE (1:SLCATS) = GDVIC (1:SLCATS)
    NoahmpIO%BBVIC_TABLE (1:SLCATS) = BBVIC (1:SLCATS)

    !---------------- NoahmpTable.TBL general parameters
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
    NoahmpIO%SLOPE_TABLE(1:NUM_SLOPE) = SLOPE_DATA(1:NUM_SLOPE)
    NoahmpIO%CSOIL_TABLE              = CSOIL_DATA
    NoahmpIO%REFDK_TABLE              = REFDK_DATA
    NoahmpIO%REFKDT_TABLE             = REFKDT_DATA
    NoahmpIO%FRZK_TABLE               = FRZK_DATA
    NoahmpIO%ZBOT_TABLE               = ZBOT_DATA
    NoahmpIO%CZIL_TABLE               = CZIL_DATA

    !---------------- NoahmpTable.TBL radiation parameters
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
    NoahmpIO%ALBSAT_TABLE(:,1) = ALBSAT_VIS ! saturated soil albedos: 1=vis, 2=nir
    NoahmpIO%ALBSAT_TABLE(:,2) = ALBSAT_NIR ! saturated soil albedos: 1=vis, 2=nir
    NoahmpIO%ALBDRY_TABLE(:,1) = ALBDRY_VIS ! dry soil albedos: 1=vis, 2=nir
    NoahmpIO%ALBDRY_TABLE(:,2) = ALBDRY_NIR ! dry soil albedos: 1=vis, 2=nir
    NoahmpIO%ALBICE_TABLE      = ALBICE
    NoahmpIO%ALBLAK_TABLE      = ALBLAK
    NoahmpIO%OMEGAS_TABLE      = OMEGAS
    NoahmpIO%BETADS_TABLE      = BETADS
    NoahmpIO%BETAIS_TABLE      = BETAIS
    NoahmpIO%EG_TABLE          = EG
    NoahmpIO%EICE_TABLE        = EICE

    !---------------- NoahmpTable.TBL global parameters
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
    NoahmpIO%CO2_TABLE              = CO2
    NoahmpIO%O2_TABLE               = O2
    NoahmpIO%TIMEAN_TABLE           = TIMEAN
    NoahmpIO%FSATMX_TABLE           = FSATMX
    NoahmpIO%Z0SNO_TABLE            = Z0SNO
    NoahmpIO%SSI_TABLE              = SSI
    NoahmpIO%SNOW_RET_FAC_TABLE     = SNOW_RET_FAC
    NoahmpIO%SNOW_EMIS_TABLE        = SNOW_EMIS
    NoahmpIO%SWEMX_TABLE            = SWEMX
    NoahmpIO%TAU0_TABLE             = TAU0
    NoahmpIO%GRAIN_GROWTH_TABLE     = GRAIN_GROWTH
    NoahmpIO%EXTRA_GROWTH_TABLE     = EXTRA_GROWTH
    NoahmpIO%DIRT_SOOT_TABLE        = DIRT_SOOT
    NoahmpIO%BATS_COSZ_TABLE        = BATS_COSZ
    NoahmpIO%BATS_VIS_NEW_TABLE     = BATS_VIS_NEW
    NoahmpIO%BATS_NIR_NEW_TABLE     = BATS_NIR_NEW
    NoahmpIO%BATS_VIS_AGE_TABLE     = BATS_VIS_AGE
    NoahmpIO%BATS_NIR_AGE_TABLE     = BATS_NIR_AGE
    NoahmpIO%BATS_VIS_DIR_TABLE     = BATS_VIS_DIR
    NoahmpIO%BATS_NIR_DIR_TABLE     = BATS_NIR_DIR
    NoahmpIO%RSURF_SNOW_TABLE       = RSURF_SNOW
    NoahmpIO%RSURF_EXP_TABLE        = RSURF_EXP
    NoahmpIO%C2_SNOWCOMPACT_TABLE   = C2_SNOWCOMPACT
    NoahmpIO%C3_SNOWCOMPACT_TABLE   = C3_SNOWCOMPACT
    NoahmpIO%C4_SNOWCOMPACT_TABLE   = C4_SNOWCOMPACT
    NoahmpIO%C5_SNOWCOMPACT_TABLE   = C5_SNOWCOMPACT
    NoahmpIO%DM_SNOWCOMPACT_TABLE   = DM_SNOWCOMPACT
    NoahmpIO%ETA0_SNOWCOMPACT_TABLE = ETA0_SNOWCOMPACT
    NoahmpIO%SNLIQMAXFRAC_TABLE     = SNLIQMAXFRAC
    NoahmpIO%SWEMAXGLA_TABLE        = SWEMAXGLA
    NoahmpIO%WSLMAX_TABLE           = WSLMAX
    NoahmpIO%ROUS_TABLE             = ROUS
    NoahmpIO%CMIC_TABLE             = CMIC
    NoahmpIO%SNOWDEN_MAX_TABLE      = SNOWDEN_MAX
    NoahmpIO%CLASS_ALB_REF_TABLE    = CLASS_ALB_REF
    NoahmpIO%CLASS_SNO_AGE_TABLE    = CLASS_SNO_AGE
    NoahmpIO%CLASS_ALB_NEW_TABLE    = CLASS_ALB_NEW
    NoahmpIO%PSIWLT_TABLE           = PSIWLT
    NoahmpIO%Z0SOIL_TABLE           = Z0SOIL
    NoahmpIO%Z0LAKE_TABLE           = Z0LAKE

    !---------------- NoahmpTable.TBL irrigation parameters
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
    if ( (FILOSS < 0.0) .or. (FILOSS > 0.99) ) then
       write(*,'("WARNING: FILOSS should be >=0.0 and <=0.99")')
       stop "STOP in NoahMP_irrigation_parameters"
    endif

    ! assign values
    NoahmpIO%IRR_FRAC_TABLE   = IRR_FRAC
    NoahmpIO%IRR_HAR_TABLE    = IRR_HAR
    NoahmpIO%IRR_LAI_TABLE    = IRR_LAI
    NoahmpIO%IRR_MAD_TABLE    = IRR_MAD
    NoahmpIO%FILOSS_TABLE     = FILOSS  
    NoahmpIO%SPRIR_RATE_TABLE = SPRIR_RATE
    NoahmpIO%MICIR_RATE_TABLE = MICIR_RATE
    NoahmpIO%FIRTFAC_TABLE    = FIRTFAC
    NoahmpIO%IR_RAIN_TABLE    = IR_RAIN 

    !---------------- NoahmpTable.TBL crop parameters
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
    NoahmpIO%DEFAULT_CROP_TABLE     = DEFAULT_CROP
    NoahmpIO%PLTDAY_TABLE           = PLTDAY
    NoahmpIO%HSDAY_TABLE            = HSDAY
    NoahmpIO%PLANTPOP_TABLE         = PLANTPOP
    NoahmpIO%IRRI_TABLE             = IRRI
    NoahmpIO%GDDTBASE_TABLE         = GDDTBASE
    NoahmpIO%GDDTCUT_TABLE          = GDDTCUT
    NoahmpIO%GDDS1_TABLE            = GDDS1
    NoahmpIO%GDDS2_TABLE            = GDDS2
    NoahmpIO%GDDS3_TABLE            = GDDS3
    NoahmpIO%GDDS4_TABLE            = GDDS4
    NoahmpIO%GDDS5_TABLE            = GDDS5
    NoahmpIO%C3PSNI_TABLE (1:5)     = C3PSNI (1:5)
    NoahmpIO%KC25I_TABLE  (1:5)     = KC25I  (1:5)
    NoahmpIO%AKCI_TABLE   (1:5)     = AKCI   (1:5)
    NoahmpIO%KO25I_TABLE  (1:5)     = KO25I  (1:5)
    NoahmpIO%AKOI_TABLE   (1:5)     = AKOI   (1:5)
    NoahmpIO%AVCMXI_TABLE (1:5)     = AVCMXI (1:5)
    NoahmpIO%VCMX25I_TABLE(1:5)     = VCMX25I(1:5)
    NoahmpIO%BPI_TABLE    (1:5)     = BPI    (1:5)
    NoahmpIO%MPI_TABLE    (1:5)     = MPI    (1:5)
    NoahmpIO%FOLNMXI_TABLE(1:5)     = FOLNMXI(1:5)
    NoahmpIO%QE25I_TABLE  (1:5)     = QE25I  (1:5)
    NoahmpIO%AREF_TABLE             = AREF
    NoahmpIO%PSNRF_TABLE            = PSNRF
    NoahmpIO%I2PAR_TABLE            = I2PAR
    NoahmpIO%TASSIM0_TABLE          = TASSIM0
    NoahmpIO%TASSIM1_TABLE          = TASSIM1
    NoahmpIO%TASSIM2_TABLE          = TASSIM2
    NoahmpIO%K_TABLE                = K
    NoahmpIO%EPSI_TABLE             = EPSI
    NoahmpIO%Q10MR_TABLE            = Q10MR
    NoahmpIO%LEFREEZ_TABLE          = LEFREEZ
    NoahmpIO%FRA_GR_TABLE           = FRA_GR
    NoahmpIO%LFMR25_TABLE           = LFMR25
    NoahmpIO%STMR25_TABLE           = STMR25
    NoahmpIO%RTMR25_TABLE           = RTMR25
    NoahmpIO%GRAINMR25_TABLE        = GRAINMR25
    NoahmpIO%BIO2LAI_TABLE          = BIO2LAI
    NoahmpIO%DILE_FC_TABLE(:,1)     = DILE_FC_S1
    NoahmpIO%DILE_FC_TABLE(:,2)     = DILE_FC_S2
    NoahmpIO%DILE_FC_TABLE(:,3)     = DILE_FC_S3
    NoahmpIO%DILE_FC_TABLE(:,4)     = DILE_FC_S4
    NoahmpIO%DILE_FC_TABLE(:,5)     = DILE_FC_S5
    NoahmpIO%DILE_FC_TABLE(:,6)     = DILE_FC_S6
    NoahmpIO%DILE_FC_TABLE(:,7)     = DILE_FC_S7
    NoahmpIO%DILE_FC_TABLE(:,8)     = DILE_FC_S8
    NoahmpIO%DILE_FW_TABLE(:,1)     = DILE_FW_S1
    NoahmpIO%DILE_FW_TABLE(:,2)     = DILE_FW_S2
    NoahmpIO%DILE_FW_TABLE(:,3)     = DILE_FW_S3
    NoahmpIO%DILE_FW_TABLE(:,4)     = DILE_FW_S4
    NoahmpIO%DILE_FW_TABLE(:,5)     = DILE_FW_S5
    NoahmpIO%DILE_FW_TABLE(:,6)     = DILE_FW_S6
    NoahmpIO%DILE_FW_TABLE(:,7)     = DILE_FW_S7
    NoahmpIO%DILE_FW_TABLE(:,8)     = DILE_FW_S8
    NoahmpIO%LF_OVRC_TABLE(:,1)     = LF_OVRC_S1
    NoahmpIO%LF_OVRC_TABLE(:,2)     = LF_OVRC_S2
    NoahmpIO%LF_OVRC_TABLE(:,3)     = LF_OVRC_S3
    NoahmpIO%LF_OVRC_TABLE(:,4)     = LF_OVRC_S4
    NoahmpIO%LF_OVRC_TABLE(:,5)     = LF_OVRC_S5
    NoahmpIO%LF_OVRC_TABLE(:,6)     = LF_OVRC_S6
    NoahmpIO%LF_OVRC_TABLE(:,7)     = LF_OVRC_S7
    NoahmpIO%LF_OVRC_TABLE(:,8)     = LF_OVRC_S8
    NoahmpIO%ST_OVRC_TABLE(:,1)     = ST_OVRC_S1
    NoahmpIO%ST_OVRC_TABLE(:,2)     = ST_OVRC_S2
    NoahmpIO%ST_OVRC_TABLE(:,3)     = ST_OVRC_S3
    NoahmpIO%ST_OVRC_TABLE(:,4)     = ST_OVRC_S4
    NoahmpIO%ST_OVRC_TABLE(:,5)     = ST_OVRC_S5
    NoahmpIO%ST_OVRC_TABLE(:,6)     = ST_OVRC_S6
    NoahmpIO%ST_OVRC_TABLE(:,7)     = ST_OVRC_S7
    NoahmpIO%ST_OVRC_TABLE(:,8)     = ST_OVRC_S8
    NoahmpIO%RT_OVRC_TABLE(:,1)     = RT_OVRC_S1
    NoahmpIO%RT_OVRC_TABLE(:,2)     = RT_OVRC_S2
    NoahmpIO%RT_OVRC_TABLE(:,3)     = RT_OVRC_S3
    NoahmpIO%RT_OVRC_TABLE(:,4)     = RT_OVRC_S4
    NoahmpIO%RT_OVRC_TABLE(:,5)     = RT_OVRC_S5
    NoahmpIO%RT_OVRC_TABLE(:,6)     = RT_OVRC_S6
    NoahmpIO%RT_OVRC_TABLE(:,7)     = RT_OVRC_S7
    NoahmpIO%RT_OVRC_TABLE(:,8)     = RT_OVRC_S8
    NoahmpIO%LFPT_TABLE   (:,1)     = LFPT_S1
    NoahmpIO%LFPT_TABLE   (:,2)     = LFPT_S2
    NoahmpIO%LFPT_TABLE   (:,3)     = LFPT_S3
    NoahmpIO%LFPT_TABLE   (:,4)     = LFPT_S4
    NoahmpIO%LFPT_TABLE   (:,5)     = LFPT_S5
    NoahmpIO%LFPT_TABLE   (:,6)     = LFPT_S6
    NoahmpIO%LFPT_TABLE   (:,7)     = LFPT_S7
    NoahmpIO%LFPT_TABLE   (:,8)     = LFPT_S8
    NoahmpIO%STPT_TABLE   (:,1)     = STPT_S1
    NoahmpIO%STPT_TABLE   (:,2)     = STPT_S2
    NoahmpIO%STPT_TABLE   (:,3)     = STPT_S3
    NoahmpIO%STPT_TABLE   (:,4)     = STPT_S4
    NoahmpIO%STPT_TABLE   (:,5)     = STPT_S5
    NoahmpIO%STPT_TABLE   (:,6)     = STPT_S6
    NoahmpIO%STPT_TABLE   (:,7)     = STPT_S7
    NoahmpIO%STPT_TABLE   (:,8)     = STPT_S8
    NoahmpIO%RTPT_TABLE   (:,1)     = RTPT_S1
    NoahmpIO%RTPT_TABLE   (:,2)     = RTPT_S2
    NoahmpIO%RTPT_TABLE   (:,3)     = RTPT_S3
    NoahmpIO%RTPT_TABLE   (:,4)     = RTPT_S4
    NoahmpIO%RTPT_TABLE   (:,5)     = RTPT_S5
    NoahmpIO%RTPT_TABLE   (:,6)     = RTPT_S6
    NoahmpIO%RTPT_TABLE   (:,7)     = RTPT_S7
    NoahmpIO%RTPT_TABLE   (:,8)     = RTPT_S8
    NoahmpIO%GRAINPT_TABLE(:,1)     = GRAINPT_S1
    NoahmpIO%GRAINPT_TABLE(:,2)     = GRAINPT_S2
    NoahmpIO%GRAINPT_TABLE(:,3)     = GRAINPT_S3
    NoahmpIO%GRAINPT_TABLE(:,4)     = GRAINPT_S4
    NoahmpIO%GRAINPT_TABLE(:,5)     = GRAINPT_S5
    NoahmpIO%GRAINPT_TABLE(:,6)     = GRAINPT_S6
    NoahmpIO%GRAINPT_TABLE(:,7)     = GRAINPT_S7
    NoahmpIO%GRAINPT_TABLE(:,8)     = GRAINPT_S8
    NoahmpIO%LFCT_TABLE   (:,1)     = LFCT_S1
    NoahmpIO%LFCT_TABLE   (:,2)     = LFCT_S2
    NoahmpIO%LFCT_TABLE   (:,3)     = LFCT_S3
    NoahmpIO%LFCT_TABLE   (:,4)     = LFCT_S4
    NoahmpIO%LFCT_TABLE   (:,5)     = LFCT_S5
    NoahmpIO%LFCT_TABLE   (:,6)     = LFCT_S6
    NoahmpIO%LFCT_TABLE   (:,7)     = LFCT_S7
    NoahmpIO%LFCT_TABLE   (:,8)     = LFCT_S8
    NoahmpIO%STCT_TABLE   (:,1)     = STCT_S1
    NoahmpIO%STCT_TABLE   (:,2)     = STCT_S2
    NoahmpIO%STCT_TABLE   (:,3)     = STCT_S3
    NoahmpIO%STCT_TABLE   (:,4)     = STCT_S4
    NoahmpIO%STCT_TABLE   (:,5)     = STCT_S5
    NoahmpIO%STCT_TABLE   (:,6)     = STCT_S6
    NoahmpIO%STCT_TABLE   (:,7)     = STCT_S7
    NoahmpIO%STCT_TABLE   (:,8)     = STCT_S8
    NoahmpIO%RTCT_TABLE   (:,1)     = RTCT_S1
    NoahmpIO%RTCT_TABLE   (:,2)     = RTCT_S2
    NoahmpIO%RTCT_TABLE   (:,3)     = RTCT_S3
    NoahmpIO%RTCT_TABLE   (:,4)     = RTCT_S4
    NoahmpIO%RTCT_TABLE   (:,5)     = RTCT_S5
    NoahmpIO%RTCT_TABLE   (:,6)     = RTCT_S6
    NoahmpIO%RTCT_TABLE   (:,7)     = RTCT_S7
    NoahmpIO%RTCT_TABLE   (:,8)     = RTCT_S8

    !---------------- NoahmpTable.TBL tile drainage parameters
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
    NoahmpIO%DRAIN_LAYER_OPT_TABLE        = DRAIN_LAYER_OPT
    NoahmpIO%TDSMC_FAC_TABLE(1:NSOILTYPE) = TDSMC_FAC(1:NSOILTYPE)
    NoahmpIO%TD_DEPTH_TABLE (1:NSOILTYPE) = TD_DEPTH (1:NSOILTYPE)
    NoahmpIO%TD_DC_TABLE    (1:NSOILTYPE) = TD_DC    (1:NSOILTYPE)
    NoahmpIO%TD_DCOEF_TABLE (1:NSOILTYPE) = TD_DCOEF (1:NSOILTYPE)
    NoahmpIO%TD_D_TABLE     (1:NSOILTYPE) = TD_D     (1:NSOILTYPE)
    NoahmpIO%TD_ADEPTH_TABLE(1:NSOILTYPE) = TD_ADEPTH(1:NSOILTYPE)
    NoahmpIO%TD_RADI_TABLE  (1:NSOILTYPE) = TD_RADI  (1:NSOILTYPE)
    NoahmpIO%TD_SPAC_TABLE  (1:NSOILTYPE) = TD_SPAC  (1:NSOILTYPE)
    NoahmpIO%TD_DDRAIN_TABLE(1:NSOILTYPE) = TD_DDRAIN(1:NSOILTYPE)
    NoahmpIO%KLAT_FAC_TABLE (1:NSOILTYPE) = KLAT_FAC (1:NSOILTYPE)

    !---------------- NoahmpTable.TBL optional parameters
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
    NoahmpIO%sr2006_theta_1500t_a_TABLE = sr2006_theta_1500t_a
    NoahmpIO%sr2006_theta_1500t_b_TABLE = sr2006_theta_1500t_b
    NoahmpIO%sr2006_theta_1500t_c_TABLE = sr2006_theta_1500t_c
    NoahmpIO%sr2006_theta_1500t_d_TABLE = sr2006_theta_1500t_d
    NoahmpIO%sr2006_theta_1500t_e_TABLE = sr2006_theta_1500t_e
    NoahmpIO%sr2006_theta_1500t_f_TABLE = sr2006_theta_1500t_f
    NoahmpIO%sr2006_theta_1500t_g_TABLE = sr2006_theta_1500t_g
    NoahmpIO%sr2006_theta_1500_a_TABLE  = sr2006_theta_1500_a
    NoahmpIO%sr2006_theta_1500_b_TABLE  = sr2006_theta_1500_b
    NoahmpIO%sr2006_theta_33t_a_TABLE   = sr2006_theta_33t_a
    NoahmpIO%sr2006_theta_33t_b_TABLE   = sr2006_theta_33t_b
    NoahmpIO%sr2006_theta_33t_c_TABLE   = sr2006_theta_33t_c
    NoahmpIO%sr2006_theta_33t_d_TABLE   = sr2006_theta_33t_d
    NoahmpIO%sr2006_theta_33t_e_TABLE   = sr2006_theta_33t_e
    NoahmpIO%sr2006_theta_33t_f_TABLE   = sr2006_theta_33t_f
    NoahmpIO%sr2006_theta_33t_g_TABLE   = sr2006_theta_33t_g
    NoahmpIO%sr2006_theta_33_a_TABLE    = sr2006_theta_33_a
    NoahmpIO%sr2006_theta_33_b_TABLE    = sr2006_theta_33_b
    NoahmpIO%sr2006_theta_33_c_TABLE    = sr2006_theta_33_c
    NoahmpIO%sr2006_theta_s33t_a_TABLE  = sr2006_theta_s33t_a
    NoahmpIO%sr2006_theta_s33t_b_TABLE  = sr2006_theta_s33t_b
    NoahmpIO%sr2006_theta_s33t_c_TABLE  = sr2006_theta_s33t_c
    NoahmpIO%sr2006_theta_s33t_d_TABLE  = sr2006_theta_s33t_d
    NoahmpIO%sr2006_theta_s33t_e_TABLE  = sr2006_theta_s33t_e
    NoahmpIO%sr2006_theta_s33t_f_TABLE  = sr2006_theta_s33t_f
    NoahmpIO%sr2006_theta_s33t_g_TABLE  = sr2006_theta_s33t_g
    NoahmpIO%sr2006_theta_s33_a_TABLE   = sr2006_theta_s33_a
    NoahmpIO%sr2006_theta_s33_b_TABLE   = sr2006_theta_s33_b
    NoahmpIO%sr2006_psi_et_a_TABLE      = sr2006_psi_et_a
    NoahmpIO%sr2006_psi_et_b_TABLE      = sr2006_psi_et_b
    NoahmpIO%sr2006_psi_et_c_TABLE      = sr2006_psi_et_c
    NoahmpIO%sr2006_psi_et_d_TABLE      = sr2006_psi_et_d
    NoahmpIO%sr2006_psi_et_e_TABLE      = sr2006_psi_et_e
    NoahmpIO%sr2006_psi_et_f_TABLE      = sr2006_psi_et_f
    NoahmpIO%sr2006_psi_et_g_TABLE      = sr2006_psi_et_g
    NoahmpIO%sr2006_psi_e_a_TABLE       = sr2006_psi_e_a
    NoahmpIO%sr2006_psi_e_b_TABLE       = sr2006_psi_e_b
    NoahmpIO%sr2006_psi_e_c_TABLE       = sr2006_psi_e_c
    NoahmpIO%sr2006_smcmax_a_TABLE      = sr2006_smcmax_a
    NoahmpIO%sr2006_smcmax_b_TABLE      = sr2006_smcmax_b


  end subroutine ReadNoahmpTable

!=== read namelist values

  subroutine ReadNamelist(NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout)  :: NoahmpIO

!---------------------------------------------------------------------
!  NAMELIST start
!---------------------------------------------------------------------

    ! local namelist variables
    
    character(len=256)      :: indir = '.'

    integer                 :: ierr
    integer                 :: NSOIL                 ! number of soil layers
    integer                 :: forcing_timestep
    integer                 :: noah_timestep
    integer                 :: start_year
    integer                 :: start_month
    integer                 :: start_day
    integer                 :: start_hour
    integer                 :: start_min
    character(len=256)      :: outdir = "."
    character(len=256)      :: restart_filename_requested = " "
    integer                 :: restart_frequency_hours
    integer                 :: output_timestep
    integer                 :: spinup_loops     = 0
    integer                 :: sf_urban_physics = 0
    integer                 :: use_wudapt_lcz   = 0  ! add for LCZ urban
    integer                 :: num_urban_ndm    = 1
    integer                 :: num_urban_ng     = 1
    integer                 :: num_urban_nwr    = 1
    integer                 :: num_urban_ngb    = 1
    integer                 :: num_urban_nf     = 1
    integer                 :: num_urban_nz     = 1
    integer                 :: num_urban_nbui   = 1
    integer                 :: num_urban_hi     = 15 
    real(kind=kind_noahmp)  :: urban_atmosphere_thickness = 2.0
 
    ! new urban var for green roof and solar panel
    integer                 :: num_urban_ngr    = 10  ! = ngr_u in bep_bem.F
    integer                 :: urban_map_zgrd   = 1

    ! derived urban dimensions
    integer                 :: urban_map_zrd
    integer                 :: urban_map_zwd
    integer                 :: urban_map_gd
    integer                 :: urban_map_zd
    integer                 :: urban_map_zdf
    integer                 :: urban_map_bd
    integer                 :: urban_map_wd
    integer                 :: urban_map_gbd
    integer                 :: urban_map_fbd
    character(len=256)      :: forcing_name_T  = "T2D"
    character(len=256)      :: forcing_name_Q  = "Q2D"
    character(len=256)      :: forcing_name_U  = "U2D"
    character(len=256)      :: forcing_name_V  = "V2D"
    character(len=256)      :: forcing_name_P  = "PSFC"
    character(len=256)      :: forcing_name_LW = "LWDOWN"
    character(len=256)      :: forcing_name_SW = "SWDOWN"
    character(len=256)      :: forcing_name_PR = "RAINRATE"
    character(len=256)      :: forcing_name_SN = ""
    integer                 :: dynamic_veg_option                 = 4
    integer                 :: canopy_stomatal_resistance_option  = 1
    integer                 :: btr_option                         = 1
    integer                 :: surface_runoff_option              = 3
    integer                 :: subsurface_runoff_option           = 3
    integer                 :: surface_drag_option                = 1
    integer                 :: supercooled_water_option           = 1
    integer                 :: frozen_soil_option                 = 1
    integer                 :: radiative_transfer_option          = 3
    integer                 :: snow_albedo_option                 = 1
    integer                 :: snow_thermal_conductivity          = 1
    integer                 :: pcp_partition_option               = 1
    integer                 :: tbot_option                        = 2
    integer                 :: temp_time_scheme_option            = 1
    integer                 :: glacier_option                     = 1
    integer                 :: surface_resistance_option          = 1
    integer                 :: soil_data_option                   = 1
    integer                 :: pedotransfer_option                = 1
    integer                 :: crop_option                        = 0
    integer                 :: irrigation_option                  = 0 
    integer                 :: irrigation_method                  = 0
    integer                 :: dvic_infiltration_option           = 1
    integer                 :: tile_drainage_option               = 0
    integer                 :: split_output_count                 = 1
    logical                 :: skip_first_output                  = .false.
    integer                 :: khour                              = -9999
    integer                 :: kday                               = -9999
    real(kind=kind_noahmp)  :: zlvl                               = 10.
    character(len=256)      :: hrldas_setup_file                  = " "
    character(len=256)      :: spatial_filename                   = " "
    character(len=256)      :: external_veg_filename_template     = " "
    character(len=256)      :: external_lai_filename_template     = " "
    character(len=256)      :: agdata_flnm                        = " "
    character(len=256)      :: tdinput_flnm                       = " "
    integer                 :: xstart                             = 1
    integer                 :: ystart                             = 1
    integer                 :: xend                               = 0
    integer                 :: yend                               = 0
    integer, parameter      :: MAX_SOIL_LEVELS                    = 10     ! maximum soil levels in namelist
    real(kind=kind_noahmp), dimension(MAX_SOIL_LEVELS) :: soil_thick_input ! depth to soil interfaces from namelist [m]
    
    namelist / NOAHLSM_OFFLINE /    &
#ifdef WRF_HYDRO
         finemesh,finemesh_factor,forc_typ, snow_assim , GEO_STATIC_FLNM, HRLDAS_ini_typ, &
#endif
         indir, nsoil, soil_thick_input, forcing_timestep, noah_timestep,                 &
         start_year, start_month, start_day, start_hour, start_min,                       &
         outdir, skip_first_output,                                                       &
         restart_filename_requested, restart_frequency_hours, output_timestep,            &
         spinup_loops,                                                                    &
         forcing_name_T,forcing_name_Q,forcing_name_U,forcing_name_V,forcing_name_P,      &
         forcing_name_LW,forcing_name_SW,forcing_name_PR,forcing_name_SN,                 &
         dynamic_veg_option, canopy_stomatal_resistance_option,                           &
         btr_option, surface_drag_option, supercooled_water_option,        &
         frozen_soil_option, radiative_transfer_option, snow_albedo_option,               &
         snow_thermal_conductivity, surface_runoff_option, subsurface_runoff_option,      &
         pcp_partition_option, tbot_option, temp_time_scheme_option,                      &
         glacier_option, surface_resistance_option,                                       &
         irrigation_option, irrigation_method, dvic_infiltration_option,                  &
         tile_drainage_option,soil_data_option, pedotransfer_option, crop_option,         &
         sf_urban_physics,use_wudapt_lcz,num_urban_hi,urban_atmosphere_thickness,         &
         num_urban_ndm,num_urban_ng,num_urban_nwr ,num_urban_ngb ,                        &
         num_urban_nf ,num_urban_nz,num_urban_nbui,                                       &
         split_output_count,                                                              & 
         khour, kday, zlvl, hrldas_setup_file,                                            &
         spatial_filename, agdata_flnm, tdinput_flnm,                                     &
         external_veg_filename_template, external_lai_filename_template,                  &
         xstart, xend, ystart, yend


    !---------------------------------------------------------------
    !  Initialize namelist variables to dummy values, so we can tell
    !  if they have not been set properly.
    !---------------------------------------------------------------
    NoahmpIO%nsoil                   = -999
    NoahmpIO%soil_thick_input        = -999
    NoahmpIO%dtbl                    = -999
    NoahmpIO%start_year              = -999
    NoahmpIO%start_month             = -999
    NoahmpIO%start_day               = -999
    NoahmpIO%start_hour              = -999
    NoahmpIO%start_min               = -999
    NoahmpIO%khour                   = -999
    NoahmpIO%kday                    = -999
    NoahmpIO%zlvl                    = -999
    NoahmpIO%forcing_timestep        = -999
    NoahmpIO%noah_timestep           = -999
    NoahmpIO%output_timestep         = -999
    NoahmpIO%restart_frequency_hours = -999
    NoahmpIO%spinup_loops            = 0

    !---------------------------------------------------------------
    ! read namelist.input
    !---------------------------------------------------------------
    
    open(30, file="namelist.hrldas", form="FORMATTED")
    read(30, NOAHLSM_OFFLINE, iostat=ierr)
    if (ierr /= 0) then
       write(*,'(/," ***** ERROR: Problem reading namelist NOAHLSM_OFFLINE",/)')
       rewind(30)
       read(30, NOAHLSM_OFFLINE)
       stop " ***** ERROR: Problem reading namelist NOAHLSM_OFFLINE"
    endif
    close(30)
  
    NoahmpIO%dtbl            = real(noah_timestep)
    NoahmpIO%num_soil_layers = nsoil      ! because surface driver uses the long form
    NoahmpIO%NSOIL           = nsoil

    !---------------------------------------------------------------------
    !  NAMELIST end
    !---------------------------------------------------------------------
   
    !---------------------------------------------------------------------
    !  NAMELIST check begin
    !---------------------------------------------------------------------
    NoahmpIO%update_lai = .true.   ! default: use LAI if present in forcing file
    if(dynamic_veg_option == 1 .or. dynamic_veg_option == 2 .or. &
       dynamic_veg_option == 3 .or. dynamic_veg_option == 4 .or. &
       dynamic_veg_option == 5 .or. dynamic_veg_option == 6) &    ! remove dveg=10 and add dveg=1,3,4 into the update_lai flag false condition
       NoahmpIO%update_lai = .false.

    NoahmpIO%update_veg = .false.  ! default: don't use VEGFRA if present in forcing file
    if (dynamic_veg_option == 1 .or. dynamic_veg_option == 6 .or. dynamic_veg_option == 7) &
        NoahmpIO%update_veg = .true.

    if (nsoil < 0) then
        stop " ***** ERROR: NSOIL must be set in the namelist."
    endif

    if ((khour < 0) .and. (kday < 0)) then
        write(*, '(" ***** Namelist error: ************************************")')
        write(*, '(" ***** ")')
        write(*, '(" *****      Either KHOUR or KDAY must be defined.")')
        write(*, '(" ***** ")')
        stop
    else if (( khour < 0 ) .and. (kday > 0)) then
        khour = kday * 24
    else if ((khour > 0) .and. (kday > 0)) then
        write(*, '("Namelist warning:  KHOUR and KDAY both defined.")')
    else
        ! all is well.  KHOUR defined
    endif

    if (forcing_timestep < 0) then
        write(*, *)
        write(*, '(" ***** Namelist error: *****************************************")')
        write(*, '(" ***** ")')
        write(*, '(" *****       FORCING_TIMESTEP needs to be set greater than zero.")')
        write(*, '(" ***** ")')
        write(*, *)
        stop
    endif

    if (noah_timestep < 0) then
        write(*, *)
        write(*, '(" ***** Namelist error: *****************************************")')
        write(*, '(" ***** ")')
        write(*, '(" *****       NOAH_TIMESTEP needs to be set greater than zero.")')
        write(*, '(" *****                     900 seconds is recommended.       ")')
        write(*, '(" ***** ")')
        write(*, *)
        stop
    endif

    !
    ! Check that OUTPUT_TIMESTEP fits into NOAH_TIMESTEP:
    !
    if (output_timestep /= 0) then
       if (mod(output_timestep, noah_timestep) > 0) then
         write(*, *)
         write(*, '(" ***** Namelist error: *********************************************************")')
         write(*, '(" ***** ")')
         write(*, '(" *****       OUTPUT_TIMESTEP should set to an integer multiple of NOAH_TIMESTEP.")')
         write(*, '(" *****            OUTPUT_TIMESTEP = ", I12, " seconds")') output_timestep
         write(*, '(" *****            NOAH_TIMESTEP   = ", I12, " seconds")') noah_timestep
         write(*, '(" ***** ")')
         write(*, *)
         stop
       endif
    endif

   !
   ! Check that RESTART_FREQUENCY_HOURS fits into NOAH_TIMESTEP:
   !
    if (restart_frequency_hours /= 0) then
       if (mod(restart_frequency_hours*3600, noah_timestep) > 0) then
         write(*, *)
         write(*, '(" ***** Namelist error: ******************************************************")')
         write(*, '(" ***** ")')
         write(*, '(" *****       RESTART_FREQUENCY_HOURS (converted to seconds) should set to an ")')
         write(*, '(" *****       integer multiple of NOAH_TIMESTEP.")')
         write(*, '(" *****            RESTART_FREQUENCY_HOURS = ", I12, " hours:  ", I12, " seconds")') &
               restart_frequency_hours, restart_frequency_hours*3600
         write(*, '(" *****            NOAH_TIMESTEP           = ", I12, " seconds")') noah_timestep
         write(*, '(" ***** ")')
         write(*, *)
         stop
       endif
    endif

    if (dynamic_veg_option == 2 .or. dynamic_veg_option == 5 .or. dynamic_veg_option == 6) then
      if ( canopy_stomatal_resistance_option /= 1) then
         write(*, *)
         write(*, '(" ***** Namelist error: ******************************************************")')
         write(*, '(" ***** ")')
         write(*, '(" *****       CANOPY_STOMATAL_RESISTANCE_OPTION must be 1 when DYNAMIC_VEG_OPTION == 2/5/6")')
         write(*, *)
         stop
      endif
    endif

    if (soil_data_option == 4 .and. spatial_filename == " ") then
        write(*, *)
        write(*, '(" ***** Namelist error: ******************************************************")')
        write(*, '(" ***** ")')
        write(*, '(" *****       SPATIAL_FILENAME must be provided when SOIL_DATA_OPTION == 4")')
        write(*, *)
        stop
    endif

    if (sf_urban_physics == 2 .or. sf_urban_physics == 3) then
       if ( urban_atmosphere_thickness <= 0.0) then
         write(*, *)
         write(*, '(" ***** Namelist error: ******************************************************")')
         write(*, '(" ***** ")')
         write(*, '(" *****       When running BEP/BEM, URBAN_ATMOSPHERE_LEVELS must contain at least 3 levels")')
         write(*, *)
         stop
       endif
       NoahmpIO%num_urban_atmosphere = int(zlvl/urban_atmosphere_thickness)
       if (zlvl - NoahmpIO%num_urban_atmosphere*urban_atmosphere_thickness >= 0.5*urban_atmosphere_thickness)  &
           NoahmpIO%num_urban_atmosphere = NoahmpIO%num_urban_atmosphere + 1
       if ( NoahmpIO%num_urban_atmosphere <= 2) then
         write(*, *)
         write(*, '(" ***** Namelist error: ******************************************************")')
         write(*, '(" ***** ")')
         write(*, '(" *****       When running BEP/BEM, num_urban_atmosphere must contain at least 3 levels, ")')
         write(*, '(" *****        decrease URBAN_ATMOSPHERE_THICKNESS")')
         write(*, *)
         stop
       endif
    endif
    
    !---------------------------------------------------------------------
    !  Transfer Namelist locals to input data structure
    !---------------------------------------------------------------------
 
    NoahmpIO%IOPT_DVEG                         = dynamic_veg_option 
    NoahmpIO%IOPT_CRS                          = canopy_stomatal_resistance_option
    NoahmpIO%IOPT_BTR                          = btr_option
    NoahmpIO%IOPT_RUNSRF                       = surface_runoff_option
    NoahmpIO%IOPT_RUNSUB                       = subsurface_runoff_option
    NoahmpIO%IOPT_SFC                          = surface_drag_option
    NoahmpIO%IOPT_FRZ                          = supercooled_water_option
    NoahmpIO%IOPT_INF                          = frozen_soil_option
    NoahmpIO%IOPT_RAD                          = radiative_transfer_option
    NoahmpIO%IOPT_ALB                          = snow_albedo_option
    NoahmpIO%IOPT_SNF                          = pcp_partition_option
    NoahmpIO%IOPT_TKSNO                        = snow_thermal_conductivity 
    NoahmpIO%IOPT_TBOT                         = tbot_option
    NoahmpIO%IOPT_STC                          = temp_time_scheme_option
    NoahmpIO%IOPT_GLA                          = glacier_option
    NoahmpIO%IOPT_RSF                          = surface_resistance_option
    NoahmpIO%IOPT_SOIL                         = soil_data_option
    NoahmpIO%IOPT_PEDO                         = pedotransfer_option
    NoahmpIO%IOPT_CROP                         = crop_option
    NoahmpIO%IOPT_IRR                          = irrigation_option
    NoahmpIO%IOPT_IRRM                         = irrigation_method
    NoahmpIO%IOPT_INFDV                        = dvic_infiltration_option
    NoahmpIO%IOPT_TDRN                         = tile_drainage_option
    
    NoahmpIO%indir                             = indir
    NoahmpIO%forcing_timestep                  = forcing_timestep
    NoahmpIO%noah_timestep                     = noah_timestep
    NoahmpIO%start_year                        = start_year
    NoahmpIO%start_month                       = start_month
    NoahmpIO%start_day                         = start_day
    NoahmpIO%start_hour                        = start_hour
    NoahmpIO%start_min                         = start_min
    NoahmpIO%outdir                            = outdir
    NoahmpIO%restart_filename_requested        = restart_filename_requested
    NoahmpIO%restart_frequency_hours           = restart_frequency_hours
    NoahmpIO%output_timestep                   = output_timestep
    NoahmpIO%spinup_loops                      = spinup_loops
    NoahmpIO%sf_urban_physics                  = sf_urban_physics
    NoahmpIO%use_wudapt_lcz                    = use_wudapt_lcz
    NoahmpIO%num_urban_ndm                     = num_urban_ndm
    NoahmpIO%num_urban_ng                      = num_urban_ng
    NoahmpIO%num_urban_nwr                     = num_urban_nwr
    NoahmpIO%num_urban_ngb                     = num_urban_ngb
    NoahmpIO%num_urban_nf                      = num_urban_nf
    NoahmpIO%num_urban_nz                      = num_urban_nz
    NoahmpIO%num_urban_nbui                    = num_urban_nbui
    NoahmpIO%num_urban_hi                      = num_urban_hi
    NoahmpIO%urban_atmosphere_thickness        = urban_atmosphere_thickness
    NoahmpIO%num_urban_ngr                     = num_urban_ngr
    NoahmpIO%urban_map_zgrd                    = urban_map_zgrd
    NoahmpIO%urban_map_zrd                     = urban_map_zrd
    NoahmpIO%urban_map_zwd                     = urban_map_zwd
    NoahmpIO%urban_map_gd                      = urban_map_gd
    NoahmpIO%urban_map_zd                      = urban_map_zd
    NoahmpIO%urban_map_zdf                     = urban_map_zdf
    NoahmpIO%urban_map_bd                      = urban_map_bd
    NoahmpIO%urban_map_wd                      = urban_map_wd
    NoahmpIO%urban_map_gbd                     = urban_map_gbd
    NoahmpIO%urban_map_fbd                     = urban_map_fbd
    NoahmpIO%forcing_name_T                    = forcing_name_T
    NoahmpIO%forcing_name_Q                    = forcing_name_Q
    NoahmpIO%forcing_name_U                    = forcing_name_U
    NoahmpIO%forcing_name_V                    = forcing_name_V
    NoahmpIO%forcing_name_P                    = forcing_name_P
    NoahmpIO%forcing_name_LW                   = forcing_name_LW
    NoahmpIO%forcing_name_SW                   = forcing_name_SW
    NoahmpIO%forcing_name_PR                   = forcing_name_PR
    NoahmpIO%forcing_name_SN                   = forcing_name_SN
    NoahmpIO%split_output_count                = split_output_count
    NoahmpIO%skip_first_output                 = skip_first_output
    NoahmpIO%khour                             = khour
    NoahmpIO%kday                              = kday
    NoahmpIO%zlvl                              = zlvl
    NoahmpIO%hrldas_setup_file                 = hrldas_setup_file
    NoahmpIO%spatial_filename                  = spatial_filename
    NoahmpIO%external_veg_filename_template    = external_veg_filename_template
    NoahmpIO%external_lai_filename_template    = external_lai_filename_template
    NoahmpIO%agdata_flnm                       = agdata_flnm
    NoahmpIO%tdinput_flnm                      = tdinput_flnm
    NoahmpIO%xstart                            = xstart
    NoahmpIO%ystart                            = ystart
    NoahmpIO%xend                              = xend
    NoahmpIO%yend                              = yend
    NoahmpIO%MAX_SOIL_LEVELS                   = MAX_SOIL_LEVELS
    NoahmpIO%soil_thick_input                  = soil_thick_input 
 
!---------------------------------------------------------------------
!  NAMELIST check end
!---------------------------------------------------------------------

  end subroutine ReadNamelist

!=== initialize with default values

  subroutine NoahmpIOVarInitDefault(NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout) :: NoahmpIO
    
    associate(XSTART  =>  NoahmpIO%XSTART   ,&
              XEND    =>  NoahmpIO%XEND     ,&
              YSTART  =>  NoahmpIO%YSTART   ,&
              YEND    =>  NoahmpIO%YEND     ,&
              KDS     =>  NoahmpIO%KDS      ,&
              KDE     =>  NoahmpIO%KDE      ,&
              NSOIL   =>  NoahmpIO%NSOIL    ,&
              NSNOW   =>  NoahmpIO%NSNOW     &
             )
    allocate ( NoahmpIO%COSZEN       (XSTART:XEND,YSTART:YEND) )            ! cosine zenith angle
    allocate ( NoahmpIO%XLAT         (XSTART:XEND,YSTART:YEND) )            ! latitude [radians] 
    allocate ( NoahmpIO%DZ8W         (XSTART:XEND,KDS:KDE,YSTART:YEND) )    ! thickness of atmo layers [m]
    allocate ( NoahmpIO%DZS          (1:NSOIL)                   )          ! thickness of soil layers [m]
    allocate ( NoahmpIO%ZSOIL        (1:NSOIL)                   )          ! depth to soil interfaces [m] 
    allocate ( NoahmpIO%IVGTYP       (XSTART:XEND,YSTART:YEND) )            ! vegetation type
    allocate ( NoahmpIO%ISLTYP       (XSTART:XEND,YSTART:YEND) )            ! soil type
    allocate ( NoahmpIO%VEGFRA       (XSTART:XEND,YSTART:YEND) )            ! vegetation fraction []
    allocate ( NoahmpIO%TMN          (XSTART:XEND,YSTART:YEND) )            ! deep soil temperature [K]
    allocate ( NoahmpIO%XLAND        (XSTART:XEND,YSTART:YEND) )            ! =2 ocean; =1 land/seaice
    allocate ( NoahmpIO%XICE         (XSTART:XEND,YSTART:YEND) )            ! fraction of grid that is seaice

    allocate ( NoahmpIO%T_PHY        (XSTART:XEND,KDS:KDE,YSTART:YEND) )    ! 3D atmospheric temperature valid at mid-levels [K]
    allocate ( NoahmpIO%QV_CURR      (XSTART:XEND,KDS:KDE,YSTART:YEND) )    ! 3D water vapor mixing ratio [kg/kg_dry]
    allocate ( NoahmpIO%U_PHY        (XSTART:XEND,KDS:KDE,YSTART:YEND) )    ! 3D U wind component [m/s]
    allocate ( NoahmpIO%V_PHY        (XSTART:XEND,KDS:KDE,YSTART:YEND) )    ! 3D V wind component [m/s]
    
    allocate ( NoahmpIO%SWDOWN       (XSTART:XEND,YSTART:YEND) )            ! solar down at surface [W m-2]
    allocate ( NoahmpIO%SWDDIR       (XSTART:XEND,YSTART:YEND) )            ! solar down at surface [W m-2] for new urban solar panel
    allocate ( NoahmpIO%SWDDIF       (XSTART:XEND,YSTART:YEND) )            ! solar down at surface [W m-2] for new urban solar panel
    allocate ( NoahmpIO%GLW          (XSTART:XEND,YSTART:YEND) )            ! longwave down at surface [W m-2]

    allocate ( NoahmpIO%P8W          (XSTART:XEND,KDS:KDE,YSTART:YEND) )    ! 3D pressure, valid at interface [Pa]
    
    allocate ( NoahmpIO%RAINBL       (XSTART:XEND,YSTART:YEND) )            ! total precipitation entering land model [mm] per time step
    allocate ( NoahmpIO%SNOWBL       (XSTART:XEND,YSTART:YEND) )            ! snow entering land model [mm] per time step
    allocate ( NoahmpIO%RAINBL_tmp   (XSTART:XEND,YSTART:YEND) )            ! precipitation entering land model [mm]
    allocate ( NoahmpIO%SR           (XSTART:XEND,YSTART:YEND) )            ! frozen precip ratio entering land model [-]
    allocate ( NoahmpIO%RAINCV       (XSTART:XEND,YSTART:YEND) )            ! convective precip forcing [mm]
    allocate ( NoahmpIO%RAINNCV      (XSTART:XEND,YSTART:YEND) )            ! non-convective precip forcing [mm]
    allocate ( NoahmpIO%RAINSHV      (XSTART:XEND,YSTART:YEND) )            ! shallow conv. precip forcing [mm]
    allocate ( NoahmpIO%SNOWNCV      (XSTART:XEND,YSTART:YEND) )            ! non-covective snow forcing (subset of rainncv) [mm]
    allocate ( NoahmpIO%GRAUPELNCV   (XSTART:XEND,YSTART:YEND) )            ! non-convective graupel forcing (subset of rainncv) [mm]
    allocate ( NoahmpIO%HAILNCV      (XSTART:XEND,YSTART:YEND) )            ! non-convective hail forcing (subset of rainncv) [mm]

    allocate ( NoahmpIO%bexp_3d      (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! C-H B exponent
    allocate ( NoahmpIO%smcdry_3D    (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! Soil Moisture Limit: Dry
    allocate ( NoahmpIO%smcwlt_3D    (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! Soil Moisture Limit: Wilt
    allocate ( NoahmpIO%smcref_3D    (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! Soil Moisture Limit: Reference
    allocate ( NoahmpIO%smcmax_3D    (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! Soil Moisture Limit: Max
    allocate ( NoahmpIO%dksat_3D     (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! Saturated Soil Conductivity
    allocate ( NoahmpIO%dwsat_3D     (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! Saturated Soil Diffusivity
    allocate ( NoahmpIO%psisat_3D    (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! Saturated Matric Potential
    allocate ( NoahmpIO%quartz_3D    (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! Soil quartz content
    allocate ( NoahmpIO%refdk_2D     (XSTART:XEND,YSTART:YEND) )            ! Reference Soil Conductivity
    allocate ( NoahmpIO%refkdt_2D    (XSTART:XEND,YSTART:YEND) )            ! Soil Infiltration Parameter
    
    allocate ( NoahmpIO%soilcomp     (XSTART:XEND,1:2*NSOIL,YSTART:YEND) )  ! Soil sand and clay content [fraction]

    allocate ( NoahmpIO%soilcl1      (XSTART:XEND,YSTART:YEND) )            ! Soil texture class with depth
    allocate ( NoahmpIO%soilcl2      (XSTART:XEND,YSTART:YEND) )            ! Soil texture class with depth
    allocate ( NoahmpIO%soilcl3      (XSTART:XEND,YSTART:YEND) )            ! Soil texture class with depth
    allocate ( NoahmpIO%soilcl4      (XSTART:XEND,YSTART:YEND) )            ! Soil texture class with depth
    allocate ( NoahmpIO%irr_frac_2D  (XSTART:XEND,YSTART:YEND) )            ! irrigation Fraction
    allocate ( NoahmpIO%irr_har_2D   (XSTART:XEND,YSTART:YEND) )            ! number of days before harvest date to stop irrigation 
    allocate ( NoahmpIO%irr_lai_2D   (XSTART:XEND,YSTART:YEND) )            ! Minimum lai to trigger irrigation
    allocate ( NoahmpIO%irr_mad_2D   (XSTART:XEND,YSTART:YEND) )            ! management allowable deficit (0-1)
    allocate ( NoahmpIO%filoss_2D    (XSTART:XEND,YSTART:YEND) )            ! fraction of flood irrigation loss (0-1) 
    allocate ( NoahmpIO%sprir_rate_2D(XSTART:XEND,YSTART:YEND) )            ! mm/h, sprinkler irrigation rate
    allocate ( NoahmpIO%micir_rate_2D(XSTART:XEND,YSTART:YEND) )            ! mm/h, micro irrigation rate
    allocate ( NoahmpIO%firtfac_2D   (XSTART:XEND,YSTART:YEND) )            ! flood application rate factor
    allocate ( NoahmpIO%ir_rain_2D   (XSTART:XEND,YSTART:YEND) )            ! maximum precipitation to stop irrigation trigger
    allocate ( NoahmpIO%bvic_2D      (XSTART:XEND,YSTART:YEND) )            ! VIC model infiltration parameter [-]
    allocate ( NoahmpIO%axaj_2D      (XSTART:XEND,YSTART:YEND) )            ! Tension water distribution inflection parameter [-]
    allocate ( NoahmpIO%bxaj_2D      (XSTART:XEND,YSTART:YEND) )            ! Tension water distribution shape parameter [-]
    allocate ( NoahmpIO%xxaj_2D      (XSTART:XEND,YSTART:YEND) )            ! Free water distribution shape parameter [-]
    allocate ( NoahmpIO%bdvic_2D     (XSTART:XEND,YSTART:YEND) )            ! DVIC model infiltration parameter [-]
    allocate ( NoahmpIO%gdvic_2D     (XSTART:XEND,YSTART:YEND) )            ! Mean Capillary Drive (m) for infiltration models
    allocate ( NoahmpIO%bbvic_2D     (XSTART:XEND,YSTART:YEND) )            ! DVIC heterogeniety parameter for infiltration [-]
    allocate ( NoahmpIO%KLAT_FAC     (XSTART:XEND,YSTART:YEND) )            ! factor multiplier to hydraulic conductivity
    allocate ( NoahmpIO%TDSMC_FAC    (XSTART:XEND,YSTART:YEND) )            ! factor multiplier to field capacity
    allocate ( NoahmpIO%TD_DC        (XSTART:XEND,YSTART:YEND) )            ! drainage coefficient for simple
    allocate ( NoahmpIO%TD_DCOEF     (XSTART:XEND,YSTART:YEND) )            ! drainge coefficient for Hooghoudt 
    allocate ( NoahmpIO%TD_DDRAIN    (XSTART:XEND,YSTART:YEND) )            ! depth of drain
    allocate ( NoahmpIO%TD_RADI      (XSTART:XEND,YSTART:YEND) )            ! tile radius
    allocate ( NoahmpIO%TD_SPAC      (XSTART:XEND,YSTART:YEND) )            ! tile spacing

! INOUT (with generic LSM equivalent) (as defined in WRF)
    allocate ( NoahmpIO%TSK          (XSTART:XEND,YSTART:YEND) )            ! surface radiative temperature [K]
    allocate ( NoahmpIO%HFX          (XSTART:XEND,YSTART:YEND) )            ! sensible heat flux [W m-2]
    allocate ( NoahmpIO%QFX          (XSTART:XEND,YSTART:YEND) )            ! latent heat flux [kg s-1 m-2]
    allocate ( NoahmpIO%LH           (XSTART:XEND,YSTART:YEND) )            ! latent heat flux [W m-2]
    allocate ( NoahmpIO%GRDFLX       (XSTART:XEND,YSTART:YEND) )            ! ground/snow heat flux [W m-2]
    allocate ( NoahmpIO%SMSTAV       (XSTART:XEND,YSTART:YEND) )            ! soil moisture avail. [not used]
    allocate ( NoahmpIO%SMSTOT       (XSTART:XEND,YSTART:YEND) )            ! total soil water [mm][not used]
    allocate ( NoahmpIO%SFCRUNOFF    (XSTART:XEND,YSTART:YEND) )            ! accumulated surface runoff [m]
    allocate ( NoahmpIO%UDRUNOFF     (XSTART:XEND,YSTART:YEND) )            ! accumulated sub-surface runoff [m]
    allocate ( NoahmpIO%ALBEDO       (XSTART:XEND,YSTART:YEND) )            ! total grid albedo []
    allocate ( NoahmpIO%SNOWC        (XSTART:XEND,YSTART:YEND) )            ! snow cover fraction []
    
    allocate ( NoahmpIO%SMOISEQ      (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! eq volumetric soil moisture [m3/m3]
    allocate ( NoahmpIO%SMOIS        (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! volumetric soil moisture [m3/m3]
    allocate ( NoahmpIO%SH2O         (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! volumetric liquid soil moisture [m3/m3]
    allocate ( NoahmpIO%TSLB         (XSTART:XEND,1:NSOIL,YSTART:YEND) )    ! soil temperature [K]
    
    allocate ( NoahmpIO%SNOW         (XSTART:XEND,YSTART:YEND) )            ! snow water equivalent [mm]
    allocate ( NoahmpIO%SNOWH        (XSTART:XEND,YSTART:YEND) )            ! physical snow depth [m]
    allocate ( NoahmpIO%CANWAT       (XSTART:XEND,YSTART:YEND) )            ! total canopy water + ice [mm]
    allocate ( NoahmpIO%ACSNOM       (XSTART:XEND,YSTART:YEND) )            ! accumulated snow melt leaving pack
    allocate ( NoahmpIO%ACSNOW       (XSTART:XEND,YSTART:YEND) )            ! accumulated snow on grid
    allocate ( NoahmpIO%EMISS        (XSTART:XEND,YSTART:YEND) )            ! surface bulk emissivity
    allocate ( NoahmpIO%QSFC         (XSTART:XEND,YSTART:YEND) )            ! bulk surface specific humidity

! INOUT (with no Noah LSM equivalent) (as defined in WRF)
    allocate ( NoahmpIO%ISNOWXY      (XSTART:XEND,YSTART:YEND) )            ! actual no. of snow layers
    allocate ( NoahmpIO%TVXY         (XSTART:XEND,YSTART:YEND) )            ! vegetation leaf temperature
    allocate ( NoahmpIO%TGXY         (XSTART:XEND,YSTART:YEND) )            ! bulk ground surface temperature
    allocate ( NoahmpIO%CANICEXY     (XSTART:XEND,YSTART:YEND) )            ! canopy-intercepted ice (mm)
    allocate ( NoahmpIO%CANLIQXY     (XSTART:XEND,YSTART:YEND) )            ! canopy-intercepted liquid water (mm)
    allocate ( NoahmpIO%EAHXY        (XSTART:XEND,YSTART:YEND) )            ! canopy air vapor pressure (pa)
    allocate ( NoahmpIO%TAHXY        (XSTART:XEND,YSTART:YEND) )            ! canopy air temperature (k)
    allocate ( NoahmpIO%CMXY         (XSTART:XEND,YSTART:YEND) )            ! bulk momentum drag coefficient
    allocate ( NoahmpIO%CHXY         (XSTART:XEND,YSTART:YEND) )            ! bulk sensible heat exchange coefficient
    allocate ( NoahmpIO%FWETXY       (XSTART:XEND,YSTART:YEND) )            ! wetted or snowed fraction of the canopy (-)
    allocate ( NoahmpIO%SNEQVOXY     (XSTART:XEND,YSTART:YEND) )            ! snow mass at last time step(mm h2o)
    allocate ( NoahmpIO%ALBOLDXY     (XSTART:XEND,YSTART:YEND) )            ! snow albedo at last time step (-)
    allocate ( NoahmpIO%QSNOWXY      (XSTART:XEND,YSTART:YEND) )            ! snowfall on the ground [mm/s]
    allocate ( NoahmpIO%QRAINXY      (XSTART:XEND,YSTART:YEND) )            ! rainfall on the ground [mm/s]
    allocate ( NoahmpIO%WSLAKEXY     (XSTART:XEND,YSTART:YEND) )            ! lake water storage [mm]
    allocate ( NoahmpIO%ZWTXY        (XSTART:XEND,YSTART:YEND) )            ! water table depth [m]
    allocate ( NoahmpIO%WAXY         (XSTART:XEND,YSTART:YEND) )            ! water in the "aquifer" [mm]
    allocate ( NoahmpIO%WTXY         (XSTART:XEND,YSTART:YEND) )            ! groundwater storage [mm]
    allocate ( NoahmpIO%SMCWTDXY     (XSTART:XEND,YSTART:YEND) )            ! soil moisture below the bottom of the column (m3m-3)
    allocate ( NoahmpIO%DEEPRECHXY   (XSTART:XEND,YSTART:YEND) )            ! recharge to the water table when deep (m)
    allocate ( NoahmpIO%RECHXY       (XSTART:XEND,YSTART:YEND) )            ! recharge to the water table (diagnostic) (m)

    allocate ( NoahmpIO%TSNOXY       (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND) )  ! snow temperature [K]
    allocate ( NoahmpIO%ZSNSOXY      (XSTART:XEND,-NSNOW+1:NSOIL,YSTART:YEND) )  ! snow layer depth [m]
    allocate ( NoahmpIO%SNICEXY      (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND) )  ! snow layer ice [mm]
    allocate ( NoahmpIO%SNLIQXY      (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND) )  ! snow layer liquid water [mm]
    
    allocate ( NoahmpIO%LFMASSXY     (XSTART:XEND,YSTART:YEND) )            ! leaf mass [g/m2]
    allocate ( NoahmpIO%RTMASSXY     (XSTART:XEND,YSTART:YEND) )            ! mass of fine roots [g/m2]
    allocate ( NoahmpIO%STMASSXY     (XSTART:XEND,YSTART:YEND) )            ! stem mass [g/m2]
    allocate ( NoahmpIO%WOODXY       (XSTART:XEND,YSTART:YEND) )            ! mass of wood (incl. woody roots) [g/m2]
    allocate ( NoahmpIO%GRAINXY      (XSTART:XEND,YSTART:YEND) )            ! mass of grain XING [g/m2]
    allocate ( NoahmpIO%GDDXY        (XSTART:XEND,YSTART:YEND) )            ! growing degree days XING FOUR
    allocate ( NoahmpIO%STBLCPXY     (XSTART:XEND,YSTART:YEND) )            ! stable carbon in deep soil [g/m2]
    allocate ( NoahmpIO%FASTCPXY     (XSTART:XEND,YSTART:YEND) )            ! short-lived carbon, shallow soil [g/m2]
    allocate ( NoahmpIO%LAI          (XSTART:XEND,YSTART:YEND) )            ! leaf area index
    allocate ( NoahmpIO%LAI_tmp      (XSTART:XEND,YSTART:YEND) )            ! leaf area index
    allocate ( NoahmpIO%XSAIXY       (XSTART:XEND,YSTART:YEND) )            ! stem area index
    allocate ( NoahmpIO%TAUSSXY      (XSTART:XEND,YSTART:YEND) )            ! snow age factor

! irrigation
    allocate ( NoahmpIO%IRFRACT      (XSTART:XEND,YSTART:YEND) )            ! irrigation fraction
    allocate ( NoahmpIO%SIFRACT      (XSTART:XEND,YSTART:YEND) )            ! sprinkler irrigation fraction
    allocate ( NoahmpIO%MIFRACT      (XSTART:XEND,YSTART:YEND) )            ! micro irrigation fraction
    allocate ( NoahmpIO%FIFRACT      (XSTART:XEND,YSTART:YEND) )            ! flood irrigation fraction   
    allocate ( NoahmpIO%IRNUMSI      (XSTART:XEND,YSTART:YEND) )            ! irrigation event number, Sprinkler
    allocate ( NoahmpIO%IRNUMMI      (XSTART:XEND,YSTART:YEND) )            ! irrigation event number, Micro
    allocate ( NoahmpIO%IRNUMFI      (XSTART:XEND,YSTART:YEND) )            ! irrigation event number, Flood 
    allocate ( NoahmpIO%IRWATSI      (XSTART:XEND,YSTART:YEND) )            ! irrigation water amount [m] to be applied, Sprinkler
    allocate ( NoahmpIO%IRWATMI      (XSTART:XEND,YSTART:YEND) )            ! irrigation water amount [m] to be applied, Micro
    allocate ( NoahmpIO%IRWATFI      (XSTART:XEND,YSTART:YEND) )            ! irrigation water amount [m] to be applied, Flood
    allocate ( NoahmpIO%IRELOSS      (XSTART:XEND,YSTART:YEND) )            ! loss of irrigation water to evaporation,sprinkler [mm]
    allocate ( NoahmpIO%IRSIVOL      (XSTART:XEND,YSTART:YEND) )            ! amount of irrigation by sprinkler (mm)
    allocate ( NoahmpIO%IRMIVOL      (XSTART:XEND,YSTART:YEND) )            ! amount of irrigation by micro (mm)
    allocate ( NoahmpIO%IRFIVOL      (XSTART:XEND,YSTART:YEND) )            ! amount of irrigation by micro (mm)
    allocate ( NoahmpIO%IRRSPLH      (XSTART:XEND,YSTART:YEND) )            ! latent heating from sprinkler evaporation (w/m2)
    allocate ( NoahmpIO%LOCTIM       (XSTART:XEND,YSTART:YEND) )            ! local time
  
! OUT (with no Noah LSM equivalent) (as defined in WRF)   
    allocate ( NoahmpIO%T2MVXY       (XSTART:XEND,YSTART:YEND) )            ! 2m temperature of vegetation part
    allocate ( NoahmpIO%T2MBXY       (XSTART:XEND,YSTART:YEND) )            ! 2m temperature of bare ground part
    allocate ( NoahmpIO%Q2MVXY       (XSTART:XEND,YSTART:YEND) )            ! 2m mixing ratio of vegetation part
    allocate ( NoahmpIO%Q2MBXY       (XSTART:XEND,YSTART:YEND) )            ! 2m mixing ratio of bare ground part
    allocate ( NoahmpIO%TRADXY       (XSTART:XEND,YSTART:YEND) )            ! surface radiative temperature (k)
    allocate ( NoahmpIO%NEEXY        (XSTART:XEND,YSTART:YEND) )            ! net ecosys exchange (g/m2/s CO2)
    allocate ( NoahmpIO%GPPXY        (XSTART:XEND,YSTART:YEND) )            ! gross primary assimilation [g/m2/s C]
    allocate ( NoahmpIO%NPPXY        (XSTART:XEND,YSTART:YEND) )            ! net primary productivity [g/m2/s C]
    allocate ( NoahmpIO%FVEGXY       (XSTART:XEND,YSTART:YEND) )            ! Noah-MP vegetation fraction [-]
    allocate ( NoahmpIO%RUNSFXY      (XSTART:XEND,YSTART:YEND) )            ! surface runoff [mm/s]
    allocate ( NoahmpIO%RUNSBXY      (XSTART:XEND,YSTART:YEND) )            ! subsurface runoff [mm/s]
    allocate ( NoahmpIO%ECANXY       (XSTART:XEND,YSTART:YEND) )            ! evaporation of intercepted water (mm/s)
    allocate ( NoahmpIO%EDIRXY       (XSTART:XEND,YSTART:YEND) )            ! soil surface evaporation rate (mm/s]
    allocate ( NoahmpIO%ETRANXY      (XSTART:XEND,YSTART:YEND) )            ! transpiration rate (mm/s)
    allocate ( NoahmpIO%FSAXY        (XSTART:XEND,YSTART:YEND) )            ! total absorbed solar radiation (w/m2)
    allocate ( NoahmpIO%FIRAXY       (XSTART:XEND,YSTART:YEND) )            ! total net longwave rad (w/m2) [+ to atm]
    allocate ( NoahmpIO%APARXY       (XSTART:XEND,YSTART:YEND) )            ! photosyn active energy by canopy (w/m2)
    allocate ( NoahmpIO%PSNXY        (XSTART:XEND,YSTART:YEND) )            ! total photosynthesis (umol co2/m2/s) [+]
    allocate ( NoahmpIO%SAVXY        (XSTART:XEND,YSTART:YEND) )            ! solar rad absorbed by veg. (w/m2)
    allocate ( NoahmpIO%SAGXY        (XSTART:XEND,YSTART:YEND) )            ! solar rad absorbed by ground (w/m2)
    allocate ( NoahmpIO%RSSUNXY      (XSTART:XEND,YSTART:YEND) )            ! sunlit leaf stomatal resistance (s/m)
    allocate ( NoahmpIO%RSSHAXY      (XSTART:XEND,YSTART:YEND) )            ! shaded leaf stomatal resistance (s/m)
    allocate ( NoahmpIO%BGAPXY       (XSTART:XEND,YSTART:YEND) )            ! between gap fraction
    allocate ( NoahmpIO%WGAPXY       (XSTART:XEND,YSTART:YEND) )            ! within gap fraction
    allocate ( NoahmpIO%TGVXY        (XSTART:XEND,YSTART:YEND) )            ! under canopy ground temperature[K]
    allocate ( NoahmpIO%TGBXY        (XSTART:XEND,YSTART:YEND) )            ! bare ground temperature [K]
    allocate ( NoahmpIO%CHVXY        (XSTART:XEND,YSTART:YEND) )            ! sensible heat exchange coefficient vegetated
    allocate ( NoahmpIO%CHBXY        (XSTART:XEND,YSTART:YEND) )            ! sensible heat exchange coefficient bare-ground
    allocate ( NoahmpIO%SHGXY        (XSTART:XEND,YSTART:YEND) )            ! veg ground sen. heat [w/m2]   [+ to atm]
    allocate ( NoahmpIO%SHCXY        (XSTART:XEND,YSTART:YEND) )            ! canopy sen. heat [w/m2]   [+ to atm]
    allocate ( NoahmpIO%SHBXY        (XSTART:XEND,YSTART:YEND) )            ! bare sensible heat [w/m2]  [+ to atm]
    allocate ( NoahmpIO%EVGXY        (XSTART:XEND,YSTART:YEND) )            ! veg ground evap. heat [w/m2]  [+ to atm]
    allocate ( NoahmpIO%EVBXY        (XSTART:XEND,YSTART:YEND) )            ! bare soil evaporation [w/m2]  [+ to atm]
    allocate ( NoahmpIO%GHVXY        (XSTART:XEND,YSTART:YEND) )            ! veg ground heat flux [w/m2]  [+ to soil]
    allocate ( NoahmpIO%GHBXY        (XSTART:XEND,YSTART:YEND) )            ! bare ground heat flux [w/m2] [+ to soil]
    allocate ( NoahmpIO%IRGXY        (XSTART:XEND,YSTART:YEND) )            ! veg ground net LW rad. [w/m2] [+ to atm]
    allocate ( NoahmpIO%IRCXY        (XSTART:XEND,YSTART:YEND) )            ! canopy net LW rad. [w/m2] [+ to atm]
    allocate ( NoahmpIO%IRBXY        (XSTART:XEND,YSTART:YEND) )            ! bare net longwave rad. [w/m2] [+ to atm]
    allocate ( NoahmpIO%TRXY         (XSTART:XEND,YSTART:YEND) )            ! transpiration [w/m2]  [+ to atm]
    allocate ( NoahmpIO%EVCXY        (XSTART:XEND,YSTART:YEND) )            ! canopy evaporation heat [w/m2]  [+ to atm]
    allocate ( NoahmpIO%CHLEAFXY     (XSTART:XEND,YSTART:YEND) )            ! leaf exchange coefficient 
    allocate ( NoahmpIO%CHUCXY       (XSTART:XEND,YSTART:YEND) )            ! under canopy exchange coefficient 
    allocate ( NoahmpIO%CHV2XY       (XSTART:XEND,YSTART:YEND) )            ! veg 2m exchange coefficient 
    allocate ( NoahmpIO%CHB2XY       (XSTART:XEND,YSTART:YEND) )            ! bare 2m exchange coefficient 
    allocate ( NoahmpIO%RS           (XSTART:XEND,YSTART:YEND) )            ! Total stomatal resistance (s/m)
    allocate ( NoahmpIO%Z0           (XSTART:XEND,YSTART:YEND) )            ! roughness length output to WRF 
    allocate ( NoahmpIO%ZNT          (XSTART:XEND,YSTART:YEND) )            ! roughness length output to WRF 
    allocate ( NoahmpIO%QTDRAIN      (XSTART:XEND,YSTART:YEND) )            ! tile drainage (mm)
    allocate ( NoahmpIO%TD_FRACTION  (XSTART:XEND,YSTART:YEND) )            ! tile drainage fraction
    allocate ( NoahmpIO%XLONG        (XSTART:XEND,YSTART:YEND) )            ! longitude
    allocate ( NoahmpIO%TERRAIN      (XSTART:XEND,YSTART:YEND) )            ! terrain height
    allocate ( NoahmpIO%GVFMIN       (XSTART:XEND,YSTART:YEND) )            ! annual minimum in vegetation fraction
    allocate ( NoahmpIO%GVFMAX       (XSTART:XEND,YSTART:YEND) )            ! annual maximum in vegetation fraction

! additional output variables
    allocate ( NoahmpIO%PAHXY        (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%PAHGXY       (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%PAHBXY       (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%PAHVXY       (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QINTSXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QINTRXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QDRIPSXY     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QDRIPRXY     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QTHROSXY     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QTHRORXY     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QSNSUBXY     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QSNFROXY     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QSUBCXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QFROCXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QEVACXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QDEWCXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QFRZCXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QMELTCXY     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QSNBOTXY     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%QMELTXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%PONDINGXY    (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%FPICEXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%RAINLSM      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%SNOWLSM      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%FORCTLSM     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%FORCQLSM     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%FORCPLSM     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%FORCZLSM     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%FORCWLSM     (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%ACC_SSOILXY  (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%ACC_QINSURXY (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%ACC_QSEVAXY  (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%ACC_ETRANIXY (XSTART:XEND,1:NSOIL,YSTART:YEND) )
    allocate ( NoahmpIO%EFLXBXY      (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%SOILENERGY   (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%SNOWENERGY   (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%CANHSXY      (XSTART:XEND,YSTART:YEND) )            ! canopy heat storage change [W/m2]
    allocate ( NoahmpIO%ACC_DWATERXY (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%ACC_PRCPXY   (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%ACC_ECANXY   (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%ACC_ETRANXY  (XSTART:XEND,YSTART:YEND) )
    allocate ( NoahmpIO%ACC_EDIRXY   (XSTART:XEND,YSTART:YEND) )

!------------------------------------------------------------------------
! Needed for MMF_RUNOFF (IOPT_RUN = 5); not part of MP driver in WRF
!------------------------------------------------------------------------

    allocate ( NoahmpIO%MSFTX        (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%MSFTY        (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%EQZWT        (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%RIVERBEDXY   (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%RIVERCONDXY  (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%PEXPXY       (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%FDEPTHXY     (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%AREAXY       (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%QRFSXY       (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%QSPRINGSXY   (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%QRFXY        (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%QSPRINGXY    (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%QSLATXY      (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%QLATXY       (XSTART:XEND,YSTART:YEND) )  !
    allocate ( NoahmpIO%RECHCLIM     (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%RIVERMASK    (XSTART:XEND,YSTART:YEND) )  ! 
    allocate ( NoahmpIO%NONRIVERXY   (XSTART:XEND,YSTART:YEND) )  ! 

!------------------------------------------------------------------------
! Needed for crop model (OPT_CROP=1)
!------------------------------------------------------------------------

    allocate ( NoahmpIO%PGSXY        (XSTART:XEND,  YSTART:YEND) )
    allocate ( NoahmpIO%CROPCAT      (XSTART:XEND,  YSTART:YEND) )
    allocate ( NoahmpIO%PLANTING     (XSTART:XEND,  YSTART:YEND) )
    allocate ( NoahmpIO%HARVEST      (XSTART:XEND,  YSTART:YEND) )
    allocate ( NoahmpIO%SEASON_GDD   (XSTART:XEND,  YSTART:YEND) )
    allocate ( NoahmpIO%CROPTYPE     (XSTART:XEND,5,YSTART:YEND) )

!------------------------------------------------------------------------
! Single- and Multi-layer Urban Models
!------------------------------------------------------------------------

    if(NoahmpIO%SF_URBAN_PHYSICS > 0 )  then  ! any urban model

       allocate ( NoahmpIO%sh_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%lh_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%g_urb2d        (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%rn_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%ts_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%HRANG          (XSTART:XEND,                 YSTART:YEND) )  !
       allocate ( NoahmpIO%DECLIN                                                    )  !
       allocate ( NoahmpIO%GMT                                                       )  !
       allocate ( NoahmpIO%JULDAY                                                    )  !
       allocate ( NoahmpIO%frc_urb2d      (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%utype_urb2d    (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%lp_urb2d       (XSTART:XEND,                 YSTART:YEND) )  !
       allocate ( NoahmpIO%lb_urb2d       (XSTART:XEND,                 YSTART:YEND) )  !
       allocate ( NoahmpIO%hgt_urb2d      (XSTART:XEND,                 YSTART:YEND) )  !
       allocate ( NoahmpIO%ust            (XSTART:XEND,                 YSTART:YEND) )  !

       !ENDIF
         
       !IF(SF_URBAN_PHYSICS == 1 ) THEN  ! single layer urban model
         
       allocate ( NoahmpIO%cmr_sfcdif     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%chr_sfcdif     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%cmc_sfcdif     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%chc_sfcdif     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%cmgr_sfcdif    (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%chgr_sfcdif    (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%tr_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%tb_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%tg_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%tc_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%qc_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%uc_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%xxxr_urb2d     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%xxxb_urb2d     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%xxxg_urb2d     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%xxxc_urb2d     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%trl_urb3d      (XSTART:XEND, nsoil,          YSTART:YEND) )  ! 
       allocate ( NoahmpIO%tbl_urb3d      (XSTART:XEND, nsoil,          YSTART:YEND) )  ! 
       allocate ( NoahmpIO%tgl_urb3d      (XSTART:XEND, nsoil,          YSTART:YEND) )  ! 

       allocate ( NoahmpIO%psim_urb2d     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%psih_urb2d     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%u10_urb2d      (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%v10_urb2d      (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%GZ1OZ0_urb2d   (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%AKMS_URB2D     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%th2_urb2d      (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%q2_urb2d       (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%ust_urb2d      (XSTART:XEND,                 YSTART:YEND) )  ! 

       allocate ( NoahmpIO%dzr            (             nsoil                      ) )  !
       allocate ( NoahmpIO%dzb            (             nsoil                      ) )  !
       allocate ( NoahmpIO%dzg            (             nsoil                      ) )  !
       allocate ( NoahmpIO%cmcr_urb2d     (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%tgr_urb2d      (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%tgrl_urb3d     (XSTART:XEND, nsoil,          YSTART:YEND) )  ! 
       allocate ( NoahmpIO%smr_urb3d      (XSTART:XEND, nsoil,          YSTART:YEND) )  ! 
       allocate ( NoahmpIO%drelr_urb2d    (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%drelb_urb2d    (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%drelg_urb2d    (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%flxhumr_urb2d  (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%flxhumb_urb2d  (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%flxhumg_urb2d  (XSTART:XEND,                 YSTART:YEND) )  ! 

       allocate ( NoahmpIO%chs            (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%chs2           (XSTART:XEND,                 YSTART:YEND) )  ! 
       allocate ( NoahmpIO%cqs2           (XSTART:XEND,                 YSTART:YEND) )  ! 

       allocate ( NoahmpIO%mh_urb2d       (XSTART:XEND,                 YSTART:YEND) )  !
       allocate ( NoahmpIO%stdh_urb2d     (XSTART:XEND,                 YSTART:YEND) )  !
       allocate ( NoahmpIO%lf_urb2d       (XSTART:XEND, 4,              YSTART:YEND) )  !

       !ENDIF

       !IF(SF_URBAN_PHYSICS == 2 .or. SF_URBAN_PHYSICS == 3) THEN  ! BEP or BEM urban models
         
       allocate ( NoahmpIO%trb_urb4d      (XSTART:XEND,NoahmpIO%urban_map_zrd,YSTART:YEND) )  !
       allocate ( NoahmpIO%tw1_urb4d      (XSTART:XEND,NoahmpIO%urban_map_zwd,YSTART:YEND) )  !
       allocate ( NoahmpIO%tw2_urb4d      (XSTART:XEND,NoahmpIO%urban_map_zwd,YSTART:YEND) )  !
       allocate ( NoahmpIO%tgb_urb4d      (XSTART:XEND,NoahmpIO%urban_map_gd ,YSTART:YEND) )  !
       allocate ( NoahmpIO%sfw1_urb3d     (XSTART:XEND,NoahmpIO%urban_map_zd ,YSTART:YEND) )  !
       allocate ( NoahmpIO%sfw2_urb3d     (XSTART:XEND,NoahmpIO%urban_map_zd ,YSTART:YEND) )  !
       allocate ( NoahmpIO%sfr_urb3d      (XSTART:XEND,NoahmpIO%urban_map_zdf,YSTART:YEND) )  !
       allocate ( NoahmpIO%sfg_urb3d      (XSTART:XEND,NoahmpIO%num_urban_ndm,YSTART:YEND) )  !
       allocate ( NoahmpIO%hi_urb2d       (XSTART:XEND,NoahmpIO%num_urban_hi, YSTART:YEND) )  !

       allocate ( NoahmpIO%theta_urban    (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%    u_urban    (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%    v_urban    (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%   dz_urban    (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%  rho_urban    (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%    p_urban    (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !

       allocate ( NoahmpIO%a_u_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%a_v_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%a_t_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%a_q_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%a_e_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%b_u_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%b_v_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%b_t_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%b_q_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%b_e_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%dlg_bep        (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%dl_u_bep       (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%sf_bep         (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !
       allocate ( NoahmpIO%vl_bep         (XSTART:XEND,KDS:KDE,         YSTART:YEND) )  !

       !ENDIF

        !IF(SF_URBAN_PHYSICS == 3) THEN  ! BEM urban model
         
       allocate ( NoahmpIO%tlev_urb3d     (XSTART:XEND,NoahmpIO%urban_map_bd  ,YSTART:YEND) )  !
       allocate ( NoahmpIO%qlev_urb3d     (XSTART:XEND,NoahmpIO%urban_map_bd  ,YSTART:YEND) )  !
       allocate ( NoahmpIO%tw1lev_urb3d   (XSTART:XEND,NoahmpIO%urban_map_wd  ,YSTART:YEND) )  !
       allocate ( NoahmpIO%tw2lev_urb3d   (XSTART:XEND,NoahmpIO%urban_map_wd  ,YSTART:YEND) )  !
       allocate ( NoahmpIO%tglev_urb3d    (XSTART:XEND,NoahmpIO%urban_map_gbd ,YSTART:YEND) )  !
       allocate ( NoahmpIO%tflev_urb3d    (XSTART:XEND,NoahmpIO%urban_map_fbd ,YSTART:YEND) )  !
       allocate ( NoahmpIO%sf_ac_urb3d    (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%lf_ac_urb3d    (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%cm_ac_urb3d    (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%sfvent_urb3d   (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%lfvent_urb3d   (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%sfwin1_urb3d   (XSTART:XEND,NoahmpIO%urban_map_wd  ,YSTART:YEND) )  !
       allocate ( NoahmpIO%sfwin2_urb3d   (XSTART:XEND,NoahmpIO%urban_map_wd  ,YSTART:YEND) )  !
       ! new urban variables greenroof & solar panel for BEM
       allocate ( NoahmpIO%ep_pv_urb3d    (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%t_pv_urb3d     (XSTART:XEND,NoahmpIO%urban_map_zdf ,YSTART:YEND) )  !
       allocate ( NoahmpIO%trv_urb4d      (XSTART:XEND,NoahmpIO%urban_map_zgrd,YSTART:YEND) ) !
       allocate ( NoahmpIO%qr_urb4d       (XSTART:XEND,NoahmpIO%urban_map_zgrd,YSTART:YEND) ) !
       allocate ( NoahmpIO%qgr_urb3d      (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%tgr_urb3d      (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%drain_urb4d    (XSTART:XEND,NoahmpIO%urban_map_zdf ,YSTART:YEND) )  !
       allocate ( NoahmpIO%draingr_urb3d  (XSTART:XEND,                        YSTART:YEND) )  !
       allocate ( NoahmpIO%sfrv_urb3d     (XSTART:XEND,NoahmpIO%urban_map_zdf ,YSTART:YEND) )  !
       allocate ( NoahmpIO%lfrv_urb3d     (XSTART:XEND,NoahmpIO%urban_map_zdf ,YSTART:YEND) )  !
       allocate ( NoahmpIO%dgr_urb3d      (XSTART:XEND,NoahmpIO%urban_map_zdf ,YSTART:YEND) )  !
       allocate ( NoahmpIO%dg_urb3d       (XSTART:XEND,NoahmpIO%num_urban_ndm ,YSTART:YEND) )  !
       allocate ( NoahmpIO%lfr_urb3d      (XSTART:XEND,NoahmpIO%urban_map_zdf ,YSTART:YEND) )  !
       allocate ( NoahmpIO%lfg_urb3d      (XSTART:XEND,NoahmpIO%num_urban_ndm ,YSTART:YEND) )  !

    endif

!------------------------------------------------------------------------

    allocate ( NoahmpIO%CHSTARXY  (XSTART:XEND,YSTART:YEND) )  ! for consistency with MP_init; delete later
    allocate ( NoahmpIO%SEAICE    (XSTART:XEND,YSTART:YEND) )  ! seaice fraction
    
#ifdef WRF_HYDRO
    allocate (NoahmpIO%infxsrt    (XSTART:XEND,YSTART:YEND) )
    allocate (NoahmpIO%sfcheadrt  (XSTART:XEND,YSTART:YEND) )
    allocate (NoahmpIO%soldrain   (XSTART:XEND,YSTART:YEND) )
    allocate (NoahmpIO%qtiledrain (XSTART:XEND,YSTART:YEND) )
    allocate (NoahmpIO%ZWATBLE2D  (XSTART:XEND,YSTART:YEND) )
#endif    

    end associate  
    
    !-------------------------------------------------------------------
    ! Initialize variables with default values 
    !-------------------------------------------------------------------
    
    NoahmpIO%ICE             = 0
    NoahmpIO%COSZEN          = undefined_real
    NoahmpIO%XLAT            = undefined_real
    NoahmpIO%DZ8W            = undefined_real
    NoahmpIO%DZS             = undefined_real
    NoahmpIO%ZSOIL           = undefined_real
    NoahmpIO%IVGTYP          = undefined_int
    NoahmpIO%ISLTYP          = undefined_int
    NoahmpIO%SOILCL1         = undefined_real
    NoahmpIO%SOILCL2         = undefined_real
    NoahmpIO%SOILCL3         = undefined_real
    NoahmpIO%SOILCL4         = undefined_real
    NoahmpIO%SOILCOMP        = undefined_real
    NoahmpIO%VEGFRA          = undefined_real
    NoahmpIO%TMN             = undefined_real
    NoahmpIO%XLAND           = undefined_real
    NoahmpIO%XICE            = undefined_real
    NoahmpIO%T_PHY           = undefined_real
    NoahmpIO%QV_CURR         = undefined_real
    NoahmpIO%U_PHY           = undefined_real
    NoahmpIO%V_PHY           = undefined_real
    NoahmpIO%SWDOWN          = undefined_real
    NoahmpIO%SWDDIR          = undefined_real
    NoahmpIO%SWDDIF          = undefined_real
    NoahmpIO%GLW             = undefined_real
    NoahmpIO%P8W             = undefined_real
    NoahmpIO%RAINBL          = undefined_real
    NoahmpIO%SNOWBL          = undefined_real
    NoahmpIO%RAINBL_tmp      = undefined_real
    NoahmpIO%SR              = undefined_real
    NoahmpIO%RAINCV          = undefined_real
    NoahmpIO%RAINNCV         = undefined_real
    NoahmpIO%RAINSHV         = undefined_real
    NoahmpIO%SNOWNCV         = undefined_real
    NoahmpIO%GRAUPELNCV      = undefined_real
    NoahmpIO%HAILNCV         = undefined_real
    NoahmpIO%TSK             = undefined_real
    NoahmpIO%QFX             = undefined_real
    NoahmpIO%SMSTAV          = undefined_real
    NoahmpIO%SMSTOT          = undefined_real
    NoahmpIO%SMOIS           = undefined_real
    NoahmpIO%SH2O            = undefined_real
    NoahmpIO%TSLB            = undefined_real
    NoahmpIO%SNOW            = undefined_real
    NoahmpIO%SNOWH           = undefined_real
    NoahmpIO%CANWAT          = undefined_real
    NoahmpIO%ACSNOM          = 0.0
    NoahmpIO%ACSNOW          = 0.0
    NoahmpIO%QSFC            = undefined_real
    NoahmpIO%SFCRUNOFF       = 0.0
    NoahmpIO%UDRUNOFF        = 0.0
    NoahmpIO%SMOISEQ         = undefined_real
    NoahmpIO%ALBEDO          = undefined_real
    NoahmpIO%ISNOWXY         = undefined_int
    NoahmpIO%TVXY            = undefined_real
    NoahmpIO%TGXY            = undefined_real
    NoahmpIO%CANICEXY        = undefined_real
    NoahmpIO%CANLIQXY        = undefined_real
    NoahmpIO%EAHXY           = undefined_real
    NoahmpIO%TAHXY           = undefined_real
    NoahmpIO%CMXY            = undefined_real
    NoahmpIO%CHXY            = undefined_real
    NoahmpIO%FWETXY          = undefined_real
    NoahmpIO%SNEQVOXY        = undefined_real
    NoahmpIO%ALBOLDXY        = undefined_real
    NoahmpIO%QSNOWXY         = undefined_real
    NoahmpIO%QRAINXY         = undefined_real
    NoahmpIO%WSLAKEXY        = undefined_real
    NoahmpIO%ZWTXY           = undefined_real
    NoahmpIO%WAXY            = undefined_real
    NoahmpIO%WTXY            = undefined_real
    NoahmpIO%TSNOXY          = undefined_real
    NoahmpIO%SNICEXY         = undefined_real
    NoahmpIO%SNLIQXY         = undefined_real
    NoahmpIO%LFMASSXY        = undefined_real
    NoahmpIO%RTMASSXY        = undefined_real
    NoahmpIO%STMASSXY        = undefined_real
    NoahmpIO%WOODXY          = undefined_real
    NoahmpIO%STBLCPXY        = undefined_real
    NoahmpIO%FASTCPXY        = undefined_real
    NoahmpIO%LAI             = undefined_real
    NoahmpIO%LAI_tmp         = undefined_real
    NoahmpIO%XSAIXY          = undefined_real
    NoahmpIO%TAUSSXY         = undefined_real
    NoahmpIO%XLONG           = undefined_real
    NoahmpIO%SEAICE          = undefined_real
    NoahmpIO%SMCWTDXY        = undefined_real
    NoahmpIO%DEEPRECHXY      = 0.0
    NoahmpIO%RECHXY          = 0.0
    NoahmpIO%ZSNSOXY         = undefined_real
    NoahmpIO%GRDFLX          = undefined_real
    NoahmpIO%HFX             = undefined_real
    NoahmpIO%LH              = undefined_real
    NoahmpIO%EMISS           = undefined_real
    NoahmpIO%SNOWC           = undefined_real
    NoahmpIO%T2MVXY          = undefined_real
    NoahmpIO%T2MBXY          = undefined_real
    NoahmpIO%Q2MVXY          = undefined_real
    NoahmpIO%Q2MBXY          = undefined_real
    NoahmpIO%TRADXY          = undefined_real
    NoahmpIO%NEEXY           = undefined_real
    NoahmpIO%GPPXY           = undefined_real
    NoahmpIO%NPPXY           = undefined_real
    NoahmpIO%FVEGXY          = undefined_real
    NoahmpIO%RUNSFXY         = undefined_real
    NoahmpIO%RUNSBXY         = undefined_real
    NoahmpIO%ECANXY          = undefined_real
    NoahmpIO%EDIRXY          = undefined_real
    NoahmpIO%ETRANXY         = undefined_real
    NoahmpIO%FSAXY           = undefined_real
    NoahmpIO%FIRAXY          = undefined_real
    NoahmpIO%APARXY          = undefined_real
    NoahmpIO%PSNXY           = undefined_real
    NoahmpIO%SAVXY           = undefined_real
    NoahmpIO%SAGXY           = undefined_real
    NoahmpIO%RSSUNXY         = undefined_real
    NoahmpIO%RSSHAXY         = undefined_real
    NoahmpIO%BGAPXY          = undefined_real
    NoahmpIO%WGAPXY          = undefined_real
    NoahmpIO%TGVXY           = undefined_real
    NoahmpIO%TGBXY           = undefined_real
    NoahmpIO%CHVXY           = undefined_real
    NoahmpIO%CHBXY           = undefined_real
    NoahmpIO%SHGXY           = undefined_real
    NoahmpIO%SHCXY           = undefined_real
    NoahmpIO%SHBXY           = undefined_real
    NoahmpIO%EVGXY           = undefined_real
    NoahmpIO%EVBXY           = undefined_real
    NoahmpIO%GHVXY           = undefined_real
    NoahmpIO%GHBXY           = undefined_real
    NoahmpIO%IRGXY           = undefined_real
    NoahmpIO%IRCXY           = undefined_real
    NoahmpIO%IRBXY           = undefined_real
    NoahmpIO%TRXY            = undefined_real
    NoahmpIO%EVCXY           = undefined_real
    NoahmpIO%CHLEAFXY        = undefined_real
    NoahmpIO%CHUCXY          = undefined_real
    NoahmpIO%CHV2XY          = undefined_real
    NoahmpIO%CHB2XY          = undefined_real
    NoahmpIO%RS              = undefined_real
    NoahmpIO%CANHSXY         = undefined_real
    ! additional output
    NoahmpIO%PAHXY           = undefined_real
    NoahmpIO%PAHGXY          = undefined_real
    NoahmpIO%PAHBXY          = undefined_real
    NoahmpIO%PAHVXY          = undefined_real
    NoahmpIO%QINTSXY         = undefined_real
    NoahmpIO%QINTRXY         = undefined_real
    NoahmpIO%QDRIPSXY        = undefined_real
    NoahmpIO%QDRIPRXY        = undefined_real
    NoahmpIO%QTHROSXY        = undefined_real
    NoahmpIO%QTHRORXY        = undefined_real
    NoahmpIO%QSNSUBXY        = undefined_real
    NoahmpIO%QSNFROXY        = undefined_real
    NoahmpIO%QSUBCXY         = undefined_real
    NoahmpIO%QFROCXY         = undefined_real
    NoahmpIO%QEVACXY         = undefined_real
    NoahmpIO%QDEWCXY         = undefined_real
    NoahmpIO%QFRZCXY         = undefined_real
    NoahmpIO%QMELTCXY        = undefined_real
    NoahmpIO%QSNBOTXY        = undefined_real
    NoahmpIO%QMELTXY         = undefined_real
    NoahmpIO%PONDINGXY       = 0.0
    NoahmpIO%FPICEXY         = undefined_real
    NoahmpIO%RAINLSM         = undefined_real
    NoahmpIO%SNOWLSM         = undefined_real
    NoahmpIO%FORCTLSM        = undefined_real
    NoahmpIO%FORCQLSM        = undefined_real
    NoahmpIO%FORCPLSM        = undefined_real
    NoahmpIO%FORCZLSM        = undefined_real
    NoahmpIO%FORCWLSM        = undefined_real
    NoahmpIO%ACC_SSOILXY     = 0.0
    NoahmpIO%ACC_QINSURXY    = 0.0
    NoahmpIO%ACC_QSEVAXY     = 0.0
    NoahmpIO%ACC_ETRANIXY    = 0.0
    NoahmpIO%EFLXBXY         = undefined_real
    NoahmpIO%SOILENERGY      = 0.0
    NoahmpIO%SNOWENERGY      = 0.0
    NoahmpIO%ACC_DWATERXY    = 0.0
    NoahmpIO%ACC_PRCPXY      = 0.0
    NoahmpIO%ACC_ECANXY      = 0.0
    NoahmpIO%ACC_ETRANXY     = 0.0
    NoahmpIO%ACC_EDIRXY      = 0.0

    NoahmpIO%TERRAIN         = undefined_real
    NoahmpIO%GVFMIN          = undefined_real
    NoahmpIO%GVFMAX          = undefined_real
    NoahmpIO%MSFTX           = undefined_real
    NoahmpIO%MSFTY           = undefined_real
    NoahmpIO%EQZWT           = undefined_real
    NoahmpIO%RIVERBEDXY      = undefined_real
    NoahmpIO%RIVERCONDXY     = undefined_real
    NoahmpIO%PEXPXY          = undefined_real
    NoahmpIO%FDEPTHXY        = undefined_real
    NoahmpIO%AREAXY          = undefined_real
    NoahmpIO%QRFSXY          = undefined_real
    NoahmpIO%QSPRINGSXY      = undefined_real
    NoahmpIO%QRFXY           = undefined_real
    NoahmpIO%QSPRINGXY       = undefined_real
    NoahmpIO%QSLATXY         = undefined_real
    NoahmpIO%QLATXY          = undefined_real
    NoahmpIO%CHSTARXY        = undefined_real
    NoahmpIO%Z0              = undefined_real
    NoahmpIO%ZNT             = undefined_real
    NoahmpIO%PGSXY           = undefined_int
    NoahmpIO%CROPCAT         = undefined_int
    NoahmpIO%PLANTING        = undefined_real
    NoahmpIO%HARVEST         = undefined_real
    NoahmpIO%SEASON_GDD      = undefined_real
    NoahmpIO%CROPTYPE        = undefined_real

    ! tile drainage
    NoahmpIO%QTDRAIN         = 0.0
    NoahmpIO%TD_FRACTION     = undefined_real

    ! irrigation
    NoahmpIO%IRFRACT         = 0.0
    NoahmpIO%SIFRACT         = 0.0
    NoahmpIO%MIFRACT         = 0.0
    NoahmpIO%FIFRACT         = 0.0
    NoahmpIO%IRNUMSI         = 0
    NoahmpIO%IRNUMMI         = 0
    NoahmpIO%IRNUMFI         = 0
    NoahmpIO%IRWATSI         = 0.0
    NoahmpIO%IRWATMI         = 0.0
    NoahmpIO%IRWATFI         = 0.0
    NoahmpIO%IRELOSS         = 0.0
    NoahmpIO%IRSIVOL         = 0.0
    NoahmpIO%IRMIVOL         = 0.0
    NoahmpIO%IRFIVOL         = 0.0
    NoahmpIO%IRRSPLH         = 0.0
    NoahmpIO%LOCTIM          = undefined_real
 
    if (NoahmpIO%SF_URBAN_PHYSICS > 0 )then  ! any urban model
      NoahmpIO%HRANG         = undefined_real
      NoahmpIO%DECLIN        = undefined_real
      NoahmpIO%sh_urb2d      = undefined_real
      NoahmpIO%lh_urb2d      = undefined_real
      NoahmpIO%g_urb2d       = undefined_real
      NoahmpIO%rn_urb2d      = undefined_real
      NoahmpIO%ts_urb2d      = undefined_real
      NoahmpIO%GMT           = undefined_real
      NoahmpIO%JULDAY        = undefined_int
      NoahmpIO%IRI_URBAN     = undefined_int
      NoahmpIO%frc_urb2d     = undefined_real
      NoahmpIO%utype_urb2d   = undefined_int
      NoahmpIO%lp_urb2d      = undefined_real
      NoahmpIO%lb_urb2d      = undefined_real
      NoahmpIO%hgt_urb2d     = undefined_real
      NoahmpIO%ust           = undefined_real
      NoahmpIO%cmr_sfcdif    = undefined_real
      NoahmpIO%chr_sfcdif    = undefined_real
      NoahmpIO%cmc_sfcdif    = undefined_real
      NoahmpIO%chc_sfcdif    = undefined_real
      NoahmpIO%cmgr_sfcdif   = undefined_real
      NoahmpIO%chgr_sfcdif   = undefined_real
      NoahmpIO%tr_urb2d      = undefined_real
      NoahmpIO%tb_urb2d      = undefined_real
      NoahmpIO%tg_urb2d      = undefined_real
      NoahmpIO%tc_urb2d      = undefined_real
      NoahmpIO%qc_urb2d      = undefined_real
      NoahmpIO%uc_urb2d      = undefined_real
      NoahmpIO%xxxr_urb2d    = undefined_real
      NoahmpIO%xxxb_urb2d    = undefined_real
      NoahmpIO%xxxg_urb2d    = undefined_real
      NoahmpIO%xxxc_urb2d    = undefined_real
      NoahmpIO%trl_urb3d     = undefined_real
      NoahmpIO%tbl_urb3d     = undefined_real
      NoahmpIO%tgl_urb3d     = undefined_real
      NoahmpIO%psim_urb2d    = undefined_real
      NoahmpIO%psih_urb2d    = undefined_real
      NoahmpIO%u10_urb2d     = undefined_real
      NoahmpIO%v10_urb2d     = undefined_real
      NoahmpIO%GZ1OZ0_urb2d  = undefined_real
      NoahmpIO%AKMS_URB2D    = undefined_real
      NoahmpIO%th2_urb2d     = undefined_real
      NoahmpIO%q2_urb2d      = undefined_real
      NoahmpIO%ust_urb2d     = undefined_real
      NoahmpIO%dzr           = undefined_real
      NoahmpIO%dzb           = undefined_real
      NoahmpIO%dzg           = undefined_real
      NoahmpIO%cmcr_urb2d    = undefined_real
      NoahmpIO%tgr_urb2d     = undefined_real
      NoahmpIO%tgrl_urb3d    = undefined_real
      NoahmpIO%smr_urb3d     = undefined_real
      NoahmpIO%drelr_urb2d   = undefined_real
      NoahmpIO%drelb_urb2d   = undefined_real
      NoahmpIO%drelg_urb2d   = undefined_real
      NoahmpIO%flxhumr_urb2d = undefined_real
      NoahmpIO%flxhumb_urb2d = undefined_real
      NoahmpIO%flxhumg_urb2d = undefined_real
      NoahmpIO%chs           = undefined_real
      NoahmpIO%chs2          = undefined_real
      NoahmpIO%cqs2          = undefined_real
      NoahmpIO%mh_urb2d      = undefined_real
      NoahmpIO%stdh_urb2d    = undefined_real
      NoahmpIO%lf_urb2d      = undefined_real
      NoahmpIO%trb_urb4d     = undefined_real
      NoahmpIO%tw1_urb4d     = undefined_real
      NoahmpIO%tw2_urb4d     = undefined_real
      NoahmpIO%tgb_urb4d     = undefined_real
      NoahmpIO%sfw1_urb3d    = undefined_real
      NoahmpIO%sfw2_urb3d    = undefined_real
      NoahmpIO%sfr_urb3d     = undefined_real
      NoahmpIO%sfg_urb3d     = undefined_real
      NoahmpIO%hi_urb2d      = undefined_real
      NoahmpIO%theta_urban   = undefined_real
      NoahmpIO%u_urban       = undefined_real
      NoahmpIO%v_urban       = undefined_real
      NoahmpIO%dz_urban      = undefined_real
      NoahmpIO%rho_urban     = undefined_real
      NoahmpIO%p_urban       = undefined_real
      NoahmpIO%a_u_bep       = undefined_real
      NoahmpIO%a_v_bep       = undefined_real
      NoahmpIO%a_t_bep       = undefined_real
      NoahmpIO%a_q_bep       = undefined_real
      NoahmpIO%a_e_bep       = undefined_real
      NoahmpIO%b_u_bep       = undefined_real
      NoahmpIO%b_v_bep       = undefined_real
      NoahmpIO%b_t_bep       = undefined_real
      NoahmpIO%b_q_bep       = undefined_real
      NoahmpIO%b_e_bep       = undefined_real
      NoahmpIO%dlg_bep       = undefined_real
      NoahmpIO%dl_u_bep      = undefined_real
      NoahmpIO%sf_bep        = undefined_real
      NoahmpIO%vl_bep        = undefined_real
      NoahmpIO%tlev_urb3d    = undefined_real
      NoahmpIO%qlev_urb3d    = undefined_real
      NoahmpIO%tw1lev_urb3d  = undefined_real
      NoahmpIO%tw2lev_urb3d  = undefined_real
      NoahmpIO%tglev_urb3d   = undefined_real
      NoahmpIO%tflev_urb3d   = undefined_real
      NoahmpIO%sf_ac_urb3d   = undefined_real
      NoahmpIO%lf_ac_urb3d   = undefined_real
      NoahmpIO%cm_ac_urb3d   = undefined_real
      NoahmpIO%sfvent_urb3d  = undefined_real
      NoahmpIO%lfvent_urb3d  = undefined_real
      NoahmpIO%sfwin1_urb3d  = undefined_real
      NoahmpIO%sfwin2_urb3d  = undefined_real
      NoahmpIO%ep_pv_urb3d   = undefined_real
      NoahmpIO%t_pv_urb3d    = undefined_real
      NoahmpIO%trv_urb4d     = undefined_real
      NoahmpIO%qr_urb4d      = undefined_real
      NoahmpIO%qgr_urb3d     = undefined_real
      NoahmpIO%tgr_urb3d     = undefined_real
      NoahmpIO%drain_urb4d   = undefined_real
      NoahmpIO%draingr_urb3d = undefined_real
      NoahmpIO%sfrv_urb3d    = undefined_real
      NoahmpIO%lfrv_urb3d    = undefined_real
      NoahmpIO%dgr_urb3d     = undefined_real
      NoahmpIO%dg_urb3d      = undefined_real
      NoahmpIO%lfr_urb3d     = undefined_real
      NoahmpIO%lfg_urb3d     = undefined_real
    endif

    NoahmpIO%XLAND           = 1.0   ! water = 2.0, land = 1.0
    NoahmpIO%XICE            = 0.0   ! fraction of grid that is seaice
    NoahmpIO%XICE_THRESHOLD  = 0.5   ! fraction of grid determining seaice (from WRF)
    NoahmpIO%SLOPETYP        =  1

#ifdef WRF_HYDRO
    NoahmpIO%infxsrt         = 0.0
    NoahmpIO%sfcheadrt       = 0.0 
    NoahmpIO%soldrain        = 0.0
    NoahmpIO%qtiledrain      = 0.0
    NoahmpIO%ZWATBLE2D       = 0.0
#endif 
    
  end subroutine NoahmpIOVarInitDefault

end module NoahmpIOVarInitMod
