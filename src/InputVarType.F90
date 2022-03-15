module InputVarType

!!! Define Noah-MP Input variables (2D forcing, namelist, table, static)
!!! Input variable initialization is done in InputInit.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use Machine, only : kind_noahmp

  implicit none
  save
  private

  type, public :: input_type

    !----------------------------------------------------------------
    ! Noahmp Namelist variables
    !----------------------------------------------------------------

    ! domain
    integer                  :: ILOCIn    ! grid index (longitude)
    integer                  :: JLOCIn    ! grid index (latitude)
    real(kind=kind_noahmp)   :: DXIn      ! grid spacing (m)
    real(kind=kind_noahmp)   :: COSZIn
    ! timing
    integer                  :: maxtime
    real(kind=kind_noahmp)   :: DTIn
    character(len=256)       :: output_filename
    logical                  :: runsnow
    real(kind=kind_noahmp)   :: JULIANIn
    ! forcing
    integer                  :: rain_duration
    integer                  :: dry_duration
    logical                  :: raining
    real(kind=kind_noahmp)   :: rainrate
    real(kind=kind_noahmp)   :: UUIn
    real(kind=kind_noahmp)   :: VVIn
    real(kind=kind_noahmp)   :: SFCPRSIn
    real(kind=kind_noahmp)   :: Q2In
    real(kind=kind_noahmp)   :: SOLDNIn 
    real(kind=kind_noahmp)   :: LWDNIn
    real(kind=kind_noahmp)   :: PSFCIn
    real(kind=kind_noahmp)   :: TBOTIn
    real(kind=kind_noahmp)   :: SFCTMPIn
    real(kind=kind_noahmp)   :: PRCPCONVIn
    real(kind=kind_noahmp)   :: PRCPNONCIn
    real(kind=kind_noahmp)   :: PRCPSHCVIn
    real(kind=kind_noahmp)   :: PRCPSNOWIn
    real(kind=kind_noahmp)   :: PRCPGRPLIn
    real(kind=kind_noahmp)   :: PRCPHAILIn
    ! structure
    integer                  :: VEGTYPEIn
    integer                  :: SOILCOLORIn
    integer                  :: SLOPETYPEIn
    integer                  :: CROPTYPEIn
    integer                  :: NSOILIn
    integer                  :: NSNOWIn
    real(kind=kind_noahmp)   :: VEGFRAIn
    real(kind=kind_noahmp)   :: VEGMAXIn
    real(kind=kind_noahmp)   :: SHDMAXIn
    real(kind=kind_noahmp)   :: ZLVLIn    ! reference height
    ! state variables
    integer               , allocatable, dimension(:) :: SOILTYPEIn
    real(kind=kind_noahmp), allocatable, dimension(:) :: ZSOILIn 
    real(kind=kind_noahmp), allocatable, dimension(:) :: ZSNSOIn
    ! namelist options
    integer                  :: OPT_DVEGIn
    integer                  :: OPT_CRSIn
    integer                  :: OPT_BTRIn
    integer                  :: OPT_RUNSRFIn
    integer                  :: OPT_RUNSUBIn
    integer                  :: OPT_SFCIn
    integer                  :: OPT_FRZIn
    integer                  :: OPT_INFIn
    integer                  :: OPT_RADIn
    integer                  :: OPT_ALBIn
    integer                  :: OPT_SNFIn
    integer                  :: OPT_TBOTIn
    integer                  :: OPT_STCIn
    integer                  :: OPT_RSFIn
    integer                  :: OPT_SOILIn
    integer                  :: OPT_PEDOIn
    integer                  :: OPT_CROPIn
    integer                  :: OPT_IRRIn
    integer                  :: OPT_IRRMIn
    integer                  :: OPT_INFDVIn
    integer                  :: OPT_TDRNIn
    integer                  :: OPT_TKSNOIn

    !----------------------------------------------------------------
    ! Noahmp Table variables
    !----------------------------------------------------------------

    ! original MPTABLE.TBL vegetation parameters
    character(len=256)                                  :: VEG_DATASET_DESCRIPTION_TABLE
    integer                                             :: NVEG_TABLE
    integer                                             :: ISURBAN_TABLE
    integer                                             :: ISWATER_TABLE
    integer                                             :: ISBARREN_TABLE
    integer                                             :: ISICE_TABLE
    integer                                             :: ISCROP_TABLE
    integer                                             :: EBLFOREST_TABLE
    integer                                             :: NATURAL_TABLE
    integer                                             :: LCZ_1_TABLE
    integer                                             :: LCZ_2_TABLE
    integer                                             :: LCZ_3_TABLE
    integer                                             :: LCZ_4_TABLE
    integer                                             :: LCZ_5_TABLE
    integer                                             :: LCZ_6_TABLE
    integer                                             :: LCZ_7_TABLE
    integer                                             :: LCZ_8_TABLE
    integer                                             :: LCZ_9_TABLE
    integer                                             :: LCZ_10_TABLE
    integer                                             :: LCZ_11_TABLE
    real(kind=kind_noahmp), allocatable, dimension(:)   :: CH2OP_TABLE       ! maximum intercepted h2o per unit lai+sai (mm)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: DLEAF_TABLE       ! characteristic leaf dimension (m)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: Z0MVT_TABLE       ! momentum roughness length (m)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: HVT_TABLE         ! top of canopy (m)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: HVB_TABLE         ! bottom of canopy (m)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: DEN_TABLE         ! tree density (no. of trunks per m2)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: RC_TABLE          ! tree crown radius (m)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: MFSNO_TABLE       ! snowmelt curve parameter ()
    real(kind=kind_noahmp), allocatable, dimension(:)   :: SCFFAC_TABLE      ! snow cover factor (m) (replace original hard-coded 2.5*z0 in SCF formulation)
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: SAIM_TABLE        ! monthly stem area index, one-sided
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: LAIM_TABLE        ! monthly leaf area index, one-sided
    real(kind=kind_noahmp), allocatable, dimension(:)   :: SLA_TABLE         ! single-side leaf area per Kg [m2/kg]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: DILEFC_TABLE      ! coeficient for leaf stress death [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: DILEFW_TABLE      ! coeficient for leaf stress death [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: FRAGR_TABLE       ! fraction of growth respiration  !original was 0.3 
    real(kind=kind_noahmp), allocatable, dimension(:)   :: LTOVRC_TABLE      ! leaf turnover [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: C3PSN_TABLE       ! photosynthetic pathway: 0. = c4, 1. = c3
    real(kind=kind_noahmp), allocatable, dimension(:)   :: KC25_TABLE        ! co2 michaelis-menten constant at 25c (pa)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: AKC_TABLE         ! q10 for kc25
    real(kind=kind_noahmp), allocatable, dimension(:)   :: KO25_TABLE        ! o2 michaelis-menten constant at 25c (pa)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: AKO_TABLE         ! q10 for ko25
    real(kind=kind_noahmp), allocatable, dimension(:)   :: VCMX25_TABLE      ! maximum rate of carboxylation at 25c (umol co2/m**2/s)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: AVCMX_TABLE       ! q10 for vcmx25
    real(kind=kind_noahmp), allocatable, dimension(:)   :: BP_TABLE          ! minimum leaf conductance (umol/m**2/s)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: MP_TABLE          ! slope of conductance-to-photosynthesis relationship
    real(kind=kind_noahmp), allocatable, dimension(:)   :: QE25_TABLE        ! quantum efficiency at 25c (umol co2 / umol photon)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: AQE_TABLE         ! q10 for qe25
    real(kind=kind_noahmp), allocatable, dimension(:)   :: RMF25_TABLE       ! leaf maintenance respiration at 25c (umol co2/m**2/s)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: RMS25_TABLE       ! stem maintenance respiration at 25c (umol co2/kg bio/s)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: RMR25_TABLE       ! root maintenance respiration at 25c (umol co2/kg bio/s)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: ARM_TABLE         ! q10 for maintenance respiration
    real(kind=kind_noahmp), allocatable, dimension(:)   :: FOLNMX_TABLE      ! foliage nitrogen concentration when f(n)=1 (%)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: TMIN_TABLE        ! minimum temperature for photosynthesis (k)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: XL_TABLE          ! leaf/stem orientation index
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: RHOL_TABLE        ! leaf reflectance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: RHOS_TABLE        ! stem reflectance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: TAUL_TABLE        ! leaf transmittance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: TAUS_TABLE        ! stem transmittance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:)   :: MRP_TABLE         ! microbial respiration parameter (umol co2 /kg c/ s)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: CWPVT_TABLE       ! empirical canopy wind parameter
    real(kind=kind_noahmp), allocatable, dimension(:)   :: WRRAT_TABLE       ! wood to non-wood ratio
    real(kind=kind_noahmp), allocatable, dimension(:)   :: WDPOOL_TABLE      ! wood pool (switch 1 or 0) depending on woody or not [-]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: TDLEF_TABLE       ! characteristic T for leaf freezing [K]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: NROOT_TABLE       ! number of soil layers with root present
    real(kind=kind_noahmp), allocatable, dimension(:)   :: RGL_TABLE         ! Parameter used in radiation stress function
    real(kind=kind_noahmp), allocatable, dimension(:)   :: RS_TABLE          ! Minimum stomatal resistance [s m-1]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: HS_TABLE          ! Parameter used in vapor pressure deficit function
    real(kind=kind_noahmp), allocatable, dimension(:)   :: TOPT_TABLE        ! Optimum transpiration air temperature [K]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: RSMAX_TABLE       ! Maximal stomatal resistance [s m-1]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: RTOVRC_TABLE      ! root turnover coefficient [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: RSDRYC_TABLE      ! degree of drying that reduces soil respiration [-]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: RSWOODC_TABLE     ! wood respiration coeficient [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: BF_TABLE          ! parameter for present wood allocation [-]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: WSTRC_TABLE       ! water stress coeficient [-]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: LAIMIN_TABLE      ! minimum leaf area index [m2/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: XSAMIN_TABLE      ! minimum stem area index [m2/m2]

    ! original MPTABLE.TBL radiation parameters
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: ALBSAT_TABLE      ! saturated soil albedos: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: ALBDRY_TABLE      ! dry soil albedos: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:)   :: ALBICE_TABLE      ! albedo land ice: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:)   :: ALBLAK_TABLE      ! albedo frozen lakes: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:)   :: OMEGAS_TABLE      ! two-stream parameter omega for snow
    real(kind=kind_noahmp)                              :: BETADS_TABLE      ! two-stream parameter betad for snow
    real(kind=kind_noahmp)                              :: BETAIS_TABLE      ! two-stream parameter betad for snow
    real(kind=kind_noahmp), allocatable, dimension(:)   :: EG_TABLE          ! emissivity soil surface
    real(kind=kind_noahmp)                              :: EICE_TABLE        ! ice surface emissivity

    ! original MPTABLE.TBL global parameters
    real(kind=kind_noahmp)                              :: CO2_TABLE                 ! co2 partial pressure
    real(kind=kind_noahmp)                              :: O2_TABLE                  ! o2 partial pressure
    real(kind=kind_noahmp)                              :: TIMEAN_TABLE              ! gridcell mean topgraphic index (global mean)
    real(kind=kind_noahmp)                              :: FSATMX_TABLE              ! maximum surface saturated fraction (global mean)
    real(kind=kind_noahmp)                              :: Z0SNO_TABLE               ! snow surface roughness length (m) (0.002)
    real(kind=kind_noahmp)                              :: SSI_TABLE                 ! liquid water holding capacity for snowpack (m3/m3) (0.03)
    real(kind=kind_noahmp)                              :: SNOW_RET_FAC_TABLE        ! snowpack water release timescale factor (1/s)
    real(kind=kind_noahmp)                              :: SNOW_EMIS_TABLE           ! snow emissivity
    real(kind=kind_noahmp)                              :: SWEMX_TABLE               ! new snow mass to fully cover old snow (mm)
    real(kind=kind_noahmp)                              :: TAU0_TABLE                ! tau0 from Yang97 eqn. 10a
    real(kind=kind_noahmp)                              :: GRAIN_GROWTH_TABLE        ! growth from vapor diffusion Yang97 eqn. 10b
    real(kind=kind_noahmp)                              :: EXTRA_GROWTH_TABLE        ! extra growth near freezing Yang97 eqn. 10c
    real(kind=kind_noahmp)                              :: DIRT_SOOT_TABLE           ! dirt and soot term Yang97 eqn. 10d
    real(kind=kind_noahmp)                              :: BATS_COSZ_TABLE           ! zenith angle snow albedo adjustment; b in Yang97 eqn. 15
    real(kind=kind_noahmp)                              :: BATS_VIS_NEW_TABLE        ! new snow visible albedo
    real(kind=kind_noahmp)                              :: BATS_NIR_NEW_TABLE        ! new snow NIR albedo
    real(kind=kind_noahmp)                              :: BATS_VIS_AGE_TABLE        ! age factor for diffuse visible snow albedo Yang97 eqn. 17
    real(kind=kind_noahmp)                              :: BATS_NIR_AGE_TABLE        ! age factor for diffuse NIR snow albedo Yang97 eqn. 18
    real(kind=kind_noahmp)                              :: BATS_VIS_DIR_TABLE        ! cosz factor for direct visible snow albedo Yang97 eqn. 15
    real(kind=kind_noahmp)                              :: BATS_NIR_DIR_TABLE        ! cosz factor for direct NIR snow albedo Yang97 eqn. 16
    real(kind=kind_noahmp)                              :: RSURF_SNOW_TABLE          ! surface resistance for snow(s/m)
    real(kind=kind_noahmp)                              :: RSURF_EXP_TABLE           ! exponent in the shape parameter for soil resistance option 1
    real(kind=kind_noahmp)                              :: C2_SNOWCOMPACT_TABLE      ! overburden snow compaction parameter (m3/kg)
    real(kind=kind_noahmp)                              :: C3_SNOWCOMPACT_TABLE      ! snow desctructive metamorphism compaction parameter1 [1/s]
    real(kind=kind_noahmp)                              :: C4_SNOWCOMPACT_TABLE      ! snow desctructive metamorphism compaction parameter2 [1/k]
    real(kind=kind_noahmp)                              :: C5_SNOWCOMPACT_TABLE      ! snow desctructive metamorphism compaction parameter3
    real(kind=kind_noahmp)                              :: DM_SNOWCOMPACT_TABLE      ! upper Limit on destructive metamorphism compaction [kg/m3]
    real(kind=kind_noahmp)                              :: ETA0_SNOWCOMPACT_TABLE    ! snow viscosity coefficient [kg-s/m2]
    real(kind=kind_noahmp)                              :: SNLIQMAXFRAC_TABLE        ! maximum liquid water fraction in snow
    real(kind=kind_noahmp)                              :: SWEMAXGLA_TABLE           ! Maximum SWE allowed at glaciers (mm)
    real(kind=kind_noahmp)                              :: WSLMAX_TABLE              ! maximum lake water storage (mm)
    real(kind=kind_noahmp)                              :: ROUS_TABLE                ! specific yield [-] for Niu et al. 2007 groundwater scheme
    real(kind=kind_noahmp)                              :: CMIC_TABLE                ! microprore content (0.0-1.0), 0.0: close to free drainage
    real(kind=kind_noahmp)                              :: SNOWDEN_MIN_TABLE         ! fresh snowfall density (kg/m3)
    real(kind=kind_noahmp)                              :: CLASS_ALB_REF_TABLE       ! reference snow albedo in CLASS scheme
    real(kind=kind_noahmp)                              :: CLASS_SNO_AGE_TABLE       ! snow aging e-folding time (s) in CLASS albedo scheme
    real(kind=kind_noahmp)                              :: CLASS_ALB_NEW_TABLE       ! fresh snow albedo in CLASS scheme
    real(kind=kind_noahmp)                              :: PSIWLT_TABLE              ! soil metric potential for wilting point (m)
    real(kind=kind_noahmp)                              :: Z0SOIL_TABLE              ! Bare-soil roughness length (m) (i.e., under the canopy)
    real(kind=kind_noahmp)                              :: Z0LAKE_TABLE              ! Lake surface roughness length (m)

    ! original MPTABLE.TBL irrigation parameters
    integer                                             :: IRR_HAR_TABLE             ! number of days before harvest date to stop irrigation 
    real(kind=kind_noahmp)                              :: IRR_FRAC_TABLE            ! irrigation Fraction
    real(kind=kind_noahmp)                              :: IRR_LAI_TABLE             ! Minimum lai to trigger irrigation
    real(kind=kind_noahmp)                              :: IRR_MAD_TABLE             ! management allowable deficit (0-1)
    real(kind=kind_noahmp)                              :: FILOSS_TABLE              ! factor of flood irrigation loss
    real(kind=kind_noahmp)                              :: SPRIR_RATE_TABLE          ! mm/h, sprinkler irrigation rate
    real(kind=kind_noahmp)                              :: MICIR_RATE_TABLE          ! mm/h, micro irrigation rate
    real(kind=kind_noahmp)                              :: FIRTFAC_TABLE             ! flood application rate factor
    real(kind=kind_noahmp)                              :: IR_RAIN_TABLE             ! maximum precipitation to stop irrigation trigger

    ! original MPTABLE.TBL tile drainage parameters
    integer                                             :: DRAIN_LAYER_OPT_TABLE
    integer               , allocatable, dimension(:)   :: TD_DEPTH_TABLE
    real(kind=kind_noahmp), allocatable, dimension(:)   :: TDSMC_FAC_TABLE
    real(kind=kind_noahmp), allocatable, dimension(:)   :: TD_DC_TABLE
    real(kind=kind_noahmp), allocatable, dimension(:)   :: TD_DCOEF_TABLE
    real(kind=kind_noahmp), allocatable, dimension(:)   :: TD_D_TABLE
    real(kind=kind_noahmp), allocatable, dimension(:)   :: TD_ADEPTH_TABLE
    real(kind=kind_noahmp), allocatable, dimension(:)   :: TD_RADI_TABLE
    real(kind=kind_noahmp), allocatable, dimension(:)   :: TD_SPAC_TABLE
    real(kind=kind_noahmp), allocatable, dimension(:)   :: TD_DDRAIN_TABLE
    real(kind=kind_noahmp), allocatable, dimension(:)   :: KLAT_FAC_TABLE

    ! original MPTABLE.TBL optional parameters
    real(kind=kind_noahmp)                              :: sr2006_theta_1500t_a_TABLE      ! sand coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_1500t_b_TABLE      ! clay coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_1500t_c_TABLE      ! orgm coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_1500t_d_TABLE      ! sand*orgm coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_1500t_e_TABLE      ! clay*orgm coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_1500t_f_TABLE      ! sand*clay coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_1500t_g_TABLE      ! constant adjustment
    real(kind=kind_noahmp)                              :: sr2006_theta_1500_a_TABLE       ! theta_1500t coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_1500_b_TABLE       ! constant adjustment
    real(kind=kind_noahmp)                              :: sr2006_theta_33t_a_TABLE        ! sand coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_33t_b_TABLE        ! clay coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_33t_c_TABLE        ! orgm coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_33t_d_TABLE        ! sand*orgm coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_33t_e_TABLE        ! clay*orgm coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_33t_f_TABLE        ! sand*clay coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_33t_g_TABLE        ! constant adjustment
    real(kind=kind_noahmp)                              :: sr2006_theta_33_a_TABLE         ! theta_33t*theta_33t coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_33_b_TABLE         ! theta_33t coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_33_c_TABLE         ! constant adjustment
    real(kind=kind_noahmp)                              :: sr2006_theta_s33t_a_TABLE       ! sand coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_s33t_b_TABLE       ! clay coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_s33t_c_TABLE       ! orgm coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_s33t_d_TABLE       ! sand*orgm coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_s33t_e_TABLE       ! clay*orgm coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_s33t_f_TABLE       ! sand*clay coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_s33t_g_TABLE       ! constant adjustment
    real(kind=kind_noahmp)                              :: sr2006_theta_s33_a_TABLE        ! theta_s33t coefficient
    real(kind=kind_noahmp)                              :: sr2006_theta_s33_b_TABLE        ! constant adjustment
    real(kind=kind_noahmp)                              :: sr2006_psi_et_a_TABLE           ! sand coefficient
    real(kind=kind_noahmp)                              :: sr2006_psi_et_b_TABLE           ! clay coefficient
    real(kind=kind_noahmp)                              :: sr2006_psi_et_c_TABLE           ! theta_s33 coefficient
    real(kind=kind_noahmp)                              :: sr2006_psi_et_d_TABLE           ! sand*theta_s33 coefficient
    real(kind=kind_noahmp)                              :: sr2006_psi_et_e_TABLE           ! clay*theta_s33 coefficient
    real(kind=kind_noahmp)                              :: sr2006_psi_et_f_TABLE           ! sand*clay coefficient
    real(kind=kind_noahmp)                              :: sr2006_psi_et_g_TABLE           ! constant adjustment
    real(kind=kind_noahmp)                              :: sr2006_psi_e_a_TABLE            ! psi_et*psi_et coefficient
    real(kind=kind_noahmp)                              :: sr2006_psi_e_b_TABLE            ! psi_et coefficient
    real(kind=kind_noahmp)                              :: sr2006_psi_e_c_TABLE            ! constant adjustment
    real(kind=kind_noahmp)                              :: sr2006_smcmax_a_TABLE           ! sand adjustment
    real(kind=kind_noahmp)                              :: sr2006_smcmax_b_TABLE           ! constant adjustment

    ! original MPTABLE.TBL crop parameters
    integer                                             :: DEFAULT_CROP_TABLE        ! Default crop index
    integer               , allocatable, dimension(:)   :: PLTDAY_TABLE              ! Planting date
    integer               , allocatable, dimension(:)   :: HSDAY_TABLE               ! Harvest date
    integer               , allocatable, dimension(:)   :: C3C4_TABLE                ! photosynthetic pathway:  1. = c3 2. = c4
    real(kind=kind_noahmp), allocatable, dimension(:)   :: PLANTPOP_TABLE            ! Plant density [per ha] - used?
    real(kind=kind_noahmp), allocatable, dimension(:)   :: IRRI_TABLE                ! Irrigation strategy 0= non-irrigation 1=irrigation (no water-stress)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: GDDTBASE_TABLE            ! Base temperature for GDD accumulation [C]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: GDDTCUT_TABLE             ! Upper temperature for GDD accumulation [C]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: GDDS1_TABLE               ! GDD from seeding to emergence
    real(kind=kind_noahmp), allocatable, dimension(:)   :: GDDS2_TABLE               ! GDD from seeding to initial vegetative 
    real(kind=kind_noahmp), allocatable, dimension(:)   :: GDDS3_TABLE               ! GDD from seeding to post vegetative 
    real(kind=kind_noahmp), allocatable, dimension(:)   :: GDDS4_TABLE               ! GDD from seeding to intial reproductive
    real(kind=kind_noahmp), allocatable, dimension(:)   :: GDDS5_TABLE               ! GDD from seeding to pysical maturity 
    real(kind=kind_noahmp), allocatable, dimension(:)   :: C3PSNI_TABLE              ! photosynthetic pathway: 0. = c4, 1. = c3 ! Zhe Zhang 2020-07-03
    real(kind=kind_noahmp), allocatable, dimension(:)   :: KC25I_TABLE               ! co2 michaelis-menten constant at 25c (pa)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: AKCI_TABLE                ! q10 for kc25
    real(kind=kind_noahmp), allocatable, dimension(:)   :: KO25I_TABLE               ! o2 michaelis-menten constant at 25c (pa)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: AKOI_TABLE                ! q10 for ko25
    real(kind=kind_noahmp), allocatable, dimension(:)   :: VCMX25I_TABLE             ! maximum rate of carboxylation at 25c (umol co2/m**2/s)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: AVCMXI_TABLE              ! q10 for vcmx25
    real(kind=kind_noahmp), allocatable, dimension(:)   :: BPI_TABLE                 ! minimum leaf conductance (umol/m**2/s)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: MPI_TABLE                 ! slope of conductance-to-photosynthesis relationship
    real(kind=kind_noahmp), allocatable, dimension(:)   :: QE25I_TABLE               ! quantum efficiency at 25c (umol co2 / umol photon)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: FOLNMXI_TABLE             ! foliage nitrogen concentration when
    real(kind=kind_noahmp), allocatable, dimension(:)   :: AREF_TABLE                ! reference maximum CO2 assimulation rate 
    real(kind=kind_noahmp), allocatable, dimension(:)   :: PSNRF_TABLE               ! CO2 assimulation reduction factor(0-1) (caused by non-modeling part,e.g.pest,weeds)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: I2PAR_TABLE               ! Fraction of incoming solar radiation to photosynthetically active radiation
    real(kind=kind_noahmp), allocatable, dimension(:)   :: TASSIM0_TABLE             ! Minimum temperature for CO2 assimulation [C]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: TASSIM1_TABLE             ! CO2 assimulation linearly increasing until temperature reaches T1 [C]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: TASSIM2_TABLE             ! CO2 assmilation rate remain at Aref until temperature reaches T2 [C]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: K_TABLE                   ! light extinction coefficient
    real(kind=kind_noahmp), allocatable, dimension(:)   :: EPSI_TABLE                ! initial light use efficiency
    real(kind=kind_noahmp), allocatable, dimension(:)   :: Q10MR_TABLE               ! q10 for maintainance respiration
    real(kind=kind_noahmp), allocatable, dimension(:)   :: FOLN_MX_TABLE             ! foliage nitrogen concentration when f(n)=1 (%)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: LEFREEZ_TABLE             ! characteristic T for leaf freezing [K]
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: DILE_FC_TABLE             ! coeficient for temperature leaf stress death [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: DILE_FW_TABLE             ! coeficient for water leaf stress death [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: FRA_GR_TABLE              ! fraction of growth respiration
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: LF_OVRC_TABLE             ! fraction of leaf turnover  [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: ST_OVRC_TABLE             ! fraction of stem turnover  [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: RT_OVRC_TABLE             ! fraction of root tunrover  [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: LFMR25_TABLE              ! leaf maintenance respiration at 25C [umol CO2/m**2  /s]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: STMR25_TABLE              ! stem maintenance respiration at 25C [umol CO2/kg bio/s]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: RTMR25_TABLE              ! root maintenance respiration at 25C [umol CO2/kg bio/s]
    real(kind=kind_noahmp), allocatable, dimension(:)   :: GRAINMR25_TABLE           ! grain maintenance respiration at 25C [umol CO2/kg bio/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: LFPT_TABLE                ! fraction of carbohydrate flux to leaf
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: STPT_TABLE                ! fraction of carbohydrate flux to stem
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: RTPT_TABLE                ! fraction of carbohydrate flux to root
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: GRAINPT_TABLE             ! fraction of carbohydrate flux to grain
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: LFCT_TABLE                ! fraction of carbohydrate translocation from leaf to grain 
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: STCT_TABLE                ! stem to grain
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: RTCT_TABLE                ! root to grain
    real(kind=kind_noahmp), allocatable, dimension(:)   :: BIO2LAI_TABLE             ! leaf are per living leaf biomass [m^2/kg]

    ! original SOILPARM.TBL parameters
    integer                                             :: SLCATS_TABLE      ! number of soil categories
    real(kind=kind_noahmp), allocatable, dimension(:)   :: BEXP_TABLE        ! soil B parameter
    real(kind=kind_noahmp), allocatable, dimension(:)   :: SMCDRY_TABLE      ! dry soil moisture threshold
    real(kind=kind_noahmp), allocatable, dimension(:)   :: F1_TABLE          ! soil thermal diffusivity/conductivity coef
    real(kind=kind_noahmp), allocatable, dimension(:)   :: SMCMAX_TABLE      ! porosity, saturated value of soil moisture (volumetric)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: SMCREF_TABLE      ! reference soil moisture (field capacity) (volumetric)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: PSISAT_TABLE      ! saturated soil matric potential
    real(kind=kind_noahmp), allocatable, dimension(:)   :: DKSAT_TABLE       ! saturated soil hydraulic conductivity
    real(kind=kind_noahmp), allocatable, dimension(:)   :: DWSAT_TABLE       ! saturated soil hydraulic diffusivity
    real(kind=kind_noahmp), allocatable, dimension(:)   :: SMCWLT_TABLE      ! wilting point soil moisture (volumetric)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: QUARTZ_TABLE      ! soil quartz content
    real(kind=kind_noahmp), allocatable, dimension(:)   :: BVIC_TABLE        ! VIC model infiltration parameter (-) for opt_run=6
    real(kind=kind_noahmp), allocatable, dimension(:)   :: AXAJ_TABLE        ! Xinanjiang: Tension water distribution inflection parameter [-] for opt_run=7
    real(kind=kind_noahmp), allocatable, dimension(:)   :: BXAJ_TABLE        ! Xinanjiang: Tension water distribution shape parameter [-] for opt_run=7
    real(kind=kind_noahmp), allocatable, dimension(:)   :: XXAJ_TABLE        ! Xinanjiang: Free water distribution shape parameter [-] for opt_run=7
    real(kind=kind_noahmp), allocatable, dimension(:)   :: BDVIC_TABLE       ! VIC model infiltration parameter (-)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: GDVIC_TABLE       ! mean capilary drive (m)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: BBVIC_TABLE       ! heterogeniety parameter for DVIC infiltration [-]

    ! original GENPARM.TBL parameters
    real(kind=kind_noahmp), allocatable, dimension(:)   :: SLOPE_TABLE       ! slope factor for soil drainage
    real(kind=kind_noahmp)                              :: CSOIL_TABLE       ! Soil heat capacity [J m-3 K-1]
    real(kind=kind_noahmp)                              :: REFDK_TABLE       ! Parameter in the surface runoff parameterization
    real(kind=kind_noahmp)                              :: REFKDT_TABLE      ! Parameter in the surface runoff parameterization
    real(kind=kind_noahmp)                              :: FRZK_TABLE        ! Frozen ground parameter
    real(kind=kind_noahmp)                              :: ZBOT_TABLE        ! Depth [m] of lower boundary soil temperature
    real(kind=kind_noahmp)                              :: CZIL_TABLE        ! Parameter used in the calculation of the roughness length for heat

    !----------------------------------------------------------------
    ! Noahmp State variables from restart or initial input files
    !----------------------------------------------------------------

    integer                                             :: ISNOWIn           ! snow layer index (0 : no snow, -1~-3 : 1~3 snow layers)
    integer                                             :: ISTIn             ! surface type 1-soil; 2-lake
    integer                                             :: IRCNTSIIn
    integer                                             :: IRCNTMIIn
    integer                                             :: IRCNTFIIn
    integer                                             :: NBANDIn
    integer                                             :: ICEIn
    integer                                             :: PGSIn
    integer                                             :: YEARLENIn
    logical                                             :: URBAN_FLAGIn      ! urban point flag
    real(kind=kind_noahmp)                              :: DZ8WIn
    real(kind=kind_noahmp)                              :: TGIn              ! ground temperature (K)
    real(kind=kind_noahmp)                              :: TVIn              ! leaf temperature (K)
    real(kind=kind_noahmp)                              :: CANLIQIn
    real(kind=kind_noahmp)                              :: CANICEIn
    real(kind=kind_noahmp)                              :: SNEQVIn
    real(kind=kind_noahmp)                              :: SNOWHIn
    real(kind=kind_noahmp)                              :: FIFACIn
    real(kind=kind_noahmp)                              :: IRAMTFIIn
    real(kind=kind_noahmp)                              :: MIFACIn
    real(kind=kind_noahmp)                              :: IRAMTMIIn
    real(kind=kind_noahmp)                              :: SIFACIn
    real(kind=kind_noahmp)                              :: IRAMTSIIn
    real(kind=kind_noahmp)                              :: ZWTIn
    real(kind=kind_noahmp)                              :: SMCWTDIn
    real(kind=kind_noahmp)                              :: DEEPRECHIn
    real(kind=kind_noahmp)                              :: WATBLEDIn
    real(kind=kind_noahmp)                              :: TDFRACMPIn
    real(kind=kind_noahmp)                              :: WAIn
    real(kind=kind_noahmp)                              :: WTIn
    real(kind=kind_noahmp)                              :: WSLAKEIn
    real(kind=kind_noahmp)                              :: PONDINGIn
    real(kind=kind_noahmp)                              :: sfcheadrtIn
    real(kind=kind_noahmp)                              :: IRRFRAIn
    real(kind=kind_noahmp)                              :: LAIIn
    real(kind=kind_noahmp)                              :: SAIIn
    real(kind=kind_noahmp)                              :: FVEGIn
    real(kind=kind_noahmp)                              :: TAUSSIn
    real(kind=kind_noahmp)                              :: SNEQVOIn
    real(kind=kind_noahmp)                              :: ALBOLDIn
    real(kind=kind_noahmp)                              :: EAHIn
    real(kind=kind_noahmp)                              :: TAHIn
    real(kind=kind_noahmp)                              :: CHIn
    real(kind=kind_noahmp)                              :: CMIn
    real(kind=kind_noahmp)                              :: FOLNIn
    real(kind=kind_noahmp)                              :: SHDFACIn
    real(kind=kind_noahmp)                              :: LATIn
    real(kind=kind_noahmp), allocatable, dimension(:)   :: STCIn             ! soil/snow layer temperature (K)
    real(kind=kind_noahmp), allocatable, dimension(:)   :: SNICEIn
    real(kind=kind_noahmp), allocatable, dimension(:)   :: SNLIQIn
    real(kind=kind_noahmp), allocatable, dimension(:)   :: FICEOLDIn
    real(kind=kind_noahmp), allocatable, dimension(:)   :: SH2OIn
    real(kind=kind_noahmp), allocatable, dimension(:)   :: SICEIn
    real(kind=kind_noahmp), allocatable, dimension(:)   :: SMCIn
    real(kind=kind_noahmp), allocatable, dimension(:)   :: SMCEQIn

  end type input_type

end module InputVarType
