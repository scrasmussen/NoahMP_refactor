module noahmp_output

  use netcdf

  implicit none

  integer           :: ncid
  integer           :: iret
  integer           :: time_dim
  integer           :: soil_dim
  integer           :: snow_dim
  integer           :: snso_dim
  integer           :: band_dim
! NoahMP output variables
  integer           :: ALBOLD_id
  integer           :: SNEQVO_id
  integer           :: STC_id
  integer           :: SH2O_id
  integer           :: SMC_id
  integer           :: EAH_id
  integer           :: TAH_id
  integer           :: FWET_id
  integer           :: CANLIQ_id
  integer           :: CANICE_id
  integer           :: TV_id
  integer           :: TG_id
  integer           :: QSFC_id
  integer           :: QRAIN_id
  integer           :: QSNOW_id
  integer           :: ISNOW_id
  integer           :: ZSNSO_id
  integer           :: SNOWH_id
  integer           :: SNEQV_id
  integer           :: SNICE_id
  integer           :: SNLIQ_id
  integer           :: ZWT_id
  integer           :: WA_id
  integer           :: WT_id
  integer           :: WSLAKE_id
  integer           :: LFMASS_id
  integer           :: RTMASS_id
  integer           :: STMASS_id
  integer           :: WOOD_id
  integer           :: STBLCP_id
  integer           :: FASTCP_id
  integer           :: LAI_id
  integer           :: SAI_id
  integer           :: CM_id
  integer           :: CH_id
  integer           :: TAUSS_id
  integer           :: GRAIN_id
  integer           :: GDD_id
  integer           :: PGS_id
  integer           :: SMCWTD_id
  integer           :: DEEPRECH_id
  integer           :: RECH_id
  integer           :: QTLDRN_id
  integer           :: Z0WRF_id
  integer           :: IRAMTFI_id
  integer           :: IRAMTMI_id
  integer           :: IRFIRATE_id
  integer           :: IRMIRATE_id
  integer           :: IRAMTSI_id
  integer           :: IRSIRATE_id
  integer           :: IRCNTSI_id
  integer           :: IRCNTMI_id
  integer           :: IRCNTFI_id
  integer           :: FIRR_id
  integer           :: EIRR_id
  integer           :: FSA_id
  integer           :: FSR_id
  integer           :: FIRA_id
  integer           :: FSH_id
  integer           :: SSOIL_id
  integer           :: FCEV_id
  integer           :: FGEV_id
  integer           :: FCTR_id
  integer           :: ECAN_id
  integer           :: ETRAN_id
  integer           :: EDIR_id
  integer           :: TRAD_id
  integer           :: TGV_id
  integer           :: TGB_id
  integer           :: T2MV_id
  integer           :: T2MB_id
  integer           :: Q2V_id
  integer           :: Q2B_id
  integer           :: RUNSRF_id
  integer           :: RUNSUB_id
  integer           :: PSN_id
  integer           :: APAR_id
  integer           :: SAV_id
  integer           :: SAG_id
  integer           :: FSNO_id
  integer           :: NEE_id
  integer           :: GPP_id
  integer           :: NPP_id
  integer           :: FVEG_id
  integer           :: ALBEDO_id
  integer           :: QSNBOT_id
  integer           :: PONDING1_id
  integer           :: PONDING2_id
  integer           :: PONDING_id
  integer           :: RSSUN_id
  integer           :: RSSHA_id
  integer           :: ALBSND_id
  integer           :: ALBSNI_id
  integer           :: BGAP_id
  integer           :: WGAP_id
  integer           :: CHV_id
  integer           :: CHB_id
  integer           :: EMISSI_id
  integer           :: SHG_id
  integer           :: SHC_id
  integer           :: SHB_id
  integer           :: EVG_id
  integer           :: EVB_id
  integer           :: EVC_id
  integer           :: GHV_id
  integer           :: GHB_id
  integer           :: IRG_id
  integer           :: IRC_id
  integer           :: IRB_id
  integer           :: TR_id
  integer           :: CHLEAF_id
  integer           :: CHUC_id
  integer           :: CHV2_id
  integer           :: CHB2_id
  integer           :: FPICE_id
  integer           :: PAHV_id
  integer           :: PAHG_id
  integer           :: PAHB_id
  integer           :: PAH_id
  integer           :: LAISUN_id
  integer           :: LAISHA_id
  integer           :: FICEOLD_id


contains

  subroutine initialize_output(output_filename, ntime, nsoil, nsnow)
 
    integer       :: ntime
    integer       :: nsoil
    integer       :: nsnow
    character*256 :: output_filename
 
    iret = nf90_create(trim(output_filename), NF90_CLOBBER, ncid)
    ! dimension variable
    iret = nf90_def_dim(ncid, "time", ntime, time_dim)
    iret = nf90_def_dim(ncid, "soil", nsoil, soil_dim)
    iret = nf90_def_dim(ncid, "snow", nsnow, snow_dim)
    iret = nf90_def_dim(ncid, "snso", nsnow+nsoil, snso_dim)
    iret = nf90_def_dim(ncid, "band", 2, band_dim)
    ! NoahMP output variables
    iret = nf90_def_var(ncid, "ALBOLD",      NF90_FLOAT, (/time_dim/), ALBOLD_id)
    iret = nf90_def_var(ncid, "SNEQVO",      NF90_FLOAT, (/time_dim/), SNEQVO_id)
    iret = nf90_def_var(ncid, "STC",         NF90_FLOAT, (/time_dim,snso_dim/), STC_id)
    iret = nf90_def_var(ncid, "SH2O",        NF90_FLOAT, (/time_dim,soil_dim/), SH2O_id)
    iret = nf90_def_var(ncid, "SMC",         NF90_FLOAT, (/time_dim,soil_dim/), SMC_id)
    iret = nf90_def_var(ncid, "EAH",         NF90_FLOAT, (/time_dim/), EAH_id)
    iret = nf90_def_var(ncid, "TAH",         NF90_FLOAT, (/time_dim/), TAH_id)
    iret = nf90_def_var(ncid, "FWET",        NF90_FLOAT, (/time_dim/), FWET_id)
    iret = nf90_def_var(ncid, "CANLIQ",      NF90_FLOAT, (/time_dim/), CANLIQ_id)
    iret = nf90_def_var(ncid, "CANICE",      NF90_FLOAT, (/time_dim/), CANICE_id)
    iret = nf90_def_var(ncid, "TV",          NF90_FLOAT, (/time_dim/), TV_id)
    iret = nf90_def_var(ncid, "TG",          NF90_FLOAT, (/time_dim/), TG_id)
    iret = nf90_def_var(ncid, "QSFC",        NF90_FLOAT, (/time_dim/), QSFC_id)
    iret = nf90_def_var(ncid, "QRAIN",       NF90_FLOAT, (/time_dim/), QRAIN_id)
    iret = nf90_def_var(ncid, "QSNOW",       NF90_FLOAT, (/time_dim/), QSNOW_id)
    iret = nf90_def_var(ncid, "ISNOW",       NF90_INT  , (/time_dim/), ISNOW_id)
    iret = nf90_def_var(ncid, "ZSNSO",       NF90_FLOAT, (/time_dim,snso_dim/), ZSNSO_id)
    iret = nf90_def_var(ncid, "SNOWH",       NF90_FLOAT, (/time_dim/), SNOWH_id)
    iret = nf90_def_var(ncid, "SNEQV",       NF90_FLOAT, (/time_dim/), SNEQV_id)
    iret = nf90_def_var(ncid, "SNICE",       NF90_FLOAT, (/time_dim,snow_dim/), SNICE_id)
    iret = nf90_def_var(ncid, "SNLIQ",       NF90_FLOAT, (/time_dim,snow_dim/), SNLIQ_id)
    iret = nf90_def_var(ncid, "ZWT",         NF90_FLOAT, (/time_dim/), ZWT_id)
    iret = nf90_def_var(ncid, "WA",          NF90_FLOAT, (/time_dim/), WA_id)
    iret = nf90_def_var(ncid, "WT",          NF90_FLOAT, (/time_dim/), WT_id)
    iret = nf90_def_var(ncid, "WSLAKE",      NF90_FLOAT, (/time_dim/), WSLAKE_id)
    iret = nf90_def_var(ncid, "LFMASS",      NF90_FLOAT, (/time_dim/), LFMASS_id)
    iret = nf90_def_var(ncid, "RTMASS",      NF90_FLOAT, (/time_dim/), RTMASS_id)
    iret = nf90_def_var(ncid, "STMASS",      NF90_FLOAT, (/time_dim/), STMASS_id)
    iret = nf90_def_var(ncid, "WOOD",        NF90_FLOAT, (/time_dim/), WOOD_id)
    iret = nf90_def_var(ncid, "STBLCP",      NF90_FLOAT, (/time_dim/), STBLCP_id)
    iret = nf90_def_var(ncid, "FASTCP",      NF90_FLOAT, (/time_dim/), FASTCP_id)
    iret = nf90_def_var(ncid, "LAI",         NF90_FLOAT, (/time_dim/), LAI_id)
    iret = nf90_def_var(ncid, "SAI",         NF90_FLOAT, (/time_dim/), SAI_id)
    iret = nf90_def_var(ncid, "CM",          NF90_FLOAT, (/time_dim/), CM_id)
    iret = nf90_def_var(ncid, "CH",          NF90_FLOAT, (/time_dim/), CH_id)
    iret = nf90_def_var(ncid, "TAUSS",       NF90_FLOAT, (/time_dim/), TAUSS_id)
    iret = nf90_def_var(ncid, "GRAIN",       NF90_FLOAT, (/time_dim/), GRAIN_id)
    iret = nf90_def_var(ncid, "GDD",         NF90_FLOAT, (/time_dim/), GDD_id)
    iret = nf90_def_var(ncid, "PGS",         NF90_INT  , (/time_dim/), PGS_id)
    iret = nf90_def_var(ncid, "SMCWTD",      NF90_FLOAT, (/time_dim/), SMCWTD_id)
    iret = nf90_def_var(ncid, "DEEPRECH",    NF90_FLOAT, (/time_dim/), DEEPRECH_id)
    iret = nf90_def_var(ncid, "RECH",        NF90_FLOAT, (/time_dim/), RECH_id)
    iret = nf90_def_var(ncid, "QTLDRN",      NF90_FLOAT, (/time_dim/), QTLDRN_id)
    iret = nf90_def_var(ncid, "Z0WRF",       NF90_FLOAT, (/time_dim/), Z0WRF_id)
    iret = nf90_def_var(ncid, "IRAMTFI",     NF90_FLOAT, (/time_dim/), IRAMTFI_id)
    iret = nf90_def_var(ncid, "IRAMTMI",     NF90_FLOAT, (/time_dim/), IRAMTMI_id)
    iret = nf90_def_var(ncid, "IRFIRATE",    NF90_FLOAT, (/time_dim/), IRFIRATE_id)
    iret = nf90_def_var(ncid, "IRMIRATE",    NF90_FLOAT, (/time_dim/), IRMIRATE_id)
    iret = nf90_def_var(ncid, "IRAMTSI",     NF90_FLOAT, (/time_dim/), IRAMTSI_id)
    iret = nf90_def_var(ncid, "IRSIRATE",    NF90_FLOAT, (/time_dim/), IRSIRATE_id)
    iret = nf90_def_var(ncid, "IRCNTSI",     NF90_FLOAT, (/time_dim/), IRCNTSI_id)
    iret = nf90_def_var(ncid, "IRCNTMI",     NF90_FLOAT, (/time_dim/), IRCNTMI_id)
    iret = nf90_def_var(ncid, "IRCNTFI",     NF90_FLOAT, (/time_dim/), IRCNTFI_id)
    iret = nf90_def_var(ncid, "FIRR",        NF90_FLOAT, (/time_dim/), FIRR_id)
    iret = nf90_def_var(ncid, "EIRR",        NF90_FLOAT, (/time_dim/), EIRR_id)
    iret = nf90_def_var(ncid, "FSA",         NF90_FLOAT, (/time_dim/), FSA_id)
    iret = nf90_def_var(ncid, "FSR",         NF90_FLOAT, (/time_dim/), FSR_id)
    iret = nf90_def_var(ncid, "FIRA",        NF90_FLOAT, (/time_dim/), FIRA_id)
    iret = nf90_def_var(ncid, "FSH",         NF90_FLOAT, (/time_dim/), FSH_id)
    iret = nf90_def_var(ncid, "SSOIL",       NF90_FLOAT, (/time_dim/), SSOIL_id)
    iret = nf90_def_var(ncid, "FCEV",        NF90_FLOAT, (/time_dim/), FCEV_id)
    iret = nf90_def_var(ncid, "FGEV",        NF90_FLOAT, (/time_dim/), FGEV_id)
    iret = nf90_def_var(ncid, "FCTR",        NF90_FLOAT, (/time_dim/), FCTR_id)
    iret = nf90_def_var(ncid, "ECAN",        NF90_FLOAT, (/time_dim/), ECAN_id)
    iret = nf90_def_var(ncid, "ETRAN",       NF90_FLOAT, (/time_dim/), ETRAN_id)
    iret = nf90_def_var(ncid, "EDIR",        NF90_FLOAT, (/time_dim/), EDIR_id)
    iret = nf90_def_var(ncid, "TRAD",        NF90_FLOAT, (/time_dim/), TRAD_id)
    iret = nf90_def_var(ncid, "TGV",         NF90_FLOAT, (/time_dim/), TGV_id)
    iret = nf90_def_var(ncid, "TGB",         NF90_FLOAT, (/time_dim/), TGB_id)
    iret = nf90_def_var(ncid, "T2MV",        NF90_FLOAT, (/time_dim/), T2MV_id)
    iret = nf90_def_var(ncid, "T2MB",        NF90_FLOAT, (/time_dim/), T2MB_id)
    iret = nf90_def_var(ncid, "Q2V",         NF90_FLOAT, (/time_dim/), Q2V_id)
    iret = nf90_def_var(ncid, "Q2B",         NF90_FLOAT, (/time_dim/), Q2B_id)
    iret = nf90_def_var(ncid, "RUNSRF",      NF90_FLOAT, (/time_dim/), RUNSRF_id)
    iret = nf90_def_var(ncid, "RUNSUB",      NF90_FLOAT, (/time_dim/), RUNSUB_id)
    iret = nf90_def_var(ncid, "PSN",         NF90_FLOAT, (/time_dim/), PSN_id)
    iret = nf90_def_var(ncid, "APAR",        NF90_FLOAT, (/time_dim/), APAR_id)
    iret = nf90_def_var(ncid, "SAV",         NF90_FLOAT, (/time_dim/), SAV_id)
    iret = nf90_def_var(ncid, "SAG",         NF90_FLOAT, (/time_dim/), SAG_id)
    iret = nf90_def_var(ncid, "FSNO",        NF90_FLOAT, (/time_dim/), FSNO_id)
    iret = nf90_def_var(ncid, "NEE",         NF90_FLOAT, (/time_dim/), NEE_id)
    iret = nf90_def_var(ncid, "GPP",         NF90_FLOAT, (/time_dim/), GPP_id)
    iret = nf90_def_var(ncid, "NPP",         NF90_FLOAT, (/time_dim/), NPP_id)
    iret = nf90_def_var(ncid, "FVEG",        NF90_FLOAT, (/time_dim/), FVEG_id)
    iret = nf90_def_var(ncid, "ALBEDO",      NF90_FLOAT, (/time_dim/), ALBEDO_id)
    iret = nf90_def_var(ncid, "QSNBOT",      NF90_FLOAT, (/time_dim/), QSNBOT_id)
    iret = nf90_def_var(ncid, "PONDING",     NF90_FLOAT, (/time_dim/), PONDING_id)
    iret = nf90_def_var(ncid, "PONDING1",    NF90_FLOAT, (/time_dim/), PONDING1_id)
    iret = nf90_def_var(ncid, "PONDING2",    NF90_FLOAT, (/time_dim/), PONDING2_id)
    iret = nf90_def_var(ncid, "RSSUN",       NF90_FLOAT, (/time_dim/), RSSUN_id)
    iret = nf90_def_var(ncid, "RSSHA",       NF90_FLOAT, (/time_dim/), RSSHA_id)
    iret = nf90_def_var(ncid, "ALBSND",      NF90_FLOAT, (/time_dim,band_dim/), ALBSND_id)
    iret = nf90_def_var(ncid, "ALBSNI",      NF90_FLOAT, (/time_dim,band_dim/), ALBSNI_id)
    iret = nf90_def_var(ncid, "BGAP",        NF90_FLOAT, (/time_dim/), BGAP_id)
    iret = nf90_def_var(ncid, "WGAP",        NF90_FLOAT, (/time_dim/), WGAP_id)
    iret = nf90_def_var(ncid, "CHV",         NF90_FLOAT, (/time_dim/), CHV_id)
    iret = nf90_def_var(ncid, "CHB",         NF90_FLOAT, (/time_dim/), CHB_id)
    iret = nf90_def_var(ncid, "EMISSI",      NF90_FLOAT, (/time_dim/), EMISSI_id)
    iret = nf90_def_var(ncid, "SHG",         NF90_FLOAT, (/time_dim/), SHG_id)
    iret = nf90_def_var(ncid, "SHC",         NF90_FLOAT, (/time_dim/), SHC_id)
    iret = nf90_def_var(ncid, "SHB",         NF90_FLOAT, (/time_dim/), SHB_id)
    iret = nf90_def_var(ncid, "EVG",         NF90_FLOAT, (/time_dim/), EVG_id)
    iret = nf90_def_var(ncid, "EVC",         NF90_FLOAT, (/time_dim/), EVC_id)
    iret = nf90_def_var(ncid, "EVB",         NF90_FLOAT, (/time_dim/), EVB_id)
    iret = nf90_def_var(ncid, "GHV",         NF90_FLOAT, (/time_dim/), GHV_id)
    iret = nf90_def_var(ncid, "GHB",         NF90_FLOAT, (/time_dim/), GHB_id)
    iret = nf90_def_var(ncid, "IRG",         NF90_FLOAT, (/time_dim/), IRG_id)
    iret = nf90_def_var(ncid, "IRC",         NF90_FLOAT, (/time_dim/), IRC_id)
    iret = nf90_def_var(ncid, "IRB",         NF90_FLOAT, (/time_dim/), IRB_id)
    iret = nf90_def_var(ncid, "TR",          NF90_FLOAT, (/time_dim/), TR_id)
    iret = nf90_def_var(ncid, "CHV2",        NF90_FLOAT, (/time_dim/), CHV2_id)
    iret = nf90_def_var(ncid, "CHLEAF",      NF90_FLOAT, (/time_dim/), CHLEAF_id)
    iret = nf90_def_var(ncid, "CHUC",        NF90_FLOAT, (/time_dim/), CHUC_id)
    iret = nf90_def_var(ncid, "CHB2",        NF90_FLOAT, (/time_dim/), CHB2_id)
    iret = nf90_def_var(ncid, "FPICE",       NF90_FLOAT, (/time_dim/), FPICE_id)
    iret = nf90_def_var(ncid, "PAHV",        NF90_FLOAT, (/time_dim/), PAHV_id)
    iret = nf90_def_var(ncid, "PAHG",        NF90_FLOAT, (/time_dim/), PAHG_id)
    iret = nf90_def_var(ncid, "PAHB",        NF90_FLOAT, (/time_dim/), PAHB_id)
    iret = nf90_def_var(ncid, "PAH",         NF90_FLOAT, (/time_dim/), PAH_id)
    iret = nf90_def_var(ncid, "LAISUN",      NF90_FLOAT, (/time_dim/), LAISUN_id)
    iret = nf90_def_var(ncid, "LAISHA",      NF90_FLOAT, (/time_dim/), LAISHA_id)
    iret = nf90_def_var(ncid, "FICEOLD",     NF90_FLOAT, (/time_dim,snow_dim/), FICEOLD_id)

    iret = nf90_enddef(ncid)
  
   end subroutine initialize_output

   subroutine add_to_output(itime,NSOIL,NSNOW,ALBOLD,SNEQVO,STC,SH2O,SMC,TAH,EAH,FWET,CANLIQ,CANICE,&
                     TV,TG,QSFC,QSNOW,QRAIN,ISNOW,ZSNSO,SNOWH,SNEQV,SNICE,SNLIQ,ZWT,WA,WT,WSLAKE,&
                     LFMASS,RTMASS,STMASS,WOOD,STBLCP,FASTCP,LAI,SAI,CM,CH,TAUSS,GRAIN,GDD,PGS,&
                     SMCWTD,DEEPRECH,RECH,QTLDRN,Z0WRF,IRCNTSI,IRCNTMI,IRCNTFI,IRAMTSI,&
                     IRAMTMI,IRAMTFI,IRSIRATE,IRMIRATE,IRFIRATE,FIRR,EIRR,FSA,FSR,FIRA,FSH,SSOIL,&
                     FCEV,FGEV,FCTR,ECAN,ETRAN,EDIR,TRAD,TGB,TGV,T2MV,T2MB,Q2V,Q2B,RUNSRF,RUNSUB,&
                     APAR,PSN,SAV,SAG,FSNO,NEE,GPP,NPP,FVEG,ALBEDO,QSNBOT,PONDING,PONDING1,PONDING2,&
                     RSSUN,RSSHA,ALBSND,ALBSNI,BGAP,WGAP,CHV,CHB,EMISSI,SHG,SHC,SHB,EVG,EVB,GHV,GHB,&
                     IRG,IRC,IRB,TR,EVC,CHLEAF,CHUC,CHV2,CHB2,FPICE,PAHV,PAHG,PAHB,PAH,LAISUN,LAISHA,FICEOLD)

     integer                       :: itime
     integer                       :: NSOIL
     integer                       :: NSNOW
     real                          :: ALBOLD
     real                          :: SNEQVO
     real, dimension(nsoil+nsnow)  :: STC     !snow/soil layer temperature [k]
     real, dimension(nsoil)        :: SH2O    ! soil liquid water content [m3/m3]
     real, dimension(nsoil)        :: SMC         !total soil water content [m3/m3]
     REAL                          :: TAH
     REAL                          :: EAH
     real                          :: FWET    !wetted/snowed fraction of canopy (-)
     real                          :: CANLIQ               ! intercepted liquid water (mm)
     real                          :: CANICE               ! intercepted ice mass (mm)
     real                          :: TV                   ! canopy temperature
     real                          :: TG                   ! ground temperature
     REAL                          :: QSFC                 ! mixing ratio at lowest model layer
     REAL                          :: QRAIN                ! rain at ground srf (mm/s) [+]
     REAL                          :: QSNOW                ! snow at ground srf (mm/s) [+]
     integer                       :: ISNOW                ! actual no. of snow layers
     real, dimension(nsoil+nsnow)  :: ZSNSO   !depth of snow/soil layer-bottom
     REAL                          :: SNOWH                ! snow height [m]
     REAL                          :: SNEQV                ! snow water eqv. [mm]
     real, dimension(nsnow)        :: SNICE   !snow layer ice [mm]
     real, dimension(nsnow)        :: SNLIQ   !snow layer liquid water [mm]
     real                          :: ZWT                  ! the depth to water table [m]
     real                          :: WA                   ! water storage in aquifer [mm]
     real                          :: WT                   ! water storage in aquifer + stuarated soil [mm]
     REAL                          :: WSLAKE               ! water storage in lake (can be -) (mm)
     REAL                          :: LFMASS               ! leaf mass [g/m2]
     REAL                          :: RTMASS               ! mass of fine roots [g/m2]
     REAL                          :: STMASS               ! stem mass [g/m2]
     REAL                          :: WOOD
     REAL                          :: STBLCP               ! stable carbon in deep soil [g/m2]
     REAL                          :: FASTCP
     real                          :: LAI                  ! leaf area index
     real                          :: SAI                  ! stem area index
     REAL                          :: CM                   ! momentum drag coefficient
     REAL                          :: CH                   ! sensible heat exchange coefficient
     real                          :: TAUSS                ! non-dimensional snow age
     real                          :: GRAIN
     real                          :: GDD
     INTEGER                       :: PGS                  ! stem respiration [g/m2/s]
     real                          :: SMCWTD               ! soil water content between bottom of the soil and water table [m3/m3]
     real                          :: DEEPRECH             ! recharge to or from the water table when deep [m]
     real                          :: RECH                 ! recharge to or from the water table when shallow [m] (diagnostic)
     real                          :: QTLDRN               ! tile drainage (mm/s)
     REAL                          :: Z0WRF
     integer                       :: IRCNTSI              ! irrigation event number, Sprinkler
     integer                       :: IRCNTMI              ! irrigation event number, Micro
     integer                       :: IRCNTFI              ! irrigation event number, Flood 
     real                          :: IRAMTFI              ! irrigation water amount [m] to be applied, flood
     real                          :: IRAMTMI              ! irrigation water amount [m] to be applied, Micro
     real                          :: IRAMTSI              ! total irrigation water amount [m]
     real                          :: IRFIRATE             ! rate of irrigation by flood [m/timestep]
     real                          :: IRMIRATE             ! rate of irrigation by micro [m/timestep]
     real                          :: IRSIRATE             ! rate of irrigation by sprinkler [m/timestep]
     real                          :: FIRR                 ! irrigation:latent heating due to sprinkler evaporation [w/m2]
     real                          :: EIRR                 ! evaporation of irrigation water to evaporation,sprinkler [mm/s]
     REAL                          :: FSA                  ! total absorbed solar radiation (w/m2)
     REAL                          :: FSR                  ! total reflected solar radiation (w/m2)
     real                          :: FIRA
     real                          :: FSH
     REAL                          :: SSOIL
     real                          :: FCEV                 ! canopy evaporation (w/m2) [+ to atm ]
     real                          :: FGEV                 ! ground evap heat (w/m2) [+ to atm]
     real                          :: FCTR                 ! transpiration (w/m2) [+ to atm]
     real                          :: ECAN                 ! evap of intercepted water (mm/s) [+]
     real                          :: ETRAN                ! transpiration rate (mm/s) [+]
     REAL                          :: EDIR                 ! net soil evaporation (mm/s)
     real                          :: TRAD
     REAL                          :: TGB                  ! bare ground temperature
     REAL                          :: TGV                  ! vegetated ground temperature
     REAL                          :: T2MV                 ! 2 m height air temperature (k)
     REAL                          :: T2MB
     REAL                          :: Q2V
     REAL                          :: Q2B
     real                          :: RUNSRF               ! surface runoff [mm/s] 
     real                          :: RUNSUB               ! baseflow (sturation excess) [mm/s]
     real                          :: PSN
     real                          :: APAR
     REAL                          :: SAV                  ! solar radiation absorbed by vegetation (w/m2)
     REAL                          :: SAG                  ! solar radiation absorbed by ground (w/m2)
     real                          :: FSNO                 ! snow cover fraction
     REAL                          :: NEE                  ! net ecosys exchange (g/m2/s CO2)
     REAL                          :: GPP                  ! gross primary assimilation [g/m2/s C]
     REAL                          :: NPP                  ! net primary productivity [g/m2/s C]
     REAL                          :: FVEG
     REAL                          :: ALBEDO
     REAL                          :: PONDING
     REAL                          :: PONDING1
     REAL                          :: PONDING2
     REAL                          :: QSNBOT               ! melting water out of snow bottom [mm/s]
     REAL                          :: RSSUN                ! sunlit leaf stomatal resistance (s/m)
     REAL                          :: RSSHA                ! shaded leaf stomatal resistance (s/m)
     REAL, DIMENSION(1:2)          :: ALBSND               ! snow albedo (direct)
     REAL, DIMENSION(1:2)          :: ALBSNI               ! snow albedo (diffuse)
     REAL                          :: BGAP
     REAL                          :: WGAP
     REAL                          :: CHV                  ! sensible heat exchange coefficient
     REAL                          :: CHB                  ! sensible heat exchange coefficient
     real                          :: EMISSI
     REAL                          :: SHG                  ! sensible heat flux (w/m2)     [+= to atm]
     REAL                          :: SHC                  ! sensible heat flux (w/m2)     [+= to atm]
     REAL                          :: SHB
     REAL                          :: EVG                  ! evaporation heat flux (w/m2)  [+= to atm]
     REAL                          :: EVB                  ! evaporation heat flux (w/m2)  [+= to atm]
     REAL                          :: GHV                  ! ground heat (w/m2) [+ = to soil]
     REAL                          :: GHB                  ! ground heat (w/m2) [+ = to soil]
     REAL                          :: IRG                  ! net longwave radiation (w/m2) [+= to atm]
     REAL                          :: IRC                  ! net longwave radiation (w/m2) [+= to atm]
     REAL                          :: IRB                  ! net longwave radiation (w/m2) [+= to atm]
     REAL                          :: EVC                  ! evaporation heat flux (w/m2)  [+= to atm]
     REAL                          :: TR                   ! transpiration heat flux (w/m2)[+= to atm]
     REAL                          :: CHV2                 ! sensible heat conductance for diagnostics
     REAL                          :: CHLEAF               ! leaf exchange coefficient
     REAL                          :: CHUC                 ! under canopy exchange coefficient
     REAL                          :: CHB2
     REAL                          :: FPICE
     REAL                          :: PAHV                 ! precipitation advected heat - vegetation net (W/m2)
     REAL                          :: PAHG                 ! precipitation advected heat - under canopy net (W/m2)
     REAL                          :: PAHB                 ! precipitation advected heat - bare ground net (W/m2)
     REAL                          :: PAH
     REAL                          :: LAISUN               ! sunlit leaf area (-)
     REAL                          :: LAISHA               ! shaded leaf area (-)
     real, dimension(nsnow)        :: FICEOLD !ice fraction at last timestep

! assign values
     iret = nf90_put_var(ncid, ALBOLD_id,   ALBOLD,        start=(/itime+1/))
     iret = nf90_put_var(ncid, SNEQVO_id,   SNEQVO,        start=(/itime+1/))
     iret = nf90_put_var(ncid, STC_id,      STC,           start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
     iret = nf90_put_var(ncid, SH2O_id,     SH2O,          start=(/itime+1,1/), count=(/1,nsoil/))
     iret = nf90_put_var(ncid, SMC_id,      SMC,           start=(/itime+1,1/), count=(/1,nsoil/))
     iret = nf90_put_var(ncid, TAH_id,      TAH,           start=(/itime+1/))
     iret = nf90_put_var(ncid, EAH_id,      EAH,           start=(/itime+1/))
     iret = nf90_put_var(ncid, FWET_id,     FWET,          start=(/itime+1/))
     iret = nf90_put_var(ncid, CANLIQ_id,   CANLIQ,        start=(/itime+1/))
     iret = nf90_put_var(ncid, CANICE_id,   CANICE,        start=(/itime+1/))
     iret = nf90_put_var(ncid, TV_id,       TV,            start=(/itime+1/))
     iret = nf90_put_var(ncid, TG_id,       TG,            start=(/itime+1/))
     iret = nf90_put_var(ncid, QSFC_id,     QSFC,          start=(/itime+1/))
     iret = nf90_put_var(ncid, QRAIN_id,    QRAIN,         start=(/itime+1/))
     iret = nf90_put_var(ncid, QSNOW_id,    QSNOW,         start=(/itime+1/))
     iret = nf90_put_var(ncid, ISNOW_id,    ISNOW,         start=(/itime+1/))
     iret = nf90_put_var(ncid, ZSNSO_id,    ZSNSO,         start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
     iret = nf90_put_var(ncid, SNOWH_id,    SNOWH,         start=(/itime+1/))
     iret = nf90_put_var(ncid, SNEQV_id,    SNEQV,         start=(/itime+1/))
     iret = nf90_put_var(ncid, SNICE_id,    SNICE,         start=(/itime+1,1/), count=(/1,nsnow/))
     iret = nf90_put_var(ncid, SNLIQ_id,    SNLIQ,         start=(/itime+1,1/), count=(/1,nsnow/))
     iret = nf90_put_var(ncid, ZWT_id,      ZWT,           start=(/itime+1/))
     iret = nf90_put_var(ncid, WA_id,       WA,            start=(/itime+1/))
     iret = nf90_put_var(ncid, WT_id,       WT,            start=(/itime+1/))
     iret = nf90_put_var(ncid, WSLAKE_id,   WSLAKE,        start=(/itime+1/))
     iret = nf90_put_var(ncid, LFMASS_id,   LFMASS,        start=(/itime+1/))
     iret = nf90_put_var(ncid, RTMASS_id,   RTMASS,        start=(/itime+1/))
     iret = nf90_put_var(ncid, STMASS_id,   STMASS,        start=(/itime+1/))
     iret = nf90_put_var(ncid, WOOD_id,     WOOD,          start=(/itime+1/))
     iret = nf90_put_var(ncid, STBLCP_id,   STBLCP,        start=(/itime+1/))
     iret = nf90_put_var(ncid, FASTCP_id,   FASTCP,        start=(/itime+1/))
     iret = nf90_put_var(ncid, LAI_id,      LAI,           start=(/itime+1/))
     iret = nf90_put_var(ncid, SAI_id,      SAI,           start=(/itime+1/))
     iret = nf90_put_var(ncid, CM_id,       CM,            start=(/itime+1/))
     iret = nf90_put_var(ncid, CH_id,       CH,            start=(/itime+1/))
     iret = nf90_put_var(ncid, TAUSS_id,    TAUSS,         start=(/itime+1/))
     iret = nf90_put_var(ncid, GRAIN_id,    GRAIN,         start=(/itime+1/))
     iret = nf90_put_var(ncid, GDD_id,      GDD,           start=(/itime+1/))
     iret = nf90_put_var(ncid, PGS_id,      PGS,           start=(/itime+1/))
     iret = nf90_put_var(ncid, SMCWTD_id,   SMCWTD,        start=(/itime+1/))
     iret = nf90_put_var(ncid, DEEPRECH_id, DEEPRECH,      start=(/itime+1/))
     iret = nf90_put_var(ncid, RECH_id,     RECH,          start=(/itime+1/))
     iret = nf90_put_var(ncid, QTLDRN_id,   QTLDRN,        start=(/itime+1/))
     iret = nf90_put_var(ncid, Z0WRF_id,    Z0WRF,         start=(/itime+1/))
     iret = nf90_put_var(ncid, IRAMTFI_id,  IRAMTFI,       start=(/itime+1/))
     iret = nf90_put_var(ncid, IRAMTMI_id,  IRAMTMI,       start=(/itime+1/))
     iret = nf90_put_var(ncid, IRFIRATE_id, IRFIRATE,      start=(/itime+1/))
     iret = nf90_put_var(ncid, IRMIRATE_id, IRMIRATE,      start=(/itime+1/))
     iret = nf90_put_var(ncid, IRAMTSI_id,  IRAMTSI,       start=(/itime+1/))
     iret = nf90_put_var(ncid, IRSIRATE_id, IRSIRATE,      start=(/itime+1/))
     iret = nf90_put_var(ncid, IRCNTSI_id,  IRCNTSI,       start=(/itime+1/))
     iret = nf90_put_var(ncid, IRCNTMI_id,  IRCNTMI,       start=(/itime+1/))
     iret = nf90_put_var(ncid, IRCNTFI_id,  IRCNTFI,       start=(/itime+1/))
     iret = nf90_put_var(ncid, FIRR_id,     FIRR,          start=(/itime+1/))
     iret = nf90_put_var(ncid, EIRR_id,     EIRR,          start=(/itime+1/))
     iret = nf90_put_var(ncid, FSA_id,      FSA,           start=(/itime+1/))
     iret = nf90_put_var(ncid, FSR_id,      FSR,           start=(/itime+1/))
     iret = nf90_put_var(ncid, FIRA_id,     FIRA,          start=(/itime+1/))
     iret = nf90_put_var(ncid, FSH_id,      FSH,           start=(/itime+1/))
     iret = nf90_put_var(ncid, SSOIL_id,    SSOIL,         start=(/itime+1/))
     iret = nf90_put_var(ncid, FCEV_id,     FCEV,          start=(/itime+1/))
     iret = nf90_put_var(ncid, FGEV_id,     FGEV,          start=(/itime+1/))
     iret = nf90_put_var(ncid, FCTR_id,     FCTR,          start=(/itime+1/))
     iret = nf90_put_var(ncid, ECAN_id,     ECAN,          start=(/itime+1/))
     iret = nf90_put_var(ncid, ETRAN_id,    ETRAN,         start=(/itime+1/))
     iret = nf90_put_var(ncid, EDIR_id,     EDIR,          start=(/itime+1/))
     iret = nf90_put_var(ncid, TRAD_id,     TRAD,          start=(/itime+1/))
     iret = nf90_put_var(ncid, TGV_id,      TGV,           start=(/itime+1/))
     iret = nf90_put_var(ncid, TGB_id,      TGB,           start=(/itime+1/))
     iret = nf90_put_var(ncid, T2MV_id,     T2MV,          start=(/itime+1/))
     iret = nf90_put_var(ncid, T2MB_id,     T2MB,          start=(/itime+1/))
     iret = nf90_put_var(ncid, Q2V_id,      Q2V,           start=(/itime+1/))
     iret = nf90_put_var(ncid, Q2B_id,      Q2B,           start=(/itime+1/))
     iret = nf90_put_var(ncid, RUNSRF_id,   RUNSRF,        start=(/itime+1/))
     iret = nf90_put_var(ncid, RUNSUB_id,   RUNSUB,        start=(/itime+1/))
     iret = nf90_put_var(ncid, PSN_id,      PSN,           start=(/itime+1/))
     iret = nf90_put_var(ncid, APAR_id,     APAR,          start=(/itime+1/))
     iret = nf90_put_var(ncid, SAV_id,      SAV,           start=(/itime+1/))
     iret = nf90_put_var(ncid, SAG_id,      SAG,           start=(/itime+1/))
     iret = nf90_put_var(ncid, FSNO_id,     FSNO,          start=(/itime+1/))
     iret = nf90_put_var(ncid, NEE_id,      NEE,           start=(/itime+1/))
     iret = nf90_put_var(ncid, GPP_id,      GPP,           start=(/itime+1/))
     iret = nf90_put_var(ncid, NPP_id,      NPP,           start=(/itime+1/))
     iret = nf90_put_var(ncid, FVEG_id,     FVEG,          start=(/itime+1/))
     iret = nf90_put_var(ncid, ALBEDO_id,   ALBEDO,        start=(/itime+1/))
     iret = nf90_put_var(ncid, QSNBOT_id,   QSNBOT,        start=(/itime+1/))
     iret = nf90_put_var(ncid, PONDING1_id, PONDING1,      start=(/itime+1/))
     iret = nf90_put_var(ncid, PONDING2_id, PONDING2,      start=(/itime+1/))
     iret = nf90_put_var(ncid, PONDING_id,  PONDING,       start=(/itime+1/))
     iret = nf90_put_var(ncid, RSSUN_id,    RSSUN,         start=(/itime+1/))
     iret = nf90_put_var(ncid, RSSHA_id,    RSSHA,         start=(/itime+1/))
     iret = nf90_put_var(ncid, ALBSND_id,   ALBSND,        start=(/itime+1,1/), count=(/1,2/))
     iret = nf90_put_var(ncid, ALBSNI_id,   ALBSNI,        start=(/itime+1,1/), count=(/1,2/))
     iret = nf90_put_var(ncid, BGAP_id,     BGAP,          start=(/itime+1/))
     iret = nf90_put_var(ncid, WGAP_id,     WGAP,          start=(/itime+1/))
     iret = nf90_put_var(ncid, CHV_id,      CHV,           start=(/itime+1/))
     iret = nf90_put_var(ncid, CHB_id,      CHB,           start=(/itime+1/))
     iret = nf90_put_var(ncid, EMISSI_id,   EMISSI,        start=(/itime+1/))
     iret = nf90_put_var(ncid, SHG_id,      SHG,           start=(/itime+1/))
     iret = nf90_put_var(ncid, SHC_id,      SHC,           start=(/itime+1/))
     iret = nf90_put_var(ncid, SHB_id,      SHB,           start=(/itime+1/))
     iret = nf90_put_var(ncid, EVG_id,      EVG,           start=(/itime+1/))
     iret = nf90_put_var(ncid, EVC_id,      EVC,           start=(/itime+1/))
     iret = nf90_put_var(ncid, EVB_id,      EVB,           start=(/itime+1/))
     iret = nf90_put_var(ncid, GHV_id,      GHV,           start=(/itime+1/))
     iret = nf90_put_var(ncid, GHB_id,      GHB,           start=(/itime+1/))
     iret = nf90_put_var(ncid, IRG_id,      IRG,           start=(/itime+1/))
     iret = nf90_put_var(ncid, IRC_id,      IRC,           start=(/itime+1/))
     iret = nf90_put_var(ncid, IRB_id,      IRB,           start=(/itime+1/))
     iret = nf90_put_var(ncid, TR_id,       TR,            start=(/itime+1/))
     iret = nf90_put_var(ncid, CHV2_id,     CHV2,          start=(/itime+1/))
     iret = nf90_put_var(ncid, CHLEAF_id,   CHLEAF,        start=(/itime+1/))
     iret = nf90_put_var(ncid, CHUC_id,     CHUC,          start=(/itime+1/))
     iret = nf90_put_var(ncid, CHB2_id,     CHB2,          start=(/itime+1/))
     iret = nf90_put_var(ncid, FPICE_id,    FPICE,         start=(/itime+1/))
     iret = nf90_put_var(ncid, PAH_id,      PAH,           start=(/itime+1/))
     iret = nf90_put_var(ncid, PAHV_id,     PAHV,          start=(/itime+1/))
     iret = nf90_put_var(ncid, PAHG_id,     PAHG,          start=(/itime+1/))
     iret = nf90_put_var(ncid, PAHB_id,     PAHB,          start=(/itime+1/))
     iret = nf90_put_var(ncid, LAISUN_id,   LAISUN,        start=(/itime+1/))
     iret = nf90_put_var(ncid, LAISHA_id,   LAISHA,        start=(/itime+1/))
     iret = nf90_put_var(ncid, FICEOLD_id,  FICEOLD,       start=(/itime+1,1/), count=(/1,nsnow/))


   end subroutine add_to_output

   subroutine finalize_output()

     iret = nf90_close(ncid)

   end subroutine finalize_output
   
end module noahmp_output

