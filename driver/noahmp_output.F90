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
  integer           :: ISNOW_id
  integer           :: CANLIQ_id
  integer           :: CANICE_id
  integer           :: TV_id
  integer           :: SNOWH_id
  integer           :: SNEQV_id
  integer           :: SNICE_id
  integer           :: SNLIQ_id
  integer           :: STC_id
  integer           :: ZSNSO_id
  integer           :: SH2O_id
  integer           :: SMC_id
  integer           :: SICE_id
  integer           :: ZWT_id
  integer           :: WA_id
  integer           :: WT_id
  integer           :: DZSNSO_id
  integer           :: WSLAKE_id
  integer           :: SMCWTD_id
  integer           :: DEEPRECH_id
  integer           :: RECH_id
  integer           :: CMC_id
  integer           :: ECAN_id
  integer           :: ETRAN_id
  integer           :: FWET_id
  integer           :: RUNSRF_id
  integer           :: RUNSUB_id
  integer           :: QIN_id
  integer           :: QDIS_id
  integer           :: PONDING1_id
  integer           :: PONDING2_id
  integer           :: QSNBOT_id
  integer           :: QTLDRN_id
  integer           :: QINSUR_id
  integer           :: QSEVA_id
  integer           :: QSDEW_id
  integer           :: QSNFRO_id
  integer           :: QSNSUB_id
  integer           :: ETRANI_id
  integer           :: WCND_id
  integer           :: QDRAIN_id
  integer           :: SNOFLOW_id
  integer           :: FCRMAX_id
  integer           :: FICEOLD_id
  integer           :: errwat_id
  integer           :: QRAIN_id
  integer           :: QSNOW_id
  integer           :: QVAP_id
  integer           :: IRAMTFI_id
  integer           :: IRAMTMI_id
  integer           :: IRFIRATE_id
  integer           :: IRMIRATE_id
  integer           :: IRAMTSI_id
  integer           :: IRSIRATE_id
  integer           :: IRCNTSI_id
  integer           :: IRCNTMI_id
  integer           :: IRCNTFI_id
  integer           :: RAIN_id
  integer           :: SNOW_id
  integer           :: IREVPLOS_id
  integer           :: FIRR_id
  integer           :: EIRR_id
  integer           :: SNOWHIN_id
  integer           :: TG_id
  integer           :: QINTR_id
  integer           :: QDRIPR_id
  integer           :: QTHROR_id
  integer           :: QINTS_id
  integer           :: QDRIPS_id
  integer           :: QTHROS_id
  integer           :: PAHV_id
  integer           :: PAHG_id
  integer           :: PAHB_id
  integer           :: EDIR_id
! thermoprop new vars
  integer           :: DF_id
  integer           :: HCPCT_id
  integer           :: SNICEV_id
  integer           :: SNLIQV_id
  integer           :: EPORE_id
  integer           :: FACT_id
! radiation new vars
  integer           :: ALBOLD_id
  integer           :: TAUSS_id
  integer           :: FSUN_id
  integer           :: LAISUN_id
  integer           :: LAISHA_id
  integer           :: PARSUN_id
  integer           :: PARSHA_id
  integer           :: SAV_id
  integer           :: SAG_id
  integer           :: FSA_id
  integer           :: FSR_id
  integer           :: FSRV_id
  integer           :: FSRG_id
  integer           :: BGAP_id
  integer           :: WGAP_id
  integer           :: ALBSND_id
  integer           :: ALBSNI_id
  integer           :: SNEQVO_id
! vege_flux new vars
  integer           :: TAH_id
  integer           :: TGV_id
  integer           :: EAH_id
  integer           :: CMV_id
  integer           :: CM_id
  integer           :: CHV_id
  integer           :: CH_id
  integer           :: QSFC_id
  integer           :: RSSUN_id
  integer           :: RSSHA_id
  integer           :: TAUXV_id
  integer           :: TAUYV_id
  integer           :: IRG_id
  integer           :: IRC_id
  integer           :: SHG_id
  integer           :: SHC_id
  integer           :: EVG_id
  integer           :: EVC_id
  integer           :: TR_id
  integer           :: GHV_id
  integer           :: T2MV_id
  integer           :: PSNSUN_id
  integer           :: PSNSHA_id
  integer           :: Q2V_id
  integer           :: CHV2_id
  integer           :: CHLEAF_id
  integer           :: CHUC_id



contains

  subroutine initialize_output(output_filename, ntime, nsoil, nsnow)
 
    integer       :: ntime
    integer       :: nsoil
    integer       :: nsnow
    character*256 :: output_filename
 
    iret = nf90_create(trim(output_filename), NF90_CLOBBER, ncid)

    iret = nf90_def_dim(ncid, "time", ntime, time_dim)
    iret = nf90_def_dim(ncid, "soil", nsoil, soil_dim)
    iret = nf90_def_dim(ncid, "snow", nsnow, snow_dim)
    iret = nf90_def_dim(ncid, "snso", nsnow+nsoil, snso_dim)
    iret = nf90_def_dim(ncid, "band", 2, band_dim)
    iret = nf90_def_var(ncid, "ISNOW",       NF90_INT  , (/time_dim/), ISNOW_id)
    iret = nf90_def_var(ncid, "CANLIQ",      NF90_FLOAT, (/time_dim/), CANLIQ_id)
    iret = nf90_def_var(ncid, "CANICE",      NF90_FLOAT, (/time_dim/), CANICE_id)
    iret = nf90_def_var(ncid, "TV",          NF90_FLOAT, (/time_dim/), TV_id)
    iret = nf90_def_var(ncid, "SNOWH",       NF90_FLOAT, (/time_dim/), SNOWH_id)
    iret = nf90_def_var(ncid, "SNEQV",       NF90_FLOAT, (/time_dim/), SNEQV_id)
    iret = nf90_def_var(ncid, "SNICE",       NF90_FLOAT, (/time_dim,snow_dim/), SNICE_id)
    iret = nf90_def_var(ncid, "SNLIQ",       NF90_FLOAT, (/time_dim,snow_dim/), SNLIQ_id)
    iret = nf90_def_var(ncid, "STC",         NF90_FLOAT, (/time_dim,snso_dim/), STC_id)
    iret = nf90_def_var(ncid, "ZSNSO",       NF90_FLOAT, (/time_dim,snso_dim/), ZSNSO_id)
    iret = nf90_def_var(ncid, "SH2O",        NF90_FLOAT, (/time_dim,soil_dim/), SH2O_id)
    iret = nf90_def_var(ncid, "SMC",         NF90_FLOAT, (/time_dim,soil_dim/), SMC_id)
    iret = nf90_def_var(ncid, "SICE",        NF90_FLOAT, (/time_dim,soil_dim/), SICE_id)
    iret = nf90_def_var(ncid, "ZWT",         NF90_FLOAT, (/time_dim/), ZWT_id)
    iret = nf90_def_var(ncid, "WA",          NF90_FLOAT, (/time_dim/), WA_id)
    iret = nf90_def_var(ncid, "WT",          NF90_FLOAT, (/time_dim/), WT_id)
    iret = nf90_def_var(ncid, "DZSNSO",      NF90_FLOAT, (/time_dim,snso_dim/), DZSNSO_id)
    iret = nf90_def_var(ncid, "WSLAKE",      NF90_FLOAT, (/time_dim/), WSLAKE_id)
    iret = nf90_def_var(ncid, "SMCWTD",      NF90_FLOAT, (/time_dim/), SMCWTD_id)
    iret = nf90_def_var(ncid, "DEEPRECH",    NF90_FLOAT, (/time_dim/), DEEPRECH_id)
    iret = nf90_def_var(ncid, "RECH",        NF90_FLOAT, (/time_dim/), RECH_id)
    iret = nf90_def_var(ncid, "IRAMTFI",     NF90_FLOAT, (/time_dim/), IRAMTFI_id)
    iret = nf90_def_var(ncid, "IRAMTMI",     NF90_FLOAT, (/time_dim/), IRAMTMI_id)
    iret = nf90_def_var(ncid, "IRFIRATE",    NF90_FLOAT, (/time_dim/), IRFIRATE_id)
    iret = nf90_def_var(ncid, "IRMIRATE",    NF90_FLOAT, (/time_dim/), IRMIRATE_id) 
    iret = nf90_def_var(ncid, "CMC",         NF90_FLOAT, (/time_dim/), CMC_id)
    iret = nf90_def_var(ncid, "ECAN",        NF90_FLOAT, (/time_dim/), ECAN_id)
    iret = nf90_def_var(ncid, "ETRAN",       NF90_FLOAT, (/time_dim/), ETRAN_id)
    iret = nf90_def_var(ncid, "FWET",        NF90_FLOAT, (/time_dim/), FWET_id)
    iret = nf90_def_var(ncid, "RUNSRF",      NF90_FLOAT, (/time_dim/), RUNSRF_id)
    iret = nf90_def_var(ncid, "RUNSUB",      NF90_FLOAT, (/time_dim/), RUNSUB_id)
    iret = nf90_def_var(ncid, "QIN",         NF90_FLOAT, (/time_dim/), QIN_id)
    iret = nf90_def_var(ncid, "QDIS",        NF90_FLOAT, (/time_dim/), QDIS_id)
    iret = nf90_def_var(ncid, "PONDING1",    NF90_FLOAT, (/time_dim/), PONDING1_id)
    iret = nf90_def_var(ncid, "PONDING2",    NF90_FLOAT, (/time_dim/), PONDING2_id)
    iret = nf90_def_var(ncid, "QSNBOT",      NF90_FLOAT, (/time_dim/), QSNBOT_id)
    iret = nf90_def_var(ncid, "QTLDRN",      NF90_FLOAT, (/time_dim/), QTLDRN_id)
    iret = nf90_def_var(ncid, "QINSUR",      NF90_FLOAT, (/time_dim/), QINSUR_id)
    iret = nf90_def_var(ncid, "QSEVA",       NF90_FLOAT, (/time_dim/), QSEVA_id)
    iret = nf90_def_var(ncid, "QSDEW",       NF90_FLOAT, (/time_dim/), QSDEW_id)
    iret = nf90_def_var(ncid, "QSNFRO",      NF90_FLOAT, (/time_dim/), QSNFRO_id)
    iret = nf90_def_var(ncid, "QSNSUB",      NF90_FLOAT, (/time_dim/), QSNSUB_id)
    iret = nf90_def_var(ncid, "ETRANI",      NF90_FLOAT, (/time_dim,soil_dim/), ETRANI_id)
    iret = nf90_def_var(ncid, "WCND",        NF90_FLOAT, (/time_dim,soil_dim/), WCND_id)
    iret = nf90_def_var(ncid, "QDRAIN",      NF90_FLOAT, (/time_dim/), QDRAIN_id)
    iret = nf90_def_var(ncid, "SNOFLOW",     NF90_FLOAT, (/time_dim/), SNOFLOW_id)
    iret = nf90_def_var(ncid, "FCRMAX",      NF90_FLOAT, (/time_dim/), FCRMAX_id)
    iret = nf90_def_var(ncid, "FICEOLD",     NF90_FLOAT, (/time_dim,snow_dim/), FICEOLD_id)
    iret = nf90_def_var(ncid, "errwat",      NF90_FLOAT, (/time_dim/), errwat_id)
    iret = nf90_def_var(ncid, "QRAIN",       NF90_FLOAT, (/time_dim/), QRAIN_id)
    iret = nf90_def_var(ncid, "QSNOW",       NF90_FLOAT, (/time_dim/), QSNOW_id)
    iret = nf90_def_var(ncid, "QVAP",        NF90_FLOAT, (/time_dim/), QVAP_id)
    iret = nf90_def_var(ncid, "IRAMTSI",     NF90_FLOAT, (/time_dim/), IRAMTSI_id)
    iret = nf90_def_var(ncid, "IRSIRATE",    NF90_FLOAT, (/time_dim/), IRSIRATE_id)
    iret = nf90_def_var(ncid, "IRCNTSI",     NF90_FLOAT, (/time_dim/), IRCNTSI_id)
    iret = nf90_def_var(ncid, "IRCNTMI",     NF90_FLOAT, (/time_dim/), IRCNTMI_id)
    iret = nf90_def_var(ncid, "IRCNTFI",     NF90_FLOAT, (/time_dim/), IRCNTFI_id)
    iret = nf90_def_var(ncid, "RAIN",        NF90_FLOAT, (/time_dim/), RAIN_id)
    iret = nf90_def_var(ncid, "SNOW",        NF90_FLOAT, (/time_dim/), SNOW_id)
    iret = nf90_def_var(ncid, "IREVPLOS",    NF90_FLOAT, (/time_dim/), IREVPLOS_id)
    iret = nf90_def_var(ncid, "FIRR",        NF90_FLOAT, (/time_dim/), FIRR_id)
    iret = nf90_def_var(ncid, "EIRR",        NF90_FLOAT, (/time_dim/), EIRR_id)
    iret = nf90_def_var(ncid, "SNOWHIN",     NF90_FLOAT, (/time_dim/), SNOWHIN_id)
    iret = nf90_def_var(ncid, "TG",          NF90_FLOAT, (/time_dim/), TG_id)
    iret = nf90_def_var(ncid, "QINTR",       NF90_FLOAT, (/time_dim/), QINTR_id)
    iret = nf90_def_var(ncid, "QDRIPR",      NF90_FLOAT, (/time_dim/), QDRIPR_id)
    iret = nf90_def_var(ncid, "QTHROR",      NF90_FLOAT, (/time_dim/), QTHROR_id)
    iret = nf90_def_var(ncid, "QINTS",       NF90_FLOAT, (/time_dim/), QINTS_id)
    iret = nf90_def_var(ncid, "QDRIPS",      NF90_FLOAT, (/time_dim/), QDRIPS_id)
    iret = nf90_def_var(ncid, "QTHROS",      NF90_FLOAT, (/time_dim/), QTHROS_id)
    iret = nf90_def_var(ncid, "PAHV",        NF90_FLOAT, (/time_dim/), PAHV_id)
    iret = nf90_def_var(ncid, "PAHG",        NF90_FLOAT, (/time_dim/), PAHG_id)
    iret = nf90_def_var(ncid, "PAHB",        NF90_FLOAT, (/time_dim/), PAHB_id)
    iret = nf90_def_var(ncid, "EDIR",        NF90_FLOAT, (/time_dim/), EDIR_id)
! thermoprop new vars
    iret = nf90_def_var(ncid, "DF",          NF90_FLOAT, (/time_dim,snso_dim/), DF_id)
    iret = nf90_def_var(ncid, "HCPCT",       NF90_FLOAT, (/time_dim,snso_dim/), HCPCT_id)
    iret = nf90_def_var(ncid, "FACT",        NF90_FLOAT, (/time_dim,snso_dim/), FACT_id)
    iret = nf90_def_var(ncid, "SNLIQV",      NF90_FLOAT, (/time_dim,snow_dim/), SNLIQV_id)
    iret = nf90_def_var(ncid, "EPORE",       NF90_FLOAT, (/time_dim,snow_dim/), EPORE_id)
    iret = nf90_def_var(ncid, "SNICEV",      NF90_FLOAT, (/time_dim,snow_dim/), SNICEV_id)
! radiation new vars
    iret = nf90_def_var(ncid, "ALBOLD",      NF90_FLOAT, (/time_dim/), ALBOLD_id)
    iret = nf90_def_var(ncid, "TAUSS",       NF90_FLOAT, (/time_dim/), TAUSS_id)
    iret = nf90_def_var(ncid, "FSUN",        NF90_FLOAT, (/time_dim/), FSUN_id)
    iret = nf90_def_var(ncid, "LAISUN",      NF90_FLOAT, (/time_dim/), LAISUN_id)
    iret = nf90_def_var(ncid, "LAISHA",      NF90_FLOAT, (/time_dim/), LAISHA_id)
    iret = nf90_def_var(ncid, "PARSUN",      NF90_FLOAT, (/time_dim/), PARSUN_id)
    iret = nf90_def_var(ncid, "PARSHA",      NF90_FLOAT, (/time_dim/), PARSHA_id)
    iret = nf90_def_var(ncid, "SAV",         NF90_FLOAT, (/time_dim/), SAV_id)
    iret = nf90_def_var(ncid, "SAG",         NF90_FLOAT, (/time_dim/), SAG_id)
    iret = nf90_def_var(ncid, "FSA",         NF90_FLOAT, (/time_dim/), FSA_id)
    iret = nf90_def_var(ncid, "FSR",         NF90_FLOAT, (/time_dim/), FSR_id)
    iret = nf90_def_var(ncid, "FSRV",        NF90_FLOAT, (/time_dim/), FSRV_id)
    iret = nf90_def_var(ncid, "FSRG",        NF90_FLOAT, (/time_dim/), FSRG_id)
    iret = nf90_def_var(ncid, "BGAP",        NF90_FLOAT, (/time_dim/), BGAP_id)
    iret = nf90_def_var(ncid, "WGAP",        NF90_FLOAT, (/time_dim/), WGAP_id)
    iret = nf90_def_var(ncid, "SNEQVO",      NF90_FLOAT, (/time_dim/), SNEQVO_id)
    iret = nf90_def_var(ncid, "ALBSND",      NF90_FLOAT, (/time_dim,band_dim/), ALBSND_id)
    iret = nf90_def_var(ncid, "ALBSNI",      NF90_FLOAT, (/time_dim,band_dim/), ALBSNI_id)
! vege_flux new vars
    iret = nf90_def_var(ncid, "TAH",         NF90_FLOAT, (/time_dim/), TAH_id)
    iret = nf90_def_var(ncid, "TGV",         NF90_FLOAT, (/time_dim/), TGV_id)
    iret = nf90_def_var(ncid, "EAH",         NF90_FLOAT, (/time_dim/), EAH_id)
    iret = nf90_def_var(ncid, "CMV",         NF90_FLOAT, (/time_dim/), CMV_id)
    iret = nf90_def_var(ncid, "CM",          NF90_FLOAT, (/time_dim/), CM_id)
    iret = nf90_def_var(ncid, "CHV",         NF90_FLOAT, (/time_dim/), CHV_id)
    iret = nf90_def_var(ncid, "CH",          NF90_FLOAT, (/time_dim/), CH_id)
    iret = nf90_def_var(ncid, "QSFC",        NF90_FLOAT, (/time_dim/), QSFC_id)
    iret = nf90_def_var(ncid, "RSSUN",       NF90_FLOAT, (/time_dim/), RSSUN_id)
    iret = nf90_def_var(ncid, "RSSHA",       NF90_FLOAT, (/time_dim/), RSSHA_id)
    iret = nf90_def_var(ncid, "TAUXV",       NF90_FLOAT, (/time_dim/), TAUXV_id)
    iret = nf90_def_var(ncid, "TAUYV",       NF90_FLOAT, (/time_dim/), TAUYV_id)
    iret = nf90_def_var(ncid, "IRG",         NF90_FLOAT, (/time_dim/), IRG_id)
    iret = nf90_def_var(ncid, "IRC",         NF90_FLOAT, (/time_dim/), IRC_id)
    iret = nf90_def_var(ncid, "SHG",         NF90_FLOAT, (/time_dim/), SHG_id)
    iret = nf90_def_var(ncid, "SHC",         NF90_FLOAT, (/time_dim/), SHC_id)
    iret = nf90_def_var(ncid, "EVG",         NF90_FLOAT, (/time_dim/), EVG_id)
    iret = nf90_def_var(ncid, "EVC",         NF90_FLOAT, (/time_dim/), EVC_id)
    iret = nf90_def_var(ncid, "TR",          NF90_FLOAT, (/time_dim/), TR_id)
    iret = nf90_def_var(ncid, "GHV",         NF90_FLOAT, (/time_dim/), GHV_id)
    iret = nf90_def_var(ncid, "T2MV",        NF90_FLOAT, (/time_dim/), T2MV_id)
    iret = nf90_def_var(ncid, "PSNSUN",      NF90_FLOAT, (/time_dim/), PSNSUN_id)
    iret = nf90_def_var(ncid, "PSNSHA",      NF90_FLOAT, (/time_dim/), PSNSHA_id)
    iret = nf90_def_var(ncid, "Q2V",         NF90_FLOAT, (/time_dim/), Q2V_id)
    iret = nf90_def_var(ncid, "CHV2",        NF90_FLOAT, (/time_dim/), CHV2_id)
    iret = nf90_def_var(ncid, "CHLEAF",      NF90_FLOAT, (/time_dim/), CHLEAF_id)
    iret = nf90_def_var(ncid, "CHUC",        NF90_FLOAT, (/time_dim/), CHUC_id)


    iret = nf90_enddef(ncid)
  
   end subroutine initialize_output

  subroutine add_to_output(itime,NSOIL,NSNOW,ISNOW,CANLIQ,CANICE,TV,SNOWH,SNEQV,&
                     SNICE,SNLIQ,STC,ZSNSO,SH2O,SMC,SICE,ZWT,WA,WT,DZSNSO,&
                     WSLAKE,SMCWTD,DEEPRECH,RECH,IRAMTFI,IRAMTMI,IRFIRATE,IRMIRATE,&
                     CMC,ECAN,ETRAN,FWET,RUNSRF,RUNSUB,QIN,QDIS,PONDING1,PONDING2,&
                     QSNBOT,QTLDRN,QINSUR,QSEVA,QSDEW,QSNFRO,QSNSUB,ETRANI,&
                     WCND,QDRAIN,SNOFLOW,FCRMAX,FICEOLD,errwat,QRAIN,QSNOW,QVAP,&
                     IRAMTSI,IRSIRATE,IRCNTSI,IRCNTMI,IRCNTFI,RAIN,SNOW,IREVPLOS,FIRR,EIRR,&
                     SNOWHIN,TG,QINTR,QDRIPR,QTHROR,QINTS,QDRIPS,QTHROS,PAHV,PAHG,PAHB,EDIR,&
                     DF,HCPCT,SNICEV,SNLIQV,EPORE,FACT,FSUN,LAISUN,LAISHA,PARSUN,PARSHA,SAV,&
                     SAG,FSA,FSR,FSRV,FSRG,BGAP,WGAP,ALBSND,ALBSNI,ALBOLD,TAUSS,SNEQVO,&
                     TAH,TGV,EAH,CMV,CM,CHV,CH,QSFC,RSSUN,RSSHA,TAUXV,TAUYV,IRG,IRC,SHG,SHC,&
                     EVG,EVC,TR,GHV,T2MV,PSNSUN,PSNSHA,Q2V,CHV2,CHLEAF,CHUC)

     integer                       :: itime
     integer                       :: nsoil
     integer                       :: nsnow
     real                          :: TV      ! canopy temperature
     real, dimension(nsnow)        :: FICEOLD !ice fraction at last timestep
     real, dimension(nsoil+nsnow)  :: ZSNSO   !depth of snow/soil layer-bottom
     integer                       :: ISNOW   !actual no. of snow layers
     real                          :: CANLIQ  !intercepted liquid water (mm)
     real                          :: CANICE  !intercepted ice mass (mm)
     REAL                          :: SNOWH   !snow height [m]
     REAL                          :: SNEQV   !snow water eqv. [mm]
     real, dimension(nsnow)        :: SNICE   !snow layer ice [mm]
     real, dimension(nsnow)        :: SNLIQ   !snow layer liquid water [mm]
     real, dimension(nsoil+nsnow)  :: STC     !snow/soil layer temperature [k]
     real, dimension(nsoil)        :: SICE    ! soil ice content [m3/m3]
     real, dimension(nsoil)        :: SH2O    ! soil liquid water content [m3/m3]
     real, dimension(nsoil)        :: SMC         !total soil water content [m3/m3]
     real                          :: ZWT        !the depth to water table [m]
     real                          :: WA      !water storage in aquifer [mm]
     real                          :: WT      !water storage in aquifer + stuarated soil [mm]
     REAL                          :: WSLAKE  !water storage in lake (can be -) (mm)
     real                          :: SMCWTD      !soil water content between bottom of the soil and water table [m3/m3]
     real                          :: DEEPRECH    !recharge to or from the water table when deep [m]
     real                          :: RECH !recharge to or from the water table when shallow [m] (diagnostic)
     real                          :: IRAMTFI  ! irrigation water amount [m] to be applied, flood
     real                          :: IRAMTMI  ! irrigation water amount [m] to be applied, Micro
     real                          :: IRFIRATE ! rate of irrigation by flood [m/timestep]
     real                          :: IRMIRATE ! rate of irrigation by micro [m/timestep]
     real                          :: CMC     !intercepted water per ground area (mm)
     real                          :: ECAN    !evap of intercepted water (mm/s) [+]
     real                          :: ETRAN   !transpiration rate (mm/s) [+]
     real                          :: FWET    !wetted/snowed fraction of canopy (-)
     real                          :: RUNSRF      !surface runoff [mm/s] 
     real                          :: RUNSUB      !baseflow (sturation excess) [mm/s]
     real                          :: QIN     !groundwater recharge [mm/s]
     real                          :: QDIS    !groundwater discharge [mm/s]
     REAL                          :: PONDING1 ![mm]
     REAL                          :: PONDING2 ![mm]
     REAL                          :: QSNBOT !melting water out of snow bottom [mm/s]
     real                          :: QTLDRN   !tile drainage (mm/s)
     real                          :: QINSUR      !water input on soil surface [m/s]
     real                          :: QSEVA   !soil surface evap rate [mm/s]
     real, dimension(nsoil)        :: ETRANI      !transpiration rate (mm/s) [+]
     REAL                          :: QSNFRO  !snow surface frost rate[mm/s]
     REAL                          :: QSNSUB  !snow surface sublimation rate [mm/s]
     REAL                          :: SNOFLOW !glacier flow [mm/s]
     REAL                          :: QSDEW   !soil surface dew rate [mm/s]
     real                          :: QDRAIN      !soil-bottom free drainage [mm/s] 
     real                          :: FCRMAX      !maximum of fcr (-)
     real, dimension(nsoil)        :: WCND        !hydraulic conductivity (m/s)
     real                          :: errwat ! water balance error at each timestep [mm]
     real, dimension(nsoil+nsnow)  :: DZSNSO  ! snow/soil layer thickness [m]
     real                          :: QRAIN
     real                          :: QSNOW
     real                          :: QVAP
     real                          :: IRAMTSI
     real                          :: IRSIRATE
     integer                       :: IRCNTSI
     integer                       :: IRCNTMI
     integer                       :: IRCNTFI
     real                          :: RAIN
     real                          :: SNOW
     real                          :: IREVPLOS
     real                          :: FIRR
     real                          :: EIRR
     real                          :: SNOWHIN
     real                          :: TG
     real                          :: QINTR
     real                          :: QDRIPR
     real                          :: QTHROR
     real                          :: QINTS
     real                          :: QDRIPS
     real                          :: QTHROS
     real                          :: PAHV
     real                          :: PAHG
     real                          :: PAHB
     real                          :: EDIR
! thermoprop new vars
     real, dimension(nsoil+nsnow)  :: DF
     real, dimension(nsoil+nsnow)  :: HCPCT
     real, dimension(nsoil+nsnow)  :: FACT
     real, dimension(nsnow)        :: SNICEV
     real, dimension(nsnow)        :: SNLIQV
     real, dimension(nsnow)        :: EPORE
! radiation new vars
     real                          :: SNEQVO 
     real                          :: ALBOLD  
     real                          :: TAUSS   
     REAL                          :: FSUN    
     REAL                          :: LAISUN  
     REAL                          :: LAISHA  
     REAL                          :: PARSUN  
     REAL                          :: PARSHA  
     REAL                          :: SAV     
     REAL                          :: SAG     
     REAL                          :: FSA     
     REAL                          :: FSR     
     REAL                          :: FSRV   
     REAL                          :: FSRG
     REAL                          :: BGAP
     REAL                          :: WGAP
     REAL, DIMENSION(1:2)          :: ALBSND
     REAL, DIMENSION(1:2)          :: ALBSNI
! vege_flux new vars
     REAL                          :: TAH
     REAL                          :: TGV
     REAL                          :: EAH
     REAL                          :: CMV
     REAL                          :: CM
     REAL                          :: CHV
     REAL                          :: CH
     REAL                          :: QSFC
     REAL                          :: RSSUN
     REAL                          :: RSSHA
     REAL                          :: TAUXV
     REAL                          :: TAUYV
     REAL                          :: IRG
     REAL                          :: IRC
     REAL                          :: SHG
     REAL                          :: SHC
     REAL                          :: EVG
     REAL                          :: EVC
     REAL                          :: TR
     REAL                          :: GHV
     REAL                          :: T2MV
     REAL                          :: PSNSUN
     REAL                          :: PSNSHA
     REAL                          :: Q2V
     REAL                          :: CHV2
     REAL                          :: CHLEAF
     REAL                          :: CHUC




! assign values
     iret = nf90_put_var(ncid, ISNOW_id,    ISNOW,         start=(/itime+1/))
     iret = nf90_put_var(ncid, CANLIQ_id,   CANLIQ,        start=(/itime+1/))
     iret = nf90_put_var(ncid, CANICE_id,   CANICE,        start=(/itime+1/))
     iret = nf90_put_var(ncid, TV_id,       TV,            start=(/itime+1/))
     iret = nf90_put_var(ncid, SNOWH_id,    SNOWH,         start=(/itime+1/))
     iret = nf90_put_var(ncid, SNEQV_id,    SNEQV,         start=(/itime+1/))
     iret = nf90_put_var(ncid, SNICE_id,    SNICE,         start=(/itime+1,1/), count=(/1,nsnow/))
     iret = nf90_put_var(ncid, SNLIQ_id,    SNLIQ,         start=(/itime+1,1/), count=(/1,nsnow/))
     iret = nf90_put_var(ncid, STC_id,      STC,           start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
     iret = nf90_put_var(ncid, ZSNSO_id,    ZSNSO,         start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
     iret = nf90_put_var(ncid, SH2O_id,     SH2O,          start=(/itime+1,1/), count=(/1,nsoil/))
     iret = nf90_put_var(ncid, SMC_id,      SMC,           start=(/itime+1,1/), count=(/1,nsoil/))
     iret = nf90_put_var(ncid, SICE_id,     SICE,          start=(/itime+1,1/), count=(/1,nsoil/))
     iret = nf90_put_var(ncid, ZWT_id,      ZWT,           start=(/itime+1/))
     iret = nf90_put_var(ncid, WA_id,       WA,            start=(/itime+1/))
     iret = nf90_put_var(ncid, WT_id,       WT,            start=(/itime+1/))
     iret = nf90_put_var(ncid, DZSNSO_id,   DZSNSO,        start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
     iret = nf90_put_var(ncid, WSLAKE_id,   WSLAKE,        start=(/itime+1/))
     iret = nf90_put_var(ncid, SMCWTD_id,   SMCWTD,        start=(/itime+1/))
     iret = nf90_put_var(ncid, DEEPRECH_id, DEEPRECH,      start=(/itime+1/))
     iret = nf90_put_var(ncid, RECH_id,     RECH,          start=(/itime+1/))
     iret = nf90_put_var(ncid, IRAMTFI_id,  IRAMTFI,       start=(/itime+1/))
     iret = nf90_put_var(ncid, IRAMTMI_id,  IRAMTMI,       start=(/itime+1/))
     iret = nf90_put_var(ncid, IRFIRATE_id, IRFIRATE,      start=(/itime+1/))
     iret = nf90_put_var(ncid, IRMIRATE_id, IRMIRATE,      start=(/itime+1/))
     iret = nf90_put_var(ncid, CMC_id,      CMC,           start=(/itime+1/))
     iret = nf90_put_var(ncid, ECAN_id,     ECAN,          start=(/itime+1/))
     iret = nf90_put_var(ncid, ETRAN_id,    ETRAN,         start=(/itime+1/))
     iret = nf90_put_var(ncid, FWET_id,     FWET,          start=(/itime+1/))
     iret = nf90_put_var(ncid, RUNSRF_id,   RUNSRF,        start=(/itime+1/))
     iret = nf90_put_var(ncid, RUNSUB_id,   RUNSUB,        start=(/itime+1/))
     iret = nf90_put_var(ncid, QIN_id,      QIN,           start=(/itime+1/))
     iret = nf90_put_var(ncid, QDIS_id,     QDIS,          start=(/itime+1/))
     iret = nf90_put_var(ncid, PONDING1_id, PONDING1,      start=(/itime+1/))
     iret = nf90_put_var(ncid, PONDING2_id, PONDING2,      start=(/itime+1/))
     iret = nf90_put_var(ncid, QSNBOT_id,   QSNBOT,        start=(/itime+1/))
     iret = nf90_put_var(ncid, QTLDRN_id,   QTLDRN,        start=(/itime+1/))
     iret = nf90_put_var(ncid, QINSUR_id,   QINSUR,        start=(/itime+1/))
     iret = nf90_put_var(ncid, QSEVA_id,    QSEVA,         start=(/itime+1/))
     iret = nf90_put_var(ncid, QSDEW_id,    QSDEW,         start=(/itime+1/))
     iret = nf90_put_var(ncid, QSNFRO_id,   QSNFRO,        start=(/itime+1/))
     iret = nf90_put_var(ncid, QSNSUB_id,   QSNSUB,        start=(/itime+1/))
     iret = nf90_put_var(ncid, ETRANI_id,   ETRANI,        start=(/itime+1,1/), count=(/1,nsoil/))
     iret = nf90_put_var(ncid, WCND_id,     WCND,          start=(/itime+1,1/), count=(/1,nsoil/))
     iret = nf90_put_var(ncid, QDRAIN_id,   QDRAIN,        start=(/itime+1/))
     iret = nf90_put_var(ncid, SNOFLOW_id,  SNOFLOW,       start=(/itime+1/))
     iret = nf90_put_var(ncid, FCRMAX_id,   FCRMAX,        start=(/itime+1/))
     iret = nf90_put_var(ncid, FICEOLD_id,  FICEOLD,       start=(/itime+1,1/), count=(/1,nsnow/))
     iret = nf90_put_var(ncid, errwat_id,   errwat,        start=(/itime+1/))
     iret = nf90_put_var(ncid, QRAIN_id,    QRAIN,         start=(/itime+1/))
     iret = nf90_put_var(ncid, QSNOW_id,    QSNOW,         start=(/itime+1/))
     iret = nf90_put_var(ncid, QVAP_id,     QVAP,          start=(/itime+1/))
     iret = nf90_put_var(ncid, IRAMTSI_id,  IRAMTSI,       start=(/itime+1/))
     iret = nf90_put_var(ncid, IRSIRATE_id, IRSIRATE,      start=(/itime+1/))
     iret = nf90_put_var(ncid, IRCNTSI_id,  IRCNTSI,       start=(/itime+1/))
     iret = nf90_put_var(ncid, IRCNTMI_id,  IRCNTMI,       start=(/itime+1/))
     iret = nf90_put_var(ncid, IRCNTFI_id,  IRCNTFI,       start=(/itime+1/))
     iret = nf90_put_var(ncid, RAIN_id,     RAIN,          start=(/itime+1/))
     iret = nf90_put_var(ncid, SNOW_id,     SNOW,          start=(/itime+1/))
     iret = nf90_put_var(ncid, IREVPLOS_id, IREVPLOS,      start=(/itime+1/))
     iret = nf90_put_var(ncid, FIRR_id,     FIRR,          start=(/itime+1/))
     iret = nf90_put_var(ncid, EIRR_id,     EIRR,          start=(/itime+1/))
     iret = nf90_put_var(ncid, SNOWHIN_id,  SNOWHIN,       start=(/itime+1/))
     iret = nf90_put_var(ncid, TG_id,       TG,            start=(/itime+1/))
     iret = nf90_put_var(ncid, QINTR_id,    QINTR,         start=(/itime+1/))
     iret = nf90_put_var(ncid, QDRIPR_id,   QDRIPR,        start=(/itime+1/))
     iret = nf90_put_var(ncid, QTHROR_id,   QTHROR,        start=(/itime+1/))
     iret = nf90_put_var(ncid, QINTS_id,    QINTS,         start=(/itime+1/))
     iret = nf90_put_var(ncid, QDRIPS_id,   QDRIPS,        start=(/itime+1/))
     iret = nf90_put_var(ncid, QTHROS_id,   QTHROS,        start=(/itime+1/))
     iret = nf90_put_var(ncid, PAHV_id,     PAHV,          start=(/itime+1/))
     iret = nf90_put_var(ncid, PAHG_id,     PAHG,          start=(/itime+1/))
     iret = nf90_put_var(ncid, PAHB_id,     PAHB,          start=(/itime+1/))
     iret = nf90_put_var(ncid, EDIR_id,     EDIR,          start=(/itime+1/))
! thermoprop new vars
     iret = nf90_put_var(ncid, DF_id,       DF,            start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
     iret = nf90_put_var(ncid, HCPCT_id,    HCPCT,         start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
     iret = nf90_put_var(ncid, FACT_id,     FACT,          start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
     iret = nf90_put_var(ncid, SNICEV_id,   SNICEV,        start=(/itime+1,1/), count=(/1,nsnow/))
     iret = nf90_put_var(ncid, SNLIQV_id,   SNLIQV,        start=(/itime+1,1/), count=(/1,nsnow/))
     iret = nf90_put_var(ncid, EPORE_id,    EPORE,         start=(/itime+1,1/), count=(/1,nsnow/))
! radiation new vars
     iret = nf90_put_var(ncid, ALBOLD_id,   ALBOLD,        start=(/itime+1/))
     iret = nf90_put_var(ncid, TAUSS_id,    TAUSS,         start=(/itime+1/))
     iret = nf90_put_var(ncid, FSUN_id,     FSUN,          start=(/itime+1/))
     iret = nf90_put_var(ncid, LAISUN_id,   LAISUN,        start=(/itime+1/))
     iret = nf90_put_var(ncid, LAISHA_id,   LAISHA,        start=(/itime+1/))
     iret = nf90_put_var(ncid, PARSUN_id,   PARSUN,        start=(/itime+1/))
     iret = nf90_put_var(ncid, PARSHA_id,   PARSHA,        start=(/itime+1/))
     iret = nf90_put_var(ncid, SAV_id,      SAV,           start=(/itime+1/))
     iret = nf90_put_var(ncid, SAG_id,      SAG,           start=(/itime+1/))
     iret = nf90_put_var(ncid, FSA_id,      FSA,           start=(/itime+1/))
     iret = nf90_put_var(ncid, FSR_id,      FSR,           start=(/itime+1/))
     iret = nf90_put_var(ncid, FSRV_id,     FSRV,          start=(/itime+1/))
     iret = nf90_put_var(ncid, FSRG_id,     FSRG,          start=(/itime+1/))
     iret = nf90_put_var(ncid, BGAP_id,     BGAP,          start=(/itime+1/))
     iret = nf90_put_var(ncid, WGAP_id,     WGAP,          start=(/itime+1/))
     iret = nf90_put_var(ncid, SNEQVO_id,   SNEQVO,        start=(/itime+1/))
     iret = nf90_put_var(ncid, ALBSND_id,   ALBSND,        start=(/itime+1,1/), count=(/1,2/))
     iret = nf90_put_var(ncid, ALBSNI_id,   ALBSNI,        start=(/itime+1,1/), count=(/1,2/))
! vege_flux new vars
     iret = nf90_put_var(ncid, TAH_id,      TAH,           start=(/itime+1/))
     iret = nf90_put_var(ncid, TGV_id,      TGV,           start=(/itime+1/))
     iret = nf90_put_var(ncid, EAH_id,      EAH,           start=(/itime+1/))
     iret = nf90_put_var(ncid, CMV_id,      CMV,           start=(/itime+1/))
     iret = nf90_put_var(ncid, CM_id,       CM,            start=(/itime+1/))
     iret = nf90_put_var(ncid, CHV_id,      CHV,           start=(/itime+1/))
     iret = nf90_put_var(ncid, CH_id,       CH,            start=(/itime+1/))
     iret = nf90_put_var(ncid, QSFC_id,     QSFC,          start=(/itime+1/))
     iret = nf90_put_var(ncid, RSSUN_id,    RSSUN,         start=(/itime+1/))
     iret = nf90_put_var(ncid, RSSHA_id,    RSSHA,         start=(/itime+1/))
     iret = nf90_put_var(ncid, TAUXV_id,    TAUXV,         start=(/itime+1/))
     iret = nf90_put_var(ncid, TAUYV_id,    TAUYV,         start=(/itime+1/))
     iret = nf90_put_var(ncid, IRG_id,      IRG,           start=(/itime+1/))
     iret = nf90_put_var(ncid, IRC_id,      IRC,           start=(/itime+1/))
     iret = nf90_put_var(ncid, SHG_id,      SHG,           start=(/itime+1/))
     iret = nf90_put_var(ncid, SHC_id,      SHC,           start=(/itime+1/))
     iret = nf90_put_var(ncid, EVG_id,      EVG,           start=(/itime+1/))
     iret = nf90_put_var(ncid, EVC_id,      EVC,           start=(/itime+1/))
     iret = nf90_put_var(ncid, TR_id,       TR,            start=(/itime+1/))
     iret = nf90_put_var(ncid, GHV_id,      GHV,           start=(/itime+1/))
     iret = nf90_put_var(ncid, T2MV_id,     T2MV,          start=(/itime+1/))
     iret = nf90_put_var(ncid, PSNSUN_id,   PSNSUN,        start=(/itime+1/))
     iret = nf90_put_var(ncid, PSNSHA_id,   PSNSHA,        start=(/itime+1/))
     iret = nf90_put_var(ncid, Q2V_id,      Q2V,           start=(/itime+1/))
     iret = nf90_put_var(ncid, CHV2_id,     CHV2,          start=(/itime+1/))
     iret = nf90_put_var(ncid, CHLEAF_id,   CHLEAF,        start=(/itime+1/))
     iret = nf90_put_var(ncid, CHUC_id,     CHUC,          start=(/itime+1/))


   end subroutine add_to_output

   subroutine finalize_output()

     iret = nf90_close(ncid)

   end subroutine finalize_output
   
end module noahmp_output

