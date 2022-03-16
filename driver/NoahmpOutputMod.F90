module NoahmpOutputMod

  use netcdf
  use NoahmpVarType
  use InputVarType
  use Machine, only : kind_noahmp

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

  subroutine initialize_output(noahmp, input, ntime)

    implicit none

    integer          , intent(in)    :: ntime
    type(input_type) , intent(inout) :: input
    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
 
    iret = nf90_create(trim(input%output_filename), NF90_CLOBBER, ncid)
    ! dimension variable
    iret = nf90_def_dim(ncid, "time", ntime, time_dim)
    iret = nf90_def_dim(ncid, "soil", noahmp%config%domain%NSOIL, soil_dim)
    iret = nf90_def_dim(ncid, "snow", noahmp%config%domain%NSNOW, snow_dim)
    iret = nf90_def_dim(ncid, "snso", noahmp%config%domain%NSOIL + &
                                      noahmp%config%domain%NSNOW, snso_dim)
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

  subroutine add_to_output(itime, noahmp, CHB2)

    implicit none

    integer               , intent(in)    :: itime
    type(noahmp_type)     , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in)    :: CHB2

! --------------------------------------------------------------------
    associate(                                           &
              nsnow     =>  noahmp%config%domain%NSNOW  ,&
              nsoil     =>  noahmp%config%domain%NSOIL   &
             ) 
! --------------------------------------------------------------------

! assign values
     iret = nf90_put_var(ncid, ALBOLD_id,   noahmp%energy%state%ALBOLD,        start=(/itime+1/))
     iret = nf90_put_var(ncid, SNEQVO_id,   noahmp%water%state%SNEQVO,         start=(/itime+1/))
     iret = nf90_put_var(ncid, STC_id,      noahmp%energy%state%STC,           start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
     iret = nf90_put_var(ncid, SH2O_id,     noahmp%water%state%SH2O,           start=(/itime+1,1/), count=(/1,nsoil/))
     iret = nf90_put_var(ncid, SMC_id,      noahmp%water%state%SMC,            start=(/itime+1,1/), count=(/1,nsoil/))
     iret = nf90_put_var(ncid, TAH_id,      noahmp%energy%state%TAH,           start=(/itime+1/))
     iret = nf90_put_var(ncid, EAH_id,      noahmp%energy%state%EAH,           start=(/itime+1/))
     iret = nf90_put_var(ncid, FWET_id,     noahmp%water%state%FWET,           start=(/itime+1/))
     iret = nf90_put_var(ncid, CANLIQ_id,   noahmp%water%state%CANLIQ,         start=(/itime+1/))
     iret = nf90_put_var(ncid, CANICE_id,   noahmp%water%state%CANICE,         start=(/itime+1/))
     iret = nf90_put_var(ncid, TV_id,       noahmp%energy%state%TV,            start=(/itime+1/))
     iret = nf90_put_var(ncid, TG_id,       noahmp%energy%state%TG,            start=(/itime+1/))
     iret = nf90_put_var(ncid, QSFC_id,     noahmp%energy%state%QSFC,          start=(/itime+1/))
     iret = nf90_put_var(ncid, QRAIN_id,    noahmp%water%flux%QRAIN,           start=(/itime+1/))
     iret = nf90_put_var(ncid, QSNOW_id,    noahmp%water%flux%QSNOW,           start=(/itime+1/))
     iret = nf90_put_var(ncid, ISNOW_id,    noahmp%config%domain%ISNOW,        start=(/itime+1/))
     iret = nf90_put_var(ncid, ZSNSO_id,    noahmp%config%domain%ZSNSO,        start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
     iret = nf90_put_var(ncid, SNOWH_id,    noahmp%water%state%SNOWH,          start=(/itime+1/))
     iret = nf90_put_var(ncid, SNEQV_id,    noahmp%water%state%SNEQV,          start=(/itime+1/))
     iret = nf90_put_var(ncid, SNICE_id,    noahmp%water%state%SNICE,          start=(/itime+1,1/), count=(/1,nsnow/))
     iret = nf90_put_var(ncid, SNLIQ_id,    noahmp%water%state%SNLIQ,          start=(/itime+1,1/), count=(/1,nsnow/))
     iret = nf90_put_var(ncid, ZWT_id,      noahmp%water%state%ZWT,            start=(/itime+1/))
     iret = nf90_put_var(ncid, WA_id,       noahmp%water%state%WA,             start=(/itime+1/))
     iret = nf90_put_var(ncid, WT_id,       noahmp%water%state%WT,             start=(/itime+1/))
     iret = nf90_put_var(ncid, WSLAKE_id,   noahmp%water%state%WSLAKE,         start=(/itime+1/))
     iret = nf90_put_var(ncid, LFMASS_id,   noahmp%biochem%state%LFMASS,       start=(/itime+1/))
     iret = nf90_put_var(ncid, RTMASS_id,   noahmp%biochem%state%RTMASS,       start=(/itime+1/))
     iret = nf90_put_var(ncid, STMASS_id,   noahmp%biochem%state%STMASS,       start=(/itime+1/))
     iret = nf90_put_var(ncid, WOOD_id,     noahmp%biochem%state%WOOD,         start=(/itime+1/))
     iret = nf90_put_var(ncid, STBLCP_id,   noahmp%biochem%state%STBLCP,       start=(/itime+1/))
     iret = nf90_put_var(ncid, FASTCP_id,   noahmp%biochem%state%FASTCP,       start=(/itime+1/))
     iret = nf90_put_var(ncid, LAI_id,      noahmp%energy%state%LAI,           start=(/itime+1/))
     iret = nf90_put_var(ncid, SAI_id,      noahmp%energy%state%SAI,           start=(/itime+1/))
     iret = nf90_put_var(ncid, CM_id,       noahmp%energy%state%CM,            start=(/itime+1/))
     iret = nf90_put_var(ncid, CH_id,       noahmp%energy%state%CH,            start=(/itime+1/))
     iret = nf90_put_var(ncid, TAUSS_id,    noahmp%energy%state%TAUSS,         start=(/itime+1/))
     iret = nf90_put_var(ncid, GRAIN_id,    noahmp%biochem%state%GRAIN,        start=(/itime+1/))
     iret = nf90_put_var(ncid, GDD_id,      noahmp%biochem%state%GDD,          start=(/itime+1/))
     iret = nf90_put_var(ncid, PGS_id,      noahmp%biochem%state%PGS,          start=(/itime+1/))
     iret = nf90_put_var(ncid, SMCWTD_id,   noahmp%water%state%SMCWTD,         start=(/itime+1/))
     iret = nf90_put_var(ncid, DEEPRECH_id, noahmp%water%state%DEEPRECH,       start=(/itime+1/))
     iret = nf90_put_var(ncid, RECH_id,     noahmp%water%state%RECH,           start=(/itime+1/))
     iret = nf90_put_var(ncid, QTLDRN_id,   noahmp%water%flux%QTLDRN,          start=(/itime+1/))
     iret = nf90_put_var(ncid, Z0WRF_id,    noahmp%energy%state%Z0WRF,         start=(/itime+1/))
     iret = nf90_put_var(ncid, IRAMTFI_id,  noahmp%water%state%IRAMTFI,        start=(/itime+1/))
     iret = nf90_put_var(ncid, IRAMTMI_id,  noahmp%water%state%IRAMTMI,        start=(/itime+1/))
     iret = nf90_put_var(ncid, IRFIRATE_id, noahmp%water%flux%IRFIRATE,        start=(/itime+1/))
     iret = nf90_put_var(ncid, IRMIRATE_id, noahmp%water%flux%IRMIRATE,        start=(/itime+1/))
     iret = nf90_put_var(ncid, IRAMTSI_id,  noahmp%water%state%IRAMTSI,        start=(/itime+1/))
     iret = nf90_put_var(ncid, IRSIRATE_id, noahmp%water%flux%IRSIRATE,        start=(/itime+1/))
     iret = nf90_put_var(ncid, IRCNTSI_id,  noahmp%water%state%IRCNTSI,        start=(/itime+1/))
     iret = nf90_put_var(ncid, IRCNTMI_id,  noahmp%water%state%IRCNTMI,        start=(/itime+1/))
     iret = nf90_put_var(ncid, IRCNTFI_id,  noahmp%water%state%IRCNTFI,        start=(/itime+1/))
     iret = nf90_put_var(ncid, FIRR_id,     noahmp%energy%flux%FIRR,           start=(/itime+1/))
     iret = nf90_put_var(ncid, EIRR_id,     noahmp%water%flux%EIRR,            start=(/itime+1/))
     iret = nf90_put_var(ncid, FSA_id,      noahmp%energy%flux%FSA,            start=(/itime+1/))
     iret = nf90_put_var(ncid, FSR_id,      noahmp%energy%flux%FSR,            start=(/itime+1/))
     iret = nf90_put_var(ncid, FIRA_id,     noahmp%energy%flux%FIRA,           start=(/itime+1/))
     iret = nf90_put_var(ncid, FSH_id,      noahmp%energy%flux%FSH,            start=(/itime+1/))
     iret = nf90_put_var(ncid, SSOIL_id,    noahmp%energy%flux%SSOIL,          start=(/itime+1/))
     iret = nf90_put_var(ncid, FCEV_id,     noahmp%energy%flux%FCEV,           start=(/itime+1/))
     iret = nf90_put_var(ncid, FGEV_id,     noahmp%energy%flux%FGEV,           start=(/itime+1/))
     iret = nf90_put_var(ncid, FCTR_id,     noahmp%energy%flux%FCTR,           start=(/itime+1/))
     iret = nf90_put_var(ncid, ECAN_id,     noahmp%water%flux%ECAN,            start=(/itime+1/))
     iret = nf90_put_var(ncid, ETRAN_id,    noahmp%water%flux%ETRAN,           start=(/itime+1/))
     iret = nf90_put_var(ncid, EDIR_id,     noahmp%water%flux%EDIR,            start=(/itime+1/))
     iret = nf90_put_var(ncid, TRAD_id,     noahmp%energy%state%TRAD,          start=(/itime+1/))
     iret = nf90_put_var(ncid, TGV_id,      noahmp%energy%state%TGV,           start=(/itime+1/))
     iret = nf90_put_var(ncid, TGB_id,      noahmp%energy%state%TGB,           start=(/itime+1/))
     iret = nf90_put_var(ncid, T2MV_id,     noahmp%energy%state%T2MV,          start=(/itime+1/))
     iret = nf90_put_var(ncid, T2MB_id,     noahmp%energy%state%T2MB,          start=(/itime+1/))
     iret = nf90_put_var(ncid, Q2V_id,      noahmp%energy%state%Q2V,           start=(/itime+1/))
     iret = nf90_put_var(ncid, Q2B_id,      noahmp%energy%state%Q2B,           start=(/itime+1/))
     iret = nf90_put_var(ncid, RUNSRF_id,   noahmp%water%flux%RUNSRF,          start=(/itime+1/))
     iret = nf90_put_var(ncid, RUNSUB_id,   noahmp%water%flux%RUNSUB,          start=(/itime+1/))
     iret = nf90_put_var(ncid, PSN_id,      noahmp%biochem%flux%PSN,           start=(/itime+1/))
     iret = nf90_put_var(ncid, APAR_id,     noahmp%energy%flux%APAR,           start=(/itime+1/))
     iret = nf90_put_var(ncid, SAV_id,      noahmp%energy%flux%SAV,            start=(/itime+1/))
     iret = nf90_put_var(ncid, SAG_id,      noahmp%energy%flux%SAG,            start=(/itime+1/))
     iret = nf90_put_var(ncid, FSNO_id,     noahmp%water%state%FSNO,           start=(/itime+1/))
     iret = nf90_put_var(ncid, NEE_id,      noahmp%biochem%flux%NEE,           start=(/itime+1/))
     iret = nf90_put_var(ncid, GPP_id,      noahmp%biochem%flux%GPP,           start=(/itime+1/))
     iret = nf90_put_var(ncid, NPP_id,      noahmp%biochem%flux%NPP,           start=(/itime+1/))
     iret = nf90_put_var(ncid, FVEG_id,     noahmp%energy%state%FVEG,          start=(/itime+1/))
     iret = nf90_put_var(ncid, ALBEDO_id,   noahmp%energy%state%ALBEDO,        start=(/itime+1/))
     iret = nf90_put_var(ncid, QSNBOT_id,   noahmp%water%flux%QSNBOT,          start=(/itime+1/))
     iret = nf90_put_var(ncid, PONDING1_id, noahmp%water%state%PONDING1,       start=(/itime+1/))
     iret = nf90_put_var(ncid, PONDING2_id, noahmp%water%state%PONDING2,       start=(/itime+1/))
     iret = nf90_put_var(ncid, PONDING_id,  noahmp%water%state%PONDING,        start=(/itime+1/))
     iret = nf90_put_var(ncid, RSSUN_id,    noahmp%energy%state%RSSUN,         start=(/itime+1/))
     iret = nf90_put_var(ncid, RSSHA_id,    noahmp%energy%state%RSSHA,         start=(/itime+1/))
     iret = nf90_put_var(ncid, ALBSND_id,   noahmp%energy%state%ALBSND,        start=(/itime+1,1/), count=(/1,2/))
     iret = nf90_put_var(ncid, ALBSNI_id,   noahmp%energy%state%ALBSNI,        start=(/itime+1,1/), count=(/1,2/))
     iret = nf90_put_var(ncid, BGAP_id,     noahmp%energy%state%BGAP,          start=(/itime+1/))
     iret = nf90_put_var(ncid, WGAP_id,     noahmp%energy%state%WGAP,          start=(/itime+1/))
     iret = nf90_put_var(ncid, CHV_id,      noahmp%energy%state%CHV,           start=(/itime+1/))
     iret = nf90_put_var(ncid, CHB_id,      noahmp%energy%state%CHB,           start=(/itime+1/))
     iret = nf90_put_var(ncid, EMISSI_id,   noahmp%energy%state%EMISSI,        start=(/itime+1/))
     iret = nf90_put_var(ncid, SHG_id,      noahmp%energy%flux%SHG,            start=(/itime+1/))
     iret = nf90_put_var(ncid, SHC_id,      noahmp%energy%flux%SHC,            start=(/itime+1/))
     iret = nf90_put_var(ncid, SHB_id,      noahmp%energy%flux%SHB,            start=(/itime+1/))
     iret = nf90_put_var(ncid, EVG_id,      noahmp%energy%flux%EVG,            start=(/itime+1/))
     iret = nf90_put_var(ncid, EVC_id,      noahmp%energy%flux%EVC,            start=(/itime+1/))
     iret = nf90_put_var(ncid, EVB_id,      noahmp%energy%flux%EVB,            start=(/itime+1/))
     iret = nf90_put_var(ncid, GHV_id,      noahmp%energy%flux%GHV,            start=(/itime+1/))
     iret = nf90_put_var(ncid, GHB_id,      noahmp%energy%flux%GHB,            start=(/itime+1/))
     iret = nf90_put_var(ncid, IRG_id,      noahmp%energy%flux%IRG,            start=(/itime+1/))
     iret = nf90_put_var(ncid, IRC_id,      noahmp%energy%flux%IRC,            start=(/itime+1/))
     iret = nf90_put_var(ncid, IRB_id,      noahmp%energy%flux%IRB,            start=(/itime+1/))
     iret = nf90_put_var(ncid, TR_id,       noahmp%energy%flux%TR,             start=(/itime+1/))
     iret = nf90_put_var(ncid, CHV2_id,     noahmp%energy%state%CHV2,          start=(/itime+1/))
     iret = nf90_put_var(ncid, CHLEAF_id,   noahmp%energy%state%CHLEAF,        start=(/itime+1/))
     iret = nf90_put_var(ncid, CHUC_id,     noahmp%energy%state%CHUC,          start=(/itime+1/))
     iret = nf90_put_var(ncid, FPICE_id,    noahmp%water%state%FPICE,          start=(/itime+1/))
     iret = nf90_put_var(ncid, PAH_id,      noahmp%energy%flux%PAH,            start=(/itime+1/))
     iret = nf90_put_var(ncid, PAHV_id,     noahmp%energy%flux%PAHV,           start=(/itime+1/))
     iret = nf90_put_var(ncid, PAHG_id,     noahmp%energy%flux%PAHG,           start=(/itime+1/))
     iret = nf90_put_var(ncid, PAHB_id,     noahmp%energy%flux%PAHB,           start=(/itime+1/))
     iret = nf90_put_var(ncid, LAISUN_id,   noahmp%energy%state%LAISUN,        start=(/itime+1/))
     iret = nf90_put_var(ncid, LAISHA_id,   noahmp%energy%state%LAISHA,        start=(/itime+1/))
     iret = nf90_put_var(ncid, FICEOLD_id,  noahmp%water%state%FICEOLD_SNOW,   start=(/itime+1,1/), count=(/1,nsnow/))
     iret = nf90_put_var(ncid, CHB2_id,     CHB2,                              start=(/itime+1/))

    end associate

  end subroutine add_to_output

  subroutine finalize_output()

     iret = nf90_close(ncid)

  end subroutine finalize_output
   
end module NoahmpOutputMod

