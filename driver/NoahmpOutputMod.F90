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
  integer           :: IRAMTFI_id
  integer           :: IRAMTMI_id
  integer           :: IRFIRATE_id
  integer           :: IRMIRATE_id
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

  subroutine initialize_output(noahmp, input, ntime)

    implicit none

    integer          , intent(in)    :: ntime
    type(input_type) , intent(inout) :: input
    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
 
    iret = nf90_create(trim(input%output_filename), NF90_CLOBBER, ncid)

    iret = nf90_def_dim(ncid, "time", ntime, time_dim)
    iret = nf90_def_dim(ncid, "soil", noahmp%config%domain%NSOIL, soil_dim)
    iret = nf90_def_dim(ncid, "snow", noahmp%config%domain%NSNOW, snow_dim)
    iret = nf90_def_dim(ncid, "snso", noahmp%config%domain%NSOIL + &
                                      noahmp%config%domain%NSNOW, snso_dim)
    iret = nf90_def_dim(ncid, "band", 2, band_dim)

    iret = nf90_def_var(ncid, "ISNOW",       NF90_INT  , (/time_dim/)         , ISNOW_id    )
    iret = nf90_def_var(ncid, "CANLIQ",      NF90_FLOAT, (/time_dim/)         , CANLIQ_id   )
    iret = nf90_def_var(ncid, "CANICE",      NF90_FLOAT, (/time_dim/)         , CANICE_id   )
    iret = nf90_def_var(ncid, "TV",          NF90_FLOAT, (/time_dim/)         , TV_id       )
    iret = nf90_def_var(ncid, "SNOWH",       NF90_FLOAT, (/time_dim/)         , SNOWH_id    )
    iret = nf90_def_var(ncid, "SNEQV",       NF90_FLOAT, (/time_dim/)         , SNEQV_id    )
    iret = nf90_def_var(ncid, "SNICE",       NF90_FLOAT, (/time_dim,snow_dim/), SNICE_id    )
    iret = nf90_def_var(ncid, "SNLIQ",       NF90_FLOAT, (/time_dim,snow_dim/), SNLIQ_id    )
    iret = nf90_def_var(ncid, "STC",         NF90_FLOAT, (/time_dim,snso_dim/), STC_id      )
    iret = nf90_def_var(ncid, "ZSNSO",       NF90_FLOAT, (/time_dim,snso_dim/), ZSNSO_id    )
    iret = nf90_def_var(ncid, "SH2O",        NF90_FLOAT, (/time_dim,soil_dim/), SH2O_id     )
    iret = nf90_def_var(ncid, "SMC",         NF90_FLOAT, (/time_dim,soil_dim/), SMC_id      )
    iret = nf90_def_var(ncid, "SICE",        NF90_FLOAT, (/time_dim,soil_dim/), SICE_id     )
    iret = nf90_def_var(ncid, "ZWT",         NF90_FLOAT, (/time_dim/)         , ZWT_id      )
    iret = nf90_def_var(ncid, "WA",          NF90_FLOAT, (/time_dim/)         , WA_id       )
    iret = nf90_def_var(ncid, "WT",          NF90_FLOAT, (/time_dim/)         , WT_id       )
    iret = nf90_def_var(ncid, "DZSNSO",      NF90_FLOAT, (/time_dim,snso_dim/), DZSNSO_id   )
    iret = nf90_def_var(ncid, "WSLAKE",      NF90_FLOAT, (/time_dim/)         , WSLAKE_id   )
    iret = nf90_def_var(ncid, "SMCWTD",      NF90_FLOAT, (/time_dim/)         , SMCWTD_id   )
    iret = nf90_def_var(ncid, "DEEPRECH",    NF90_FLOAT, (/time_dim/)         , DEEPRECH_id )
    iret = nf90_def_var(ncid, "RECH",        NF90_FLOAT, (/time_dim/)         , RECH_id     )
    iret = nf90_def_var(ncid, "IRAMTFI",     NF90_FLOAT, (/time_dim/)         , IRAMTFI_id  )
    iret = nf90_def_var(ncid, "IRAMTMI",     NF90_FLOAT, (/time_dim/)         , IRAMTMI_id  )
    iret = nf90_def_var(ncid, "IRFIRATE",    NF90_FLOAT, (/time_dim/)         , IRFIRATE_id )
    iret = nf90_def_var(ncid, "IRMIRATE",    NF90_FLOAT, (/time_dim/)         , IRMIRATE_id ) 
    iret = nf90_def_var(ncid, "CMC",         NF90_FLOAT, (/time_dim/)         , CMC_id      )
    iret = nf90_def_var(ncid, "ECAN",        NF90_FLOAT, (/time_dim/)         , ECAN_id     )
    iret = nf90_def_var(ncid, "ETRAN",       NF90_FLOAT, (/time_dim/)         , ETRAN_id    )
    iret = nf90_def_var(ncid, "FWET",        NF90_FLOAT, (/time_dim/)         , FWET_id     )
    iret = nf90_def_var(ncid, "RUNSRF",      NF90_FLOAT, (/time_dim/)         , RUNSRF_id   )
    iret = nf90_def_var(ncid, "RUNSUB",      NF90_FLOAT, (/time_dim/)         , RUNSUB_id   )
    iret = nf90_def_var(ncid, "QIN",         NF90_FLOAT, (/time_dim/)         , QIN_id      )
    iret = nf90_def_var(ncid, "QDIS",        NF90_FLOAT, (/time_dim/)         , QDIS_id     )
    iret = nf90_def_var(ncid, "PONDING1",    NF90_FLOAT, (/time_dim/)         , PONDING1_id )
    iret = nf90_def_var(ncid, "PONDING2",    NF90_FLOAT, (/time_dim/)         , PONDING2_id )
    iret = nf90_def_var(ncid, "QSNBOT",      NF90_FLOAT, (/time_dim/)         , QSNBOT_id   )
    iret = nf90_def_var(ncid, "QTLDRN",      NF90_FLOAT, (/time_dim/)         , QTLDRN_id   )
    iret = nf90_def_var(ncid, "QINSUR",      NF90_FLOAT, (/time_dim/)         , QINSUR_id   )
    iret = nf90_def_var(ncid, "QSEVA",       NF90_FLOAT, (/time_dim/)         , QSEVA_id    )
    iret = nf90_def_var(ncid, "QSDEW",       NF90_FLOAT, (/time_dim/)         , QSDEW_id    )
    iret = nf90_def_var(ncid, "QSNFRO",      NF90_FLOAT, (/time_dim/)         , QSNFRO_id   )
    iret = nf90_def_var(ncid, "QSNSUB",      NF90_FLOAT, (/time_dim/)         , QSNSUB_id   )
    iret = nf90_def_var(ncid, "ETRANI",      NF90_FLOAT, (/time_dim,soil_dim/), ETRANI_id   )
    iret = nf90_def_var(ncid, "WCND",        NF90_FLOAT, (/time_dim,soil_dim/), WCND_id     )
    iret = nf90_def_var(ncid, "QDRAIN",      NF90_FLOAT, (/time_dim/)         , QDRAIN_id   )
    iret = nf90_def_var(ncid, "SNOFLOW",     NF90_FLOAT, (/time_dim/)         , SNOFLOW_id  )
    iret = nf90_def_var(ncid, "FCRMAX",      NF90_FLOAT, (/time_dim/)         , FCRMAX_id   )
    iret = nf90_def_var(ncid, "FICEOLD",     NF90_FLOAT, (/time_dim,snow_dim/), FICEOLD_id  )
    iret = nf90_def_var(ncid, "errwat",      NF90_FLOAT, (/time_dim/)         , errwat_id   )
    iret = nf90_def_var(ncid, "QRAIN",       NF90_FLOAT, (/time_dim/)         , QRAIN_id    )
    iret = nf90_def_var(ncid, "QSNOW",       NF90_FLOAT, (/time_dim/)         , QSNOW_id    )
    iret = nf90_def_var(ncid, "QVAP",        NF90_FLOAT, (/time_dim/)         , QVAP_id     )
    iret = nf90_def_var(ncid, "IRAMTSI",     NF90_FLOAT, (/time_dim/)         , IRAMTSI_id  )
    iret = nf90_def_var(ncid, "IRSIRATE",    NF90_FLOAT, (/time_dim/)         , IRSIRATE_id )
    iret = nf90_def_var(ncid, "IRCNTSI",     NF90_FLOAT, (/time_dim/)         , IRCNTSI_id  )
    iret = nf90_def_var(ncid, "IRCNTMI",     NF90_FLOAT, (/time_dim/)         , IRCNTMI_id  )
    iret = nf90_def_var(ncid, "IRCNTFI",     NF90_FLOAT, (/time_dim/)         , IRCNTFI_id  )
    iret = nf90_def_var(ncid, "RAIN",        NF90_FLOAT, (/time_dim/)         , RAIN_id     )
    iret = nf90_def_var(ncid, "SNOW",        NF90_FLOAT, (/time_dim/)         , SNOW_id     )
    iret = nf90_def_var(ncid, "IREVPLOS",    NF90_FLOAT, (/time_dim/)         , IREVPLOS_id )
    iret = nf90_def_var(ncid, "FIRR",        NF90_FLOAT, (/time_dim/)         , FIRR_id     )
    iret = nf90_def_var(ncid, "EIRR",        NF90_FLOAT, (/time_dim/)         , EIRR_id     )
    iret = nf90_def_var(ncid, "SNOWHIN",     NF90_FLOAT, (/time_dim/)         , SNOWHIN_id  )
    iret = nf90_def_var(ncid, "TG",          NF90_FLOAT, (/time_dim/)         , TG_id       )
    iret = nf90_def_var(ncid, "QINTR",       NF90_FLOAT, (/time_dim/)         , QINTR_id    )
    iret = nf90_def_var(ncid, "QDRIPR",      NF90_FLOAT, (/time_dim/)         , QDRIPR_id   )
    iret = nf90_def_var(ncid, "QTHROR",      NF90_FLOAT, (/time_dim/)         , QTHROR_id   )
    iret = nf90_def_var(ncid, "QINTS",       NF90_FLOAT, (/time_dim/)         , QINTS_id    )
    iret = nf90_def_var(ncid, "QDRIPS",      NF90_FLOAT, (/time_dim/)         , QDRIPS_id   )
    iret = nf90_def_var(ncid, "QTHROS",      NF90_FLOAT, (/time_dim/)         , QTHROS_id   )
    iret = nf90_def_var(ncid, "PAHV",        NF90_FLOAT, (/time_dim/)         , PAHV_id     )
    iret = nf90_def_var(ncid, "PAHG",        NF90_FLOAT, (/time_dim/)         , PAHG_id     )
    iret = nf90_def_var(ncid, "PAHB",        NF90_FLOAT, (/time_dim/)         , PAHB_id     )
    iret = nf90_def_var(ncid, "EDIR",        NF90_FLOAT, (/time_dim/)         , EDIR_id     )
! thermoprop new vars
    iret = nf90_def_var(ncid, "DF",          NF90_FLOAT, (/time_dim,snso_dim/), DF_id       )
    iret = nf90_def_var(ncid, "HCPCT",       NF90_FLOAT, (/time_dim,snso_dim/), HCPCT_id    )
    iret = nf90_def_var(ncid, "FACT",        NF90_FLOAT, (/time_dim,snso_dim/), FACT_id     )
    iret = nf90_def_var(ncid, "SNLIQV",      NF90_FLOAT, (/time_dim,snow_dim/), SNLIQV_id   )
    iret = nf90_def_var(ncid, "EPORE",       NF90_FLOAT, (/time_dim,snow_dim/), EPORE_id    )
    iret = nf90_def_var(ncid, "SNICEV",      NF90_FLOAT, (/time_dim,snow_dim/), SNICEV_id   )
! radiation new vars
    iret = nf90_def_var(ncid, "ALBOLD",      NF90_FLOAT, (/time_dim/)         , ALBOLD_id   )
    iret = nf90_def_var(ncid, "TAUSS",       NF90_FLOAT, (/time_dim/)         , TAUSS_id    )
    iret = nf90_def_var(ncid, "FSUN",        NF90_FLOAT, (/time_dim/)         , FSUN_id     )
    iret = nf90_def_var(ncid, "LAISUN",      NF90_FLOAT, (/time_dim/)         , LAISUN_id   )
    iret = nf90_def_var(ncid, "LAISHA",      NF90_FLOAT, (/time_dim/)         , LAISHA_id   )
    iret = nf90_def_var(ncid, "PARSUN",      NF90_FLOAT, (/time_dim/)         , PARSUN_id   )
    iret = nf90_def_var(ncid, "PARSHA",      NF90_FLOAT, (/time_dim/)         , PARSHA_id   )
    iret = nf90_def_var(ncid, "SAV",         NF90_FLOAT, (/time_dim/)         , SAV_id      )
    iret = nf90_def_var(ncid, "SAG",         NF90_FLOAT, (/time_dim/)         , SAG_id      )
    iret = nf90_def_var(ncid, "FSA",         NF90_FLOAT, (/time_dim/)         , FSA_id      )
    iret = nf90_def_var(ncid, "FSR",         NF90_FLOAT, (/time_dim/)         , FSR_id      )
    iret = nf90_def_var(ncid, "FSRV",        NF90_FLOAT, (/time_dim/)         , FSRV_id     )
    iret = nf90_def_var(ncid, "FSRG",        NF90_FLOAT, (/time_dim/)         , FSRG_id     )
    iret = nf90_def_var(ncid, "BGAP",        NF90_FLOAT, (/time_dim/)         , BGAP_id     )
    iret = nf90_def_var(ncid, "WGAP",        NF90_FLOAT, (/time_dim/)         , WGAP_id     )
    iret = nf90_def_var(ncid, "SNEQVO",      NF90_FLOAT, (/time_dim/)         , SNEQVO_id   )
    iret = nf90_def_var(ncid, "ALBSND",      NF90_FLOAT, (/time_dim,band_dim/), ALBSND_id   )
    iret = nf90_def_var(ncid, "ALBSNI",      NF90_FLOAT, (/time_dim,band_dim/), ALBSNI_id   )
! vege_flux new vars
    iret = nf90_def_var(ncid, "TAH",         NF90_FLOAT, (/time_dim/)         , TAH_id      )
    iret = nf90_def_var(ncid, "TGV",         NF90_FLOAT, (/time_dim/)         , TGV_id      )
    iret = nf90_def_var(ncid, "EAH",         NF90_FLOAT, (/time_dim/)         , EAH_id      )
    iret = nf90_def_var(ncid, "CMV",         NF90_FLOAT, (/time_dim/)         , CMV_id      )
    iret = nf90_def_var(ncid, "CM",          NF90_FLOAT, (/time_dim/)         , CM_id       )
    iret = nf90_def_var(ncid, "CHV",         NF90_FLOAT, (/time_dim/)         , CHV_id      )
    iret = nf90_def_var(ncid, "CH",          NF90_FLOAT, (/time_dim/)         , CH_id       )
    iret = nf90_def_var(ncid, "QSFC",        NF90_FLOAT, (/time_dim/)         , QSFC_id     )
    iret = nf90_def_var(ncid, "RSSUN",       NF90_FLOAT, (/time_dim/)         , RSSUN_id    )
    iret = nf90_def_var(ncid, "RSSHA",       NF90_FLOAT, (/time_dim/)         , RSSHA_id    )
    iret = nf90_def_var(ncid, "TAUXV",       NF90_FLOAT, (/time_dim/)         , TAUXV_id    )
    iret = nf90_def_var(ncid, "TAUYV",       NF90_FLOAT, (/time_dim/)         , TAUYV_id    )
    iret = nf90_def_var(ncid, "IRG",         NF90_FLOAT, (/time_dim/)         , IRG_id      )
    iret = nf90_def_var(ncid, "IRC",         NF90_FLOAT, (/time_dim/)         , IRC_id      )
    iret = nf90_def_var(ncid, "SHG",         NF90_FLOAT, (/time_dim/)         , SHG_id      )
    iret = nf90_def_var(ncid, "SHC",         NF90_FLOAT, (/time_dim/)         , SHC_id      )
    iret = nf90_def_var(ncid, "EVG",         NF90_FLOAT, (/time_dim/)         , EVG_id      )
    iret = nf90_def_var(ncid, "EVC",         NF90_FLOAT, (/time_dim/)         , EVC_id      )
    iret = nf90_def_var(ncid, "TR",          NF90_FLOAT, (/time_dim/)         , TR_id       )
    iret = nf90_def_var(ncid, "GHV",         NF90_FLOAT, (/time_dim/)         , GHV_id      )
    iret = nf90_def_var(ncid, "T2MV",        NF90_FLOAT, (/time_dim/)         , T2MV_id     )
    iret = nf90_def_var(ncid, "PSNSUN",      NF90_FLOAT, (/time_dim/)         , PSNSUN_id   )
    iret = nf90_def_var(ncid, "PSNSHA",      NF90_FLOAT, (/time_dim/)         , PSNSHA_id   )
    iret = nf90_def_var(ncid, "Q2V",         NF90_FLOAT, (/time_dim/)         , Q2V_id      )
    iret = nf90_def_var(ncid, "CHV2",        NF90_FLOAT, (/time_dim/)         , CHV2_id     )
    iret = nf90_def_var(ncid, "CHLEAF",      NF90_FLOAT, (/time_dim/)         , CHLEAF_id   )
    iret = nf90_def_var(ncid, "CHUC",        NF90_FLOAT, (/time_dim/)         , CHUC_id     )


    iret = nf90_enddef(ncid)
  
  end subroutine initialize_output

  subroutine add_to_output(itime, noahmp, errwat)

    implicit none

    integer               , intent(in)    :: itime
    type(noahmp_type)     , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in)    :: errwat

! --------------------------------------------------------------------
    associate(                                           &
              nsnow     =>  noahmp%config%domain%NSNOW  ,&
              nsoil     =>  noahmp%config%domain%NSOIL   &
             ) 
! --------------------------------------------------------------------

    iret = nf90_put_var(ncid, ISNOW_id,    noahmp%config%domain%ISNOW,       start=(/itime+1/))
    iret = nf90_put_var(ncid, CANLIQ_id,   noahmp%water%state%CANLIQ,        start=(/itime+1/))
    iret = nf90_put_var(ncid, CANICE_id,   noahmp%water%state%CANICE,        start=(/itime+1/))
    iret = nf90_put_var(ncid, TV_id,       noahmp%energy%state%TV,           start=(/itime+1/))
    iret = nf90_put_var(ncid, SNOWH_id,    noahmp%water%state%SNOWH,         start=(/itime+1/))
    iret = nf90_put_var(ncid, SNEQV_id,    noahmp%water%state%SNEQV,         start=(/itime+1/))
    iret = nf90_put_var(ncid, SNICE_id,    noahmp%water%state%SNICE,         start=(/itime+1,1/), count=(/1,nsnow/))
    iret = nf90_put_var(ncid, SNLIQ_id,    noahmp%water%state%SNLIQ,         start=(/itime+1,1/), count=(/1,nsnow/))
    iret = nf90_put_var(ncid, STC_id,      noahmp%energy%state%STC,          start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
    iret = nf90_put_var(ncid, ZSNSO_id,    noahmp%config%domain%ZSNSO,       start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
    iret = nf90_put_var(ncid, SH2O_id,     noahmp%water%state%SH2O,          start=(/itime+1,1/), count=(/1,nsoil/))
    iret = nf90_put_var(ncid, SMC_id,      noahmp%water%state%SMC,           start=(/itime+1,1/), count=(/1,nsoil/))
    iret = nf90_put_var(ncid, SICE_id,     noahmp%water%state%SICE,          start=(/itime+1,1/), count=(/1,nsoil/))
    iret = nf90_put_var(ncid, ZWT_id,      noahmp%water%state%ZWT,           start=(/itime+1/))
    iret = nf90_put_var(ncid, WA_id,       noahmp%water%state%WA,            start=(/itime+1/))
    iret = nf90_put_var(ncid, WT_id,       noahmp%water%state%WT,            start=(/itime+1/))
    iret = nf90_put_var(ncid, DZSNSO_id,   noahmp%config%domain%DZSNSO,      start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
    iret = nf90_put_var(ncid, WSLAKE_id,   noahmp%water%state%WSLAKE,        start=(/itime+1/))
    iret = nf90_put_var(ncid, SMCWTD_id,   noahmp%water%state%SMCWTD,        start=(/itime+1/))
    iret = nf90_put_var(ncid, DEEPRECH_id, noahmp%water%state%DEEPRECH,      start=(/itime+1/))
    iret = nf90_put_var(ncid, RECH_id,     noahmp%water%state%RECH,          start=(/itime+1/))
    iret = nf90_put_var(ncid, IRAMTFI_id,  noahmp%water%state%IRAMTFI,       start=(/itime+1/))
    iret = nf90_put_var(ncid, IRAMTMI_id,  noahmp%water%state%IRAMTMI,       start=(/itime+1/))
    iret = nf90_put_var(ncid, IRFIRATE_id, noahmp%water%flux%IRFIRATE,       start=(/itime+1/))
    iret = nf90_put_var(ncid, IRMIRATE_id, noahmp%water%flux%IRMIRATE,       start=(/itime+1/))
    iret = nf90_put_var(ncid, CMC_id,      noahmp%water%state%CMC,           start=(/itime+1/))
    iret = nf90_put_var(ncid, ECAN_id,     noahmp%water%flux%ECAN,           start=(/itime+1/))
    iret = nf90_put_var(ncid, ETRAN_id,    noahmp%water%flux%ETRAN,          start=(/itime+1/))
    iret = nf90_put_var(ncid, FWET_id,     noahmp%water%state%FWET,          start=(/itime+1/))
    iret = nf90_put_var(ncid, RUNSRF_id,   noahmp%water%flux%RUNSRF,         start=(/itime+1/))
    iret = nf90_put_var(ncid, RUNSUB_id,   noahmp%water%flux%RUNSUB,         start=(/itime+1/))
    iret = nf90_put_var(ncid, QIN_id,      noahmp%water%flux%QIN,            start=(/itime+1/))
    iret = nf90_put_var(ncid, QDIS_id,     noahmp%water%flux%QDIS,           start=(/itime+1/))
    iret = nf90_put_var(ncid, PONDING1_id, noahmp%water%state%PONDING1,      start=(/itime+1/))
    iret = nf90_put_var(ncid, PONDING2_id, noahmp%water%state%PONDING2,      start=(/itime+1/))
    iret = nf90_put_var(ncid, QSNBOT_id,   noahmp%water%flux%QSNBOT,         start=(/itime+1/))
    iret = nf90_put_var(ncid, QTLDRN_id,   noahmp%water%flux%QTLDRN,         start=(/itime+1/))
    iret = nf90_put_var(ncid, QINSUR_id,   noahmp%water%flux%QINSUR,         start=(/itime+1/))
    iret = nf90_put_var(ncid, QSEVA_id,    noahmp%water%flux%QSEVA,          start=(/itime+1/))
    iret = nf90_put_var(ncid, QSDEW_id,    noahmp%water%flux%QSDEW,          start=(/itime+1/))
    iret = nf90_put_var(ncid, QSNFRO_id,   noahmp%water%flux%QSNFRO,         start=(/itime+1/))
    iret = nf90_put_var(ncid, QSNSUB_id,   noahmp%water%flux%QSNSUB,         start=(/itime+1/))
    iret = nf90_put_var(ncid, ETRANI_id,   noahmp%water%flux%ETRANI,         start=(/itime+1,1/), count=(/1,nsoil/))
    iret = nf90_put_var(ncid, WCND_id,     noahmp%water%state%WCND,          start=(/itime+1,1/), count=(/1,nsoil/))
    iret = nf90_put_var(ncid, QDRAIN_id,   noahmp%water%flux%QDRAIN,         start=(/itime+1/))
    iret = nf90_put_var(ncid, SNOFLOW_id,  noahmp%water%flux%SNOFLOW,        start=(/itime+1/))
    iret = nf90_put_var(ncid, FCRMAX_id,   noahmp%water%state%FCRMAX,        start=(/itime+1/))
    iret = nf90_put_var(ncid, FICEOLD_id,  noahmp%water%state%FICEOLD_SNOW,  start=(/itime+1,1/), count=(/1,nsnow/))
    iret = nf90_put_var(ncid, errwat_id,   errwat,                           start=(/itime+1/))
    iret = nf90_put_var(ncid, QRAIN_id,    noahmp%water%flux%QRAIN,          start=(/itime+1/))
    iret = nf90_put_var(ncid, QSNOW_id,    noahmp%water%flux%QSNOW,          start=(/itime+1/))
    iret = nf90_put_var(ncid, QVAP_id,     noahmp%water%flux%QVAP,           start=(/itime+1/))
    iret = nf90_put_var(ncid, IRAMTSI_id,  noahmp%water%state%IRAMTSI,       start=(/itime+1/))
    iret = nf90_put_var(ncid, IRSIRATE_id, noahmp%water%flux%IRSIRATE,       start=(/itime+1/))
    iret = nf90_put_var(ncid, IRCNTSI_id,  noahmp%water%state%IRCNTSI,       start=(/itime+1/))
    iret = nf90_put_var(ncid, IRCNTMI_id,  noahmp%water%state%IRCNTMI,       start=(/itime+1/))
    iret = nf90_put_var(ncid, IRCNTFI_id,  noahmp%water%state%IRCNTFI,       start=(/itime+1/))
    iret = nf90_put_var(ncid, RAIN_id,     noahmp%water%flux%RAIN,           start=(/itime+1/))
    iret = nf90_put_var(ncid, SNOW_id,     noahmp%water%flux%SNOW,           start=(/itime+1/))
    iret = nf90_put_var(ncid, IREVPLOS_id, noahmp%water%flux%IREVPLOS,       start=(/itime+1/))
    iret = nf90_put_var(ncid, FIRR_id,     noahmp%energy%flux%FIRR,          start=(/itime+1/))
    iret = nf90_put_var(ncid, EIRR_id,     noahmp%water%flux%EIRR,           start=(/itime+1/))
    iret = nf90_put_var(ncid, SNOWHIN_id,  noahmp%water%flux%SNOWHIN,        start=(/itime+1/))
    iret = nf90_put_var(ncid, TG_id,       noahmp%energy%state%TG,           start=(/itime+1/))
    iret = nf90_put_var(ncid, QINTR_id,    noahmp%water%flux%QINTR,          start=(/itime+1/))
    iret = nf90_put_var(ncid, QDRIPR_id,   noahmp%water%flux%QDRIPR,         start=(/itime+1/))
    iret = nf90_put_var(ncid, QTHROR_id,   noahmp%water%flux%QTHROR,         start=(/itime+1/))
    iret = nf90_put_var(ncid, QINTS_id,    noahmp%water%flux%QINTS,          start=(/itime+1/))
    iret = nf90_put_var(ncid, QDRIPS_id,   noahmp%water%flux%QDRIPS,         start=(/itime+1/))
    iret = nf90_put_var(ncid, QTHROS_id,   noahmp%water%flux%QTHROS,         start=(/itime+1/))
    iret = nf90_put_var(ncid, PAHV_id,     noahmp%energy%flux%PAHV,          start=(/itime+1/))
    iret = nf90_put_var(ncid, PAHG_id,     noahmp%energy%flux%PAHG,          start=(/itime+1/))
    iret = nf90_put_var(ncid, PAHB_id,     noahmp%energy%flux%PAHB,          start=(/itime+1/))
    iret = nf90_put_var(ncid, EDIR_id,     noahmp%water%flux%EDIR,           start=(/itime+1/))
! thermoprop new vars
     iret = nf90_put_var(ncid, DF_id,      noahmp%energy%state%DF,           start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
     iret = nf90_put_var(ncid, HCPCT_id,   noahmp%energy%state%HCPCT,        start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
     iret = nf90_put_var(ncid, FACT_id,    noahmp%energy%state%FACT,         start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
     iret = nf90_put_var(ncid, SNICEV_id,  noahmp%water%state%SNICEV,        start=(/itime+1,1/), count=(/1,nsnow/))
     iret = nf90_put_var(ncid, SNLIQV_id,  noahmp%water%state%SNLIQV,        start=(/itime+1,1/), count=(/1,nsnow/))
     iret = nf90_put_var(ncid, EPORE_id,   noahmp%water%state%EPORE_SNOW2,   start=(/itime+1,1/), count=(/1,nsnow/))
! radiation new vars
     iret = nf90_put_var(ncid, ALBOLD_id,  noahmp%energy%state%ALBOLD,       start=(/itime+1/))
     iret = nf90_put_var(ncid, TAUSS_id,   noahmp%energy%state%TAUSS,        start=(/itime+1/))
     iret = nf90_put_var(ncid, FSUN_id,    noahmp%energy%state%FSUN,         start=(/itime+1/))
     iret = nf90_put_var(ncid, LAISUN_id,  noahmp%energy%state%LAISUN,       start=(/itime+1/))
     iret = nf90_put_var(ncid, LAISHA_id,  noahmp%energy%state%LAISHA,       start=(/itime+1/))
     iret = nf90_put_var(ncid, PARSUN_id,  noahmp%energy%flux%PARSUN,        start=(/itime+1/))
     iret = nf90_put_var(ncid, PARSHA_id,  noahmp%energy%flux%PARSHA,        start=(/itime+1/))
     iret = nf90_put_var(ncid, SAV_id,     noahmp%energy%flux%SAV,           start=(/itime+1/))
     iret = nf90_put_var(ncid, SAG_id,     noahmp%energy%flux%SAG,           start=(/itime+1/))
     iret = nf90_put_var(ncid, FSA_id,     noahmp%energy%flux%FSA,           start=(/itime+1/))
     iret = nf90_put_var(ncid, FSR_id,     noahmp%energy%flux%FSR,           start=(/itime+1/))
     iret = nf90_put_var(ncid, FSRV_id,    noahmp%energy%flux%FSRV,          start=(/itime+1/))
     iret = nf90_put_var(ncid, FSRG_id,    noahmp%energy%flux%FSRG,          start=(/itime+1/))
     iret = nf90_put_var(ncid, BGAP_id,    noahmp%energy%state%BGAP,         start=(/itime+1/))
     iret = nf90_put_var(ncid, WGAP_id,    noahmp%energy%state%WGAP,         start=(/itime+1/))
     iret = nf90_put_var(ncid, SNEQVO_id,  noahmp%water%state%SNEQVO,        start=(/itime+1/))
     iret = nf90_put_var(ncid, ALBSND_id,  noahmp%energy%state%ALBSND,       start=(/itime+1,1/), count=(/1,2/))
     iret = nf90_put_var(ncid, ALBSNI_id,  noahmp%energy%state%ALBSNI,       start=(/itime+1,1/), count=(/1,2/))
! vege_flux new vars
     iret = nf90_put_var(ncid, TAH_id,     noahmp%energy%state%TAH,          start=(/itime+1/))
     iret = nf90_put_var(ncid, TGV_id,     noahmp%energy%state%TGV,          start=(/itime+1/))
     iret = nf90_put_var(ncid, EAH_id,     noahmp%energy%state%EAH,          start=(/itime+1/))
     iret = nf90_put_var(ncid, CMV_id,     noahmp%energy%state%CMV,          start=(/itime+1/))
     iret = nf90_put_var(ncid, CM_id,      noahmp%energy%state%CM,           start=(/itime+1/))
     iret = nf90_put_var(ncid, CHV_id,     noahmp%energy%state%CHV,          start=(/itime+1/))
     iret = nf90_put_var(ncid, CH_id,      noahmp%energy%state%CH,           start=(/itime+1/))
     iret = nf90_put_var(ncid, QSFC_id,    noahmp%energy%state%QSFC,         start=(/itime+1/))
     iret = nf90_put_var(ncid, RSSUN_id,   noahmp%energy%state%RSSUN,        start=(/itime+1/))
     iret = nf90_put_var(ncid, RSSHA_id,   noahmp%energy%state%RSSHA,        start=(/itime+1/))
     iret = nf90_put_var(ncid, TAUXV_id,   noahmp%energy%state%TAUXV,        start=(/itime+1/))
     iret = nf90_put_var(ncid, TAUYV_id,   noahmp%energy%state%TAUYV,        start=(/itime+1/))
     iret = nf90_put_var(ncid, IRG_id,     noahmp%energy%flux%IRG,           start=(/itime+1/))
     iret = nf90_put_var(ncid, IRC_id,     noahmp%energy%flux%IRC,           start=(/itime+1/))
     iret = nf90_put_var(ncid, SHG_id,     noahmp%energy%flux%SHG,           start=(/itime+1/))
     iret = nf90_put_var(ncid, SHC_id,     noahmp%energy%flux%SHC,           start=(/itime+1/))
     iret = nf90_put_var(ncid, EVG_id,     noahmp%energy%flux%EVG,           start=(/itime+1/))
     iret = nf90_put_var(ncid, EVC_id,     noahmp%energy%flux%EVC,           start=(/itime+1/))
     iret = nf90_put_var(ncid, TR_id,      noahmp%energy%flux%TR,            start=(/itime+1/))
     iret = nf90_put_var(ncid, GHV_id,     noahmp%energy%flux%GHV,           start=(/itime+1/))
     iret = nf90_put_var(ncid, T2MV_id,    noahmp%energy%state%T2MV,         start=(/itime+1/))
     iret = nf90_put_var(ncid, PSNSUN_id,  noahmp%biochem%flux%PSNSUN,       start=(/itime+1/))
     iret = nf90_put_var(ncid, PSNSHA_id,  noahmp%biochem%flux%PSNSHA,       start=(/itime+1/))
     iret = nf90_put_var(ncid, Q2V_id,     noahmp%energy%state%Q2V,          start=(/itime+1/))
     iret = nf90_put_var(ncid, CHV2_id,    noahmp%energy%state%CHV2,         start=(/itime+1/))
     iret = nf90_put_var(ncid, CHLEAF_id,  noahmp%energy%state%CHLEAF,       start=(/itime+1/))
     iret = nf90_put_var(ncid, CHUC_id,    noahmp%energy%state%CHUC,         start=(/itime+1/))


    end associate

  end subroutine add_to_output

  subroutine finalize_output()

     iret = nf90_close(ncid)

  end subroutine finalize_output
   
end module NoahmpOutputMod

