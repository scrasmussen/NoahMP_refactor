module noahmp_output

  use netcdf

  implicit none

  integer           :: ncid
  integer           :: iret
  integer           :: time_dim
  integer           :: soil_dim
  integer           :: snow_dim
  integer           :: snso_dim
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


    iret = nf90_enddef(ncid)
  
   end subroutine initialize_output

  subroutine add_to_output(itime,NSOIL,NSNOW,ISNOW,CANLIQ,CANICE,TV,SNOWH,SNEQV,&
                     SNICE,SNLIQ,STC,ZSNSO,SH2O,SMC,SICE,ZWT,WA,WT,DZSNSO,&
                     WSLAKE,SMCWTD,DEEPRECH,RECH,IRAMTFI,IRAMTMI,IRFIRATE,IRMIRATE,&
                     CMC,ECAN,ETRAN,FWET,RUNSRF,RUNSUB,QIN,QDIS,PONDING1,PONDING2,&
                     QSNBOT,QTLDRN,QINSUR,QSEVA,QSDEW,QSNFRO,QSNSUB,ETRANI,&
                     WCND,QDRAIN,SNOFLOW,FCRMAX,FICEOLD,errwat,QRAIN,QSNOW,QVAP,&
                     IRAMTSI,IRSIRATE,IRCNTSI,IRCNTMI,IRCNTFI,RAIN,SNOW,IREVPLOS,FIRR,EIRR,&
                     SNOWHIN,TG,QINTR,QDRIPR,QTHROR,QINTS,QDRIPS,QTHROS,PAHV,PAHG,PAHB,EDIR)

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


   end subroutine add_to_output

   subroutine finalize_output()

     iret = nf90_close(ncid)

   end subroutine finalize_output
   
end module noahmp_output

