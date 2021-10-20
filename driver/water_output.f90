module output

  use netcdf

  integer           :: ncid
  integer           :: iret
  integer           :: time_dim
  integer           :: soil_dim
  integer           :: snow_dim
  integer           :: snso_dim
  integer           :: time_id
  integer           :: evap_id
  integer           :: tran_id
  integer           :: smc_id
  integer           :: smcm_id
  integer           :: prcp_id
  integer           :: sfrn_id
  integer           :: ugrn_id
  integer           :: qinsur_id

  integer           :: qintr_id
  integer           :: qints_id
  integer           :: qdripr_id
  integer           :: qdrips_id
  integer           :: qthror_id
  integer           :: qthros_id
  integer           :: qrain_id
  integer           :: qsnow_id
  integer           :: snowhin_id
  integer           :: fwet_id
  integer           :: cmc_id
  integer           :: canliq_id
  integer           :: canice_id
  integer           :: ecan_id
  integer           :: etran_id
 
  integer           :: snowh_id
  integer           :: sneqv_id
  integer           :: ponding_id
  integer           :: ponding1_id
  integer           :: ponding2_id
  integer           :: qsnbot_id
  integer           :: qsnfro_id
  integer           :: qsnsub_id
  integer           :: snice_id
  integer           :: snliq_id
  integer           :: stc_id
  integer           :: zsnso_id

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

! for soil water
    iret = nf90_def_var(ncid, "timestep",             NF90_INT  , (/time_dim/), time_id)
    iret = nf90_def_var(ncid, "precipitation",        NF90_FLOAT, (/time_dim/), prcp_id)
    iret = nf90_def_var(ncid, "waterin_sfc",        NF90_FLOAT, (/time_dim/), qinsur_id)
    iret = nf90_def_var(ncid, "surface_runoff",       NF90_FLOAT, (/time_dim/), sfrn_id)
    iret = nf90_def_var(ncid, "subsurf_runoff",       NF90_FLOAT, (/time_dim/), ugrn_id)
    iret = nf90_def_var(ncid, "evaporation",          NF90_FLOAT, (/time_dim/), evap_id)
    iret = nf90_def_var(ncid, "transpiration",        NF90_FLOAT, (/time_dim/), tran_id)
    iret = nf90_def_var(ncid, "soil_moisture_mm",     NF90_FLOAT, (/time_dim,soil_dim/), smcm_id)
    iret = nf90_def_var(ncid, "soil_moisture",        NF90_FLOAT, (/time_dim,soil_dim/), smc_id)
! for canopy water
    iret = nf90_def_var(ncid, "rain_intercept",        NF90_FLOAT, (/time_dim/), qintr_id)
    iret = nf90_def_var(ncid, "snow_intercept",        NF90_FLOAT, (/time_dim/), qints_id)
    iret = nf90_def_var(ncid, "rain_drip",        NF90_FLOAT, (/time_dim/), qdripr_id)
    iret = nf90_def_var(ncid, "snow_drip",        NF90_FLOAT, (/time_dim/), qdrips_id)
    iret = nf90_def_var(ncid, "rain_through",        NF90_FLOAT, (/time_dim/), qthror_id)
    iret = nf90_def_var(ncid, "snow_through",        NF90_FLOAT, (/time_dim/), qthros_id)
    iret = nf90_def_var(ncid, "rain_surface",        NF90_FLOAT, (/time_dim/), qrain_id)
    iret = nf90_def_var(ncid, "snow_surface",        NF90_FLOAT, (/time_dim/), qsnow_id)
    iret = nf90_def_var(ncid, "snowhin",        NF90_FLOAT, (/time_dim/), snowhin_id)
    iret = nf90_def_var(ncid, "FWET",        NF90_FLOAT, (/time_dim/), fwet_id)
    iret = nf90_def_var(ncid, "CMC",        NF90_FLOAT, (/time_dim/), cmc_id)
    iret = nf90_def_var(ncid, "CANLIQ",        NF90_FLOAT, (/time_dim/), canliq_id)
    iret = nf90_def_var(ncid, "CANICE",        NF90_FLOAT, (/time_dim/), canice_id)
    iret = nf90_def_var(ncid, "ECAN",        NF90_FLOAT, (/time_dim/), ecan_id)
    iret = nf90_def_var(ncid, "ETRAN",        NF90_FLOAT, (/time_dim/), etran_id)
! for snow water
    iret = nf90_def_var(ncid, "SNOWH",        NF90_FLOAT, (/time_dim/), snowh_id) 
    iret = nf90_def_var(ncid, "SNEQV",        NF90_FLOAT, (/time_dim/), sneqv_id)
    iret = nf90_def_var(ncid, "PONDING",        NF90_FLOAT, (/time_dim/), ponding_id)
    iret = nf90_def_var(ncid, "PONDING1",        NF90_FLOAT, (/time_dim/), ponding1_id)
    iret = nf90_def_var(ncid, "PONDING2",        NF90_FLOAT, (/time_dim/), ponding2_id)
    iret = nf90_def_var(ncid, "QSNBOT",        NF90_FLOAT, (/time_dim/), qsnbot_id)
    iret = nf90_def_var(ncid, "QSNFRO",        NF90_FLOAT, (/time_dim/), qsnfro_id)
    iret = nf90_def_var(ncid, "QSNSUB",        NF90_FLOAT, (/time_dim/), qsnsub_id)
    iret = nf90_def_var(ncid, "SNICE",        NF90_FLOAT, (/time_dim,snow_dim/), snice_id)
    iret = nf90_def_var(ncid, "SNLIQ",        NF90_FLOAT, (/time_dim,snow_dim/), snliq_id)
    iret = nf90_def_var(ncid, "STC",        NF90_FLOAT, (/time_dim,snso_dim/), stc_id)
    iret = nf90_def_var(ncid, "ZSNSO",        NF90_FLOAT, (/time_dim,snso_dim/), zsnso_id)

    iret = nf90_enddef(ncid)
  
   end subroutine initialize_output

   subroutine add_to_output(itime,nsoil,dzsnso,dt,qinsur,runsrf,runsub,qseva,etrani,smc,rain,&
                            qintr,qints,qdripr,qdrips,qthror,qthros,qrain,qsnow,snowhin,fwet,&
                            cmc,canliq,canice,ecan,etran,nsnow,snowh,sneqv,ponding,ponding1,ponding2,&
                            QSNBOT,QSNFRO,QSNSUB,SNICE,SNLIQ,STC,zsnso )

     integer                :: itime
     integer                :: nsoil
     real                   :: dt 
     real                   :: qinsur      !water input on soil surface [m/s]
     real                   :: runsrf      !surface runoff [mm/s] 
     real                   :: runsub      !baseflow (sturation excess) [mm/s]
     real                   :: qseva       !soil surface evap rate [mm/s]
     real, dimension(nsoil) :: etrani      !transpiration rate (mm/s) [+]
     real, dimension(nsoil) :: smc         !total soil water content [m3/m3]
     real, dimension(nsoil) :: dzsnso      !soil level thickness [m]
     real, dimension(nsoil) :: smcmm       !total soil water content [mm]
     real                   :: rain
     real                   :: qintr
     real                   :: qints
     real                   :: qdripr
     real                   :: qdrips
     real                   :: qthror
     real                   :: qthros
     real                   :: qrain
     real                   :: qsnow
     real                   :: snowhin
     real                   :: fwet
     real                   :: cmc
     real                   :: canliq
     real                   :: canice
     real                   :: ecan
     real                   :: etran
     integer                :: nsnow
     real                   :: snowh
     real                   :: sneqv
     real                   :: ponding
     real                   :: ponding1
     real                   :: ponding2
     real                   :: qsnbot
     real                   :: qsnfro
     real                   :: qsnsub
     real, dimension(nsnow) :: snice
     real, dimension(nsnow) :: snliq
     real, dimension(nsoil+nsnow) :: stc
     real, dimension(nsoil+nsnow) :: zsnso

     smcmm = smc*dzsnso*1000.0
! for soilwater
     iret = nf90_put_var(ncid, time_id, itime,                 start=(/itime+1/))
     iret = nf90_put_var(ncid, prcp_id, rain*dt,               start=(/itime+1/))
     iret = nf90_put_var(ncid, qinsur_id,qinsur*1000*dt,        start=(/itime+1/))
     iret = nf90_put_var(ncid, sfrn_id, runsrf*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, ugrn_id, runsub*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, evap_id, qseva*1000.0*dt,       start=(/itime+1/))
     iret = nf90_put_var(ncid, tran_id, sum(etrani)*1000.0*dt, start=(/itime+1/))
     iret = nf90_put_var(ncid, smcm_id, smcmm,                 start=(/itime+1,1/), count=(/1,nsoil/))
     iret = nf90_put_var(ncid,  smc_id, smc,                   start=(/itime+1,1/), count=(/1,nsoil/))
! for canopy water  
     iret = nf90_put_var(ncid, qintr_id, qintr*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, qints_id, qints*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, qdripr_id, qdripr*dt,           start=(/itime+1/))
     iret = nf90_put_var(ncid, qdrips_id, qdrips*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, qthror_id, qthror*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, qthros_id, qthros*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, qrain_id, qrain*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, qsnow_id, qsnow*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, snowhin_id, snowhin*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, fwet_id, fwet,             start=(/itime+1/))
     iret = nf90_put_var(ncid, cmc_id, cmc,             start=(/itime+1/))
     iret = nf90_put_var(ncid, canliq_id, canliq,             start=(/itime+1/))
     iret = nf90_put_var(ncid, canice_id, canice,             start=(/itime+1/))
     iret = nf90_put_var(ncid, ecan_id, ecan*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, etran_id, etran*dt,             start=(/itime+1/))
! for snow water
     iret = nf90_put_var(ncid, snowh_id, snowh,             start=(/itime+1/))
     iret = nf90_put_var(ncid, sneqv_id, sneqv,             start=(/itime+1/))
     iret = nf90_put_var(ncid, ponding_id, ponding,             start=(/itime+1/))
     iret = nf90_put_var(ncid, ponding1_id, ponding1,             start=(/itime+1/))
     iret = nf90_put_var(ncid, ponding2_id, ponding2,             start=(/itime+1/))
     iret = nf90_put_var(ncid, qsnbot_id, qsnbot*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, qsnfro_id, qsnfro*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, qsnsub_id, qsnsub*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, snice_id, snice,       start=(/itime+1,1/), count=(/1,nsnow/))
     iret = nf90_put_var(ncid, snliq_id, snliq,       start=(/itime+1,1/), count=(/1,nsnow/))
     iret = nf90_put_var(ncid, stc_id, stc,       start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
     iret = nf90_put_var(ncid, zsnso_id, zsnso,   start=(/itime+1,1/), count=(/1,nsoil+nsnow/))

   end subroutine add_to_output

   subroutine finalize_output()

     iret = nf90_close(ncid)

   end subroutine finalize_output
   
end module output

