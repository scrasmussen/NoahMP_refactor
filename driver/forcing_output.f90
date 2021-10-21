module output

  use netcdf
  
  implicit none  

  integer           :: ncid
  integer           :: iret
  integer           :: time_dim
  integer           :: rad_dim = 2
  integer           :: time_id
  integer           :: sfcprs_id
  integer           :: sfctmp_id
  integer           :: q2_id
  integer           :: prcpconv_id
  integer           :: prcpnonc_id
  integer           :: prcpshcv_id
  integer           :: prcpsnow_id
  integer           :: prcpgrpl_id
  integer           :: prcphail_id
  integer           :: soldn_id
  integer           :: cosz_id        
  integer           :: thair_id
  integer           :: qair_id
  integer           :: eair_id
  integer           :: rhoair_id
  integer           :: qprecc_id
  integer           :: qprecl_id
  integer           :: solad_id       
  integer           :: solai_id
  integer           :: swdown_id
  integer           :: bdfall_id
  integer           :: rain_id
  integer           :: snow_id
  integer           :: fp_id
  integer           :: fpice_id
  integer           :: prcp_id

contains

  subroutine initialize_output(output_filename, ntime)

    implicit none

    integer       :: ntime
    character*256 :: output_filename

    iret = nf90_create(trim(output_filename), NF90_CLOBBER, ncid)
    iret = nf90_def_dim(ncid, "time",  ntime, time_dim)
    iret = nf90_def_dim(ncid, "level", 2,     rad_dim)
 
    iret = nf90_def_var(ncid, "timestep",NF90_INT  , (/time_dim/), time_id)
    iret = nf90_def_var(ncid, "SFCPRS",  NF90_FLOAT, (/time_dim/), sfcprs_id)
    iret = nf90_def_var(ncid, "SFCTMP",  NF90_FLOAT, (/time_dim/), sfctmp_id)
    iret = nf90_def_var(ncid, "Q2",      NF90_FLOAT, (/time_dim/), q2_id)
    iret = nf90_def_var(ncid, "PRCPCONV",NF90_FLOAT, (/time_dim/), prcpconv_id)
    iret = nf90_def_var(ncid, "PRCPNONC",NF90_FLOAT, (/time_dim/), prcpnonc_id)
    iret = nf90_def_var(ncid, "PRCPSHCV",NF90_FLOAT, (/time_dim/), prcpshcv_id)
    iret = nf90_def_var(ncid, "PRCPSNOW",NF90_FLOAT, (/time_dim/), prcpsnow_id)
    iret = nf90_def_var(ncid, "PRCPGRPL",NF90_FLOAT, (/time_dim/), prcpgrpl_id)
    iret = nf90_def_var(ncid, "PRCPHAIL",NF90_FLOAT, (/time_dim/), prcphail_id)
    iret = nf90_def_var(ncid, "SOLDN",   NF90_FLOAT, (/time_dim/), soldn_id)
    iret = nf90_def_var(ncid, "COSZ",    NF90_FLOAT, (/time_dim/), cosz_id)
    iret = nf90_def_var(ncid, "THAIR",   NF90_FLOAT, (/time_dim/), thair_id)
    iret = nf90_def_var(ncid, "QAIR",    NF90_FLOAT, (/time_dim/), qair_id)
    iret = nf90_def_var(ncid, "EAIR",    NF90_FLOAT, (/time_dim/), eair_id)
    iret = nf90_def_var(ncid, "RHOAIR",  NF90_FLOAT, (/time_dim/), rhoair_id)
    iret = nf90_def_var(ncid, "QPRECC",  NF90_FLOAT, (/time_dim/), qprecc_id)
    iret = nf90_def_var(ncid, "QPRECL",  NF90_FLOAT, (/time_dim/), qprecl_id)
    iret = nf90_def_var(ncid, "SOLAD",   NF90_FLOAT, (/time_dim,rad_dim/), solad_id)
    iret = nf90_def_var(ncid, "SOLAI",   NF90_FLOAT, (/time_dim,rad_dim/), solai_id)
    iret = nf90_def_var(ncid, "SWDOWN",  NF90_FLOAT, (/time_dim/), swdown_id)
    iret = nf90_def_var(ncid, "BDFALL",  NF90_FLOAT, (/time_dim/), bdfall_id)
    iret = nf90_def_var(ncid, "RAIN",    NF90_FLOAT, (/time_dim/), rain_id)
    iret = nf90_def_var(ncid, "SNOW",    NF90_FLOAT, (/time_dim/), snow_id)
    iret = nf90_def_var(ncid, "FP",      NF90_FLOAT, (/time_dim/), fp_id)
    iret = nf90_def_var(ncid, "FPICE",   NF90_FLOAT, (/time_dim/), fpice_id)
    iret = nf90_def_var(ncid, "PRCP",    NF90_FLOAT, (/time_dim/), prcp_id)      

    iret = nf90_enddef(ncid)

  end subroutine initialize_output

  subroutine add_to_output(itime,SFCPRS,SFCTMP,Q2,PRCPCONV,PRCPNONC,PRCPSHCV,& !in
                           PRCPSNOW,PRCPGRPL,PRCPHAIL,SOLDN,COSZ,            & !in
                           THAIR,QAIR,EAIR,RHOAIR,QPRECC,QPRECL,SOLAD,       & !out
                           SOLAI,SWDOWN,BDFALL,RAIN,SNOW,FP,FPICE,PRCP)        !out

    implicit none

    integer                                     :: itime
    real                          , intent(in)  :: SFCPRS   !pressure (pa)
    real                          , intent(in)  :: SFCTMP   !surface air temperature [k]
    real                          , intent(in)  :: Q2       !mixing ratio (kg/kg)
    real                          , intent(in)  :: PRCPCONV !convective precipitation entering  [mm/s]    ! MB/AN : v3.7
    real                          , intent(in)  :: PRCPNONC !non-convective precipitation entering [mm/s] ! MB/AN : v3.7
    real                          , intent(in)  :: PRCPSHCV !shallow convective precip entering  [mm/s]   ! MB/AN : v3.7
    real                          , intent(in)  :: PRCPSNOW !snow entering land model [mm/s]              ! MB/AN : v3.7
    real                          , intent(in)  :: PRCPGRPL !graupel entering land model [mm/s]           ! MB/AN : v3.7
    real                          , intent(in)  :: PRCPHAIL !hail entering land model [mm/s]              ! MB/AN : v3.7
    real                          , intent(in)  :: SOLDN    !downward shortwave radiation (w/m2)
    real                          , intent(in)  :: COSZ     !cosine solar zenith angle [0-1]

! outputs

    real                          , intent(in) :: THAIR     !potential temperature (k)
    real                          , intent(in) :: QAIR      !specific humidity (kg/kg) (q2/(1+q2))
    real                          , intent(in) :: EAIR      !vapor pressure air (pa)
    real                          , intent(in) :: RHOAIR    !density air (kg/m3)
    real                          , intent(in) :: QPRECC    !convective precipitation (mm/s)
    real                          , intent(in) :: QPRECL    !large-scale precipitation (mm/s)
    real, DIMENSION(       1:   2), intent(in) :: SOLAD     !incoming direct solar radiation (w/m2)
    real, DIMENSION(       1:   2), intent(in) :: SOLAI     !incoming diffuse solar radiation (w/m2)
    real                          , intent(in) :: SWDOWN    !downward solar filtered by sun angle [w/m2]
    real                          , intent(in) :: BDFALL    !!bulk density of snowfall (kg/m3) AJN
    real                          , intent(in) :: RAIN      !rainfall (mm/s) AJN
    real                          , intent(in) :: SNOW      !liquid equivalent snowfall (mm/s) AJN
    real                          , intent(in) :: FP        !fraction of area receiving precipitation  AJN
    real                          , intent(in) :: FPICE     !fraction of ice                AJN
    real                          , intent(in) :: PRCP      !total precipitation [mm/s]     ! MB/AN : v3.7



    iret = nf90_put_var(ncid, time_id,       itime,       start=(/itime+1/))
    iret = nf90_put_var(ncid, sfcprs_id,     SFCPRS,      start=(/itime+1/))
    iret = nf90_put_var(ncid, sfctmp_id,     SFCTMP,      start=(/itime+1/))
    iret = nf90_put_var(ncid, q2_id,         Q2,          start=(/itime+1/))
    iret = nf90_put_var(ncid, prcpconv_id,   PRCPCONV,    start=(/itime+1/))
    iret = nf90_put_var(ncid, prcpnonc_id,   PRCPNONC,    start=(/itime+1/))
    iret = nf90_put_var(ncid, prcpshcv_id,   PRCPSHCV,    start=(/itime+1/))
    iret = nf90_put_var(ncid, prcpsnow_id,   PRCPSNOW,    start=(/itime+1/))
    iret = nf90_put_var(ncid, prcpgrpl_id,   PRCPGRPL,    start=(/itime+1/))
    iret = nf90_put_var(ncid, prcphail_id,   PRCPHAIL,    start=(/itime+1/))
    iret = nf90_put_var(ncid, soldn_id,      SOLDN,       start=(/itime+1/))
    iret = nf90_put_var(ncid, cosz_id,       COSZ,        start=(/itime+1/))
    iret = nf90_put_var(ncid, thair_id,      THAIR,       start=(/itime+1/))
    iret = nf90_put_var(ncid, qair_id,       QAIR,        start=(/itime+1/))
    iret = nf90_put_var(ncid, eair_id,       EAIR,        start=(/itime+1/))
    iret = nf90_put_var(ncid, rhoair_id,     RHOAIR,      start=(/itime+1/))
    iret = nf90_put_var(ncid, qprecc_id,     QPRECC,      start=(/itime+1/))
    iret = nf90_put_var(ncid, qprecl_id,     QPRECL,      start=(/itime+1/))
    iret = nf90_put_var(ncid, solad_id,      SOLAD,       start=(/itime+1,1/),count=(/1,2/))
    iret = nf90_put_var(ncid, solai_id,      SOLAI,       start=(/itime+1,1/),count=(/1,2/))
    iret = nf90_put_var(ncid, swdown_id,     SWDOWN,      start=(/itime+1/))
    iret = nf90_put_var(ncid, bdfall_id,     BDFALL,      start=(/itime+1/))
    iret = nf90_put_var(ncid, rain_id,       RAIN,        start=(/itime+1/))
    iret = nf90_put_var(ncid, snow_id,       SNOW,        start=(/itime+1/))
    iret = nf90_put_var(ncid, fp_id,         FP,          start=(/itime+1/))
    iret = nf90_put_var(ncid, fpice_id,      FPICE,       start=(/itime+1/))
    iret = nf90_put_var(ncid, prcp_id,       PRCP,        start=(/itime+1/))


  end subroutine add_to_output


  subroutine finalize_output()

   implicit none
   
    iret = nf90_close(ncid)

  end subroutine finalize_output

end module output
