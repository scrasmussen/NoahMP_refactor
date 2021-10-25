module output

  use netcdf
  use NoahmpType 
  
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

  subroutine add_to_output(itime,noahmp)        !out

    implicit none

    integer                                     :: itime
    type(noahmp_type),intent(in)                :: noahmp
 
    associate(                                         &
              SFCPRS    => noahmp%forcing%SFCTMP      ,&
              SFCTMP    => noahmp%forcing%SFCTMP      ,&
              Q2        => noahmp%forcing%Q2          ,&
              PRCPCONV  => noahmp%forcing%PRCPCONV    ,&
              PRCPNONC  => noahmp%forcing%PRCPNONC    ,&
              PRCPSHCV  => noahmp%forcing%PRCPSHCV    ,&
              PRCPSNOW  => noahmp%forcing%PRCPSNOW    ,&
              PRCPGRPL  => noahmp%forcing%PRCPGRPL    ,&
              PRCPHAIL  => noahmp%forcing%PRCPHAIL    ,&
              SOLDN     => noahmp%forcing%SOLDN       ,&
              COSZ      => noahmp%config%domain%COSZ  ,&
              THAIR     => noahmp%energy%state%THAIR  ,&
              QAIR      => noahmp%energy%state%QAIR   ,&
              EAIR      => noahmp%energy%state%EAIR   ,&
              RHOAIR    => noahmp%energy%state%RHOAIR ,&
              QPRECC    => noahmp%water%flux%QPRECC   ,&
              QPRECL    => noahmp%water%flux%QPRECL   ,&
              SOLAD     => noahmp%energy%flux%SOLAD   ,&
              SOLAI     => noahmp%energy%flux%SOLAI   ,&
              SWDOWN    => noahmp%energy%flux%SWDOWN  ,&
              BDFALL    => noahmp%water%state%BDFALL  ,&
              RAIN      => noahmp%water%flux%RAIN     ,&
              SNOW      => noahmp%water%flux%SNOW     ,&
              FP        => noahmp%water%state%FP      ,&
              FPICE     => noahmp%water%state%FPICE   ,&
              PRCP      => noahmp%water%flux%PRCP      &
             )
    


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

    endassociate

  end subroutine add_to_output


  subroutine finalize_output()

   implicit none
   
    iret = nf90_close(ncid)

  end subroutine finalize_output

end module output
