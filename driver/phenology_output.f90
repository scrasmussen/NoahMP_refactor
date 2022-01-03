module output

  use netcdf
  
  implicit none  

  integer           :: ncid
  integer           :: iret
  integer           :: time_dim
  integer           :: time_id

  integer           :: vegtype_id
  integer           :: croptype_id 
  integer           :: snowh_id   
  integer           :: tv_id  
  integer           :: lat_id 
  integer           :: yearlen_id 
  integer           :: julian_id 
  integer           :: lai_id    
  integer           :: sai_id   
  integer           :: troot_id  
  integer           :: elai_id 
  integer           :: esai_id  
  integer           :: igs_id 
  integer           :: pgs_id

contains

  subroutine initialize_output(output_filename, ntime)

    implicit none

    integer       :: ntime
    character*256 :: output_filename

    iret = nf90_create(trim(output_filename), NF90_CLOBBER, ncid)
    iret = nf90_def_dim(ncid, "time",  ntime, time_dim)

    iret = nf90_def_var(ncid, "timestep", NF90_INT  , (/time_dim/), time_id)
    iret = nf90_def_var(ncid, "VEGTYP",   NF90_FLOAT, (/time_dim/), vegtype_id)
    iret = nf90_def_var(ncid, "croptype", NF90_FLOAT, (/time_dim/), croptype_id)
    iret = nf90_def_var(ncid, "SNOWH",    NF90_FLOAT, (/time_dim/), snowh_id)
    iret = nf90_def_var(ncid, "TV",       NF90_FLOAT, (/time_dim/), tv_id)
    iret = nf90_def_var(ncid, "LAT",      NF90_FLOAT, (/time_dim/), lat_id)
    iret = nf90_def_var(ncid, "YEARLEN",  NF90_FLOAT, (/time_dim/), yearlen_id)
    iret = nf90_def_var(ncid, "JULIAN",   NF90_FLOAT, (/time_dim/), julian_id)
    iret = nf90_def_var(ncid, "LAI",      NF90_FLOAT, (/time_dim/), lai_id)
    iret = nf90_def_var(ncid, "SAI",      NF90_FLOAT, (/time_dim/), sai_id)
    iret = nf90_def_var(ncid, "TROOT",    NF90_FLOAT, (/time_dim/), troot_id)
    iret = nf90_def_var(ncid, "ELAI",     NF90_FLOAT, (/time_dim/), elai_id)
    iret = nf90_def_var(ncid, "ESAI",     NF90_FLOAT, (/time_dim/), esai_id)
    iret = nf90_def_var(ncid, "IGS",      NF90_FLOAT, (/time_dim/), igs_id)
    iret = nf90_def_var(ncid, "PGS",      NF90_FLOAT, (/time_dim/), pgs_id)

    iret = nf90_enddef(ncid)

  end subroutine initialize_output


  subroutine add_to_output(itime, VEGTYP, croptype, SNOWH,   TV,  LAT, YEARLEN, JULIAN, & 
                             LAI,    SAI,    TROOT,  ELAI, ESAI,  IGS, PGS)

    implicit none

    integer           , intent(in)  :: itime
    integer           , intent(in)  :: VEGTYP
    integer           , intent(in)  :: croptype
    integer           , intent(in)  :: YEARLEN
    real              , intent(in)  :: SNOWH
    real              , intent(in)  :: TV
    real              , intent(in)  :: LAT 
    real              , intent(in)  :: JULIAN
    real              , intent(in)  :: LAI
    real              , intent(in)  :: SAI
    real              , intent(in)  :: TROOT
    real              , intent(in)  :: ELAI 
    real              , intent(in)  :: ESAI
    real              , intent(in)  :: IGS
    integer           , intent(in)  :: PGS


    iret = nf90_put_var(ncid,     time_id,     itime,      start=(/itime+1/))
    iret = nf90_put_var(ncid,  vegtype_id,    VEGTYP,      start=(/itime+1/))
    iret = nf90_put_var(ncid, croptype_id,  croptype,      start=(/itime+1/))
    iret = nf90_put_var(ncid,    snowh_id,     SNOWH,      start=(/itime+1/))
    iret = nf90_put_var(ncid,       tv_id,        TV,      start=(/itime+1/))
    iret = nf90_put_var(ncid,      lat_id,       LAT,      start=(/itime+1/))
    iret = nf90_put_var(ncid,  yearlen_id,   YEARLEN,      start=(/itime+1/))
    iret = nf90_put_var(ncid,   julian_id,    JULIAN,      start=(/itime+1/))
    iret = nf90_put_var(ncid,      lai_id,       LAI,      start=(/itime+1/))
    iret = nf90_put_var(ncid,      sai_id,       SAI,      start=(/itime+1/))
    iret = nf90_put_var(ncid,    troot_id,     TROOT,      start=(/itime+1/))
    iret = nf90_put_var(ncid,     elai_id,      ELAI,      start=(/itime+1/))
    iret = nf90_put_var(ncid,     esai_id,      ESAI,      start=(/itime+1/))
    iret = nf90_put_var(ncid,      igs_id,       IGS,      start=(/itime+1/))
    iret = nf90_put_var(ncid,      pgs_id,       PGS,      start=(/itime+1/))

  end subroutine add_to_output


  subroutine finalize_output()

   implicit none
   
    iret = nf90_close(ncid)

  end subroutine finalize_output

end module output
