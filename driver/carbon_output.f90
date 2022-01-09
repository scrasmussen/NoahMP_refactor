module output

  use netcdf
  
  implicit none  

  integer           :: ncid
  integer           :: iret
  integer           :: time_dim
  integer           :: soil_dim
  integer           :: snow_dim
  integer           :: snso_dim

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
  integer           :: ZSOIL_id
  integer           :: DZSNSO_id
  integer           :: STC_id
  integer           :: SMC_id
  integer           :: PSN_id
  integer           :: FOLN_id
  integer           :: BTRAN_id
  integer           :: SOLDN_id
  integer           :: T2M_id
  integer           :: LFMASS_id
  integer           :: RTMASS_id
  integer           :: STMASS_id
  integer           :: WOOD_id
  integer           :: STBLCP_id
  integer           :: FASTCP_id
  integer           :: GRAIN_id
  integer           :: GDD_id
  integer           :: GPP_id
  integer           :: NPP_id
  integer           :: NEE_id
  integer           :: AUTORS_id
  integer           :: HETERS_id
  integer           :: TOTSC_id
  integer           :: TOTLB_id
  integer           :: TG_id
  integer           :: NSNOW_id
  integer           :: NSOIL_id
  integer           :: DT_id
  integer           :: APAR_id
  integer           :: FVEG_id

contains

  subroutine initialize_output(output_filename, ntime, nsoil, nsnow)

    implicit none

    integer, intent(in)       :: ntime, nsoil, nsnow
    character*256             :: output_filename
    

    iret = nf90_create(trim(output_filename), NF90_CLOBBER, ncid)
    iret = nf90_def_dim(ncid, "time",  ntime, time_dim)
    iret = nf90_def_dim(ncid, "soil", nsoil, soil_dim)
    iret = nf90_def_dim(ncid, "snso", nsnow+nsoil, snso_dim)

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
    iret = nf90_def_var(ncid, "ZSOIL",    NF90_FLOAT, (/time_dim,soil_dim/), ZSOIL_id)
    iret = nf90_def_var(ncid, "DZSNSO",   NF90_FLOAT, (/time_dim,snso_dim/), DZSNSO_id)
    iret = nf90_def_var(ncid, "STC",      NF90_FLOAT, (/time_dim,snso_dim/), STC_id)
    iret = nf90_def_var(ncid, "SMC",      NF90_FLOAT, (/time_dim,soil_dim/), SMC_id)
    iret = nf90_def_var(ncid, "PSN",      NF90_FLOAT, (/time_dim/), PSN_id)
    iret = nf90_def_var(ncid, "FOLN",     NF90_FLOAT, (/time_dim/), FOLN_id)
    iret = nf90_def_var(ncid, "BTRAN",    NF90_FLOAT, (/time_dim/), BTRAN_id)
    iret = nf90_def_var(ncid, "SOLDN",    NF90_FLOAT, (/time_dim/), SOLDN_id)
    iret = nf90_def_var(ncid, "T2M",      NF90_FLOAT, (/time_dim/), T2M_id)
    iret = nf90_def_var(ncid, "LFMASS",   NF90_FLOAT, (/time_dim/), LFMASS_id)
    iret = nf90_def_var(ncid, "RTMASS",   NF90_FLOAT, (/time_dim/), RTMASS_id)
    iret = nf90_def_var(ncid, "STMASS",   NF90_FLOAT, (/time_dim/), STMASS_id)
    iret = nf90_def_var(ncid, "WOOD",     NF90_FLOAT, (/time_dim/), WOOD_id)
    iret = nf90_def_var(ncid, "STBLCP",   NF90_FLOAT, (/time_dim/), STBLCP_id)
    iret = nf90_def_var(ncid, "FASTCP",   NF90_FLOAT, (/time_dim/), FASTCP_id)
    iret = nf90_def_var(ncid, "GRAIN",    NF90_FLOAT, (/time_dim/), GRAIN_id)
    iret = nf90_def_var(ncid, "GDD",      NF90_FLOAT, (/time_dim/), GDD_id)
    iret = nf90_def_var(ncid, "GPP",      NF90_FLOAT, (/time_dim/), GPP_id)
    iret = nf90_def_var(ncid, "NPP",      NF90_FLOAT, (/time_dim/), NPP_id)
    iret = nf90_def_var(ncid, "NEE",      NF90_FLOAT, (/time_dim/), NEE_id)
    iret = nf90_def_var(ncid, "AUTORS",   NF90_FLOAT, (/time_dim/), AUTORS_id)
    iret = nf90_def_var(ncid, "HETERS",   NF90_FLOAT, (/time_dim/), HETERS_id)
    iret = nf90_def_var(ncid, "TOTSC",    NF90_FLOAT, (/time_dim/), TOTSC_id)
    iret = nf90_def_var(ncid, "TOTLB",    NF90_FLOAT, (/time_dim/), TOTLB_id)
    iret = nf90_def_var(ncid, "TG",       NF90_FLOAT, (/time_dim/), TG_id)
    iret = nf90_def_var(ncid, "NSNOW",    NF90_FLOAT, (/time_dim/), NSNOW_id)
    iret = nf90_def_var(ncid, "NSOIL",    NF90_FLOAT, (/time_dim/), NSOIL_id)
    iret = nf90_def_var(ncid, "DT",       NF90_FLOAT, (/time_dim/), DT_id)
    iret = nf90_def_var(ncid, "APAR",     NF90_FLOAT, (/time_dim/), APAR_id)
    iret = nf90_def_var(ncid, "FVEG",     NF90_FLOAT, (/time_dim/), FVEG_id)

    iret = nf90_enddef(ncid)

  end subroutine initialize_output


  subroutine add_to_output(itime, VEGTYP, croptype, SNOWH,     TV,    LAT, YEARLEN, JULIAN, &
                          LAI,     SAI,    TROOT,  ELAI,   ESAI,    IGS,     PGS,  ZSOIL, &
                       DZSNSO,     STC,      SMC,   PSN,   FOLN,  BTRAN,   SOLDN,    T2M, &
                       LFMASS,  RTMASS,   STMASS,  WOOD, STBLCP, FASTCP,   GRAIN,    GDD, &
                          GPP,     NPP,      NEE,AUTORS, HETERS,  TOTSC,   TOTLB,     TG, &
                        NSNOW,    NSOIL,    DT,   APAR,   FVEG)

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

    real, dimension(nsoil)              , intent(in)  :: ZSOIL
    real, dimension(nsoil+nsnow)        , intent(in)  :: DZSNSO
    real, dimension(nsoil+nsnow)        , intent(in)  :: STC     
    real, dimension(nsoil)              , intent(in)  :: SMC   
    real              , intent(in)  :: PSN   
    real              , intent(in)  :: FOLN  
    real              , intent(in)  :: BTRAN   
    real              , intent(in)  :: SOLDN    
    real              , intent(in)  :: T2M
    real              , intent(in)  :: LFMASS  
    real              , intent(in)  :: RTMASS   
    real              , intent(in)  :: STMASS  
    real              , intent(in)  :: WOOD 
    real              , intent(in)  :: STBLCP 
    real              , intent(in)  :: FASTCP   
    real              , intent(in)  :: GRAIN   
    real              , intent(in)  :: GDD
    real              , intent(in)  :: GPP     
    real              , intent(in)  :: NPP      
    real              , intent(in)  :: NEE
    real              , intent(in)  :: AUTORS 
    real              , intent(in)  :: HETERS  
    real              , intent(in)  :: TOTSC  
    real              , intent(in)  :: TOTLB     
    real              , intent(in)  :: TG
    integer           , intent(in)  :: NSNOW    
    integer           , intent(in)  :: NSOIL    
    real              , intent(in)  :: DT  
    real              , intent(in)  :: APAR   
    real              , intent(in)  :: FVEG


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

    iret = nf90_put_var(ncid,     ZSOIL_id, ZSOIL,         start=(/itime+1,1/), count=(/1,nsoil/))
    iret = nf90_put_var(ncid,    DZSNSO_id, DZSNSO,        start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
    iret = nf90_put_var(ncid,       STC_id, STC,           start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
    iret = nf90_put_var(ncid,       SMC_id, SMC,           start=(/itime+1,1/), count=(/1,nsoil/))
    iret = nf90_put_var(ncid,       PSN_id, PSN,           start=(/itime+1/))
    iret = nf90_put_var(ncid,      FOLN_id, FOLN,          start=(/itime+1/))
    iret = nf90_put_var(ncid,     BTRAN_id, BTRAN,         start=(/itime+1/))
    iret = nf90_put_var(ncid,     SOLDN_id, SOLDN,         start=(/itime+1/))
    iret = nf90_put_var(ncid,       T2M_id, T2M,           start=(/itime+1/))
    iret = nf90_put_var(ncid,    LFMASS_id, LFMASS,        start=(/itime+1/))
    iret = nf90_put_var(ncid,    RTMASS_id, RTMASS,        start=(/itime+1/))
    iret = nf90_put_var(ncid,    STMASS_id, STMASS,        start=(/itime+1/))
    iret = nf90_put_var(ncid,      WOOD_id, WOOD,          start=(/itime+1/))
    iret = nf90_put_var(ncid,    STBLCP_id, STBLCP,        start=(/itime+1/))
    iret = nf90_put_var(ncid,    FASTCP_id, FASTCP,        start=(/itime+1/))
    iret = nf90_put_var(ncid,     GRAIN_id, GRAIN,         start=(/itime+1/))
    iret = nf90_put_var(ncid,       GDD_id, GDD,           start=(/itime+1/))
    iret = nf90_put_var(ncid,       GPP_id, GPP,           start=(/itime+1/))
    iret = nf90_put_var(ncid,       NPP_id, NPP,           start=(/itime+1/))
    iret = nf90_put_var(ncid,       NEE_id, NEE,           start=(/itime+1/))
    iret = nf90_put_var(ncid,    AUTORS_id, AUTORS,        start=(/itime+1/))
    iret = nf90_put_var(ncid,    HETERS_id, HETERS,        start=(/itime+1/))
    iret = nf90_put_var(ncid,     TOTSC_id, TOTSC,         start=(/itime+1/))
    iret = nf90_put_var(ncid,     TOTLB_id, TOTLB,         start=(/itime+1/))
    iret = nf90_put_var(ncid,        TG_id, TG,            start=(/itime+1/))
    iret = nf90_put_var(ncid,     NSNOW_id, NSNOW,         start=(/itime+1/))
    iret = nf90_put_var(ncid,     NSOIL_id, NSOIL,         start=(/itime+1/))
    iret = nf90_put_var(ncid,        DT_id, DT,            start=(/itime+1/))
    iret = nf90_put_var(ncid,      APAR_id, APAR,          start=(/itime+1/))
    iret = nf90_put_var(ncid,      FVEG_id, FVEG,          start=(/itime+1/))

  end subroutine add_to_output


  subroutine finalize_output()

   implicit none
   
    iret = nf90_close(ncid)

  end subroutine finalize_output

end module output
