program phenology_driver

use output
use phenology_routines !only:ATM, noahmp_parameters, noahmp_options
use NOAHMP_TABLES

  implicit none

!---------------------------------------------------------------------
!  inputs start
!---------------------------------------------------------------------
  type (noahmp_parameters) :: parameters
  real          :: dt
  integer       :: maxtime
  character*256 :: output_filename
  integer       :: VEGTYPE
  integer       :: croptype
  integer       :: YEARLEN
  integer       :: idveg
  real          :: SNOWH
  real          :: TV
  real          :: LAT
  real          :: JULIAN
  real          :: LAI
  real          :: SAI
  real          :: TROOT
  real          :: ELAI
  real          :: ESAI
  real          :: IGS
  integer       :: PGS

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------

  integer :: itime, iz, ihr     ! some loop counters
  integer :: ntime      = 0     ! number of timesteps to run


!---------------------------------------------------------------------
! read namelist (for test, include MPTABLE in namelist) 
!---------------------------------------------------------------------

  namelist / timing          / dt,maxtime, output_filename
  namelist / forcing         / croptype,vegtype,yearlen,&
                               snowh,tv,lat,troot, pgs
  namelist / options         / idveg 

!---------------------------------------------------------------------
!  read input file
!---------------------------------------------------------------------

  open(30, file="namelist.input", form="formatted")
   read(30, timing)
   read(30, forcing)
   read(30, options)
  close(30)

!---------------------------------------------------------------------
!  initialize variables
!---------------------------------------------------------------------

  LAI  = 0.
  SAI  = 0.
  ELAI = 0.
  ESAI = 0.
  IGS  = 0.

!---------------------------------------------------------------------
! transfer noah-mp options
!---------------------------------------------------------------------

  call noahmp_options(idveg)


!---------------------------------------------------------------------
!  read noah-mp parameter tables
!---------------------------------------------------------------------
 
  call read_mp_veg_parameters("MODIFIED_IGBP_MODIS_NOAH")  

!---------------------------------------------------------------------
!  transfer parameters  based on TRANSFER_MP_PARAMETERS
!---------------------------------------------------------------------

  parameters%ISWATER   =   ISWATER_TABLE
  parameters%ISBARREN  =  ISBARREN_TABLE
  parameters%ISICE     =     ISICE_TABLE
  parameters%ISCROP    =    ISCROP_TABLE
  parameters%EBLFOREST = EBLFOREST_TABLE
  parameters%URBAN_FLAG = .FALSE.

!------------------------------------------------------------------------------------------!
! Transfer veg parameters
!------------------------------------------------------------------------------------------!

  parameters%HVT    =    HVT_TABLE(VEGTYPE)       !top of canopy (m)
  parameters%HVB    =    HVB_TABLE(VEGTYPE)       !bottom of canopy (m)
  parameters%SAIM   =   SAIM_TABLE(VEGTYPE,:)     !monthly stem area index, one-sided
  parameters%LAIM   =   LAIM_TABLE(VEGTYPE,:)     !monthly leaf area index, one-sided

!---------------------------------------------------------------------
! create output file and add initial values
!---------------------------------------------------------------------

  ntime  = maxtime

  call initialize_output(output_filename, ntime)


  do itime = 1, ntime

     JULIAN = itime * 1.0
     
     call PHENOLOGY (parameters, VEGTYPE, croptype, SNOWH,   TV,  LAT, YEARLEN, JULIAN, & !in
                            LAI,    SAI,    TROOT,  ELAI, ESAI,  IGS, PGS)

     
     call add_to_output(itime, VEGTYPE, croptype, SNOWH,   TV,  LAT, YEARLEN, JULIAN, & 
                          LAI,    SAI,    TROOT,  ELAI, ESAI,  IGS, PGS)

  enddo

  call finalize_output()

end program phenology_driver
