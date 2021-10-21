!
! compile:
!

program forcing_driver

use output
use forcing_routines !only:ATM, noahmp_parameters, noahmp_options

  implicit none

!---------------------------------------------------------------------
!  inputs start
!---------------------------------------------------------------------

  real          :: dt
  integer       :: maxtime
  character*256 :: output_filename
  real          :: prate_conv
  real          :: prate_nconv
  real          :: prate_shalw
  real          :: prate_snow
  real          :: prate_grup
  real          :: prate_hail
  real          :: pres_sfc
  real          :: spec_humd
  real          :: zenith
  real          :: t2max
  real          :: t2min
  real          :: sradmax
  real          :: sradmin
  integer       :: iopt_snf

  integer       :: rain_duration
  integer       :: dry_duration
  logical       :: raining

  real, dimension (24), parameter :: mul_factor = (/0.0,  0.0,  0.0,  0.0,  0.0,  0.0, &
                                                   0.05, 0.10, 0.40, 0.65, 0.80, 0.90, &
                                                    1.0, 0.90, 0.80, 0.65, 0.40, 0.10, &
                                                   0.05,  0.0,  0.0,  0.0,  0.0,  0.0/)  

! read namelist (for test, include MPTABLE in namelist) 
  namelist / timing          / dt,maxtime, output_filename
  namelist / forcing         / rain_duration, dry_duration, raining, &
                               prate_conv, prate_nconv, prate_shalw, prate_snow,&
                               prate_grup, prate_hail, pres_sfc, spec_humd,&
                               zenith, t2max, t2min, sradmax, sradmin
  namelist / options         / iopt_snf 
!---------------------------------------------------------------------
!  inputs end
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!  variables passed to ATM
!---------------------------------------------------------------------
! inputs to ATM
  type (noahmp_parameters)       :: parameters ! no parameters used in ATM 
  REAL                           :: SFCPRS !pressure (pa)
  REAL                           :: SFCTMP !surface air temperature [k]
  REAL                           :: Q2     !mixing ratio (kg/kg)
  REAL                           :: PRCPCONV ! convective precipitation entering  [mm/s]    ! MB/AN : v3.7
  REAL                           :: PRCPNONC ! non-convective precipitation entering [mm/s] ! MB/AN : v3.7
  REAL                           :: PRCPSHCV ! shallow convective precip entering  [mm/s]   ! MB/AN : v3.7
  REAL                           :: PRCPSNOW ! snow entering land model [mm/s]              ! MB/AN : v3.7
  REAL                           :: PRCPGRPL ! graupel entering land model [mm/s]           ! MB/AN : v3.7
  REAL                           :: PRCPHAIL ! hail entering land model [mm/s]              ! MB/AN : v3.7
  REAL                           :: SOLDN  !downward shortwave radiation (w/m2)
  REAL                           :: COSZ   !cosine solar zenith angle [0-1]

! outputs from ATM
  REAL                           :: THAIR  !potential temperature (k)
  REAL                           :: QAIR   !specific humidity (kg/kg) (q2/(1+q2))
  REAL                           :: EAIR   !vapor pressure air (pa)
  REAL                           :: RHOAIR !density air (kg/m3)
  REAL                           :: QPRECC !convective precipitation (mm/s)
  REAL                           :: QPRECL !large-scale precipitation (mm/s)
  REAL, DIMENSION(       1:   2) :: SOLAD  !incoming direct solar radiation (w/m2)
  REAL, DIMENSION(       1:   2) :: SOLAI  !incoming diffuse solar radiation (w/m2)
  REAL                           :: SWDOWN !downward solar filtered by sun angle [w/m2]
  REAL                           :: BDFALL  !!bulk density of snowfall (kg/m3) AJN
  REAL                           :: RAIN    !rainfall (mm/s) AJN
  REAL                           :: SNOW    !liquid equivalent snowfall (mm/s) AJN
  REAL                           :: FP      !fraction of area receiving precipitation  AJN
  REAL                           :: FPICE   !fraction of ice                AJN
  REAL                           :: PRCP    !total precipitation [mm/s]     ! MB/AN : v3.7

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------

  integer :: itime, iz, ihr     ! some loop counters
  integer :: ntime      = 0     ! number of timesteps to run
  integer :: rain_steps = 0     ! number of timesteps in rain event
  integer :: dry_steps  = 0     ! number of timesteps between rain events
  integer :: rain_step  = 0     ! number of timesteps in current event
  integer :: dry_step   = 0     ! number of timesteps in current event

!---------------------------------------------------------------------
!  end declarations
!---------------------------------------------------------------------

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

 SFCPRS   = 0.
 SFCTMP   = 0.
 Q2       = 0.
 PRCPCONV = 0.
 PRCPNONC = 0.
 PRCPSHCV = 0.
 PRCPSNOW = 0.
 PRCPGRPL = 0.
 PRCPHAIL = 0.
 SOLDN    = 0.
 COSZ     = 0.
 THAIR    = 0.
 QAIR     = 0.
 EAIR     = 0.
 RHOAIR   = 0.
 QPRECC   = 0.
 QPRECL   = 0.
 SOLAD    = 0.
 SOLAI    = 0.
 SWDOWN   = 0.
 BDFALL   = 0.
 RAIN     = 0.
 SNOW     = 0.
 FP       = 0.
 FPICE    = 0.
 PRCP     = 0.

!---------------------------------------------------------------------
! transfer noah-mp options
!---------------------------------------------------------------------

 call noahmp_options(iopt_snf)

! modulate precipitation 
  ntime      =  nint(maxtime * 3600.0 / dt)
  rain_steps =  rain_duration * 3600.0 / dt
  dry_steps  =  dry_duration * 3600.0 / dt

!---------------------------------------------------------------------
! create output file and add initial values
!---------------------------------------------------------------------

  call initialize_output(output_filename, ntime+1)

!---------------------------------------------------------------------
! start the time loop
!---------------------------------------------------------------------

  do itime = 1, ntime
!---------------------------------------------------------------------
!  transfer forcings
!---------------------------------------------------------------------

    SFCPRS   = pres_sfc
    Q2       = spec_humd
    COSZ     = zenith

    if(raining) then
      PRCPCONV = prate_conv/3600.0  ! input water mm/s
      PRCPNONC = prate_nconv/3600.0 ! input water mm/s
      PRCPSHCV = prate_shalw/3600.0 ! input water mm/s
      PRCPSNOW = prate_snow/3600.0  ! input water mm/s
      PRCPGRPL = prate_grup/3600.0  ! input water mm/s
      PRCPHAIL = prate_hail/3600.0  ! input water mm/s
      rain_step = rain_step + 1
      if(rain_step == rain_steps) then      ! event length met
        rain_step = 0
        raining   = .false.
      end if
    else
      PRCPCONV = 0.
      PRCPNONC = 0.
      PRCPSHCV = 0.
      PRCPSNOW = 0.
      PRCPGRPL = 0.
      PRCPHAIL = 0.
      dry_step = dry_step + 1
      if(dry_step == dry_steps) then        ! between event length met
        dry_step = 0
        raining  = .true.
      end if
    end if

    ! create an arbitary sub-daily shortwave and temperature cycle
    ihr   = ((itime/24.0)-INT(itime/24.0))*24.0
    if(ihr==0) ihr=1

    SOLDN  = (sradmax-sradmin)*mul_factor(ihr)
    SFCTMP = (t2max-t2min)*mul_factor(ihr)+t2min 

    CALL ATM (parameters,SFCPRS  ,SFCTMP   ,Q2      ,                            &
              PRCPCONV, PRCPNONC,PRCPSHCV,PRCPSNOW,PRCPGRPL,PRCPHAIL, &
              SOLDN   ,COSZ     ,THAIR   ,QAIR    ,                   & 
              EAIR    ,RHOAIR   ,QPRECC  ,QPRECL  ,SOLAD   ,SOLAI   , &
              SWDOWN  ,BDFALL   ,RAIN    ,SNOW    ,FP      ,FPICE   , PRCP )

    call add_to_output(itime,SFCPRS,SFCTMP,Q2,PRCPCONV,PRCPNONC,PRCPSHCV,& !in
                       PRCPSNOW,PRCPGRPL,PRCPHAIL,SOLDN,COSZ,            & !in
                       THAIR,QAIR,EAIR,RHOAIR,QPRECC,QPRECL,SOLAD,       & !out
                       SOLAI,SWDOWN,BDFALL,RAIN,SNOW,FP,FPICE,PRCP)        !out
 
  end do ! time loop

  call finalize_output()

end program
