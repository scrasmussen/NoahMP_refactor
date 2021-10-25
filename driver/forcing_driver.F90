!
! compile:
!

program forcing_driver

  use ConfigInitMod
  use NoahmpType
  use InputType
  use InputInitMod
  use ForcingInitMod
  use WaterInitMod
  use EnergyInitMod
  use AtmosForcingMod

  use output  

  implicit none

!---------------------------------------------------------------------
!  inputs start
!---------------------------------------------------------------------
  type(input_type)                :: input
  type(noahmp_type)               :: noahmp

  real(kind=kind_noahmp)          :: dt
  integer                         :: maxtime
  character*256                   :: output_filename
  real(kind=kind_noahmp)          :: prate_conv
  real(kind=kind_noahmp)          :: prate_nconv
  real(kind=kind_noahmp)          :: prate_shalw
  real(kind=kind_noahmp)          :: prate_snow
  real(kind=kind_noahmp)          :: prate_grup
  real(kind=kind_noahmp)          :: prate_hail
  real(kind=kind_noahmp)          :: pres_sfc
  real(kind=kind_noahmp)          :: spec_humd
  real(kind=kind_noahmp)          :: zenith
  real(kind=kind_noahmp)          :: t2max
  real(kind=kind_noahmp)          :: t2min
  real(kind=kind_noahmp)          :: sradmax
  real(kind=kind_noahmp)          :: sradmin
  integer       :: iopt_snf

  integer       :: rain_duration
  integer       :: dry_duration
  logical       :: raining
  integer       :: xstart
  integer       :: xend
  integer       :: kds
  integer       :: kde
  integer       :: kts
  integer       :: kte
  integer       :: ystart
  integer       :: yend

  real(kind=kind_noahmp), dimension (24), parameter :: mul_factor =&
                                                  (/0.0,  0.0,  0.0,  0.0,  0.0,  0.0, &
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
  xstart  = 1
  xend    = 1
  kds     = 1
  kde     = 2
  kts     = 1
  kte     = 1
  ystart  = 1
  yend    = 2

  input%XSTART = xstart
  input%XEND   = xend
  input%KDS    = kds
  input%KDE    = kde
  input%KTS    = kts
  input%KTE    = kte
  input%YSTART = ystart
  input%YEND   = yend

  input%IOPT_SNF = iopt_snf


  call ConfigInitDefault(noahmp)
  call InputInitDefault(input)
  call ForcingInitDefault(noahmp)
  call WaterInitDefault(noahmp)
  call EnergyInitDefault(noahmp)


  noahmp%config%domain%iloc = 1
  noahmp%config%domain%jloc = 1
  input%timestep            = dt
  input%COSZIN (XSTART:XEND, YSTART:YEND) = zenith

  call ConfigInitTransfer(noahmp, input)


!---------------------------------------------------------------------

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
!  create fake forcings
!---------------------------------------------------------------------    

    input%P8W3D (XSTART:XEND,KDS:KDE,YSTART:YEND) =  pres_sfc
    input%QV3D  (XSTART:XEND,KDS:KDE,YSTART:YEND) =  spec_humd

    if(raining) then

      input%MP_RAINC  (XSTART:XEND,YSTART:YEND) = prate_conv/3600.0  ! input water mm/s
      input%MP_RAINNC (XSTART:XEND,YSTART:YEND) = prate_nconv/3600.0 ! input water mm/s
      input%MP_SHCV   (XSTART:XEND,YSTART:YEND) = prate_shalw/3600.0 ! input water mm/s
      input%MP_SNOW   (XSTART:XEND,YSTART:YEND) = prate_snow/3600.0  ! input water mm/s
      input%MP_GRAUP  (XSTART:XEND,YSTART:YEND) = prate_grup/3600.0  ! input water mm/s
      input%MP_HAIL   (XSTART:XEND,YSTART:YEND) = prate_hail/3600.0  ! input water mm/s

      rain_step = rain_step + 1
      if(rain_step == rain_steps) then      ! event length met
        rain_step = 0
        raining   = .false.
      end if
    else

      input%MP_RAINC  (XSTART:XEND,YSTART:YEND) = 0.0
      input%MP_RAINNC (XSTART:XEND,YSTART:YEND) = 0.0
      input%MP_SHCV   (XSTART:XEND,YSTART:YEND) = 0.0
      input%MP_SNOW   (XSTART:XEND,YSTART:YEND) = 0.0
      input%MP_GRAUP  (XSTART:XEND,YSTART:YEND) = 0.0
      input%MP_HAIL   (XSTART:XEND,YSTART:YEND) = 0.0

      dry_step = dry_step + 1
      if(dry_step == dry_steps) then        ! between event length met
        dry_step = 0
        raining  = .true.
      end if
    end if

    ! create an arbitary sub-daily shortwave and temperature cycle
    ihr   = ((itime/24.0)-INT(itime/24.0))*24.0
    if(ihr==0) ihr=1

    input%SWDOWN (XSTART:XEND,YSTART:YEND)      = (sradmax-sradmin)*mul_factor(ihr)
    input%T3D (XSTART:XEND,KDS:KDE,YSTART:YEND) = (t2max-t2min)*mul_factor(ihr)+t2min 


!---------------------------------------------------------------------
!  transfer forcings
!---------------------------------------------------------------------
    call ForcingInitTransfer(noahmp, input)

!---------------------------------------------------------------------
!  call ProcessAtmosphericForcing
!---------------------------------------------------------------------
    call ProcessAtmosphericForcing(noahmp)

!---------------------------------------------------------------------
!  call output
!---------------------------------------------------------------------
    call add_to_output(itime,noahmp)      
 
  end do ! time loop

  call finalize_output()

end program
