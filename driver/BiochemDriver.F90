Program BiochemDriver

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use InputVarType
  use InputVarInitMod
  use ConfigVarInitMod
  use ForcingVarInitMod
  use EnergyVarInitMod
  use WaterVarInitMod
  use BiochemVarInitMod
  use PhenologyMainMod
  use BiochemCropMainMod
  use BiochemMainMod
  use BiochemOutput

  implicit none

!---------------------------------------------------------------------
!  types
!---------------------------------------------------------------------
  type(input_type)    :: input
  type(noahmp_type)   :: noahmp

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------

  integer :: itime, iz, ihr     ! some loop counters
  integer :: ntime      = 0     ! number of timesteps to run
  integer :: isoil
  logical :: crop_active = .false.
  logical :: dveg_active = .false.

  associate (                                          &
             SMC      => noahmp%water%state%SMC       ,&
             STC      => noahmp%energy%state%STC      ,&
             JULIAN   => noahmp%config%domain%JULIAN  ,&
             DVEG     => noahmp%config%nmlist%DVEG    ,&
             OPT_CROP => noahmp%config%nmlist%OPT_CROP,&
             CROPTYPE => noahmp%config%domain%CROPTYP ,&
             GPP      => noahmp%biochem%flux%GPP      ,&
             NPP      => noahmp%biochem%flux%NPP      ,&
             NEE      => noahmp%biochem%flux%NEE      ,&
             AUTORS   => noahmp%biochem%flux%AUTORS   ,&
             HETERS   => noahmp%biochem%flux%HETERS   ,&
             TOTSC    => noahmp%biochem%state%TOTSC   ,&
             TOTLB    => noahmp%biochem%state%TOTLB   ,&
             LAI      => noahmp%energy%state%LAI      ,&
             SAI      => noahmp%energy%state%SAI      ,&
             IGS      => noahmp%biochem%state%IGS     ,&
             PGS      => noahmp%biochem%state%PGS     ,&
             ELAI     => noahmp%energy%state%ELAI     ,&
             ESAI     => noahmp%energy%state%ESAI     ,&
             LFMASS   => noahmp%biochem%state%LFMASS  ,&
             RTMASS   => noahmp%biochem%state%RTMASS  ,&
             STMASS   => noahmp%biochem%state%STMASS  ,&
             WOOD     => noahmp%biochem%state%WOOD    ,&
             STBLCP   => noahmp%biochem%state%STBLCP  ,& 
             FASTCP   => noahmp%biochem%state%FASTCP  ,&
             GDD      => noahmp%biochem%state%GDD     ,&
             GRAIN    => noahmp%biochem%state%GRAIN    &
            ) 

!---------------------------------------------------------------------
!  read in input data from table and initial file
!---------------------------------------------------------------------
 
  call InputVarInitDefault(input)
  call ReadNamelist(input)
  call ReadNoahmpTable(input)

!---------------------------------------------------------------------
!  initialize
!---------------------------------------------------------------------
 
  call ConfigVarInitDefault(noahmp)
  call ConfigVarInitTransfer(noahmp, input)
  call ForcingVarInitDefault(noahmp)
  call ForcingVarInitTransfer(noahmp, input)
  call EnergyVarInitDefault(noahmp)
  call EnergyVarInitTransfer(noahmp, input)
  call WaterVarInitDefault(noahmp)
  call WaterVarInitTransfer(noahmp, input)
  call BiochemVarInitDefault(noahmp)
  call BiochemVarInitTransfer(noahmp, input)

  LFMASS = 0.0
  RTMASS = 0.0
  STMASS = 0.0
  WOOD   = 0.0
  STBLCP = 0.0
  FASTCP = 0.0
  GRAIN  = 0.0
  GDD    = 0.0
  GPP    = 0.0
  NPP    = 0.0
  NEE    = 0.0
  AUTORS = 0.0
  HETERS = 0.0
  TOTSC  = 0.0
  TOTLB  = 0.0
  LAI    = 0.0
  SAI    = 0.0
  ELAI   = 0.0
  ESAI   = 0.0
  IGS    = 0.0 
  PGS    = 3

  noahmp%water%state%SMC(1:4)   = 0.29
  noahmp%energy%state%STC(1:4)   = 280.0
  noahmp%energy%state%STC(-2:0)  = 0.0


!---------------------------------------------------------------------
! create output file and add initial values
!---------------------------------------------------------------------

  ntime  = input%maxtime

  call initialize_output(ntime, input, noahmp)

  do itime = 1, ntime

     JULIAN = itime * 1.0

     call PhenologyMain(noahmp)


! compute carbon budgets (carbon storages and co2 & bvoc fluxes)

     crop_active = .false.
     dveg_active = .false.
 
     if (DVEG == 2 .OR. DVEG == 5 .OR. DVEG == 6) dveg_active = .true.
     if (OPT_CROP > 0 .and. CROPTYPE > 0) then
       crop_active = .true.
       dveg_active = .false.
     endif

 
     if (dveg_active) then
 
        call BiochemMain (noahmp)
 
     endif
     
     if (OPT_CROP == 1 .and. crop_active) then

        call BiochemCropMain (noahmp)
 
     endif
 
     call add_to_output(itime,noahmp)
 
  end do

  call finalize_output()

  end associate

end program BiochemDriver
