Program PhenologyDriver

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use InputVarType
  use InputVarInitMod
  use ConfigVarInitMod
  use EnergyVarInitMod
  use WaterVarInitMod
  use BiochemVarInitMod
  use PhenologyMainMod
  use phenology_output

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
  call EnergyVarInitDefault(noahmp)
  call EnergyVarInitTransfer(noahmp, input)
  call WaterVarInitDefault(noahmp)
  call WaterVarInitTransfer(noahmp, input)
  call BiochemVarInitDefault(noahmp)
  call BiochemVarInitTransfer(noahmp, input)

!---------------------------------------------------------------------
! create output file and add initial values
!---------------------------------------------------------------------

  ntime  = input%maxtime

  call initialize_output(input, ntime)
  
  do itime = 1, ntime

     noahmp%config%domain%JULIAN = itime * 1.0

     call PhenologyMain(noahmp)
     
     call add_to_output(itime,noahmp)

  end do

  call finalize_output()

end program PhenologyDriver
