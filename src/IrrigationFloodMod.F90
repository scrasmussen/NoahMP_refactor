module IrrigationFloodMod

!!! Estimate irrigation water depth (m) based on surface flooding irrigation method
!!! Reference: chapter 4 of NRCS, Part 623 National Engineering Handbook
!!! Irrigation water is applied on the surface based on present soil moisture and
!!! infiltration rate of the soil. Flooding or overland flow is based on infiltration excess

  use Machine, only : kind_noahmp
  use NoahmpType
  use ConstantDefineMod
  use IrrigationPhilipInfilMod, only: IrrigationPhilipInfil

  implicit none

contains

  subroutine FloodIrrigation(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: FLOOD_IRRIGATION
! Original code: P. Valayamkunnath (NCAR) <prasanth@ucar.edu> (08/06/2020)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                        &
              DT              => noahmp%config%domain%DT             ,& ! in,     noahmp time step (s)
              FIFAC           => noahmp%water%state%FIFAC            ,& ! in,     fraction of grid under flood irrigation (0 to 1)
              FIRTFAC         => noahmp%water%param%FIRTFAC          ,& ! in,     flood application rate factor
              IRAMTFI         => noahmp%water%state%IRAMTFI          ,& ! inout,  flood irrigation water amount [m]
              IRFIRATE        => noahmp%water%flux%IRFIRATE          ,& ! inout,  flood irrigation water rate [m/timestep]
              FSUR            => noahmp%water%flux%FloodIrriFSUR      & ! out,    flood irrigation infiltration rate [m/s]
             )
! ----------------------------------------------------------------------

    ! initialize out-only variables
    FSUR = 0.0

    ! estimate infiltration rate based on Philips Eq.
    call IrrigationPhilipInfil(noahmp)  

    ! irrigation rate of flood irrigation. It should be
    ! greater than infiltration rate to get infiltration
    ! excess runoff at the time of application
    IRFIRATE = FSUR * DT * FIRTFAC   ! Limit the application rate to fac*infiltration rate 
    IRFIRATE = IRFIRATE * FIFAC

    if ( IRFIRATE >= IRAMTFI ) then
       IRFIRATE = IRAMTFI
       IRAMTFI  = 0.0
    else
       IRAMTFI  = IRAMTFI - IRFIRATE
    endif

    end associate

  end subroutine FloodIrrigation

end module IrrigationFloodMod
