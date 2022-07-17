module IrrigationFloodMod

!!! Estimate irrigation water depth (m) based on surface flooding irrigation method
!!! Reference: chapter 4 of NRCS, Part 623 National Engineering Handbook
!!! Irrigation water is applied on the surface based on present soil moisture and
!!! infiltration rate of the soil. Flooding or overland flow is based on infiltration excess

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use IrrigationInfilPhilipMod, only : IrrigationInfilPhilip

  implicit none

contains

  subroutine IrrigationFlood(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: FLOOD_IRRIGATION
! Original code: P. Valayamkunnath (NCAR) <prasanth@ucar.edu> (08/06/2020)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp) :: FSUR     ! surface infiltration rate (m/s)

! --------------------------------------------------------------------
    associate(                                                        &
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,     noahmp main time step (s)
              IrrigationFracFlood           => noahmp%water%state%IrrigationFracFlood            ,& ! in,     fraction of grid under flood irrigation (0 to 1)
              FIRTFAC         => noahmp%water%param%FIRTFAC          ,& ! in,     flood application rate factor
              IrrigationAmtFlood         => noahmp%water%state%IrrigationAmtFlood          ,& ! inout,  flood irrigation water amount [m]
              SoilSfcInflow          => noahmp%water%flux%SoilSfcInflow            ,& ! inout,  water input on soil surface [mm/s]
              IrrigationRateFlood        => noahmp%water%flux%IrrigationRateFlood           & ! inout,  flood irrigation water rate [m/timestep]
             )
! ----------------------------------------------------------------------

    ! initialize local variables
    FSUR = 0.0

    ! estimate infiltration rate based on Philips Eq.
    call IrrigationInfilPhilip(noahmp, MainTimeStep, FSUR)  

    ! irrigation rate of flood irrigation. It should be
    ! greater than infiltration rate to get infiltration
    ! excess runoff at the time of application
    IrrigationRateFlood = FSUR * MainTimeStep * FIRTFAC   ! Limit the application rate to fac*infiltration rate 
    IrrigationRateFlood = IrrigationRateFlood * IrrigationFracFlood

    if ( IrrigationRateFlood >= IrrigationAmtFlood ) then
       IrrigationRateFlood = IrrigationAmtFlood
       IrrigationAmtFlood  = 0.0
    else
       IrrigationAmtFlood  = IrrigationAmtFlood - IrrigationRateFlood
    endif

    ! update water flux going to surface soil
    SoilSfcInflow = SoilSfcInflow + (IrrigationRateFlood / MainTimeStep)  ! [m/s]

    end associate

  end subroutine IrrigationFlood

end module IrrigationFloodMod
