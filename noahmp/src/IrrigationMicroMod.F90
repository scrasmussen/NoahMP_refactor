module IrrigationMicroMod

!!! Estimate irrigation water depth (m) based on Micro irrigation method
!!! Reference: chapter 7 of NRCS, Part 623 National Engineering Handbook
!!! Irrigation water is applied under the canopy, within first layer 
!!! (at ~5 cm depth) considering current soil moisture

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use IrrigationInfilPhilipMod, only : IrrigationInfilPhilip

  implicit none

contains

  subroutine IrrigationMicro(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: MICRO_IRRIGATION
! Original code: P. Valayamkunnath (NCAR) <prasanth@ucar.edu> (08/06/2020)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp) :: FSUR         ! surface infiltration rate (m/s)
    real(kind=kind_noahmp) :: TEMP_RATE    ! temporary micro irrigation rate m/timestep

! --------------------------------------------------------------------
    associate(                                                        &
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,     noahmp main time step (s)
              DepthSoilLayer           => noahmp%config%domain%DepthSoilLayer          ,& ! in,     depth [m] of layer-bottom from soil surface
              IrrigationFracMicro           => noahmp%water%state%IrrigationFracMicro            ,& ! in,     fraction of grid under micro irrigation (0 to 1)
              IrriMicroRate      => noahmp%water%param%IrriMicroRate       ,& ! in,     micro irrigation rate (mm/hr)
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! inout,  soil water content [m3/m3]
              IrrigationAmtMicro         => noahmp%water%state%IrrigationAmtMicro          ,& ! inout,  micro irrigation water amount [m]
              IrrigationRateMicro        => noahmp%water%flux%IrrigationRateMicro           & ! inout,  micro irrigation water rate [m/timestep]
             )
! ----------------------------------------------------------------------
    
    ! initialize local variables
    FSUR      = 0.0
    TEMP_RATE = 0.0

    ! estimate infiltration rate based on Philips Eq.
    call IrrigationInfilPhilip(noahmp, MainTimeStep, FSUR)

    ! irrigation rate of micro irrigation
    TEMP_RATE = IrriMicroRate * (1.0/1000.0) * MainTimeStep/ 3600.0   ! NRCS rate/time step - calibratable
    IrrigationRateMicro  = min( 0.5*FSUR*MainTimeStep, IrrigationAmtMicro, TEMP_RATE )   ! Limit the application rate to minimum of 0.5*infiltration rate
                                                         ! and to the NRCS recommended rate, (m)
    IrrigationRateMicro  = IrrigationRateMicro * IrrigationFracMicro

    if ( IrrigationRateMicro >= IrrigationAmtMicro ) then
       IrrigationRateMicro  = IrrigationAmtMicro
       IrrigationAmtMicro   = 0.0
    else
       IrrigationAmtMicro = IrrigationAmtMicro - IrrigationRateMicro
    endif

    ! update soil moisture
    ! we implement drip in first layer of the Noah-MP. Change layer 1 moisture wrt to MI rate
    SoilLiqWater(1) = SoilLiqWater(1) + ( IrrigationRateMicro / (-1.0*DepthSoilLayer(1)) )

    end associate

  end subroutine IrrigationMicro

end module IrrigationMicroMod
