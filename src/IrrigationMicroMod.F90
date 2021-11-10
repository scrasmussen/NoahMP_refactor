module IrrigationMicroMod

!!! Estimate irrigation water depth (m) based on Micro irrigation method
!!! Reference: chapter 7 of NRCS, Part 623 National Engineering Handbook
!!! Irrigation water is applied under the canopy, within first layer 
!!! (at ~5 cm depth) considering current soil moisture

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use IrrigationInfilPhilipMod, only : IrrigationInfilPhilip

  implicit none

contains

  subroutine MicroIrrigation(noahmp)

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
              DT              => noahmp%config%domain%DT             ,& ! in,     noahmp time step (s)
              MIFAC           => noahmp%water%state%MIFAC            ,& ! in,     fraction of grid under micro irrigation (0 to 1)
              MICIR_RATE      => noahmp%water%param%MICIR_RATE       ,& ! in,     micro irrigation rate (mm/hr)
              IRAMTMI         => noahmp%water%state%IRAMTMI          ,& ! inout,  micro irrigation water amount [m]
              IRMIRATE        => noahmp%water%flux%IRMIRATE           & ! inout,  micro irrigation water rate [m/timestep]
             )
! ----------------------------------------------------------------------
    
    ! initialize local variables
    FSUR      = 0.0
    TEMP_RATE = 0.0

    ! estimate infiltration rate based on Philips Eq.
    call IrrigationInfilPhilip(noahmp, DT, FSUR)

    ! irrigation rate of micro irrigation
    TEMP_RATE = MICIR_RATE * (1.0/1000.0) * DT/ 3600.0   ! NRCS rate/time step - calibratable
    IRMIRATE  = min( 0.5*FSUR*DT, IRAMTMI, TEMP_RATE )   ! Limit the application rate to minimum of 0.5*infiltration rate
                                                         ! and to the NRCS recommended rate, (m)
    IRMIRATE  = IRMIRATE * MIFAC

    if ( IRMIRATE >= IRAMTMI ) then
       IRMIRATE  = IRAMTMI
       IRAMTMI   = 0.0
    else
       IRAMTMI = IRAMTMI - IRMIRATE
    endif

    end associate

  end subroutine MicroIrrigation

end module IrrigationMicroMod
