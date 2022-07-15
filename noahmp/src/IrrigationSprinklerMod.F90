module IrrigationSprinklerMod

!!! Estimate irrigation water depth (m) based on sprinkler method 
!!! Reference: chapter 11 of NRCS, Part 623 National Engineering Handbook. 
!!! Irrigation water will be applied over the canopy, affecting  present soil moisture, 
!!! infiltration rate of the soil, and evaporative loss, which should be executed before canopy process.
 
  use Machine
  use CheckNanMod
  use NoahmpVarType
  use ConstantDefineMod
  use IrrigationInfilPhilipMod, only : IrrigationInfilPhilip

  implicit none

contains

  subroutine IrrigationSprinkler(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: SPRINKLER_IRRIGATION
! Original code: P. Valayamkunnath (NCAR) <prasanth@ucar.edu> (08/06/2020)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    logical                :: NaNInd       ! NaN value indicator: if NaN, return true
    real(kind=kind_noahmp) :: FSUR         ! surface infiltration rate (m/s)
    real(kind=kind_noahmp) :: TEMP_RATE    ! temporary irrigation rate (m/timestep)
    real(kind=kind_noahmp) :: WINDSPEED    ! total wind speed (m/s)
    real(kind=kind_noahmp) :: IRRLOSS      ! temporary var for irr loss [%]
    real(kind=kind_noahmp) :: ESAT1        ! satuarated air pressure (pa)

! --------------------------------------------------------------------
    associate(                                                        &
              MainTimeStep            => noahmp%config%domain%MainTimeStep      ,& ! in,    noahmp main time step (s)
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight ,& ! in,    air temperature [K] at reference height
              WindEastwardRefHeight   => noahmp%forcing%WindEastwardRefHeight   ,& ! in,    wind speed [m/s] in eastward direction at reference height
              WindNorthwardRefHeight  => noahmp%forcing%WindNorthwardRefHeight  ,& ! in,    wind speed [m/s] in northward direction at reference height
              EAIR            => noahmp%energy%state%EAIR            ,& ! in,     vapor pressure air (pa)
              SPRIR_RATE      => noahmp%water%param%SPRIR_RATE       ,& ! in,     sprinkler irrigation rate (mm/h)
              IrrigationFracSprinkler           => noahmp%water%state%IrrigationFracSprinkler            ,& ! in,     sprinkler irrigation fraction (0 to 1)
              SoilMoisture             => noahmp%water%state%SoilMoisture              ,& ! in,     total soil moisture [m3/m3]
              SoilLiqWater            => noahmp%water%state%SoilLiqWater             ,& ! in,     soil water content [m3/m3]
              FIRR            => noahmp%energy%flux%FIRR             ,& ! inout,  latent heating due to sprinkler evaporation [w/m2]
              IrrigationAmtSprinkler         => noahmp%water%state%IrrigationAmtSprinkler          ,& ! inout,  irrigation water amount [m] to be applied, Sprinkler
              EIRR            => noahmp%water%flux%EIRR              ,& ! inout,  evaporation of irrigation water to evaporation,sprink
              RAIN            => noahmp%water%flux%RAIN              ,& ! inout,  rainfall rate
              IRSIRATE        => noahmp%water%flux%IRSIRATE          ,& ! inout,  rate of irrigation by sprinkler [m/timestep]
              IREVPLOS        => noahmp%water%flux%IREVPLOS          ,& ! inout,  loss of irrigation water to evaporation,sprinkler [m/timestep]
              SoilIce            => noahmp%water%state%SoilIce              & ! out,    soil ice content [m3/m3]
             )
! ----------------------------------------------------------------------

    ! initialize
    SoilIce(:) = max(0.0, SoilMoisture(:)-SoilLiqWater(:))

    ! estimate infiltration rate based on Philips Eq.
    call IrrigationInfilPhilip(noahmp, MainTimeStep, FSUR)

    ! irrigation rate of sprinkler
    TEMP_RATE = SPRIR_RATE * (1.0/1000.0) * MainTimeStep / 3600.0   ! NRCS rate/time step - calibratable
    IRSIRATE  = min( FSUR*MainTimeStep, IrrigationAmtSprinkler, TEMP_RATE )        ! Limit the application rate to minimum of infiltration rate
                                                          ! and to the NRCS recommended rate, (m)
    ! evaporative loss from droplets: Based on Bavi et al., (2009). Evaporation 
    ! losses from sprinkler irrigation systems under various operating 
    ! conditions. Journal of Applied Sciences, 9(3), 597-600.
    WINDSPEED = sqrt( (WindEastwardRefHeight**2.0) + (WindNorthwardRefHeight**2.0) )   ! [m/s]
    ESAT1     = 610.8 * exp( (17.27*(TemperatureAirRefHeight-273.15)) / (237.3+(TemperatureAirRefHeight-273.15)) )  ! [Pa]

    if ( TemperatureAirRefHeight > 273.15 ) then ! Equation (3)
       IRRLOSS = 4.375 * ( exp(0.106*WINDSPEED) ) * ( ((ESAT1-EAIR)*0.01)**(-0.092) ) * ( (TemperatureAirRefHeight-273.15)**(-0.102) ) ! [%]
    else ! Equation (4)
       IRRLOSS = 4.337 * ( exp(0.077*WINDSPEED) ) * ( ((ESAT1-EAIR)*0.01)**(-0.098) ) ! [%]
    endif
    ! Old PGI Fortran compiler does not support ISNAN
    call CheckRealNaN(IRRLOSS, NaNInd)
    if ( NaNInd .eqv. .true. ) IRRLOSS = 4.0 ! In case if IRRLOSS is NaN
    if ( (IRRLOSS > 100.0) .or. (IRRLOSS < 0.0) ) IRRLOSS = 4.0 ! In case if IRRLOSS is out of range

    ! Sprinkler water (m) for sprinkler fraction 
    IRSIRATE  = IRSIRATE * IrrigationFracSprinkler
    if ( IRSIRATE >= IrrigationAmtSprinkler ) then
       IRSIRATE = IrrigationAmtSprinkler
       IrrigationAmtSprinkler  = 0.0
    else
       IrrigationAmtSprinkler = IrrigationAmtSprinkler - IRSIRATE
    endif

    IREVPLOS = IRSIRATE * IRRLOSS * (1.0/100.0)
    IRSIRATE = IRSIRATE - IREVPLOS

    ! include sprinkler water to total rain for canopy process later
    RAIN = RAIN + (IRSIRATE * 1000.0 / MainTimeStep) ![mm/s]

    ! cooling and humidification due to sprinkler evaporation, per m^2 calculation 
    FIRR = IREVPLOS * 1000.0 * ConstLatHeatVapor / MainTimeStep   ! heat used for evaporation (W/m2)
    EIRR = IREVPLOS * 1000.0 / MainTimeStep          ! sprinkler evaporation (mm/s)

    end associate

  end subroutine IrrigationSprinkler

end module IrrigationSprinklerMod
