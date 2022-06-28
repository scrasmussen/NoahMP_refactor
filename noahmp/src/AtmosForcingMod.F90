module AtmosForcingMod

!!! Process input atmospheric forcing variables

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine ProcessAtmosForcing(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: ATM
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local varibles
    real(kind=kind_noahmp)             :: PAIR                   ! atm bottom level pressure (pa)
    real(kind=kind_noahmp)             :: PRCP_FROZEN            ! total frozen precipitation [mm/s] ! MB/AN : v3.7
    real(kind=kind_noahmp)             :: dir_frac               ! direct solar radiation fraction
    real(kind=kind_noahmp)             :: vis_frac               ! visible band solar radiation fraction
    real(kind=kind_noahmp)             :: ESATAIR                ! saturated vapor pressure of air
    real(kind=kind_noahmp)             :: LATHEA                 ! latent heat of vapor/sublimation
    real(kind=kind_noahmp)             :: GAMMA_b                ! (cp*p)/(eps*L)
    real(kind=kind_noahmp)             :: TDC                    ! air temperature [C]
    real(kind=kind_noahmp)             :: TWET                   ! wetbulb temperature
    integer                            :: ITER                   ! loop index
    integer, parameter                 :: NITER = 10             ! iterations for Twet calculation

! ------------------------------------------------------------------------
    associate(                                                           &
              COSZ                  => noahmp%config%domain%COSZ        ,& ! in,   cosine solar zenith angle [0-1]
              OPT_SNF               => noahmp%config%nmlist%OPT_SNF     ,& ! in,   rain-snow partition physics option
              PressureAirRefHeight    => noahmp%forcing%PressureAirRefHeight,& ! in,   air pressure [Pa] at reference height
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight,& ! in,   air temperature [K] at reference height
              SpecHumidityRefHeight   => noahmp%forcing%SpecHumidityRefHeight, & ! in,  specific humidity (kg/kg) forcing at reference height
              PrecipConvRefHeight     => noahmp%forcing%PrecipConvRefHeight,& ! in,   convective precipitation rate [mm/s] at reference height
              PrecipNonConvRefHeight  => noahmp%forcing%PrecipNonConvRefHeight,& ! in,   non-convective precipitation rate [mm/s] at reference height
              PrecipShConvRefHeight   => noahmp%forcing%PrecipShConvRefHeight ,& ! in,   shallow convective precipitation rate [mm/s] at reference height
              PrecipSnowRefHeight     => noahmp%forcing%PrecipSnowRefHeight,& ! in,   snowfall rate [mm/s] at reference height
              PrecipGraupelRefHeight  => noahmp%forcing%PrecipGraupelRefHeight,& ! in,   graupel rate [mm/s] at reference height
              PrecipHailRefHeight     => noahmp%forcing%PrecipHailRefHeight,& ! in,   hail rate [mm/s] at reference height
              RadSWDownRefHeight => noahmp%forcing%RadSWDownRefHeight ,& ! in,   downward shortwave radiation (W/m2) at reference height
              THAIR                 => noahmp%energy%state%THAIR        ,& ! out,  surface potential temperature (k)
              EAIR                  => noahmp%energy%state%EAIR         ,& ! out,  vapor pressure air (pa)
              RHOAIR                => noahmp%energy%state%RHOAIR       ,& ! out,  density air (kg/m3)
              SOLAD                 => noahmp%energy%flux%SOLAD         ,& ! out,  incoming direct solar radiation (w/m2)
              SOLAI                 => noahmp%energy%flux%SOLAI         ,& ! out,  incoming diffuse solar radiation (w/m2)
              RAIN                  => noahmp%water%flux%RAIN           ,& ! out,  rainfall (mm/s)
              SNOW                  => noahmp%water%flux%SNOW           ,& ! out,  liquid equivalent snowfall (mm/s)
              PRCP                  => noahmp%water%flux%PRCP           ,& ! out,  total precipitation [mm/s]
              QPRECC                => noahmp%water%flux%QPRECC         ,& ! out,  convective precipitation (mm/s)
              QPRECL                => noahmp%water%flux%QPRECL         ,& ! out,  large-scale precipitation (mm/s)
              FP                    => noahmp%water%state%FP            ,& ! out,  fraction of area receiving precipitation
              FPICE                 => noahmp%water%state%FPICE         ,& ! out,  snowfall fraction
              BDFALL                => noahmp%water%state%BDFALL         & ! out,  !bulk density of snowfall (kg/m3)
             )
! ------------------------------------------------------------------------

    ! surface air quantities
    PAIR   = PressureAirRefHeight  ! to be consistent with resistanceChen97 calculation (based on ground reference level)
    THAIR  = TemperatureAirRefHeight * (PressureAirRefHeight/PAIR)**(ConstGasDryAir/ConstHeatCapacAir) 
    EAIR   = SpecHumidityRefHeight * PressureAirRefHeight / (0.622+0.378*SpecHumidityRefHeight)
    RHOAIR = (PressureAirRefHeight - 0.378*EAIR) / (ConstGasDryAir * TemperatureAirRefHeight)

    ! downward solar radiation
    dir_frac = 0.7
    vis_frac = 0.5
    if ( COSZ <= 0.0 ) RadSWDownRefHeight = 0.0     ! filter by solar zenith angle
    SOLAD(1) = RadSWDownRefHeight * dir_frac       * vis_frac        ! direct  vis
    SOLAD(2) = RadSWDownRefHeight * dir_frac       * (1.0-vis_frac)  ! direct  nir
    SOLAI(1) = RadSWDownRefHeight * (1.0-dir_frac) * vis_frac        ! diffuse vis
    SOLAI(2) = RadSWDownRefHeight * (1.0-dir_frac) * (1.0-vis_frac)  ! diffuse nir

    ! precipitation
    PRCP = PrecipConvRefHeight + PrecipNonConvRefHeight + PrecipShConvRefHeight
    if ( OPT_SNF == 4 ) then
       QPRECC = PrecipConvRefHeight + PrecipShConvRefHeight
       QPRECL = PrecipNonConvRefHeight
    else
       QPRECC = 0.10 * PRCP    ! should be from the atmospheric model
       QPRECL = 0.90 * PRCP    ! should be from the atmospheric model
    endif

    ! fractional area that receives precipitation (see, Niu et al. 2005)
    FP = 0.0
    if ( (QPRECC+QPRECL) > 0.0 ) then
       FP = (QPRECC + QPRECL) / (10.0*QPRECC + QPRECL)
    endif

    ! partition precipitation into rain and snow. Moved from CANWAT MB/AN: v3.7

    ! Jordan (1991)
    if ( OPT_SNF == 1 ) then
       if ( TemperatureAirRefHeight > (ConstFreezePoint+2.5) ) then
          FPICE = 0.0
       else
          if ( TemperatureAirRefHeight <= (ConstFreezePoint+0.5) ) then
             FPICE = 1.0
          elseif ( TemperatureAirRefHeight <= (ConstFreezePoint+2.0) ) then
             FPICE = 1.0 - (-54.632 + 0.2*TemperatureAirRefHeight)
          else
             FPICE = 0.6
          endif
       endif
    endif

    ! BATS scheme
    if ( OPT_SNF == 2 ) then
       if ( TemperatureAirRefHeight >= (ConstFreezePoint+2.2) ) then
          FPICE = 0.0
       else
          FPICE = 1.0
       endif
    endif

    ! Simple temperature scheme
    if ( OPT_SNF == 3 ) then
       if ( TemperatureAirRefHeight >= ConstFreezePoint ) then
          FPICE = 0.0
       else
          FPICE = 1.0
       endif
    endif

    ! Use WRF microphysics output
    ! Hedstrom NR and JW Pomeroy (1998), Hydrol. Processes, 12, 1611-1625
    BDFALL = min( 120.0, 67.92 + 51.25*exp((TemperatureAirRefHeight-ConstFreezePoint)/2.59) )   !fresh snow density !MB/AN: change to MIN  
    if ( OPT_SNF == 4 ) then
       PRCP_FROZEN = PrecipSnowRefHeight + PrecipGraupelRefHeight + PrecipHailRefHeight
       if ( (PrecipNonConvRefHeight > 0.0) .and. (PRCP_FROZEN > 0.0) ) then
          FPICE  = min( 1.0, PRCP_FROZEN/PrecipNonConvRefHeight )
          FPICE  = max( 0.0, FPICE )
          BDFALL = BDFALL * (PrecipSnowRefHeight/PRCP_FROZEN) + ConstDensityGraupel * (PrecipGraupelRefHeight/PRCP_FROZEN) + &
                   ConstDensityHail * (PrecipHailRefHeight/PRCP_FROZEN)
       else
          FPICE = 0.0
       endif
    endif

    ! wet-bulb scheme (Wang et al., 2019 GRL), C.He, 12/18/2020
    if ( OPT_SNF == 5 ) then
       TDC = min( 50.0, max(-50.0,(TemperatureAirRefHeight - ConstFreezePoint)) )         ! Kelvin to degree Celsius with limit -50 to +50
       if ( TemperatureAirRefHeight > ConstFreezePoint ) then
          LATHEA = ConstLatHeatVapor
       else
          LATHEA = ConstLatHeatSublim
       endif
       GAMMA_b = ConstHeatCapacAir * PressureAirRefHeight / (0.622 * LATHEA)
       TWET    = TDC - 5.0                                     ! first guess wetbulb temperature
       do ITER = 1, NITER
          ESATAIR = 610.8 * exp( (17.27*TWET) / (237.3+TWET) )
          TWET    = TWET - (ESATAIR - EAIR) / GAMMA_b          ! Wang et al., 2019 GRL Eq.2
       enddo
       FPICE = 1.0 / (1.0 + 6.99e-5 * exp(2.0*(TWET+3.97)))    ! Wang et al., 2019 GRL Eq. 1
    endif

    ! rain-snow partitioning
    RAIN = PRCP * (1.0 - FPICE)
    SNOW = PRCP * FPICE

    end associate

  end subroutine ProcessAtmosForcing

end module AtmosForcingMod
