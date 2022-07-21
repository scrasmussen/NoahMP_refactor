module EnergyMainGlacierMod

!!! Main energy module for glacier points including all energy relevant processes
!!! snow thermal property -> radiation -> ground heat flux -> snow temperature solver -> snow/ice phase change

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowCoverGlacierMod,                   only : SnowCoverGlacier
  use GroundRoughnessPropertyGlacierMod,     only : GroundRoughnessPropertyGlacier
  use GroundThermalPropertyGlacierMod,       only : GroundThermalPropertyGlacier
  use SurfaceAlbedoGlacierMod,               only : SurfaceAlbedoGlacier
  use SurfaceRadiationGlacierMod,            only : SurfaceRadiationGlacier
  use SurfaceEmissivityGlacierMod,           only : SurfaceEmissivityGlacier
  use ResistanceGroundEvaporationGlacierMod, only : ResistanceGroundEvaporationGlacier
  use PsychrometricVariableGlacierMod,       only : PsychrometricVariableGlacier
  use SurfaceEnergyFluxGlacierMod,           only : SurfaceEnergyFluxGlacier
  use GlacierTemperatureMainMod,             only : GlacierTemperatureMain
  use GlacierPhaseChangeMod,                 only : GlacierPhaseChange

  implicit none

contains

  subroutine EnergyMainGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: ENERGY_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable

! --------------------------------------------------------------------
    associate(                                                        &
              RadLwDownRefHeight => noahmp%forcing%RadLwDownRefHeight,& ! in,    downward longwave radiation [W/m2] at reference height
              RadSwDownRefHeight     => noahmp%forcing%RadSwDownRefHeight,& ! in,    downward shortwave radiation [W/m2] at reference height
              WindEastwardRefHeight  => noahmp%forcing%WindEastwardRefHeight,& ! in,    wind speed [m/s] in eastward direction at reference height
              WindNorthwardRefHeight => noahmp%forcing%WindNorthwardRefHeight,& ! in,    wind speed [m/s] in northward direction at reference height
              OptSnowSoilTempTime => noahmp%config%nmlist%OptSnowSoilTempTime ,& ! in,    options for snow/soil temperature time scheme
              HeatPrecipAdvBareGrd            => noahmp%energy%flux%HeatPrecipAdvBareGrd             ,& ! in,    precipitation advected heat - bare ground net (W/m2)
              TS              => noahmp%energy%state%TS              ,& ! inout, surface temperature (K)
              TG              => noahmp%energy%state%TG              ,& ! inout, ground temperature (K)
              QSFC            => noahmp%energy%state%QSFC            ,& ! inout, water vapor mixing ratio bare ground
              Q1              => noahmp%energy%state%Q1              ,& ! inout, surface layer water vapor mixing ratio
              CM              => noahmp%energy%state%CM              ,& ! inout, exchange coefficient (m/s) for momentum, surface, grid mean
              CH              => noahmp%energy%state%CH              ,& ! inout, exchange coefficient (m/s) for heat, surface, grid mean
              SnowDepth           => noahmp%water%state%SnowDepth            ,& ! inout, snow depth [m]
              Z0WRF           => noahmp%energy%state%Z0WRF           ,& ! out,   roughness length, momentum, surface, sent to coupled model
              TAUX            => noahmp%energy%state%TAUX            ,& ! out,   wind stress: east-west (n/m2) grid mean
              TAUY            => noahmp%energy%state%TAUY            ,& ! out,   wind stress: north-south (n/m2) grid mean
              TRAD            => noahmp%energy%state%TRAD            ,& ! out,   radiative temperature (K)
              T2M             => noahmp%energy%state%T2M             ,& ! out,   grid mean 2-m air temperature (K)
              T2MB            => noahmp%energy%state%T2MB            ,& ! out,   2 m height air temperature (k) bare ground
              EMISSI          => noahmp%energy%state%EMISSI          ,& ! out,   surface emissivity
              UR              => noahmp%energy%state%UR              ,& ! out,   wind speed (m/s) at reference height
              Z0M             => noahmp%energy%state%Z0M             ,& ! out,   roughness length, momentum, (m), surface
              Z0MG            => noahmp%energy%state%Z0MG            ,& ! out,   roughness length, momentum, ground (m)
              TAUXB           => noahmp%energy%state%TAUXB           ,& ! out,   wind stress: east-west (n/m2) bare ground
              TAUYB           => noahmp%energy%state%TAUYB           ,& ! out,   wind stress: north-south (n/m2) bare ground
              Q2B             => noahmp%energy%state%Q2B             ,& ! out,   bare ground 2-m water vapor mixing ratio
              Q2E             => noahmp%energy%state%Q2E             ,& ! out,   grid mean 2-m water vapor mixing ratio
              TGB             => noahmp%energy%state%TGB             ,& ! out,   bare ground temperature (K)
              CMB             => noahmp%energy%state%CMB             ,& ! out,   drag coefficient for momentum, above ZPD, bare ground
              CHB             => noahmp%energy%state%CHB             ,& ! out,   drag coefficient for heat, above ZPD, bare ground
              ALBEDO          => noahmp%energy%state%ALBEDO          ,& ! out,   total shortwave surface albedo
              RadSwReflTot             => noahmp%energy%flux%RadSwReflTot              ,& ! out,   total reflected solar radiation (w/m2)
              RadLwNetTot            => noahmp%energy%flux%RadLwNetTot             ,& ! out,   total net LW. rad (w/m2)   [+ to atm]
              HeatSensibleTot             => noahmp%energy%flux%HeatSensibleTot              ,& ! out,   total sensible heat (w/m2) [+ to atm]
              HeatLatentGrdTot            => noahmp%energy%flux%HeatLatentGrdTot             ,& ! out,   total ground latent heat (w/m2) [+ to atm]
              HeatGroundTot           => noahmp%energy%flux%HeatGroundTot            ,& ! out,   total ground heat flux (w/m2) [+ to soil/snow]
              HeatPrecipAdvTot             => noahmp%energy%flux%HeatPrecipAdvTot              ,& ! out,   precipitation advected heat - total (W/m2)
              RadLwEmitTot            => noahmp%energy%flux%RadLwEmitTot             ,& ! out,   emitted outgoing IR (w/m2)
              RadLwNetBareGrd             => noahmp%energy%flux%RadLwNetBareGrd              ,& ! out,   net longwave rad (w/m2) bare ground [+ to atm]
              HeatSensibleBareGrd             => noahmp%energy%flux%HeatSensibleBareGrd              ,& ! out,   sensible heat flux (w/m2) bare ground [+ to atm]
              HeatLatentBareGrd             => noahmp%energy%flux%HeatLatentBareGrd              ,& ! out,   latent heat flux (w/m2) bare ground [+ to atm]
              HeatGroundBareGrd             => noahmp%energy%flux%HeatGroundBareGrd               & ! out,   bare ground heat flux (w/m2) [+ to soil/snow]
             )
! ----------------------------------------------------------------------

    ! wind speed at reference height: ur >= 1
    UR = max( sqrt(WindEastwardRefHeight**2.0 + WindNorthwardRefHeight**2.0), 1.0 )

    ! glaicer snow cover fraction
    call SnowCoverGlacier(noahmp)

    ! ground and surface roughness length and reference height
    call GroundRoughnessPropertyGlacier(noahmp)

    ! Thermal properties of snow and glacier ice
    call GroundThermalPropertyGlacier(noahmp)

    ! Glacier surface shortwave abeldo
    call SurfaceAlbedoGlacier(noahmp)

    ! Glacier surface shortwave radiation
    call SurfaceRadiationGlacier(noahmp)

    ! longwave emissivity for glacier surface
    call SurfaceEmissivityGlacier(noahmp)

    ! glacier surface resistance for ground evaporation/sublimation
    call ResistanceGroundEvaporationGlacier(noahmp)

    ! set psychrometric variable/constant
    call PsychrometricVariableGlacier(noahmp)

    ! temperatures and energy fluxes of glacier ground
    TGB = TG
    CMB = CM
    CHB = CH
    call SurfaceEnergyFluxGlacier(noahmp)

    ! assign glacier bare ground quantity to grid-level quantity
    ! Energy balance at glacier (bare) ground: RadSwAbsGrd+HeatPrecipAdvBareGrd=RadLwNetBareGrd+HeatSensibleBareGrd+HeatLatentBareGrd+HeatGroundBareGrd
    TAUX  = TAUXB
    TAUY  = TAUYB
    RadLwNetTot  = RadLwNetBareGrd
    HeatSensibleTot   = HeatSensibleBareGrd
    HeatLatentGrdTot  = HeatLatentBareGrd
    HeatGroundTot = HeatGroundBareGrd
    TG    = TGB
    T2M   = T2MB
    HeatPrecipAdvTot   = HeatPrecipAdvBareGrd
    TS    = TG
    CM    = CMB
    CH    = CHB
    Q1    = QSFC
    Q2E   = Q2B
    Z0WRF = Z0MG

    ! emitted longwave radiation and physical check
    RadLwEmitTot = RadLwDownRefHeight + RadLwNetTot
    if ( RadLwEmitTot <= 0.0 ) then
       write(*,*) 'emitted longwave <0; skin T may be wrong due to inconsistent'
       write(*,*) 'RadLwDownRefHeight=',RadLwDownRefHeight,'RadLwNetTot=',RadLwNetTot,'SnowDepth=',SnowDepth
       stop 'error'
       !call wrf_error_fatal("STOP in Noah-MP")
    endif

    ! radiative temperature: subtract from the emitted IR the
    ! reflected portion of the incoming longwave radiation, so just
    ! considering the IR originating/emitted in the ground system.
    ! Old TRAD calculation not taking into account Emissivity:
    ! TRAD = (RadLwEmitTot/ConstStefanBoltzmann)**0.25
    TRAD = ( (RadLwEmitTot - (1.0 - EMISSI)*RadLwDownRefHeight) / (EMISSI * ConstStefanBoltzmann) ) ** 0.25

    ! compute snow and glacier ice temperature
    call GlacierTemperatureMain(noahmp)

    ! adjusting suface temperature based on snow condition
    if ( OptSnowSoilTempTime == 2 ) then
       if ( (SnowDepth > 0.05) .and. (TG > ConstFreezePoint) ) then
          TGB = ConstFreezePoint
          TG = TGB
          TS = TGB
       endif
    endif

    ! Phase change and Energy released or consumed by snow & glacier ice
    call GlacierPhaseChange(noahmp)

    ! update total surface albedo
    if ( RadSwDownRefHeight > 0.0 ) then
       ALBEDO = RadSwReflTot / RadSwDownRefHeight
    else
       ALBEDO = -999.9
    endif

    end associate

  end subroutine EnergyMainGlacier

end module EnergyMainGlacierMod
