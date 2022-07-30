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
              TemperatureSfc              => noahmp%energy%state%TemperatureSfc              ,& ! inout, surface temperature (K)
              TemperatureGrd              => noahmp%energy%state%TemperatureGrd              ,& ! inout, ground temperature (K)
              SpecHumiditySfcBare            => noahmp%energy%state%SpecHumiditySfcBare            ,& ! inout, specific humidity at bare surface
              SpecHumiditySfc              => noahmp%energy%state%SpecHumiditySfc              ,& ! inout, specific humidity at surface grid mean
              CM              => noahmp%energy%state%CM              ,& ! inout, exchange coefficient (m/s) for momentum, surface, grid mean
              CH              => noahmp%energy%state%CH              ,& ! inout, exchange coefficient (m/s) for heat, surface, grid mean
              SnowDepth           => noahmp%water%state%SnowDepth            ,& ! inout, snow depth [m]
              RoughLenMomSfcToAtm           => noahmp%energy%state%RoughLenMomSfcToAtm           ,& ! out,   roughness length, momentum, surface, sent to coupled model
              WindStressEwTot            => noahmp%energy%state%WindStressEwTot            ,& ! out,   wind stress: east-west (n/m2) grid mean
              WindStressNsTot            => noahmp%energy%state%WindStressNsTot            ,& ! out,   wind stress: north-south (n/m2) grid mean
              TemperatureRadSfc            => noahmp%energy%state%TemperatureRadSfc            ,& ! out,   radiative temperature (K)
              TemperatureAir2m             => noahmp%energy%state%TemperatureAir2m             ,& ! out,   grid mean 2-m air temperature (K)
              TemperatureAir2mBare            => noahmp%energy%state%TemperatureAir2mBare            ,& ! out,   2 m height air temperature (k) bare ground
              EmissivitySfc          => noahmp%energy%state%EmissivitySfc          ,& ! out,   surface emissivity
              WindSpdRefHeight              => noahmp%energy%state%WindSpdRefHeight              ,& ! out,   wind speed (m/s) at reference height
              RoughLenMomGrd            => noahmp%energy%state%RoughLenMomGrd            ,& ! out,   roughness length, momentum, ground (m)
              WindStressEwBare           => noahmp%energy%state%WindStressEwBare           ,& ! out,   wind stress: east-west (n/m2) bare ground
              WindStressNsBare           => noahmp%energy%state%WindStressNsBare           ,& ! out,   wind stress: north-south (n/m2) bare ground
              SpecHumidity2mBare             => noahmp%energy%state%SpecHumidity2mBare             ,& ! out,   bare ground 2-m water vapor mixing ratio
              SpecHumidity2m             => noahmp%energy%state%SpecHumidity2m             ,& ! out,   grid mean 2-m water vapor mixing ratio
              TemperatureGrdBare             => noahmp%energy%state%TemperatureGrdBare             ,& ! out,   bare ground temperature (K)
              CMB             => noahmp%energy%state%CMB             ,& ! out,   drag coefficient for momentum, above ZeroPlaneDisp, bare ground
              CHB             => noahmp%energy%state%CHB             ,& ! out,   drag coefficient for heat, above ZeroPlaneDisp, bare ground
              AlbedoSfc          => noahmp%energy%state%AlbedoSfc          ,& ! out,   total shortwave surface albedo
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
    WindSpdRefHeight = max( sqrt(WindEastwardRefHeight**2.0 + WindNorthwardRefHeight**2.0), 1.0 )

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
    TemperatureGrdBare = TemperatureGrd
    CMB = CM
    CHB = CH
    call SurfaceEnergyFluxGlacier(noahmp)

    ! assign glacier bare ground quantity to grid-level quantity
    ! Energy balance at glacier (bare) ground: RadSwAbsGrd+HeatPrecipAdvBareGrd=RadLwNetBareGrd+HeatSensibleBareGrd+HeatLatentBareGrd+HeatGroundBareGrd
    WindStressEwTot  = WindStressEwBare
    WindStressNsTot  = WindStressNsBare
    RadLwNetTot  = RadLwNetBareGrd
    HeatSensibleTot   = HeatSensibleBareGrd
    HeatLatentGrdTot  = HeatLatentBareGrd
    HeatGroundTot = HeatGroundBareGrd
    TemperatureGrd    = TemperatureGrdBare
    TemperatureAir2m   = TemperatureAir2mBare
    HeatPrecipAdvTot   = HeatPrecipAdvBareGrd
    TemperatureSfc    = TemperatureGrd
    CM    = CMB
    CH    = CHB
    SpecHumiditySfc    = SpecHumiditySfcBare
    SpecHumidity2m   = SpecHumidity2mBare
    RoughLenMomSfcToAtm = RoughLenMomGrd

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
    ! Old TemperatureRadSfc calculation not taking into account Emissivity:
    ! TemperatureRadSfc = (RadLwEmitTot/ConstStefanBoltzmann)**0.25
    TemperatureRadSfc = ( (RadLwEmitTot - (1.0 - EmissivitySfc)*RadLwDownRefHeight) / (EmissivitySfc * ConstStefanBoltzmann) ) ** 0.25

    ! compute snow and glacier ice temperature
    call GlacierTemperatureMain(noahmp)

    ! adjusting suface temperature based on snow condition
    if ( OptSnowSoilTempTime == 2 ) then
       if ( (SnowDepth > 0.05) .and. (TemperatureGrd > ConstFreezePoint) ) then
          TemperatureGrdBare = ConstFreezePoint
          TemperatureGrd = TemperatureGrdBare
          TemperatureSfc = TemperatureGrdBare
       endif
    endif

    ! Phase change and Energy released or consumed by snow & glacier ice
    call GlacierPhaseChange(noahmp)

    ! update total surface albedo
    if ( RadSwDownRefHeight > 0.0 ) then
       AlbedoSfc = RadSwReflTot / RadSwDownRefHeight
    else
       AlbedoSfc = -999.9
    endif

    end associate

  end subroutine EnergyMainGlacier

end module EnergyMainGlacierMod
