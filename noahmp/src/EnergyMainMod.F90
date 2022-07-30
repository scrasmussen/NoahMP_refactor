module EnergyMainMod

!!! Main energy module including all energy relevant processes
!!! soil/snow thermal property -> radiation -> ground/vegtation heat flux -> snow/soil temperature solver -> soil/snow phase change
!
! --------------------------------------------------------------------------------------------------
! NoahMP uses different approaches to deal with subgrid features of radiation transfer and turbulent
! transfer. It uses 'tile' approach to compute turbulent fluxes, while it uses two-stream approx.
! to compute radiation transfer. Tile approach, assemblying vegetation canopies together,
! may expose too much ground surfaces (either covered by snow or grass) to solar radiation. The
! modified two-stream assumes vegetation covers fully the gridcell but with gaps between tree crowns.
! --------------------------------------------------------------------------------------------------
! turbulence transfer : 'tile' approach to compute energy fluxes in vegetated fraction and
!                         bare fraction separately and then sum them up weighted by fraction
!                     --------------------------------------
!                    / O  O  O  O  O  O  O  O  /          / 
!                   /  |  |  |  |  |  |  |  | /          /
!                  / O  O  O  O  O  O  O  O  /          /
!                 /  |  |  |tile1|  |  |  | /  tile2   /
!                / O  O  O  O  O  O  O  O  /  bare    /
!               /  |  |  | vegetated |  | /          /
!              / O  O  O  O  O  O  O  O  /          /
!             /  |  |  |  |  |  |  |  | /          /
!            --------------------------------------
! --------------------------------------------------------------------------------------------------
! radiation transfer : modified two-stream (Yang and Friedl, 2003, JGR; Niu ang Yang, 2004, JGR)
!                     --------------------------------------  two-stream treats leaves as
!                    /   O   O   O   O   O   O   O   O    /  cloud over the entire grid-cell,
!                   /    |   |   |   |   |   |   |   |   / while the modified two-stream 
!                  /   O   O   O   O   O   O   O   O    / aggregates cloudy leaves into  
!                 /    |   |   |   |   |   |   |   |   / tree crowns with gaps (as shown in
!                /   O   O   O   O   O   O   O   O    / the left figure). We assume these
!               /    |   |   |   |   |   |   |   |   / tree crowns are evenly distributed
!              /   O   O   O   O   O   O   O   O    / within the gridcell with 100% veg
!             /    |   |   |   |   |   |   |   |   / fraction, but with gaps. The 'tile'
!            -------------------------------------- approach overlaps too much shadows.
! --------------------------------------------------------------------------------------------------

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowCoverGroundNiu07Mod,        only : SnowCoverGroundNiu07
  use GroundRoughnessPropertyMod,     only : GroundRoughnessProperty
  use GroundThermalPropertyMod,       only : GroundThermalProperty
  use SurfaceAlbedoMod,               only : SurfaceAlbedo
  use SurfaceRadiationMod,            only : SurfaceRadiation
  use SurfaceEmissivityMod,           only : SurfaceEmissivity
  use SoilWaterTranspirationMod,      only : SoilWaterTranspiration
  use ResistanceGroundEvaporationMod, only : ResistanceGroundEvaporation
  use PsychrometricVariableMod,       only : PsychrometricVariable
  use SurfaceEnergyFluxVegetatedMod,  only : SurfaceEnergyFluxVegetated
  use SurfaceEnergyFluxBareGroundMod, only : SurfaceEnergyFluxBareGround
  use SoilSnowTemperatureMainMod,     only : SoilSnowTemperatureMain
  use SoilSnowWaterPhaseChangeMod,    only : SoilSnowWaterPhaseChange

  implicit none

contains

  subroutine EnergyMain(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: ENERGY
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    logical                          :: VEG          ! true if vegetated surface

! --------------------------------------------------------------------
    associate(                                                                    &
              PressureAirRefHeight   => noahmp%forcing%PressureAirRefHeight      ,& ! in,    air pressure [Pa] at reference height
              RadLwDownRefHeight     => noahmp%forcing%RadLwDownRefHeight        ,& ! in,    downward longwave radiation [W/m2] at reference height
              WindEastwardRefHeight  => noahmp%forcing%WindEastwardRefHeight     ,& ! in,    wind speed [m/s] in eastward direction at reference height
              WindNorthwardRefHeight => noahmp%forcing%WindNorthwardRefHeight    ,& ! in,    wind speed [m/s] in northward direction at reference height
              RadSwDownRefHeight     => noahmp%forcing%RadSwDownRefHeight        ,& ! in,    downward shortwave radiation [W/m2] at reference height
              OptSnowSoilTempTime    => noahmp%config%nmlist%OptSnowSoilTempTime ,& ! in,    options for snow/soil temperature time scheme
              FlagCropland           => noahmp%config%domain%FlagCropland        ,& ! in,    flag to identify croplands
              IrriFracThreshold        => noahmp%water%param%IrriFracThreshold         ,& ! in,    irrigation fraction parameter
              IrrigationFracGrid          => noahmp%water%state%IrrigationFracGrid           ,& ! in,    total input irrigation fraction
              LeafAreaIndEff            => noahmp%energy%state%LeafAreaIndEff            ,& ! in,    leaf area index, after burying by snow
              StemAreaIndEff            => noahmp%energy%state%StemAreaIndEff            ,& ! in,    stem area index, after burying by snow
              VegFrac            => noahmp%energy%state%VegFrac            ,& ! in,    greeness vegetation fraction (-)
              HeatLatentIrriEvap            => noahmp%energy%flux%HeatLatentIrriEvap             ,& ! in,    latent heating due to sprinkler evaporation [w/m2]
              HeatPrecipAdvCanopy            => noahmp%energy%flux%HeatPrecipAdvCanopy             ,& ! in,    precipitation advected heat - vegetation net (W/m2)
              HeatPrecipAdvVegGrd            => noahmp%energy%flux%HeatPrecipAdvVegGrd             ,& ! in,    precipitation advected heat - under canopy net (W/m2)
              HeatPrecipAdvBareGrd            => noahmp%energy%flux%HeatPrecipAdvBareGrd             ,& ! in,    precipitation advected heat - bare ground net (W/m2)
              TemperatureSfc              => noahmp%energy%state%TemperatureSfc              ,& ! inout, surface temperature (K)
              TemperatureGrd              => noahmp%energy%state%TemperatureGrd              ,& ! inout, ground temperature (K)
              TemperatureCanopy              => noahmp%energy%state%TemperatureCanopy              ,& ! inout, vegetation temperature (K)
              SpecHumiditySfcBare            => noahmp%energy%state%SpecHumiditySfcBare            ,& ! inout, specific humidity at bare surface
              SpecHumiditySfc              => noahmp%energy%state%SpecHumiditySfc              ,& ! inout, specific humidity at surface grid mean
              PressureVaporCanAir             => noahmp%energy%state%PressureVaporCanAir             ,& ! inout, canopy air vapor pressure (pa)
              CM              => noahmp%energy%state%CM              ,& ! inout, exchange coefficient (m/s) for momentum, surface, grid mean
              CH              => noahmp%energy%state%CH              ,& ! inout, exchange coefficient (m/s) for heat, surface, grid mean
              SnowDepth           => noahmp%water%state%SnowDepth            ,& ! inout, snow depth [m]
              RoughLenMomSfcToAtm           => noahmp%energy%state%RoughLenMomSfcToAtm           ,& ! out,   roughness length, momentum, surface, sent to coupled model
              WindStressEwTot            => noahmp%energy%state%WindStressEwTot            ,& ! out,   wind stress: east-west (n/m2) grid mean
              WindStressNsTot            => noahmp%energy%state%WindStressNsTot            ,& ! out,   wind stress: north-south (n/m2) grid mean
              TemperatureRadSfc            => noahmp%energy%state%TemperatureRadSfc            ,& ! out,   surface radiative temperature (K)
              TemperatureAir2m             => noahmp%energy%state%TemperatureAir2m             ,& ! out,   grid mean 2-m air temperature (K)
              ResistanceStomataSunlit           => noahmp%energy%state%ResistanceStomataSunlit           ,& ! out,   sunlit leaf stomatal resistance (s/m)
              ResistanceStomataShade           => noahmp%energy%state%ResistanceStomataShade           ,& ! out,   shaded leaf stomatal resistance (s/m)
              TemperatureAir2mVeg            => noahmp%energy%state%TemperatureAir2mVeg            ,& ! out,   2 m height air temperature (k), vegetated
              TemperatureAir2mBare            => noahmp%energy%state%TemperatureAir2mBare            ,& ! out,   2 m height air temperature (k) bare ground
              LeafAreaIndSunlit          => noahmp%energy%state%LeafAreaIndSunlit          ,& ! out,   sunlit leaf area index, one-sided (m2/m2)
              LeafAreaIndShade          => noahmp%energy%state%LeafAreaIndShade          ,& ! out,   shaded leaf area index, one-sided (m2/m2)
              EmissivitySfc          => noahmp%energy%state%EmissivitySfc          ,& ! out,   surface emissivity
              VegAreaIndEff             => noahmp%energy%state%VegAreaIndEff             ,& ! out,   one-sided leaf+stem area index (m2/m2)
              WindSpdRefHeight              => noahmp%energy%state%WindSpdRefHeight              ,& ! out,   wind speed (m/s) at reference height
              RoughLenMomSfc             => noahmp%energy%state%RoughLenMomSfc             ,& ! out,   roughness length, momentum, (m), surface
              RoughLenMomGrd            => noahmp%energy%state%RoughLenMomGrd            ,& ! out,   roughness length, momentum, ground (m)
              WindStressEwVeg           => noahmp%energy%state%WindStressEwVeg           ,& ! out,   wind stress: east-west (n/m2) above canopy
              WindStressNsVeg           => noahmp%energy%state%WindStressNsVeg           ,& ! out,   wind stress: north-south (n/m2) above canopy
              WindStressEwBare           => noahmp%energy%state%WindStressEwBare           ,& ! out,   wind stress: east-west (n/m2) bare ground
              WindStressNsBare           => noahmp%energy%state%WindStressNsBare           ,& ! out,   wind stress: north-south (n/m2) bare ground
              SpecHumidity2mVeg             => noahmp%energy%state%SpecHumidity2mVeg             ,& ! out,   water vapor mixing ratio at 2m vegetated
              SpecHumidity2mBare             => noahmp%energy%state%SpecHumidity2mBare             ,& ! out,   bare ground 2-m water vapor mixing ratio
              SpecHumidity2m             => noahmp%energy%state%SpecHumidity2m             ,& ! out,   grid mean 2-m water vapor mixing ratio
              TemperatureGrdVeg             => noahmp%energy%state%TemperatureGrdVeg             ,& ! out,   vegetated ground (below-canopy) temperature (K)
              TemperatureGrdBare             => noahmp%energy%state%TemperatureGrdBare             ,& ! out,   bare ground temperature (K)
              CMV             => noahmp%energy%state%CMV             ,& ! out,   drag coefficient for momentum, above ZeroPlaneDisp, vegetated
              CMB             => noahmp%energy%state%CMB             ,& ! out,   drag coefficient for momentum, above ZeroPlaneDisp, bare ground
              CHV             => noahmp%energy%state%CHV             ,& ! out,   drag coefficient for heat, above ZeroPlaneDisp, vegetated
              CHB             => noahmp%energy%state%CHB             ,& ! out,   drag coefficient for heat, above ZeroPlaneDisp, bare ground
              CHLEAF          => noahmp%energy%state%CHLEAF          ,& ! out,   leaf sensible heat exchange coeff (m/s), leaf to canopy air
              CHUC            => noahmp%energy%state%CHUC            ,& ! out,   under canopy sensible heat exchange coefficient (m/s)
              CHV2            => noahmp%energy%state%CHV2            ,& ! out,   2m sensible heat exchange coefficient (m/s) vegetated
              AlbedoSfc          => noahmp%energy%state%AlbedoSfc          ,& ! out,   total shortwave surface albedo
              RadSwReflTot             => noahmp%energy%flux%RadSwReflTot              ,& ! out,   total reflected solar radiation (w/m2)
              RadLwNetTot            => noahmp%energy%flux%RadLwNetTot             ,& ! out,   total net LW. rad (w/m2)   [+ to atm]
              HeatSensibleTot             => noahmp%energy%flux%HeatSensibleTot              ,& ! out,   total sensible heat (w/m2) [+ to atm]
              HeatLatentGrdTot            => noahmp%energy%flux%HeatLatentGrdTot             ,& ! out,   total ground latent heat (w/m2) [+ to atm]
              HeatLatentCanopy            => noahmp%energy%flux%HeatLatentCanopy             ,& ! out,   canopy latent heat flux (w/m2) [+ to atm]
              HeatLatentTransp            => noahmp%energy%flux%HeatLatentTransp             ,& ! out,   latent heat flux from transpiration (w/m2) [+ to atm]
              RadPhotoActAbsCan            => noahmp%energy%flux%RadPhotoActAbsCan             ,& ! out,   total photosyn. active energy (w/m2) absorbed by canopy
              RadPhotoActAbsSunlit          => noahmp%energy%flux%RadPhotoActAbsSunlit           ,& ! out,   average absorbed par for sunlit leaves (w/m2)
              RadPhotoActAbsShade          => noahmp%energy%flux%RadPhotoActAbsShade           ,& ! out,   average absorbed par for shaded leaves (w/m2)
              HeatGroundTot           => noahmp%energy%flux%HeatGroundTot            ,& ! out,   total ground heat flux (w/m2) [+ to soil/snow]
              HeatPrecipAdvTot             => noahmp%energy%flux%HeatPrecipAdvTot              ,& ! out,   precipitation advected heat - total (W/m2)
              RadLwEmitTot            => noahmp%energy%flux%RadLwEmitTot             ,& ! out,   emitted outgoing IR (w/m2)
              RadLwNetCanopy             => noahmp%energy%flux%RadLwNetCanopy              ,& ! out,   canopy net longwave radiation (w/m2) [+= to atm]
              RadLwNetVegGrd             => noahmp%energy%flux%RadLwNetVegGrd              ,& ! out,   ground net longwave radiation (w/m2) [+= to atm]
              RadLwNetBareGrd             => noahmp%energy%flux%RadLwNetBareGrd              ,& ! out,   net longwave rad (w/m2) bare ground [+ to atm]
              HeatSensibleCanopy             => noahmp%energy%flux%HeatSensibleCanopy              ,& ! out,   canopy sensible heat flux (w/m2)     [+= to atm]
              HeatSensibleVegGrd             => noahmp%energy%flux%HeatSensibleVegGrd              ,& ! out,   vegetated ground sensible heat flux (w/m2)     [+= to atm]
              HeatSensibleBareGrd             => noahmp%energy%flux%HeatSensibleBareGrd              ,& ! out,   sensible heat flux (w/m2) bare ground [+ to atm]
              HeatLatentVegGrd             => noahmp%energy%flux%HeatLatentVegGrd              ,& ! out,   ground evaporation heat flux (w/m2)  [+= to atm]
              HeatLatentBareGrd             => noahmp%energy%flux%HeatLatentBareGrd              ,& ! out,   latent heat flux (w/m2) bare ground [+ to atm]
              HeatLatentCanEvap             => noahmp%energy%flux%HeatLatentCanEvap              ,& ! out,   canopy evaporation heat flux (w/m2)  [+= to atm]
              HeatLatentCanTransp     => noahmp%energy%flux%HeatLatentCanTransp     ,& ! out,   canopy transpiration heat flux (w/m2)[+= to atm]
              HeatGroundVegGrd             => noahmp%energy%flux%HeatGroundVegGrd              ,& ! out,   vegetated ground heat (w/m2) [+ = to soil/snow]
              HeatGroundBareGrd             => noahmp%energy%flux%HeatGroundBareGrd              ,& ! out,   bare ground heat flux (w/m2) [+ to soil/snow]
              PhotosynTotal             => noahmp%biochem%flux%PhotosynTotal             ,& ! out,   total leaf photosynthesis (umol co2 /m2 /s)
              PhotosynLeafSunlit          => noahmp%biochem%flux%PhotosynLeafSunlit          ,& ! out,   sunlit leaf photosynthesis (umol co2 /m2 /s)
              PhotosynLeafShade          => noahmp%biochem%flux%PhotosynLeafShade           & ! out,   shaded leaf photosynthesis (umol co2 /m2 /s)
             )
! ----------------------------------------------------------------------

    ! initialization
    WindStressEwVeg   = 0.0
    WindStressNsVeg   = 0.0
    RadLwNetCanopy     = 0.0
    HeatSensibleCanopy     = 0.0
    RadLwNetVegGrd     = 0.0
    HeatSensibleVegGrd     = 0.0
    HeatLatentVegGrd     = 0.0
    HeatLatentCanEvap     = 0.0
    HeatLatentCanTransp  = 0.0
    HeatGroundVegGrd     = 0.0
    PhotosynLeafSunlit  = 0.0
    PhotosynLeafShade  = 0.0
    TemperatureAir2mVeg    = 0.0
    SpecHumidity2mVeg     = 0.0
    CHV     = 0.0
    CHLEAF  = 0.0
    CHUC    = 0.0
    CHV2    = 0.0
    HeatPrecipAdvTot     = 0.0

    ! wind speed at reference height: ur >= 1
    WindSpdRefHeight = max( sqrt(WindEastwardRefHeight**2.0 + WindNorthwardRefHeight**2.0), 1.0 )

    ! vegetated or non-vegetated
    VegAreaIndEff = LeafAreaIndEff + StemAreaIndEff
    VEG = .false.
    if ( VegAreaIndEff > 0.0 ) VEG = .true.

    ! ground snow cover fraction [Niu and Yang, 2007, JGR]
    call SnowCoverGroundNiu07(noahmp)

    ! ground and surface roughness length and reference height
    call GroundRoughnessProperty(noahmp, VEG)

    ! Thermal properties of soil, snow, lake, and frozen soil
    call GroundThermalProperty(noahmp)

    ! Surface shortwave albedo: ground and canopy radiative transfer
    call SurfaceAlbedo(noahmp)

    ! Surface shortwave radiation: absorbed & reflected by the ground and canopy
    call SurfaceRadiation(noahmp)

    ! longwave emissivity for vegetation, ground, total net surface
    call SurfaceEmissivity(noahmp)

    ! soil water transpiration factor controlling stomatal resistance and evapotranspiration
    call SoilWaterTranspiration(noahmp)

    ! soil surface resistance for ground evaporation/sublimation
    call ResistanceGroundEvaporation(noahmp)

    ! set psychrometric variable/constant
    call PsychrometricVariable(noahmp)

    ! temperatures and energy fluxes of canopy and below-canopy ground
    if ( (VEG .eqv. .true.) .and. (VegFrac > 0) ) then ! vegetated portion of the grid
       TemperatureGrdVeg = TemperatureGrd
       CMV = CM
       CHV = CH
       call SurfaceEnergyFluxVegetated(noahmp)
    endif

    ! temperatures and energy fluxes of bare ground
    TemperatureGrdBare = TemperatureGrd
    CMB = CM
    CHB = CH
    call SurfaceEnergyFluxBareGround(noahmp)

    ! compute grid mean quantities by weighting vegetated and bare portions
    ! Energy balance at vege canopy: RadSwAbsVeg          =(RadLwNetCanopy+HeatSensibleCanopy+HeatLatentCanEvap+HeatLatentCanTransp)     *VegFrac  at   VegFrac 
    ! Energy balance at vege ground: RadSwAbsGrd*    VegFrac =(RadLwNetVegGrd+HeatSensibleVegGrd+HeatLatentVegGrd+HeatGroundVegGrd)    *VegFrac  at   VegFrac
    ! Energy balance at bare ground: RadSwAbsGrd*(1.-VegFrac)=(RadLwNetBareGrd+HeatSensibleBareGrd+HeatLatentBareGrd+HeatGroundBareGrd)*(1.-VegFrac) at 1-VegFrac
    if ( (VEG .eqv. .true.) .and. (VegFrac > 0) ) then
       WindStressEwTot  = VegFrac * WindStressEwVeg + (1.0 - VegFrac) * WindStressEwBare
       WindStressNsTot  = VegFrac * WindStressNsVeg + (1.0 - VegFrac) * WindStressNsBare
       RadLwNetTot  = VegFrac * RadLwNetVegGrd   + (1.0 - VegFrac) * RadLwNetBareGrd   + RadLwNetCanopy
       HeatSensibleTot   = VegFrac * HeatSensibleVegGrd   + (1.0 - VegFrac) * HeatSensibleBareGrd   + HeatSensibleCanopy
       HeatLatentGrdTot  = VegFrac * HeatLatentVegGrd   + (1.0 - VegFrac) * HeatLatentBareGrd
       HeatGroundTot = VegFrac * HeatGroundVegGrd   + (1.0 - VegFrac) * HeatGroundBareGrd
       HeatLatentCanopy  = HeatLatentCanEvap
       HeatLatentTransp  = HeatLatentCanTransp
       HeatPrecipAdvTot   = VegFrac * HeatPrecipAdvVegGrd  + (1.0 - VegFrac) * HeatPrecipAdvBareGrd  + HeatPrecipAdvCanopy
       TemperatureGrd    = VegFrac * TemperatureGrdVeg   + (1.0 - VegFrac) * TemperatureGrdBare
       TemperatureAir2m   = VegFrac * TemperatureAir2mVeg  + (1.0 - VegFrac) * TemperatureAir2mBare
       TemperatureSfc    = VegFrac * TemperatureCanopy    + (1.0 - VegFrac) * TemperatureGrdBare
       CM    = VegFrac * CMV   + (1.0 - VegFrac) * CMB     ! better way to average?
       CH    = VegFrac * CHV   + (1.0 - VegFrac) * CHB
       SpecHumidity2m   = VegFrac * SpecHumidity2mVeg   + (1.0 - VegFrac) * SpecHumidity2mBare 
       RoughLenMomSfcToAtm = RoughLenMomSfc
       SpecHumiditySfc    = VegFrac * (PressureVaporCanAir*0.622/(PressureAirRefHeight-0.378*PressureVaporCanAir)) + (1.0 - VegFrac) * SpecHumiditySfcBare
    else
       WindStressEwTot  = WindStressEwBare
       WindStressNsTot  = WindStressNsBare
       RadLwNetTot  = RadLwNetBareGrd
       HeatSensibleTot   = HeatSensibleBareGrd
       HeatLatentGrdTot  = HeatLatentBareGrd
       HeatGroundTot = HeatGroundBareGrd
       TemperatureGrd    = TemperatureGrdBare
       TemperatureAir2m   = TemperatureAir2mBare
       HeatLatentCanopy  = 0.0
       HeatLatentTransp  = 0.0
       HeatPrecipAdvTot   = HeatPrecipAdvBareGrd
       TemperatureSfc    = TemperatureGrd
       CM    = CMB
       CH    = CHB
       SpecHumiditySfc    = SpecHumiditySfcBare
       SpecHumidity2m   = SpecHumidity2mBare
       ResistanceStomataSunlit = 0.0
       ResistanceStomataShade = 0.0
       TemperatureGrdVeg   = TemperatureGrdBare
       CHV   = CHB
       RoughLenMomSfcToAtm = RoughLenMomGrd
    endif

    ! emitted longwave radiation and physical check
    RadLwEmitTot = RadLwDownRefHeight + RadLwNetTot
    if ( RadLwEmitTot <= 0.0 ) then
       write(6,*) 'emitted longwave <0; skin T may be wrong due to inconsistent'
       write(6,*) 'input of VegFracGreen with LeafAreaIndex'
       write(6,*) 'VegFrac=',VegFrac,'VegAreaIndEff=',VegAreaIndEff,'TemperatureCanopy=',TemperatureCanopy,'TemperatureGrd=',TemperatureGrd
       write(6,*) 'RadLwDownRefHeight=',RadLwDownRefHeight,'RadLwNetTot=',RadLwNetTot,'SnowDepth=',SnowDepth
       !call wrf_error_fatal("STOP in Noah-MP")
    endif

    ! radiative temperature: subtract from the emitted IR the
    ! reflected portion of the incoming longwave radiation, so just
    ! considering the IR originating/emitted in the canopy/ground system.
    ! Old TemperatureRadSfc calculation not taking into account Emissivity:
    ! TemperatureRadSfc = (RadLwEmitTot/ConstStefanBoltzmann)**0.25
    TemperatureRadSfc = ( (RadLwEmitTot - (1.0 - EmissivitySfc)*RadLwDownRefHeight) / (EmissivitySfc * ConstStefanBoltzmann) )**0.25

    ! other photosynthesis related quantities for biochem process
    RadPhotoActAbsCan = RadPhotoActAbsSunlit * LeafAreaIndSunlit + RadPhotoActAbsShade * LeafAreaIndShade
    PhotosynTotal  = PhotosynLeafSunlit * LeafAreaIndSunlit + PhotosynLeafShade * LeafAreaIndShade

    ! compute snow and soil layer temperature
    call SoilSnowTemperatureMain(noahmp)

    ! adjusting suface temperature based on snow condition
    if ( OptSnowSoilTempTime == 2 ) then
       if ( (SnowDepth > 0.05) .and. (TemperatureGrd > ConstFreezePoint) ) then
          TemperatureGrdVeg = ConstFreezePoint
          TemperatureGrdBare = ConstFreezePoint
          if ( (VEG .eqv. .true.) .and. (VegFrac > 0) ) then
             TemperatureGrd = VegFrac * TemperatureGrdVeg + (1.0 - VegFrac) * TemperatureGrdBare
             TemperatureSfc = VegFrac * TemperatureCanopy  + (1.0 - VegFrac) * TemperatureGrdBare
          else
             TemperatureGrd = TemperatureGrdBare
             TemperatureSfc = TemperatureGrdBare
          endif
       endif
    endif

    ! Phase change and Energy released or consumed by snow & frozen soil
    call SoilSnowWaterPhaseChange(noahmp)

    ! update sensible heat flux due to sprinkler irrigation evaporation
    if ( (FlagCropland .eqv. .true.) .and. (IrrigationFracGrid >= IrriFracThreshold) ) &
       HeatSensibleTot = HeatSensibleTot - HeatLatentIrriEvap

    ! update total surface albedo
    if ( RadSwDownRefHeight > 0.0 ) then
       AlbedoSfc = RadSwReflTot / RadSwDownRefHeight
    else
       AlbedoSfc = -999.9
    endif

    end associate

  end subroutine EnergyMain

end module EnergyMainMod
