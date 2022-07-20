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
              RadLWDownRefHeight     => noahmp%forcing%RadLWDownRefHeight        ,& ! in,    downward longwave radiation [W/m2] at reference height
              WindEastwardRefHeight  => noahmp%forcing%WindEastwardRefHeight     ,& ! in,    wind speed [m/s] in eastward direction at reference height
              WindNorthwardRefHeight => noahmp%forcing%WindNorthwardRefHeight    ,& ! in,    wind speed [m/s] in northward direction at reference height
              RadSWDownRefHeight     => noahmp%forcing%RadSWDownRefHeight        ,& ! in,    downward shortwave radiation [W/m2] at reference height
              OptSnowSoilTempTime    => noahmp%config%nmlist%OptSnowSoilTempTime ,& ! in,    options for snow/soil temperature time scheme
              FlagCropland           => noahmp%config%domain%FlagCropland        ,& ! in,    flag to identify croplands
              IrriFracThreshold        => noahmp%water%param%IrriFracThreshold         ,& ! in,    irrigation fraction parameter
              IrrigationFracGrid          => noahmp%water%state%IrrigationFracGrid           ,& ! in,    total input irrigation fraction
              ELAI            => noahmp%energy%state%ELAI            ,& ! in,    leaf area index, after burying by snow
              ESAI            => noahmp%energy%state%ESAI            ,& ! in,    stem area index, after burying by snow
              FVEG            => noahmp%energy%state%FVEG            ,& ! in,    greeness vegetation fraction (-)
              FIRR            => noahmp%energy%flux%FIRR             ,& ! in,    latent heating due to sprinkler evaporation [w/m2]
              PAHV            => noahmp%energy%flux%PAHV             ,& ! in,    precipitation advected heat - vegetation net (W/m2)
              PAHG            => noahmp%energy%flux%PAHG             ,& ! in,    precipitation advected heat - under canopy net (W/m2)
              PAHB            => noahmp%energy%flux%PAHB             ,& ! in,    precipitation advected heat - bare ground net (W/m2)
              TS              => noahmp%energy%state%TS              ,& ! inout, surface temperature (K)
              TG              => noahmp%energy%state%TG              ,& ! inout, ground temperature (K)
              TV              => noahmp%energy%state%TV              ,& ! inout, vegetation temperature (K)
              QSFC            => noahmp%energy%state%QSFC            ,& ! inout, water vapor mixing ratio bare ground
              Q1              => noahmp%energy%state%Q1              ,& ! inout, surface layer water vapor mixing ratio
              EAH             => noahmp%energy%state%EAH             ,& ! inout, canopy air vapor pressure (pa)
              CM              => noahmp%energy%state%CM              ,& ! inout, exchange coefficient (m/s) for momentum, surface, grid mean
              CH              => noahmp%energy%state%CH              ,& ! inout, exchange coefficient (m/s) for heat, surface, grid mean
              SnowDepth           => noahmp%water%state%SnowDepth            ,& ! inout, snow depth [m]
              Z0WRF           => noahmp%energy%state%Z0WRF           ,& ! out,   roughness length, momentum, surface, sent to coupled model
              TAUX            => noahmp%energy%state%TAUX            ,& ! out,   wind stress: east-west (n/m2) grid mean
              TAUY            => noahmp%energy%state%TAUY            ,& ! out,   wind stress: north-south (n/m2) grid mean
              TRAD            => noahmp%energy%state%TRAD            ,& ! out,   radiative temperature (K)
              T2M             => noahmp%energy%state%T2M             ,& ! out,   grid mean 2-m air temperature (K)
              RSSUN           => noahmp%energy%state%RSSUN           ,& ! out,   sunlit leaf stomatal resistance (s/m)
              RSSHA           => noahmp%energy%state%RSSHA           ,& ! out,   shaded leaf stomatal resistance (s/m)
              T2MV            => noahmp%energy%state%T2MV            ,& ! out,   2 m height air temperature (k), vegetated
              T2MB            => noahmp%energy%state%T2MB            ,& ! out,   2 m height air temperature (k) bare ground
              LAISUN          => noahmp%energy%state%LAISUN          ,& ! out,   sunlit leaf area index, one-sided (m2/m2)
              LAISHA          => noahmp%energy%state%LAISHA          ,& ! out,   shaded leaf area index, one-sided (m2/m2)
              EMISSI          => noahmp%energy%state%EMISSI          ,& ! out,   surface emissivity
              VAI             => noahmp%energy%state%VAI             ,& ! out,   one-sided leaf+stem area index (m2/m2)
              UR              => noahmp%energy%state%UR              ,& ! out,   wind speed (m/s) at reference height
              Z0M             => noahmp%energy%state%Z0M             ,& ! out,   roughness length, momentum, (m), surface
              Z0MG            => noahmp%energy%state%Z0MG            ,& ! out,   roughness length, momentum, ground (m)
              TAUXV           => noahmp%energy%state%TAUXV           ,& ! out,   wind stress: east-west (n/m2) above canopy
              TAUYV           => noahmp%energy%state%TAUYV           ,& ! out,   wind stress: north-south (n/m2) above canopy
              TAUXB           => noahmp%energy%state%TAUXB           ,& ! out,   wind stress: east-west (n/m2) bare ground
              TAUYB           => noahmp%energy%state%TAUYB           ,& ! out,   wind stress: north-south (n/m2) bare ground
              Q2V             => noahmp%energy%state%Q2V             ,& ! out,   water vapor mixing ratio at 2m vegetated
              Q2B             => noahmp%energy%state%Q2B             ,& ! out,   bare ground 2-m water vapor mixing ratio
              Q2E             => noahmp%energy%state%Q2E             ,& ! out,   grid mean 2-m water vapor mixing ratio
              TGV             => noahmp%energy%state%TGV             ,& ! out,   vegetated ground (below-canopy) temperature (K)
              TGB             => noahmp%energy%state%TGB             ,& ! out,   bare ground temperature (K)
              CMV             => noahmp%energy%state%CMV             ,& ! out,   drag coefficient for momentum, above ZPD, vegetated
              CMB             => noahmp%energy%state%CMB             ,& ! out,   drag coefficient for momentum, above ZPD, bare ground
              CHV             => noahmp%energy%state%CHV             ,& ! out,   drag coefficient for heat, above ZPD, vegetated
              CHB             => noahmp%energy%state%CHB             ,& ! out,   drag coefficient for heat, above ZPD, bare ground
              CHLEAF          => noahmp%energy%state%CHLEAF          ,& ! out,   leaf sensible heat exchange coeff (m/s), leaf to canopy air
              CHUC            => noahmp%energy%state%CHUC            ,& ! out,   under canopy sensible heat exchange coefficient (m/s)
              CHV2            => noahmp%energy%state%CHV2            ,& ! out,   2m sensible heat exchange coefficient (m/s) vegetated
              ALBEDO          => noahmp%energy%state%ALBEDO          ,& ! out,   total shortwave surface albedo
              FSR             => noahmp%energy%flux%FSR              ,& ! out,   total reflected solar radiation (w/m2)
              FIRA            => noahmp%energy%flux%FIRA             ,& ! out,   total net LW. rad (w/m2)   [+ to atm]
              FSH             => noahmp%energy%flux%FSH              ,& ! out,   total sensible heat (w/m2) [+ to atm]
              FGEV            => noahmp%energy%flux%FGEV             ,& ! out,   soil evap heat (w/m2) [+ to atm]
              FCEV            => noahmp%energy%flux%FCEV             ,& ! out,   canopy evaporation (w/m2) [+ = to atm]
              FCTR            => noahmp%energy%flux%FCTR             ,& ! out,   transpiration (w/m2) [+ = to atm]
              APAR            => noahmp%energy%flux%APAR             ,& ! out,   total photosyn. active energy (w/m2)
              PARSUN          => noahmp%energy%flux%PARSUN           ,& ! out,   average absorbed par for sunlit leaves (w/m2)
              PARSHA          => noahmp%energy%flux%PARSHA           ,& ! out,   average absorbed par for shaded leaves (w/m2)
              SSOIL           => noahmp%energy%flux%SSOIL            ,& ! out,   soil heat flux (w/m2) [+ to soil]
              PAH             => noahmp%energy%flux%PAH              ,& ! out,   precipitation advected heat - total (W/m2)
              FIRE            => noahmp%energy%flux%FIRE             ,& ! out,   emitted outgoing IR (w/m2)
              IRC             => noahmp%energy%flux%IRC              ,& ! out,   canopy net longwave radiation (w/m2) [+= to atm]
              IRG             => noahmp%energy%flux%IRG              ,& ! out,   ground net longwave radiation (w/m2) [+= to atm]
              IRB             => noahmp%energy%flux%IRB              ,& ! out,   net longwave rad (w/m2) bare ground [+ to atm]
              SHC             => noahmp%energy%flux%SHC              ,& ! out,   canopy sensible heat flux (w/m2)     [+= to atm]
              SHG             => noahmp%energy%flux%SHG              ,& ! out,   ground sensible heat flux (w/m2)     [+= to atm]
              SHB             => noahmp%energy%flux%SHB              ,& ! out,   sensible heat flux (w/m2) bare ground [+ to atm]
              EVG             => noahmp%energy%flux%EVG              ,& ! out,   ground evaporation heat flux (w/m2)  [+= to atm]
              EVB             => noahmp%energy%flux%EVB              ,& ! out,   latent heat flux (w/m2) bare ground [+ to atm]
              EVC             => noahmp%energy%flux%EVC              ,& ! out,   canopy evaporation heat flux (w/m2)  [+= to atm]
              TR              => noahmp%energy%flux%TR               ,& ! out,   canopy transpiration heat flux (w/m2)[+= to atm]
              GHV             => noahmp%energy%flux%GHV              ,& ! out,   vegetated ground heat (w/m2) [+ = to soil]
              GHB             => noahmp%energy%flux%GHB              ,& ! out,   bare ground heat flux (w/m2) [+ to soil]
              PhotosynTotal             => noahmp%biochem%flux%PhotosynTotal             ,& ! out,   total leaf photosynthesis (umol co2 /m2 /s)
              PhotosynLeafSunlit          => noahmp%biochem%flux%PhotosynLeafSunlit          ,& ! out,   sunlit leaf photosynthesis (umol co2 /m2 /s)
              PhotosynLeafShade          => noahmp%biochem%flux%PhotosynLeafShade           & ! out,   shaded leaf photosynthesis (umol co2 /m2 /s)
             )
! ----------------------------------------------------------------------

    ! initialization
    TAUXV   = 0.0
    TAUYV   = 0.0
    IRC     = 0.0
    SHC     = 0.0
    IRG     = 0.0
    SHG     = 0.0
    EVG     = 0.0
    EVC     = 0.0
    TR      = 0.0
    GHV     = 0.0
    PhotosynLeafSunlit  = 0.0
    PhotosynLeafShade  = 0.0
    T2MV    = 0.0
    Q2V     = 0.0
    CHV     = 0.0
    CHLEAF  = 0.0
    CHUC    = 0.0
    CHV2    = 0.0
    PAH     = 0.0

    ! wind speed at reference height: ur >= 1
    UR = max( sqrt(WindEastwardRefHeight**2.0 + WindNorthwardRefHeight**2.0), 1.0 )

    ! vegetated or non-vegetated
    VAI = ELAI + ESAI
    VEG = .false.
    if ( VAI > 0.0 ) VEG = .true.

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
    if ( (VEG .eqv. .true.) .and. (FVEG > 0) ) then ! vegetated portion of the grid
       TGV = TG
       CMV = CM
       CHV = CH
       call SurfaceEnergyFluxVegetated(noahmp)
    endif

    ! temperatures and energy fluxes of bare ground
    TGB = TG
    CMB = CM
    CHB = CH
    call SurfaceEnergyFluxBareGround(noahmp)

    ! compute grid mean quantities by weighting vegetated and bare portions
    ! Energy balance at vege canopy: SAV          =(IRC+SHC+EVC+TR)     *FVEG  at   FVEG 
    ! Energy balance at vege ground: SAG*    FVEG =(IRG+SHG+EVG+GHV)    *FVEG  at   FVEG
    ! Energy balance at bare ground: SAG*(1.-FVEG)=(IRB+SHB+EVB+GHB)*(1.-FVEG) at 1-FVEG
    if ( (VEG .eqv. .true.) .and. (FVEG > 0) ) then
       TAUX  = FVEG * TAUXV + (1.0 - FVEG) * TAUXB
       TAUY  = FVEG * TAUYV + (1.0 - FVEG) * TAUYB
       FIRA  = FVEG * IRG   + (1.0 - FVEG) * IRB   + IRC
       FSH   = FVEG * SHG   + (1.0 - FVEG) * SHB   + SHC
       FGEV  = FVEG * EVG   + (1.0 - FVEG) * EVB
       SSOIL = FVEG * GHV   + (1.0 - FVEG) * GHB
       FCEV  = EVC
       FCTR  = TR
       PAH   = FVEG * PAHG  + (1.0 - FVEG) * PAHB  + PAHV
       TG    = FVEG * TGV   + (1.0 - FVEG) * TGB
       T2M   = FVEG * T2MV  + (1.0 - FVEG) * T2MB
       TS    = FVEG * TV    + (1.0 - FVEG) * TGB
       CM    = FVEG * CMV   + (1.0 - FVEG) * CMB     ! better way to average?
       CH    = FVEG * CHV   + (1.0 - FVEG) * CHB
       Q2E   = FVEG * Q2V   + (1.0 - FVEG) * Q2B 
       Z0WRF = Z0M
       Q1    = FVEG * (EAH*0.622/(PressureAirRefHeight-0.378*EAH)) + (1.0 - FVEG) * QSFC
    else
       TAUX  = TAUXB
       TAUY  = TAUYB
       FIRA  = IRB
       FSH   = SHB
       FGEV  = EVB
       SSOIL = GHB
       TG    = TGB
       T2M   = T2MB
       FCEV  = 0.0
       FCTR  = 0.0
       PAH   = PAHB
       TS    = TG
       CM    = CMB
       CH    = CHB
       Q1    = QSFC
       Q2E   = Q2B
       RSSUN = 0.0
       RSSHA = 0.0
       TGV   = TGB
       CHV   = CHB
       Z0WRF = Z0MG
    endif

    ! emitted longwave radiation and physical check
    FIRE = RadLWDownRefHeight + FIRA
    if ( FIRE <= 0.0 ) then
       write(6,*) 'emitted longwave <0; skin T may be wrong due to inconsistent'
       write(6,*) 'input of SHDFAC with LAI'
       write(6,*) 'SHDFAC=',FVEG,'VAI=',VAI,'TV=',TV,'TG=',TG
       write(6,*) 'RadLWDownRefHeight=',RadLWDownRefHeight,'FIRA=',FIRA,'SnowDepth=',SnowDepth
       !call wrf_error_fatal("STOP in Noah-MP")
    endif

    ! radiative temperature: subtract from the emitted IR the
    ! reflected portion of the incoming longwave radiation, so just
    ! considering the IR originating/emitted in the canopy/ground system.
    ! Old TRAD calculation not taking into account Emissivity:
    ! TRAD = (FIRE/ConstStefanBoltzmann)**0.25
    TRAD = ( (FIRE - (1.0 - EMISSI)*RadLWDownRefHeight) / (EMISSI * ConstStefanBoltzmann) )**0.25

    ! other photosynthesis related quantities for biochem process
    APAR = PARSUN * LAISUN + PARSHA * LAISHA
    PhotosynTotal  = PhotosynLeafSunlit * LAISUN + PhotosynLeafShade * LAISHA

    ! compute snow and soil layer temperature
    call SoilSnowTemperatureMain(noahmp)

    ! adjusting suface temperature based on snow condition
    if ( OptSnowSoilTempTime == 2 ) then
       if ( (SnowDepth > 0.05) .and. (TG > ConstFreezePoint) ) then
          TGV = ConstFreezePoint
          TGB = ConstFreezePoint
          if ( (VEG .eqv. .true.) .and. (FVEG > 0) ) then
             TG = FVEG * TGV + (1.0 - FVEG) * TGB
             TS = FVEG * TV  + (1.0 - FVEG) * TGB
          else
             TG = TGB
             TS = TGB
          endif
       endif
    endif

    ! Phase change and Energy released or consumed by snow & frozen soil
    call SoilSnowWaterPhaseChange(noahmp)

    ! update sensible heat flux due to sprinkler irrigation evaporation
    if ( (FlagCropland .eqv. .true.) .and. (IrrigationFracGrid >= IrriFracThreshold) ) FSH = FSH - FIRR  ! (W/m2)

    ! update total surface albedo
    if ( RadSWDownRefHeight > 0.0 ) then
       ALBEDO = FSR / RadSWDownRefHeight
    else
       ALBEDO = -999.9
    endif

    end associate

  end subroutine EnergyMain

end module EnergyMainMod
