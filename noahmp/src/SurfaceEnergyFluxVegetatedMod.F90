module SurfaceEnergyFluxVegetatedMod

!!! Compute surface energy fluxes and budget for vegetated surface
!!! Use newton-raphson iteration to solve for vegetation and ground temperatures
!!! Surface energy balance:
!!! Canopy level: -RadSwAbsVeg + RadLwNetCanopy + HeatSensibleCanopy + HeatLatentCanEvap + HeatLatentCanTransp = 0
!!! Ground level: -RadSwAbsGrd + RadLwNetVegGrd + HeatSensibleVegGrd + HeatLatentVegGrd + HeatGroundVegGrd = 0

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use VaporPressureSaturationMod,          only : VaporPressureSaturation
  use ResistanceAboveCanopyMostMod,        only : ResistanceAboveCanopyMOST
  use ResistanceAboveCanopyChen97Mod,      only : ResistanceAboveCanopyChen97
  use ResistanceLeafToGroundMod,           only : ResistanceLeafToGround
  use ResistanceCanopyStomataBallBerryMod, only : ResistanceCanopyStomataBallBerry
  use ResistanceCanopyStomataJarvisMod,    only : ResistanceCanopyStomataJarvis

  implicit none

contains

  subroutine SurfaceEnergyFluxVegetated(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: VEGE_FLUX
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type)     , intent(inout) :: noahmp

! local variable
    integer                               :: ITER         ! iteration index
    integer                               :: LITER        ! Last iteration
    integer                               :: MOZSGN       ! number of times MoStabParaAbvCan changes sign
    integer                               :: IndexShade   ! index for sunlit/shaded (0=sunlit;1=shaded)
    integer, parameter                    :: NITERC = 20  ! number of iterations for surface temperature (5~20)
    integer, parameter                    :: NITERG = 5   ! number of iterations for ground temperature (3~5)
    character(len=80)                     :: message      ! error message
    real(kind=kind_noahmp)                :: CAH          ! sensible heat conductance, canopy air to reference height air (m/s)
    real(kind=kind_noahmp)                :: DTV          ! change in tv, last iteration (k)
    real(kind=kind_noahmp)                :: DTG          ! change in tg, last iteration (k)
    real(kind=kind_noahmp)                :: AIR,CIR      ! coefficients for ir as function of ts**4
    real(kind=kind_noahmp)                :: CSH          ! coefficients for sh as function of ts
    real(kind=kind_noahmp)                :: CEV          ! coefficients for ev as function of esat[ts]
    real(kind=kind_noahmp)                :: CGH          ! coefficients for st as function of ts
    real(kind=kind_noahmp)                :: ATR,CTR      ! coefficients for tr as function of esat[ts]
    real(kind=kind_noahmp)                :: ATA,BTA      ! coefficients for tah as function of ts
    real(kind=kind_noahmp)                :: AEA,BEA      ! coefficients for eah as function of esat[ts]
    real(kind=kind_noahmp)                :: ESATW        ! es for water
    real(kind=kind_noahmp)                :: ESATI        ! es for ice
    real(kind=kind_noahmp)                :: DSATW        ! d(es)/dt at tg (pa/k) for water
    real(kind=kind_noahmp)                :: DSATI        ! d(es)/dt at tg (pa/k) for ice
    real(kind=kind_noahmp)                :: A            ! temporary calculation
    real(kind=kind_noahmp)                :: B            ! temporary calculation
    real(kind=kind_noahmp)                :: CVH          ! sensible heat conductance, leaf surface to canopy air (m/s)
    real(kind=kind_noahmp)                :: COND         ! sum of conductances (m/s)
    real(kind=kind_noahmp)                :: H            ! temporary sensible heat flux (w/m2)
    real(kind=kind_noahmp)                :: HG           ! temporary sensible heat flux (w/m2)
    real(kind=kind_noahmp)                :: CQ2V         ! exchange coefficient for water vapor, 2m over vegetation.
    real(kind=kind_noahmp)                :: MoistureFluxSfc          ! moisture flux
    real(kind=kind_noahmp)                :: VegAreaIndTmp         ! total leaf area index + stem area index,effective
    real(kind=kind_noahmp)                :: LAISUNE      ! sunlit leaf area index, one-sided (m2/m2),effective
    real(kind=kind_noahmp)                :: LAISHAE      ! shaded leaf area index, one-sided (m2/m2),effective
    real(kind=kind_noahmp)                :: T, TDC       ! Kelvin to degree Celsius with limit -50 to +50
! local statement function
    TDC(T) = min( 50.0, max(-50.0, (T - ConstFreezePoint)) )

! --------------------------------------------------------------------
    associate(                                                        &
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,    main noahmp timestep (s)
              GridIndexI      => noahmp%config%domain%GridIndexI     ,& ! in,    grid index in x-direction
              GridIndexJ      => noahmp%config%domain%GridIndexJ     ,& ! in,    grid index in y-direction
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg,& ! in,    actual number of snow layers (negative)
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! in,    thickness of snow/soil layers (m)
              OptSurfaceDrag => noahmp%config%nmlist%OptSurfaceDrag,& ! in,    options for surface layer drag/exchange coefficient
              OptStomataResistance => noahmp%config%nmlist%OptStomataResistance ,& ! in,    options for canopy stomatal resistance
              OptSnowSoilTempTime => noahmp%config%nmlist%OptSnowSoilTempTime,& ! in,    options for snow/soil temperature time scheme (only layer 1)
              WindEastwardRefHeight   => noahmp%forcing%WindEastwardRefHeight   ,& ! in,    wind speed [m/s] in eastward direction at reference height
              WindNorthwardRefHeight  => noahmp%forcing%WindNorthwardRefHeight  ,& ! in,    wind speed [m/s] in northward direction at reference height
              RadLwDownRefHeight      => noahmp%forcing%RadLwDownRefHeight      ,& ! in,    downward longwave radiation [W/m2] at reference height
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight ,& ! in,    air temperature [K] at reference height
              PressureAirRefHeight    => noahmp%forcing%PressureAirRefHeight    ,& ! in,    air pressure [Pa] at reference height
              PressureAirSurface      => noahmp%forcing%PressureAirSurface      ,& ! in,    air pressure [Pa] at surface-atmos interface
              SnowDepth           => noahmp%water%state%SnowDepth            ,& ! in,    snow depth [m]
              SnowCoverFrac            => noahmp%water%state%SnowCoverFrac             ,& ! in,    snow cover fraction [-]
              CanopyWetFrac            => noahmp%water%state%CanopyWetFrac             ,& ! in,    wetted or snowed fraction of the canopy
              CanopyLiqWater          => noahmp%water%state%CanopyLiqWater           ,& ! in,    canopy intercepted liquid water (mm)
              CanopyIce          => noahmp%water%state%CanopyIce           ,& ! in,    canopy intercepted ice [mm]
              HeightCanopyTop             => noahmp%energy%param%HeightCanopyTop             ,& ! in,    top of canopy (m)
              ZilitinkevichCoeff            => noahmp%energy%param%ZilitinkevichCoeff            ,& ! in,    Zilitinkevich Coefficient for exchange coefficient calculation
              RadSwAbsVeg             => noahmp%energy%flux%RadSwAbsVeg              ,& ! in,    solar radiation absorbed by vegetation (w/m2)
              RadSwAbsGrd             => noahmp%energy%flux%RadSwAbsGrd              ,& ! in,    solar radiation absorbed by ground (w/m2)
              HeatPrecipAdvCanopy            => noahmp%energy%flux%HeatPrecipAdvCanopy             ,& ! in,    precipitation advected heat - vegetation net (W/m2)
              HeatPrecipAdvVegGrd            => noahmp%energy%flux%HeatPrecipAdvVegGrd             ,& ! in,    precipitation advected heat - under canopy net (W/m2)
              RefHeightAboveGrd            => noahmp%energy%state%RefHeightAboveGrd            ,& ! in,    surface reference height (m)
              VegFrac            => noahmp%energy%state%VegFrac            ,& ! in,    greeness vegetation fraction (-)
              WindSpdRefHeight              => noahmp%energy%state%WindSpdRefHeight              ,& ! in,    wind speed (m/s) at reference height
              PressureVaporRefHeight            => noahmp%energy%state%PressureVaporRefHeight            ,& ! in,    vapor pressure air (pa) at reference height
              SpecHumidityRefHeight => noahmp%forcing%SpecHumidityRefHeight,& ! in,    specific humidity (kg/kg) at reference height
              DensityAirRefHeight          => noahmp%energy%state%DensityAirRefHeight          ,& ! in,    density air (kg/m3)
              VegAreaIndEff             => noahmp%energy%state%VegAreaIndEff             ,& ! in,    one-sided leaf+stem area index (m2/m2)
              LeafAreaIndSunlit          => noahmp%energy%state%LeafAreaIndSunlit          ,& ! in,    sunlit leaf area index, one-sided (m2/m2)
              LeafAreaIndShade          => noahmp%energy%state%LeafAreaIndShade          ,& ! in,    shaded leaf area index, one-sided (m2/m2)
              ZeroPlaneDispSfc             => noahmp%energy%state%ZeroPlaneDispSfc             ,& ! in,    zero plane displacement (m)
              RoughLenMomSfc             => noahmp%energy%state%RoughLenMomSfc             ,& ! in,    roughness length, momentum, (m), surface
              RoughLenMomGrd            => noahmp%energy%state%RoughLenMomGrd            ,& ! in,    roughness length, momentum, ground (m)
              EmissivityVeg             => noahmp%energy%state%EmissivityVeg             ,& ! in,    vegetation emissivity
              EmissivityGrd             => noahmp%energy%state%EmissivityGrd             ,& ! in,    ground emissivity
              TemperatureSoilSnow             => noahmp%energy%state%TemperatureSoilSnow             ,& ! in,    snow and soil layer temperature [k]
              ThermConductSoilSnow              => noahmp%energy%state%ThermConductSoilSnow              ,& ! in,    thermal conductivity [w/m/k] for all soil & snow
              ResistanceGrdEvap           => noahmp%energy%state%ResistanceGrdEvap           ,& ! in,    ground surface resistance (s/m) to evaporation
              PsychConstCanopy          => noahmp%energy%state%PsychConstCanopy          ,& ! in,    psychrometric constant (pa/K), canopy
              LatHeatVapCanopy         => noahmp%energy%state%LatHeatVapCanopy         ,& ! in,    latent heat of vaporization/subli (j/kg), canopy
              PsychConstGrd          => noahmp%energy%state%PsychConstGrd          ,& ! in,    psychrometric constant (pa/K), ground
              RelHumidityGrd           => noahmp%energy%state%RelHumidityGrd           ,& ! in,    raltive humidity in surface soil/snow air space (-)
              SpecHumiditySfcBare            => noahmp%energy%state%SpecHumiditySfcBare            ,& ! inout, specific humidity at bare surface
              PressureVaporCanAir             => noahmp%energy%state%PressureVaporCanAir             ,& ! inout, canopy air vapor pressure (pa)
              TemperatureCanopyAir             => noahmp%energy%state%TemperatureCanopyAir             ,& ! inout, canopy air temperature (K)
              TemperatureCanopy              => noahmp%energy%state%TemperatureCanopy              ,& ! inout, vegetation temperature (K)
              TemperatureGrdVeg             => noahmp%energy%state%TemperatureGrdVeg             ,& ! inout, vegetated ground (below-canopy) temperature (K)
              ExchCoeffMomAbvCan              => noahmp%energy%state%ExchCoeffMomAbvCan             ,& ! inout, momentum exchange coefficient (m/s), above ZeroPlaneDisp, vegetated
              ExchCoeffShAbvCan              => noahmp%energy%state%ExchCoeffShAbvCan             ,& ! inout, heat exchange coefficient (m/s), above ZeroPlaneDisp, vegetated
              WindStressEwVeg           => noahmp%energy%state%WindStressEwVeg           ,& ! out,   wind stress: east-west (n/m2) above canopy
              WindStressNsVeg           => noahmp%energy%state%WindStressNsVeg           ,& ! out,   wind stress: north-south (n/m2) above canopy
              TemperatureAir2mVeg            => noahmp%energy%state%TemperatureAir2mVeg            ,& ! out,   2 m height air temperature (k), vegetated
              ExchCoeffShLeaf          => noahmp%energy%state%ExchCoeffShLeaf          ,& ! out,   leaf sensible heat exchange coefficient (m/s),leaf surface to canopy air
              ExchCoeffShUndCan            => noahmp%energy%state%ExchCoeffShUndCan            ,& ! out,   under canopy sensible heat exchange coefficient (m/s)
              ExchCoeffSh2mVeg            => noahmp%energy%state%ExchCoeffSh2mVeg            ,& ! out,   2m sensible heat exchange coefficient (m/s)
              SpecHumidity2mVeg             => noahmp%energy%state%SpecHumidity2mVeg            ,& ! out,   specific humidity at 2m vegetated
              ResistanceStomataSunlit           => noahmp%energy%state%ResistanceStomataSunlit           ,& ! out,   sunlit leaf stomatal resistance (s/m)
              ResistanceStomataShade           => noahmp%energy%state%ResistanceStomataShade           ,& ! out,   shaded leaf stomatal resistance (s/m)
              FrictionVelVeg              => noahmp%energy%state%FrictionVelVeg             ,& ! out,   friction velocity (m/s), vegetated
              RoughLenShCanopy             => noahmp%energy%state%RoughLenShCanopy            ,& ! out,   roughness length, sensible heat (m), vegetated
              RoughLenShVegGrd            => noahmp%energy%state%RoughLenShVegGrd            ,& ! out,   roughness length, sensible heat ground (m), below canopy
              ResistanceLeafBoundary              => noahmp%energy%state%ResistanceLeafBoundary              ,& ! out,   bulk leaf boundary layer resistance (s/m)
              ResistanceShAbvCan            => noahmp%energy%state%ResistanceShAbvCan            ,& ! out,   aerodynamic resistance for sensible heat (s/m), above canopy
              ResistanceLhAbvCan            => noahmp%energy%state%ResistanceLhAbvCan            ,& ! out,   aerodynamic resistance for water vapor (s/m), above canopy
              ResistanceShUndCan            => noahmp%energy%state%ResistanceShUndCan            ,& ! out,   ground aerodynamic resistance for sensible heat (s/m)
              ResistanceLhUndCan            => noahmp%energy%state%ResistanceLhUndCan            ,& ! out,   ground aerodynamic resistance for water vapor (s/m)
              ExchCoeffLhAbvCan             => noahmp%energy%state%ExchCoeffLhAbvCan             ,& ! out,   latent heat conductance, canopy air to reference height air (m/s)
              ExchCoeffLhTransp             => noahmp%energy%state%ExchCoeffLhTransp             ,& ! out,   transpiration conductance, leaf to canopy air (m/s)
              ExchCoeffLhEvap             => noahmp%energy%state%ExchCoeffLhEvap             ,& ! out,   evaporation conductance, leaf to canopy air (m/s)
              ExchCoeffLhUndCan             => noahmp%energy%state%ExchCoeffLhUndCan             ,& ! out,   latent heat conductance, ground to canopy air (m/s)
              VapPresSatCanopy            => noahmp%energy%state%VapPresSatCanopy            ,& ! out,   saturation vapor pressure at TemperatureCanopy (pa)
              VapPresSatGrdVeg            => noahmp%energy%state%VapPresSatGrdVeg            ,& ! out,   saturation vapor pressure at TemperatureGrd (pa)
              VapPresSatCanTempD           => noahmp%energy%state%VapPresSatCanTempD           ,& ! out,   d(VapPresSatCanopy)/dt at TemperatureCanopy (pa/k)
              VapPresSatGrdVegTempD           => noahmp%energy%state%VapPresSatGrdVegTempD           ,& ! out,   d(VapPresSatGrdVeg)/dt at TemperatureGrd (pa/k)
              CanopyHeight            => noahmp%energy%state%CanopyHeight            ,& ! out,   canopy height (m)
              WindSpdCanopyTop              => noahmp%energy%state%WindSpdCanopyTop             ,& ! out,   wind speed at top of canopy (m/s)
              MoStabParaAbvCan           => noahmp%energy%state%MoStabParaAbvCan          ,& ! out,   Monin-Obukhov stability (z/L), above ZeroPlaneDispSfc, vegetated
              MoStabCorrShVeg2m             => noahmp%energy%state%MoStabCorrShVeg2m            ,& ! out,   M-O sen heat stability correction, 2m, vegetated
              RadLwNetCanopy             => noahmp%energy%flux%RadLwNetCanopy              ,& ! out,   canopy net longwave radiation (w/m2) [+= to atm]
              HeatSensibleCanopy             => noahmp%energy%flux%HeatSensibleCanopy              ,& ! out,   canopy sensible heat flux (w/m2)     [+= to atm]
              HeatLatentCanEvap             => noahmp%energy%flux%HeatLatentCanEvap              ,& ! out,   canopy evaporation heat flux (w/m2)  [+= to atm]
              RadLwNetVegGrd             => noahmp%energy%flux%RadLwNetVegGrd              ,& ! out,   ground net longwave radiation (w/m2) [+= to atm]
              HeatSensibleVegGrd             => noahmp%energy%flux%HeatSensibleVegGrd              ,& ! out,   vegetated ground sensible heat flux (w/m2)     [+= to atm]
              HeatLatentVegGrd             => noahmp%energy%flux%HeatLatentVegGrd              ,& ! out,   ground evaporation heat flux (w/m2)  [+= to atm]
              HeatLatentCanTransp              => noahmp%energy%flux%HeatLatentCanTransp               ,& ! out,   canopy transpiration heat flux (w/m2)[+= to atm]
              HeatGroundVegGrd   => noahmp%energy%flux%HeatGroundVegGrd               & ! out,   vegetated ground heat (w/m2) [+ = to soil/snow]
             )
! ----------------------------------------------------------------------

    ! initialization (including variables that do not depend on stability iteration)
    LITER   = 0
    FrictionVelVeg      = 0.1
    DTV     = 0.0
    DTG     = 0.0
    MoStabParaAbvCan     = 0.0
    MOZSGN  = 0
    MoStabCorrShVeg2m     = 0.0
    HG      = 0.0
    H       = 0.0
    MoistureFluxSfc     = 0.0
    ! limit LeafAreaIndex
    VegAreaIndTmp    = min( 6.0, VegAreaIndEff    )
    LAISUNE = min( 6.0, LeafAreaIndSunlit )
    LAISHAE = min( 6.0, LeafAreaIndShade )

    ! saturation vapor pressure at ground temperature
    T = TDC(TemperatureGrdVeg)
    call VaporPressureSaturation(T, ESATW, ESATI, DSATW, DSATI)
    if ( T > 0.0 ) then
       VapPresSatGrdVeg = ESATW
    else
       VapPresSatGrdVeg = ESATI
    endif
    !jref - consistent surface specific humidity for sfcdif3 and sfcdif4
    SpecHumiditySfcBare = 0.622 * PressureVaporRefHeight / (PressureAirSurface - 0.378*PressureVaporRefHeight)

    ! canopy height
    CanopyHeight = HeightCanopyTop
    ! wind speed at canopy height
    !WindSpdCanopyTop = WindSpdRefHeight * log(CanopyHeight/RoughLenMomSfc) / log(RefHeightAboveGrd/RoughLenMomSfc)
    WindSpdCanopyTop = WindSpdRefHeight * log( (CanopyHeight - ZeroPlaneDispSfc + RoughLenMomSfc)/RoughLenMomSfc ) / log(RefHeightAboveGrd/RoughLenMomSfc)   ! MB: add ZeroPlaneDispSfc v3.7
    if ( (CanopyHeight-ZeroPlaneDispSfc) <= 0.0 ) then
       print*, 'CRITICAL PROBLEM: CanopyHeight <= ZeroPlaneDispSfc'
       print*, 'GridIndexI,GridIndexJ =',GridIndexI, GridIndexJ
       print*, 'CanopyHeight  =',CanopyHeight
       print*, 'ZeroPlaneDispSfc   =',ZeroPlaneDispSfc
       print*, 'SnowDepth =',SnowDepth
       stop 'error'
    endif

    ! prepare for longwave rad.
    AIR = -EmissivityVeg * (1.0 + (1.0-EmissivityVeg)*(1.0-EmissivityGrd)) * RadLwDownRefHeight - &
          EmissivityVeg * EmissivityGrd * ConstStefanBoltzmann * TemperatureGrdVeg**4
    CIR = ( 2.0 - EmissivityVeg * (1.0-EmissivityGrd) ) * EmissivityVeg * ConstStefanBoltzmann

    ! begin stability iteration for canopy temperature and flux
    loop1: do ITER = 1, NITERC

       ! ground and surface roughness length
       if ( ITER == 1 ) then
          RoughLenShCanopy  = RoughLenMomSfc
          RoughLenShVegGrd = RoughLenMomGrd
       else
          RoughLenShCanopy  = RoughLenMomSfc    !* exp(-ZilitinkevichCoeff * 0.4 * 258.2 * sqrt(FrictionVelVeg*RoughLenMomSfc))
          RoughLenShVegGrd = RoughLenMomGrd   !* exp(-ZilitinkevichCoeff * 0.4 * 258.2 * sqrt(FrictionVelVeg*RoughLenMomGrd))
       endif

       ! aerodyn resistances between RefHeightAboveGrd and d+z0v
       if ( OptSurfaceDrag == 1 ) call ResistanceAboveCanopyMOST(noahmp, ITER, H, MOZSGN)
       if ( OptSurfaceDrag == 2 ) call ResistanceAboveCanopyChen97(noahmp, ITER)

       ! aerodyn resistance between z0g and d+z0v, and leaf boundary layer resistance
       call ResistanceLeafToGround(noahmp, ITER, VegAreaIndTmp, HG)

       ! ES and d(ES)/dt evaluated at TemperatureCanopy
       T = TDC(TemperatureCanopy)
       call VaporPressureSaturation(T, ESATW, ESATI, DSATW, DSATI)
       if ( T > 0.0 ) then
          VapPresSatCanopy  = ESATW
          VapPresSatCanTempD = DSATW
       else
          VapPresSatCanopy  = ESATI
          VapPresSatCanTempD = DSATI
       endif

       ! stomatal resistance
       if ( ITER == 1 ) then
          if ( OptStomataResistance == 1 ) then  ! Ball-Berry
             IndexShade = 0 ! sunlit case
             call ResistanceCanopyStomataBallBerry(noahmp, IndexShade)
             IndexShade = 1 ! shaded case
             call ResistanceCanopyStomataBallBerry(noahmp, IndexShade)
          endif
          if ( OptStomataResistance == 2 ) then  ! Jarvis
             IndexShade = 0 ! sunlit case
             call ResistanceCanopyStomataJarvis(noahmp, IndexShade)
             IndexShade = 1 ! shaded case
             call ResistanceCanopyStomataJarvis(noahmp, IndexShade)
          endif
       endif

       ! sensible heat conductance and coeff above veg.
       CAH  = 1.0 / ResistanceShAbvCan
       CVH  = 2.0 * VegAreaIndTmp / ResistanceLeafBoundary
       CGH  = 1.0 / ResistanceShUndCan
       COND = CAH + CVH + CGH
       ATA  = (TemperatureAirRefHeight * CAH + TemperatureGrdVeg * CGH) / COND
       BTA  = CVH / COND
       CSH  = (1.0 - BTA) * DensityAirRefHeight * ConstHeatCapacAir * CVH

       ! latent heat conductance and coeff above veg.
       ExchCoeffLhAbvCan  = 1.0 / ResistanceLhAbvCan
       ExchCoeffLhEvap  = CanopyWetFrac * VegAreaIndTmp / ResistanceLeafBoundary
       ExchCoeffLhTransp  = (1.0 - CanopyWetFrac) * ( LAISUNE/(ResistanceLeafBoundary+ResistanceStomataSunlit) + LAISHAE/(ResistanceLeafBoundary+ResistanceStomataShade) )
       ExchCoeffLhUndCan  = 1.0 / (ResistanceLhUndCan + ResistanceGrdEvap)
       COND = ExchCoeffLhAbvCan + ExchCoeffLhEvap + ExchCoeffLhTransp + ExchCoeffLhUndCan
       AEA  = ( PressureVaporRefHeight*ExchCoeffLhAbvCan + VapPresSatGrdVeg*ExchCoeffLhUndCan ) / COND
       BEA  = (ExchCoeffLhEvap + ExchCoeffLhTransp) / COND
       CEV  = (1.0 - BEA) * ExchCoeffLhEvap * DensityAirRefHeight * ConstHeatCapacAir / PsychConstCanopy   ! Barlage: change to vegetation v3.6
       CTR  = (1.0 - BEA) * ExchCoeffLhTransp * DensityAirRefHeight * ConstHeatCapacAir / PsychConstCanopy

       ! evaluate surface fluxes with current temperature and solve for dts
       TemperatureCanopyAir  = ATA + BTA * TemperatureCanopy               ! canopy air T.
       PressureVaporCanAir  = AEA + BEA * VapPresSatCanopy             ! canopy air e
       RadLwNetCanopy  = VegFrac * (AIR + CIR * TemperatureCanopy**4)
       HeatSensibleCanopy  = VegFrac * DensityAirRefHeight * ConstHeatCapacAir * CVH * (TemperatureCanopy - TemperatureCanopyAir)
       HeatLatentCanEvap  = VegFrac * DensityAirRefHeight * ConstHeatCapacAir * ExchCoeffLhEvap * (VapPresSatCanopy - PressureVaporCanAir) / PsychConstCanopy ! Barlage: change to v in v3.6
       HeatLatentCanTransp   = VegFrac * DensityAirRefHeight * ConstHeatCapacAir * ExchCoeffLhTransp * (VapPresSatCanopy - PressureVaporCanAir) / PsychConstCanopy
       if ( TemperatureCanopy > ConstFreezePoint ) then
          HeatLatentCanEvap = min( CanopyLiqWater*LatHeatVapCanopy/MainTimeStep, HeatLatentCanEvap )    ! Barlage: add if block for canopy ice in v3.6
       else
          HeatLatentCanEvap = min( CanopyIce*LatHeatVapCanopy/MainTimeStep, HeatLatentCanEvap )
       endif
       B    = RadSwAbsVeg - RadLwNetCanopy - HeatSensibleCanopy - &
              HeatLatentCanEvap - HeatLatentCanTransp + HeatPrecipAdvCanopy  ! additional w/m2
       A    = VegFrac * ( 4.0*CIR*TemperatureCanopy**3 + CSH + (CEV+CTR)*VapPresSatCanTempD ) !volumetric heat capacity
       DTV  = B / A
       RadLwNetCanopy  = RadLwNetCanopy + VegFrac * 4.0 * CIR * TemperatureCanopy**3 * DTV
       HeatSensibleCanopy  = HeatSensibleCanopy + VegFrac * CSH * DTV
       HeatLatentCanEvap  = HeatLatentCanEvap + VegFrac * CEV * VapPresSatCanTempD * DTV
       HeatLatentCanTransp   = HeatLatentCanTransp  + VegFrac * CTR * VapPresSatCanTempD * DTV
       TemperatureCanopy   = TemperatureCanopy + DTV       ! update vegetation surface temperature
       !TemperatureCanopyAir = ATA + BTA * TemperatureCanopy  ! canopy air T; update here for consistency

       ! for computing M-O length in the next iteration
       H    = DensityAirRefHeight * ConstHeatCapacAir * (TemperatureCanopyAir - TemperatureAirRefHeight) / ResistanceShAbvCan
       HG   = DensityAirRefHeight * ConstHeatCapacAir * (TemperatureGrdVeg  - TemperatureCanopyAir)   / ResistanceShUndCan

       ! consistent specific humidity from canopy air vapor pressure
       SpecHumiditySfcBare = (0.622 * PressureVaporCanAir) / (PressureAirRefHeight - 0.378 * PressureVaporCanAir)
       if ( LITER == 1 ) then
          exit loop1
       endif
       if ( (ITER >= 5) .and. (abs(DTV) <= 0.01) .and. (LITER == 0) ) then
          LITER = 1
       endif
    enddo loop1  ! end stability iteration

    ! under-canopy fluxes and ground temperature
    AIR = -EmissivityGrd * (1.0 - EmissivityVeg) * RadLwDownRefHeight - EmissivityGrd * EmissivityVeg * ConstStefanBoltzmann * TemperatureCanopy**4
    CIR = EmissivityGrd * ConstStefanBoltzmann
    CSH = DensityAirRefHeight * ConstHeatCapacAir / ResistanceShUndCan
    CEV = DensityAirRefHeight * ConstHeatCapacAir / (PsychConstGrd * (ResistanceLhUndCan+ResistanceGrdEvap))  ! Barlage: change to ground v3.6
    CGH = 2.0 * ThermConductSoilSnow(NumSnowLayerNeg+1) / ThicknessSnowSoilLayer(NumSnowLayerNeg+1)
    ! begin stability iteration
    loop2: do ITER = 1, NITERG
       T = TDC(TemperatureGrdVeg)
       call VaporPressureSaturation(T, ESATW, ESATI, DSATW, DSATI)
       if ( T > 0.0 ) then
          VapPresSatGrdVeg  = ESATW
          VapPresSatGrdVegTempD = DSATW
       else
          VapPresSatGrdVeg  = ESATI
          VapPresSatGrdVegTempD = DSATI
       endif
       RadLwNetVegGrd = CIR * TemperatureGrdVeg**4 + AIR
       HeatSensibleVegGrd = CSH * (TemperatureGrdVeg        - TemperatureCanopyAir         )
       HeatLatentVegGrd = CEV * (VapPresSatGrdVeg*RelHumidityGrd - PressureVaporCanAir         )
       HeatGroundVegGrd  = CGH * (TemperatureGrdVeg        - TemperatureSoilSnow(NumSnowLayerNeg+1))
       B   = RadSwAbsGrd - RadLwNetVegGrd - HeatSensibleVegGrd - HeatLatentVegGrd - HeatGroundVegGrd + HeatPrecipAdvVegGrd
       A   = 4.0 * CIR * TemperatureGrdVeg**3 + CSH + CEV*VapPresSatGrdVegTempD + CGH
       DTG = B / A
       RadLwNetVegGrd = RadLwNetVegGrd + 4.0 * CIR * TemperatureGrdVeg**3 * DTG
       HeatSensibleVegGrd = HeatSensibleVegGrd + CSH * DTG
       HeatLatentVegGrd = HeatLatentVegGrd + CEV * VapPresSatGrdVegTempD * DTG
       HeatGroundVegGrd  = HeatGroundVegGrd  + CGH * DTG
       TemperatureGrdVeg = TemperatureGrdVeg + DTG
    enddo loop2
    !TemperatureCanopyAir = (CAH*TemperatureAirRefHeight + CVH*TemperatureCanopy + CGH*TemperatureGrdVeg)/(CAH + CVH + CGH)

    ! if snow on ground and TemperatureGrdVeg > freezing point: reset TemperatureGrdVeg = freezing point. reevaluate ground fluxes.
    if ( (OptSnowSoilTempTime == 1) .or. (OptSnowSoilTempTime == 3) ) then
       if ( (SnowDepth > 0.05) .and. (TemperatureGrdVeg > ConstFreezePoint) ) then
          if ( OptSnowSoilTempTime == 1 ) TemperatureGrdVeg = ConstFreezePoint
          if ( OptSnowSoilTempTime == 3 ) TemperatureGrdVeg = (1.0 - SnowCoverFrac) * TemperatureGrdVeg + SnowCoverFrac * ConstFreezePoint   ! MB: allow TemperatureGrdVeg>0C during melt v3.7
          RadLwNetVegGrd = CIR * TemperatureGrdVeg**4 - EmissivityGrd * (1.0-EmissivityVeg) * RadLwDownRefHeight - EmissivityGrd * EmissivityVeg * ConstStefanBoltzmann * TemperatureCanopy**4
          HeatSensibleVegGrd = CSH * (TemperatureGrdVeg        - TemperatureCanopyAir)
          HeatLatentVegGrd = CEV * (VapPresSatGrdVeg*RelHumidityGrd - PressureVaporCanAir)
          HeatGroundVegGrd  = RadSwAbsGrd + HeatPrecipAdvVegGrd - (RadLwNetVegGrd + HeatSensibleVegGrd + HeatLatentVegGrd)
       endif
    endif

    ! wind stresses
    WindStressEwVeg = -DensityAirRefHeight * ExchCoeffMomAbvCan * WindSpdRefHeight * WindEastwardRefHeight
    WindStressNsVeg = -DensityAirRefHeight * ExchCoeffMomAbvCan * WindSpdRefHeight * WindNorthwardRefHeight

    ! consistent vegetation air temperature and vapor pressure since TemperatureGrdVeg is not consistent with the TemperatureCanopyAir/PressureVaporCanAir calculation.
    ! TemperatureCanopyAir = TemperatureAirRefHeight + (HeatSensibleVegGrd+HeatSensibleCanopy) / (DensityAirRefHeight*ConstHeatCapacAir*CAH) 
    ! TemperatureCanopyAir = TemperatureAirRefHeight + (HeatSensibleVegGrd*VegFrac+HeatSensibleCanopy) / (DensityAirRefHeight*ConstHeatCapacAir*CAH) ! ground flux need fveg
    ! PressureVaporCanAir = PressureVaporRefHeight + (HeatLatentCanEvap+VegFrac*(HeatLatentCanTransp+HeatLatentVegGrd)) / (DensityAirRefHeight*ExchCoeffLhAbvCan*ConstHeatCapacAir/PsychConstGrd)
    ! MoistureFluxSfc = (SpecHumiditySfcBare-SpecHumidityRefHeight) * DensityAirRefHeight * ExchCoeffLhAbvCan !*ConstHeatCapacAir/PsychConstGrd

    ! 2m temperature over vegetation ( corrected for low CQ2V values )
    if ( (OptSurfaceDrag == 1) .or. (OptSurfaceDrag == 2) ) then
       ! ExchCoeffSh2mVeg = FrictionVelVeg * 1.0 / ConstVonKarman * log((2.0+RoughLenShCanopy)/RoughLenShCanopy)
       ! ExchCoeffSh2mVeg = FrictionVelVeg * ConstVonKarman / log((2.0+RoughLenShCanopy)/RoughLenShCanopy)
       ExchCoeffSh2mVeg = FrictionVelVeg * ConstVonKarman / ( log((2.0+RoughLenShCanopy)/RoughLenShCanopy) - MoStabCorrShVeg2m )
       CQ2V = ExchCoeffSh2mVeg
       if ( ExchCoeffSh2mVeg < 1.0e-5 ) then
          TemperatureAir2mVeg = TemperatureCanopyAir
          !SpecHumidity2mVeg  = (PressureVaporCanAir*0.622/(PressureAirRefHeight - 0.378*PressureVaporCanAir))
          SpecHumidity2mVeg  = SpecHumiditySfcBare
       else
          TemperatureAir2mVeg = TemperatureCanopyAir - (HeatSensibleVegGrd + HeatSensibleCanopy/VegFrac) / &
                                (DensityAirRefHeight * ConstHeatCapacAir) * 1.0 / ExchCoeffSh2mVeg
          !SpecHumidity2mVeg = (PressureVaporCanAir*0.622/(PressureAirRefHeight - 0.378*PressureVaporCanAir))- MoistureFluxSfc/(DensityAirRefHeight*FrictionVelVeg)* 1./ConstVonKarman * LOG((2.+RoughLenShCanopy)/RoughLenShCanopy)
          SpecHumidity2mVeg  = SpecHumiditySfcBare - ( (HeatLatentCanEvap+HeatLatentCanTransp)/VegFrac + HeatLatentVegGrd ) / (LatHeatVapCanopy * DensityAirRefHeight) * 1.0 / CQ2V
       endif
    endif

    ! update ExchCoeffSh for output
    ExchCoeffShAbvCan     = CAH
    ExchCoeffShLeaf = CVH
    ExchCoeffShUndCan   = 1.0 / ResistanceShUndCan

    end associate

  end subroutine SurfaceEnergyFluxVegetated

end module SurfaceEnergyFluxVegetatedMod
