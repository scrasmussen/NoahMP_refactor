module SurfaceEnergyFluxBareGroundMod

!!! Compute surface energy fluxes and budget for bare ground
!!! Use newton-raphson iteration to solve for ground temperatures
!!! Surface energy balance (bare soil):
!!! Ground level: -RadSwAbsGrd + RadLwNetBareGrd + HeatSensibleBareGrd + HeatLatentBareGrd + HeatGroundBareGrd = 0

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use VaporPressureSaturationMod,    only : VaporPressureSaturation
  use ResistanceBareGroundMostMod,   only : ResistanceBareGroundMOST
  use ResistanceBareGroundChen97Mod, only : ResistanceBareGroundChen97

  implicit none

contains

  subroutine SurfaceEnergyFluxBareGround(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: BARE_FLUX
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type)     , intent(inout) :: noahmp

! local variable
    integer                               :: ITER         ! iteration index
    integer                               :: MOZSGN       ! number of times MoStabParaBare changes sign
    integer, parameter                    :: NITERB = 5   ! number of iterations for surface temperature
    real(kind=kind_noahmp)                :: DTG          ! change in tg, last iteration (k)
    real(kind=kind_noahmp)                :: CIR          ! coefficients for ir as function of ts**4
    real(kind=kind_noahmp)                :: CSH          ! coefficients for sh as function of ts
    real(kind=kind_noahmp)                :: CEV          ! coefficients for ev as function of esat[ts]
    real(kind=kind_noahmp)                :: CGH          ! coefficients for st as function of ts
    real(kind=kind_noahmp)                :: CQ2B         ! exchange coefficient for 2m temp. 
    real(kind=kind_noahmp)                :: EHB          ! temporary sensible heat exchange coefficient (m/s)
    real(kind=kind_noahmp)                :: EMB          ! temporary momentum heat exchange coefficient (m/s)
    real(kind=kind_noahmp)                :: MoistureFluxSfc          ! moisture flux
    real(kind=kind_noahmp)                :: ESATW        ! es for water
    real(kind=kind_noahmp)                :: ESATI        ! es for ice
    real(kind=kind_noahmp)                :: DSATW        ! d(es)/dt at tg (pa/k) for water
    real(kind=kind_noahmp)                :: DSATI        ! d(es)/dt at tg (pa/k) for ice
    real(kind=kind_noahmp)                :: A            ! temporary calculation
    real(kind=kind_noahmp)                :: B            ! temporary calculation
    real(kind=kind_noahmp)                :: H            ! temporary sensible heat flux (w/m2)
    real(kind=kind_noahmp)                :: T, TDC       ! Kelvin to degree Celsius with limit -50 to +50
! local statement function
    TDC(T) = min( 50.0, max(-50.0, (T - ConstFreezePoint)) )

! --------------------------------------------------------------------
    associate(                                                        &
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg,& ! in,    actual number of snow layers (negative)
              ThicknessSnowSoilLayer          => noahmp%config%domain%ThicknessSnowSoilLayer         ,& ! in,    thickness of snow/soil layers (m)
              FlagUrban      => noahmp%config%domain%FlagUrban     ,& ! in,    logical flag for urban grid
              OptSurfaceDrag  => noahmp%config%nmlist%OptSurfaceDrag ,& ! in,    options for surface layer drag/exchange coefficient
              OptSnowSoilTempTime => noahmp%config%nmlist%OptSnowSoilTempTime,& ! in,    options for snow/soil temperature time scheme (only layer 1)
              RadLwDownRefHeight => noahmp%forcing%RadLwDownRefHeight,& ! in,    downward longwave radiation [W/m2] at reference height
              WindEastwardRefHeight   => noahmp%forcing%WindEastwardRefHeight,& ! in,    wind speed [m/s] in eastward direction at reference height
              WindNorthwardRefHeight  => noahmp%forcing%WindNorthwardRefHeight,& ! in,    wind speed [m/s] in northward direction at reference height
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight,& ! in,    air temperature [K] at reference height
              PressureAirSurface      => noahmp%forcing%PressureAirSurface ,& ! in,    air pressure [Pa] at surface-atmosphere interface
              ZilitinkevichCoeff            => noahmp%energy%param%ZilitinkevichCoeff            ,& ! in,    Zilitinkevich Coefficient for exchange coefficient calculation
              SnowDepth           => noahmp%water%state%SnowDepth            ,& ! in,    snow depth [m]
              SnowCoverFrac            => noahmp%water%state%SnowCoverFrac             ,& ! in,    snow cover fraction [-]
              RadSwAbsGrd             => noahmp%energy%flux%RadSwAbsGrd              ,& ! in,    solar radiation absorbed by ground (w/m2)
              HeatPrecipAdvBareGrd            => noahmp%energy%flux%HeatPrecipAdvBareGrd             ,& ! in,    precipitation advected heat - bare ground net (W/m2)
              WindSpdRefHeight              => noahmp%energy%state%WindSpdRefHeight              ,& ! in,    wind speed (m/s) at reference height
              PressureVaporRefHeight            => noahmp%energy%state%PressureVaporRefHeight            ,& ! in,    vapor pressure air (pa) at reference height
              SpecHumidityRefHeight => noahmp%forcing%SpecHumidityRefHeight,& ! in,    specific humidity (kg/kg) at reference height
              DensityAirRefHeight          => noahmp%energy%state%DensityAirRefHeight          ,& ! in,    density air (kg/m3)
              RelHumidityGrd           => noahmp%energy%state%RelHumidityGrd           ,& ! in,    raltive humidity in surface soil/snow air space (-)
              EmissivityGrd             => noahmp%energy%state%EmissivityGrd             ,& ! in,    ground emissivity
              TemperatureSoilSnow             => noahmp%energy%state%TemperatureSoilSnow             ,& ! in,    snow and soil layer temperature [k]
              ThermConductSoilSnow              => noahmp%energy%state%ThermConductSoilSnow              ,& ! in,    thermal conductivity [w/m/k] for all soil & snow
              ResistanceGrdEvap           => noahmp%energy%state%ResistanceGrdEvap           ,& ! in,    ground surface resistance (s/m) to evaporation
              RoughLenMomGrd             => noahmp%energy%state%RoughLenMomGrd            ,& ! in,    roughness length, momentum, ground (m)
              LatHeatVapGrd         => noahmp%energy%state%LatHeatVapGrd         ,& ! in,    latent heat of vaporization/subli (j/kg), ground
              PsychConstGrd          => noahmp%energy%state%PsychConstGrd          ,& ! in,    psychrometric constant (pa/K), ground
              SpecHumiditySfcBare            => noahmp%energy%state%SpecHumiditySfcBare            ,& ! inout, specific humidity (kg/kg) at bare surface
              TemperatureGrdBare             => noahmp%energy%state%TemperatureGrdBare             ,& ! inout, bare ground temperature (K)
              ExchCoeffMomBare              => noahmp%energy%state%ExchCoeffMomBare             ,& ! inout, momentum exchange coefficient (m/s), above ZeroPlaneDisp, bare ground
              ExchCoeffShBare              => noahmp%energy%state%ExchCoeffShBare             ,& ! inout, heat exchange coefficient (m/s), above ZeroPlaneDisp, bare ground
              WindStressEwBare           => noahmp%energy%state%WindStressEwBare           ,& ! out,   wind stress: east-west (n/m2) bare ground
              WindStressNsBare           => noahmp%energy%state%WindStressNsBare           ,& ! out,   wind stress: north-south (n/m2) bare ground
              TemperatureAir2mBare            => noahmp%energy%state%TemperatureAir2mBare            ,& ! out,   2 m height air temperature (k) bare ground
              SpecHumidity2mBare             => noahmp%energy%state%SpecHumidity2mBare             ,& ! out,   bare ground 2-m specific humidity
              ExchCoeffSh2mBare            => noahmp%energy%state%ExchCoeffSh2mBare            ,& ! out,   bare ground 2-m sensible heat exchange coefficient (m/s)
              FrictionVelBare              => noahmp%energy%state%FrictionVelBare             ,& ! out,   friction velocity (m/s), vegetated
              RoughLenShBareGrd             => noahmp%energy%state%RoughLenShBareGrd            ,& ! out,   roughness length, sensible heat (m), bare ground
              ResistanceLhBareGrd            => noahmp%energy%state%ResistanceLhBareGrd            ,& ! out,   aerodynamic resistance for water vapor (s/m), bare ground
              ResistanceShBareGrd            => noahmp%energy%state%ResistanceShBareGrd            ,& ! out,   aerodynamic resistance for sensible heat (s/m), bare ground
              ResistanceMomBareGrd            => noahmp%energy%state%ResistanceMomBareGrd            ,& ! out,   aerodynamic resistance for momentum (s/m), bare ground
              VapPresSatGrdBare            => noahmp%energy%state%VapPresSatGrdBare            ,& ! out,   bare ground saturation vapor pressure at TemperatureGrd (pa)
              VapPresSatGrdBareTempD           => noahmp%energy%state%VapPresSatGrdBareTempD           ,& ! out,   bare ground d(VapPresSatGrdBare)/dt at TemperatureGrd (pa/k)
              MoStabParaBare             => noahmp%energy%state%MoStabParaBare            ,& ! out,   Monin-Obukhov stability (z/L), above ZeroPlaneDisp, bare ground
              MoStabCorrShBare2m             => noahmp%energy%state%MoStabCorrShBare2m            ,& ! out,   M-O sen heat stability correction, 2m, bare ground
              RadLwNetBareGrd             => noahmp%energy%flux%RadLwNetBareGrd              ,& ! out,   net longwave rad (w/m2) bare ground [+ to atm]
              HeatSensibleBareGrd             => noahmp%energy%flux%HeatSensibleBareGrd              ,& ! out,   sensible heat flux (w/m2) bare ground [+ to atm]
              HeatLatentBareGrd             => noahmp%energy%flux%HeatLatentBareGrd              ,& ! out,   latent heat flux (w/m2) bare ground [+ to atm]
              HeatGroundBareGrd             => noahmp%energy%flux%HeatGroundBareGrd               & ! out,   bare ground heat flux (w/m2) [+ to soil/snow]
             )
! ----------------------------------------------------------------------

    ! initialization (including variables that do not depend on stability iteration)
    DTG    = 0.0
    MoStabParaBare    = 0.0
    MOZSGN = 0
    MoStabCorrShBare2m    = 0.0
    H      = 0.0
    MoistureFluxSfc    = 0.0
    FrictionVelBare     = 0.1
    CIR    = EmissivityGrd * ConstStefanBoltzmann
    CGH    = 2.0 * ThermConductSoilSnow(NumSnowLayerNeg+1) / ThicknessSnowSoilLayer(NumSnowLayerNeg+1)

    ! begin stability iteration for ground temperature and flux
    loop3: do ITER = 1, NITERB

       ! ground roughness length
       if ( ITER == 1 ) then
          RoughLenShBareGrd = RoughLenMomGrd
       else
          RoughLenShBareGrd = RoughLenMomGrd !* exp(-ZilitinkevichCoeff * 0.4 * 258.2 * sqrt(FrictionVelBare*RoughLenMomGrd))
       endif

       ! aerodyn resistances between reference heigths and d+z0v
       if ( OptSurfaceDrag == 1 ) call ResistanceBareGroundMOST(noahmp, ITER, H, MOZSGN)
       if ( OptSurfaceDrag == 2 ) call ResistanceBareGroundChen97(noahmp, ITER)

       ! conductance variables for diagnostics         
       EMB = 1.0 / ResistanceMomBareGrd
       EHB = 1.0 / ResistanceShBareGrd

       ! ES and d(ES)/dt evaluated at ground temperatue
       T = TDC(TemperatureGrdBare)
       call VaporPressureSaturation(T, ESATW, ESATI, DSATW, DSATI)
       if ( T > 0.0 ) then
          VapPresSatGrdBare  = ESATW
          VapPresSatGrdBareTempD = DSATW
       else
          VapPresSatGrdBare  = ESATI
          VapPresSatGrdBareTempD = DSATI
       endif

       ! ground fluxes and temperature change
       CSH = DensityAirRefHeight * ConstHeatCapacAir / ResistanceShBareGrd
       CEV = DensityAirRefHeight * ConstHeatCapacAir / PsychConstGrd / (ResistanceGrdEvap + ResistanceLhBareGrd)
       RadLwNetBareGrd = CIR * TemperatureGrdBare**4 - EmissivityGrd * RadLwDownRefHeight
       HeatSensibleBareGrd = CSH * (TemperatureGrdBare        - TemperatureAirRefHeight )
       HeatLatentBareGrd = CEV * (VapPresSatGrdBare*RelHumidityGrd - PressureVaporRefHeight        )
       HeatGroundBareGrd = CGH * (TemperatureGrdBare        - TemperatureSoilSnow(NumSnowLayerNeg+1))
       B   = RadSwAbsGrd - RadLwNetBareGrd - HeatSensibleBareGrd - HeatLatentBareGrd - HeatGroundBareGrd + HeatPrecipAdvBareGrd
       A   = 4.0*CIR*TemperatureGrdBare**3 + CSH + CEV*VapPresSatGrdBareTempD + CGH
       DTG = B / A
       RadLwNetBareGrd = RadLwNetBareGrd + 4.0 * CIR * TemperatureGrdBare**3 * DTG
       HeatSensibleBareGrd = HeatSensibleBareGrd + CSH * DTG
       HeatLatentBareGrd = HeatLatentBareGrd + CEV * VapPresSatGrdBareTempD * DTG
       HeatGroundBareGrd = HeatGroundBareGrd + CGH * DTG
       TemperatureGrdBare = TemperatureGrdBare + DTG  ! update ground temperature

       ! for computing M-O length
       H = CSH * (TemperatureGrdBare - TemperatureAirRefHeight)

       ! update specific humidity
       T = TDC(TemperatureGrdBare)
       call VaporPressureSaturation(T, ESATW, ESATI, DSATW, DSATI)
       if ( T > 0.0 ) then
          VapPresSatGrdBare = ESATW
       else
          VapPresSatGrdBare = ESATI
       endif
       SpecHumiditySfcBare = 0.622 * (VapPresSatGrdBare*RelHumidityGrd) / (PressureAirSurface - 0.378 * (VapPresSatGrdBare*RelHumidityGrd))
       MoistureFluxSfc  = (SpecHumiditySfcBare - SpecHumidityRefHeight) * CEV * PsychConstGrd / ConstHeatCapacAir

    enddo loop3 ! end stability iteration

    ! if snow on ground and TemperatureGrdBare > freezing point: reset TemperatureGrdBare = freezing point. reevaluate ground fluxes.
    if ( (OptSnowSoilTempTime == 1) .or. (OptSnowSoilTempTime == 3) ) then
       if ( (SnowDepth > 0.05) .and. (TemperatureGrdBare > ConstFreezePoint) ) then
          if ( OptSnowSoilTempTime == 1 ) TemperatureGrdBare = ConstFreezePoint
          if ( OptSnowSoilTempTime == 3 ) TemperatureGrdBare = (1.0 - SnowCoverFrac) * TemperatureGrdBare + SnowCoverFrac * ConstFreezePoint  ! MB: allow TemperatureGrd>0C during melt v3.7
          RadLwNetBareGrd = CIR * TemperatureGrdBare**4 - EmissivityGrd * RadLwDownRefHeight
          HeatSensibleBareGrd = CSH * (TemperatureGrdBare        - TemperatureAirRefHeight)
          HeatLatentBareGrd = CEV * (VapPresSatGrdBare*RelHumidityGrd - PressureVaporRefHeight  )          !VapPresSatGrdBare reevaluate ?
          HeatGroundBareGrd = RadSwAbsGrd + HeatPrecipAdvBareGrd - (RadLwNetBareGrd + HeatSensibleBareGrd + HeatLatentBareGrd)
       endif
    endif

    ! wind stresses
    WindStressEwBare = -DensityAirRefHeight * ExchCoeffMomBare * WindSpdRefHeight * WindEastwardRefHeight
    WindStressNsBare = -DensityAirRefHeight * ExchCoeffMomBare * WindSpdRefHeight * WindNorthwardRefHeight

    ! 2m air temperature
    if ( (OptSurfaceDrag == 1) .or. (OptSurfaceDrag == 2) ) then
       !ExchCoeffSh2mBare = FrictionVelBare * ConstVonKarman / LOG((2.0+RoughLenShBareGrd)/RoughLenShBareGrd)
       ExchCoeffSh2mBare = FrictionVelBare * ConstVonKarman / ( log((2.0+RoughLenShBareGrd)/RoughLenShBareGrd) - MoStabCorrShBare2m )
       CQ2B = ExchCoeffSh2mBare
       if ( ExchCoeffSh2mBare < 1.0e-5 ) then
          TemperatureAir2mBare = TemperatureGrdBare
          SpecHumidity2mBare  = SpecHumiditySfcBare
       else
          TemperatureAir2mBare = TemperatureGrdBare - HeatSensibleBareGrd / (DensityAirRefHeight*ConstHeatCapacAir) * 1.0 / ExchCoeffSh2mBare
          SpecHumidity2mBare  = SpecHumiditySfcBare - HeatLatentBareGrd / (LatHeatVapGrd*DensityAirRefHeight) * (1.0/CQ2B + ResistanceGrdEvap)
       endif
       if ( FlagUrban .eqv. .true. ) SpecHumidity2mBare = SpecHumiditySfcBare
    endif

    ! update ExchCoeffShBare 
    ExchCoeffShBare = EHB

    end associate

  end subroutine SurfaceEnergyFluxBareGround

end module SurfaceEnergyFluxBareGroundMod
