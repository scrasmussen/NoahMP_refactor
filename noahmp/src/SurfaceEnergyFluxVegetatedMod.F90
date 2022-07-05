module SurfaceEnergyFluxVegetatedMod

!!! Compute surface energy fluxes and budget for vegetated surface
!!! Use newton-raphson iteration to solve for vegetation (tv) and ground (tg) temperatures
!!! Surface energy balance:
!!! Canopy level: -SAV + IRC[TV] + SHC[TV] + EVC[TV] + TR[TV] = 0
!!! Ground level: -SAG + IRG[TG] + SHG[TG] + EVG[TG] + GH[TG] = 0

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
    integer                               :: MOZSGN       ! number of times MOZ changes sign
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
    real(kind=kind_noahmp)                :: QFX          ! moisture flux
    real(kind=kind_noahmp)                :: VAIE         ! total leaf area index + stem area index,effective
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
              RadLWDownRefHeight      => noahmp%forcing%RadLWDownRefHeight      ,& ! in,    downward longwave radiation [W/m2] at reference height
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight ,& ! in,    air temperature [K] at reference height
              PressureAirRefHeight    => noahmp%forcing%PressureAirRefHeight    ,& ! in,    air pressure [Pa] at reference height
              PressureAirSurface      => noahmp%forcing%PressureAirSurface      ,& ! in,    air pressure [Pa] at surface-atmos interface
              SNOWH           => noahmp%water%state%SNOWH            ,& ! in,    snow depth [m]
              FSNO            => noahmp%water%state%FSNO             ,& ! in,    snow cover fraction (-)
              FWET            => noahmp%water%state%FWET             ,& ! in,    wetted or snowed fraction of the canopy
              CANLIQ          => noahmp%water%state%CANLIQ           ,& ! in,    canopy intercepted liquid water (mm)
              CANICE          => noahmp%water%state%CANICE           ,& ! in,    canopy intercepted ice mass (mm)
              HVT             => noahmp%energy%param%HVT             ,& ! in,    top of canopy (m)
              SAV             => noahmp%energy%flux%SAV              ,& ! in,    solar radiation absorbed by vegetation (w/m2)
              SAG             => noahmp%energy%flux%SAG              ,& ! in,    solar radiation absorbed by ground (w/m2)
              PAHV            => noahmp%energy%flux%PAHV             ,& ! in,    precipitation advected heat - vegetation net (W/m2)
              PAHG            => noahmp%energy%flux%PAHG             ,& ! in,    precipitation advected heat - under canopy net (W/m2)
              RefHeightAboveGround            => noahmp%energy%state%RefHeightAboveGround            ,& ! in,    surface reference height (m)
              FVEG            => noahmp%energy%state%FVEG            ,& ! in,    greeness vegetation fraction (-)
              UR              => noahmp%energy%state%UR              ,& ! in,    wind speed (m/s) at reference height
              THAIR           => noahmp%energy%state%THAIR           ,& ! in,    potential temp at reference height (k)           
              EAIR            => noahmp%energy%state%EAIR            ,& ! in,    vapor pressure air (pa) at reference height
              SpecHumidityRefHeight => noahmp%forcing%SpecHumidityRefHeight,& ! in,    specific humidity (kg/kg) at reference height
              RHOAIR          => noahmp%energy%state%RHOAIR          ,& ! in,    density air (kg/m3)
              VAI             => noahmp%energy%state%VAI             ,& ! in,    one-sided leaf+stem area index (m2/m2)
              LAISUN          => noahmp%energy%state%LAISUN          ,& ! in,    sunlit leaf area index, one-sided (m2/m2)
              LAISHA          => noahmp%energy%state%LAISHA          ,& ! in,    shaded leaf area index, one-sided (m2/m2)
              ZPD             => noahmp%energy%state%ZPD             ,& ! in,    zero plane displacement (m)
              Z0M             => noahmp%energy%state%Z0M             ,& ! in,    roughness length, momentum, (m), surface
              Z0MG            => noahmp%energy%state%Z0MG            ,& ! in,    roughness length, momentum, ground (m)
              EMV             => noahmp%energy%state%EMV             ,& ! in,    vegetation emissivity
              EMG             => noahmp%energy%state%EMG             ,& ! in,    ground emissivity
              STC             => noahmp%energy%state%STC             ,& ! in,    snow and soil layer temperature [k]
              DF              => noahmp%energy%state%DF              ,& ! in,    thermal conductivity [w/m/k] for all soil & snow
              RSURF           => noahmp%energy%state%RSURF           ,& ! in,    ground surface resistance (s/m)
              GAMMAV          => noahmp%energy%state%GAMMAV          ,& ! in,    psychrometric constant (pa/K), canopy
              LATHEAV         => noahmp%energy%state%LATHEAV         ,& ! in,    latent heat of vaporization/subli (j/kg), canopy
              GAMMAG          => noahmp%energy%state%GAMMAG          ,& ! in,    psychrometric constant (pa/K), ground
              LATHEAG         => noahmp%energy%state%LATHEAG         ,& ! in,    latent heat of vaporization/subli (j/kg), ground
              RHSUR           => noahmp%energy%state%RHSUR           ,& ! in,    raltive humidity in surface soil/snow air space (-)
              QSFC            => noahmp%energy%state%QSFC            ,& ! inout, water vapor mixing ratio at lowest model layer
              EAH             => noahmp%energy%state%EAH             ,& ! inout, canopy air vapor pressure (pa)
              TAH             => noahmp%energy%state%TAH             ,& ! inout, canopy air temperature (K)
              TV              => noahmp%energy%state%TV              ,& ! inout, vegetation temperature (K)
              TGV             => noahmp%energy%state%TGV             ,& ! inout, vegetated ground (below-canopy) temperature (K)
              CM              => noahmp%energy%state%CMV             ,& ! inout, momentum exchange coefficient (m/s), above ZPD, vegetated
              CH              => noahmp%energy%state%CHV             ,& ! inout, heat exchange coefficient (m/s), above ZPD, vegetated
              TAUXV           => noahmp%energy%state%TAUXV           ,& ! out,   wind stress: east-west (n/m2) above canopy
              TAUYV           => noahmp%energy%state%TAUYV           ,& ! out,   wind stress: north-south (n/m2) above canopy
              T2MV            => noahmp%energy%state%T2MV            ,& ! out,   2 m height air temperature (k), vegetated
              CHLEAF          => noahmp%energy%state%CHLEAF          ,& ! out,   leaf sensible heat exchange coefficient (m/s),leaf surface to canopy air
              CHUC            => noahmp%energy%state%CHUC            ,& ! out,   under canopy sensible heat exchange coefficient (m/s)
              CAH2            => noahmp%energy%state%CHV2            ,& ! out,   2m sensible heat exchange coefficient (m/s)
              Q2V             => noahmp%energy%state%Q2V             ,& ! out,   water vapor mixing ratio at 2m vegetated
              RSSUN           => noahmp%energy%state%RSSUN           ,& ! out,   sunlit leaf stomatal resistance (s/m)
              RSSHA           => noahmp%energy%state%RSSHA           ,& ! out,   shaded leaf stomatal resistance (s/m)
              FV              => noahmp%energy%state%FVV             ,& ! out,   friction velocity (m/s), vegetated
              Z0H             => noahmp%energy%state%Z0HV            ,& ! out,   roughness length, sensible heat (m), vegetated
              Z0HG            => noahmp%energy%state%Z0HG            ,& ! out,   roughness length, sensible heat ground (m), below canopy
              RB              => noahmp%energy%state%RB              ,& ! out,   bulk leaf boundary layer resistance (s/m)
              RAHC            => noahmp%energy%state%RAHC            ,& ! out,   aerodynamic resistance for sensible heat (s/m), above canopy
              RAWC            => noahmp%energy%state%RAWC            ,& ! out,   aerodynamic resistance for water vapor (s/m), above canopy
              RAHG            => noahmp%energy%state%RAHG            ,& ! out,   ground aerodynamic resistance for sensible heat (s/m)
              RAWG            => noahmp%energy%state%RAWG            ,& ! out,   ground aerodynamic resistance for water vapor (s/m)
              CAW             => noahmp%energy%state%CAW             ,& ! out,   latent heat conductance, canopy air to reference height air (m/s)
              CTW             => noahmp%energy%state%CTW             ,& ! out,   transpiration conductance, leaf to canopy air (m/s)
              CEW             => noahmp%energy%state%CEW             ,& ! out,   evaporation conductance, leaf to canopy air (m/s)
              CGW             => noahmp%energy%state%CGW             ,& ! out,   latent heat conductance, ground to canopy air (m/s)
              ESTV            => noahmp%energy%state%ESTV            ,& ! out,   saturation vapor pressure at TV (pa)
              ESTG            => noahmp%energy%state%ESTG            ,& ! out,   saturation vapor pressure at TG (pa)
              DESTV           => noahmp%energy%state%DESTV           ,& ! out,   d(ESTV)/dt at TV (pa/k)
              DESTG           => noahmp%energy%state%DESTG           ,& ! out,   d(ESTG)/dt at TG (pa/k)
              HCAN            => noahmp%energy%state%HCAN            ,& ! out,   canopy height (m) [note: hcan >= z0mg]
              UC              => noahmp%energy%state%UC              ,& ! out,   wind speed at top of canopy (m/s)
              MOZ             => noahmp%energy%state%MOZV            ,& ! out,   Monin-Obukhov stability (z/L), above ZPD, vegetated
              FH2             => noahmp%energy%state%FH2V            ,& ! out,   M-O sen heat stability correction, 2m, vegetated
              IRC             => noahmp%energy%flux%IRC              ,& ! out,   canopy net longwave radiation (w/m2) [+= to atm]
              SHC             => noahmp%energy%flux%SHC              ,& ! out,   canopy sensible heat flux (w/m2)     [+= to atm]
              EVC             => noahmp%energy%flux%EVC              ,& ! out,   canopy evaporation heat flux (w/m2)  [+= to atm]
              IRG             => noahmp%energy%flux%IRG              ,& ! out,   ground net longwave radiation (w/m2) [+= to atm]
              SHG             => noahmp%energy%flux%SHG              ,& ! out,   ground sensible heat flux (w/m2)     [+= to atm]
              EVG             => noahmp%energy%flux%EVG              ,& ! out,   ground evaporation heat flux (w/m2)  [+= to atm]
              TR              => noahmp%energy%flux%TR               ,& ! out,   canopy transpiration heat flux (w/m2)[+= to atm]
              GH              => noahmp%energy%flux%GHV               & ! out,   vegetated ground heat (w/m2) [+ = to soil]
             )
! ----------------------------------------------------------------------

    ! initialization (including variables that do not depend on stability iteration)
    LITER   = 0
    FV      = 0.1
    DTV     = 0.0
    DTG     = 0.0
    MOZ     = 0.0
    MOZSGN  = 0
    FH2     = 0.0
    HG      = 0.0
    H       = 0.0
    QFX     = 0.0
    ! limit LAI
    VAIE    = min( 6.0, VAI    )
    LAISUNE = min( 6.0, LAISUN )
    LAISHAE = min( 6.0, LAISHA )

    ! saturation vapor pressure at ground temperature
    T = TDC(TGV)
    call VaporPressureSaturation(T, ESATW, ESATI, DSATW, DSATI)
    if ( T > 0.0 ) then
       ESTG = ESATW
    else
       ESTG = ESATI
    endif
    !jref - consistent surface specific humidity for sfcdif3 and sfcdif4
    QSFC = 0.622 * EAIR / (PressureAirSurface - 0.378*EAIR)

    ! canopy height
    HCAN = HVT
    ! wind speed at canopy height
    !UC = UR * log(HCAN/Z0M) / log(RefHeightAboveGround/Z0M)
    UC = UR * log( (HCAN - ZPD + Z0M)/Z0M ) / log(RefHeightAboveGround/Z0M)   ! MB: add ZPD v3.7
    if ( (HCAN-ZPD) <= 0.0 ) then
       print*, 'CRITICAL PROBLEM: HCAN <= ZPD'
       print*, 'GridIndexI,GridIndexJ =',GridIndexI, GridIndexJ
       print*, 'HCAN  =',HCAN
       print*, 'ZPD   =',ZPD
       print*, 'SNOWH =',SNOWH
       stop 'error'
    endif

    ! prepare for longwave rad.
    AIR = -EMV * (1.0 + (1.0-EMV)*(1.0-EMG)) * RadLWDownRefHeight - &
          EMV * EMG * ConstStefanBoltzmann * TGV**4
    CIR = ( 2.0 - EMV * (1.0-EMG) ) * EMV * ConstStefanBoltzmann

    ! begin stability iteration for canopy temperature and flux
    loop1: do ITER = 1, NITERC

       ! ground and surface roughness length
       if ( ITER == 1 ) then
          Z0H  = Z0M
          Z0HG = Z0MG
       else
          Z0H  = Z0M    !* exp(-CZIL * 0.4 * 258.2 * sqrt(FV*Z0M))
          Z0HG = Z0MG   !* exp(-CZIL * 0.4 * 258.2 * sqrt(FV*Z0MG))
       endif

       ! aerodyn resistances between RefHeightAboveGround and d+z0v
       if ( OptSurfaceDrag == 1 ) call ResistanceAboveCanopyMOST(noahmp, ITER, H, MOZSGN)
       if ( OptSurfaceDrag == 2 ) call ResistanceAboveCanopyChen97(noahmp, ITER)

       ! aerodyn resistance between z0g and d+z0v, and leaf boundary layer resistance
       call ResistanceLeafToGround(noahmp, ITER, VAIE, HG)

       ! ES and d(ES)/dt evaluated at TV
       T = TDC(TV)
       call VaporPressureSaturation(T, ESATW, ESATI, DSATW, DSATI)
       if ( T > 0.0 ) then
          ESTV  = ESATW
          DESTV = DSATW
       else
          ESTV  = ESATI
          DESTV = DSATI
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
       CAH  = 1.0 / RAHC
       CVH  = 2.0 * VAIE / RB
       CGH  = 1.0 / RAHG
       COND = CAH + CVH + CGH
       ATA  = (TemperatureAirRefHeight * CAH + TGV * CGH) / COND
       BTA  = CVH / COND
       CSH  = (1.0 - BTA) * RHOAIR * ConstHeatCapacAir * CVH

       ! latent heat conductance and coeff above veg.
       CAW  = 1.0 / RAWC
       CEW  = FWET * VAIE / RB
       CTW  = (1.0 - FWET) * ( LAISUNE/(RB+RSSUN) + LAISHAE/(RB+RSSHA) )
       CGW  = 1.0 / (RAWG + RSURF)
       COND = CAW + CEW + CTW + CGW
       AEA  = ( EAIR*CAW + ESTG*CGW ) / COND
       BEA  = (CEW + CTW) / COND
       CEV  = (1.0 - BEA) * CEW * RHOAIR * ConstHeatCapacAir / GAMMAV   ! Barlage: change to vegetation v3.6
       CTR  = (1.0 - BEA) * CTW * RHOAIR * ConstHeatCapacAir / GAMMAV

       ! evaluate surface fluxes with current temperature and solve for dts
       TAH  = ATA + BTA * TV               ! canopy air T.
       EAH  = AEA + BEA * ESTV             ! canopy air e
       IRC  = FVEG * (AIR + CIR * TV**4)
       SHC  = FVEG * RHOAIR * ConstHeatCapacAir * CVH * (TV - TAH)
       EVC  = FVEG * RHOAIR * ConstHeatCapacAir * CEW * (ESTV - EAH) / GAMMAV ! Barlage: change to v in v3.6
       TR   = FVEG * RHOAIR * ConstHeatCapacAir * CTW * (ESTV - EAH) / GAMMAV
       if ( TV > ConstFreezePoint ) then
          EVC = min( CANLIQ*LATHEAV/MainTimeStep, EVC )    ! Barlage: add if block for canice in v3.6
       else
          EVC = min( CANICE*LATHEAV/MainTimeStep, EVC )
       endif
       B    = SAV - IRC - SHC - EVC - TR + PAHV  ! additional w/m2
       A    = FVEG * ( 4.0*CIR*TV**3 + CSH + (CEV+CTR)*DESTV ) !volumetric heat capacity
       DTV  = B / A
       IRC  = IRC + FVEG * 4.0 * CIR * TV**3 * DTV
       SHC  = SHC + FVEG * CSH * DTV
       EVC  = EVC + FVEG * CEV * DESTV * DTV
       TR   = TR  + FVEG * CTR * DESTV * DTV
       TV   = TV + DTV       ! update vegetation surface temperature
       !TAH = ATA + BTA * TV  ! canopy air T; update here for consistency

       ! for computing M-O length in the next iteration
       H    = RHOAIR * ConstHeatCapacAir * (TAH - TemperatureAirRefHeight) / RAHC
       HG   = RHOAIR * ConstHeatCapacAir * (TGV  - TAH)   / RAHG

       ! consistent specific humidity from canopy air vapor pressure
       QSFC = (0.622 * EAH) / (PressureAirRefHeight - 0.378 * EAH)
       if ( LITER == 1 ) then
          exit loop1
       endif
       if ( (ITER >= 5) .and. (abs(DTV) <= 0.01) .and. (LITER == 0) ) then
          LITER = 1
       endif
    enddo loop1  ! end stability iteration

    ! under-canopy fluxes and ground temperature
    AIR = -EMG * (1.0 - EMV) * RadLWDownRefHeight - EMG * EMV * ConstStefanBoltzmann * TV**4
    CIR = EMG * ConstStefanBoltzmann
    CSH = RHOAIR * ConstHeatCapacAir / RAHG
    CEV = RHOAIR * ConstHeatCapacAir / (GAMMAG * (RAWG+RSURF))  ! Barlage: change to ground v3.6
    CGH = 2.0 * DF(NumSnowLayerNeg+1) / ThicknessSnowSoilLayer(NumSnowLayerNeg+1)
    ! begin stability iteration
    loop2: do ITER = 1, NITERG
       T = TDC(TGV)
       call VaporPressureSaturation(T, ESATW, ESATI, DSATW, DSATI)
       if ( T > 0.0 ) then
          ESTG  = ESATW
          DESTG = DSATW
       else
          ESTG  = ESATI
          DESTG = DSATI
       endif
       IRG = CIR * TGV**4 + AIR
       SHG = CSH * (TGV        - TAH         )
       EVG = CEV * (ESTG*RHSUR - EAH         )
       GH  = CGH * (TGV        - STC(NumSnowLayerNeg+1))
       B   = SAG - IRG - SHG - EVG - GH + PAHG
       A   = 4.0 * CIR * TGV**3 + CSH + CEV*DESTG + CGH
       DTG = B / A
       IRG = IRG + 4.0 * CIR * TGV**3 * DTG
       SHG = SHG + CSH * DTG
       EVG = EVG + CEV * DESTG * DTG
       GH  = GH  + CGH * DTG
       TGV = TGV + DTG
    enddo loop2
    !TAH = (CAH*TemperatureAirRefHeight + CVH*TV + CGH*TGV)/(CAH + CVH + CGH)

    ! if snow on ground and TGV > freezing point: reset TGV = freezing point. reevaluate ground fluxes.
    if ( (OptSnowSoilTempTime == 1) .or. (OptSnowSoilTempTime == 3) ) then
       if ( (SNOWH > 0.05) .and. (TGV > ConstFreezePoint) ) then
          if ( OptSnowSoilTempTime == 1 ) TGV = ConstFreezePoint
          if ( OptSnowSoilTempTime == 3 ) TGV = (1.0 - FSNO) * TGV + FSNO * ConstFreezePoint   ! MB: allow TGV>0C during melt v3.7
          IRG = CIR * TGV**4 - EMG * (1.0-EMV) * RadLWDownRefHeight - EMG * EMV * ConstStefanBoltzmann * TV**4
          SHG = CSH * (TGV        - TAH)
          EVG = CEV * (ESTG*RHSUR - EAH)
          GH  = SAG + PAHG - (IRG + SHG + EVG)
       endif
    endif

    ! wind stresses
    TAUXV = -RHOAIR * CM * UR * WindEastwardRefHeight
    TAUYV = -RHOAIR * CM * UR * WindNorthwardRefHeight

    ! consistent vegetation air temperature and vapor pressure since TGV is not consistent with the TAH/EAH calculation.
    ! TAH = TemperatureAirRefHeight + (SHG+SHC) / (RHOAIR*ConstHeatCapacAir*CAH) 
    ! TAH = TemperatureAirRefHeight + (SHG*FVEG+SHC) / (RHOAIR*ConstHeatCapacAir*CAH) ! ground flux need fveg
    ! EAH = EAIR + (EVC+FVEG*(TR+EVG)) / (RHOAIR*CAW*ConstHeatCapacAir/GAMMAG)
    ! QFX = (QSFC-SpecHumidityRefHeight) * RHOAIR * CAW !*ConstHeatCapacAir/GAMMAG

    ! 2m temperature over vegetation ( corrected for low CQ2V values )
    if ( (OptSurfaceDrag == 1) .or. (OptSurfaceDrag == 2) ) then
       ! CAH2 = FV * 1.0 / ConstVonKarman * log((2.0+Z0H)/Z0H)
       ! CAH2 = FV * ConstVonKarman / log((2.0+Z0H)/Z0H)
       CAH2 = FV * ConstVonKarman / ( log((2.0+Z0H)/Z0H) - FH2 )
       CQ2V = CAH2
       if ( CAH2 < 1.0e-5 ) then
          T2MV = TAH
          !Q2V  = (EAH*0.622/(PressureAirRefHeight - 0.378*EAH))
          Q2V  = QSFC
       else
          T2MV = TAH - (SHG + SHC/FVEG) / (RHOAIR * ConstHeatCapacAir) * 1.0 / CAH2
          !Q2V = (EAH*0.622/(PressureAirRefHeight - 0.378*EAH))- QFX/(RHOAIR*FV)* 1./ConstVonKarman * LOG((2.+Z0H)/Z0H)
          Q2V  = QSFC - ( (EVC+TR)/FVEG + EVG ) / (LATHEAV * RHOAIR) * 1.0 / CQ2V
       endif
    endif

    ! update CH for output
    CH     = CAH
    CHLEAF = CVH
    CHUC   = 1.0 / RAHG

    end associate

  end subroutine SurfaceEnergyFluxVegetated

end module SurfaceEnergyFluxVegetatedMod
