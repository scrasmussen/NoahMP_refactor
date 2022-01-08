module SurfaceEnergyFluxBareGroundMod

!!! Compute surface energy fluxes and budget for bare ground
!!! Use newton-raphson iteration to solve for ground (tg) temperatures
!!! Surface energy balance (bare soil):
!!! Ground level: -SAB + IRB[TG] + SHB[TG] + EVB[TG] + GHB[TG] = 0

  use Machine, only : kind_noahmp
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
    integer                               :: MOZSGN       ! number of times MOZ changes sign
    integer, parameter                    :: NITERB = 5   ! number of iterations for surface temperature
    real(kind=kind_noahmp)                :: DTG          ! change in tg, last iteration (k)
    real(kind=kind_noahmp)                :: CIR          ! coefficients for ir as function of ts**4
    real(kind=kind_noahmp)                :: CSH          ! coefficients for sh as function of ts
    real(kind=kind_noahmp)                :: CEV          ! coefficients for ev as function of esat[ts]
    real(kind=kind_noahmp)                :: CGH          ! coefficients for st as function of ts
    real(kind=kind_noahmp)                :: CQ2B         ! exchange coefficient for 2m temp. 
    real(kind=kind_noahmp)                :: QFX          ! moisture flux
    real(kind=kind_noahmp)                :: ESATW        ! es for water
    real(kind=kind_noahmp)                :: ESATI        ! es for ice
    real(kind=kind_noahmp)                :: DSATW        ! d(es)/dt at tg (pa/k) for water
    real(kind=kind_noahmp)                :: DSATI        ! d(es)/dt at tg (pa/k) for ice
    real(kind=kind_noahmp)                :: A            ! temporary calculation
    real(kind=kind_noahmp)                :: B            ! temporary calculation
    real(kind=kind_noahmp)                :: H            ! temporary sensible heat flux (w/m2)
    real(kind=kind_noahmp)                :: T, TDC       ! Kelvin to degree Celsius with limit -50 to +50
! local statement function
    TDC(T) = min( 50.0, max(-50.0, (T - TFRZ)) )

! --------------------------------------------------------------------
    associate(                                                        &
              DT              => noahmp%config%domain%DT             ,& ! in,    main noahmp timestep (s)
              ISNOW           => noahmp%config%domain%ISNOW          ,& ! in,    actual number of snow layers
              DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! in,    thickness of snow/soil layers (m)
              URBAN_FLAG      => noahmp%config%domain%URBAN_FLAG     ,& ! in,     logical flag for urban grid
              OPT_SFC         => noahmp%config%nmlist%OPT_SFC        ,& ! in,    options for surface layer drag coeff (CH & CM)
              OPT_STC         => noahmp%config%nmlist%OPT_STC        ,& ! in,    options for snow/soil temperature time scheme (only layer 1)
              LWDN            => noahmp%forcing%LWDN                 ,& ! in,    downward longwave radiation [w/m2]
              UU              => noahmp%forcing%UU                   ,& ! in,    wind speed in eastward direction (m/s)
              VV              => noahmp%forcing%VV                   ,& ! in,    wind speed in northward direction (m/s)
              SFCTMP          => noahmp%forcing%SFCTMP               ,& ! in,    surface air temperature [k] from Atmos forcing
              PSFC            => noahmp%forcing%PSFC                 ,& ! in,    pressure at lowest model layer
              SFCPRS          => noahmp%forcing%SFCPRS               ,& ! in,    surface air pressure at reference height (pa)
              SNOWH           => noahmp%water%state%SNOWH            ,& ! in,    snow depth [m]
              FSNO            => noahmp%water%state%FSNO             ,& ! in,    snow cover fraction (-)
              SAG             => noahmp%energy%flux%SAG              ,& ! in,    solar radiation absorbed by ground (w/m2)
              PAHB            => noahmp%energy%flux%PAHB             ,& ! in,    precipitation advected heat - bare ground net (W/m2)
              UR              => noahmp%energy%state%UR              ,& ! in,    wind speed (m/s) at reference height ZLVL
              THAIR           => noahmp%energy%state%THAIR           ,& ! in,    potential temp at reference height (k)           
              EAIR            => noahmp%energy%state%EAIR            ,& ! in,    vapor pressure air (pa) at zlvl
              QAIR            => noahmp%energy%state%QAIR            ,& ! in,    specific humidity at reference height (kg/kg)
              RHOAIR          => noahmp%energy%state%RHOAIR          ,& ! in,    density air (kg/m3)
              RHSUR           => noahmp%energy%state%RHSUR           ,& ! in,    raltive humidity in surface soil/snow air space (-)
              EMG             => noahmp%energy%state%EMG             ,& ! in,    ground emissivity
              STC             => noahmp%energy%state%STC             ,& ! in,    snow and soil layer temperature [k]
              DF              => noahmp%energy%state%DF              ,& ! in,    thermal conductivity [w/m/k] for all soil & snow
              RSURF           => noahmp%energy%state%RSURF           ,& ! in,    ground surface resistance (s/m)
              Z0M             => noahmp%energy%state%Z0MG            ,& ! in,    roughness length, momentum, ground (m)
              LATHEAG         => noahmp%energy%state%LATHEAG         ,& ! in,    latent heat of vaporization/subli (j/kg), ground
              GAMMAG          => noahmp%energy%state%GAMMAG          ,& ! in,    psychrometric constant (pa/K), ground
              QSFC            => noahmp%energy%state%QSFC            ,& ! inout, water vapor mixing ratio at lowest model layer
              TGB             => noahmp%energy%state%TGB             ,& ! inout, bare ground temperature (K)
              CM              => noahmp%energy%state%CMB             ,& ! inout, momentum exchange coefficient (m/s), above ZPD, bare ground
              CH              => noahmp%energy%state%CHB             ,& ! inout, heat exchange coefficient (m/s), above ZPD, bare ground
              TAUXB           => noahmp%energy%state%TAUXB           ,& ! out,   wind stress: east-west (n/m2) bare ground
              TAUYB           => noahmp%energy%state%TAUYB           ,& ! out,   wind stress: north-south (n/m2) bare ground
              T2MB            => noahmp%energy%state%T2MB            ,& ! out,   2 m height air temperature (k) bare ground
              Q2B             => noahmp%energy%state%Q2B             ,& ! out,   bare ground 2-m water vapor mixing ratio
              EHB             => noahmp%energy%state%EHB             ,& ! out,   bare ground sensible heat exchange coefficient (m/s)
              EHB2            => noahmp%energy%state%EHB2            ,& ! out,   bare ground 2-m sensible heat exchange coefficient (m/s)
              EMB             => noahmp%energy%state%EMB             ,& ! out,   bare ground momentum exchange coefficient (m/s)
              FV              => noahmp%energy%state%FVB             ,& ! out,   friction velocity (m/s), vegetated
              Z0H             => noahmp%energy%state%Z0HB            ,& ! out,   roughness length, sensible heat (m), vegetated
              RAWB            => noahmp%energy%state%RAWB            ,& ! out,   aerodynamic resistance for water vapor (s/m), bare ground
              RAHB            => noahmp%energy%state%RAHB            ,& ! out,   aerodynamic resistance for sensible heat (s/m), bare ground
              RAMB            => noahmp%energy%state%RAMB            ,& ! out,   aerodynamic resistance for momentum (s/m), bare ground
              ESTG            => noahmp%energy%state%ESTB            ,& ! out,   bare ground saturation vapor pressure at TG (pa)
              DESTG           => noahmp%energy%state%DESTB           ,& ! out,   bare ground d(ESTB)/dt at TG (pa/k)
              MOZ             => noahmp%energy%state%MOZB            ,& ! out,   Monin-Obukhov stability (z/L), above ZPD, bare ground
              FH2             => noahmp%energy%state%FH2B            ,& ! out,   M-O sen heat stability correction, 2m, bare ground
              IRB             => noahmp%energy%flux%IRB              ,& ! out,   net longwave rad (w/m2) bare ground [+ to atm]
              SHB             => noahmp%energy%flux%SHB              ,& ! out,   sensible heat flux (w/m2) bare ground [+ to atm]
              EVB             => noahmp%energy%flux%EVB              ,& ! out,   latent heat flux (w/m2) bare ground [+ to atm]
              GHB             => noahmp%energy%flux%GHB               & ! out,   bare ground heat flux (w/m2) [+ to soil]
             )
! ----------------------------------------------------------------------

    ! initialization (including variables that do not depend on stability iteration)
    DTG    = 0.0
    MOZ    = 0.0
    MOZSGN = 0
    FH2    = 0.0
    H      = 0.0
    QFX    = 0.0
    FV     = 0.1
    CIR    = EMG * SB
    CGH    = 2.0 * DF(ISNOW+1) / DZSNSO(ISNOW+1)

    ! begin stability iteration for ground temperature and flux
    loop3: do ITER = 1, NITERB

       ! ground roughness length
       if ( ITER == 1 ) then
          Z0H = Z0M
       else
          Z0H = Z0M !* exp(-CZIL * 0.4 * 258.2 * sqrt(FV*Z0M))
       endif

       ! aerodyn resistances between heights zlvl and d+z0v
       if ( OPT_SFC == 1 ) call ResistanceBareGroundMOST(noahmp, ITER, H, MOZSGN)
       if ( OPT_SFC == 2 ) call ResistanceBareGroundChen97(noahmp, ITER)

       ! conductance variables for diagnostics         
       EMB = 1.0 / RAMB
       EHB = 1.0 / RAHB

       ! ES and d(ES)/dt evaluated at TG
       T = TDC(TGB)
       call VaporPressureSaturation(T, ESATW, ESATI, DSATW, DSATI)
       if ( T > 0.0 ) then
          ESTG  = ESATW
          DESTG = DSATW
       else
          ESTG  = ESATI
          DESTG = DSATI
       endif

       ! ground fluxes and temperature change
       CSH = RHOAIR * CPAIR / RAHB
       CEV = RHOAIR * CPAIR / GAMMAG / (RSURF + RAWB)
       IRB = CIR * TGB**4 - EMG * LWDN
       SHB = CSH * (TGB        - SFCTMP      )
       EVB = CEV * (ESTG*RHSUR - EAIR        )
       GHB = CGH * (TGB        - STC(ISNOW+1))
       B   = SAG - IRB - SHB - EVB - GHB + PAHB
       A   = 4.0*CIR*TGB**3 + CSH + CEV*DESTG + CGH
       DTG = B / A
       IRB = IRB + 4.0 * CIR * TGB**3 * DTG
       SHB = SHB + CSH * DTG
       EVB = EVB + CEV * DESTG * DTG
       GHB = GHB + CGH * DTG
       TGB = TGB + DTG  ! update ground temperature

       ! for computing M-O length
       H = CSH * (TGB - SFCTMP)

       ! update specific humidity
       T = TDC(TGB)
       call VaporPressureSaturation(T, ESATW, ESATI, DSATW, DSATI)
       if ( T > 0.0 ) then
          ESTG = ESATW
       else
          ESTG = ESATI
       endif
       QSFC = 0.622 * (ESTG*RHSUR) / (PSFC - 0.378 * (ESTG*RHSUR))
       QFX  = (QSFC - QAIR) * CEV * GAMMAG / CPAIR

    enddo loop3 ! end stability iteration

    ! if snow on ground and TGB > TFRZ: reset TGB = TFRZ. reevaluate ground fluxes.
    if ( (OPT_STC == 1) .or. (OPT_STC == 3) ) then
       if ( (SNOWH > 0.05) .and. (TGB > TFRZ) ) then
          if ( OPT_STC == 1 ) TGB = TFRZ
          if ( OPT_STC == 3 ) TGB = (1.0 - FSNO) * TGB + FSNO * TFRZ  ! MB: allow TG>0C during melt v3.7
          IRB = CIR * TGB**4 - EMG * LWDN
          SHB = CSH * (TGB        - SFCTMP)
          EVB = CEV * (ESTG*RHSUR - EAIR  )          !ESTG reevaluate ?
          GHB = SAG + PAHB - (IRB + SHB + EVB)
       endif
    endif

    ! wind stresses
    TAUXB = -RHOAIR * CM * UR * UU
    TAUYB = -RHOAIR * CM * UR * VV

    ! 2m air temperature
    if ( (OPT_SFC == 1) .or. (OPT_SFC == 2) ) then
       !EHB2 = FV * VKC / LOG((2.0+Z0H)/Z0H)
       EHB2 = FV * VKC / ( log((2.0+Z0H)/Z0H) - FH2 )
       CQ2B = EHB2
       if ( EHB2 < 1.0e-5 ) then
          T2MB = TGB
          Q2B  = QSFC
       else
          T2MB = TGB - SHB / (RHOAIR*CPAIR) * 1.0 / EHB2
          Q2B  = QSFC - EVB / (LATHEAG*RHOAIR) * (1.0/CQ2B + RSURF)
       endif
       if ( URBAN_FLAG .eqv. .true. ) Q2B = QSFC
    endif

    ! update CH 
    CH = EHB

    end associate

  end subroutine SurfaceEnergyFluxBareGround

end module SurfaceEnergyFluxBareGroundMod
