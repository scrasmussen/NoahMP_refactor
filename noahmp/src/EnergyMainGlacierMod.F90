module EnergyMainGlacierMod

!!! Main energy module for glacier points including all energy relevant processes
!!! snow thermal property -> radiation -> ground heat flux -> snow temperature solver -> snow/ice phase change

  use Machine, only : kind_noahmp
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
              SFCPRS          => noahmp%forcing%SFCPRS               ,& ! in,    surface air pressure at reference height (pa)
              LWDN            => noahmp%forcing%LWDN                 ,& ! in,    downward longwave radiation [w/m2]
              UU              => noahmp%forcing%UU                   ,& ! in,    east-west direction wind (m/s)
              VV              => noahmp%forcing%VV                   ,& ! in,    north-south direction wind (m/s)
              OPT_STC         => noahmp%config%nmlist%OPT_STC        ,& ! in,    options for snow/soil temperature time scheme
              SWDOWN          => noahmp%energy%flux%SWDOWN           ,& ! in,    downward solar filtered by sun angle [w/m2]
              PAHB            => noahmp%energy%flux%PAHB             ,& ! in,    precipitation advected heat - bare ground net (W/m2)
              TS              => noahmp%energy%state%TS              ,& ! inout, surface temperature (K)
              TG              => noahmp%energy%state%TG              ,& ! inout, ground temperature (K)
              QSFC            => noahmp%energy%state%QSFC            ,& ! inout, water vapor mixing ratio bare ground
              Q1              => noahmp%energy%state%Q1              ,& ! inout, surface layer water vapor mixing ratio
              CM              => noahmp%energy%state%CM              ,& ! inout, exchange coefficient (m/s) for momentum, surface, grid mean
              CH              => noahmp%energy%state%CH              ,& ! inout, exchange coefficient (m/s) for heat, surface, grid mean
              SNOWH           => noahmp%water%state%SNOWH            ,& ! inout, snow depth [m]
              Z0WRF           => noahmp%energy%state%Z0WRF           ,& ! out,   roughness length, momentum, surface, sent to coupled model
              TAUX            => noahmp%energy%state%TAUX            ,& ! out,   wind stress: east-west (n/m2) grid mean
              TAUY            => noahmp%energy%state%TAUY            ,& ! out,   wind stress: north-south (n/m2) grid mean
              TRAD            => noahmp%energy%state%TRAD            ,& ! out,   radiative temperature (K)
              T2M             => noahmp%energy%state%T2M             ,& ! out,   grid mean 2-m air temperature (K)
              T2MB            => noahmp%energy%state%T2MB            ,& ! out,   2 m height air temperature (k) bare ground
              EMISSI          => noahmp%energy%state%EMISSI          ,& ! out,   surface emissivity
              UR              => noahmp%energy%state%UR              ,& ! out,   wind speed (m/s) at reference height ZLVL
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
              FSR             => noahmp%energy%flux%FSR              ,& ! out,   total reflected solar radiation (w/m2)
              FIRA            => noahmp%energy%flux%FIRA             ,& ! out,   total net LW. rad (w/m2)   [+ to atm]
              FSH             => noahmp%energy%flux%FSH              ,& ! out,   total sensible heat (w/m2) [+ to atm]
              FGEV            => noahmp%energy%flux%FGEV             ,& ! out,   soil evap heat (w/m2) [+ to atm]
              FCEV            => noahmp%energy%flux%FCEV             ,& ! out,   canopy evaporation (w/m2) [+ = to atm]
              SSOIL           => noahmp%energy%flux%SSOIL            ,& ! out,   soil heat flux (w/m2) [+ to soil]
              PAH             => noahmp%energy%flux%PAH              ,& ! out,   precipitation advected heat - total (W/m2)
              FIRE            => noahmp%energy%flux%FIRE             ,& ! out,   emitted outgoing IR (w/m2)
              IRB             => noahmp%energy%flux%IRB              ,& ! out,   net longwave rad (w/m2) bare ground [+ to atm]
              SHB             => noahmp%energy%flux%SHB              ,& ! out,   sensible heat flux (w/m2) bare ground [+ to atm]
              EVB             => noahmp%energy%flux%EVB              ,& ! out,   latent heat flux (w/m2) bare ground [+ to atm]
              GHB             => noahmp%energy%flux%GHB               & ! out,   bare ground heat flux (w/m2) [+ to soil]
             )
! ----------------------------------------------------------------------

    ! wind speed at reference height: ur >= 1
    UR = max( sqrt(UU**2.0 + VV**2.0), 1.0 )

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
    ! Energy balance at glacier (bare) ground: SAG+PAHB=IRB+SHB+EVB+GHB
    TAUX  = TAUXB
    TAUY  = TAUYB
    FIRA  = IRB
    FSH   = SHB
    FGEV  = EVB
    SSOIL = GHB
    TG    = TGB
    T2M   = T2MB
    PAH   = PAHB
    TS    = TG
    CM    = CMB
    CH    = CHB
    Q1    = QSFC
    Q2E   = Q2B
    Z0WRF = Z0MG

    ! emitted longwave radiation and physical check
    FIRE = LWDN + FIRA
    if ( FIRE <= 0.0 ) then
       write(*,*) 'emitted longwave <0; skin T may be wrong due to inconsistent'
       write(*,*) 'LWDN=',LWDN,'FIRA=',FIRA,'SNOWH=',SNOWH
       stop 'error'
       !call wrf_error_fatal("STOP in Noah-MP")
    endif

    ! radiative temperature: subtract from the emitted IR the
    ! reflected portion of the incoming LWDN, so just
    ! considering the IR originating/emitted in the ground system.
    ! Old TRAD calculation not taking into account Emissivity:
    ! TRAD = (FIRE/SB)**0.25
    TRAD = ( (FIRE - (1.0 - EMISSI)*LWDN) / (EMISSI * SB) ) ** 0.25

    ! compute snow and glacier ice temperature
    call GlacierTemperatureMain(noahmp)

    ! adjusting suface temperature based on snow condition
    if ( OPT_STC == 2 ) then
       if ( (SNOWH > 0.05) .and. (TG > TFRZ) ) then
          TGB = TFRZ
          TG = TGB
          TS = TGB
       endif
    endif

    ! Phase change and Energy released or consumed by snow & glacier ice
    call GlacierPhaseChange(noahmp)

    ! update total surface albedo
    if ( SWDOWN > 0.0 ) then
       ALBEDO = FSR / SWDOWN
    else
       ALBEDO = -999.9
    endif

    end associate

  end subroutine EnergyMainGlacier

end module EnergyMainGlacierMod
