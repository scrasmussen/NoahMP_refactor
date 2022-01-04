module EnergyMainMod

!!! Main energy module including all energy relevant processes
!!! soil/snow thermal property -> radiation -> ground/vegtation heat flux -> snow/soil temperature solver -> soil/snow phase change

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod
  use GroundThermalPropertyMod,      only : GroundThermalProperty
  use RadiationMainMod,              only : RadiationMain
  use SurfaceEnergyFluxVegetatedMod, only : SurfaceEnergyFluxVegetated

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
    associate(                                                        &
              IST             => noahmp%config%domain%IST            ,& ! in,    surface type 1-soil; 2-lake
              ZREF            => noahmp%config%domain%ZREF           ,& ! in,    reference height  (m)
              SFCPRS          => noahmp%forcing%SFCPRS               ,& ! in,    surface air pressure at reference height (pa)
              SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,    saturated value of soil moisture [m3/m3]
              PSISAT          => noahmp%water%param%PSISAT           ,& ! in,    saturated soil matric potential (m)
              BEXP            => noahmp%water%param%BEXP             ,& ! in,    soil B parameter
              SNOWH           => noahmp%water%state%SNOWH            ,& ! in,    snow depth [m]
              Z0MVT           => noahmp%energy%param%Z0MVT           ,& ! in,    momentum roughness length (m)
              HVT             => noahmp%energy%param%HVT             ,& ! in,    top of canopy (m)
              EG              => noahmp%energy%param%EG              ,& ! in,    emissivity soil surface
              SNOW_EMIS       => noahmp%energy%param%SNOW_EMIS       ,& ! in,    snow emissivity
              ELAI            => noahmp%energy%state%ELAI            ,& ! in,    leaf area index, after burying by snow
              ESAI            => noahmp%energy%state%ESAI            ,& ! in,    stem area index, after burying by snow
              FVEG            => noahmp%energy%state%FVEG            ,& ! in,    greeness vegetation fraction (-)
              TG              => noahmp%energy%state%TG              ,& ! inout, vegetated ground (below-canopy) temperature (K)
              TV              => noahmp%energy%state%TV              ,& ! inout, vegetation temperature (K)
              CM              => noahmp%energy%state%CM              ,& ! inout, exchange coefficient (m/s) for momentum, surface, grid mean
              CH              => noahmp%energy%state%CH              ,& ! inout, exchange coefficient (m/s) for heat, surface, grid mean
              SH2O            => noahmp%water%state%SH2O             ,& ! inout, soil water content [m3/m3]
              PSI             => noahmp%water%state%PSI              ,& ! out,   surface layer soil matrix potential (m)
              LATHEAV         => noahmp%energy%state%LATHEAV         ,& ! out,   latent heat of vaporization/subli (j/kg), canopy
              LATHEAG         => noahmp%energy%state%LATHEAG         ,& ! out,   latent heat of vaporization/subli (j/kg), ground
              FROZEN_CANOPY   => noahmp%energy%state%FROZEN_CANOPY   ,& ! out,   used to define latent heat pathway
              FROZEN_GROUND   => noahmp%energy%state%FROZEN_GROUND   ,& ! out,   frozen ground (logical) to define latent heat pathway
              VAI             => noahmp%energy%state%VAI             ,& ! out,   one-sided leaf+stem area index (m2/m2)
              TGV             => noahmp%energy%state%TGV             ,& ! out,   vegetated ground (below-canopy) temperature (K)
              GAMMAV          => noahmp%energy%state%GAMMAV          ,& ! out,   psychrometric constant (pa/K), canopy
              GAMMAG          => noahmp%energy%state%GAMMAG          ,& ! out,   psychrometric constant (pa/K), ground
              ZPDG            => noahmp%energy%state%ZPD             ,& ! out,   ground zero plane displacement (m)
              ZPD             => noahmp%energy%state%ZPD             ,& ! out,   surface zero plane displacement (m)
              Z0M             => noahmp%energy%state%Z0M             ,& ! out,   roughness length, momentum, (m), surface
              Z0MG            => noahmp%energy%state%Z0MG            ,& ! out,   roughness length, momentum, ground (m)
              ZLVL            => noahmp%energy%state%ZLVL            ,& ! out,   surface reference height  (m)
              RSURF           => noahmp%energy%state%RSURF           ,& ! out,   ground surface resistance (s/m)
              RHSUR           => noahmp%energy%state%RHSUR           ,& ! out,   raltive humidity in surface soil/snow air space (-)
              EMV             => noahmp%energy%state%EMV             ,& ! out,   vegetation emissivity
              EMG             => noahmp%energy%state%EMG             ,& ! out,   ground emissivity
              CMV             => noahmp%energy%state%CMV             ,& ! out,   drag coefficient for momentum, above ZPD, vegetated
              CHV             => noahmp%energy%state%CHV             ,& ! out,   drag coefficient for heat, above ZPD, vegetated
              EVG             => noahmp%energy%flux%EVG              ,& ! out,   ground evaporation heat flux (w/m2)  [+= to atm]
              EVC             => noahmp%energy%flux%EVC              ,& ! out,   canopy evaporation heat flux (w/m2)  [+= to atm]
              TR              => noahmp%energy%flux%TR               ,& ! out,   canopy transpiration heat flux (w/m2)[+= to atm]
              FGEV            => noahmp%energy%flux%FGEV             ,& ! out,   soil evap heat (w/m2) [+ to atm]
              FCEV            => noahmp%energy%flux%FCEV             ,& ! out,   canopy evaporation (w/m2) [+ = to atm]
              FCTR            => noahmp%energy%flux%FCTR             ,& ! out,   transpiration (w/m2) [+ = to atm]
              FSNO            => noahmp%water%state%FSNO              & ! out,   snow cover fraction (-)
             )
! ----------------------------------------------------------------------

!=== some input variables need to be updated every time step (temporally, will be included in completed Energy subroutine)

    VAI = ELAI + ESAI
    VEG = .false.
    if ( VAI > 0.0 ) VEG = .true.

    if ( TV > TFRZ ) then
      LATHEAV       = HVAP
      FROZEN_CANOPY = .false.
    else
      LATHEAV       = HSUB
      FROZEN_CANOPY = .true.
    endif
    GAMMAV = CPAIR * SFCPRS / (0.622 * LATHEAV)
    if ( TGV > TFRZ ) then
      LATHEAG       = HVAP
      FROZEN_GROUND = .false.
    else
      LATHEAG       = HSUB
      FROZEN_GROUND = .true.
    endif
    GAMMAG = CPAIR * SFCPRS / (0.622 * LATHEAG)

    ZPDG  = SNOWH
    if ( VEG .eqv. .true. ) then
       Z0M  = Z0MVT
       ZPD  = 0.65 * HVT
       if ( SNOWH > ZPD ) ZPD = SNOWH
    else
       Z0M  = Z0MG
       ZPD  = ZPDG
    endif
    ZLVL = max(ZPD, HVT) + ZREF
    if ( ZPDG >= ZLVL ) ZLVL = ZPDG + ZREF

    RSURF = FSNO * 1.0 + (1.0-FSNO) * exp(8.25-4.225*(max(0.0,SH2O(1)/SMCMAX(1)))) !Sellers (1992)
    PSI   = -PSISAT(1)*(max(0.01,SH2O(1))/SMCMAX(1))**(-BEXP(1))
    RHSUR = FSNO + (1.0-FSNO) * exp(PSI*GRAV/(RW*TG))
    EMV   = 1.0 - exp(-(ELAI+ESAI)/1.0)
    EMG   = EG(IST)*(1.0-FSNO) + SNOW_EMIS*FSNO

!=== input variable update end


    ! Thermal properties of soil, snow, lake, and frozen soil
    call GroundThermalProperty(noahmp)

    ! Solar radiation: absorbed & reflected by the ground and canopy
    call RadiationMain(noahmp)

    ! Surface energy flux and temperature: vegetated ground
    if ( VEG .eqv. .true. .and. FVEG > 0 ) then
       TGV = TG
       CMV = CM
       CHV = CH
       call SurfaceEnergyFluxVegetated(noahmp)
    endif
    TG   = TGV
    CM   = CMV
    CH   = CHV
    FGEV = EVG
    FCEV = EVC
    FCTR = TR


    ! Surface energy flux: bare ground





    end associate

  end subroutine EnergyMain

end module EnergyMainMod
