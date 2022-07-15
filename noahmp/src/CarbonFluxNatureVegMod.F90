module CarbonFluxNatureVegMod

!!! Main Carbon assimilation for natural vegetation
!!! based on RE Dickinson et al.(1998), modifed by Guo-Yue Niu, 2004

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
        
  implicit none
        
contains

  subroutine CarbonFluxNatureVeg(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CO2FLUX
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------
        
    implicit none
        
    type(noahmp_type), intent(inout) :: noahmp

! local variables
    real(kind=kind_noahmp)           :: SC          ! temperature stress death coefficient
    real(kind=kind_noahmp)           :: SD          ! water stress death coefficient
    real(kind=kind_noahmp)           :: ADDNPPLF    ! leaf assimil after resp. losses removed [g/m2/s] 
    real(kind=kind_noahmp)           :: ADDNPPST    ! stem assimil after resp. losses removed [g/m2/s]
    ! Respiration as a function of temperature
    real(kind=kind_noahmp)           :: r,x
    r(x) = exp(0.08 * (x - 298.16))

!------------------------------------------------------------------------
    associate(                                                        &
              VegType         => noahmp%config%domain%VegType        ,& ! in,    vegetation type
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,    main noahmp timestep (s)
              IndexEBLForest       => noahmp%config%domain%IndexEBLForest      ,& ! in,    flag for Evergreen Broadleaf Forest
              WoodToRootRatio           => noahmp%biochem%param%WoodToRootRatio          ,& ! in,    wood to root ratio
              TurnoverCoeffLeafVeg          => noahmp%biochem%param%TurnoverCoeffLeafVeg         ,& ! in,    leaf turnover coefficient [1/s] for generic vegetation
              TemperaureLeafFreeze           => noahmp%biochem%param%TemperaureLeafFreeze          ,& ! in,    characteristic temperature for leaf freezing [K]
              LeafDeathWaterCoeffVeg          => noahmp%biochem%param%LeafDeathWaterCoeffVeg         ,& ! in,    coeficient for leaf water stress death [1/s] for generic veg
              LeafDeathTempCoeffVeg          => noahmp%biochem%param%LeafDeathTempCoeffVeg         ,& ! in,    coeficient for leaf temperature stress death [1/s] for generic veg
              GrowthRespFrac           => noahmp%biochem%param%GrowthRespFrac          ,& ! in,    fraction of growth respiration  !original was 0.3
              TemperatureMinPhotosyn            => noahmp%biochem%param%TemperatureMinPhotosyn           ,& ! in,    minimum temperature for photosynthesis (k)
              MicroRespCoeff             => noahmp%biochem%param%MicroRespCoeff            ,& ! in,    microbial respiration parameter (umol co2/kg c/s)
              NitrogenConcFoliageMax          => noahmp%biochem%param%NitrogenConcFoliageMax         ,& ! in,    foliage nitrogen concentration when f(n)=1 (%)
              RespMaintQ10             => noahmp%biochem%param%RespMaintQ10            ,& ! in,    q10 for maintenance respiration
              RespMaintLeaf25C           => noahmp%biochem%param%RespMaintLeaf25C          ,& ! in,    leaf maintenance respiration at 25c (umol co2/m**2/s)
              RespMaintRoot25C           => noahmp%biochem%param%RespMaintRoot25C          ,& ! in,    root maintenance respiration at 25c (umol co2/kg bio/s)
              RespMaintStem25C           => noahmp%biochem%param%RespMaintStem25C          ,& ! in,    stem maintenance respiration at 25c (umol co2/kg bio/s)
              WoodPoolIndex          => noahmp%biochem%param%WoodPoolIndex         ,& ! in,    wood pool index (0~1) depending on woody or not
              TurnoverCoeffRootVeg          => noahmp%biochem%param%TurnoverCoeffRootVeg         ,& ! in,    root turnover coefficient [1/s] for generic vegetation
              WoodRespCoeff         => noahmp%biochem%param%WoodRespCoeff        ,& ! in,    wood respiration coeficient [1/s]
              WoodAllocFac              => noahmp%biochem%param%WoodAllocFac             ,& ! in,    parameter for present wood allocation [-]
              WaterStressCoeff           => noahmp%biochem%param%WaterStressCoeff          ,& ! in,    water stress coeficient [-]
              LeafAreaIndexMin          => noahmp%biochem%param%LeafAreaIndexMin         ,& ! in,    minimum leaf area index [m2/m2]
              StemAreaIndexMin          => noahmp%biochem%param%StemAreaIndexMin         ,& ! in,    minimum stem area index [m2/m2]
              IndexGrowSeason             => noahmp%biochem%state%IndexGrowSeason            ,& ! in,    growing season index (0=off, 1=on)
              NitrogenConcFoliage => noahmp%biochem%state%NitrogenConcFoliage ,& ! in,    foliage nitrogen concentration (%)
              LeafAreaPerMass            => noahmp%biochem%state%LeafAreaPerMass           ,& ! in,    leaf area per unit mass [m2/g]
              PhotosynTotal             => noahmp%biochem%flux%PhotosynTotal             ,& ! in,    total leaf photosynthesis (umol co2 /m2 /s)
              SoilWaterRootZone           => noahmp%water%state%SoilWaterRootZone            ,& ! in,    root zone soil water [-]
              SoilWaterStress          => noahmp%water%state%SoilWaterStress           ,& ! in,    water stress coeficient [-]  (1. for wilting)
              STC             => noahmp%energy%state%STC             ,& ! in,    snow and soil layer temperature [K]
              TROOT           => noahmp%energy%state%TROOT           ,& ! in,    root-zone averaged temperature (k)
              TV              => noahmp%energy%state%TV              ,& ! in,    vegetation temperature (k)
              FVEG            => noahmp%energy%state%FVEG            ,& ! in,    greeness vegetation fraction (-)
              XLAI            => noahmp%energy%state%LAI             ,& ! inout, leaf area index [-]
              XSAI            => noahmp%energy%state%SAI             ,& ! inout, stem area index [-]
              LeafMass          => noahmp%biochem%state%LeafMass         ,& ! inout, leaf mass [g/m2]
              RootMass          => noahmp%biochem%state%RootMass         ,& ! inout, mass of fine roots [g/m2]
              StemMass          => noahmp%biochem%state%StemMass         ,& ! inout, stem mass [g/m2]
              WoodMass         => noahmp%biochem%state%WoodMass  ,& ! inout, mass of wood (incl. woody roots) [g/m2]
              CarbonMassDeepSoil          => noahmp%biochem%state%CarbonMassDeepSoil         ,& ! inout, stable carbon in deep soil [g/m2]
              CarbonMassShallowSoil          => noahmp%biochem%state%CarbonMassShallowSoil         ,& ! inout, short-lived carbon in shallow soil [g/m2]
              CarbonMassSoilTot           => noahmp%biochem%state%CarbonMassSoilTot          ,& ! out,   total soil carbon [g/m2 C]
              CarbonMassLiveTot           => noahmp%biochem%state%CarbonMassLiveTot          ,& ! out,   total living carbon ([g/m2 C]
              LeafMassMin          => noahmp%biochem%state%LeafMassMin         ,& ! out,   minimum leaf mass [g/m2]
              CarbonFracToLeaf          => noahmp%biochem%state%CarbonFracToLeaf         ,& ! out,   fraction of carbon allocated to leaves [-]
              WoodCarbonFrac           => noahmp%biochem%state%WoodCarbonFrac          ,& ! out,   calculated wood to root ratio [-]
              CarbonFracToWoodRoot          => noahmp%biochem%state%CarbonFracToWoodRoot         ,& ! out,   fraction of carbon to root and wood [-]
              CarbonFracToRoot          => noahmp%biochem%state%CarbonFracToRoot         ,& ! out,   fraction of carbon flux to roots [-]
              CarbonFracToWood          => noahmp%biochem%state%CarbonFracToWood         ,& ! out,   fraction of carbon flux to wood [-]
              CarbonFracToStem          => noahmp%biochem%state%CarbonFracToStem         ,& ! out,   fraction of carbon flux to stem [-]
              MicroRespFactorSoilWater             => noahmp%biochem%state%MicroRespFactorSoilWater            ,& ! out,   soil water factor for microbial respiration
              MicroRespFactorSoilTemp             => noahmp%biochem%state%MicroRespFactorSoilTemp            ,& ! out,   soil temperature factor for microbial respiration
              RespFacNitrogenFoliage             => noahmp%biochem%state%RespFacNitrogenFoliage            ,& ! out,   foliage nitrogen adjustemt to respiration (<= 1)
              RespFacTemperature              => noahmp%biochem%state%RespFacTemperature             ,& ! out,   temperature factor
              RespReductionFac              => noahmp%biochem%state%RespReductionFac             ,& ! out,   respiration reduction factor (<= 1)
              StemMassMin          => noahmp%biochem%state%StemMassMin         ,& ! out,   minimum stem mass [g/m2]
              StemAreaPerMass            => noahmp%biochem%state%StemAreaPerMass           ,& ! out,   stem area per unit mass [m2/g]
              CarbonAssim          => noahmp%biochem%flux%CarbonAssim          ,& ! out,   carbon assimilated rate [gC/m2/s]
              GrossPriProduction             => noahmp%biochem%flux%GrossPriProduction             ,& ! out,   gross primary production [g/m2/s C]
              NetPriProductionTot  => noahmp%biochem%flux%NetPriProductionTot ,& ! out,   total net primary productivity [g/m2/s C]
              NetEcoExchange             => noahmp%biochem%flux%NetEcoExchange             ,& ! out,   net ecosystem exchange [g/m2/s CO2]
              RespirationPlantTot          => noahmp%biochem%flux%RespirationPlantTot          ,& ! out,   total plant respiration [g/m2/s C]
              RespirationSoilOrg          => noahmp%biochem%flux%RespirationSoilOrg          ,& ! out,   soil organic respiration [g/m2/s C]
              CarbonToAtmos           => noahmp%biochem%flux%CarbonToAtmos           ,& ! out,   carbon flux to atmosphere [g/m2/s]
              NetPriProductionLeaf  => noahmp%biochem%flux%NetPriProductionLeaf ,& ! out,   leaf net primary productivity [g/m2/s]
              NetPriProductionRoot            => noahmp%biochem%flux%NetPriProductionRoot            ,& ! out,   root net primary productivity [g/m2/s]
              NetPriProductionWood            => noahmp%biochem%flux%NetPriProductionWood            ,& ! out,   wood net primary productivity [g/m2/s]
              NetPriProductionStem  => noahmp%biochem%flux%NetPriProductionStem   ,& ! out,   stem net primary productivity [g/m2/s]
              GrowthRespLeaf          => noahmp%biochem%flux%GrowthRespLeaf          ,& ! out,   growth respiration rate for leaf [g/m2/s]
              GrowthRespRoot          => noahmp%biochem%flux%GrowthRespRoot          ,& ! out,   growth respiration rate for root [g/m2/s]
              GrowthRespWood          => noahmp%biochem%flux%GrowthRespWood          ,& ! out,   growth respiration rate for wood [g/m2/s]
              GrowthRespStem          => noahmp%biochem%flux%GrowthRespStem          ,& ! out,   growth respiration rate for stem [g/m2/s]
              LeafMassMaxChg           => noahmp%biochem%flux%LeafMassMaxChg           ,& ! out,   maximum leaf mass available to change [g/m2/s]
              CarbonDecayToStable          => noahmp%biochem%flux%CarbonDecayToStable          ,& ! out,   decay rate of fast carbon to slow carbon [g/m2/s]
              RespirationLeaf            => noahmp%biochem%flux%RespirationLeaf            ,& ! out,   leaf respiration rate [umol CO2/m2/s]
              RespirationStem          => noahmp%biochem%flux%RespirationStem          ,& ! out,   stem respiration rate [g/m2/s]
              RespirationWood => noahmp%biochem%flux%RespirationWood ,& ! out,   wood respiration rate [g/m2/s]
              RespirationLeafMaint          => noahmp%biochem%flux%RespirationLeafMaint          ,& ! out,   leaf maintenance respiration rate [g/m2/s]
              RespirationRoot          => noahmp%biochem%flux%RespirationRoot          ,& ! out,   fine root respiration rate [g/m2/s]
              RespirationSoil          => noahmp%biochem%flux%RespirationSoil          ,& ! out,   soil respiration rate [g/m2/s]
              DeathLeaf           => noahmp%biochem%flux%DeathLeaf           ,& ! out,   death rate of leaf mass [g/m2/s]
              DeathStem           => noahmp%biochem%flux%DeathStem           ,& ! out,   death rate of stem mass [g/m2/s]
              TurnoverLeaf          => noahmp%biochem%flux%TurnoverLeaf          ,& ! out,   leaf turnover rate [g/m2/s]
              TurnoverStem          => noahmp%biochem%flux%TurnoverStem          ,& ! out,   stem turnover rate [g/m2/s]
              TurnoverWood          => noahmp%biochem%flux%TurnoverWood          ,& ! out,   wood turnover rate [g/m2/s]
              TurnoverRoot          => noahmp%biochem%flux%TurnoverRoot          ,& ! out,   root turnover rate [g/m2/s]
              StemMassMaxChg           => noahmp%biochem%flux%StemMassMaxChg            & ! out,   maximum steam mass available to change [g/m2/s]
             )
!-----------------------------------------------------------------------

    ! initialization
    StemAreaPerMass    = 3.0 * 0.001      ! m2/kg -->m2/g
    LeafMassMin  = LeafAreaIndexMin / LeafAreaPerMass
    StemMassMin  = StemAreaIndexMin / StemAreaPerMass
        
    ! respiration
    if ( IndexGrowSeason == 0.0 ) then
       RespReductionFac = 0.5
    else
       RespReductionFac = 1.0
    endif             
    RespFacNitrogenFoliage    = min( NitrogenConcFoliage / max(1.0e-06,NitrogenConcFoliageMax), 1.0 )
    RespFacTemperature     = RespMaintQ10**((TV - 298.16) / 10.0)
    RespirationLeaf   = RespMaintLeaf25C * RespFacTemperature * RespFacNitrogenFoliage * &
                        XLAI * RespReductionFac * (1.0 - SoilWaterStress)           ! umol CO2/m2/s
    RespirationLeafMaint = min( (LeafMass-LeafMassMin)/MainTimeStep, RespirationLeaf*12.0e-6 )                 ! g/m2/s
    RespirationRoot = RespMaintRoot25C * (RootMass*1.0e-3) * RespFacTemperature * RespReductionFac * 12.0e-6             ! g/m2/s
    RespirationStem = RespMaintStem25C * ((StemMass-StemMassMin) * 1.0e-3) * &
                      RespFacTemperature * RespReductionFac * 12.0e-6  ! g/m2/s
    RespirationWood = WoodRespCoeff * r(TV) * WoodMass * WoodPoolIndex
    
    !!! carbon assimilation start
    ! 1 mole -> 12 g carbon or 44 g CO2; 1 umol -> 12.e-6 g carbon;   
    CarbonAssim  = PhotosynTotal * 12.0e-6      ! umol co2 /m2/ s -> g/m2/s carbon

    ! fraction of carbon into leaf versus nonleaf
    CarbonFracToLeaf = exp(0.01 * (1.0 - exp(0.75*XLAI)) * XLAI)
    if ( VegType == IndexEBLForest ) CarbonFracToLeaf = exp(0.01 * (1.0 - exp(0.50*XLAI)) * XLAI)
    CarbonFracToWoodRoot = 1.0 - CarbonFracToLeaf
    CarbonFracToStem = XLAI / 10.0 * CarbonFracToLeaf
    CarbonFracToLeaf = CarbonFracToLeaf - CarbonFracToStem
      
    !  fraction of carbon into wood versus root 
    if ( WoodMass > 1.0e-6 ) then
       WoodCarbonFrac = (1.0 - exp(-WoodAllocFac * (WoodToRootRatio*RootMass/WoodMass)) / WoodAllocFac) * WoodPoolIndex
    else
       WoodCarbonFrac = WoodPoolIndex
    endif   
    CarbonFracToRoot = CarbonFracToWoodRoot * (1.0 - WoodCarbonFrac)
    CarbonFracToWood = CarbonFracToWoodRoot * WoodCarbonFrac

    ! leaf and root turnover per time step  
    TurnoverLeaf = TurnoverCoeffLeafVeg * 5.0e-7 * LeafMass
    TurnoverStem = TurnoverCoeffLeafVeg * 5.0e-7 * StemMass
    TurnoverRoot = TurnoverCoeffRootVeg * RootMass
    TurnoverWood = 9.5e-10 * WoodMass
       
    ! seasonal leaf die rate dependent on temp and water stress
    ! water stress is set to 1 at permanent wilting point      
    SC    = exp(-0.3 * max(0.0, TV-TemperaureLeafFreeze)) * (LeafMass / 120.0) 
    SD    = exp((SoilWaterStress - 1.0) * WaterStressCoeff)
    DeathLeaf = LeafMass * 1.0e-6 * (LeafDeathWaterCoeffVeg * SD + LeafDeathTempCoeffVeg * SC)
    DeathStem = StemMass * 1.0e-6 * (LeafDeathWaterCoeffVeg * SD + LeafDeathTempCoeffVeg * SC)
     
    ! calculate growth respiration for leaf, root and wood 
    GrowthRespLeaf = max( 0.0, GrowthRespFrac * (CarbonFracToLeaf*CarbonAssim - RespirationLeafMaint) )
    GrowthRespStem = max( 0.0, GrowthRespFrac * (CarbonFracToStem*CarbonAssim - RespirationStem) )
    GrowthRespRoot = max( 0.0, GrowthRespFrac * (CarbonFracToRoot*CarbonAssim - RespirationRoot) )
    GrowthRespWood = max( 0.0, GrowthRespFrac * (CarbonFracToWood*CarbonAssim - RespirationWood) )
        
    ! Impose lower T limit for photosynthesis
    ADDNPPLF = max( 0.0, CarbonFracToLeaf*CarbonAssim - GrowthRespLeaf - RespirationLeafMaint )
    ADDNPPST = max( 0.0, CarbonFracToStem*CarbonAssim - GrowthRespStem - RespirationStem )
    !ADDNPPLF = CarbonFracToLeaf*CarbonAssim - GrowthRespLeaf - RespirationLeafMaint  ! MB: test Kjetil 
    !ADDNPPST = CarbonFracToStem*CarbonAssim - GrowthRespStem - RespirationStem  ! MB: test Kjetil 
    if ( TV < TemperatureMinPhotosyn ) ADDNPPLF = 0.0
    if ( TV < TemperatureMinPhotosyn ) ADDNPPST = 0.0
     
    ! update leaf, root, and wood carbon
    ! avoid reducing leaf mass below its minimum value but conserve mass
    LeafMassMaxChg = (LeafMass - LeafMassMin) / MainTimeStep
    StemMassMaxChg = (StemMass - StemMassMin) / MainTimeStep
    DeathLeaf = min( DeathLeaf, LeafMassMaxChg+ADDNPPLF-TurnoverLeaf )
    DeathStem = min( DeathStem, StemMassMaxChg+ADDNPPST-TurnoverStem )
      
    ! net primary productivities
    NetPriProductionLeaf  = max( ADDNPPLF, -LeafMassMaxChg )
    NetPriProductionStem  = max( ADDNPPST, -StemMassMaxChg )
    NetPriProductionRoot  = CarbonFracToRoot * CarbonAssim - RespirationRoot - GrowthRespRoot
    NetPriProductionWood  = CarbonFracToWood * CarbonAssim - RespirationWood - GrowthRespWood
       
    ! masses of plant components
    LeafMass = LeafMass + (NetPriProductionLeaf - TurnoverLeaf - DeathLeaf) * MainTimeStep
    StemMass = StemMass + (NetPriProductionStem - TurnoverStem - DeathStem) * MainTimeStep   ! g/m2
    RootMass = RootMass + (NetPriProductionRoot - TurnoverRoot) * MainTimeStep
    if ( RootMass < 0.0 ) then
       TurnoverRoot = NetPriProductionRoot
       RootMass = 0.0
    endif 
    WoodMass = (WoodMass + (NetPriProductionWood - TurnoverWood) * MainTimeStep ) * WoodPoolIndex

    ! soil carbon budgets 
    CarbonMassShallowSoil = CarbonMassShallowSoil + &
          (TurnoverRoot+TurnoverLeaf+TurnoverStem+TurnoverWood+DeathLeaf+DeathStem) * MainTimeStep  ! MB: add DeathStem v3.7
    MicroRespFactorSoilTemp    = 2.0**( (STC(1) - 283.16) / 10.0 )
    MicroRespFactorSoilWater    = SoilWaterRootZone / (0.20 + SoilWaterRootZone) * 0.23 / (0.23 + SoilWaterRootZone)
    RespirationSoil = MicroRespFactorSoilWater * MicroRespFactorSoilTemp * &
                      MicroRespCoeff * max(0.0, CarbonMassShallowSoil*1.0e-3) * 12.0e-6
    CarbonDecayToStable = 0.1 * RespirationSoil
    CarbonMassShallowSoil = CarbonMassShallowSoil - (RespirationSoil + CarbonDecayToStable) * MainTimeStep
    CarbonMassDeepSoil = CarbonMassDeepSoil + CarbonDecayToStable * MainTimeStep
     
    !  total carbon flux     
    CarbonToAtmos  = - CarbonAssim + RespirationLeafMaint + RespirationRoot + RespirationWood + RespirationStem &     ! MB: add RespirationStem,GrowthRespStem,0.9*RespirationSoil v3.7
             + 0.9*RespirationSoil + GrowthRespLeaf + GrowthRespRoot + GrowthRespWood + GrowthRespStem   ! g/m2/s

    ! for outputs   
    GrossPriProduction  = CarbonAssim                                             !g/m2/s C
    NetPriProductionTot = NetPriProductionLeaf + NetPriProductionWood + NetPriProductionRoot + NetPriProductionStem   !g/m2/s C
    RespirationPlantTot = RespirationRoot + RespirationWood + RespirationLeafMaint + RespirationStem + &              !g/m2/s C  MB: add RespirationStem, GrowthRespStem v3.7
             GrowthRespLeaf + GrowthRespRoot + GrowthRespWood + GrowthRespStem                  !g/m2/s C  MB: add 0.9* v3.7
    RespirationSoilOrg = 0.9 * RespirationSoil                                       !g/m2/s C
    NetEcoExchange    = (RespirationPlantTot + RespirationSoilOrg - GrossPriProduction) * 44.0 / 12.0              !g/m2/s CO2
    CarbonMassSoilTot  = CarbonMassShallowSoil + CarbonMassDeepSoil                                    !g/m2   C
    CarbonMassLiveTot  = LeafMass + RootMass + StemMass + WoodMass      !g/m2   C  MB: add StemMass v3.7
    
    ! leaf area index and stem area index
    XLAI   = max( LeafMass*LeafAreaPerMass, LeafAreaIndexMin )
    XSAI   = max( StemMass*StemAreaPerMass, StemAreaIndexMin )

    end associate

  end subroutine CarbonFluxNatureVeg

end module CarbonFluxNatureVegMod
