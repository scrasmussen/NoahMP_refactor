module CarbonFluxCropMod

!!! Main Carbon assimilation for crops
!!! based on RE Dickinson et al.(1998), modifed by Guo-Yue Niu, 2004
!!! Modified by Xing Liu, 2014
        
  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none
        
contains

  subroutine CarbonFluxCrop(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CO2FLUX_CROP
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: P. Valayamkunnath, C. He & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------
 
    implicit none
        
    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: SC          ! temperature stress death coefficient
    real(kind=kind_noahmp)           :: SD          ! water stress death coefficient
    real(kind=kind_noahmp)           :: ADDNPPLF    ! leaf assimil after resp. losses removed [g/m2/s] 
    real(kind=kind_noahmp)           :: ADDNPPST    ! stem assimil after resp. losses removed [g/m2/s]
    ! Respiration as a function of temperature
    real(kind=kind_noahmp)           :: r,x
    r(x) = exp(0.08 * (x - 298.16))

!------------------------------------------------------------------------
    associate(                                                        &
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,    main noahmp timestep (s)
              RTOVRC          => noahmp%biochem%param%RTOVRC         ,& ! in,    root turnover coefficient [1/s]
              RSDRYC          => noahmp%biochem%param%RSDRYC         ,& ! in,    degree of drying that reduces soil respiration [-]
              BF              => noahmp%biochem%param%BF             ,& ! in,    parameter for present wood allocation [-]
              WSTRC           => noahmp%biochem%param%WSTRC          ,& ! in,    water stress coeficient [-]
              LAIMIN          => noahmp%biochem%param%LAIMIN         ,& ! in,    minimum leaf area index [m2/m2]
              XSAMIN          => noahmp%biochem%param%XSAMIN         ,& ! in,    minimum stem area index [m2/m2]
              FOLN_MX         => noahmp%biochem%param%FOLN_MX        ,& ! in,    foliage nitrogen concentration when f(n)=1 (%)
              Q10MR           => noahmp%biochem%param%Q10MR          ,& ! in,    q10 for maintainance respiration
              LFMR25          => noahmp%biochem%param%LFMR25         ,& ! in,    leaf maintenance respiration at 25C [umol CO2/m**2/s]
              RTMR25          => noahmp%biochem%param%RTMR25         ,& ! in,    root maintenance respiration at 25C [umol CO2/kg bio/s]
              STMR25          => noahmp%biochem%param%STMR25         ,& ! in,    stem maintenance respiration at 25C [umol CO2/kg bio/s]
              GRAINMR25       => noahmp%biochem%param%GRAINMR25      ,& ! in,    grain maintenance respiration at 25C [umol CO2/kg bio/s]
              FRA_GR          => noahmp%biochem%param%FRA_GR         ,& ! in,    fraction of growth respiration
              LFPT            => noahmp%biochem%param%LFPT           ,& ! in,    fraction of carbohydrate flux to leaf
              STPT            => noahmp%biochem%param%STPT           ,& ! in,    fraction of carbohydrate flux to stem
              RTPT            => noahmp%biochem%param%RTPT           ,& ! in,    fraction of carbohydrate flux to root
              GRAINPT         => noahmp%biochem%param%GRAINPT        ,& ! in,    fraction of carbohydrate flux to grain
              LF_OVRC         => noahmp%biochem%param%LF_OVRC        ,& ! in,    fraction of leaf turnover  [1/s]
              RT_OVRC         => noahmp%biochem%param%RT_OVRC        ,& ! in,    fraction of root tunrover  [1/s]
              ST_OVRC         => noahmp%biochem%param%ST_OVRC        ,& ! in,    fraction of stem turnover  [1/s]
              LEFREEZ         => noahmp%biochem%param%LEFREEZ        ,& ! in,    characteristic T for leaf freezing [K]
              DILE_FW         => noahmp%biochem%param%DILE_FW        ,& ! in,    coeficient for water leaf stress death [1/s]
              DILE_FC         => noahmp%biochem%param%DILE_FC        ,& ! in,    coeficient for temperature leaf stress death [1/s]
              LFCT            => noahmp%biochem%param%LFCT           ,& ! in,    fraction of carbohydrate translocation from leaf to grain
              STCT            => noahmp%biochem%param%STCT           ,& ! in,    fraction of carbohydrate translocation from stem to grain
              RTCT            => noahmp%biochem%param%RTCT           ,& ! in,    fraction of carbohydrate translocation from root to grain
              MRP             => noahmp%biochem%param%MRP            ,& ! in,    microbial respiration parameter (umol co2/kg c/s)
              BIO2LAI         => noahmp%biochem%param%BIO2LAI        ,& ! in,    leaf are per living leaf biomass [m^2/kg]
              WROOT           => noahmp%water%state%WROOT            ,& ! in,    root zone soil water [-]
              WSTRES          => noahmp%water%state%WSTRES           ,& ! in,    water stress coeficient [-]  (1. for wilting)
              PhotosynTotal             => noahmp%biochem%flux%PhotosynTotal             ,& ! in,    total leaf photosynthesis (umol co2 /m2 /s)
              NitrogenConcFoliage => noahmp%biochem%state%NitrogenConcFoliage ,& ! in,    foliage nitrogen concentration (%)
              IndexPlanting             => noahmp%biochem%state%IndexPlanting            ,& ! in,    Planting index
              PlantGrowStage             => noahmp%biochem%state%PlantGrowStage            ,& ! in,    plant growing stage
              STC             => noahmp%energy%state%STC             ,& ! in,    snow and soil layer temperature [K]
              TV              => noahmp%energy%state%TV              ,& ! in,    vegetation temperature (k)
              XLAI            => noahmp%energy%state%LAI             ,& ! inout, leaf area index [-]
              XSAI            => noahmp%energy%state%SAI             ,& ! inout, stem area index [-]
              LeafMass          => noahmp%biochem%state%LeafMass         ,& ! inout, leaf mass [g/m2]
              RootMass          => noahmp%biochem%state%RootMass         ,& ! inout, mass of fine roots [g/m2]
              StemMass          => noahmp%biochem%state%StemMass         ,& ! inout, stem mass [g/m2]
              WoodMass        => noahmp%biochem%state%WoodMass    ,& ! inout, mass of wood (incl. woody roots) [g/m2]
              CarbonMassDeepSoil          => noahmp%biochem%state%CarbonMassDeepSoil         ,& ! inout, stable carbon in deep soil [g/m2]
              CarbonMassShallowSoil          => noahmp%biochem%state%CarbonMassShallowSoil         ,& ! inout, short-lived carbon in shallow soil [g/m2]
              GrainMass     => noahmp%biochem%state%GrainMass     ,& ! inout, mass of grain [g/m2]
              RespFacNitrogenFoliage             => noahmp%biochem%state%RespFacNitrogenFoliage            ,& ! out,   foliage nitrogen adjustemt to respiration (<= 1)
              MicroRespFactorSoilWater             => noahmp%biochem%state%MicroRespFactorSoilWater            ,& ! out,   soil water factor for microbial respiration
              MicroRespFactorSoilTemp             => noahmp%biochem%state%MicroRespFactorSoilTemp            ,& ! out,   soil temperature factor for microbial respiration
              LeafMassMin          => noahmp%biochem%state%LeafMassMin         ,& ! out,   minimum leaf mass [g/m2]
              StemMassMin          => noahmp%biochem%state%StemMassMin         ,& ! out,   minimum stem mass [g/m2]
              StemAreaPerMass            => noahmp%biochem%state%StemAreaPerMass           ,& ! out,   stem area per unit mass [m2/g]
              RespFacTemperature              => noahmp%biochem%state%RespFacTemperature             ,& ! out,   temperature factor
              CarbonMassSoilTot           => noahmp%biochem%state%CarbonMassSoilTot          ,& ! out,   total soil carbon [g/m2 C]
              CarbonMassLiveTot           => noahmp%biochem%state%CarbonMassLiveTot          ,& ! out,   total living carbon ([g/m2 C]
              CarbonAssim          => noahmp%biochem%flux%CarbonAssim          ,& ! out,   carbon assimilated rate [gC/m2/s]
              CarbonHydrateAssim       => noahmp%biochem%flux%CarbonHydrateAssim       ,& ! out,   carbonhydrate assimilated rate [g/m2/s]
              TurnoverLeaf          => noahmp%biochem%flux%TurnoverLeaf          ,& ! out,   leaf turnover rate [g/m2/s]
              TurnoverStem          => noahmp%biochem%flux%TurnoverStem          ,& ! out,   stem turnover rate [g/m2/s]
              TurnoverRoot          => noahmp%biochem%flux%TurnoverRoot          ,& ! out,   root carbon loss rate by turnover [g/m2/s]
              ConvLeafToGrain       => noahmp%biochem%flux%ConvLeafToGrain       ,& ! out,   leaf to grain conversion [g/m2/s]
              ConvRootToGrain       => noahmp%biochem%flux%ConvRootToGrain       ,& ! out,   root to grain conversion [g/m2/s]
              ConvStemToGrain       => noahmp%biochem%flux%ConvStemToGrain       ,& ! out,   stem to grain conversion [g/m2/s]
              RespirationPlantTot          => noahmp%biochem%flux%RespirationPlantTot          ,& ! out,  total plant respiration [g/m2/s C]
              CarbonToAtmos           => noahmp%biochem%flux%CarbonToAtmos           ,& ! out,   carbon flux to atmosphere [g/m2/s]
              GrossPriProduction  => noahmp%biochem%flux%GrossPriProduction             ,& ! out,  gross primary production [g/m2/s C]
              NetPriProductionTot => noahmp%biochem%flux%NetPriProductionTot  ,& ! out,  total net primary productivity [g/m2/s C]
              NetPriProductionLeaf  => noahmp%biochem%flux%NetPriProductionLeaf ,& ! out,   leaf net primary productivity [g/m2/s]
              NetPriProductionRoot            => noahmp%biochem%flux%NetPriProductionRoot            ,& ! out,   root net primary productivity [g/m2/s]
              NetPriProductionStem   => noahmp%biochem%flux%NetPriProductionStem      ,& ! out,   stem net primary productivity [g/m2/s]
              NetPriProductionGrain            => noahmp%biochem%flux%NetPriProductionGrain            ,& ! out,   grain net primary productivity [g/m2/s]
              NetEcoExchange             => noahmp%biochem%flux%NetEcoExchange             ,& ! out,   net ecosystem exchange [g/m2/s CO2]
              GrowthRespGrain         => noahmp%biochem%flux%GrowthRespGrain         ,& ! out,   growth respiration rate for grain [g/m2/s]
              GrowthRespLeaf          => noahmp%biochem%flux%GrowthRespLeaf          ,& ! out,   growth respiration rate for leaf [g/m2/s]
              GrowthRespRoot          => noahmp%biochem%flux%GrowthRespRoot          ,& ! out,   growth respiration rate for root [g/m2/s]
              GrowthRespStem          => noahmp%biochem%flux%GrowthRespStem          ,& ! out,   growth respiration rate for stem [g/m2/s]
              RespirationSoilOrg          => noahmp%biochem%flux%RespirationSoilOrg          ,& ! out,   soil organic respiration rate [g/m2/s C]
              LeafMassMaxChg           => noahmp%biochem%flux%LeafMassMaxChg           ,& ! out,   maximum leaf mass available to change [g/m2/s]
              StemMassMaxChg           => noahmp%biochem%flux%StemMassMaxChg           ,& ! out,   maximum steam  mass available to change [g/m2/s]
              RespirationLeaf            => noahmp%biochem%flux%RespirationLeaf            ,& ! out,   leaf respiration rate [g/m2/s]
              RespirationStem          => noahmp%biochem%flux%RespirationStem          ,& ! out,   stem respiration rate [g/m2/s]
              RespirationLeafMaint          => noahmp%biochem%flux%RespirationLeafMaint          ,& ! out,   leaf maintenance respiration rate [g/m2/s]
              RespirationRoot          => noahmp%biochem%flux%RespirationRoot          ,& ! out,   fine root respiration rate [g/m2/s]
              RespirationSoil          => noahmp%biochem%flux%RespirationSoil          ,& ! out,   soil respiration rate [g/m2/s]
              RespirationGrain         => noahmp%biochem%flux%RespirationGrain         ,& ! out,   grain respiration rate [g/m2/s]
              DeathLeaf           => noahmp%biochem%flux%DeathLeaf           ,& ! out,   death rate of leaf mass [g/m2/s]
              CarbonDecayToStable          => noahmp%biochem%flux%CarbonDecayToStable           & ! out,   decay rate of fast carbon to slow carbon [g/m2/s]
             )
!----------------------------------------------------------------------

    ! initialization
    StemAreaPerMass   = 3.0 * 0.001         ! m2/kg -->m2/g
    LeafMassMin = LAIMIN / 0.035
    StemMassMin = XSAMIN / StemAreaPerMass

    !!! carbon assimilation starts
    ! 1 mole -> 12 g carbon or 44 g CO2 or 30 g CH20
    CarbonAssim    = PhotosynTotal * 12.0e-6   !*IndexPlanting   !umol co2 /m2/ s -> g/m2/s C
    CarbonHydrateAssim = PhotosynTotal * 30.0e-6   !*IndexPlanting

    ! mainteinance respiration
    RespFacNitrogenFoliage     = min( NitrogenConcFoliage / max(1.0e-06, FOLN_MX), 1.0 )
    RespFacTemperature      = Q10MR**((TV - 298.16) / 10.0)
    RespirationLeaf    = LFMR25 * RespFacTemperature * RespFacNitrogenFoliage * XLAI * (1.0 - WSTRES)         ! umol/m2/s
    RespirationLeafMaint  = min( (LeafMass - LeafMassMin) / MainTimeStep, RespirationLeaf*30.0e-6 )       ! g/m2/s
    RespirationRoot  = RTMR25 * (RootMass * 1.0e-3) * RespFacTemperature * 30.0e-6         ! g/m2/s
    RespirationStem  = STMR25 * (StemMass * 1.0e-3) * RespFacTemperature * 30.0e-6         ! g/m2/s
    RespirationGrain = GRAINMR25 * (GrainMass * 1.0e-3) * RespFacTemperature * 30.0e-6       ! g/m2/s

    ! calculate growth respiration for leaf, root and grain
    GrowthRespLeaf  = max( 0.0, FRA_GR * (LFPT(PlantGrowStage)*CarbonHydrateAssim - RespirationLeafMaint) )
    GrowthRespStem  = max( 0.0, FRA_GR * (STPT(PlantGrowStage)*CarbonHydrateAssim - RespirationStem) )
    GrowthRespRoot  = max( 0.0, FRA_GR * (RTPT(PlantGrowStage)*CarbonHydrateAssim - RespirationRoot) )
    GrowthRespGrain = max( 0.0, FRA_GR * (GRAINPT(PlantGrowStage)*CarbonHydrateAssim - RespirationGrain) )

    ! leaf turnover, stem turnover, root turnover and leaf death caused by soil water and soil temperature stress
    TurnoverLeaf  = LF_OVRC(PlantGrowStage) * 1.0e-6 * LeafMass
    TurnoverRoot  = RT_OVRC(PlantGrowStage) * 1.0e-6 * RootMass
    TurnoverStem  = ST_OVRC(PlantGrowStage) * 1.0e-6 * StemMass
    SC      = exp( -0.3 * max(0.0, TV-LEFREEZ) ) * (LeafMass/120.0)
    SD      = exp( (WSTRES - 1.0) * WSTRC )
    DeathLeaf   = LeafMass * 1.0e-6 * (DILE_FW(PlantGrowStage) * SD + DILE_FC(PlantGrowStage) * SC)

    ! Allocation of CarbonHydrateAssim to leaf, stem, root and grain at each growth stage
    ADDNPPLF = max( 0.0, LFPT(PlantGrowStage)*CarbonHydrateAssim - GrowthRespLeaf - RespirationLeafMaint )
    ADDNPPLF = LFPT(PlantGrowStage)*CarbonHydrateAssim - GrowthRespLeaf - RespirationLeafMaint
    ADDNPPST = max( 0.0, STPT(PlantGrowStage)*CarbonHydrateAssim - GrowthRespStem - RespirationStem )
    ADDNPPST = STPT(PlantGrowStage)*CarbonHydrateAssim - GrowthRespStem - RespirationStem
    
    ! avoid reducing leaf mass below its minimum value but conserve mass
    LeafMassMaxChg  = (LeafMass - LeafMassMin) / MainTimeStep
    StemMassMaxChg  = (StemMass - StemMassMin) / MainTimeStep
    TurnoverLeaf = min( TurnoverLeaf, LeafMassMaxChg+ADDNPPLF )
    TurnoverStem = min( TurnoverStem, StemMassMaxChg+ADDNPPST )
    DeathLeaf  = min( DeathLeaf, LeafMassMaxChg+ADDNPPLF-TurnoverLeaf )

    ! net primary productivities
    NetPriProductionLeaf = max( ADDNPPLF, -LeafMassMaxChg )
    NetPriProductionLeaf   = ADDNPPLF
    NetPriProductionStem  = max( ADDNPPST, -StemMassMaxChg )
    NetPriProductionStem  = ADDNPPST
    NetPriProductionRoot   = RTPT(PlantGrowStage) * CarbonHydrateAssim - RespirationRoot - GrowthRespRoot
    NetPriProductionGrain   = GRAINPT(PlantGrowStage) * CarbonHydrateAssim - RespirationGrain - GrowthRespGrain

    ! masses of plant components
    LeafMass = LeafMass + (NetPriProductionLeaf - TurnoverLeaf - DeathLeaf) * MainTimeStep
    StemMass = StemMass + (NetPriProductionStem - TurnoverStem) * MainTimeStep       ! g/m2
    RootMass = RootMass + (NetPriProductionRoot - TurnoverRoot) * MainTimeStep
    GrainMass  = GrainMass + NetPriProductionGrain * MainTimeStep 
    GrossPriProduction    = CarbonHydrateAssim * 0.4                                  ! g/m2/s C  0.4=12/30, CH20 to C

    ! carbon convert to grain
    ConvLeafToGrain = 0.0              ! Zhe Zhang 2020-07-13
    ConvStemToGrain = 0.0
    ConvRootToGrain = 0.0
    ConvLeafToGrain = LeafMass * (LFCT(PlantGrowStage) * MainTimeStep / 3600.0)
    ConvStemToGrain = StemMass * (STCT(PlantGrowStage) * MainTimeStep / 3600.0)
    ConvRootToGrain = RootMass * (RTCT(PlantGrowStage) * MainTimeStep / 3600.0)
    LeafMass    = LeafMass - ConvLeafToGrain
    StemMass    = StemMass - ConvStemToGrain
    RootMass    = RootMass - ConvRootToGrain
    GrainMass     = GrainMass + ConvStemToGrain + ConvRootToGrain + ConvLeafToGrain
    !if ( PlantGrowStage==6 ) then
    !   ConvStemToGrain = StemMass * (0.00005 * MainTimeStep / 3600.0)
    !   StemMass    = StemMass - ConvStemToGrain
    !   ConvRootToGrain = RootMass * (0.0005 * MainTimeStep / 3600.0)
    !   RootMass    = RootMass - ConvRootToGrain
    !   GrainMass     = GrainMass + ConvStemToGrain + ConvRootToGrain
    !endif
    
    if ( RootMass < 0.0 ) then
       TurnoverRoot = NetPriProductionRoot
       RootMass = 0.0
    endif
    if ( GrainMass < 0.0 ) then
       GrainMass = 0.0
    endif

    ! soil carbon budgets
    !if ( (PlantGrowStage == 1) .or. (PlantGrowStage == 2) .or. (PlantGrowStage == 8) ) then
    !   CarbonMassShallowSoil=1000
    !else
    CarbonMassShallowSoil = CarbonMassShallowSoil + &
                            (TurnoverRoot+TurnoverLeaf+TurnoverStem+DeathLeaf) * MainTimeStep 
    !endif
    MicroRespFactorSoilTemp    = 2.0**((STC(1) - 283.16) / 10.0)
    MicroRespFactorSoilWater    = WROOT / (0.20 + WROOT) * 0.23 / (0.23 + WROOT)
    RespirationSoil = MicroRespFactorSoilWater * MicroRespFactorSoilTemp * &
                      MRP * max(0.0, CarbonMassShallowSoil*1.0e-3) * 12.0e-6
    CarbonDecayToStable = 0.1 * RespirationSoil
    CarbonMassShallowSoil = CarbonMassShallowSoil - (RespirationSoil + CarbonDecayToStable) * MainTimeStep
    CarbonMassDeepSoil = CarbonMassDeepSoil + CarbonDecayToStable * MainTimeStep
 
    !  total carbon flux
    CarbonToAtmos  = - CarbonAssim + RespirationLeafMaint + RespirationRoot + RespirationStem &
             + RespirationSoil + GrowthRespLeaf + GrowthRespRoot                           !g/m2/s 0.4=12/30, CH20 to C

    ! for outputs
    NetPriProductionTot = (NetPriProductionLeaf + NetPriProductionStem + &
                           NetPriProductionRoot + NetPriProductionGrain) * 0.4  !g/m2/s C  0.4=12/30, CH20 to C 
    RespirationPlantTot = RespirationRoot + RespirationGrain + RespirationLeafMaint + &                        !g/m2/s C
             GrowthRespLeaf + GrowthRespRoot + GrowthRespGrain                            !g/m2/s C
    RespirationSoilOrg = RespirationSoil                                               !g/m2/s C
    NetEcoExchange    = (RespirationPlantTot + RespirationSoilOrg - GrossPriProduction) * 44.0 / 30.0                !g/m2/s CO2
    CarbonMassSoilTot  = CarbonMassShallowSoil + CarbonMassDeepSoil                                      !g/m2   C
    CarbonMassLiveTot  = LeafMass + RootMass + GrainMass        
 
    ! leaf area index and stem area index
    XLAI   = max( LeafMass*BIO2LAI, LAIMIN )
    XSAI   = max( StemMass*StemAreaPerMass, XSAMIN )
   
    ! After harversting
    !if ( PlantGrowStage == 8 ) then
    !   LeafMass = 0.62
    !   StemMass = 0
    !   GrainMass  = 0
    !endif

    !if ( (PlantGrowStage == 1) .or. (PlantGrowStage == 2) .or. (PlantGrowStage == 8) ) then
    if ( (PlantGrowStage == 8) .and. &
         ((GrainMass > 0.0) .or. (LeafMass > 0) .or. (StemMass > 0) .or. (RootMass > 0)) ) then
       XLAI   = 0.05
       XSAI   = 0.05
       LeafMass = LeafMassMin
       StemMass = StemMassMin
       RootMass = 0
       GrainMass  = 0
    endif 
        
    end associate

  end subroutine CarbonFluxCrop

end module CarbonFluxCropMod
