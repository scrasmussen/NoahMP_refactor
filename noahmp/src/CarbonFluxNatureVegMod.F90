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
    ! Respiration as a function of temperature
    real(kind=kind_noahmp)           :: r,x
    r(x) = exp(0.08 * (x - 298.16))

!------------------------------------------------------------------------
    associate(                                                        &
              VegType         => noahmp%config%domain%VegType        ,& ! in,    vegetation type
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,    main noahmp timestep (s)
              IndexEBLForest       => noahmp%config%domain%IndexEBLForest      ,& ! in,    flag for Evergreen Broadleaf Forest
              WRRAT           => noahmp%biochem%param%WRRAT          ,& ! in,    wood to non-wood ratio
              LTOVRC          => noahmp%biochem%param%LTOVRC         ,& ! in,    leaf turnover coefficient [1/s]
              TDLEF           => noahmp%biochem%param%TDLEF          ,& ! in,    characteristic T for leaf freezing [K]
              DILEFW          => noahmp%biochem%param%DILEFW         ,& ! in,    coeficient for leaf water stress death [1/s]
              DILEFC          => noahmp%biochem%param%DILEFC         ,& ! in,    coeficient for leaf temperature stress death [1/s]
              FRAGR           => noahmp%biochem%param%FRAGR          ,& ! in,    fraction of growth respiration  !original was 0.3
              TMIN            => noahmp%biochem%param%TMIN           ,& ! in,    minimum temperature for photosynthesis (k)
              MRP             => noahmp%biochem%param%MRP            ,& ! in,    microbial respiration parameter (umol co2/kg c/s)
              FOLNMX          => noahmp%biochem%param%FOLNMX         ,& ! in,    foliage nitrogen concentration when f(n)=1 (%)
              ARM             => noahmp%biochem%param%ARM            ,& ! in,    q10 for maintenance respiration
              RMF25           => noahmp%biochem%param%RMF25          ,& ! in,    leaf maintenance respiration at 25c (umol co2/m**2/s)
              RMR25           => noahmp%biochem%param%RMR25          ,& ! in,    root maintenance respiration at 25c (umol co2/kg bio/s)
              RMS25           => noahmp%biochem%param%RMS25          ,& ! in,    stem maintenance respiration at 25c (umol co2/kg bio/s)
              WDPOOL          => noahmp%biochem%param%WDPOOL         ,& ! in,    wood pool (switch 1 or 0) depending on woody or not
              RTOVRC          => noahmp%biochem%param%RTOVRC         ,& ! in,    root turnover coefficient [1/s]
              RSDRYC          => noahmp%biochem%param%RSDRYC         ,& ! in,    degree of drying that reduces soil respiration [-]
              RSWOODC         => noahmp%biochem%param%RSWOODC        ,& ! in,    wood respiration coeficient [1/s]
              BF              => noahmp%biochem%param%BF             ,& ! in,    parameter for present wood allocation [-]
              WSTRC           => noahmp%biochem%param%WSTRC          ,& ! in,    water stress coeficient [-]
              LAIMIN          => noahmp%biochem%param%LAIMIN         ,& ! in,    minimum leaf area index [m2/m2]
              XSAMIN          => noahmp%biochem%param%XSAMIN         ,& ! in,    minimum stem area index [m2/m2]
              IndexGrowSeason             => noahmp%biochem%state%IndexGrowSeason            ,& ! in,    growing season index (0=off, 1=on)
              NitrogenConcFoliage => noahmp%biochem%state%NitrogenConcFoliage ,& ! in,    foliage nitrogen concentration (%)
              LeafAreaPerMass            => noahmp%biochem%state%LeafAreaPerMass           ,& ! in,    leaf area per unit mass [m2/g]
              PSN             => noahmp%biochem%flux%PSN             ,& ! in,    total leaf photosynthesis (umol co2 /m2 /s)
              WROOT           => noahmp%water%state%WROOT            ,& ! in,    root zone soil water [-]
              WSTRES          => noahmp%water%state%WSTRES           ,& ! in,    water stress coeficient [-]  (1. for wilting)
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
              RespReductonFac              => noahmp%biochem%state%RespReductonFac             ,& ! out,   respiration reduction factor (<= 1)
              StemMassMin          => noahmp%biochem%state%StemMassMin         ,& ! out,   minimum stem mass [g/m2]
              StemAreaPerMass            => noahmp%biochem%state%StemAreaPerMass           ,& ! out,   stem area per unit mass [m2/g]
              CARBFX          => noahmp%biochem%flux%CARBFX          ,& ! out,   carbon assimilated rate [gC/m2/s]
              ADDNPPLF        => noahmp%biochem%flux%ADDNPPLF       ,& ! out,   leaf assimil after resp. losses removed [g/m2/s]
              ADDNPPST        => noahmp%biochem%flux%ADDNPPST       ,& ! out,   stem assimil after resp. losses removed [g/m2/s]
              GPP             => noahmp%biochem%flux%GPP             ,& ! out,   net instantaneous assimilation [g/m2/s C]
              NPP             => noahmp%biochem%flux%NPP             ,& ! out,   net primary productivity [g/m2/s C]
              NEE             => noahmp%biochem%flux%NEE             ,& ! out,   net ecosystem exchange [g/m2/s CO2]
              AUTORS          => noahmp%biochem%flux%AUTORS          ,& ! out,   net ecosystem respiration [g/m2/s C]
              HETERS          => noahmp%biochem%flux%HETERS          ,& ! out,   organic respiration [g/m2/s C]
              CFLUX           => noahmp%biochem%flux%CFLUX           ,& ! out,   carbon flux to atmosphere [g/m2/s]
              NPPL            => noahmp%biochem%flux%NPPL            ,& ! out,   leaf net primary productivity [g/m2/s]
              NPPR            => noahmp%biochem%flux%NPPR            ,& ! out,   root net primary productivity [g/m2/s]
              NPPW            => noahmp%biochem%flux%NPPW            ,& ! out,   wood net primary productivity [g/m2/s]
              NPPS            => noahmp%biochem%flux%NPPS            ,& ! out,   stem net primary productivity [g/m2/s]
              GRLEAF          => noahmp%biochem%flux%GRLEAF          ,& ! out,   growth respiration rate for leaf [g/m2/s]
              GRROOT          => noahmp%biochem%flux%GRROOT          ,& ! out,   growth respiration rate for root [g/m2/s]
              GRWOOD          => noahmp%biochem%flux%GRWOOD          ,& ! out,   growth respiration rate for wood [g/m2/s]
              GRSTEM          => noahmp%biochem%flux%GRSTEM          ,& ! out,   growth respiration rate for stem [g/m2/s]
              LFDEL           => noahmp%biochem%flux%LFDEL           ,& ! out,   maximum leaf mass available to change [g/m2/s]
              STABLC          => noahmp%biochem%flux%STABLC          ,& ! out,   decay rate of fast carbon to slow carbon [g/m2/s]
              RESP            => noahmp%biochem%flux%RESP            ,& ! out,   leaf respiration [umol/m2/s]
              RSSTEM          => noahmp%biochem%flux%RSSTEM          ,& ! out,   stem respiration [g/m2/s]
              RSWOOD          => noahmp%biochem%flux%RSWOOD          ,& ! out,   wood respiration [g/m2/s]
              RSLEAF          => noahmp%biochem%flux%RSLEAF          ,& ! out,   leaf maintenance respiration [g/m2/s]
              RSROOT          => noahmp%biochem%flux%RSROOT          ,& ! out,   fine root respiration [g/m2/s]
              RSSOIL          => noahmp%biochem%flux%RSSOIL          ,& ! out,   soil respiration [g/m2/s]
              DIELF           => noahmp%biochem%flux%DIELF           ,& ! out,   death rate of leaf mass [g/m2/s]
              DIEST           => noahmp%biochem%flux%DIEST           ,& ! out,   death rate of stem mass [g/m2/s]
              LFTOVR          => noahmp%biochem%flux%LFTOVR          ,& ! out,   stem turnover rate [g/m2/s]
              STTOVR          => noahmp%biochem%flux%STTOVR          ,& ! out,   stem turnover rate [g/m2/s]
              WDTOVR          => noahmp%biochem%flux%WDTOVR          ,& ! out,   wood turnover rate [g/m2/s]
              RTTOVR          => noahmp%biochem%flux%RTTOVR          ,& ! out,   root carbon loss rate by turnover [g/m2/s]
              STDEL           => noahmp%biochem%flux%STDEL            & ! out,   maximum steam mass available to change [g/m2/s]
             )
!-----------------------------------------------------------------------

    ! initialization
    StemAreaPerMass    = 3.0 * 0.001      ! m2/kg -->m2/g
    LeafMassMin  = LAIMIN / LeafAreaPerMass
    StemMassMin  = XSAMIN / StemAreaPerMass
        
    ! respiration
    if ( IndexGrowSeason == 0.0 ) then
       RespReductonFac = 0.5
    else
       RespReductonFac = 1.0
    endif             
    RespFacNitrogenFoliage    = min( NitrogenConcFoliage / max(1.0e-06,FOLNMX), 1.0 )
    RespFacTemperature     = ARM**((TV - 298.16) / 10.0)
    RESP   = RMF25 * RespFacTemperature * RespFacNitrogenFoliage * XLAI * RespReductonFac * (1.0 - WSTRES)           ! umol/m2/s
    RSLEAF = min( (LeafMass-LeafMassMin)/MainTimeStep, RESP*12.0e-6 )                 ! g/m2/s
    RSROOT = RMR25 * (RootMass*1.0e-3) * RespFacTemperature * RespReductonFac * 12.0e-6             ! g/m2/s
    RSSTEM = RMS25 * ((StemMass-StemMassMin) * 1.0e-3) * RespFacTemperature * RespReductonFac * 12.0e-6  ! g/m2/s
    RSWOOD = RSWOODC * r(TV) * WoodMass * WDPOOL
    
    !!! carbon assimilation start
    ! 1 mole -> 12 g carbon or 44 g CO2; 1 umol -> 12.e-6 g carbon;   
    CARBFX  = PSN * 12.0e-6      ! umol co2 /m2/ s -> g/m2/s carbon

    ! fraction of carbon into leaf versus nonleaf
    CarbonFracToLeaf = exp(0.01 * (1.0 - exp(0.75*XLAI)) * XLAI)
    if ( VegType == IndexEBLForest ) CarbonFracToLeaf = exp(0.01 * (1.0 - exp(0.50*XLAI)) * XLAI)
    CarbonFracToWoodRoot = 1.0 - CarbonFracToLeaf
    CarbonFracToStem = XLAI / 10.0 * CarbonFracToLeaf
    CarbonFracToLeaf = CarbonFracToLeaf - CarbonFracToStem
      
    !  fraction of carbon into wood versus root 
    if ( WoodMass > 1.0e-6 ) then
       WoodCarbonFrac = (1.0 - exp(-BF * (WRRAT*RootMass/WoodMass)) / BF) * WDPOOL
    else
       WoodCarbonFrac = WDPOOL
    endif   
    CarbonFracToRoot = CarbonFracToWoodRoot * (1.0 - WoodCarbonFrac)
    CarbonFracToWood = CarbonFracToWoodRoot * WoodCarbonFrac

    ! leaf and root turnover per time step  
    LFTOVR = LTOVRC * 5.0e-7 * LeafMass
    STTOVR = LTOVRC * 5.0e-7 * StemMass
    RTTOVR = RTOVRC * RootMass
    WDTOVR = 9.5e-10 * WoodMass
       
    ! seasonal leaf die rate dependent on temp and water stress
    ! water stress is set to 1 at permanent wilting point      
    SC    = exp(-0.3 * max(0.0, TV-TDLEF)) * (LeafMass / 120.0) 
    SD    = exp((WSTRES - 1.0) * WSTRC)
    DIELF = LeafMass * 1.0e-6 * (DILEFW * SD + DILEFC * SC)
    DIEST = StemMass * 1.0e-6 * (DILEFW * SD + DILEFC * SC)
     
    ! calculate growth respiration for leaf, root and wood 
    GRLEAF = max( 0.0, FRAGR * (CarbonFracToLeaf*CARBFX - RSLEAF) )
    GRSTEM = max( 0.0, FRAGR * (CarbonFracToStem*CARBFX - RSSTEM) )
    GRROOT = max( 0.0, FRAGR * (CarbonFracToRoot*CARBFX - RSROOT) )
    GRWOOD = max( 0.0, FRAGR * (CarbonFracToWood*CARBFX - RSWOOD) )
        
    ! Impose lower T limit for photosynthesis
    ADDNPPLF = max( 0.0, CarbonFracToLeaf*CARBFX - GRLEAF - RSLEAF )
    ADDNPPST = max( 0.0, CarbonFracToStem*CARBFX - GRSTEM - RSSTEM )
    !ADDNPPLF = CarbonFracToLeaf*CARBFX - GRLEAF - RSLEAF  ! MB: test Kjetil 
    !ADDNPPST = CarbonFracToStem*CARBFX - GRSTEM - RSSTEM  ! MB: test Kjetil 
    if ( TV < TMIN ) ADDNPPLF = 0.0
    if ( TV < TMIN ) ADDNPPST = 0.0
     
    ! update leaf, root, and wood carbon
    ! avoid reducing leaf mass below its minimum value but conserve mass
    LFDEL = (LeafMass - LeafMassMin) / MainTimeStep
    STDEL = (StemMass - StemMassMin) / MainTimeStep
    DIELF = min( DIELF, LFDEL+ADDNPPLF-LFTOVR )
    DIEST = min( DIEST, STDEL+ADDNPPST-STTOVR )
      
    ! net primary productivities
    NPPL  = max( ADDNPPLF, -LFDEL )
    NPPS  = max( ADDNPPST, -STDEL )
    NPPR  = CarbonFracToRoot * CARBFX - RSROOT - GRROOT
    NPPW  = CarbonFracToWood * CARBFX - RSWOOD - GRWOOD
       
    ! masses of plant components
    LeafMass = LeafMass + (NPPL - LFTOVR - DIELF) * MainTimeStep
    StemMass = StemMass + (NPPS - STTOVR - DIEST) * MainTimeStep   ! g/m2
    RootMass = RootMass + (NPPR - RTTOVR) * MainTimeStep
    if ( RootMass < 0.0 ) then
       RTTOVR = NPPR
       RootMass = 0.0
    endif 
    WoodMass = (WoodMass + (NPPW - WDTOVR) * MainTimeStep ) * WDPOOL

    ! soil carbon budgets 
    CarbonMassShallowSoil = CarbonMassShallowSoil + (RTTOVR+LFTOVR+STTOVR+WDTOVR+DIELF+DIEST) * MainTimeStep  ! MB: add DIEST v3.7
    MicroRespFactorSoilTemp    = 2.0**( (STC(1) - 283.16) / 10.0 )
    MicroRespFactorSoilWater    = WROOT / (0.20 + WROOT) * 0.23 / (0.23 + WROOT)
    RSSOIL = MicroRespFactorSoilWater * MicroRespFactorSoilTemp * MRP * max(0.0, CarbonMassShallowSoil*1.0e-3) * 12.0e-6
    STABLC = 0.1 * RSSOIL
    CarbonMassShallowSoil = CarbonMassShallowSoil - (RSSOIL + STABLC) * MainTimeStep
    CarbonMassDeepSoil = CarbonMassDeepSoil + STABLC * MainTimeStep
     
    !  total carbon flux     
    CFLUX  = - CARBFX + RSLEAF + RSROOT + RSWOOD + RSSTEM &     ! MB: add RSSTEM,GRSTEM,0.9*RSSOIL v3.7
             + 0.9*RSSOIL + GRLEAF + GRROOT + GRWOOD + GRSTEM   ! g/m2/s

    ! for outputs   
    GPP    = CARBFX                                             !g/m2/s C
    NPP    = NPPL + NPPW + NPPR +NPPS                           !g/m2/s C
    AUTORS = RSROOT + RSWOOD + RSLEAF + RSSTEM + &              !g/m2/s C  MB: add RSSTEM, GRSTEM v3.7
             GRLEAF + GRROOT + GRWOOD + GRSTEM                  !g/m2/s C  MB: add 0.9* v3.7
    HETERS = 0.9 * RSSOIL                                       !g/m2/s C
    NEE    = (AUTORS + HETERS - GPP) * 44.0 / 12.0              !g/m2/s CO2
    CarbonMassSoilTot  = CarbonMassShallowSoil + CarbonMassDeepSoil                                    !g/m2   C
    CarbonMassLiveTot  = LeafMass + RootMass + StemMass + WoodMass      !g/m2   C  MB: add StemMass v3.7
    
    ! leaf area index and stem area index
    XLAI   = max( LeafMass*LeafAreaPerMass, LAIMIN )
    XSAI   = max( StemMass*StemAreaPerMass, XSAMIN )

    end associate

  end subroutine CarbonFluxNatureVeg

end module CarbonFluxNatureVegMod
