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
    ! Respiration as a function of temperature
    real(kind=kind_noahmp)           :: r,x
    r(x) = exp(0.08 * (x - 298.16))

!------------------------------------------------------------------------
    associate(                                                        &
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,    main noahmp timestep (s)
              RTOVRC          => noahmp%biochem%param%RTOVRC         ,& ! in,    root turnover coefficient [1/s]
              RSDRYC          => noahmp%biochem%param%RSDRYC         ,& ! in,    degree of drying that reduces soil respiration [-]
              RSWOODC         => noahmp%biochem%param%RSWOODC        ,& ! in,    wood respiration coeficient [1/s]
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
              PSN             => noahmp%biochem%flux%PSN             ,& ! in,    total leaf photosynthesis (umol co2 /m2 /s)
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
              CARBFX          => noahmp%biochem%flux%CARBFX          ,& ! out,   carbon assimilated rate [gC/m2/s]
              CBHYDRAFX       => noahmp%biochem%flux%CBHYDRAFX       ,& ! out,   carbonhydrate assimilated rate [g/m2/s]
              ADDNPPLF        => noahmp%biochem%flux%ADDNPPLF       ,& ! out,   leaf assimil after resp. losses removed [g/m2/s]
              ADDNPPST        => noahmp%biochem%flux%ADDNPPST       ,& ! out,   stem assimil after resp. losses removed [g/m2/s]
              LFTOVR          => noahmp%biochem%flux%LFTOVR          ,& ! out,   stem turnover rate [g/m2/s]
              STTOVR          => noahmp%biochem%flux%STTOVR          ,& ! out,   stem turnover rate [g/m2/s]
              WDTOVR          => noahmp%biochem%flux%WDTOVR          ,& ! out,   wood turnover rate [g/m2/s]
              RTTOVR          => noahmp%biochem%flux%RTTOVR          ,& ! out,   root carbon loss rate by turnover [g/m2/s]
              GRTOVR          => noahmp%biochem%flux%GRTOVR          ,& ! out,   grain turnover rate [g/m2/s]
              LFCONVERT       => noahmp%biochem%flux%LFCONVERT       ,& ! out,   leaf to grain conversion [g/m2/s]
              RTCONVERT       => noahmp%biochem%flux%RTCONVERT       ,& ! out,   root to grain conversion [g/m2/s]
              STCONVERT       => noahmp%biochem%flux%STCONVERT       ,& ! out,   stem to grain conversion [g/m2/s]
              AUTORS          => noahmp%biochem%flux%AUTORS          ,& ! out,   net ecosystem respiration [g/m2/s C]
              CFLUX           => noahmp%biochem%flux%CFLUX           ,& ! out,   carbon flux to atmosphere [g/m2/s]
              GPP             => noahmp%biochem%flux%GPP             ,& ! out,   net instantaneous assimilation [g/m2/s C]
              NPP             => noahmp%biochem%flux%NPP             ,& ! out,   net primary productivity [g/m2/s C]
              NPPL            => noahmp%biochem%flux%NPPL            ,& ! out,   leaf net primary productivity [g/m2/s]
              NPPR            => noahmp%biochem%flux%NPPR            ,& ! out,   root net primary productivity [g/m2/s]
              NPPW            => noahmp%biochem%flux%NPPW            ,& ! out,   wood net primary productivity [g/m2/s]
              NPPS            => noahmp%biochem%flux%NPPS            ,& ! out,   stem net primary productivity [g/m2/s]
              NPPG            => noahmp%biochem%flux%NPPG            ,& ! out,   grain net primary productivity [g/m2/s]
              NEE             => noahmp%biochem%flux%NEE             ,& ! out,   net ecosystem exchange [g/m2/s CO2]
              GRGRAIN         => noahmp%biochem%flux%GRGRAIN         ,& ! out,   growth respiration rate for grain [g/m2/s]
              GRLEAF          => noahmp%biochem%flux%GRLEAF          ,& ! out,   growth respiration rate for leaf [g/m2/s]
              GRROOT          => noahmp%biochem%flux%GRROOT          ,& ! out,   growth respiration rate for root [g/m2/s]
              GRWOOD          => noahmp%biochem%flux%GRWOOD          ,& ! out,   growth respiration rate for wood [g/m2/s]
              GRSTEM          => noahmp%biochem%flux%GRSTEM          ,& ! out,   growth respiration rate for stem [g/m2/s]
              HETERS          => noahmp%biochem%flux%HETERS          ,& ! out,   organic respiration [g/m2/s C]
              LFDEL           => noahmp%biochem%flux%LFDEL           ,& ! out,   maximum leaf mass available to change [g/m2/s]
              STDEL           => noahmp%biochem%flux%STDEL           ,& ! out,   maximum steam  mass available to change [g/m2/s]
              RESP            => noahmp%biochem%flux%RESP            ,& ! out,   leaf respiration [umol/m2/s]
              RSSTEM          => noahmp%biochem%flux%RSSTEM          ,& ! out,   stem respiration [g/m2/s]
              RSLEAF          => noahmp%biochem%flux%RSLEAF          ,& ! out,   leaf maintenance respiration [g/m2/s]
              RSROOT          => noahmp%biochem%flux%RSROOT          ,& ! out,   fine root respiration [g/m2/s]
              RSSOIL          => noahmp%biochem%flux%RSSOIL          ,& ! out,   soil respiration [g/m2/s]
              RSWOOD          => noahmp%biochem%flux%RSWOOD          ,& ! out,   wood respiration [g/m2/s]
              RSGRAIN         => noahmp%biochem%flux%RSGRAIN         ,& ! out,   grain respiration [g/m2/s]
              DIELF           => noahmp%biochem%flux%DIELF           ,& ! out,   death rate of leaf mass [g/m2/s]
              DIEST           => noahmp%biochem%flux%DIEST           ,& ! out,   death rate of stem mass [g/m2/s]
              STABLC          => noahmp%biochem%flux%STABLC           & ! out,   decay rate of fast carbon to slow carbon [g/m2/s]
             )
!----------------------------------------------------------------------

    ! initialization
    StemAreaPerMass   = 3.0 * 0.001         ! m2/kg -->m2/g
    LeafMassMin = LAIMIN / 0.035
    StemMassMin = XSAMIN / StemAreaPerMass

    !!! carbon assimilation starts
    ! 1 mole -> 12 g carbon or 44 g CO2 or 30 g CH20
    CARBFX    = PSN * 12.0e-6   !*IndexPlanting   !umol co2 /m2/ s -> g/m2/s C
    CBHYDRAFX = PSN * 30.0e-6   !*IndexPlanting

    ! mainteinance respiration
    RespFacNitrogenFoliage     = min( NitrogenConcFoliage / max(1.0e-06, FOLN_MX), 1.0 )
    RespFacTemperature      = Q10MR**((TV - 298.16) / 10.0)
    RESP    = LFMR25 * RespFacTemperature * RespFacNitrogenFoliage * XLAI * (1.0 - WSTRES)         ! umol/m2/s
    RSLEAF  = min( (LeafMass - LeafMassMin) / MainTimeStep, RESP*30.0e-6 )       ! g/m2/s
    RSROOT  = RTMR25 * (RootMass * 1.0e-3) * RespFacTemperature * 30.0e-6         ! g/m2/s
    RSSTEM  = STMR25 * (StemMass * 1.0e-3) * RespFacTemperature * 30.0e-6         ! g/m2/s
    RSGRAIN = GRAINMR25 * (GrainMass * 1.0e-3) * RespFacTemperature * 30.0e-6       ! g/m2/s

    ! calculate growth respiration for leaf, root and grain
    GRLEAF  = max( 0.0, FRA_GR * (LFPT(PlantGrowStage)*CBHYDRAFX - RSLEAF) )
    GRSTEM  = max( 0.0, FRA_GR * (STPT(PlantGrowStage)*CBHYDRAFX - RSSTEM) )
    GRROOT  = max( 0.0, FRA_GR * (RTPT(PlantGrowStage)*CBHYDRAFX - RSROOT) )
    GRGRAIN = max( 0.0, FRA_GR * (GRAINPT(PlantGrowStage)*CBHYDRAFX - RSGRAIN) )

    ! leaf turnover, stem turnover, root turnover and leaf death caused by soil water and soil temperature stress
    LFTOVR  = LF_OVRC(PlantGrowStage) * 1.0e-6 * LeafMass
    RTTOVR  = RT_OVRC(PlantGrowStage) * 1.0e-6 * RootMass
    STTOVR  = ST_OVRC(PlantGrowStage) * 1.0e-6 * StemMass
    SC      = exp( -0.3 * max(0.0, TV-LEFREEZ) ) * (LeafMass/120.0)
    SD      = exp( (WSTRES - 1.0) * WSTRC )
    DIELF   = LeafMass * 1.0e-6 * (DILE_FW(PlantGrowStage) * SD + DILE_FC(PlantGrowStage) * SC)

    ! Allocation of CBHYDRAFX to leaf, stem, root and grain at each growth stage
    ADDNPPLF = max( 0.0, LFPT(PlantGrowStage)*CBHYDRAFX - GRLEAF - RSLEAF )
    ADDNPPLF = LFPT(PlantGrowStage)*CBHYDRAFX - GRLEAF - RSLEAF
    ADDNPPST = max( 0.0, STPT(PlantGrowStage)*CBHYDRAFX - GRSTEM - RSSTEM )
    ADDNPPST = STPT(PlantGrowStage)*CBHYDRAFX - GRSTEM - RSSTEM
    
    ! avoid reducing leaf mass below its minimum value but conserve mass
    LFDEL  = (LeafMass - LeafMassMin) / MainTimeStep
    STDEL  = (StemMass - StemMassMin) / MainTimeStep
    LFTOVR = min( LFTOVR, LFDEL+ADDNPPLF )
    STTOVR = min( STTOVR, STDEL+ADDNPPST )
    DIELF  = min( DIELF, LFDEL+ADDNPPLF-LFTOVR )

    ! net primary productivities
    NPPL   = max( ADDNPPLF, -LFDEL )
    NPPL   = ADDNPPLF
    NPPS   = max( ADDNPPST, -STDEL )
    NPPS   = ADDNPPST
    NPPR   = RTPT(PlantGrowStage) * CBHYDRAFX - RSROOT - GRROOT
    NPPG   = GRAINPT(PlantGrowStage) * CBHYDRAFX - RSGRAIN - GRGRAIN

    ! masses of plant components
    LeafMass = LeafMass + (NPPL - LFTOVR - DIELF) * MainTimeStep
    StemMass = StemMass + (NPPS - STTOVR) * MainTimeStep       ! g/m2
    RootMass = RootMass + (NPPR - RTTOVR) * MainTimeStep
    GrainMass  = GrainMass + NPPG * MainTimeStep 
    GPP    = CBHYDRAFX * 0.4                                  ! g/m2/s C  0.4=12/30, CH20 to C

    ! carbon convert to grain
    LFCONVERT = 0.0              ! Zhe Zhang 2020-07-13
    STCONVERT = 0.0
    RTCONVERT = 0.0
    LFCONVERT = LeafMass * (LFCT(PlantGrowStage) * MainTimeStep / 3600.0)
    STCONVERT = StemMass * (STCT(PlantGrowStage) * MainTimeStep / 3600.0)
    RTCONVERT = RootMass * (RTCT(PlantGrowStage) * MainTimeStep / 3600.0)
    LeafMass    = LeafMass - LFCONVERT
    StemMass    = StemMass - STCONVERT
    RootMass    = RootMass - RTCONVERT
    GrainMass     = GrainMass + STCONVERT + RTCONVERT + LFCONVERT
    !if ( PlantGrowStage==6 ) then
    !   STCONVERT = StemMass * (0.00005 * MainTimeStep / 3600.0)
    !   StemMass    = StemMass - STCONVERT
    !   RTCONVERT = RootMass * (0.0005 * MainTimeStep / 3600.0)
    !   RootMass    = RootMass - RTCONVERT
    !   GrainMass     = GrainMass + STCONVERT + RTCONVERT
    !endif
    
    if ( RootMass < 0.0 ) then
       RTTOVR = NPPR
       RootMass = 0.0
    endif
    if ( GrainMass < 0.0 ) then
       GrainMass = 0.0
    endif

    ! soil carbon budgets
    !if ( (PlantGrowStage == 1) .or. (PlantGrowStage == 2) .or. (PlantGrowStage == 8) ) then
    !   CarbonMassShallowSoil=1000
    !else
    CarbonMassShallowSoil = CarbonMassShallowSoil + (RTTOVR+LFTOVR+STTOVR+DIELF) * MainTimeStep 
    !endif
    MicroRespFactorSoilTemp    = 2.0**((STC(1) - 283.16) / 10.0)
    MicroRespFactorSoilWater    = WROOT / (0.20 + WROOT) * 0.23 / (0.23 + WROOT)
    RSSOIL = MicroRespFactorSoilWater * MicroRespFactorSoilTemp * MRP * max(0.0, CarbonMassShallowSoil*1.0e-3) * 12.0e-6
    STABLC = 0.1 * RSSOIL
    CarbonMassShallowSoil = CarbonMassShallowSoil - (RSSOIL + STABLC) * MainTimeStep
    CarbonMassDeepSoil = CarbonMassDeepSoil + STABLC * MainTimeStep
 
    !  total carbon flux
    CFLUX  = - CARBFX + RSLEAF + RSROOT + RSSTEM &
             + RSSOIL + GRLEAF + GRROOT                           !g/m2/s 0.4=12/30, CH20 to C

    ! for outputs
    NPP    = (NPPL + NPPS + NPPR + NPPG) * 0.4                    !g/m2/s C  0.4=12/30, CH20 to C 
    AUTORS = RSROOT + RSGRAIN + RSLEAF + &                        !g/m2/s C
             GRLEAF + GRROOT + GRGRAIN                            !g/m2/s C
    HETERS = RSSOIL                                               !g/m2/s C
    NEE    = (AUTORS + HETERS - GPP) * 44.0 / 30.0                !g/m2/s CO2
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
