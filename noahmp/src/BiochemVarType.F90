module BiochemVarType

!!! Define column (1-D) Noah-MP Biochemistry (carbon,nitrogen,etc) variables
!!! Biochemistry variable initialization is done in BiochemVarInitMod.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use Machine

  implicit none
  save
  private

!=== define "flux" sub-type of biochem_type (biochem%flux%variable)
  type :: flux_type

    ! define specific biochem flux variables
    real(kind=kind_noahmp) :: PhotosynTotal              ! total leaf photosynthesis [umol co2/m2/s]
    real(kind=kind_noahmp) :: PhotosynLeafSunlit         ! sunlit leaf photosynthesis [umol co2/m2/s]
    real(kind=kind_noahmp) :: PhotosynLeafShade          ! shaded leaf photosynthesis [umol co2/m2/s]
    real(kind=kind_noahmp) :: PhotosynCrop               ! crop photosynthesis rate [umol co2/m2/s]
    real(kind=kind_noahmp) :: GrossPriProduction         ! gross primary production [g/m2/s C]
    real(kind=kind_noahmp) :: NetEcoExchange             ! net ecosystem exchange [g/m2/s CO2]
    real(kind=kind_noahmp) :: NetPriProductionTot        ! total net primary production [g/m2/s C]
    real(kind=kind_noahmp) :: NetPriProductionLeaf       ! leaf net primary production [g/m2/s]
    real(kind=kind_noahmp) :: NetPriProductionRoot       ! root net primary production [g/m2/s]
    real(kind=kind_noahmp) :: NetPriProductionWood       ! wood net primary production [g/m2/s]
    real(kind=kind_noahmp) :: NetPriProductionStem       ! stem net primary production [g/m2/s]
    real(kind=kind_noahmp) :: NetPriProductionGrain      ! grain net primary production [g/m2/s] 
    real(kind=kind_noahmp) :: RespirationPlantTot        ! total plant respiration (leaf,stem,root,wood,grain) [g/m2/s C]
    real(kind=kind_noahmp) :: RespirationSoilOrg         ! soil heterotrophic (organic) respiration [g/m2/s C]
    real(kind=kind_noahmp) :: CarbonToAtmos              ! carbon flux to atmosphere [g/m2/s]
    real(kind=kind_noahmp) :: GrowthRespLeaf             ! growth respiration rate for leaf [g/m2/s]
    real(kind=kind_noahmp) :: GrowthRespRoot             ! growth respiration rate for root [g/m2/s]
    real(kind=kind_noahmp) :: GrowthRespWood             ! growth respiration rate for wood [g/m2/s]
    real(kind=kind_noahmp) :: GrowthRespStem             ! growth respiration rate for stem [g/m2/s]
    real(kind=kind_noahmp) :: GrowthRespGrain            ! growth respiration rate for grain [g/m2/s]
    real(kind=kind_noahmp) :: LeafMassMaxChg             ! maximum leaf mass available to change [g/m2/s]
    real(kind=kind_noahmp) :: StemMassMaxChg             ! maximum stem mass available to change [g/m2/s]
    real(kind=kind_noahmp) :: CarbonDecayToStable        ! decay rate of fast carbon to slow carbon [g/m2/s]
    real(kind=kind_noahmp) :: RespirationLeaf            ! leaf respiration [g/m2/s]
    real(kind=kind_noahmp) :: RespirationStem            ! stem respiration [g/m2/s]
    real(kind=kind_noahmp) :: RespirationWood            ! wood respiration rate [g/m2/s]
    real(kind=kind_noahmp) :: RespirationLeafMaint       ! leaf maintenance respiration rate [g/m2/s]
    real(kind=kind_noahmp) :: RespirationRoot            ! fine root respiration rate [g/m2/s]
    real(kind=kind_noahmp) :: RespirationSoil            ! soil respiration rate [g/m2/s]
    real(kind=kind_noahmp) :: RespirationGrain           ! grain respiration rate [g/m2/s]
    real(kind=kind_noahmp) :: ConvRootToGrain            ! root to grain conversion [g/m2/s]
    real(kind=kind_noahmp) :: ConvStemToGrain            ! stem to grain conversion [g/m2/s]
    real(kind=kind_noahmp) :: ConvLeafToGrain            ! leaf to grain conversion [g/m2/s]
    real(kind=kind_noahmp) :: TurnoverLeaf               ! leaf turnover rate [g/m2/s]
    real(kind=kind_noahmp) :: TurnoverStem               ! stem turnover rate [g/m2/s]
    real(kind=kind_noahmp) :: TurnoverWood               ! wood turnover rate [g/m2/s]
    real(kind=kind_noahmp) :: TurnoverRoot               ! root turnover rate [g/m2/s]
    real(kind=kind_noahmp) :: TurnoverGrain              ! grain turnover rate [g/m2/s]
    real(kind=kind_noahmp) :: DeathLeaf                  ! death rate of leaf mass [g/m2/s]
    real(kind=kind_noahmp) :: DeathStem                  ! death rate of stem mass [g/m2/s]
    real(kind=kind_noahmp) :: CarbonAssim                ! carbon assimilated rate [g/m2/s]
    real(kind=kind_noahmp) :: CarbonHydrateAssim         ! carbonhydrate assimilated rate [g/m2/s]

  end type flux_type


!=== define "state" sub-type of biochem_type (biochem%state%variable)
  type :: state_type

    ! define specific biochem state variables
    integer                :: PlantGrowStage             ! plant growing stage
    integer                :: IndexPlanting              ! Planting index (0=off, 1=on)
    integer                :: IndexHarvest               ! Harvest index (0=on,1=off)
    real(kind=kind_noahmp) :: IndexGrowSeason            ! growing season index (0=off, 1=on)    
    real(kind=kind_noahmp) :: NitrogenConcFoliage        ! foliage nitrogen concentration [%]
    real(kind=kind_noahmp) :: LeafMass                   ! leaf mass [g/m2]
    real(kind=kind_noahmp) :: RootMass                   ! mass of fine roots [g/m2]
    real(kind=kind_noahmp) :: StemMass                   ! stem mass [g/m2]
    real(kind=kind_noahmp) :: WoodMass                   ! mass of wood (include woody roots) [g/m2]
    real(kind=kind_noahmp) :: GrainMass                  ! mass of grain [g/m2]
    real(kind=kind_noahmp) :: CarbonMassDeepSoil         ! stable carbon in deep soil [g/m2]
    real(kind=kind_noahmp) :: CarbonMassShallowSoil      ! short-lived carbon in shallow soil [g/m2]
    real(kind=kind_noahmp) :: CarbonMassSoilTot          ! total soil carbon mass [g/m2 C]
    real(kind=kind_noahmp) :: CarbonMassLiveTot          ! total living carbon mass ([g/m2 C]
    real(kind=kind_noahmp) :: LeafAreaPerMass            ! leaf area per unit mass [m2/g]
    real(kind=kind_noahmp) :: StemAreaPerMass            ! stem area per unit mass (m2/g)
    real(kind=kind_noahmp) :: LeafMassMin                ! minimum leaf mass [g/m2]
    real(kind=kind_noahmp) :: StemMassMin                ! minimum stem mass [g/m2]
    real(kind=kind_noahmp) :: CarbonFracToLeaf           ! fraction of carbon flux allocated to leaves [-]
    real(kind=kind_noahmp) :: CarbonFracToRoot           ! fraction of carbon flux allocated to roots [-]
    real(kind=kind_noahmp) :: CarbonFracToWood           ! fraction of carbon flux allocated to wood [-]
    real(kind=kind_noahmp) :: CarbonFracToStem           ! fraction of carbon flux allocated to stem [-]
    real(kind=kind_noahmp) :: WoodCarbonFrac             ! wood carbon fraction in (root + wood) carbon [-]
    real(kind=kind_noahmp) :: CarbonFracToWoodRoot       ! fraction of carbon to root and wood [-]
    real(kind=kind_noahmp) :: MicroRespFactorSoilWater   ! soil water factor for microbial respiration
    real(kind=kind_noahmp) :: MicroRespFactorSoilTemp    ! soil temperature factor for microbial respiration
    real(kind=kind_noahmp) :: RespFacNitrogenFoliage     ! foliage nitrogen adjustemt factor to respiration (<= 1)
    real(kind=kind_noahmp) :: RespFacTemperature         ! temperature factor for respiration
    real(kind=kind_noahmp) :: RespReductionFac            ! respiration reduction factor (<= 1)
    real(kind=kind_noahmp) :: GrowDegreeDay              ! growing degree days

  end type state_type


!=== define "parameter" sub-type of biochem_type (biochem%param%variable)
  type :: parameter_type

    ! define specific biochem parameter variables
    integer                :: DatePlanting           ! Planting date
    integer                :: DateHarvest            ! Harvest date
    integer                :: PhotosynPath               ! photosynthetic pathway:  1 = c3 2 = c4
    real(kind=kind_noahmp) :: FOLNMX           ! foliage nitrogen concentration when f(n)=1 (%)
    real(kind=kind_noahmp) :: QE25             ! quantum efficiency at 25c (umol co2 / umol photon)
    real(kind=kind_noahmp) :: VCMX25           ! maximum rate of carboxylation at 25c (umol co2/m**2/s)
    real(kind=kind_noahmp) :: AVCMX            ! q10 for vcmx25
    real(kind=kind_noahmp) :: C3PSN            ! photosynthetic pathway: 0. = c4, 1. = c3
    real(kind=kind_noahmp) :: MP               ! slope of conductance-to-photosynthesis relationship
    real(kind=kind_noahmp) :: TMIN               ! minimum temperature for photosynthesis (k)
    real(kind=kind_noahmp) :: SLA                ! single-side leaf area per Kg [m2/kg] 
    real(kind=kind_noahmp) :: FOLN_MX            ! foliage nitrogen concentration when f(n)=1 (%)
    real(kind=kind_noahmp) :: ARM                ! q10 for maintenance respiration
    real(kind=kind_noahmp) :: RMF25              ! leaf maintenance respiration at 25c (umol co2/m**2/s)
    real(kind=kind_noahmp) :: RMS25              ! stem maintenance respiration at 25c (umol co2/kg bio/s)
    real(kind=kind_noahmp) :: RMR25              ! root maintenance respiration at 25c (umol co2/kg bio/s)
    real(kind=kind_noahmp) :: WRRAT              ! wood to non-wood ratio
    real(kind=kind_noahmp) :: WDPOOL             ! wood pool (switch 1 or 0) depending on woody or not [-]
    real(kind=kind_noahmp) :: LTOVRC             ! leaf turnover [1/s]
    real(kind=kind_noahmp) :: TDLEF              ! characteristic T for leaf freezing [K]
    real(kind=kind_noahmp) :: DILEFW             ! coeficient for leaf stress death [1/s]
    real(kind=kind_noahmp) :: DILEFC             ! coeficient for leaf stress death [1/s]
    real(kind=kind_noahmp) :: FRAGR              ! fraction of growth respiration  !original was 0.3 
    real(kind=kind_noahmp) :: MRP                ! microbial respiration parameter (umol co2 /kg c/ s)
    real(kind=kind_noahmp) :: Q10MR              ! q10 for maintainance respiration
    real(kind=kind_noahmp) :: LFMR25             ! leaf maintenance respiration at 25C [umol CO2/m**2  /s]
    real(kind=kind_noahmp) :: STMR25             ! stem maintenance respiration at 25C [umol CO2/kg bio/s]
    real(kind=kind_noahmp) :: RTMR25             ! root maintenance respiration at 25C [umol CO2/kg bio/s]
    real(kind=kind_noahmp) :: GRAINMR25          ! grain maintenance respiration at 25C [umol CO2/kg bio/s]
    real(kind=kind_noahmp) :: FRA_GR             ! fraction of growth respiration 
    real(kind=kind_noahmp) :: LEFREEZ            ! characteristic T for leaf freezing [K]
    real(kind=kind_noahmp) :: BIO2LAI            ! leaf are per living leaf biomass [m^2/kg]
    real(kind=kind_noahmp) :: TempBaseGrowDegDay           ! Base temperature for growing degree day (GDD) accumulation [C]
    real(kind=kind_noahmp) :: TempMaxGrowDegDay            ! Maximum temperature for growing degree day (GDD) accumulation [C]
    real(kind=kind_noahmp) :: GrowDegDayEmerg              ! growing degree day (GDD) from seeding to emergence
    real(kind=kind_noahmp) :: GrowDegDayInitVeg              ! growing degree day (GDD) from seeding to initial vegetative 
    real(kind=kind_noahmp) :: GrowDegDayPostVeg              ! growing degree day (GDD) from seeding to post vegetative 
    real(kind=kind_noahmp) :: GrowDegDayInitReprod              ! growing degree day (GDD) from seeding to intial reproductive
    real(kind=kind_noahmp) :: GrowDegDayMature              ! growing degree day (GDD) from seeding to pysical maturity 
    real(kind=kind_noahmp) :: I2PAR              ! Fraction of incoming solar radiation to photosynthetically active radiation 
    real(kind=kind_noahmp) :: TASSIM0            ! Minimum temperature for CO2 assimulation [C]
    real(kind=kind_noahmp) :: TASSIM1            ! CO2 assimulation linearly increasing until temperature reaches T1 [C]
    real(kind=kind_noahmp) :: TASSIM2            ! CO2 assmilation rate remain at CarbonAssimRefMax until temperature reaches T2 [C]
    real(kind=kind_noahmp) :: CarbonAssimRefMax               ! reference maximum CO2 assimilation rate [g co2/m2/s] 
    real(kind=kind_noahmp) :: K                  ! light extinction coefficient 
    real(kind=kind_noahmp) :: EPSI               ! initial light use efficiency 
    real(kind=kind_noahmp) :: CarbonAssimReducFac              ! CO2 assimilation reduction factor(0-1) (caused by non-modeling part,e.g.pest,weeds)
    real(kind=kind_noahmp) :: SLAREA             ! single leaf area [m2]?
    real(kind=kind_noahmp) :: XSAMIN             ! minimum stem area index [m2/m2]
    real(kind=kind_noahmp) :: BF                 ! parameter for present wood allocation [-]
    real(kind=kind_noahmp) :: WSTRC              ! water stress coeficient [-] 
    real(kind=kind_noahmp) :: LAIMIN             ! minimum leaf area index [m2/m2] 
    real(kind=kind_noahmp) :: RTOVRC             ! root turnover coefficient [1/s]
    real(kind=kind_noahmp) :: RSDRYC             ! degree of drying that reduces soil respiration [-]
    real(kind=kind_noahmp) :: RSWOODC            ! wood respiration coeficient [1/s]

    real(kind=kind_noahmp), allocatable, dimension(:)  :: DILE_FC    ! coeficient for temperature leaf stress death [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)  :: DILE_FW    ! coeficient for water leaf stress death [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)  :: LFCT       ! fraction of carbohydrate flux transallocate from leaf to grain ! Zhe Zhang 2020-07-13
    real(kind=kind_noahmp), allocatable, dimension(:)  :: STCT       ! fraction of carbohydrate flux transallocate from stem to grain
    real(kind=kind_noahmp), allocatable, dimension(:)  :: RTCT       ! fraction of carbohydrate flux transallocate from root to grain
    real(kind=kind_noahmp), allocatable, dimension(:)  :: LFPT       ! fraction of carbohydrate flux to leaf
    real(kind=kind_noahmp), allocatable, dimension(:)  :: STPT       ! fraction of carbohydrate flux to stem
    real(kind=kind_noahmp), allocatable, dimension(:)  :: RTPT       ! fraction of carbohydrate flux to root
    real(kind=kind_noahmp), allocatable, dimension(:)  :: GRAINPT    ! fraction of carbohydrate flux to grain
    real(kind=kind_noahmp), allocatable, dimension(:)  :: LF_OVRC    ! fraction of leaf turnover  [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)  :: ST_OVRC    ! fraction of stem turnover  [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)  :: RT_OVRC    ! fraction of root tunrover  [1/s]

  end type parameter_type


!=== define biochem type that includes 4 subtypes (flux,state,parameter,diagnose)
  type, public :: biochem_type

    type(flux_type)      :: flux
    type(state_type)     :: state
    type(parameter_type) :: param
    type(diagnose_type)  :: diag

  end type biochem_type

end module BiochemVarType
