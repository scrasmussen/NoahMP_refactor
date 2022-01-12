module BiochemVarType

!!! Define column (1-D) Noah-MP Biochemistry (carbon,nitrogen,etc) variables
!!! Biochemistry variable initialization is done in BiochemInit.f90

  use Machine, only : kind_noahmp

  implicit none
  save
  private

!=== define "flux" sub-type of biochem_type (biochem%flux%variable)
  type :: flux_type

    ! define specific biochem flux variables
    real(kind=kind_noahmp) :: PSN        ! total leaf photosyn (umolco2/m2/s) [+]
    real(kind=kind_noahmp) :: GPP        ! net instantaneous assimilation [g/m2/s C]
    real(kind=kind_noahmp) :: NPP        ! net primary productivity [g/m2/s C]
    real(kind=kind_noahmp) :: NEE        ! net ecosystem exchange [g/m2/s CO2]
    real(kind=kind_noahmp) :: AUTORS     ! net ecosystem respiration [g/m2/s C]
    real(kind=kind_noahmp) :: HETERS     ! organic respiration [g/m2/s C]
    real(kind=kind_noahmp) :: VOCFLX(5)  ! voc fluxes [ug C m-2 h-1]

    ! CarbonDiOxideFlux local variables 
    real(kind=kind_noahmp) :: CFLUX      ! carbon flux to atmosphere [g/m2/s]
    real(kind=kind_noahmp) :: NPPL       ! leaf net primary productivity [g/m2/s]
    real(kind=kind_noahmp) :: NPPR       ! root net primary productivity [g/m2/s]
    real(kind=kind_noahmp) :: NPPW       ! wood net primary productivity [g/m2/s]
    real(kind=kind_noahmp) :: NPPS       ! wood net primary productivity [g/m2/s]
    real(kind=kind_noahmp) :: GRLEAF     ! growth respiration rate for leaf [g/m2/s]
    real(kind=kind_noahmp) :: GRROOT     ! growth respiration rate for root [g/m2/s]
    real(kind=kind_noahmp) :: GRWOOD     ! growth respiration rate for wood [g/m2/s]
    real(kind=kind_noahmp) :: GRSTEM     ! growth respiration rate for stem [g/m2/s]
    real(kind=kind_noahmp) :: LFDEL      ! maximum  leaf mass  available to change [g/m2/s]
    real(kind=kind_noahmp) :: STDEL      ! maximum  stem mass  available to change [g/m2/s]
    real(kind=kind_noahmp) :: STABLC     ! decay rate of fast carbon to slow carbon [g/m2/s]
    real(kind=kind_noahmp) :: RESP       ! leaf respiration [umol/m2/s]
    real(kind=kind_noahmp) :: RSSTEM     ! stem respiration [g/m2/s]

    ! crop
    real(kind=kind_noahmp) :: GRGRAIN    ! growth respiration rate for stem [g/m2/s]
    real(kind=kind_noahmp) :: NPPG       ! grain net primary productivity [g/m2/s] 
    real(kind=kind_noahmp) :: RTCONVERT  ! root to grain conversion [g/m2/s]
    real(kind=kind_noahmp) :: STCONVERT  ! stem to grain conversion [g/m2/s]
    real(kind=kind_noahmp) :: PSNCROP    ! crop photosynthesis rate

  end type flux_type

!=== define "state" sub-type of biochem_type (biochem%state%variable)
  type :: state_type

    ! define specific biochem state variables
    integer                :: PGS        ! plant growing stage
    integer                :: IPA       !Planting index
    integer                :: IHA       !Havestindex(0=on,1=off)
    real(kind=kind_noahmp) :: IGS        ! growing season index (0=off, 1=on)
    real(kind=kind_noahmp) :: FOLN       ! foliage nitrogen (%)
    real(kind=kind_noahmp) :: LFMASS     ! leaf mass [g/m2]
    real(kind=kind_noahmp) :: RTMASS     ! mass of fine roots [g/m2]
    real(kind=kind_noahmp) :: STMASS     ! stem mass [g/m2]
    real(kind=kind_noahmp) :: WOOD       ! mass of wood (incl. woody roots) [g/m2]
    real(kind=kind_noahmp) :: STBLCP     ! stable carbon in deep soil [g/m2]
    real(kind=kind_noahmp) :: FASTCP     ! short-lived carbon in shallow soil [g/m2]
    real(kind=kind_noahmp) :: TOTSC      ! total soil carbon [g/m2 C]
    real(kind=kind_noahmp) :: TOTLB      ! total living carbon ([g/m2 C]
    real(kind=kind_noahmp) :: LAPM       ! leaf area per unit mass [m2/g]
    real(kind=kind_noahmp) :: SAPM       ! stem area per unit mass (m2/g)
   
    ! CarbonDiOxideFlux local variables
    real(kind=kind_noahmp) :: LFMSMN     ! minimum leaf mass [g/m2]
    real(kind=kind_noahmp) :: STMSMN     ! minimum stem mass [g/m2]
    real(kind=kind_noahmp) :: RSWOOD     ! wood respiration [g/m2]
    real(kind=kind_noahmp) :: RSLEAF     ! leaf maintenance respiration per timestep [g/m2]
    real(kind=kind_noahmp) :: RSROOT     ! fine root respiration per time step [g/m2]
    real(kind=kind_noahmp) :: DIELF      ! death of leaf mass per time step [g/m2]
    real(kind=kind_noahmp) :: DIEST      ! death of stem mass per time step [g/m2]
    real(kind=kind_noahmp) :: ADDNPPLF   ! leaf assimil after resp. losses removed [g/m2]
    real(kind=kind_noahmp) :: ADDNPPST   ! stem assimil after resp. losses removed [g/m2]
    real(kind=kind_noahmp) :: CARBFX     ! carbon assimilated per model step [g/m2]
    real(kind=kind_noahmp) :: LEAFPT     ! fraction of carbon allocated to leaves [-]
    real(kind=kind_noahmp) :: LFTOVR     ! stem turnover per time step [g/m2]
    real(kind=kind_noahmp) :: STTOVR     ! stem turnover per time step [g/m2]
    real(kind=kind_noahmp) :: WDTOVR     ! wood turnover per time step [g/m2]
    real(kind=kind_noahmp) :: RSSOIL     ! soil respiration per time step [g/m2]
    real(kind=kind_noahmp) :: RTTOVR     ! root carbon loss per time step by turnover [g/m2]
    real(kind=kind_noahmp) :: WOODF      ! calculated wood to root ratio [-]
    real(kind=kind_noahmp) :: NONLEF     ! fraction of carbon to root and wood [-]
    real(kind=kind_noahmp) :: ROOTPT     ! fraction of carbon flux to roots [-]
    real(kind=kind_noahmp) :: WOODPT     ! fraction of carbon flux to wood [-]
    real(kind=kind_noahmp) :: STEMPT     ! fraction of carbon flux to stem [-]
    real(kind=kind_noahmp) :: FSW        ! soil water factor for microbial respiration
    real(kind=kind_noahmp) :: FST        ! soil temperature factor for microbial respiration
    real(kind=kind_noahmp) :: FNF        ! foliage nitrogen adjustemt to respiration (<= 1)
    real(kind=kind_noahmp) :: TF         ! temperature factor
    real(kind=kind_noahmp) :: RF         ! respiration reduction factor (<= 1)
    
    ! crop
    real(kind=kind_noahmp) :: GRAIN      ! mass of GRAIN [g/m2]
    real(kind=kind_noahmp) :: GDD        ! growing degree days
    real(kind=kind_noahmp) :: CBHYDRAFX  ! carbonhydrate assimilated per model step [g/m2]
    real(kind=kind_noahmp) :: GRTOVR     ! grainturnover per time step [g/m2]
    real(kind=kind_noahmp) :: LFCONVERT  ! leaf to grain conversion ! Zhe Zhang 2020-07-13
    real(kind=kind_noahmp) :: RSGRAIN    ! grain respiration [g/m2]

  end type state_type

!=== define "parameter" sub-type of biochem_type (biochem%param%variable)
  type :: parameter_type

    ! define specific biochem parameter variables
    INTEGER                :: DEFAULT_CROP       ! The default crop type(1-5); if zero, use generic dynamic vegetation 
    integer                :: PLTDAY             ! Planting date
    integer                :: HSDAY              ! Harvest date
    integer                :: C3C4               ! photosynthetic pathway:  1 = c3 2 = c4
    real(kind=kind_noahmp) :: FOLNMX             ! foliage nitrogen concentration when f(n)=1 (%)
    real(kind=kind_noahmp) :: QE25               ! quantum efficiency at 25c (umol co2 / umol photon)
    real(kind=kind_noahmp) :: VCMX25             ! maximum rate of carboxylation at 25c (umol co2/m**2/s)
    real(kind=kind_noahmp) :: AVCMX              ! q10 for vcmx25
    real(kind=kind_noahmp) :: C3PSN              ! photosynthetic pathway: 0. = c4, 1. = c3
    real(kind=kind_noahmp) :: MP                 ! slope of conductance-to-photosynthesis relationship
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
    real(kind=kind_noahmp) :: GDDTBASE           ! Base temperature for GDD accumulation [C]
    real(kind=kind_noahmp) :: GDDTCUT            ! Upper temperature for GDD accumulation [C]
    real(kind=kind_noahmp) :: GDDS1              ! GDD from seeding to emergence
    real(kind=kind_noahmp) :: GDDS2              ! GDD from seeding to initial vegetative 
    real(kind=kind_noahmp) :: GDDS3              ! GDD from seeding to post vegetative 
    real(kind=kind_noahmp) :: GDDS4              ! GDD from seeding to intial reproductive
    real(kind=kind_noahmp) :: GDDS5              ! GDD from seeding to pysical maturity 
    real(kind=kind_noahmp) :: I2PAR              ! Fraction of incoming solar radiation to photosynthetically active radiation 
    real(kind=kind_noahmp) :: TASSIM0            ! Minimum temperature for CO2 assimulation [C]
    real(kind=kind_noahmp) :: TASSIM1            ! CO2 assimulation linearly increasing until temperature reaches T1 [C]
    real(kind=kind_noahmp) :: TASSIM2            ! CO2 assmilation rate remain at Aref until temperature reaches T2 [C]
    real(kind=kind_noahmp) :: AREF               ! reference maximum CO2 assimulation rate  
    real(kind=kind_noahmp) :: K                  ! light extinction coefficient 
    real(kind=kind_noahmp) :: EPSI               ! initial light use efficiency 
    real(kind=kind_noahmp) :: PSNRF              ! CO2 assimulation reduction factor(0-1) (caused by non-modeling part,e.g.pest,weeds)
    real(kind=kind_noahmp) :: SLAREA             ! single leaf area [m2]?

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

!=== define "diagnose" sub-type of biochem_type (biochem%diag%variable)
  type :: diagnose_type

    ! define specific biochem diagnose variables
    

  end type diagnose_type

!=== define biochem type that includes 4 subtypes (flux,state,parameter,diagnose)
  type, public :: biochem_type

    type(flux_type)      :: flux
    type(state_type)     :: state
    type(parameter_type) :: param
    type(diagnose_type)  :: diag

  end type biochem_type

end module BiochemVarType
