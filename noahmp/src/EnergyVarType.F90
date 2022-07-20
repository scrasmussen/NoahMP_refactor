module EnergyVarType

!!! Define column (1-D) Noah-MP Energy variables
!!! Energy variable initialization is done in EnergyInit.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use Machine, only : kind_noahmp

  implicit none
  save
  private

!=== define "flux" sub-type of energy_type (energy%flux%variable)
  type :: flux_type

    ! define specific energy flux variables
    real(kind=kind_noahmp) :: FCEV            ! canopy evaporation (w/m2) [+ = to atm] 
    real(kind=kind_noahmp) :: FCTR            ! transpiration (w/m2) [+ = to atm]
    real(kind=kind_noahmp) :: FGEV            ! soil evap heat (w/m2) [+ to atm]
    real(kind=kind_noahmp) :: FIRR            ! latent heating due to sprinkler irrigation evaporation [w/m2]
    real(kind=kind_noahmp) :: PAHV            ! precipitation advected heat - vegetation net (W/m2)
    real(kind=kind_noahmp) :: PAHG            ! precipitation advected heat - under canopy net (W/m2)
    real(kind=kind_noahmp) :: PAHB            ! precipitation advected heat - bare ground net (W/m2)
    real(kind=kind_noahmp) :: PAH             ! precipitation advected heat - total (W/m2)
    real(kind=kind_noahmp) :: PARSUN          ! average absorbed par for sunlit leaves (w/m2)
    real(kind=kind_noahmp) :: PARSHA          ! average absorbed par for shaded leaves (w/m2)
    real(kind=kind_noahmp) :: SAV             ! solar radiation absorbed by vegetation (w/m2)
    real(kind=kind_noahmp) :: SAG             ! solar radiation absorbed by ground (w/m2)
    real(kind=kind_noahmp) :: FSA             ! total absorbed solar radiation (w/m2)
    real(kind=kind_noahmp) :: FSR             ! total reflected solar radiation (w/m2)
    real(kind=kind_noahmp) :: FSRV            ! reflected solar radiation by vegetation (w/m2)
    real(kind=kind_noahmp) :: FSRG            ! reflected solar radiation by ground (w/m2)
    real(kind=kind_noahmp) :: IRC             ! canopy net longwave radiation (w/m2) [+= to atm]
    real(kind=kind_noahmp) :: SHC             ! canopy sensible heat flux (w/m2)     [+= to atm]
    real(kind=kind_noahmp) :: EVC             ! canopy evaporation heat flux (w/m2)  [+= to atm]
    real(kind=kind_noahmp) :: IRG             ! vegetated ground net longwave radiation (w/m2) [+= to atm]
    real(kind=kind_noahmp) :: SHG             ! vegetated ground sensible heat flux (w/m2)     [+= to atm]
    real(kind=kind_noahmp) :: EVG             ! vegetated ground evaporation heat flux (w/m2)  [+= to atm]
    real(kind=kind_noahmp) :: TR              ! canopy transpiration heat flux (w/m2)[+= to atm]
    real(kind=kind_noahmp) :: GHV             ! vegetated ground heat (w/m2) [+ = to soil]
    real(kind=kind_noahmp) :: IRB             ! net longwave rad (w/m2) bare ground [+ to atm]
    real(kind=kind_noahmp) :: SHB             ! sensible heat flux (w/m2) bare ground [+ to atm]
    real(kind=kind_noahmp) :: EVB             ! latent heat flux (w/m2) bare ground [+ to atm]
    real(kind=kind_noahmp) :: GHB             ! bare ground heat flux (w/m2) [+ to soil]
    real(kind=kind_noahmp) :: SSOIL           ! soil heat flux (w/m2) [+ to soil]
    real(kind=kind_noahmp) :: EFLXB           ! energy influx from soil bottom (w/m2)
    real(kind=kind_noahmp) :: FIRA            ! total net LW. rad (w/m2)   [+ to atm]
    real(kind=kind_noahmp) :: FSH             ! total sensible heat (w/m2) [+ to atm]
    real(kind=kind_noahmp) :: APAR            ! total photosyn. active energy (w/m2)
    real(kind=kind_noahmp) :: FIRE            ! emitted outgoing IR (w/m2)

    real(kind=kind_noahmp), allocatable, dimension(:) :: FABD        ! flux abs by veg (per unit direct flux)
    real(kind=kind_noahmp), allocatable, dimension(:) :: FABI        ! flux abs by veg (per unit diffuse flux)
    real(kind=kind_noahmp), allocatable, dimension(:) :: FTDD        ! down direct flux below veg (per unit dir flux)
    real(kind=kind_noahmp), allocatable, dimension(:) :: FTDI        ! down direct flux below veg per unit dif flux
    real(kind=kind_noahmp), allocatable, dimension(:) :: FTID        ! down diffuse flux below veg (per unit dir flux)
    real(kind=kind_noahmp), allocatable, dimension(:) :: FTII        ! down diffuse flux below veg (per unit dif flux)
    real(kind=kind_noahmp), allocatable, dimension(:) :: FREVD       ! flux reflected by veg layer (per unit direct flux)
    real(kind=kind_noahmp), allocatable, dimension(:) :: FREVI       ! flux reflected by veg layer (per unit diffuse flux)
    real(kind=kind_noahmp), allocatable, dimension(:) :: FREGD       ! flux reflected by ground (per unit direct flux)
    real(kind=kind_noahmp), allocatable, dimension(:) :: FREGI       ! flux reflected by ground (per unit diffuse flux)
    real(kind=kind_noahmp), allocatable, dimension(:) :: SOLAD       ! incoming direct solar radiation (w/m2)
    real(kind=kind_noahmp), allocatable, dimension(:) :: SOLAI       ! incoming diffuse solar radiation (w/m2)
    real(kind=kind_noahmp), allocatable, dimension(:) :: PHI         ! light penetrating through soil/snow water (W/m2)

  end type flux_type


!=== define "state" sub-type of energy_type (energy%state%variable)
  type :: state_type

    ! define specific energy state variables
    logical                :: FROZEN_CANOPY   ! used to define latent heat pathway
    logical                :: FROZEN_GROUND   ! used to define latent heat pathway
    real(kind=kind_noahmp) :: ELAI            ! leaf area index, after burying by snow
    real(kind=kind_noahmp) :: ESAI            ! stem area index, after burying by snow
    real(kind=kind_noahmp) :: LAI             ! leaf area index
    real(kind=kind_noahmp) :: SAI             ! stem area index
    real(kind=kind_noahmp) :: VAI             ! one-sided leaf+stem area index (m2/m2), after burying by snow
    real(kind=kind_noahmp) :: FVEG            ! greeness vegetation fraction
    real(kind=kind_noahmp) :: TG              ! ground temperature (k)
    real(kind=kind_noahmp) :: TV              ! vegetation temperature (k)
    real(kind=kind_noahmp) :: TS              ! surface temperature (K)
    real(kind=kind_noahmp) :: TROOT           ! root-zone averaged temperature (k)
    real(kind=kind_noahmp) :: EAIR            ! vapor pressure air (pa)
    real(kind=kind_noahmp) :: FAGE            ! snow age factor
    real(kind=kind_noahmp) :: TAUSS           ! non-dimensional snow age
    real(kind=kind_noahmp) :: ALBOLD          ! snow albedo at last time step
    real(kind=kind_noahmp) :: GDIR            ! projected leaf+stem area in solar direction
    real(kind=kind_noahmp) :: BGAP            ! between canopy gap fraction for beam
    real(kind=kind_noahmp) :: WGAP            ! within canopy gap fraction for beam
    real(kind=kind_noahmp) :: KOPEN           ! gap fraction for diffue light
    real(kind=kind_noahmp) :: GAP             ! total gap fraction for beam (<=1-shafac)
    real(kind=kind_noahmp) :: FSUN            ! sunlit fraction of canopy
    real(kind=kind_noahmp) :: FSHA            ! shaded fraction of canopy
    real(kind=kind_noahmp) :: LAISUN          ! sunlit leaf area
    real(kind=kind_noahmp) :: LAISHA          ! shaded leaf area
    real(kind=kind_noahmp) :: ESTV            ! canopy saturation vapor pressure at TV (pa)
    real(kind=kind_noahmp) :: ESTG            ! below-canopy saturation vapor pressure at TG (pa)
    real(kind=kind_noahmp) :: ESTB            ! bare groudn saturation vapor pressure at TG (pa)
    real(kind=kind_noahmp) :: DESTV           ! d(ESTV)/dt at TV (pa/k)
    real(kind=kind_noahmp) :: DESTG           ! d(ESTG)/dt at TG (pa/k)
    real(kind=kind_noahmp) :: DESTB           ! d(ESTB)/dt at TG (pa/k)
    real(kind=kind_noahmp) :: EAH             ! canopy air vapor pressure (pa)
    real(kind=kind_noahmp) :: CO2AIR          ! atmospheric co2 concentration (pa)
    real(kind=kind_noahmp) :: O2AIR           ! atmospheric o2 concentration (pa)
    real(kind=kind_noahmp) :: RSSUN           ! sunlit leaf stomatal resistance (s/m)
    real(kind=kind_noahmp) :: RSSHA           ! shaded leaf stomatal resistance (s/m)
    real(kind=kind_noahmp) :: RHOAIR          ! density air (kg/m3)
    real(kind=kind_noahmp) :: TAH             ! canopy air temperature (K)
    real(kind=kind_noahmp) :: ZPD             ! surface zero plane displacement (m)
    real(kind=kind_noahmp) :: ZPDG            ! ground zero plane displacement (m)
    real(kind=kind_noahmp) :: Z0MG            ! roughness length, momentum, ground (m)
    real(kind=kind_noahmp) :: Z0M             ! roughness length, momentum, surface (m)
    real(kind=kind_noahmp) :: Z0HV            ! roughness length, sensible heat (m), vegetated
    real(kind=kind_noahmp) :: Z0HG            ! roughness length, sensible heat, ground (m), below canopy
    real(kind=kind_noahmp) :: Z0HB            ! roughness length, sensible heat, bare ground (m)
    real(kind=kind_noahmp) :: HCAN            ! canopy height (m) [note: hcan >= z0mg]
    real(kind=kind_noahmp) :: UC              ! wind speed at top of canopy (m/s)
    real(kind=kind_noahmp) :: FVV             ! friction velocity (m/s), vegetated
    real(kind=kind_noahmp) :: FVB             ! friction velocity (m/s), bare ground
    real(kind=kind_noahmp) :: CWPC            ! canopy wind extinction coefficient
    real(kind=kind_noahmp) :: MOZG            ! M-O stability parameter ground, below canopy
    real(kind=kind_noahmp) :: MOZV            ! M-O stability parameter (z/L), above ZPD, vegetated
    real(kind=kind_noahmp) :: MOZB            ! M-O stability parameter (z/L), above ZPD, bare ground
    real(kind=kind_noahmp) :: MOZ2V           ! M-O stability (2/L), 2m, vegetated
    real(kind=kind_noahmp) :: MOZ2B           ! M-O stability (2/L), 2m, bare ground
    real(kind=kind_noahmp) :: MOLG            ! M-O length (m), ground, below canopy
    real(kind=kind_noahmp) :: MOLV            ! M-O length (m), above ZPD, vegetated
    real(kind=kind_noahmp) :: MOLB            ! M-O length (m), above ZPD, bare ground
    real(kind=kind_noahmp) :: FHG             ! M-O stability correction ground, below canopy
    real(kind=kind_noahmp) :: FMV             ! M-O momentum stability correction, above ZPD, vegetated
    real(kind=kind_noahmp) :: FHV             ! M-O sen heat stability correction, above ZPD, vegetated
    real(kind=kind_noahmp) :: FM2V            ! M-O momentum stability correction, 2m, vegetated
    real(kind=kind_noahmp) :: FH2V            ! M-O sen heat stability correction, 2m, vegetated
    real(kind=kind_noahmp) :: FMB             ! M-O momentum stability correction, above ZPD, bare ground
    real(kind=kind_noahmp) :: FHB             ! M-O sen heat stability correction, above ZPD, bare ground
    real(kind=kind_noahmp) :: FM2B            ! M-O momentum stability correction, 2m, bare ground
    real(kind=kind_noahmp) :: FH2B            ! M-O sen heat stability correction, 2m, bare ground
    real(kind=kind_noahmp) :: CM              ! exchange coefficient (m/s) for momentum, surface, grid mean
    real(kind=kind_noahmp) :: CMV             ! exchange coefficient (m/s) for momentum, above ZPD, vegetated
    real(kind=kind_noahmp) :: CMB             ! exchange coefficient (m/s) for momentum, above ZPD, bare ground
    real(kind=kind_noahmp) :: CH              ! exchange coefficient (m/s) for heat, surface, grid mean
    real(kind=kind_noahmp) :: CHV             ! exchange coefficient (m/s) for heat, above ZPD, vegetated
    real(kind=kind_noahmp) :: CHB             ! exchange coefficient (m/s) for heat, above ZPD, bare ground
    real(kind=kind_noahmp) :: CH2V            ! exchange coefficient (m/s) for heat, 2m, vegetated from MOST scheme
    real(kind=kind_noahmp) :: CH2B            ! exchange coefficient (m/s) for heat, 2m, bare ground from MOST scheme
    real(kind=kind_noahmp) :: CHV2            ! exchange coefficient (m/s) for heat, 2m, vegetated from vege_flux
    real(kind=kind_noahmp) :: CAW             ! latent heat conductance/exchange coeff, canopy air (m/s)
    real(kind=kind_noahmp) :: CTW             ! transpiration conductance, leaf to canopy air (m/s)
    real(kind=kind_noahmp) :: CEW             ! evaporation conductance, leaf to canopy air (m/s)
    real(kind=kind_noahmp) :: CGW             ! latent heat conductance, ground to canopy air (m/s)
    real(kind=kind_noahmp) :: RAMG            ! aerodynamic resistance for momentum (s/m), ground, below canopy
    real(kind=kind_noahmp) :: RAHG            ! aerodynamic resistance for sensible heat (s/m), ground, below canopy
    real(kind=kind_noahmp) :: RAWG            ! aerodynamic resistance for water vapor (s/m), ground, below canopy
    real(kind=kind_noahmp) :: RAMC            ! aerodynamic resistance for momentum (s/m), above canopy
    real(kind=kind_noahmp) :: RAHC            ! aerodynamic resistance for sensible heat (s/m), above canopy
    real(kind=kind_noahmp) :: RAWC            ! aerodynamic resistance for water vapor (s/m), above canopy
    real(kind=kind_noahmp) :: RAMB            ! aerodynamic resistance for momentum (s/m), bare ground
    real(kind=kind_noahmp) :: RAHB            ! aerodynamic resistance for sensible heat (s/m), bare ground
    real(kind=kind_noahmp) :: RAWB            ! aerodynamic resistance for water vapor (s/m), bare ground
    real(kind=kind_noahmp) :: RB              ! bulk leaf boundary layer resistance (s/m)
    real(kind=kind_noahmp) :: THAIR           ! potential temp at reference height (K)
    real(kind=kind_noahmp) :: UR              ! wind speed (m/s) at reference height
    real(kind=kind_noahmp) :: WSTARV          ! friction velocity in vertical direction (m/s), vegetated (only for Chen97)
    real(kind=kind_noahmp) :: WSTARB          ! friction velocity in vertical direction (m/s), bare ground (only for Chen97)
    real(kind=kind_noahmp) :: EMV             ! vegetation emissivity
    real(kind=kind_noahmp) :: EMG             ! ground emissivity
    real(kind=kind_noahmp) :: RSURF           ! ground surface resistance (s/m)
    real(kind=kind_noahmp) :: GAMMAV          ! psychrometric constant (pa/K), canopy
    real(kind=kind_noahmp) :: LATHEAV         ! latent heat of vaporization/subli (j/kg), canopy
    real(kind=kind_noahmp) :: GAMMAG          ! psychrometric constant (pa/K), ground
    real(kind=kind_noahmp) :: LATHEAG         ! latent heat of vaporization/subli (j/kg), ground
    real(kind=kind_noahmp) :: RHSUR           ! raltive humidity in surface soil/snow air space (-)
    real(kind=kind_noahmp) :: QSFC            ! water vapor mixing ratio at lowest model layer bare ground
    real(kind=kind_noahmp) :: Q1              ! water vapor mixing ratio at lowest model layer grid mean
    real(kind=kind_noahmp) :: Q2V             ! water vapor mixing ratio at 2m vegetated
    real(kind=kind_noahmp) :: Q2B             ! water vapor mixing ratio at 2m bare ground
    real(kind=kind_noahmp) :: Q2E             ! water vapor mixing ratio at 2m grid mean
    real(kind=kind_noahmp) :: TGV             ! vegetated ground (below-canopy) temperature (K)
    real(kind=kind_noahmp) :: TGB             ! bare ground temperature (K)
    real(kind=kind_noahmp) :: TAUXV           ! wind stress: east-west (n/m2) above canopy
    real(kind=kind_noahmp) :: TAUYV           ! wind stress: north-south (n/m2) above canopy
    real(kind=kind_noahmp) :: TAUXB           ! wind stress: east-west (n/m2) bare ground
    real(kind=kind_noahmp) :: TAUYB           ! wind stress: north-south (n/m2) bare ground
    real(kind=kind_noahmp) :: TAUX            ! wind stress: east-west (n/m2) grid mean
    real(kind=kind_noahmp) :: TAUY            ! wind stress: north-south (n/m2) grid mean
    real(kind=kind_noahmp) :: T2MV            ! 2 m height air temperature (k), vegetated
    real(kind=kind_noahmp) :: T2MB            ! 2 m height air temperature (k), bare ground
    real(kind=kind_noahmp) :: T2M             ! 2 m height air temperature (k), grid mean
    real(kind=kind_noahmp) :: CHLEAF          ! leaf sensible heat exchange coefficient (m/s)
    real(kind=kind_noahmp) :: CHUC            ! under canopy sensible heat exchange coefficient (m/s)
    real(kind=kind_noahmp) :: EHB             ! bare ground sensible heat exchange coefficient (m/s)
    real(kind=kind_noahmp) :: EHB2            ! bare ground 2-m sensible heat exchange coefficient (m/s)
    real(kind=kind_noahmp) :: EMB             ! bare ground momentum exchange coefficient (m/s)
    real(kind=kind_noahmp) :: RefHeightAboveGround    ! reference height [m] above ground
    real(kind=kind_noahmp) :: FB_snow         ! fraction of canopy buried by snow
    real(kind=kind_noahmp) :: ZBOTSNO         ! depth of lower boundary condition (m) from snow surface
    real(kind=kind_noahmp) :: Z0WRF           ! roughness length, momentum, surface, sent to coupled model
    real(kind=kind_noahmp) :: TRAD            ! radiative temperature (K)
    real(kind=kind_noahmp) :: EMISSI          ! surface emissivity
    real(kind=kind_noahmp) :: ALBEDO          ! total surface albedo
    real(kind=kind_noahmp) :: ERRENG          ! error in surface energy balance [w/m2]
    real(kind=kind_noahmp) :: ERRSW           ! error in shortwave radiation balance [w/m2]

    real(kind=kind_noahmp), allocatable, dimension(:) :: STC         ! snow and soil layer temperature [k]
    real(kind=kind_noahmp), allocatable, dimension(:) :: CVSNO       ! snow layer volumetric specific heat (j/m3/k)
    real(kind=kind_noahmp), allocatable, dimension(:) :: TKSNO       ! snow layer thermal conductivity (w/m/k)
    real(kind=kind_noahmp), allocatable, dimension(:) :: CVSOIL      ! soil layer volumetric specific heat (j/m3/k)
    real(kind=kind_noahmp), allocatable, dimension(:) :: TKSOIL      ! soil layer thermal conductivity (w/m/k)
    real(kind=kind_noahmp), allocatable, dimension(:) :: CVGLAICE    ! glacier ice layer volumetric specific heat (j/m3/k)
    real(kind=kind_noahmp), allocatable, dimension(:) :: TKGLAICE    ! glacier ice thermal conductivity (w/m/k)
    real(kind=kind_noahmp), allocatable, dimension(:) :: DF          ! thermal conductivity [w/m/k] for all soil and snow layers
    real(kind=kind_noahmp), allocatable, dimension(:) :: HCPCT       ! heat capacity [j/m3/k] for all snow and soil layers
    real(kind=kind_noahmp), allocatable, dimension(:) :: FACT        ! energy factor for soil and snow phase change
    real(kind=kind_noahmp), allocatable, dimension(:) :: ALBSND      ! snow albedo for direct(1=vis, 2=nir)
    real(kind=kind_noahmp), allocatable, dimension(:) :: ALBSNI      ! snow albedo for diffuse(1=vis, 2=nir)
    real(kind=kind_noahmp), allocatable, dimension(:) :: ALBSOD      ! soil albedo (direct)
    real(kind=kind_noahmp), allocatable, dimension(:) :: ALBSOI      ! soil albedo (diffuse)
    real(kind=kind_noahmp), allocatable, dimension(:) :: ALBGRD      ! ground albedo (direct beam: vis, nir)
    real(kind=kind_noahmp), allocatable, dimension(:) :: ALBGRI      ! ground albedo (diffuse: vis, nir)
    real(kind=kind_noahmp), allocatable, dimension(:) :: RHO         ! leaf/stem reflectance weighted by fraction LAI and SAI
    real(kind=kind_noahmp), allocatable, dimension(:) :: TAU         ! leaf/stem transmittance weighted by fraction LAI and SAI
    real(kind=kind_noahmp), allocatable, dimension(:) :: ALBD        ! surface albedo (direct)
    real(kind=kind_noahmp), allocatable, dimension(:) :: ALBI        ! surface albedo (diffuse)

  end type state_type


!=== define "parameter" sub-type of energy_type (energy%param%variable)
  type :: parameter_type

    ! define specific energy parameter variables
    real(kind=kind_noahmp) :: RC               ! tree crown radius (m)
    real(kind=kind_noahmp) :: HVT              ! top of canopy (m)
    real(kind=kind_noahmp) :: HVB              ! bottom of canopy (m)
    real(kind=kind_noahmp) :: Z0MVT            ! momentum roughness length (m)
    real(kind=kind_noahmp) :: DEN              ! tree density (no. of trunks per m2)
    real(kind=kind_noahmp) :: XL               ! leaf/stem orientation index
    real(kind=kind_noahmp) :: BETADS           ! two-stream parameter betad for snow (dir rad)
    real(kind=kind_noahmp) :: BETAIS           ! two-stream parameter betad for snow (dif rad)
    real(kind=kind_noahmp) :: CSOIL            ! vol. soil heat capacity [j/m3/K]
    real(kind=kind_noahmp) :: TAU0             ! snow aging parameter for BATS snow albedo
    real(kind=kind_noahmp) :: GRAIN_GROWTH     ! vapor diffusion snow growth factor for BATS snow albedo
    real(kind=kind_noahmp) :: DIRT_SOOT        ! dirt and soot effect factor for BATS snow albedo
    real(kind=kind_noahmp) :: EXTRA_GROWTH     ! extra snow growth factor near freezing for BATS snow albedo
    real(kind=kind_noahmp) :: BATS_COSZ        ! zenith angle snow albedo adjustment
    real(kind=kind_noahmp) :: BATS_VIS_NEW     ! new snow visible albedo
    real(kind=kind_noahmp) :: BATS_NIR_NEW     ! new snow NIR albedo
    real(kind=kind_noahmp) :: BATS_VIS_AGE     ! age factor for diffuse visible snow albedo
    real(kind=kind_noahmp) :: BATS_NIR_AGE     ! age factor for diffuse NIR snow albedo
    real(kind=kind_noahmp) :: BATS_VIS_DIR     ! cosz factor for direct visible snow albedo
    real(kind=kind_noahmp) :: BATS_NIR_DIR     ! cosz factor for direct NIR snow albedo
    real(kind=kind_noahmp) :: CLASS_ALB_REF    ! reference snow albedo in CLASS scheme
    real(kind=kind_noahmp) :: CLASS_SNO_AGE    ! snow aging e-folding time (s) in CLASS albedo scheme
    real(kind=kind_noahmp) :: CLASS_ALB_NEW    ! fresh snow albedo in CLASS albedo scheme
    real(kind=kind_noahmp) :: BP               ! minimum leaf conductance (umol/m**2/s)
    real(kind=kind_noahmp) :: KC25             ! co2 michaelis-menten constant at 25c (pa)
    real(kind=kind_noahmp) :: KO25             ! o2 michaelis-menten constant at 25c (pa)
    real(kind=kind_noahmp) :: AKC              ! q10 for kc25
    real(kind=kind_noahmp) :: AKO              ! q10 for ko25
    real(kind=kind_noahmp) :: RGL              ! Parameter used in radiation stress function in Jarvis scheme
    real(kind=kind_noahmp) :: RSMIN            ! Minimum stomatal resistance [s m-1] in Jarvis scheme
    real(kind=kind_noahmp) :: RSMAX            ! Maximal stomatal resistance [s m-1] in Jarvis scheme
    real(kind=kind_noahmp) :: TOPT             ! Optimum transpiration air temperature [K] in Jarvis scheme
    real(kind=kind_noahmp) :: HS               ! Parameter used in vapor pressure deficit function in Jarvis scheme
    real(kind=kind_noahmp) :: DLEAF            ! characteristic leaf dimension (m)
    real(kind=kind_noahmp) :: CZIL             ! Calculate roughness length of heat
    real(kind=kind_noahmp) :: SNOW_EMIS        ! snow emissivity
    real(kind=kind_noahmp) :: CWPVT            ! empirical canopy wind absorption parameter
    real(kind=kind_noahmp) :: Z0SNO            ! snow surface roughness length (m) (0.002)
    real(kind=kind_noahmp) :: Z0SOIL           ! Bare-soil roughness length (m) (i.e., under the canopy)
    real(kind=kind_noahmp) :: Z0LAKE           ! lake surface roughness length (m)
    real(kind=kind_noahmp) :: EICE             ! ice surface emissivity
    real(kind=kind_noahmp) :: RSURF_EXP        ! exponent in the shape parameter for soil resistance option 1
    real(kind=kind_noahmp) :: RSURF_SNOW       ! surface resistance for snow(s/m)
    real(kind=kind_noahmp) :: SHDFAC           ! vegetation fraction
    real(kind=kind_noahmp) :: SHDMAX           ! yearly maximum vegetation fraction

    real(kind=kind_noahmp), allocatable, dimension(:) :: LAIM        ! monthly leaf area index, one-sided
    real(kind=kind_noahmp), allocatable, dimension(:) :: SAIM        ! monthly stem area index, one-sided
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilQuartzFrac      ! soil quartz content
    real(kind=kind_noahmp), allocatable, dimension(:) :: ALBSAT      ! saturated soil albedos: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:) :: ALBDRY      ! dry soil albedos: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:) :: ALBLAK      ! albedo frozen lakes: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:) :: OMEGAS      ! two-stream parameter omega for snow
    real(kind=kind_noahmp), allocatable, dimension(:) :: RHOL        ! leaf reflectance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:) :: RHOS        ! stem reflectance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:) :: TAUL        ! leaf transmittance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:) :: TAUS        ! stem transmittance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:) :: EG          ! emissivity soil surface: 1=soil, 2=lake
    real(kind=kind_noahmp), allocatable, dimension(:) :: ALBICE      ! land/glacier ice albedo: 1=vis, 2=nir

  end type parameter_type


!=== define energy type that includes 3 subtypes (flux,state,parameter)
  type, public :: energy_type

    type(flux_type)      :: flux
    type(state_type)     :: state
    type(parameter_type) :: param

  end type energy_type

end module EnergyVarType
