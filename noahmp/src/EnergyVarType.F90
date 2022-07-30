module EnergyVarType

!!! Define column (1-D) Noah-MP Energy variables
!!! Energy variable initialization is done in EnergyVarInitMod.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (July 2022)
! -------------------------------------------------------------------------

  use Machine

  implicit none
  save
  private

!=== define "flux" sub-type of energy (energy%flux%variable)
  type :: flux_type

    real(kind=kind_noahmp) :: HeatLatentCanopy            ! canopy latent heat flux [W/m2] (+ = to atm)
    real(kind=kind_noahmp) :: HeatLatentTransp            ! latent heat flux from transpiration [W/m2] (+ = to atm)
    real(kind=kind_noahmp) :: HeatLatentGrdTot            ! total ground latent heat [W/m2] (+ = to atm)
    real(kind=kind_noahmp) :: HeatLatentIrriEvap          ! latent heating due to sprinkler irrigation evaporation [W/m2]
    real(kind=kind_noahmp) :: HeatPrecipAdvCanopy         ! precipitation advected heat - canopy net [W/m2]
    real(kind=kind_noahmp) :: HeatPrecipAdvVegGrd         ! precipitation advected heat - vegetated ground net [W/m2]
    real(kind=kind_noahmp) :: HeatPrecipAdvBareGrd        ! precipitation advected heat - bare ground net [W/m2]
    real(kind=kind_noahmp) :: HeatPrecipAdvTot            ! precipitation advected heat - total [W/m2]
    real(kind=kind_noahmp) :: RadPhotoActAbsSunlit        ! absorbed photosyn. active radiation for sunlit leaves [W/m2]
    real(kind=kind_noahmp) :: RadPhotoActAbsShade         ! absorbed photosyn. active radiation  for shaded leaves [W/m2]
    real(kind=kind_noahmp) :: RadSwAbsVeg                 ! solar radiation absorbed by vegetation [W/m2]
    real(kind=kind_noahmp) :: RadSwAbsGrd                 ! solar radiation absorbed by ground [W/m2]
    real(kind=kind_noahmp) :: RadSwAbsTot                 ! total absorbed solar radiation [W/m2]
    real(kind=kind_noahmp) :: RadSwReflTot                ! total reflected solar radiation [W/m2]
    real(kind=kind_noahmp) :: RadSwReflVeg                ! reflected solar radiation by vegetation [W/m2]
    real(kind=kind_noahmp) :: RadSwReflGrd                ! reflected solar radiation by ground [W/m2]
    real(kind=kind_noahmp) :: RadLwNetCanopy              ! canopy net longwave radiation [W/m2] (+ = to atm)
    real(kind=kind_noahmp) :: HeatSensibleCanopy          ! canopy sensible heat flux [W/m2]     (+ = to atm)
    real(kind=kind_noahmp) :: HeatLatentCanEvap           ! canopy evaporation heat flux [W/m2]  (+ = to atm)
    real(kind=kind_noahmp) :: RadLwNetVegGrd              ! vegetated ground net longwave radiation [W/m2] (+ = to atm)
    real(kind=kind_noahmp) :: HeatSensibleVegGrd          ! vegetated ground sensible heat flux [W/m2]     (+ = to atm)
    real(kind=kind_noahmp) :: HeatLatentVegGrd            ! vegetated ground latent heat flux [W/m2]  (+ = to atm)
    real(kind=kind_noahmp) :: HeatLatentCanTransp         ! canopy transpiration latent heat flux [W/m2] (+ = to atm)
    real(kind=kind_noahmp) :: HeatGroundVegGrd            ! vegetated ground heat flux [W/m2] (+ = to soil/snow)
    real(kind=kind_noahmp) :: RadLwNetBareGrd             ! bare ground net longwave rad [W/m2] (+ = to atm)
    real(kind=kind_noahmp) :: HeatSensibleBareGrd         ! bare ground sensible heat flux [W/m2] (+ = to atm)
    real(kind=kind_noahmp) :: HeatLatentBareGrd           ! bare ground latent heat flux [W/m2] (+ = to atm)
    real(kind=kind_noahmp) :: HeatGroundBareGrd           ! bare ground heat flux [W/m2] (+ = to soil/snow)
    real(kind=kind_noahmp) :: HeatGroundTot               ! total ground heat flux [W/m2] (+ = to soil/snow)
    real(kind=kind_noahmp) :: HeatFromSoilBot             ! energy influx from soil bottom [W/m2]
    real(kind=kind_noahmp) :: RadLwNetTot                 ! total net longwave radiation [W/m2] (+ = to atm)
    real(kind=kind_noahmp) :: HeatSensibleTot             ! total sensible heat [W/m2] (+ = to atm)
    real(kind=kind_noahmp) :: RadPhotoActAbsCan           ! total photosyn. active energy [W/m2] absorbed by canopy
    real(kind=kind_noahmp) :: RadLwEmitTot                ! emitted outgoing longwave radiation [W/m2]

    real(kind=kind_noahmp), allocatable, dimension(:) :: RadSwAbsVegDir        ! solar flux absorbed by veg per unit direct flux
    real(kind=kind_noahmp), allocatable, dimension(:) :: RadSwAbsVegDif        ! solar flux absorbed by veg per unit diffuse flux
    real(kind=kind_noahmp), allocatable, dimension(:) :: RadSwDirTranGrdDir    ! transmitted direct flux below veg per unit direct flux
    real(kind=kind_noahmp), allocatable, dimension(:) :: RadSwDirTranGrdDif    ! transmitted direct flux below veg per unit diffuse flux
    real(kind=kind_noahmp), allocatable, dimension(:) :: RadSwDifTranGrdDir    ! transmitted diffuse flux below veg per unit direct flux
    real(kind=kind_noahmp), allocatable, dimension(:) :: RadSwDifTranGrdDif    ! transmitted diffuse flux below veg per unit diffuse flux
    real(kind=kind_noahmp), allocatable, dimension(:) :: RadSwReflVegDir       ! solar flux reflected by veg layer per unit direct flux
    real(kind=kind_noahmp), allocatable, dimension(:) :: RadSwReflVegDif       ! solar flux reflected by veg layer per unit diffuse flux
    real(kind=kind_noahmp), allocatable, dimension(:) :: RadSwReflGrdDir       ! solar flux reflected by ground per unit direct flux
    real(kind=kind_noahmp), allocatable, dimension(:) :: RadSwReflGrdDif       ! solar flux reflected by ground per unit diffuse flux
    real(kind=kind_noahmp), allocatable, dimension(:) :: RadSwDownDir          ! incoming direct solar radiation [W/m2]
    real(kind=kind_noahmp), allocatable, dimension(:) :: RadSwDownDif          ! incoming diffuse solar radiation [W/m2]
    real(kind=kind_noahmp), allocatable, dimension(:) :: RadSwPenetrateGrd     ! light penetrating through soil/snow water [W/m2]

  end type flux_type


!=== define "state" sub-type of energy (energy%state%variable)
  type :: state_type

    logical                :: FlagFrozenCanopy   ! frozen canopy flag used to define latent heat pathway
    logical                :: FlagFrozenGround   ! frozen ground flag used to define latent heat pathway
    real(kind=kind_noahmp) :: LeafAreaIndEff            ! effective leaf area index, after burying by snow
    real(kind=kind_noahmp) :: StemAreaIndEff            ! effective stem area index, after burying by snow
    real(kind=kind_noahmp) :: LeafAreaIndex             ! leaf area index
    real(kind=kind_noahmp) :: StemAreaIndex             ! stem area index
    real(kind=kind_noahmp) :: VegAreaIndEff             ! one-sided leaf+stem area index (m2/m2), after burying by snow
    real(kind=kind_noahmp) :: VegFrac            ! greeness vegetation fraction
    real(kind=kind_noahmp) :: TemperatureGrd              ! ground temperature (k)
    real(kind=kind_noahmp) :: TemperatureCanopy              ! vegetation/canopy temperature (k)
    real(kind=kind_noahmp) :: TemperatureSfc              ! surface temperature (K)
    real(kind=kind_noahmp) :: TemperatureRootZone           ! root-zone averaged temperature (k)
    real(kind=kind_noahmp) :: PressureVaporRefHeight            ! vapor pressure air (pa)
    real(kind=kind_noahmp) :: SnowAgeFac            ! snow age factor
    real(kind=kind_noahmp) :: SnowAgeNondim           ! non-dimensional snow age
    real(kind=kind_noahmp) :: AlbedoSnowPrev          ! snow albedo at last time step
    real(kind=kind_noahmp) :: VegAreaProjDir            ! projected leaf+stem area in solar direction
    real(kind=kind_noahmp) :: GapBtwCanopy            ! between canopy gap fraction for beam
    real(kind=kind_noahmp) :: GapInCanopy            ! within canopy gap fraction for beam
    real(kind=kind_noahmp) :: GapCanopyDif           ! gap fraction for diffue light
    real(kind=kind_noahmp) :: GapCanopyDir             ! total gap fraction for beam (<=1-shafac)
    real(kind=kind_noahmp) :: CanopySunlitFrac            ! sunlit fraction of canopy
    real(kind=kind_noahmp) :: CanopyShadeFrac            ! shaded fraction of canopy
    real(kind=kind_noahmp) :: LeafAreaIndSunlit          ! sunlit leaf area
    real(kind=kind_noahmp) :: LeafAreaIndShade          ! shaded leaf area
    real(kind=kind_noahmp) :: VapPresSatCanopy            ! canopy saturation vapor pressure at veg temp. (pa)
    real(kind=kind_noahmp) :: VapPresSatGrdVeg            ! below-canopy saturation vapor pressure at ground temp. (pa)
    real(kind=kind_noahmp) :: VapPresSatGrdBare            ! bare ground saturation vapor pressure at ground temp. (pa)
    real(kind=kind_noahmp) :: VapPresSatCanTempD           ! canopy saturation vapor pressure derivative with temperature at veg temp. (pa/k)
    real(kind=kind_noahmp) :: VapPresSatGrdVegTempD           ! below-canopy saturation vapor pressure derivative with temperature at ground temp. (pa/k)
    real(kind=kind_noahmp) :: VapPresSatGrdBareTempD           ! bare ground saturation vapor pressure derivative with temperature at ground temp. (pa/k)
    real(kind=kind_noahmp) :: PressureVaporCanAir            ! canopy air vapor pressure (pa)
    real(kind=kind_noahmp) :: PressureAtmosCO2          ! atmospheric co2 partial pressure (pa)
    real(kind=kind_noahmp) :: PressureAtmosO2           ! atmospheric o2 partial pressure (pa)
    real(kind=kind_noahmp) :: ResistanceStomataSunlit           ! sunlit leaf stomatal resistance (s/m)
    real(kind=kind_noahmp) :: ResistanceStomataShade           ! shaded leaf stomatal resistance (s/m)
    real(kind=kind_noahmp) :: DensityAirRefHeight          ! density air (kg/m3) at reference height
    real(kind=kind_noahmp) :: TemperatureCanopyAir             ! canopy air temperature (K)
    real(kind=kind_noahmp) :: ZeroPlaneDispSfc             ! surface zero plane displacement (m)
    real(kind=kind_noahmp) :: ZeroPlaneDispGrd            ! ground zero plane displacement (m)
    real(kind=kind_noahmp) :: RoughLenMomGrd            ! roughness length, momentum, ground (m)
    real(kind=kind_noahmp) :: RoughLenMomSfc             ! roughness length, momentum, surface (m)
    real(kind=kind_noahmp) :: RoughLenShCanopy            ! roughness length, sensible heat (m), canopy
    real(kind=kind_noahmp) :: RoughLenShVegGrd            ! roughness length, sensible heat, ground (m), below canopy
    real(kind=kind_noahmp) :: RoughLenShBareGrd            ! roughness length, sensible heat, bare ground (m)
    real(kind=kind_noahmp) :: CanopyHeight            ! canopy height (m)
    real(kind=kind_noahmp) :: WindSpdCanopyTop              ! wind speed at top of canopy (m/s)
    real(kind=kind_noahmp) :: FVV             ! friction velocity (m/s), vegetated
    real(kind=kind_noahmp) :: FVB             ! friction velocity (m/s), bare ground
    real(kind=kind_noahmp) :: WindExtCoeffCanopy            ! canopy wind extinction coefficient
    real(kind=kind_noahmp) :: MOZG            ! M-O stability parameter ground, below canopy
    real(kind=kind_noahmp) :: MOZV            ! M-O stability parameter (z/L), above ZeroPlaneDisp, vegetated
    real(kind=kind_noahmp) :: MOZB            ! M-O stability parameter (z/L), above ZeroPlaneDisp, bare ground
    real(kind=kind_noahmp) :: MOZ2V           ! M-O stability (2/L), 2m, vegetated
    real(kind=kind_noahmp) :: MOZ2B           ! M-O stability (2/L), 2m, bare ground
    real(kind=kind_noahmp) :: MOLG            ! M-O length (m), ground, below canopy
    real(kind=kind_noahmp) :: MOLV            ! M-O length (m), above ZeroPlaneDisp, vegetated
    real(kind=kind_noahmp) :: MOLB            ! M-O length (m), above ZeroPlaneDisp, bare ground
    real(kind=kind_noahmp) :: FHG             ! M-O stability correction ground, below canopy
    real(kind=kind_noahmp) :: FMV             ! M-O momentum stability correction, above ZeroPlaneDisp, vegetated
    real(kind=kind_noahmp) :: FHV             ! M-O sen heat stability correction, above ZeroPlaneDisp, vegetated
    real(kind=kind_noahmp) :: FM2V            ! M-O momentum stability correction, 2m, vegetated
    real(kind=kind_noahmp) :: FH2V            ! M-O sen heat stability correction, 2m, vegetated
    real(kind=kind_noahmp) :: FMB             ! M-O momentum stability correction, above ZeroPlaneDisp, bare ground
    real(kind=kind_noahmp) :: FHB             ! M-O sen heat stability correction, above ZeroPlaneDisp, bare ground
    real(kind=kind_noahmp) :: FM2B            ! M-O momentum stability correction, 2m, bare ground
    real(kind=kind_noahmp) :: FH2B            ! M-O sen heat stability correction, 2m, bare ground
    real(kind=kind_noahmp) :: CM              ! exchange coefficient (m/s) for momentum, surface, grid mean
    real(kind=kind_noahmp) :: CMV             ! exchange coefficient (m/s) for momentum, above ZeroPlaneDisp, vegetated
    real(kind=kind_noahmp) :: CMB             ! exchange coefficient (m/s) for momentum, above ZeroPlaneDisp, bare ground
    real(kind=kind_noahmp) :: CH              ! exchange coefficient (m/s) for heat, surface, grid mean
    real(kind=kind_noahmp) :: CHV             ! exchange coefficient (m/s) for heat, above ZeroPlaneDisp, vegetated
    real(kind=kind_noahmp) :: CHB             ! exchange coefficient (m/s) for heat, above ZeroPlaneDisp, bare ground
    real(kind=kind_noahmp) :: CH2V            ! exchange coefficient (m/s) for heat, 2m, vegetated from MOST scheme
    real(kind=kind_noahmp) :: CH2B            ! exchange coefficient (m/s) for heat, 2m, bare ground from MOST scheme
    real(kind=kind_noahmp) :: CHV2            ! exchange coefficient (m/s) for heat, 2m, vegetated from vege_flux
    real(kind=kind_noahmp) :: CAW             ! latent heat conductance/exchange coeff, canopy air (m/s)
    real(kind=kind_noahmp) :: CTW             ! transpiration conductance, leaf to canopy air (m/s)
    real(kind=kind_noahmp) :: CEW             ! evaporation conductance, leaf to canopy air (m/s)
    real(kind=kind_noahmp) :: CGW             ! latent heat conductance, ground to canopy air (m/s)
    real(kind=kind_noahmp) :: ResistanceMomUndCan            ! aerodynamic resistance for momentum (s/m), ground, below canopy
    real(kind=kind_noahmp) :: ResistanceShUndCan            ! aerodynamic resistance for sensible heat (s/m), ground, below canopy
    real(kind=kind_noahmp) :: ResistanceLhUndCan            ! aerodynamic resistance for water vapor (s/m), ground, below canopy
    real(kind=kind_noahmp) :: ResistanceMomAbvCan            ! aerodynamic resistance for momentum (s/m), above canopy
    real(kind=kind_noahmp) :: ResistanceShAbvCan            ! aerodynamic resistance for sensible heat (s/m), above canopy
    real(kind=kind_noahmp) :: ResistanceLhAbvCan            ! aerodynamic resistance for water vapor (s/m), above canopy
    real(kind=kind_noahmp) :: ResistanceMomBareGrd            ! aerodynamic resistance for momentum (s/m), bare ground
    real(kind=kind_noahmp) :: ResistanceShBareGrd            ! aerodynamic resistance for sensible heat (s/m), bare ground
    real(kind=kind_noahmp) :: ResistanceLhBareGrd            ! aerodynamic resistance for water vapor (s/m), bare ground
    real(kind=kind_noahmp) :: ResistanceLeafBoundary              ! bulk leaf boundary layer resistance (s/m)
    real(kind=kind_noahmp) :: TemperaturePotRefHeight           ! potential temp at reference height (K)
    real(kind=kind_noahmp) :: WindSpdRefHeight              ! wind speed (m/s) at reference height
    real(kind=kind_noahmp) :: WSTARV          ! friction velocity in vertical direction (m/s), vegetated (only for Chen97)
    real(kind=kind_noahmp) :: WSTARB          ! friction velocity in vertical direction (m/s), bare ground (only for Chen97)
    real(kind=kind_noahmp) :: EmissivityVeg             ! vegetation emissivity
    real(kind=kind_noahmp) :: EmissivityGrd             ! ground emissivity
    real(kind=kind_noahmp) :: RSURF           ! ground surface resistance (s/m)
    real(kind=kind_noahmp) :: GAMMAV          ! psychrometric constant (pa/K), canopy
    real(kind=kind_noahmp) :: LATHEAV         ! latent heat of vaporization/subli (j/kg), canopy
    real(kind=kind_noahmp) :: GAMMAG          ! psychrometric constant (pa/K), ground
    real(kind=kind_noahmp) :: LATHEAG         ! latent heat of vaporization/subli (j/kg), ground
    real(kind=kind_noahmp) :: RHSUR           ! raltive humidity in surface soil/snow air space (-)
    real(kind=kind_noahmp) :: SpecHumiditySfcBare            ! specific humidity at bare surface
    real(kind=kind_noahmp) :: SpecHumiditySfc              ! specific humidity at surface grid mean
    real(kind=kind_noahmp) :: SpecHumidity2mVeg             ! specific humidity at 2m vegetated
    real(kind=kind_noahmp) :: SpecHumidity2mBare             ! specific humidity at 2m bare ground
    real(kind=kind_noahmp) :: SpecHumidity2m           ! specific humidity at 2m grid mean
    real(kind=kind_noahmp) :: TemperatureGrdVeg             ! vegetated ground (below-canopy) temperature (K)
    real(kind=kind_noahmp) :: TemperatureGrdBare             ! bare ground temperature (K)
    real(kind=kind_noahmp) :: WindStressEwVeg           ! wind stress: east-west (n/m2) above canopy
    real(kind=kind_noahmp) :: WindStressNsVeg           ! wind stress: north-south (n/m2) above canopy
    real(kind=kind_noahmp) :: WindStressEwBare           ! wind stress: east-west (n/m2) bare ground
    real(kind=kind_noahmp) :: WindStressNsBare           ! wind stress: north-south (n/m2) bare ground
    real(kind=kind_noahmp) :: WindStressEwTot            ! wind stress: east-west (n/m2) grid mean
    real(kind=kind_noahmp) :: WindStressNsTot            ! wind stress: north-south (n/m2) grid mean
    real(kind=kind_noahmp) :: TemperatureAir2mVeg            ! 2 m height air temperature (k), vegetated
    real(kind=kind_noahmp) :: TemperatureAir2mBare            ! 2 m height air temperature (k), bare ground
    real(kind=kind_noahmp) :: TemperatureAir2m             ! 2 m height air temperature (k), grid mean
    real(kind=kind_noahmp) :: CHLEAF          ! leaf sensible heat exchange coefficient (m/s)
    real(kind=kind_noahmp) :: CHUC            ! under canopy sensible heat exchange coefficient (m/s)
    real(kind=kind_noahmp) :: EHB             ! bare ground sensible heat exchange coefficient (m/s)
    real(kind=kind_noahmp) :: EHB2            ! bare ground 2-m sensible heat exchange coefficient (m/s)
    real(kind=kind_noahmp) :: EMB             ! bare ground momentum exchange coefficient (m/s)
    real(kind=kind_noahmp) :: RefHeightAboveGrd    ! reference height [m] above ground
    real(kind=kind_noahmp) :: CanopyFracSnowBury         ! fraction of canopy buried by snow
    real(kind=kind_noahmp) :: ZBOTSNO         ! depth of lower boundary condition (m) from snow surface
    real(kind=kind_noahmp) :: RoughLenMomSfcToAtm           ! roughness length, momentum, surface, sent to coupled atmos model
    real(kind=kind_noahmp) :: TemperatureRadSfc            ! radiative temperature (K)
    real(kind=kind_noahmp) :: EmissivitySfc          ! surface emissivity
    real(kind=kind_noahmp) :: AlbedoSfc          ! total surface albedo
    real(kind=kind_noahmp) :: ERRENG          ! error in surface energy balance [w/m2]
    real(kind=kind_noahmp) :: ERRSW           ! error in shortwave radiation balance [w/m2]

    real(kind=kind_noahmp), allocatable, dimension(:) :: TemperatureSoilSnow         ! snow and soil layer temperature [k]
    real(kind=kind_noahmp), allocatable, dimension(:) :: HeatCapacVolSnow       ! snow layer volumetric specific heat capacity (j/m3/k)
    real(kind=kind_noahmp), allocatable, dimension(:) :: ThermConductSnow       ! snow layer thermal conductivity (w/m/k)
    real(kind=kind_noahmp), allocatable, dimension(:) :: HeatCapacVolSoil      ! soil layer volumetric specific heat capacity (j/m3/k)
    real(kind=kind_noahmp), allocatable, dimension(:) :: ThermConductSoil      ! soil layer thermal conductivity (w/m/k)
    real(kind=kind_noahmp), allocatable, dimension(:) :: CVGLAICE    ! glacier ice layer volumetric specific heat (j/m3/k)
    real(kind=kind_noahmp), allocatable, dimension(:) :: TKGLAICE    ! glacier ice thermal conductivity (w/m/k)
    real(kind=kind_noahmp), allocatable, dimension(:) :: ThermConductSoilSnow          ! thermal conductivity [w/m/k] for all soil and snow layers
    real(kind=kind_noahmp), allocatable, dimension(:) :: HeatCapacSoilSnow       ! heat capacity [j/m3/k] for all snow and soil layers
    real(kind=kind_noahmp), allocatable, dimension(:) :: PhaseChgFacSoilSnow        ! energy factor for soil and snow phase change
    real(kind=kind_noahmp), allocatable, dimension(:) :: AlbedoSnowDir      ! snow albedo for direct(1=vis, 2=nir)
    real(kind=kind_noahmp), allocatable, dimension(:) :: AlbedoSnowDif      ! snow albedo for diffuse(1=vis, 2=nir)
    real(kind=kind_noahmp), allocatable, dimension(:) :: AlbedoSoilDir      ! soil albedo (direct)
    real(kind=kind_noahmp), allocatable, dimension(:) :: AlbedoSoilDif      ! soil albedo (diffuse)
    real(kind=kind_noahmp), allocatable, dimension(:) :: AlbedoGrdDir      ! ground albedo (direct beam: vis, nir)
    real(kind=kind_noahmp), allocatable, dimension(:) :: AlbedoGrdDif      ! ground albedo (diffuse: vis, nir)
    real(kind=kind_noahmp), allocatable, dimension(:) :: ReflectanceVeg         ! leaf/stem reflectance weighted by LeafAreaIndex and StemAreaIndex
    real(kind=kind_noahmp), allocatable, dimension(:) :: TransmittanceVeg         ! leaf/stem transmittance weighted by LeafAreaIndex and StemAreaIndex
    real(kind=kind_noahmp), allocatable, dimension(:) :: AlbedoSfcDir        ! surface albedo (direct)
    real(kind=kind_noahmp), allocatable, dimension(:) :: AlbedoSfcDif        ! surface albedo (diffuse)

  end type state_type


!=== define "parameter" sub-type of energy (energy%param%variable)
  type :: parameter_type

    real(kind=kind_noahmp) :: TreeCrownRadius             ! tree crown radius [m]
    real(kind=kind_noahmp) :: HeightCanopyTop             ! height of canopy top [m]
    real(kind=kind_noahmp) :: HeightCanopyBot             ! height of canopy bottom [m]
    real(kind=kind_noahmp) :: RoughLenMomVeg              ! momentum roughness length [m] vegetated
    real(kind=kind_noahmp) :: TreeDensity                 ! tree density [no. of trunks per m2]
    real(kind=kind_noahmp) :: CanopyOrientIndex           ! leaf/stem orientation index
    real(kind=kind_noahmp) :: UpscatterCoeffSnowDir       ! Upscattering parameters for snow for direct radiation
    real(kind=kind_noahmp) :: UpscatterCoeffSnowDif       ! Upscattering parameters for snow for diffuse radiation
    real(kind=kind_noahmp) :: SoilHeatCapacity            ! volumetric soil heat capacity [j/m3/K]
    real(kind=kind_noahmp) :: SnowAgeFacBats              ! snow aging parameter for BATS snow albedo
    real(kind=kind_noahmp) :: SnowGrowVapFacBats          ! vapor diffusion snow growth factor for BATS snow albedo
    real(kind=kind_noahmp) :: SnowSootFacBats             ! dirt and soot effect factor for BATS snow albedo
    real(kind=kind_noahmp) :: SnowGrowFrzFacBats          ! extra snow growth factor near freezing for BATS snow albedo
    real(kind=kind_noahmp) :: SolarZenithAdjBats          ! zenith angle snow albedo adjustment
    real(kind=kind_noahmp) :: FreshSnoAlbVisBats          ! new snow visible albedo for BATS
    real(kind=kind_noahmp) :: FreshSnoAlbNirBats          ! new snow NIR albedo for BATS
    real(kind=kind_noahmp) :: SnoAgeFacDifVisBats         ! age factor for diffuse visible snow albedo for BATS
    real(kind=kind_noahmp) :: SnoAgeFacDifNirBats         ! age factor for diffuse NIR snow albedo for BATS
    real(kind=kind_noahmp) :: SzaFacDirVisBats            ! cosz factor for direct visible snow albedo for BATS
    real(kind=kind_noahmp) :: SzaFacDirNirBats            ! cosz factor for direct NIR snow albedo for BATS
    real(kind=kind_noahmp) :: SnowAlbRefClass             ! reference snow albedo in CLASS scheme
    real(kind=kind_noahmp) :: SnowAgeFacClass             ! snow aging e-folding time [s] in CLASS albedo scheme
    real(kind=kind_noahmp) :: SnowAlbFreshClass           ! fresh snow albedo in CLASS albedo scheme
    real(kind=kind_noahmp) :: ConductanceLeafMin          ! minimum leaf conductance [umol/m**2/s]
    real(kind=kind_noahmp) :: Co2MmConst25C               ! co2 michaelis-menten constant at 25c [Pa]
    real(kind=kind_noahmp) :: O2MmConst25C                ! o2 michaelis-menten constant at 25c [Pa]
    real(kind=kind_noahmp) :: Co2MmConstQ10               ! change in co2 Michaelis-Menten constant for every 10-deg C temperature change
    real(kind=kind_noahmp) :: O2MmConstQ10                ! change in o2 michaelis-menten constant for every 10-deg C temperature change
    real(kind=kind_noahmp) :: RadiationStressFac          ! Parameter used in radiation stress function in Jarvis scheme
    real(kind=kind_noahmp) :: ResistanceStomataMin        ! Minimum stomatal resistance [s m-1] in Jarvis scheme
    real(kind=kind_noahmp) :: ResistanceStomataMax        ! Maximal stomatal resistance [s m-1] in Jarvis scheme
    real(kind=kind_noahmp) :: AirTempOptimTransp          ! Optimum transpiration air temperature [K] in Jarvis scheme
    real(kind=kind_noahmp) :: VaporPresDeficitFac         ! Parameter used in vapor pressure deficit function in Jarvis scheme
    real(kind=kind_noahmp) :: LeafDimLength               ! characteristic leaf dimension [m]
    real(kind=kind_noahmp) :: ZilitinkevichCoeff          ! Zilitinkevich coefficient for heat exchange coefficient calculation
    real(kind=kind_noahmp) :: EmissivitySnow              ! snow emissivity
    real(kind=kind_noahmp) :: CanopyWindExtFac            ! empirical canopy wind extinction parameter
    real(kind=kind_noahmp) :: RoughLenMomSnow              ! snow surface roughness length [m]
    real(kind=kind_noahmp) :: RoughLenMomSoil             ! Bare-soil roughness length [m]
    real(kind=kind_noahmp) :: RoughLenMomLake             ! lake surface roughness length [m]
    real(kind=kind_noahmp) :: EmissivityIceSfc            ! ice surface emissivity
    real(kind=kind_noahmp) :: ResistanceSoilExp           ! exponent in the shape parameter for soil resistance option 1
    real(kind=kind_noahmp) :: ResistanceSnowSfc           ! surface resistance for snow [s/m]
    real(kind=kind_noahmp) :: VegFracGreen                ! green vegetation fraction
    real(kind=kind_noahmp) :: VegFracAnnMax               ! annual maximum vegetation fraction

    real(kind=kind_noahmp), allocatable, dimension(:) :: LeafAreaIndexMon      ! monthly leaf area index, one-sided
    real(kind=kind_noahmp), allocatable, dimension(:) :: StemAreaIndexMon      ! monthly stem area index, one-sided
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilQuartzFrac        ! soil quartz content
    real(kind=kind_noahmp), allocatable, dimension(:) :: AlbedoSoilSat         ! saturated soil albedos: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:) :: AlbedoSoilDry         ! dry soil albedos: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:) :: AlbedoLakeFrz         ! albedo frozen lakes: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:) :: ScatterCoeffSnow      ! Scattering coefficient for snow
    real(kind=kind_noahmp), allocatable, dimension(:) :: ReflectanceLeaf       ! leaf reflectance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:) :: ReflectanceStem       ! stem reflectance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:) :: TransmittanceLeaf     ! leaf transmittance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:) :: TransmittanceStem     ! stem transmittance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:) :: EmissivitySoilLake    ! emissivity soil surface: 1=soil, 2=lake
    real(kind=kind_noahmp), allocatable, dimension(:) :: AlbedoLandIce         ! land/glacier ice albedo: 1=vis, 2=nir

  end type parameter_type


!=== define energy type that includes 3 subtypes (flux,state,parameter)
  type, public :: energy_type

    type(flux_type)      :: flux
    type(state_type)     :: state
    type(parameter_type) :: param

  end type energy_type

end module EnergyVarType
