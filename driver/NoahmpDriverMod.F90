Program NoahmpDriverMod

  use Machine, only : kind_noahmp
  use ConstantDefineMod
  use NoahmpVarType
  use InputVarType
  use InputVarInitMod
  use ConfigVarInitMod
  use EnergyVarInitMod
  use ForcingVarInitMod
  use WaterVarInitMod
  use WaterMainMod
  use NoahmpOutputMod
  use IrrigationTriggerMod
  use IrrigationSprinklerMod

  implicit none
!---------------------------------------------------------------------
!  types
!---------------------------------------------------------------------
  type(input_type)    :: input
  type(noahmp_type)   :: noahmp

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------

  integer                :: itime, iz, isoil   ! some loop counters
  integer                :: ntime      = 0     ! number of timesteps to run
  integer                :: rain_steps = 0     ! number of timesteps in rain event
  integer                :: dry_steps  = 0     ! number of timesteps between rain events
  integer                :: rain_step  = 0     ! number of timesteps in current event
  integer                :: dry_step   = 0     ! number of timesteps in current event
  real(kind=kind_noahmp) :: totalwat   = 0.0   ! total soil water [mm]
  real(kind=kind_noahmp) :: tw0        = 0.0   ! initial total soil water [mm]
  real(kind=kind_noahmp) :: errwat     = 0.0   ! water balance error at each timestep [mm]
  real(kind=kind_noahmp) :: FB_snow            ! canopy fraction buried by snow
  real(kind=kind_noahmp) :: LAI                ! leaf area index
  real(kind=kind_noahmp) :: SAI                ! stem area index
  real(kind=kind_noahmp) :: RAIN               ! total rain rate mm/s
  real(kind=kind_noahmp) :: SNOW               ! total snow rate mm/s
  real(kind=kind_noahmp) :: LATHEAV            ! latent heat vap./sublimation (j/kg) for canopy
  real(kind=kind_noahmp) :: LATHEAG            ! latent heat vap./sublimation (j/kg) for ground
  real(kind=kind_noahmp) :: QAIR               ! specific humidity
  logical                :: raining            ! .true. if raining

!---------------------------------------------------------------------
!  read in input data from table and initial file
!---------------------------------------------------------------------
  call InputVarInitDefault(input)
  call ReadNamelist(input)
  call ReadNoahmpTable(input)

!---------------------------------------------------------------------
!  initialize
!---------------------------------------------------------------------
  call ConfigVarInitDefault(noahmp)
  call ConfigVarInitTransfer(noahmp, input)
  call ForcingVarInitDefault(noahmp)
  call ForcingVarInitTransfer(noahmp, input)
  call EnergyVarInitDefault(noahmp)
  call EnergyVarInitTransfer(noahmp, input)
  call WaterVarInitDefault(noahmp)
  call WaterVarInitTransfer(noahmp, input)
!---------------------------------------------------------------------

!---------------------------------------------------------------------
  associate(                                                        &
            DT              => noahmp%config%domain%DT             ,& ! in,     main noahmp timestep (s)
            NSOIL           => noahmp%config%domain%NSOIL          ,& ! in,     number of soil layers
            ZSNSO           => noahmp%config%domain%ZSNSO          ,& ! inout,  depth of snow/soil layer-bottom (m)
            SFCTMP          => noahmp%forcing%SFCTMP               ,& ! in,     surface air temperature [k] from Atmos forcing
            TV              => noahmp%energy%state%TV              ,& ! inout, vegetation temperature (k)
            TG              => noahmp%energy%state%TG              ,& ! in,    ground temperature (k)
            IMELT           => noahmp%water%state%IMELT            ,& ! in,     phase change index [0-none;1-melt;2-refreeze]
            CANLIQ          => noahmp%water%state%CANLIQ           ,& ! inout, intercepted liquid water (mm)
            CANICE          => noahmp%water%state%CANICE           ,& ! inout, intercepted ice mass (mm)
            STC             => noahmp%energy%state%STC             ,& ! inout,  snow and soil layer temperature [k]
            SH2O            => noahmp%water%state%SH2O             ,& ! inout,  soil water content [m3/m3]
            SICE            => noahmp%water%state%SICE             ,& ! inout,  soil ice moisture (m3/m3)
            IST             => noahmp%config%domain%IST            ,& ! in,     surface type 1-soil; 2-lake 
            DX              => noahmp%config%domain%DX             ,& ! in,     noahmp model grid spacing (m)
            FCEV            => noahmp%energy%flux%FCEV             ,& ! in,    canopy evaporation (w/m2) [+ = to atm]
            FCTR            => noahmp%energy%flux%FCTR             ,& ! in,    transpiration (w/m2) [+ = to atm]
            PONDING         => noahmp%water%state%PONDING          ,& ! inout,  melting water from snow when there is no layer (mm)
            FICEOLD         => noahmp%water%state%FICEOLD_SNOW     ,& ! in,     ice fraction in snow layers at last timestep
            FVEG            => noahmp%energy%state%FVEG            ,& ! in,    greeness vegetation fraction (-)
            SMCEQ           => noahmp%water%state%SMCEQ            ,& ! in,     equilibrium soil water  content [m3/m3]
            BDFALL          => noahmp%water%state%BDFALL           ,& ! in,    bulk density of snowfall (kg/m3)
            QRAIN           => noahmp%water%flux%QRAIN             ,& ! in,     snow surface rain rate[mm/s]
            QSNOW           => noahmp%water%flux%QSNOW             ,& ! in,     snow at ground srf (mm/s) [+]
            SNOWHIN         => noahmp%water%flux%SNOWHIN           ,& ! in,     snow depth increasing rate (m/s)
            ISNOW           => noahmp%config%domain%ISNOW          ,& ! inout,  actual number of snow layers
            SNOWH           => noahmp%water%state%SNOWH            ,& ! inout,  snow depth [m]
            SNEQV           => noahmp%water%state%SNEQV            ,& ! inout,  snow water equivalent [mm]
            SNICE           => noahmp%water%state%SNICE            ,& ! inout,  snow layer ice [mm]
            SNLIQ           => noahmp%water%state%SNLIQ            ,& ! inout,  snow layer liquid water [mm]
            SMC             => noahmp%water%state%SMC              ,& ! inout,  total soil moisture [m3/m3]
            ZWT             => noahmp%water%state%ZWT              ,& ! inout,  water table depth [m]
            WA              => noahmp%water%state%WA               ,& ! inout,  water storage in aquifer [mm]
            WT              => noahmp%water%state%WT               ,& ! inout,  water storage in aquifer + saturated soil [mm]
            WSLAKE          => noahmp%water%state%WSLAKE           ,& ! inout,  water storage in lake (can be -) (mm)
            SMCWTD          => noahmp%water%state%SMCWTD           ,& ! inout,  soil moisture between bottom of the soil and the water table
            DEEPRECH        => noahmp%water%state%DEEPRECH         ,& ! inout,  recharge to or from the water table when deep [m]
            RECH            => noahmp%water%state%RECH             ,& ! out,    groundwater recharge (net vertical flux across the water table), positive up
            CMC             => noahmp%water%state%CMC              ,& ! out,   total canopy intercepted water (mm)
            ECAN            => noahmp%water%flux%ECAN              ,& ! out,   evaporation of intercepted water (mm/s) [+]
            ETRAN           => noahmp%water%flux%ETRAN             ,& ! out,   transpiration rate (mm/s) [+]
            FWET            => noahmp%water%state%FWET             ,& ! out,   wetted or snowed fraction of the canopy
            RUNSRF          => noahmp%water%flux%RUNSRF            ,& ! out,    surface runoff [mm/s]
            RUNSUB          => noahmp%water%flux%RUNSUB            ,& ! out,    subsurface runoff [mm/s] 
            QDIS            => noahmp%water%flux%QDIS              ,& ! out,    groundwater discharge [mm/s]
            QIN             => noahmp%water%flux%QIN               ,& ! out,    groundwater recharge [mm/s] 
            PONDING1        => noahmp%water%state%PONDING1         ,& ! out,    surface ponding 1 (mm)
            PONDING2        => noahmp%water%state%PONDING2         ,& ! out,    surface ponding 2 (mm)
            QSNBOT          => noahmp%water%flux%QSNBOT            ,& ! out,    melting water out of snow bottom [mm/s]
            QTLDRN          => noahmp%water%flux%QTLDRN            ,& ! inout,  tile drainage (mm/s)
            QINSUR          => noahmp%water%flux%QINSUR            ,& ! in,     water input on soil surface [mm/s]
            QSEVA           => noahmp%water%flux%QSEVA             ,& ! in,     evaporation from soil surface [mm/s]
            ETRANI          => noahmp%water%flux%ETRANI            ,& ! in,     evapotranspiration from soil layers [mm/s]
            QSNFRO          => noahmp%water%flux%QSNFRO            ,& ! in,     snow surface frost rate[mm/s]
            QSNSUB          => noahmp%water%flux%QSNSUB            ,& ! in,     snow surface sublimation rate[mm/s]
            SNOFLOW         => noahmp%water%flux%SNOFLOW           ,& ! out,    glacier flow [mm/s]
            QSDEW           => noahmp%water%flux%QSDEW             ,& ! inout,  soil surface dew rate [mm/s]
            QDRAIN          => noahmp%water%flux%QDRAIN            ,& ! in,    soil bottom drainage (m/s)
            FCRMAX          => noahmp%water%state%FCRMAX           ,& ! in,     maximum fraction of imperviousness (FCR)
            WCND            => noahmp%water%state%WCND             ,& ! out,    soil hydraulic conductivity (m/s)
            sfcheadrt       => noahmp%water%state%sfcheadrt        ,& ! inout,  surface water head (mm) 
            WATBLED         => noahmp%water%state%WATBLED          ,& ! in,     water table depth estimated in WRF-Hydro fine grids (m)
            FROZEN_CANOPY   => noahmp%energy%state%FROZEN_CANOPY   ,& ! in,    used to define latent heat pathway
            FROZEN_GROUND   => noahmp%energy%state%FROZEN_GROUND   ,& ! in,     frozen ground (logical) to define latent heat pathway
            QVAP            => noahmp%water%flux%QVAP              ,& ! in,     soil surface evaporation rate[mm/s]
            QDEW            => noahmp%water%flux%QDEW              ,& ! in,     soil surface dew rate[mm/s]
            FGEV            => noahmp%energy%flux%FGEV             ,& ! in,     soil evap heat (w/m2) [+ to atm]
            BTRANI          => noahmp%water%state%BTRANI           ,& ! in,     soil water transpiration factor (0 to 1)
            OPT_IRR         => noahmp%config%nmlist%OPT_IRR        ,& ! in,     options for irrigation
            OPT_IRRM        => noahmp%config%nmlist%OPT_IRRM       ,& ! in,     options for irrigation method
            CROPLU          => noahmp%config%domain%CROPLU         ,& ! in,     flag to identify croplands
            IRRFRA          => noahmp%water%state%IRRFRA           ,& ! in,     irrigation fraction
            SIFAC           => noahmp%water%state%SIFAC            ,& ! in,     sprinkler irrigation fraction (0 to 1)
            MIFAC           => noahmp%water%state%MIFAC            ,& ! in,     fraction of grid under micro irrigation (0 to 1)
            FIFAC           => noahmp%water%state%FIFAC            ,& ! in,     fraction of grid under flood irrigation (0 to 1)
            IRAMTFI         => noahmp%water%state%IRAMTFI          ,& ! inout,  flood irrigation water amount [m]
            IRAMTMI         => noahmp%water%state%IRAMTMI          ,& ! inout,  micro irrigation water amount [m]
            IRFIRATE        => noahmp%water%flux%IRFIRATE          ,& ! inout,  flood irrigation water rate [m/timestep]
            IRMIRATE        => noahmp%water%flux%IRMIRATE          ,& ! inout,  micro irrigation water rate [m/timestep]
            OPT_TDRN        => noahmp%config%nmlist%OPT_TDRN       ,& ! in,     options for tile drainage
            TDFRACMP        => noahmp%water%state%TDFRACMP         ,& ! in,     tile drainage map(fraction)
            SMCMAX          => noahmp%water%param%SMCMAX           ,& ! in,     saturated value of soil moisture [m3/m3]
            DZSNSO          => noahmp%config%domain%DZSNSO         ,& ! in,     thickness of snow/soil layers (m)
            LAIM            => noahmp%energy%param%LAIM            ,& ! in,     monthly LAI from table
            SAIM            => noahmp%energy%param%SAIM            ,& ! in,     monthly SAI from table
            ELAI            => noahmp%energy%state%ELAI            ,& ! out,    leaf area index, after burying by snow
            ESAI            => noahmp%energy%state%ESAI            ,& ! out,    stem area index, after burying by snow
            Q2              => noahmp%forcing%Q2                   ,& ! in,     specific humidity kg/kg
            IRAMTSI         => noahmp%water%state%IRAMTSI          ,& ! inout,  irrigation water amount [m] to be applied, Sprinkler
            IRSIRATE        => noahmp%water%flux%IRSIRATE          ,& ! inout,  rate of irrigation by sprinkler [m/timestep]
            IRCNTSI         => noahmp%water%state%IRCNTSI          ,& ! inout,  irrigation event number, Sprinkler
            IRCNTMI         => noahmp%water%state%IRCNTMI          ,& ! inout,  irrigation event number, Micro
            IRCNTFI         => noahmp%water%state%IRCNTFI          ,& ! inout,  irrigation event number, Flood
            EAIR            => noahmp%energy%state%EAIR            ,& ! in,     vapor pressure air (pa)
            IREVPLOS        => noahmp%water%flux%IREVPLOS          ,& ! inout,  loss of irrigation water to evaporation,sprinkler [m/timestep]
            FIRR            => noahmp%energy%flux%FIRR             ,& ! inout,  latent heating due to sprinkler evaporation [w/m2]
            EIRR            => noahmp%water%flux%EIRR              ,& ! inout,  evaporation of irrigation water to evaporation,sprinkler [mm/s]
            SFCPRS          => noahmp%forcing%SFCPRS               ,& ! in,     surface pressure (pa)
            IRR_FRAC        => noahmp%water%param%IRR_FRAC         ,& ! in,     irrigation fraction parameter
            RAIN            => noahmp%water%flux%RAIN              ,& ! inout,  rainfall rate
            SNOW            => noahmp%water%flux%SNOW              ,& ! inout,  snowfall rate
            IR_RAIN         => noahmp%water%param%IR_RAIN           & ! in,     maximum precipitation to stop irrigation trigger
            )
!---------------------------------------------------------------------

! start with a default value at time 0

! input used to adjust for snow and non-snow cases
  if ( input%runsnow .eqv. .true. ) then
     SFCTMP    = 265.0 
     FB_snow   = 0.5
     TV        = 265.0
     TG        = 265.0
     IMELT     = 2  
     CANLIQ    = 0.1
     CANICE    = 4.0
     STC(1:4)  = 265.0
     STC(-2:0) = 0.0
     SH2O(1:4) = 0.03
     SICE(1:4) = 0.2
  else
     SFCTMP    = 298.0
     FB_snow   = 0.0
     TV        = 298.0
     TG        = 298.0
     IMELT     = 1
     CANLIQ    = 0.4
     CANICE    = 0.0
     STC(1:4)  = 298.0
     STC(-2:0) = 0.0
     SH2O(1:4) = 0.2
     SICE(1:4) = 0.03
  end if

! others
  IST     = 1                     ! surface type 1-soil; 2-lake
  DX      = 4000.0                ! grid spacing 4km
  FCEV    = input%FCEVIn          ! canopy evaporation (w/m2) [+ to atm ]
  FCTR    = input%FCTRIn          ! transpiration (w/m2) [+ to atm]
  FGEV    = input%FGEVIn          ! soil evap heat (w/m2) [+ to atm]
  LAI     = LAIM(6)               ! June LAI as an example
  SAI     = SAIM(6)               ! June SAI
  ELAI    = LAI * (1.0 - FB_snow) ! leaf area index, after burying by snow
  ESAI    = SAI * (1.0 - FB_snow) ! stem area index, after burying by snow 
  PONDING = 0.0
  FICEOLD = 0.0
  FVEG    = input%SHDMAXIn / 100.0  ! yearly max vegetation fraction
  if ( FVEG <= 0.05 ) FVEG = 0.05
  SMCEQ(1:4) = 0.3                ! used only for MMF, so set to fixed value
  BDFALL     = 120.0              ! bulk density of snowfall (kg/m3)
  RAIN       = 0.0                ! total rain
  SNOW       = 0.0                ! total snowfall (mm/s)
  QRAIN      = RAIN * 0.99
  QSNOW      = SNOW * 0.9
  SNOWHIN    = QSNOW / BDFALL     ! m/s
  ISNOW      = 0
  SNOWH      = 0.0
  SNEQV      = 0.0
  SNICE      = 0.0
  SNLIQ      = 0.0
  SMC        = SH2O + SICE        ! initial volumetric soil water
  ZWT        = 2.5
  WA         = 0.0
  WT         = 0.0
  WSLAKE     = 0.0
  SMCWTD     = 0.3                ! should only be needed for run=5
  DEEPRECH   = 0.0                ! should only be needed for run=5
  RECH       = 0.0                ! should only be needed for run=5
  CMC        = 0.0
  ECAN       = 0.0
  ETRAN      = 0.0
  FWET       = 0.0  
  RUNSRF     = 0.0
  RUNSUB     = 0.0
  QDIS       = 0.0
  QIN        = 0.0
  PONDING1   = 0.0
  PONDING2   = 0.0
  QSNBOT     = 0.0
  QTLDRN     = 0.0
  QINSUR     = 0.0
  QSEVA      = 0.0
  ETRANI     = 0.0
  QSNFRO     = 0.0
  QSNSUB     = 0.0
  SNOFLOW    = 0.0
  QSDEW      = 0.0
  QDRAIN     = 0.0
  FCRMAX     = 0.0
  WCND       = 0.0
  sfcheadrt  = 0.0
  WATBLED    = 0.0


! set psychrometric constant
  if ( TV > TFRZ ) then           
     LATHEAV       = HVAP          
     FROZEN_CANOPY = .false.
  else
     LATHEAV       = HSUB
     FROZEN_CANOPY = .true.
  endif
  if ( TG > TFRZ ) then
     LATHEAG       = HVAP
     FROZEN_GROUND = .false.
  else
     LATHEAG       = HSUB
     FROZEN_GROUND = .true.
  endif

  QVAP = max( FGEV / LATHEAG, 0.0 )       ! positive part of fgev; Barlage change to ground v3.6
  QDEW = abs(min(FGEV / LATHEAG, 0.0))    ! negative part of fgev
  BTRANI(1:nsoil) = 0.2 ! 0~1


  if ( OPT_IRR > 0) then
     IRRFRA = 1.0  ! irrigation fraction
     CROPLU = .true.
  else
     IRRFRA = 0.0
     CROPLU = .false.
  endif

  if ( OPT_IRRM == 0 ) then
     SIFAC    = 0.3
     MIFAC    = 0.3
     FIFAC    = 0.4
     IRAMTFI  = 0.25
     IRAMTMI  = 0.25
     IRAMTSI  = 0.5
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  elseif ( OPT_IRRM == 1 ) then ! sprinkler
     SIFAC    = 1.0
     MIFAC    = 0.0
     FIFAC    = 0.0
     IRAMTFI  = 0.0
     IRAMTMI  = 0.0
     IRAMTSI  = 0.5
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  elseif ( OPT_IRRM == 2 ) then ! micro
     SIFAC    = 0.0
     MIFAC    = 1.0
     FIFAC    = 0.0
     IRAMTFI  = 0.0
     IRAMTMI  = 0.5
     IRAMTSI  = 0.0
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  elseif ( OPT_IRRM == 3 ) then ! flood
     SIFAC    = 0.0
     MIFAC    = 0.0
     FIFAC    = 1.0
     IRAMTFI  = 0.5
     IRAMTMI  = 0.0
     IRAMTSI  = 0.0
     IRFIRATE = 0.0
     IRMIRATE = 0.0
     IRSIRATE = 0.0
  endif

!  IRAMTFI = 0.0
!  IRAMTMI = 0.0
!  IRAMTSI = 0.0


  IRCNTSI = 0
  IRCNTMI = 0
  IRCNTFI = 0
  QAIR = Q2
  EAIR   = QAIR*SFCPRS / (0.622+0.378*QAIR)
  IREVPLOS = 0.0
  FIRR = 0.0
  EIRR = 0.0

  if ( OPT_TDRN > 0 ) then
      TDFRACMP = 0.5
      ZWT      = 0.2  ! to allow the drainage effect to show up
  else
      TDFRACMP = 0.0
  endif


! for other variables
  DT         = input%DTIn
  ntime      = nint(input%maxtime * 3600.0 / DT)
  rain_steps = input%rain_duration * 3600.0 / DT
  dry_steps  = input%dry_duration * 3600.0 / DT
  raining    = input%raining

! prevent too large SMC initial values
  do isoil = 1, NSOIL
     if ( SMC(isoil) > SMCMAX(isoil) ) then
        SH2O(isoil) = SMCMAX(isoil) * SH2O(isoil) / SMC(isoil)
        SMC(isoil)  = SMCMAX(isoil)
        SICE(isoil) = max( 0.0, SMC(isoil)-SH2O(isoil) )
     endif
  enddo

!!!!!!========= initialization complete ==================================

!---------------------------------------------------------------------
! create output file and add initial values
!---------------------------------------------------------------------

  call initialize_output(noahmp, input, ntime+1)
  call add_to_output(0, noahmp, errwat)


!---------------------------------------------------------------------
! start the time loop
!---------------------------------------------------------------------

  do itime = 1, ntime


    tw0 = sum(DZSNSO(1:NSOIL) * SMC * 1000.0) + SNEQV + WA ! [mm] 

    IRFIRATE = 0.0
    IRMIRATE = 0.0
    IRSIRATE = 0.0
    IREVPLOS = 0.0 
    FIRR     = 0.0
    EIRR     = 0.0

  !---------------------------------------------------------------------
  ! calculate the input water
  !---------------------------------------------------------------------

    if ( raining .eqv. .true. ) then
       RAIN      = input%rainrate / 3600.0    ! input water [m/s]
       rain_step = rain_step + 1
       if ( rain_step == rain_steps ) then            ! event length met
          rain_step = 0
          raining   = .false.
      endif
    else
      RAIN     = 0.0                        ! stop water input [m/s]
      dry_step = dry_step + 1
      if ( dry_step == dry_steps ) then              ! between event length met
        dry_step = 0
        raining  = .true.
      endif
    endif

    if ( input%runsnow .eqv. .true. ) then
       SNOW = RAIN * 1.0
       RAIN = 0.0
    else
       SNOW = 0.0
    endif


!---------------------------------------------------------------------
!--------------------------------------------------------------------- 
! main noahmplsm subroutine below

    !---------------------------------------------------------------------
    ! call irrigation trigger and sprinkler irrigation
    !--------------------------------------------------------------------- 

    if ( (CROPLU .eqv. .true.) .and. (IRRFRA >= IRR_FRAC) .and. (RAIN < (IR_RAIN/3600.0)) .and. &
         ((IRAMTSI+IRAMTMI+IRAMTFI) == 0.0) ) then
       call IrrigationTrigger(noahmp)
    endif
    ! set irrigation off if larger than IR_RAIN mm/h for this time step and irr triggered last time step
    if ( (RAIN >= (IR_RAIN/3600.0)) .or. (IRRFRA < IRR_FRAC) ) then
        IRAMTSI = 0.0
        IRAMTMI = 0.0
        IRAMTFI = 0.0
    endif

    ! call sprinkler irrigation before CANWAT/PRECIP_HEAT to have canopy interception
    if ( (CROPLU .eqv. .true.) .and. (IRAMTSI > 0.0) ) then
       call SprinklerIrrigation(noahmp)
       RAIN = RAIN + (IRSIRATE * 1000.0 / DT) ![mm/s]
       ! cooling and humidification due to sprinkler evaporation, per m^2 calculation 
       FIRR = IREVPLOS * 1000.0 * HVAP / DT   ! heat used for evaporation (W/m2)
       EIRR = IREVPLOS * 1000.0 / DT          ! sprinkler evaporation (mm/s)
    endif

    !---------------------------------------------------------------------
    ! call canopy water interception and precip heat advection
    !--------------------------------------------------------------------- 
    QRAIN = RAIN * 0.99
    QSNOW = SNOW * 0.9
    SNOWHIN = QSNOW / BDFALL

    !---------------------------------------------------------------------
    ! call the main water routines
    !--------------------------------------------------------------------- 

    call WaterMain(noahmp)


! main noahmplsm subroutines above
!--------------------------------------------------------------------- 
!---------------------------------------------------------------------


! some updates from last time step for use in next step (from drv)

    FICEOLD(ISNOW+1:0) = SNICE(ISNOW+1:0) /(SNICE(ISNOW+1:0) + SNLIQ(ISNOW+1:0))

! balance check for soil and snow layers  
    totalwat = sum(DZSNSO(1:NSOIL) * SMC * 1000.0) + SNEQV + WA      ! total soil+snow water [mm]
    errwat   = (QRAIN+QSNOW+IRMIRATE*1000.0/DT+IRFIRATE*1000.0/DT+QDEW-QVAP-ETRAN-RUNSRF-RUNSUB-QTLDRN)*DT - (totalwat - tw0)  ! water balance error [mm]

  if (abs(errwat) > 0.1) print*,'water not balanced ....'

    !---------------------------------------------------------------------
    ! add to output file
    !---------------------------------------------------------------------

    call add_to_output(itime, noahmp, errwat)
   
  end do ! time loop

  call finalize_output()
   
  end associate

  print*, 'model run successfully completed ...'


end program NoahmpDriverMod
