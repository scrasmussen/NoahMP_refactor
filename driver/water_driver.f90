!
! compile: 
!

program water_driver

use output
use water_routines, only: canwater_intercept, canwater, snowwater, soilwater, noahmp_parameters

  implicit none

!---------------------------------------------------------------------
!  inputs start
!---------------------------------------------------------------------

  real          :: dt
  integer       :: maxtime
  character*256 :: output_filename
  real          :: rainrate
  integer       :: rain_duration
  integer       :: dry_duration
  logical       :: raining
  real          :: evaprate
  real          :: tranrate
  integer       :: isltyp
  integer       :: nsoil
  integer       :: nsnow
  integer       :: structure_option
  real          :: soil_depth
  real          :: vegfra
  real          :: vegmax
  integer       :: vegtyp
  real          :: shdmax
  real          :: fcev_e
  real          :: fctr_e
  real          :: qvap

  real, allocatable, dimension(:) :: zsoil   ! depth of layer-bottom from soil surface
  real, allocatable, dimension(:) :: dzsnso  ! snow/soil layer thickness [m]
  real, allocatable, dimension(:) :: sice    ! soil ice content [m3/m3]
  real, allocatable, dimension(:) :: sh2o    ! soil liquid water content [m3/m3]
  logical :: initial_uniform                 ! initial all levels the same
  real    :: initial_sh2o_value              ! constant sh2o value
  real    :: initial_sice_value              ! constant sice value

  !--------------------!
  !  soil parameters   !
  !--------------------!

  real, dimension(12) ::      bb  ! b parameter
  real, dimension(12) ::   satdk  ! conductivity at saturation
  real, dimension(12) ::   satdw  ! diffusivity at saturation
  real, dimension(12) ::  maxsmc  ! porosity
  real, dimension(12) ::  satpsi  ! matric potential at saturation
  real, dimension(12) ::  wltsmc  ! wilting point
  real, dimension(12) ::  refsmc  ! field capacity
  real, dimension(12) :: pctsand  ! percent sand
  real, dimension(12) :: pctclay  ! percent clay
  real                ::   slope  ! free drainage parameter
! addrunoff
  real, dimension(12) ::   bvic   !VIC or DVIC model infiltration parameter
  real, dimension(12) ::   AXAJ   !Xinanjiang: Tension water distribution inflection parameter [-]
  real, dimension(12) ::   BXAJ   !Xinanjiang: Tension water distribution shape parameter [-]
  real, dimension(12) ::   XXAJ   !Xinanjiang: Free water distribution shape parameter [-]
  real, dimension(12) ::   G      !Mean Capillary Drive (m) for infiltration models
  real, dimension(12) ::   BBVIC  !DVIC heterogeniety parameter for infiltration 

  !--------------------!
  ! vegetation parameters   !
  !--------------------!
  real, dimension(20) ::  CH2OP !maximum intercepted h2o per unit lai+sai (mm)
  real, dimension(20) ::  SAI_APR 
  real, dimension(20) ::  LAI_APR

! read namelist (for test, include MPTABLE in namelist) 
  namelist / timing          / dt,maxtime,output_filename
  namelist / forcing         / rainrate,rain_duration,dry_duration,&
                               raining,evaprate,tranrate,fcev_e,fctr_e,qvap
  namelist / structure       / isltyp,nsoil,nsnow,structure_option,soil_depth,&
                               vegfra,vegmax,vegtyp,shdmax
  namelist / fixed_initial   / zsoil,dzsnso,sice,sh2o
  namelist / uniform_initial / initial_uniform,initial_sh2o_value,&
                               initial_sice_value
  namelist / soil_parameters / bb,satdk,satdw,maxsmc,satpsi,wltsmc, &
                               refsmc,pctsand,pctclay,bvic,AXAJ,BXAJ,XXAJ,&
                               BBVIC,G,slope !addrunoff
  namelist / veg_modis_parameters / CH2OP,SAI_APR,LAI_APR 
 
!---------------------------------------------------------------------
!  inputs end
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!  additional variables passed to soilwater
!---------------------------------------------------------------------

  real                            :: qinsur      !water input on soil surface [m/s]
  real                            :: qseva       !soil surface evap rate [mm/s]
  real                            :: runsrf      !surface runoff [mm/s] 
  real                            :: runsub      !baseflow (sturation excess) [mm/s]
  real                            :: qdrain      !soil-bottom free drainage [mm/s] 
  real                            :: zwt         !the depth to water table [m]
  real                            :: smcwtd      !soil water content between bottom of the soil and water table [m3/m3]
  real                            :: deeprech    !recharge to or from the water table when deep [m]
  real                            :: fcrmax      !maximum of fcr (-)
  real                            :: WA      !water storage in aquifer [mm]
  real                            :: WT      !water storage in aquifer + stuarated soil [mm]
  real                            :: RECH !recharge to or from the water table when shallow [m] (diagnostic)
  real, allocatable, dimension(:) :: etrani      !transpiration rate (mm/s) [+]
  real, allocatable, dimension(:) :: smc         !total soil water content [m3/m3]
  real, allocatable, dimension(:) :: wcnd        !hydraulic conductivity (m/s)
  integer                         :: iloc  = 1   !grid index
  integer                         :: jloc  = 1   !grid index

!---------------------------------------------------------------------
!  additional variables passed to canopy water
!---------------------------------------------------------------------
  real                           :: FCEV    !canopy evaporation (w/m2) [+ to atm ]
  real                           :: FCTR    !transpiration (w/m2) [+ to atm]
  real                           :: ELAI    !leaf area index, after burying by snow
  real                           :: ESAI    !stem area index, after burying by snow
  real                           :: TG      !ground temperature (k)
  real                           :: FVEG    !greeness vegetation fraction (-)        
  real                           :: BDFALL   !bulk density of snowfall (kg/m3) ! MB/AN: v3.7
  real                           :: CANLIQ  !intercepted liquid water (mm)
  real                           :: CANICE  !intercepted ice mass (mm)
  real                           :: TV      !vegetation temperature (k)
  real                           :: CMC     !intercepted water per ground area (mm)
  real                           :: ECAN    !evap of intercepted water (mm/s) [+]
  real                           :: ETRAN   !transpiration rate (mm/s) [+]
  REAL, allocatable, dimension(:):: BTRANI !Soil water transpiration factor (0 - 1) !!!!!!! Cenlin
  real                           :: FWET    !wetted/snowed fraction of canopy (-)
  real                           :: FVGMAX  !annual max greeness vegetation fraction (-) 
  real                           :: EAH     !canopy vapor pressure [Pa]
  real                           :: TAH      ! canopy temperature [K]       
  real                           :: LAI
  real                           :: SAI
  real                           :: FB_snow  ! canopy fraction buried by snow
  LOGICAL                        :: FROZEN_CANOPY ! used to define latent heat pathway
  real                           :: UU      !u-direction wind speed [m/s]
  real                           :: VV      !v-direction wind speed [m/s]
  integer                        :: IST     !surface type 1-soil; 2-lake
  real                           :: SNOW    !snowfall (mm/s)
  real                           :: RAIN    !ralfall mm/s 
  real                           :: FP      !fraction of the gridcell that receives precipitation
  real                           :: SFCTMP  !model-level temperature (k)
  REAL                           :: QINTR   !interception rate for rain (mm/s)
  REAL                           :: QDRIPR  !drip rate for rain (mm/s)
  REAL                           :: QTHROR  !throughfall for rain (mm/s)
  REAL                           :: QINTS   !interception (loading) rate for snowfall (mm/s)
  REAL                           :: QDRIPS  !drip (unloading) rate for intercepted snow (mm/s)
  REAL                           :: QTHROS  !throughfall of snowfall (mm/s)
  REAL                           :: QRAIN   !rain at ground srf (mm/s) [+]
  REAL                           :: QSNOW   !snow at ground srf (mm/s) [+]
  REAL                           :: SNOWHIN !snow depth increasing rate (m/s)

!---------------------------------------------------------------------
!  additional variables passed to snow water
!---------------------------------------------------------------------
  integer                        :: ISNOW   !actual no. of snow layers
  REAL                           :: SNOWH   !snow height [m]
  REAL                           :: SNEQV   !snow water eqv. [mm]
  REAL                           :: WSLAKE  !water storage in lake (can be -) (mm)
  REAL                           :: PONDING ![mm]
  REAL                           :: PONDING1 ![mm]
  REAL                           :: PONDING2 ![mm]
  REAL                           :: QSNBOT !melting water out of snow bottom [mm/s]
!  REAL                           :: LATHEAV !latent heat vap./sublimation (j/kg) 
!  REAL                           :: LATHEAG !latent heat vap./sublimation (j/kg)
  LOGICAL                        :: FROZEN_GROUND ! used to define latent heat pathway
  REAL                           :: QSNFRO  !snow surface frost rate[mm/s]
  REAL                           :: QSNSUB  !snow surface sublimation rate [mm/s]
  REAL                           :: SNOFLOW !glacier flow [mm/s]
  REAL                           :: QDEW    !ground surface dew rate [mm/s]
  REAL                           :: QSDEW   !soil surface dew rate [mm/s]
  real, allocatable, dimension(:) :: SNICE   !snow layer ice [mm]
  real, allocatable, dimension(:) :: SNLIQ   !snow layer liquid water [mm]
  real, allocatable, dimension(:) :: STC     !snow/soil layer temperature [k]
  real, allocatable, dimension(:) :: ZSNSO   !depth of snow/soil layer-bottom
  integer, allocatable, dimension(:) :: IMELT  !phase change index
  real, allocatable, dimension(:) :: FICEOLD !ice fraction at last timestep

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------

  integer :: itime, iz          ! some loop counters
  integer :: ntime      = 0     ! number of timesteps to run
  integer :: rain_steps = 0     ! number of timesteps in rain event
  integer :: dry_steps  = 0     ! number of timesteps between rain events
  integer :: rain_step  = 0     ! number of timesteps in current event
  integer :: dry_step   = 0     ! number of timesteps in current event
  real    :: dtheta_max = 0.0   ! maximum value of theta change in all levels
  real    :: totalwat   = 0.0   ! total soil water [mm]
  real    :: tw0        = 0.0   ! initial total soil water [mm]
  real    :: acsrf      = 0.0   ! accumulated surface runoff [mm]
  real    :: acsub      = 0.0   ! accumulated drainage [mm]
  real    :: acpcp      = 0.0   ! accumulated precipitation [mm]
  real    :: errwat     = 0.0   ! accumulated error [mm]
  logical :: done               ! logical check
  real, allocatable, dimension(:) :: smcold        !previous timestep smc
  real    :: WSLMAX = 5000.      !maximum lake water storage (mm)

!---------------------------------------------------------------------
!  parameters
!---------------------------------------------------------------------

  type (noahmp_parameters) :: parameters
  
!---------------------------------------------------------------------
!  end declarations
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!  read input file
!---------------------------------------------------------------------

  open(30, file="namelist.input", form="formatted")
   read(30, timing)
   read(30, forcing)
   read(30, structure)
   read(30, uniform_initial)
   read(30, soil_parameters)
   read(30, veg_modis_parameters)
  close(30)

!---------------------------------------------------------------------
!  allocate for dynamic levels
!---------------------------------------------------------------------

  allocate (zsoil (       1:nsoil))   !depth of layer-bottom from soil surface
  allocate (dzsnso(-nsnow+1:nsoil))   !snow/soil layer thickness [m]
  allocate (etrani(       1:nsoil))   !transpiration rate (mm/s) [+]
  allocate (btrani(       1:nsoil))   !!!!!!!! Cenlin
  allocate (sice  (       1:nsoil))   !soil ice content [m3/m3]
  allocate (sh2o  (       1:nsoil))   !soil liquid water content [m3/m3]
  allocate (smc   (       1:nsoil))   !total soil water content [m3/m3]
  allocate (wcnd  (       1:nsoil))   !hydraulic conductivity (m/s)
  allocate (smcold(       1:nsoil)) 

  allocate (SNICE (-nsnow+1:0    ))   !snow layer ice [mm]
  allocate (SNLIQ (-nsnow+1:0    ))   !snow layer liquid water [mm]
  allocate (STC   (-nsnow+1:nsoil))   !snow/soil layer temperature [k]
  allocate (zsnso (-nsnow+1:nsoil))   !depth of snow/soil layer-bottom
  allocate (IMELT (-nsnow+1:0    ))   !phase change index [1-melt; 2-freeze]
  allocate (FICEOLD(-nsnow+1:0   ))   !ice fraction at last timestep

  allocate (parameters%bexp  (nsoil))
  allocate (parameters%smcmax(nsoil))
  allocate (parameters%smcwlt(nsoil))
  allocate (parameters%smcref(nsoil))
  allocate (parameters%dksat (nsoil))
  allocate (parameters%dwsat (nsoil))
  allocate (parameters%psisat(nsoil))

!---------------------------------------------------------------------
!  transfer parameters
!---------------------------------------------------------------------

  parameters%smcmax = maxsmc(isltyp)
  parameters%bexp   = bb(isltyp)
  parameters%dksat  = satdk(isltyp)
  parameters%dwsat  = satdw(isltyp)
  parameters%smcwlt = wltsmc(isltyp)
  parameters%smcref = refsmc(isltyp)
  parameters%psisat = satpsi(isltyp)
  parameters%bvic   = bvic(isltyp)  !addrunoff
  parameters%AXAJ   = AXAJ(isltyp)  !addrunoff
  parameters%BXAJ   = BXAJ(isltyp)  !addrunoff
  parameters%XXAJ   = XXAJ(isltyp)  !addrunoff
  parameters%G      = G(isltyp)  !addrunoff
  parameters%BBVIC  = BBVIC(isltyp)  !addrunoff

  parameters%kdt    = 3.0 * parameters%dksat(1) / 2.0e-6
  parameters%frzx   = 0.15 * (parameters%smcmax(1) / parameters%smcref(1)) * (0.412 / 0.468)
  
  parameters%slope  = slope
  parameters%urban_flag = .false.
 
! for canopy water
  parameters%CH2OP  = CH2OP(vegtyp)  ! maximum intercepted h2o per unit lai+sai (mm)
  parameters%SAIM   = SAI_APR(vegtyp) !monthly stem area index, one-sided
  parameters%LAIM   = LAI_APR(vegtyp) !monthly leaf area index, one-sided

 
!---------------------------------------------------------------------
!  read input file, part 2: initialize
!---------------------------------------------------------------------

  if(structure_option == 1) then       ! user-defined levels
    open(30, file="namelist.input", form="formatted")
     read(30, fixed_initial)
    close(30)
  else if(structure_option == 2) then  ! fixed levels
    dzsnso = soil_depth / nsoil
    do iz = 1, nsoil
      zsoil(iz) = -1. * sum(dzsnso(1:iz))
    end do
    if(.not.initial_uniform) &
      stop "structure_option > 1 must have initial_uniform == .true."
  end if

! add for snow water
  zsnso(-2:0) = 0.0
  zsnso(1:4) = zsoil(1:4)

  if(initial_uniform) then
    sh2o = initial_sh2o_value
    sice = initial_sice_value
  end if

!---------------------------------------------------------------------
! initialize any other values
!---------------------------------------------------------------------

  smc       = sh2o + sice  ! initial volumetric soil water

  zwt       = -100.0       ! should only be needed for run=1
  smcwtd    = 0.0          ! should only be needed for run=5
  deeprech  = 0.0          ! should only be needed for run=5
  qinsur    = 0.0          ! 
  runsrf    = 0.0          ! 
  runsub    = 0.0          ! 
  qdrain    = 0.0          ! 
  wcnd      = 0.0          ! 
  fcrmax    = 0.0          ! 

  rain = 0.0
  qintr = 0.0
  qints = 0.0
  qdripr = 0.0
  qdrips = 0.0
  qthror = 0.0
  qthros = 0.0
  qrain = 0.0
  qsnow = 0.0
  snowhin = 0.0
  fwet = 0.0
  cmc = 0.0
  canliq = 0.0
  canice = 0.0
  ecan = 0.0
  etran = 0.0

  qseva           = evaprate/3600.0 ! soil evaporation [mm/s]
  btrani(1:nsoil) = 0.0
  totalwat = sum(dzsnso(1:nsoil)*smc*1000.0) ! [mm]
  tw0 = totalwat

  ntime      =  nint(maxtime * 3600.0 / dt)
  rain_steps = rain_duration * 3600.0 / dt
  dry_steps  =  dry_duration * 3600.0 / dt

!!!!!!!!!!!!!!!!!! for canopy water
   LAI = LAI_APR(vegtyp)
   SAI = SAI_APR(vegtyp)
   FB_snow = 0.0
!  FVEG   = VEGFRA(I,J)/100.       ! vegetation fraction [0-1]
  FVEG = SHDMAX/100.        !yearly max vegetation fraction
  IF(FVEG <= 0.05) FVEG = 0.05

  FVGMAX = VEGMAX/100.      ! Vegetation fraction annual max [0-1]
  TV = 298   ! leaf temperature [K]
  TG = 298   ! ground temperature [K]
  CANLIQ = 0.0  ! canopy liquid water [mm]
  CANICE = 0.0  ! canopy frozen water [mm]
  EAH = 400 ! canopy vapor pressure [Pa]
  TAH = 298 ! canopy temperature [K]
  FWET = 0.0  ! canopy fraction wet or snow
  FCEV = fcev_e  !canopy evaporation (w/m2) [+ to atm ]
  FCTR = fctr_e  !transpiration (w/m2) [+ to atm]
  ELAI = LAI * (1. - FB_snow) !leaf area index, after burying by snow
  ESAI = SAI * (1. - FB_snow) !!stem area index, after burying by snow 
  BDFALL = 120.0       !bulk density of snowfall (kg/m3)
  IF (TV .GT. 273.15) THEN 
     frozen_canopy = .false.
  ELSE
     frozen_canopy = .true.
  END IF
! intercepted water
  UU = 3.0 ! wind speed
  VV = 3.0 
  IST = 1 !surface type 1-soil; 2-lake
  SNOW = 0.0  !snowfall (mm/s)
  FP = 1.0 !fraction of the gridcell that receives precipitation
  SFCTMP = 298.0 !model-level temperature (k)

!!!!!!!!!!!!!!!!!! for snow water
  IF (TG .GT. 273.15) THEN
     frozen_ground = .false.
  ELSE
     frozen_ground = .true.
  END IF
  ISNOW = 0
  SNOWH = 0.0
  SNEQV = 0.0
  WSLAKE = 0.0
  PONDING = 0.0
  PONDING1 = 0.0
  PONDING2 = 0.0
  QSNBOT = 0.0
  QSNFRO = 0.0
  QSNSUB = 0.0
  QDEW = 0.0
  QSDEW = 0.0
  SNICE = 0.0
  SNLIQ = 0.0
  STC = 298.0
  IMELT = 1 ! freeze
  FICEOLD = 0.0

!---------------------------------------------------------------------
! create output file and add initial values
!---------------------------------------------------------------------

  call initialize_output(output_filename, ntime+1, nsoil, nsnow)
  call add_to_output(0,nsoil,dzsnso,dt,qinsur,runsrf,runsub,qseva,etrani,smc,rain,&
                  qintr,qints,qdripr,qdrips,qthror,qthros,qrain,qsnow,snowhin,fwet,&
                  cmc,canliq,canice,ecan,etran,nsnow,snowh,sneqv,ponding,ponding1,ponding2,&
                  QSNBOT,QSNFRO,QSNSUB,SNICE,SNLIQ,STC,zsnso)

!---------------------------------------------------------------------
! start the time loop
!---------------------------------------------------------------------

  do itime = 1, ntime
   
  !---------------------------------------------------------------------
  ! calculate the input water
  !---------------------------------------------------------------------

    if(raining) then
      RAIN = rainrate/3600.0 ! input water mm/s
      rain_step = rain_step + 1
      if(rain_step == rain_steps) then      ! event length met
        rain_step = 0
        raining   = .false.
      end if
    else
      RAIN   = 0.0                        ! stop water input [m/s]
      dry_step = dry_step + 1
      if(dry_step == dry_steps) then        ! between event length met
        dry_step = 0
        raining  = .true.
      end if
    end if

    !SNOW = RAIN * 0.9
    !RAIN = RAIN * 0.1

!!!============================================= Start the original Water Subroutine ==========================================
! initialize

   ETRANI(1:NSOIL) = 0.
   SNOFLOW         = 0.
   RUNSUB          = 0.
   QINSUR          = 0.


  !---------------------------------------------------------------------
  ! call the canopy water routines
  !---------------------------------------------------------------------
   call canwater_intercept (parameters,ILOC ,JLOC ,VEGTYP ,DT ,UU ,VV    , & !in
                          ELAI   ,ESAI   ,FVEG   ,IST    ,                 & !in
                          BDFALL ,RAIN   ,SNOW   ,FP     ,                 & !in
                          CANLIQ ,CANICE ,TV     ,SFCTMP ,TG     ,         & !in
                          QINTR  ,QDRIPR ,QTHROR ,QINTS  ,QDRIPS ,QTHROS , & !out
                          QRAIN  ,QSNOW  ,SNOWHIN, FWET   ,CMC  )   !out

! compute canopu water evaporation and transpiration
   CALL CANWATER (parameters,VEGTYP ,DT     , & !in
                  FCEV   ,FCTR   ,ELAI   , & !in
                  ESAI   ,TG     ,FVEG   ,ILOC   , JLOC, & !in
                  BDFALL ,FROZEN_CANOPY  , & !in     
                  CANLIQ ,CANICE ,TV     ,                 & !inout
                  CMC    ,ECAN   ,ETRAN  , & !out
                  FWET      )                           !out


  !---------------------------------------------------------------------
  ! call the snow water routines
  !---------------------------------------------------------------------
! sublimation, frost, evaporation, and dew
     QSNSUB = 0.0
     IF (SNEQV > 0.) THEN
       QSNSUB = MIN(QVAP, SNEQV/DT)
     ENDIF
     QSEVA = QVAP-QSNSUB
     QSNFRO = 0.0
     IF (SNEQV > 0.) THEN
        QSNFRO = QDEW
     ENDIF
     QSDEW = QDEW - QSNFRO
     CALL SNOWWATER (parameters,NSNOW  ,NSOIL  ,IMELT  ,DT     ,ZSOIL  , & !in
          &          SFCTMP ,SNOWHIN,QSNOW  ,QSNFRO ,QSNSUB , & !in
          &          QRAIN  ,FICEOLD,ILOC   ,JLOC   ,         & !in
          &          ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  , & !inout
          &          SH2O   ,SICE   ,STC    ,ZSNSO  ,DZSNSO , & !inout
          &          QSNBOT ,SNOFLOW,PONDING1       ,PONDING2)  !out
   IF(FROZEN_GROUND) THEN
      SICE(1) =  SICE(1) + (QSDEW-QSEVA)*DT/(DZSNSO(1)*1000.)
      QSDEW = 0.0
      QSEVA = 0.0
      IF(SICE(1) < 0.) THEN
         SH2O(1) = SH2O(1) + SICE(1)
         SICE(1) = 0.
      END IF
   END IF
! convert units (mm/s -> m/s)
    !PONDING: melting water from snow when there is no layer
    QINSUR = (PONDING+PONDING1+PONDING2)/DT * 0.001
!    QINSUR = PONDING/DT * 0.001
    IF(ISNOW == 0) THEN
       QINSUR = QINSUR+(QSNBOT + QSDEW + QRAIN) * 0.001
    ELSE
       QINSUR = QINSUR+(QSNBOT + QSDEW) * 0.001
    ENDIF
    QSEVA  = QSEVA * 0.001 


!!!!!!!! change place Cenlin
    runsub = 0.0
    runsrf = 0.0
    smcold = smc

!!!!!!!!!!!! cenlin: added to complete the default code
   DO IZ = 1, parameters%NROOT
       ETRANI(IZ) = ETRAN * BTRANI(IZ) * 0.001
    ENDDO

! #ifdef WRF_HYDRO
!       QINSUR = QINSUR+sfcheadrt/DT*0.001  !sfcheadrt units (m)
! #endif

! lake/soil water balances
    IF (IST == 2) THEN                                        ! lake
       RUNSRF = 0.
       IF(WSLAKE >= WSLMAX) RUNSRF = QINSUR*1000.             !mm/s
       WSLAKE = WSLAKE + (QINSUR-QSEVA)*1000.*DT -RUNSRF*DT   !mm
    ELSE                                                      ! soil
!!!!!!!!!!!!!!

  !---------------------------------------------------------------------
  ! call the soil water routines
  !---------------------------------------------------------------------
 
   !qinsur   = RAIN / 1000.0 ! input water m/s
   ! qinsur   = ( QRAIN + QSNOW ) / 1000.0 ! m/s

!    runsub = 0.0
!    runsrf = 0.0
!    smcold = smc

    call soilwater (parameters,nsoil  ,nsnow  ,dt     ,zsoil  ,dzsnso , & !in
                       qinsur ,qseva  ,etrani ,sice   ,iloc   , jloc ,  & !in
                       sh2o   ,smc    ,zwt    ,vegtyp ,                 & !inout
                       smcwtd ,deeprech                       ,         & !inout
                       runsrf ,qdrain ,runsub ,wcnd   ,fcrmax )           !out

!!!!!! did not include groundwater part
!!!!!!! Cenlin
    ENDIF

    do iz = 1, nsoil
      smc(iz) = sh2o(iz) + sice(iz)
    end do
   
  !---------------------------------------------------------------------
  ! accumulate some fields and error checks
  !---------------------------------------------------------------------

    ! here needs more thoughts, originally only for OPT_RUN=3,4,5 !addrunoff also for 6,7,8
    runsub = qdrain + runsub              ! drainage [mm/s]
    acsrf  = acsrf + runsrf * dt          ! accumulated surface runoff [mm]
    acsub  = acsub + runsub * dt          ! accumulated drainage [mm]
    acpcp  = acpcp + qinsur * dt * 1000.0 ! accumulated precipitation [mm]
   
    dtheta_max = maxval(abs(smc-smcold))
!    if (dtheta_max .lt. 0.00001) done = .true.
   
    totalwat = sum(dzsnso(1:nsoil)*smc*1000.0)         ! total soil water [mm]
    errwat = acpcp - acsrf - acsub - (totalwat - tw0)  ! accum error [mm]
   
  !---------------------------------------------------------------------
  ! add to output file
  !---------------------------------------------------------------------

  call add_to_output(itime,nsoil,dzsnso,dt,qinsur,runsrf,runsub,qseva,etrani,smc,rain,&
                  qintr,qints,qdripr,qdrips,qthror,qthros,qrain,qsnow,snowhin,fwet,&
                  cmc,canliq,canice,ecan,etran,nsnow,snowh,sneqv,ponding,ponding1,ponding2,&
                  QSNBOT,QSNFRO,QSNSUB,SNICE,SNLIQ,STC,zsnso)
 
  end do ! time loop

  call finalize_output()
   
end program
