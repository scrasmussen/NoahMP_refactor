module CanopyHydrologyMod

!!! Canopy Hydrology processes for intercepted rain and snow water
!!! Canopy liquid water evaporation and dew; canopy ice water sublimation and frost
  
  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine CanopyHydrology(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CANWATER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                        &
              DT              => noahmp%config%domain%DT             ,& ! in,    noahmp time step (s)
              FCEV            => noahmp%energy%flux%FCEV             ,& ! in,    canopy evaporation (w/m2) [+ = to atm]
              FCTR            => noahmp%energy%flux%FCTR             ,& ! in,    transpiration (w/m2) [+ = to atm]
              ELAI            => noahmp%energy%state%ELAI            ,& ! in,    leaf area index, after burying by snow
              ESAI            => noahmp%energy%state%ESAI            ,& ! in,    stem area index, after burying by snow
              TG              => noahmp%energy%state%TG              ,& ! in,    ground temperature (k)
              FVEG            => noahmp%energy%state%FVEG            ,& ! in,    greeness vegetation fraction (-)
              FROZEN_CANOPY   => noahmp%energy%state%FROZEN_CANOPY   ,& ! in,    used to define latent heat pathway
              BDFALL          => noahmp%water%state%BDFALL           ,& ! in,    bulk density of snowfall (kg/m3)
              CH2OP           => noahmp%water%param%CH2OP            ,& ! in,    maximum intercepted water per unit lai+sai (mm)
              CANLIQ          => noahmp%water%state%CANLIQ           ,& ! inout, intercepted liquid water (mm)
              CANICE          => noahmp%water%state%CANICE           ,& ! inout, intercepted ice mass (mm)
              TV              => noahmp%energy%state%TV              ,& ! inout, vegetation temperature (k)
              CMC             => noahmp%water%state%CMC              ,& ! out,   total canopy intercepted water (mm)
              FWET            => noahmp%water%state%FWET             ,& ! out,   wetted or snowed fraction of the canopy
              MAXSNO          => noahmp%water%state%MAXSNO           ,& ! out,   canopy capacity for snow interception (mm)
              MAXLIQ          => noahmp%water%state%MAXLIQ           ,& ! out,   canopy capacity for rain interception (mm)
              ECAN            => noahmp%water%flux%ECAN              ,& ! out,   evaporation of intercepted water (mm/s) [+]
              ETRAN           => noahmp%water%flux%ETRAN             ,& ! out,   transpiration rate (mm/s) [+]
              QEVAC           => noahmp%water%flux%QEVAC             ,& ! out,   canopy water evaporation rate (mm/s)
              QDEWC           => noahmp%water%flux%QDEWC             ,& ! out,   canopy water dew rate (mm/s)
              QFROC           => noahmp%water%flux%QFROC             ,& ! out,   canopy ice frost rate (mm/s)
              QSUBC           => noahmp%water%flux%QSUBC             ,& ! out,   canopy ice sublimation rate (mm/s)
              QMELTC          => noahmp%water%flux%QMELTC            ,& ! out,   canopy ice melting rate (mm/s)
              QFRZC           => noahmp%water%flux%QFRZC              & ! out,   canopy water refreezing rate (mm/s)
             )
! --------------------------------------------------------------------

! initialization for out-only variables
    ECAN    = 0.0
    ETRAN   = 0.0
    QEVAC   = 0.0
    QDEWC   = 0.0
    QFROC   = 0.0
    QSUBC   = 0.0
    QMELTC  = 0.0
    QFRZC   = 0.0
    MAXLIQ  = 0.0
    MAXSNO  = 0.0
    FWET    = 0.0
    CMC     = 0.0

!=== canopy liquid water
    ! maximum canopy intercepted water
    MAXLIQ =  CH2OP * (ELAI + ESAI)

    ! canopy evaporation, transpiration, and dew
    if ( FROZEN_CANOPY .eqv. .false. ) then    ! Barlage: change to frozen_canopy
       ETRAN = max( FCTR/ConstLatHeatVapor, 0.0 )
       QEVAC = max( FCEV/ConstLatHeatVapor, 0.0 )
       QDEWC = abs( min( FCEV/ConstLatHeatVapor, 0.0 ) )
       QSUBC = 0.0
       QFROC = 0.0
    else
       ETRAN = max( FCTR/ConstLatHeatSublim, 0.0 )
       QEVAC = 0.0
       QDEWC = 0.0
       QSUBC = max( FCEV/ConstLatHeatSublim, 0.0 )
       QFROC = abs( min( FCEV/ConstLatHeatSublim, 0.0 ) )
    endif

    ! canopy water balance. for convenience allow dew to bring CANLIQ above
    ! maxh2o or else would have to re-adjust drip
    QEVAC   = min( CANLIQ/DT, QEVAC )
    CANLIQ  = max( 0.0, CANLIQ+(QDEWC-QEVAC)*DT )
    if ( CANLIQ <= 1.0e-06 ) CANLIQ = 0.0

!=== canopy ice 
    ! maximum canopy intercepted ice
    MAXSNO = 6.6 * (0.27 + 46.0/BDFALL) * (ELAI + ESAI)

    ! canopy sublimation and frost
    QSUBC = min( CANICE/DT, QSUBC )
    CANICE= max( 0.0, CANICE+(QFROC-QSUBC)*DT )
    if ( CANICE <= 1.0e-6 ) CANICE = 0.0

!=== wetted fraction of canopy
    if ( CANICE > 0.0 ) then
       FWET = max(0.0,CANICE) / max(MAXSNO,1.0e-06)
    else
       FWET = max(0.0,CANLIQ) / max(MAXLIQ,1.0e-06)
    endif
    FWET = min(FWET, 1.0) ** 0.667

!=== phase change
    ! canopy ice melting
    if ( (CANICE > 1.0e-6) .and. (TV > ConstFreezePoint) ) then
       QMELTC = min( CANICE/DT, (TV-ConstFreezePoint)*ConstHeatCapacIce*CANICE/ConstDensityIce/(DT*ConstLatHeatFusion) )
       CANICE = max( 0.0, CANICE - QMELTC*DT )
       CANLIQ = max( 0.0, CANLIQ + QMELTC*DT )
       TV     = FWET*ConstFreezePoint + (1.0 - FWET)*TV
    endif

    ! canopy water refreeezing
    if ( (CANLIQ > 1.0e-6) .and. (TV < ConstFreezePoint) ) then
       QFRZC  = min( CANLIQ/DT, (ConstFreezePoint-TV)*ConstHeatCapacWater*CANLIQ/ConstDensityWater/(DT*ConstLatHeatFusion) )
       CANLIQ = max( 0.0, CANLIQ - QFRZC*DT )
       CANICE = max( 0.0, CANICE + QFRZC*DT )
       TV     = FWET*ConstFreezePoint + (1.0 - FWET)*TV
    ENDIF

!=== update total canopy water
    CMC = CANLIQ + CANICE

!=== total canopy evaporation
    ECAN = QEVAC + QSUBC - QDEWC - QFROC

    end associate

  end subroutine CanopyHydrology

end module CanopyHydrologyMod
