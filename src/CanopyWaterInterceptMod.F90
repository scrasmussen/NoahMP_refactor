module CanopyWaterInterceptMod

!!! Canopy water processes for snow and rain interception
!!! Subsequent hydrological process for intercepted water is done in CanopyHydrology

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine CanopyWaterIntercept(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: PRECIP_HEAT
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! The water and heat portions of PRECIP_HEAT are separated in refactored code
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: FT         ! temperature factor for unloading rate
    real(kind=kind_noahmp)           :: FV         ! wind factor for unloading rate
    real(kind=kind_noahmp)           :: ICEDRIP    ! canice unloading 

! --------------------------------------------------------------------
    associate(                                                        &
              IST             => noahmp%config%domain%IST            ,& ! in,    surface type 1-soil; 2-lake
              DT              => noahmp%config%domain%DT             ,& ! in,    noahmp time step (s)
              UU              => noahmp%forcing%UU                   ,& ! in,    u direction wind
              VV              => noahmp%forcing%VV                   ,& ! in,    v direction wind
              ELAI            => noahmp%energy%state%ELAI            ,& ! in,    leaf area index, after burying by snow
              ESAI            => noahmp%energy%state%ESAI            ,& ! in,    stem area index, after burying by snow
              FVEG            => noahmp%energy%state%FVEG            ,& ! in,    greeness vegetation fraction (-)
              TV              => noahmp%energy%state%TV              ,& ! in,    vegetation temperature (k)
              TG              => noahmp%energy%state%TG              ,& ! in,    ground temperature (k)
              CH2OP           => noahmp%water%param%CH2OP            ,& ! in,    maximum intercepted water per unit lai+sai (mm)
              RAIN            => noahmp%water%flux%RAIN              ,& ! in,    total liquid rainfall (mm/s) before interception
              SNOW            => noahmp%water%flux%SNOW              ,& ! in,    total liquid snowfall (mm/s) before interception
              BDFALL          => noahmp%water%state%BDFALL           ,& ! in,    bulk density of snowfall (kg/m3)
              FP              => noahmp%water%state%FP               ,& ! in,    fraction of the gridcell that receives precipitation
              CANLIQ          => noahmp%water%state%CANLIQ           ,& ! inout, intercepted liquid water (mm)
              CANICE          => noahmp%water%state%CANICE           ,& ! inout, intercepted ice mass (mm)
              FWET            => noahmp%water%state%FWET             ,& ! out,   wetted or snowed fraction of the canopy
              CMC             => noahmp%water%state%CMC              ,& ! out,   total canopy intercepted water (mm)
              MAXSNO          => noahmp%water%state%MAXSNO           ,& ! out,   canopy capacity for snow interception (mm)
              MAXLIQ          => noahmp%water%state%MAXLIQ           ,& ! out,   canopy capacity for rain interception (mm)
              QINTR           => noahmp%water%flux%QINTR             ,& ! out,   interception rate for rain (mm/s)
              QDRIPR          => noahmp%water%flux%QDRIPR            ,& ! out,   drip rate for rain (mm/s)
              QTHROR          => noahmp%water%flux%QTHROR            ,& ! out,   throughfall for rain (mm/s)
              QINTS           => noahmp%water%flux%QINTS             ,& ! out,   interception (loading) rate for snowfall (mm/s)
              QDRIPS          => noahmp%water%flux%QDRIPS            ,& ! out,   drip (unloading) rate for intercepted snow (mm/s)
              QTHROS          => noahmp%water%flux%QTHROS            ,& ! out,   throughfall of snowfall (mm/s)
              QRAIN           => noahmp%water%flux%QRAIN             ,& ! out,   rainfall at ground surface (mm/s)
              QSNOW           => noahmp%water%flux%QSNOW             ,& ! out,   snowfall at ground surface (mm/s)
              SNOWHIN         => noahmp%water%flux%SNOWHIN            & ! out,   snow depth increasing rate (m/s)
             )
! ----------------------------------------------------------------------

    ! initialization
    QINTR   = 0.0
    QDRIPR  = 0.0
    QTHROR  = 0.0
    QINTS   = 0.0
    QDRIPS  = 0.0
    QTHROS  = 0.0
    QRAIN   = 0.0
    QSNOW   = 0.0
    SNOWHIN = 0.0
    ICEDRIP = 0.0
    FT      = 0.0
    FV      = 0.0

    ! ----------------------- canopy liquid water ------------------------------
    ! maximum canopy water
    MAXLIQ =  CH2OP * (ELAI + ESAI)

    ! average rain interception and throughfall
    if ( (ELAI+ESAI) > 0.0 ) then
       QINTR  = FVEG * RAIN * FP  ! interception capability
       QINTR  = min( QINTR, (MAXLIQ-CANLIQ)/DT * (1.0-exp(-RAIN*DT/MAXLIQ)) )
       QINTR  = max( QINTR, 0.0 )
       QDRIPR = FVEG * RAIN - QINTR
       QTHROR = (1.0 - FVEG) * RAIN
       CANLIQ = max( 0.0, CANLIQ + QINTR*DT )
    else
       QINTR  = 0.0
       QDRIPR = 0.0
       QTHROR = RAIN
       if ( CANLIQ > 0.0 ) then   ! FOR CASE OF CANOPY GETTING BURIED
          QDRIPR = QDRIPR + CANLIQ / DT
          CANLIQ = 0.0
       endif
    endif

    ! ----------------------- canopy ice ------------------------------
    ! maximum canopy ice
    MAXSNO = 6.6 * (0.27 + 46.0/BDFALL) * (ELAI + ESAI)

    ! average snow interception and throughfall
    if ( (ELAI+ESAI) > 0.0 ) then
       QINTS = FVEG * SNOW * FP
       QINTS = min( QINTS, (MAXSNO-CANICE)/DT * (1.0-exp(-SNOW*DT/MAXSNO)) )
       QINTS = max( QINTS, 0.0 )
       FT    = max( 0.0, (TV - 270.15) / 1.87e5 )
       FV    = sqrt(UU*UU + VV*VV) / 1.56e5
       ! MB: changed below to reflect the rain assumption that all precip gets intercepted 
       ICEDRIP = max( 0.0, CANICE ) * (FV + FT)    !MB: removed /DT
       QDRIPS  = (FVEG * SNOW - QINTS) + ICEDRIP
       QTHROS  = (1.0 - FVEG) * SNOW
       CANICE  = max( 0.0, CANICE + (QINTS-ICEDRIP)*DT )
    else
       QINTS  = 0.0
       QDRIPS = 0.0
       QTHROS = SNOW
       if ( CANICE > 0.0 ) then   ! FOR CASE OF CANOPY GETTING BURIED
          QDRIPS = QDRIPS + CANICE / DT
          CANICE = 0.0
       endif
    endif

    ! wetted fraction of canopy
    if ( CANICE > 0.0 ) then
       FWET = max( 0.0, CANICE ) / max( MAXSNO, 1.0e-06 )
    else
       FWET = max( 0.0, CANLIQ ) / max( MAXLIQ, 1.0e-06 )
    endif
    FWET    = min( FWET, 1.0 ) ** 0.667

    ! total canopy water
    CMC = CANLIQ + CANICE

    ! rain or snow on the ground
    QRAIN   = QDRIPR + QTHROR
    QSNOW   = QDRIPS + QTHROS
    SNOWHIN = QSNOW / BDFALL
    if ( IST == 2 .and. TG > TFRZ ) then
       QSNOW   = 0.0
       SNOWHIN = 0.0
    endif

    end associate

  end subroutine CanopyWaterIntercept

end module CanopyWaterInterceptMod
