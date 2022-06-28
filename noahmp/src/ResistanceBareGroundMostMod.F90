module ResistanceBareGroundMostMod

!!! Compute bare ground resistance and drag coefficient for momentum and heat
!!! based on Monin-Obukhov (M-O) Similarity Theory (MOST)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine ResistanceBareGroundMOST(noahmp, ITER, H, MOZSGN)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SFCDIF1 for bare ground portion
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

! in & out variables
    integer               , intent(in   ) :: ITER         ! iteration index
    integer               , intent(inout) :: MOZSGN       ! number of times moz changes sign
    real(kind=kind_noahmp), intent(in   ) :: H            ! temporary sensible heat flux (w/m2) in each iteration
    type(noahmp_type)     , intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)                :: MPE                         ! prevents overflow for division by zero
    real(kind=kind_noahmp)                :: TMPCM                       ! temporary calculation for CM
    real(kind=kind_noahmp)                :: TMPCH                       ! temporary calculation for CH
    real(kind=kind_noahmp)                :: FMNEW                       ! stability correction factor, momentum, for current moz
    real(kind=kind_noahmp)                :: FHNEW                       ! stability correction factor, sen heat, for current moz
    real(kind=kind_noahmp)                :: MOZOLD                      ! Monin-Obukhov stability parameter from prior iteration
    real(kind=kind_noahmp)                :: TMP1,TMP2,TMP3,TMP4,TMP5    ! temporary calculation
    real(kind=kind_noahmp)                :: TVIR                        ! temporary virtual temperature (k)
    real(kind=kind_noahmp)                :: TMPCM2                      ! temporary calculation for CM2
    real(kind=kind_noahmp)                :: TMPCH2                      ! temporary calculation for CH2
    real(kind=kind_noahmp)                :: FM2NEW                      ! stability correction factor, momentum, for current moz
    real(kind=kind_noahmp)                :: FH2NEW                      ! stability correction factor, sen heat, for current moz
    real(kind=kind_noahmp)                :: TMP12,TMP22,TMP32           ! temporary calculation
    real(kind=kind_noahmp)                :: CMFM, CHFH, CM2FM2, CH2FH2  ! temporary calculation

! --------------------------------------------------------------------
    associate(                                                        &
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight,& ! in,    air temperature [K] at reference height
              ZLVL            => noahmp%energy%state%ZLVL            ,& ! in,    reference height  (m)
              RHOAIR          => noahmp%energy%state%RHOAIR          ,& ! in,    density air (kg/m3)
              SpecHumidityRefHeight => noahmp%forcing%SpecHumidityRefHeight ,& ! in,    specific humidity (kg/kg) at reference height
              UR              => noahmp%energy%state%UR              ,& ! in,    wind speed (m/s) at reference height ZLVL
              ZPD             => noahmp%energy%state%ZPDG            ,& ! in,    ground zero plane displacement (m)
              Z0H             => noahmp%energy%state%Z0HB            ,& ! in,    roughness length, sensible heat (m), bare ground
              Z0M             => noahmp%energy%state%Z0MG            ,& ! in,    roughness length, momentum, (m), ground
              FM              => noahmp%energy%state%FMB             ,& ! inout, M-O momentum stability correction, above ZPDG, bare ground
              FH              => noahmp%energy%state%FHB             ,& ! inout, M-O sen heat stability correction, above ZPDG, bare ground
              FM2             => noahmp%energy%state%FM2B            ,& ! inout, M-O momentum stability correction, 2m, bare ground
              FH2             => noahmp%energy%state%FH2B            ,& ! inout, M-O sen heat stability correction, 2m, bare ground
              FV              => noahmp%energy%state%FVB             ,& ! inout, friction velocity (m/s), bare ground
              MOZ             => noahmp%energy%state%MOZB            ,& ! inout, Monin-Obukhov stability (z/L), above ZPD, bare ground
              MOZ2            => noahmp%energy%state%MOZ2B           ,& ! out,   Monin-Obukhov stability (2/L), 2m, bare ground
              MOL             => noahmp%energy%state%MOLB            ,& ! out,   Monin-Obukhov length (m), above ZPD, bare ground
              CM              => noahmp%energy%state%CMB             ,& ! out,   drag coefficient for momentum, above ZPD, bare ground
              CH              => noahmp%energy%state%CHB             ,& ! out,   drag coefficient for heat, above ZPD, bare ground
              CH2             => noahmp%energy%state%CH2B            ,& ! out,   drag coefficient for heat, 2m, bare ground
              RAMB            => noahmp%energy%state%RAMB            ,& ! out,   aerodynamic resistance for momentum (s/m), bare ground
              RAHB            => noahmp%energy%state%RAHB            ,& ! out,   aerodynamic resistance for sensible heat (s/m), bare ground
              RAWB            => noahmp%energy%state%RAWB             & ! out,   aerodynamic resistance for water vapor (s/m), bare ground
             )
! ----------------------------------------------------------------------

    ! initialization
    MPE = 1.0e-6
    MOZOLD = MOZ  ! M-O stability parameter for next iteration
    if ( ZLVL <= ZPD ) then
       write(*,*) 'WARNING: critical problem: ZLVL <= ZPD; model stops'
       stop 'error'
       !call wrf_error_fatal("STOP in Noah-MP")
    endif

    ! temporary drag coefficients
    TMPCM  = log( (ZLVL - ZPD) / Z0M )
    TMPCH  = log( (ZLVL - ZPD) / Z0H )
    TMPCM2 = log( (2.0 + Z0M) / Z0M )
    TMPCH2 = log( (2.0 + Z0H) / Z0H )

    ! compute M-O stability parameter
    if ( ITER == 1 ) then
       FV   = 0.0
       MOZ  = 0.0
       MOL  = 0.0
       MOZ2 = 0.0
    else
       TVIR = (1.0 + 0.61 * SpecHumidityRefHeight) * TemperatureAirRefHeight
       TMP1 = ConstVonKarman * (ConstGravityAcc / TVIR) * H / (RHOAIR * ConstHeatCapacAir)
       if ( abs(TMP1) <= MPE ) TMP1 = MPE
       MOL  = -1.0 * FV**3 / TMP1
       MOZ  = min( (ZLVL - ZPD) / MOL, 1.0 )
       MOZ2 = min( (2.0 + Z0H) / MOL, 1.0 )
    endif

    ! accumulate number of times moz changes sign.
    if ( MOZOLD*MOZ < 0.0 ) MOZSGN = MOZSGN + 1
    if ( MOZSGN >= 2 ) then
       MOZ  = 0.0
       FM   = 0.0
       FH   = 0.0
       MOZ2 = 0.0
       FM2  = 0.0
       FH2  = 0.0
    endif

    ! evaluate stability-dependent variables using moz from prior iteration
    if ( MOZ < 0.0 ) then
       TMP1   = (1.0 - 16.0 * MOZ)**0.25
       TMP2   = log( (1.0 + TMP1*TMP1) / 2.0 )
       TMP3   = log( (1.0 + TMP1) / 2.0 )
       FMNEW  = 2.0 * TMP3 + TMP2 - 2.0 * atan(TMP1) + 1.5707963
       FHNEW  = 2 * TMP2
       ! 2-meter quantities
       TMP12  = (1.0 - 16.0 * MOZ2)**0.25
       TMP22  = log( (1.0 + TMP12*TMP12) / 2.0 )
       TMP32  = log( (1.0 + TMP12) / 2.0 )
       FM2NEW = 2.0 * TMP32 + TMP22 - 2.0 * atan(TMP12) + 1.5707963
       FH2NEW = 2 * TMP22
    else
       FMNEW  = -5.0 * MOZ
       FHNEW  = FMNEW
       FM2NEW = -5.0 * MOZ2
       FH2NEW = FM2NEW
    endif

    ! except for first iteration, weight stability factors for previous
    ! iteration to help avoid flip-flops from one iteration to the next
    if ( ITER == 1 ) then
       FM  = FMNEW
       FH  = FHNEW
       FM2 = FM2NEW
       FH2 = FH2NEW
    else
       FM  = 0.5 * (FM + FMNEW)
       FH  = 0.5 * (FH + FHNEW)
       FM2 = 0.5 * (FM2 + FM2NEW)
       FH2 = 0.5 * (FH2 + FH2NEW)
    endif

    ! exchange coefficients
    FH     = min( FH, 0.9*TMPCH )
    FM     = min( FM, 0.9*TMPCM )
    FH2    = min( FH2, 0.9*TMPCH2 )
    FM2    = min( FM2, 0.9*TMPCM2 )
    CMFM   = TMPCM - FM
    CHFH   = TMPCH - FH
    CM2FM2 = TMPCM2 - FM2
    CH2FH2 = TMPCH2 - FH2
    if ( abs(CMFM) <= MPE ) CMFM = MPE
    if ( abs(CHFH) <= MPE ) CHFH = MPE
    if ( abs(CM2FM2) <= MPE ) CM2FM2 = MPE
    if ( abs(CH2FH2) <= MPE ) CH2FH2 = MPE
    CM  = ConstVonKarman * ConstVonKarman / (CMFM * CMFM)
    CH  = ConstVonKarman * ConstVonKarman / (CMFM * CHFH)
    CH2 = ConstVonKarman * ConstVonKarman / (CM2FM2 * CH2FH2)

    ! friction velocity
    FV  = UR * sqrt(CM)
    CH2 = ConstVonKarman * FV / CH2FH2

    ! aerodynamic resistance
    RAMB = max( 1.0, 1.0 / (CM*UR) )
    RAHB = max( 1.0, 1.0 / (CH*UR) )
    RAWB = RAHB

    end associate

  end subroutine ResistanceBareGroundMOST

end module ResistanceBareGroundMostMod
