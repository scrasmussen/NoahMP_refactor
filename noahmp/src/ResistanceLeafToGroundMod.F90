module ResistanceLeafToGroundMod

!!! Compute under-canopy aerodynamic resistance (RAG) and leaf boundary layer resistance (RB)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine ResistanceLeafToGround(noahmp, ITER, VAI, HG)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: RAGRB
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    integer               , intent(in   ) :: ITER         ! iteration index
    real(kind=kind_noahmp), intent(in   ) :: HG           ! temporary ground sensible heat flux (w/m2) in each iteration
    real(kind=kind_noahmp), intent(in   ) :: VAI          ! temporary effective LAI+SAI with constraint (<=6.0)
    type(noahmp_type)     , intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)                :: MPE          ! prevents overflow for division by zero
    real(kind=kind_noahmp)                :: KH           ! turbulent transfer coefficient, sensible heat, (m2/s)
    real(kind=kind_noahmp)                :: TMP1         ! temporary calculation
    real(kind=kind_noahmp)                :: TMP2         ! temporary calculation
    real(kind=kind_noahmp)                :: TMPRAH2      ! temporary calculation for aerodynamic resistances
    real(kind=kind_noahmp)                :: TMPRB        ! temporary calculation for rb
    real(kind=kind_noahmp)                :: FHGNEW       ! temporary vars

! --------------------------------------------------------------------
    associate(                                                        &
              DLEAF           => noahmp%energy%param%DLEAF           ,& ! in,    characteristic leaf dimension (m)
              CWPVT           => noahmp%energy%param%CWPVT           ,& ! in,    canopy wind extinction parameter
              RHOAIR          => noahmp%energy%state%RHOAIR          ,& ! in,    density air (kg/m3)
              TV              => noahmp%energy%state%TV              ,& ! in,    vegetation temperature (K)
              TAH             => noahmp%energy%state%TAH             ,& ! in,    canopy air temperature (K)
              ZPD             => noahmp%energy%state%ZPD             ,& ! in,    zero plane displacement (m)
              Z0MG            => noahmp%energy%state%Z0MG            ,& ! in,    roughness length, momentum, ground (m)
              HCAN            => noahmp%energy%state%HCAN            ,& ! in,    canopy height (m) [note: hcan >= z0mg]
              UC              => noahmp%energy%state%UC              ,& ! in,    wind speed at top of canopy (m/s)
              Z0H             => noahmp%energy%state%Z0HV            ,& ! in,    roughness length, sensible heat (m), vegetated
              Z0HG            => noahmp%energy%state%Z0HG            ,& ! in,    roughness length, sensible heat ground (m), below canopy
              FV              => noahmp%energy%state%FVV             ,& ! in,    friction velocity (m/s), vegetated
              FHG             => noahmp%energy%state%FHG             ,& ! inout, stability correction ground, below canopy
              CWPC            => noahmp%energy%state%CWPC            ,& ! out,   canopy wind extinction coefficient
              MOZG            => noahmp%energy%state%MOZG            ,& ! out,   Monin-Obukhov stability parameter ground, below canopy
              MOLG            => noahmp%energy%state%MOLG            ,& ! out,   Monin-Obukhov length (m), ground, below canopy
              RAMG            => noahmp%energy%state%RAMG            ,& ! out,   ground aerodynamic resistance for momentum (s/m)
              RAHG            => noahmp%energy%state%RAHG            ,& ! out,   ground aerodynamic resistance for sensible heat (s/m)
              RAWG            => noahmp%energy%state%RAWG            ,& ! out,   ground aerodynamic resistance for water vapor (s/m)
              RB              => noahmp%energy%state%RB               & ! out,   bulk leaf boundary layer resistance (s/m)
             )
! ----------------------------------------------------------------------

    ! initialization
    MPE  = 1.0e-6
    MOZG = 0.0
    MOLG = 0.0

    ! stability correction to below canopy resistance
    if ( ITER > 1 ) then
       TMP1 = ConstVonKarman * (ConstGravityAcc / TAH) * HG / (RHOAIR * ConstHeatCapacAir)
       if ( abs(TMP1) <= MPE ) TMP1 = MPE
       MOLG = -1.0 * FV**3 / TMP1
       MOZG = min( (ZPD-Z0MG)/MOLG, 1.0 )
    endif
    if ( MOZG < 0.0 ) then
       FHGNEW = (1.0 - 15.0 * MOZG)**(-0.25)
    else
       FHGNEW = 1.0 + 4.7 * MOZG
    endif
    if ( ITER == 1 ) then
       FHG = FHGNEW
    else
       FHG = 0.5 * (FHG + FHGNEW)
    endif

    ! wind attenuation within canopy
    CWPC    = (CWPVT * VAI * HCAN * FHG)**0.5
    TMP1    = exp( -CWPC * Z0HG / HCAN )
    TMP2    = exp( -CWPC * (Z0H + ZPD) / HCAN )
    TMPRAH2 = HCAN * exp(CWPC) / CWPC * (TMP1-TMP2)

    ! aerodynamic resistances raw and rah between heights zpd+z0h and z0hg.
    KH   = max ( ConstVonKarman * FV * (HCAN-ZPD), MPE )
    RAMG = 0.0
    RAHG = TMPRAH2 / KH
    RAWG = RAHG

    ! leaf boundary layer resistance
    TMPRB = CWPC * 50.0 / ( 1.0 - exp(-CWPC/2.0) )
    RB    = TMPRB * sqrt(DLEAF / UC)
    RB    = min( max(RB, 5.0), 50.0 ) ! limit RB to 5-50, typically RB<50

    end associate

  end subroutine ResistanceLeafToGround

end module ResistanceLeafToGroundMod
