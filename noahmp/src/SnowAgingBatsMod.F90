module SnowAgingBatsMod

!!! Estimate snow age based on BATS snow albedo scheme for use in BATS snow albedo calculation
!!! Reference: Yang et al. (1997) J.of Climate

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowAgingBats(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SNOW_AGE
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: TAGE       ! total aging effects
    real(kind=kind_noahmp)           :: AGE1       ! effects of grain growth due to vapor diffusion
    real(kind=kind_noahmp)           :: AGE2       ! effects of grain growth at freezing of melt water
    real(kind=kind_noahmp)           :: AGE3       ! effects of soot
    real(kind=kind_noahmp)           :: DELA       ! temporary variable
    real(kind=kind_noahmp)           :: SGE        ! temporary variable
    real(kind=kind_noahmp)           :: DELS       ! temporary variable
    real(kind=kind_noahmp)           :: DELA0      ! temporary variable
    real(kind=kind_noahmp)           :: ARG        ! temporary variable

! --------------------------------------------------------------------
    associate(                                                        &
              MainTimeStep    => noahmp%config%domain%MainTimeStep   ,& ! in,     main noahmp timestep (s)
              SWEMX           => noahmp%water%param%SWEMX            ,& ! in,     new snow mass to fully cover old snow (mm)
              TAU0            => noahmp%energy%param%TAU0            ,& ! in,     snow aging parameter
              GRAIN_GROWTH    => noahmp%energy%param%GRAIN_GROWTH    ,& ! in,     vapor diffusion snow growth factor
              EXTRA_GROWTH    => noahmp%energy%param%EXTRA_GROWTH    ,& ! in,     extra snow growth factor near freezing
              DIRT_SOOT       => noahmp%energy%param%DIRT_SOOT       ,& ! in,     dirt and soot effect factor
              TG              => noahmp%energy%state%TG              ,& ! in,     ground temperature (k)
              SnowWaterEquiv           => noahmp%water%state%SnowWaterEquiv            ,& ! in,     snow water equivalent [mm]
              SnowWaterEquivPrev          => noahmp%water%state%SnowWaterEquivPrev           ,& ! in,     snow water equivalent at previous time step (mm)
              TAUSS           => noahmp%energy%state%TAUSS           ,& ! inout,  non-dimensional snow age
              FAGE            => noahmp%energy%state%FAGE             & ! out,    snow age factor
             )
! ----------------------------------------------------------------------

    if ( SnowWaterEquiv <= 0.0 ) then
       TAUSS = 0.0
    else
       DELA0 = MainTimeStep / TAU0
       ARG   = GRAIN_GROWTH * (1.0/ConstFreezePoint - 1.0/TG)
       AGE1  = exp(ARG)
       AGE2  = exp( amin1( 0.0, EXTRA_GROWTH*ARG ) )
       AGE3  = DIRT_SOOT
       TAGE  = AGE1 + AGE2 + AGE3
       DELA  = DELA0 * TAGE
       DELS  = amax1( 0.0, SnowWaterEquiv-SnowWaterEquivPrev ) / SWEMX
       SGE   = (TAUSS + DELA) * (1.0 - DELS)
       TAUSS = amax1( 0.0, SGE )
    endif

    FAGE = TAUSS / (TAUSS + 1.0)

    end associate

  end subroutine SnowAgingBats

end module SnowAgingBatsMod
