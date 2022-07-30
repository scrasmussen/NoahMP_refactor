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
              SnowMassFullCoverOld           => noahmp%water%param%SnowMassFullCoverOld            ,& ! in,     new snow mass to fully cover old snow [mm]
              SnowAgeFacBats            => noahmp%energy%param%SnowAgeFacBats            ,& ! in,     snow aging parameter
              SnowGrowVapFacBats    => noahmp%energy%param%SnowGrowVapFacBats    ,& ! in,     vapor diffusion snow growth factor
              SnowGrowFrzFacBats    => noahmp%energy%param%SnowGrowFrzFacBats    ,& ! in,     extra snow growth factor near freezing
              SnowSootFacBats       => noahmp%energy%param%SnowSootFacBats       ,& ! in,     dirt and soot effect factor
              TemperatureGrd              => noahmp%energy%state%TemperatureGrd              ,& ! in,     ground temperature (k)
              SnowWaterEquiv           => noahmp%water%state%SnowWaterEquiv            ,& ! in,     snow water equivalent [mm]
              SnowWaterEquivPrev          => noahmp%water%state%SnowWaterEquivPrev           ,& ! in,     snow water equivalent at previous time step (mm)
              SnowAgeNondim           => noahmp%energy%state%SnowAgeNondim           ,& ! inout,  non-dimensional snow age
              SnowAgeFac            => noahmp%energy%state%SnowAgeFac             & ! out,    snow age factor
             )
! ----------------------------------------------------------------------

    if ( SnowWaterEquiv <= 0.0 ) then
       SnowAgeNondim = 0.0
    else
       DELA0 = MainTimeStep / SnowAgeFacBats
       ARG   = SnowGrowVapFacBats * (1.0/ConstFreezePoint - 1.0/TemperatureGrd)
       AGE1  = exp(ARG)
       AGE2  = exp( amin1( 0.0, SnowGrowFrzFacBats*ARG ) )
       AGE3  = SnowSootFacBats
       TAGE  = AGE1 + AGE2 + AGE3
       DELA  = DELA0 * TAGE
       DELS  = amax1( 0.0, SnowWaterEquiv-SnowWaterEquivPrev ) / SnowMassFullCoverOld
       SGE   = (SnowAgeNondim + DELA) * (1.0 - DELS)
       SnowAgeNondim = amax1( 0.0, SGE )
    endif

    SnowAgeFac = SnowAgeNondim / (SnowAgeNondim + 1.0)

    end associate

  end subroutine SnowAgingBats

end module SnowAgingBatsMod
