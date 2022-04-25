module SnowCoverGroundNiu07Mod

!!! Compute ground snow cover fraction based on Niu and Yang (2007, JGR) scheme

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowCoverGroundNiu07(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Dec 21, 2021)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: BDSNO        ! bulk density of snow (kg/m3)
    real(kind=kind_noahmp)           :: FMELT        ! melting factor for snow cover frac

! --------------------------------------------------------------------
    associate(                                                        &
              MFSNO           => noahmp%water%param%MFSNO            ,& ! in,    snowmelt m parameter
              SCFFAC          => noahmp%water%param%SCFFAC           ,& ! in,    snow cover factor (m) (originally hard-coded 2.5*z0)
              SNOWH           => noahmp%water%state%SNOWH            ,& ! in,    snow depth [m]
              SNEQV           => noahmp%water%state%SNEQV            ,& ! in,    snow water equivalent [mm]
              FSNO            => noahmp%water%state%FSNO              & ! out,   snow cover fraction (-)
             )
! ----------------------------------------------------------------------

    FSNO = 0.0
    if ( SNOWH > 0.0 ) then
         BDSNO = SNEQV / SNOWH
         FMELT = (BDSNO / 100.0)**MFSNO
         !FSNO = tanh( SNOWH /(2.5 * Z0 * FMELT))
         FSNO  = tanh( SNOWH /(SCFFAC * FMELT)) ! C.He: bring hard-coded 2.5*z0 to MPTABLE tunable parameter SCFFAC
    endif

    end associate

  end subroutine SnowCoverGroundNiu07

end module SnowCoverGroundNiu07Mod
