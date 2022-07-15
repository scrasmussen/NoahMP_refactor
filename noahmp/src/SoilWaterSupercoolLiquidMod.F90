module SoilWaterSupercoolLiquidMod

!!! Calculate amount of supercooled liquid soil water content if soil temperature < freezing point
!!! This uses Newton-type iteration to solve the nonlinear implicit equation 
!!! Reference: Eqn.17 in Koren et al. 1999 JGR VOL 104(D16), 19569-19585
!!! New version (June 2001): much faster and more accurate Newton iteration achieved by first
!!! taking log of Eqn above -- less than 4 (typically 1 or 2) iterations achieves convergence.
!!! Explicit 1-step solution option for special case of parameter CK=0, which reduces the
!!! original implicit equation to a simpler explicit form, known as "Flerchinger Eqn". Improved
!!! handling of solution in the limit of freezing point temperature.

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SoilWaterSupercoolLiquid(noahmp, ISOIL, FREE, TKELV, SoilMoisture, SoilLiqWater)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: FRH2O
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp
    integer               , intent(in   ) :: ISOIL          ! soil layer index
    real(kind=kind_noahmp), intent(in   ) :: SoilLiqWater           ! soil liquid water content (m3/m3)
    real(kind=kind_noahmp), intent(in   ) :: SoilMoisture            ! total soil moisture content (m3/m3)
    real(kind=kind_noahmp), intent(in   ) :: TKELV          ! soil temperature (K)
    real(kind=kind_noahmp), intent(out  ) :: FREE           ! soil supercooled liquid water content (m3/m3)

! local variable
    integer                               :: NLOG,KCOUNT    ! temporary variables                 
    character(len=256)                    :: message        ! error message
    real(kind=kind_noahmp)                :: BX,DENOM,DF    ! temporary variables
    real(kind=kind_noahmp)                :: DSWL,FK        ! temporary variables
    real(kind=kind_noahmp)                :: SWL,SWLK       ! temporary frozen content variables
    real(kind=kind_noahmp), parameter     :: CK    = 8.0    ! parameter
    real(kind=kind_noahmp), parameter     :: BLIM  = 5.5    ! limit of B soil parameter
    real(kind=kind_noahmp), parameter     :: ERROR = 0.005  ! error threshold
    real(kind=kind_noahmp), parameter     :: DICE  = 920.0  ! ice density (kg/m3)

! --------------------------------------------------------------------
    associate(                                                        &
              BEXP            => noahmp%water%param%BEXP             ,& ! in,    soil B parameter
              PSISAT          => noahmp%water%param%PSISAT           ,& ! in,    saturated soil matric potential (m)
              SMCMAX          => noahmp%water%param%SMCMAX            & ! in,    saturated value of soil moisture [m3/m3]
             )
! ----------------------------------------------------------------------

    ! limit on parameter B: B < 5.5  (use parameter BLIM)
    ! simulations showed if B > 5.5 unfrozen water content is
    ! non-realistically high at very low temperatures
    BX = BEXP(ISOIL)

    ! initializing iterations counter and interative solution flag
    if ( BEXP(ISOIL) >  BLIM ) BX = BLIM
    NLOG = 0

    ! if soil temperature not largely below freezing point, SoilLiqWater = SoilMoisture
    KCOUNT = 0
    if ( TKELV > (ConstFreezePoint-1.0e-3) ) then
       FREE = SoilMoisture
    else  ! frozen soil case

       !--- Option 1: iterated solution in Koren et al. 1999 JGR Eqn.17
       ! initial guess for SWL (frozen content) 
       if ( CK /= 0.0 ) then
          SWL = SoilMoisture - SoilLiqWater
          if ( SWL > (SoilMoisture-0.02) ) SWL = SoilMoisture - 0.02   ! keep within bounds
          ! start the iterations
          if ( SWL < 0.0 ) SWL = 0.0
1001      Continue
          if ( .not. ((NLOG < 10) .and. (KCOUNT == 0)) ) goto 1002
          NLOG  = NLOG +1
          DF    = alog ( (PSISAT(ISOIL)*ConstGravityAcc/ConstLatHeatFusion) * &
                         ((1.0 + CK*SWL)**2.0) * (SMCMAX(ISOIL)/(SoilMoisture - SWL))**BX ) - &
                         alog ( -(TKELV - ConstFreezePoint) / TKELV )
          DENOM = 2.0 * CK / (1.0 + CK * SWL) + BX / (SoilMoisture - SWL)
          SWLK  = SWL - DF / DENOM
          ! bounds useful for mathematical solution
          if ( SWLK > (SoilMoisture-0.02) ) SWLK = SoilMoisture - 0.02
          if ( SWLK < 0.0 ) SWLK = 0.0
          DSWL = abs(SWLK - SWL)    ! mathematical solution bounds applied
          ! if more than 10 iterations, use explicit method (CK=0 approx.)
          ! when DSWL <= ERROR, no more interations required.
          SWL = SWLK
          if ( DSWL <= ERROR ) then
             KCOUNT = KCOUNT +1
          endif
          ! end of iteration
          ! bounds applied within do-block are valid for physical solution 
          goto 1001
1002      continue
          FREE = SoilMoisture - SWL
       endif
       !--- End Option 1

       !--- Option 2: explicit solution for Flerchinger Eq. i.e., CK=0
       ! in Koren et al. 1999 JGR Eqn. 17
       ! apply physical bounds to Flerchinger solution
       if ( KCOUNT == 0 ) then
          print*, 'Flerchinger used in NEW version. Iterations=', NLOG
          !write(message, '("Flerchinger used in NEW version. Iterations=", I6)') NLOG
          !call wrf_message(trim(message))
          FK = ( ( (ConstLatHeatFusion / (ConstGravityAcc * (-PSISAT(ISOIL)))) * &
                   ((TKELV-ConstFreezePoint) / TKELV) )**(-1.0/BX) ) * SMCMAX(ISOIL)
          if ( FK < 0.02 ) FK = 0.02
          FREE = min( FK, SoilMoisture )
       endif
       !--- End Option 2

    endif

    end associate

  end subroutine SoilWaterSupercoolLiquid

end module SoilWaterSupercoolLiquidMod
