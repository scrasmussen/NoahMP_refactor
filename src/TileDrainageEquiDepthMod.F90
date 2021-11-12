module TileDrainageEquiDepthMod

!!! Calculate tile drainage equivalent depth (currently used in Hooghoudt's scheme)

  use Machine, only : kind_noahmp
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine TileDrainageEquiDepth(TD_D, TD_L, TD_RD, TD_DE)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: TD_EQUIVALENT_DEPTH
! Original code: P. Valayamkunnath (NCAR)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Nov 8, 2021)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    real(kind=kind_noahmp), intent(in)    :: TD_D    ! tile drainage depth to impermeable layer (m)
    real(kind=kind_noahmp), intent(in)    :: TD_L    ! distance between two drain tubes or tiles (m)
    real(kind=kind_noahmp), intent(in)    :: TD_RD   ! effective radius of drains (m)
    real(kind=kind_noahmp), intent(out)   :: TD_DE   ! Height of water table in the drain Above Impermeable Layer (m)

! local variables
    integer                :: I               ! loop index
    real(kind=kind_noahmp) :: PII = 22.0/7.0  ! pi
    real(kind=kind_noahmp) :: TD_X            ! temporary drain variable
    real(kind=kind_noahmp) :: TD_FX, EX, TERM ! temporary drain variable

! ----------------------------------------------------------------------

    TD_FX = 0.0
    EX    = 0.0
    TERM  = 0.0
    TD_X  = (2.0 * PII * TD_D) / TD_L

    if ( TD_X > 0.5 ) then
       do I = 1, 45, 2
          EX    = exp(-2.0 * I * TD_X)
          TERM  = (4.0 * EX) / ( I* (1.0-EX) )
          TD_FX = TD_FX + TERM
          if ( TERM < 1.0e-6) then
             TD_DE = ( (PII*TD_L) / 8.0 ) / ( log(TD_L/(PII*TD_RD)) + TD_FX )
             exit
          endif
       enddo
    else if ( TD_X < 1.0e-8 ) then
       TD_DE = TD_D
    else
       TD_FX = ( (PII*PII) / (4.0*TD_X) ) + ( log(TD_X/(2.0*PII)) )
       TD_DE = ( (PII*TD_L) / 8.0 ) / ( log(TD_L/(PII*TD_RD)) + TD_FX )
    endif

    if ( TD_DE < 0.0 .and. I <= 2 ) TD_DE = TD_D

  end subroutine TileDrainageEquiDepth

end module TileDrainageEquiDepthMod
