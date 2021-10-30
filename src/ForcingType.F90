module ForcingType

!!! Define column (1-D) Noah-MP forcing variables
!!! Forcing variable initialization is done in ForcingInit.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use Machine, only : kind_noahmp

  implicit none
  save
  private

  type, public :: forcing_type

    ! define specific forcing variables
    real(kind=kind_noahmp) :: SFCTMP          ! surface air temperature [k]

  end type forcing_type

end module ForcingType
