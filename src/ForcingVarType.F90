module ForcingVarType

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
    real(kind=kind_noahmp) :: UU              ! wind speed in eastward dir [m/s]
    real(kind=kind_noahmp) :: VV              ! wind speed in northward dir [m/s]
    real(kind=kind_noahmp) :: Q2              ! 2-m specific humidity (kg/kg)
    real(kind=kind_noahmp) :: SFCPRS          ! surface pressure (pa)

  end type forcing_type

end module ForcingVarType
