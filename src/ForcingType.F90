module ForcingType

!!! Define column (1-D) Noah-MP forcing variables
!!! Forcing variable initialization is done in ForcingInit.f90

  use Machine, only : kind_noahmp

  implicit none
  save
  private

  type, public :: forcing_type

    ! define specific forcing variables
    real(kind=kind_noahmp) :: UU

  end type forcing_type

end module ForcingType
