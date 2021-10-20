module InputType
!!! Define Noah-MP Input variables (2D forcing, namelist, table, static)
!!! Input variable initialization is done in InputInit.f90

  use Machine, only : kind_noahmp

  implicit none
  save
  private

  type, public :: input_type

    ! define specific input variables
    real(kind=kind_noahmp), allocatable, dimension(:,:) :: U2D  ! U wind component 2-D forcing

  end type input_type

end module InputType
