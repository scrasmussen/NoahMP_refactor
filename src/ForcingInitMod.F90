module ForcingInitMod

!!! Initialize column (1-D) Noah-MP forcing variables
!!! Forcing variables should be first defined in ForcingType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use InputType
  use NoahmpType

  implicit none

contains

!=== initialize with default values
  subroutine ForcingInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    noahmp%forcing%SFCTMP   = huge(1.0)


  end subroutine ForcingInitDefault

!=== initialize with input data or table values
  subroutine ForcingInitTransfer(noahmp, input)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp
    type(input_type) , intent(in)    :: input

    associate(                                   &
              iloc => noahmp%config%domain%iloc ,&
              jloc => noahmp%config%domain%jloc ,&   
              KTS  => input%KTS                 ,&
              KTE  => input%KTE                  &
             )

    noahmp%forcing%SFCTMP   = input%T3D (iloc,1,jloc)

    end associate

  end subroutine ForcingInitTransfer

end module ForcingInitMod
