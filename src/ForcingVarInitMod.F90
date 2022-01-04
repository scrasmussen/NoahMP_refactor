module ForcingVarInitMod

!!! Initialize column (1-D) Noah-MP forcing variables
!!! Forcing variables should be first defined in ForcingType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use InputVarType
  use NoahmpVarType

  implicit none

contains

!=== initialize with default values
  subroutine ForcingVarInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    noahmp%forcing%SFCTMP   = huge(1.0)
    noahmp%forcing%UU       = huge(1.0)
    noahmp%forcing%VV       = huge(1.0)
    noahmp%forcing%Q2       = huge(1.0)
    noahmp%forcing%SFCPRS   = huge(1.0)
    noahmp%forcing%LWDN     = huge(1.0)
    noahmp%forcing%PSFC     = huge(1.0)

  end subroutine ForcingVarInitDefault

!=== initialize with input data or table values
  subroutine ForcingVarInitTransfer(noahmp, input)

    implicit none

    type(input_type) , intent(inout) :: input
    type(noahmp_type), intent(inout) :: noahmp

    !noahmp%forcing%SFCTMP    = input%SFCTMPIn
    noahmp%forcing%UU        = input%UUIn
    noahmp%forcing%VV        = input%VVIn
    noahmp%forcing%Q2        = input%Q2In
    noahmp%forcing%SFCPRS    = input%SFCPRSIn
    noahmp%forcing%LWDN      = input%LWDNIn
    noahmp%forcing%PSFC      = input%SFCPRSIn  ! to be changed when merging with forcing

  end subroutine ForcingVarInitTransfer

end module ForcingVarInitMod
