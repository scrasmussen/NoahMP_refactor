module ForcingVarInitMod

!!! Initialize column (1-D) Noah-MP forcing variables
!!! Forcing variables should be first defined in ForcingVarType.F90

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

    noahmp%forcing%SFCTMP      = huge(1.0)
    noahmp%forcing%UU          = huge(1.0)
    noahmp%forcing%VV          = huge(1.0)
    noahmp%forcing%Q2          = huge(1.0)
    noahmp%forcing%SFCPRS      = huge(1.0)
    noahmp%forcing%LWDN        = huge(1.0)
    noahmp%forcing%PSFC        = huge(1.0)
    noahmp%forcing%SOLDN       = huge(1.0)
    noahmp%forcing%PRCPCONV    = huge(1.0)
    noahmp%forcing%PRCPNONC    = huge(1.0)
    noahmp%forcing%PRCPSHCV    = huge(1.0)
    noahmp%forcing%PRCPSNOW    = huge(1.0)
    noahmp%forcing%PRCPGRPL    = huge(1.0)
    noahmp%forcing%PRCPHAIL    = huge(1.0)
    noahmp%forcing%PSFC        = huge(1.0)
    noahmp%forcing%TBOT        = huge(1.0)

  end subroutine ForcingVarInitDefault

!=== initialize with input data or table values
  subroutine ForcingVarInitTransfer(noahmp, input)

    implicit none

    type(input_type) , intent(inout) :: input
    type(noahmp_type), intent(inout) :: noahmp

    noahmp%forcing%SFCTMP      = input%SFCTMPIn
    noahmp%forcing%UU          = input%UUIn
    noahmp%forcing%VV          = input%VVIn
    noahmp%forcing%Q2          = input%Q2In
    noahmp%forcing%SFCPRS      = input%SFCPRSIn
    noahmp%forcing%LWDN        = input%LWDNIn
    noahmp%forcing%PSFC        = input%PSFCIn
    noahmp%forcing%SOLDN       = input%SOLDNIn
    noahmp%forcing%PRCPCONV    = input%PRCPCONVIn
    noahmp%forcing%PRCPNONC    = input%PRCPNONCIn
    noahmp%forcing%PRCPSHCV    = input%PRCPSHCVIn
    noahmp%forcing%PRCPSNOW    = input%PRCPSNOWIn
    noahmp%forcing%PRCPGRPL    = input%PRCPGRPLIn
    noahmp%forcing%PRCPHAIL    = input%PRCPHAILIn
    noahmp%forcing%TBOT        = input%TBOTIn

  end subroutine ForcingVarInitTransfer

end module ForcingVarInitMod
