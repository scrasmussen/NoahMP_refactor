module EnergyVarInitMod

!!! Initialize column (1-D) Noah-MP energy variables
!!! Energy variables should be first defined in EnergyType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use InputVarType
  use NoahmpVarType

  implicit none

contains

!=== initialize with default values
  subroutine EnergyVarInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    associate(                                      &
              NSNOW => noahmp%config%domain%NSNOW  ,&
              NSOIL => noahmp%config%domain%NSOIL   &
             )

    ! energy state variable
    noahmp%energy%state%ELAI            = huge(1.0)
    noahmp%energy%state%ESAI            = huge(1.0)
    noahmp%energy%state%LAI             = huge(1.0)
    noahmp%energy%state%SAI             = huge(1.0)
    noahmp%energy%state%FVEG            = huge(1.0)
    noahmp%energy%state%TG              = huge(1.0)
    noahmp%energy%state%TV              = huge(1.0)
    noahmp%energy%state%EAIR            = huge(1.0)
    noahmp%energy%state%FROZEN_CANOPY   = .false.
    noahmp%energy%state%FROZEN_GROUND   = .false.

    allocate( noahmp%energy%state%STC (-NSNOW+1:NSOIL) )

    noahmp%energy%state%STC(:)          = huge(1.0)

    ! energy flux variable
    noahmp%energy%flux%FCEV             = huge(1.0)
    noahmp%energy%flux%FCTR             = huge(1.0)
    noahmp%energy%flux%FGEV             = huge(1.0)
    noahmp%energy%flux%FIRR             = huge(1.0)

    ! energy parameter variable
    allocate( noahmp%energy%param%LAIM (12) )
    allocate( noahmp%energy%param%SAIM (12) )

    noahmp%energy%param%LAIM(:)         = huge(1.0)
    noahmp%energy%param%SAIM(:)         = huge(1.0)

    end associate

  end subroutine EnergyVarInitDefault

!=== initialize with input data or table values
  subroutine EnergyVarInitTransfer(noahmp, input)

    implicit none

    type(input_type) , intent(inout) :: input
    type(noahmp_type), intent(inout) :: noahmp

    associate(                                                  &
              ILOC        => noahmp%config%domain%ILOC         ,&
              JLOC        => noahmp%config%domain%JLOC         ,&
              VEGTYP      => noahmp%config%domain%VEGTYP       ,&
              NSNOW       => noahmp%config%domain%NSNOW        ,&
              NSOIL       => noahmp%config%domain%NSOIL         &
             )

    ! energy state variable
    !noahmp%energy%state%LAI   = input%LAIIn
    !noahmp%energy%state%SAI   = input%SAIIn
    !noahmp%energy%state%FVEG  = input%FVEGIn

    ! energy parameter variable
    noahmp%energy%param%LAIM = input%LAIM_TABLE(VEGTYP,:)
    noahmp%energy%param%SAIM = input%SAIM_TABLE(VEGTYP,:)


    end associate

  end subroutine EnergyVarInitTransfer

end module EnergyVarInitMod
