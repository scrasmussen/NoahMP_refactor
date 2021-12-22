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

    allocate( noahmp%energy%state%STC     (-NSNOW+1:NSOIL) )
    allocate( noahmp%energy%state%DF      (-NSNOW+1:NSOIL) )
    allocate( noahmp%energy%state%HCPCT   (-NSNOW+1:NSOIL) )
    allocate( noahmp%energy%state%FACT    (-NSNOW+1:NSOIL) )
    allocate( noahmp%energy%state%CVSNO   (-NSNOW+1:0    ) )
    allocate( noahmp%energy%state%TKSNO   (-NSNOW+1:0    ) )
    allocate( noahmp%energy%state%CVSOIL  (       1:NSOIL) )
    allocate( noahmp%energy%state%TKSOIL  (       1:NSOIL) )

    noahmp%energy%state%STC(:)          = huge(1.0)
    noahmp%energy%state%DF(:)           = huge(1.0)
    noahmp%energy%state%HCPCT(:)        = huge(1.0)
    noahmp%energy%state%FACT(:)         = huge(1.0)
    noahmp%energy%state%CVSNO(:)        = huge(1.0)
    noahmp%energy%state%TKSNO(:)        = huge(1.0)
    noahmp%energy%state%CVSOIL(:)       = huge(1.0)
    noahmp%energy%state%TKSOIL(:)       = huge(1.0)

    ! energy flux variable
    noahmp%energy%flux%FCEV             = huge(1.0)
    noahmp%energy%flux%FCTR             = huge(1.0)
    noahmp%energy%flux%FGEV             = huge(1.0)
    noahmp%energy%flux%FIRR             = huge(1.0)
    noahmp%energy%flux%PAHV             = huge(1.0)
    noahmp%energy%flux%PAHG             = huge(1.0)
    noahmp%energy%flux%PAHB             = huge(1.0)

    ! energy parameter variable
    noahmp%energy%param%CSOIL           = huge(1.0)

    allocate( noahmp%energy%param%LAIM (12) )
    allocate( noahmp%energy%param%SAIM (12) )
    allocate( noahmp%energy%param%QUARTZ  (       1:NSOIL) )

    noahmp%energy%param%LAIM(:)         = huge(1.0)
    noahmp%energy%param%SAIM(:)         = huge(1.0)
    noahmp%energy%param%QUARTZ(:)       = huge(1.0)

    end associate

  end subroutine EnergyVarInitDefault

!=== initialize with input data or table values
  subroutine EnergyVarInitTransfer(noahmp, input)

    implicit none

    type(input_type) , intent(inout) :: input
    type(noahmp_type), intent(inout) :: noahmp

    ! local loop index
    integer                          :: ISOIL

    associate(                                                  &
              ILOC        => noahmp%config%domain%ILOC         ,&
              JLOC        => noahmp%config%domain%JLOC         ,&
              VEGTYP      => noahmp%config%domain%VEGTYP       ,&
              SOILTYP     => noahmp%config%domain%SOILTYP      ,&
              NSNOW       => noahmp%config%domain%NSNOW        ,&
              NSOIL       => noahmp%config%domain%NSOIL         &
             )

    ! energy state variable
    !noahmp%energy%state%LAI   = input%LAIIn
    !noahmp%energy%state%SAI   = input%SAIIn
    !noahmp%energy%state%FVEG  = input%FVEGIn
    !noahmp%energy%state%TG    = input%TGIn
    !noahmp%energy%state%TV    = input%TVIn
    !noahmp%energy%state%STC   = input%STCIn

    ! energy parameter variable
    noahmp%energy%param%CSOIL              = input%CSOIL_TABLE
    noahmp%energy%param%LAIM(1:12)         = input%LAIM_TABLE(VEGTYP,1:12)
    noahmp%energy%param%SAIM(1:12)         = input%SAIM_TABLE(VEGTYP,1:12)

    do ISOIL = 1, size(SOILTYP)
       noahmp%energy%param%QUARTZ(ISOIL)    = input%QUARTZ_TABLE(SOILTYP(ISOIL))
    enddo


    end associate

  end subroutine EnergyVarInitTransfer

end module EnergyVarInitMod
