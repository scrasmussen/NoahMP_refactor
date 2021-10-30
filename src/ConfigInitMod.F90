module ConfigInitMod

!!! Initialize column (1-D) Noah-MP configuration variables
!!! Configuration variables should be first defined in ConfigType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use InputType
  use NoahmpType

  implicit none

contains

!=== initialize with default values
  subroutine ConfigInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    associate(                                      &
              NSNOW => noahmp%config%domain%NSNOW  ,&
              NSOIL => noahmp%config%domain%NSOIL   &
             )

    ! config namelist variable

    ! config domain variable
    NSNOW                           = 3
    NSOIL                           = 4
    noahmp%config%domain%ILOC       = huge(1)
    noahmp%config%domain%JLOC       = huge(1)
    noahmp%config%domain%VEGTYP     = huge(1)
    noahmp%config%domain%ISNOW      = huge(1)
    noahmp%config%domain%DT         = huge(1.0)

    allocate( noahmp%config%domain%ZSOIL  (       1:NSOIL) )
    allocate( noahmp%config%domain%DZSNSO (-NSNOW+1:NSOIL) )
    allocate( noahmp%config%domain%ZSNSO  (-NSNOW+1:NSOIL) )

    noahmp%config%domain%ZSOIL (:)    = huge(1.0)
    noahmp%config%domain%DZSNSO(:)    = huge(1.0)
    noahmp%config%domain%ZSNSO (:)    = huge(1.0)

    end associate

  end subroutine ConfigInitDefault

!=== initialize with input/restart data or table values
  subroutine ConfigInitTransfer(noahmp, input)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp
    type(input_type) , intent(in)    :: input

    associate(                                      &
              ILOC  => noahmp%config%domain%ILOC   ,&
              JLOC  => noahmp%config%domain%JLOC   ,&
              NSNOW => noahmp%config%domain%NSNOW  ,&
              NSOIL => noahmp%config%domain%NSOIL   &
             )

    ! config namelist variable

    ! config domain variable
    ILOC                         = input%LonIndex
    JLOC                         = input%LatIndex
    NSNOW                        = input%NSNOWIn
    NSOIL                        = input%NSOILIn
    noahmp%config%domain%DT      = input%TimeStep
    noahmp%config%domain%ISNOW   = input%ISNOWIn(ILOC,JLOC)
    noahmp%config%domain%VEGTYP  = input%VegTypeIn(ILOC,JLOC)

    noahmp%config%domain%ZSOIL (       1:NSOIL) = input%ZSOILIn(ILOC,1:NSOIL,JLOC)
    noahmp%config%domain%DZSNSO(-NSNOW+1:NSOIL) = input%DZSNSOIn(ILOC,-NSNOW+1:NSOIL,JLOC)
    noahmp%config%domain%ZSNSO (-NSNOW+1:NSOIL) = input%ZSNSOIn(ILOC,-NSNOW+1:NSOIL,JLOC)

    end associate

  end subroutine ConfigInitTransfer

end module ConfigInitMod
