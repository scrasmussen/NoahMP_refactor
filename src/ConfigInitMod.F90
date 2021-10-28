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

    type(noahmp_type) :: noahmp

    ! config namelist variable

    ! config domain variable
    noahmp%config%domain%ILOC       = huge(1)
    noahmp%config%domain%JLOC       = huge(1)
    noahmp%config%domain%VEGTYP     = huge(1)
    noahmp%config%domain%DT         = huge(1.0)

  end subroutine ConfigInitDefault

!=== initialize with input data or table values
  subroutine ConfigInitTransfer(noahmp, input)

    type(noahmp_type) :: noahmp
    type(input_type)  :: input

    associate(                                   &
              ILOC => noahmp%config%domain%ILOC ,&
              JLOC => noahmp%config%domain%JLOC  &
             )

    ! config namelist variable

    ! config domain variable
    ILOC                         = input%LonIndex
    JLOC                         = input%LatIndex
    noahmp%config%domain%DT      = input%TimeStep
    noahmp%config%domain%VEGTYP  = input%VegTypeIn(ILOC,JLOC)
    

  end subroutine ConfigInitTransfer

end module ConfigInitMod
