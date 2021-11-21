module ConfigVarInitMod

!!! Initialize column (1-D) Noah-MP configuration variables
!!! Configuration variables should be first defined in ConfigType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use InputVarType
  use NoahmpVarType

  implicit none

contains

!=== initialize with default values
  subroutine ConfigVarInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    associate(                                      &
              NSNOW => noahmp%config%domain%NSNOW  ,&
              NSOIL => noahmp%config%domain%NSOIL   &
             )

    ! config namelist variable
    noahmp%config%nmlist%OPT_RUNSRF = 1
    noahmp%config%nmlist%OPT_RUNSUB = 1
    noahmp%config%nmlist%OPT_INF    = 1
    noahmp%config%nmlist%OPT_INFDV  = 1
    noahmp%config%nmlist%OPT_TDRN   = 0

    ! config domain variable
    NSNOW                           = 3
    NSOIL                           = 4
    noahmp%config%domain%urban_flag = .false.
    noahmp%config%domain%CROPLU     = .false.
    noahmp%config%domain%ILOC       = huge(1)
    noahmp%config%domain%JLOC       = huge(1)
    noahmp%config%domain%VEGTYP     = huge(1)
    noahmp%config%domain%ISNOW      = huge(1)
    noahmp%config%domain%IST        = huge(1)
    noahmp%config%domain%DT         = huge(1.0)
    noahmp%config%domain%DX         = huge(1.0)

    allocate( noahmp%config%domain%ZSOIL  (       1:NSOIL) )
    allocate( noahmp%config%domain%ZLAYER (       1:NSOIL) )
    allocate( noahmp%config%domain%DZSNSO (-NSNOW+1:NSOIL) )
    allocate( noahmp%config%domain%ZSNSO  (-NSNOW+1:NSOIL) )

    noahmp%config%domain%ZSOIL (:)    = huge(1.0)
    noahmp%config%domain%ZLAYER(:)    = huge(1.0)
    noahmp%config%domain%DZSNSO(:)    = huge(1.0)
    noahmp%config%domain%ZSNSO (:)    = huge(1.0)

    end associate

  end subroutine ConfigVarInitDefault

!=== initialize with input/restart data or table values
  subroutine ConfigVarInitTransfer(noahmp, input)

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
    noahmp%config%nmlist%OPT_RUNSRF = input%OPT_RUNSRFIn
    noahmp%config%nmlist%OPT_RUNSUB = input%OPT_RUNSUBIn
    if (input%OPT_RUNSUBIn /= input%OPT_RUNSRFIn) then
       noahmp%config%nmlist%OPT_RUNSUB = input%OPT_RUNSRFIn
       print*,'reset OPT_RUNSUB to be the same as OPT_RUNSRF ...'
    endif
    noahmp%config%nmlist%OPT_INF    = input%OPT_INFIn
    noahmp%config%nmlist%OPT_INFDV  = input%OPT_INFDVIn
    noahmp%config%nmlist%OPT_TDRN   = input%OPT_TDRNIn

    ! config domain variable
    ILOC                         = input%ILOCIn
    JLOC                         = input%JLOCIn
    NSNOW                        = input%NSNOWIn
    NSOIL                        = input%NSOILIn
    noahmp%config%domain%DT      = input%DTIn
    noahmp%config%domain%DX      = input%DXIn
    noahmp%config%domain%ISNOW   = input%ISNOWIn(ILOC,JLOC)
    noahmp%config%domain%IST     = input%ISTIn(ILOC,JLOC)
    noahmp%config%domain%VEGTYP  = input%VegTypeIn(ILOC,JLOC)
    noahmp%config%domain%urban_flag = input%urban_flagIn(ILOC,JLOC)

    noahmp%config%domain%ZSOIL (       1:NSOIL) = input%ZSOILIn(ILOC,1:NSOIL,JLOC)
    noahmp%config%domain%DZSNSO(-NSNOW+1:NSOIL) = input%DZSNSOIn(ILOC,-NSNOW+1:NSOIL,JLOC)
    noahmp%config%domain%ZSNSO (-NSNOW+1:NSOIL) = input%ZSNSOIn(ILOC,-NSNOW+1:NSOIL,JLOC)

    end associate

  end subroutine ConfigVarInitTransfer

end module ConfigVarInitMod
