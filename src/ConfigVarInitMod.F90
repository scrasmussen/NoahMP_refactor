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

    ! config namelist variable
    noahmp%config%nmlist%OPT_RUNSRF = huge(1)
    noahmp%config%nmlist%OPT_RUNSUB = huge(1)
    noahmp%config%nmlist%OPT_INF    = huge(1)
    noahmp%config%nmlist%OPT_INFDV  = huge(1)
    noahmp%config%nmlist%OPT_TDRN   = huge(1)
    noahmp%config%nmlist%OPT_IRR    = huge(1)
    noahmp%config%nmlist%OPT_IRRM   = huge(1)

    ! config domain variable
    noahmp%config%domain%NSNOW      = 3
    noahmp%config%domain%NSOIL      = 4
    noahmp%config%domain%URBAN_FLAG = .false.
    noahmp%config%domain%CROPLU     = .false.
    noahmp%config%domain%ILOC       = huge(1)
    noahmp%config%domain%JLOC       = huge(1)
    noahmp%config%domain%VEGTYP     = huge(1)
    noahmp%config%domain%ISNOW      = huge(1)
    noahmp%config%domain%IST        = huge(1)
    noahmp%config%domain%DT         = huge(1.0)
    noahmp%config%domain%DX         = huge(1.0)

  end subroutine ConfigVarInitDefault

!=== initialize with input/restart data or table values
  subroutine ConfigVarInitTransfer(noahmp, input)

    implicit none

    type(input_type) , intent(inout) :: input
    type(noahmp_type), intent(inout) :: noahmp

    associate(                                      &
              ILOC  => noahmp%config%domain%ILOC   ,&
              JLOC  => noahmp%config%domain%JLOC   ,&
              NSNOW => noahmp%config%domain%NSNOW  ,&
              NSOIL => noahmp%config%domain%NSOIL   &
             )

    ! config namelist variable
    noahmp%config%nmlist%OPT_INF    = input%OPT_INFIn
    noahmp%config%nmlist%OPT_INFDV  = input%OPT_INFDVIn
    noahmp%config%nmlist%OPT_TDRN   = input%OPT_TDRNIn
    noahmp%config%nmlist%OPT_IRR    = input%OPT_IRRIn
    noahmp%config%nmlist%OPT_IRRM   = input%OPT_IRRMIn
    noahmp%config%nmlist%OPT_RUNSRF = input%OPT_RUNSRFIn
    noahmp%config%nmlist%OPT_RUNSUB = input%OPT_RUNSUBIn
    if ( input%OPT_RUNSUBIn /= input%OPT_RUNSRFIn ) then
       noahmp%config%nmlist%OPT_RUNSUB = input%OPT_RUNSRFIn
       print*,'reset OPT_RUNSUB to be the same as OPT_RUNSRF for now ...'
    endif

    ! config domain variable
    ILOC                         = input%ILOCIn
    JLOC                         = input%JLOCIn
    NSNOW                        = input%NSNOWIn
    NSOIL                        = input%NSOILIn
    noahmp%config%domain%DT      = input%DTIn
    noahmp%config%domain%DX      = input%DXIn
    !noahmp%config%domain%ISNOW   = input%ISNOWIn
    !noahmp%config%domain%IST     = input%ISTIn
    noahmp%config%domain%VEGTYP  = input%VEGTYPEIn
    !noahmp%config%domain%URBAN_FLAG = input%URBAN_FLAGIn

    allocate( noahmp%config%domain%ZSOIL  (       1:NSOIL) )
    allocate( noahmp%config%domain%ZLAYER (       1:NSOIL) )
    allocate( noahmp%config%domain%SOILTYP(       1:NSOIL) )
    allocate( noahmp%config%domain%DZSNSO (-NSNOW+1:NSOIL) )
    allocate( noahmp%config%domain%ZSNSO  (-NSNOW+1:NSOIL) )
    noahmp%config%domain%SOILTYP(       1:NSOIL) = huge(1)
    noahmp%config%domain%ZSOIL  (       1:NSOIL) = huge(1.0)
    noahmp%config%domain%ZLAYER (       1:NSOIL) = huge(1.0)
    noahmp%config%domain%DZSNSO (-NSNOW+1:NSOIL) = huge(1.0)
    noahmp%config%domain%ZSNSO  (-NSNOW+1:NSOIL) = huge(1.0)

    noahmp%config%domain%SOILTYP(       1:NSOIL) = input%SOILTYPEIn(1:NSOIL)
    noahmp%config%domain%ZSOIL  (       1:NSOIL) = input%ZSOILIn(1:NSOIL)
    noahmp%config%domain%DZSNSO (-NSNOW+1:NSOIL) = input%DZSNSOIn(-NSNOW+1:NSOIL)
    noahmp%config%domain%ZSNSO  (-NSNOW+1:NSOIL) = input%ZSNSOIn(-NSNOW+1:NSOIL)

    end associate

  end subroutine ConfigVarInitTransfer

end module ConfigVarInitMod
