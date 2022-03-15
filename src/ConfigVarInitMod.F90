module ConfigVarInitMod

!!! Initialize column (1-D) Noah-MP configuration variables
!!! Configuration variables should be first defined in ConfigVarType.f90

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
    noahmp%config%nmlist%OPT_DVEG     = huge(1)
    noahmp%config%nmlist%OPT_SNF      = huge(1)
    noahmp%config%nmlist%OPT_BTR      = huge(1)
    noahmp%config%nmlist%OPT_RSF      = huge(1)
    noahmp%config%nmlist%OPT_SFC      = huge(1)
    noahmp%config%nmlist%OPT_CRS      = huge(1)
    noahmp%config%nmlist%OPT_ALB      = huge(1)
    noahmp%config%nmlist%OPT_RAD      = huge(1)
    noahmp%config%nmlist%OPT_STC      = huge(1)
    noahmp%config%nmlist%OPT_TKSNO    = huge(1)
    noahmp%config%nmlist%OPT_TBOT     = huge(1)
    noahmp%config%nmlist%OPT_FRZ      = huge(1)
    noahmp%config%nmlist%OPT_RUNSRF   = huge(1)
    noahmp%config%nmlist%OPT_RUNSUB   = huge(1)
    noahmp%config%nmlist%OPT_INF      = huge(1)
    noahmp%config%nmlist%OPT_INFDV    = huge(1)
    noahmp%config%nmlist%OPT_TDRN     = huge(1)
    noahmp%config%nmlist%OPT_IRR      = huge(1)
    noahmp%config%nmlist%OPT_IRRM     = huge(1)
    noahmp%config%nmlist%OPT_CROP     = huge(1)
    noahmp%config%nmlist%OPT_SOIL     = huge(1)
    noahmp%config%nmlist%OPT_PEDO     = huge(1)

    ! config domain variable
    noahmp%config%domain%LLANDUSE     = "MODIFIED_IGBP_MODIS_NOAH"
    noahmp%config%domain%URBAN_FLAG   = .false.
    noahmp%config%domain%CROPLU       = .false.
    noahmp%config%domain%CROP_ACTIVE  = .false.
    noahmp%config%domain%DVEG_ACTIVE  = .false.
    noahmp%config%domain%NSNOW        = huge(1)
    noahmp%config%domain%NSOIL        = huge(1)
    noahmp%config%domain%ILOC         = huge(1)
    noahmp%config%domain%JLOC         = huge(1)
    noahmp%config%domain%VEGTYP       = huge(1)
    noahmp%config%domain%CROPTYP      = huge(1)
    noahmp%config%domain%ISNOW        = huge(1)
    noahmp%config%domain%IST          = huge(1)
    noahmp%config%domain%NBAND        = huge(1)
    noahmp%config%domain%SOILCOLOR    = huge(1)
    noahmp%config%domain%ICE          = huge(1)
    noahmp%config%domain%NSTAGE       = huge(1)
    noahmp%config%domain%ISWATER      = huge(1)
    noahmp%config%domain%ISBARREN     = huge(1)
    noahmp%config%domain%ISICE        = huge(1)
    noahmp%config%domain%ISCROP       = huge(1)
    noahmp%config%domain%EBLFOREST    = huge(1)
    noahmp%config%domain%YEARLEN      = huge(1)
    noahmp%config%domain%DT           = huge(1.0)
    noahmp%config%domain%DX           = huge(1.0)
    noahmp%config%domain%JULIAN       = huge(1.0)
    noahmp%config%domain%COSZ         = huge(1.0)
    noahmp%config%domain%ZREF         = huge(1.0)
    noahmp%config%domain%DZ8W         = huge(1.0)
    noahmp%config%domain%LAT          = huge(1.0)

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
    noahmp%config%nmlist%OPT_DVEG   = input%OPT_DVEGIn
    noahmp%config%nmlist%OPT_SNF    = input%OPT_SNFIn
    noahmp%config%nmlist%OPT_BTR    = input%OPT_BTRIn
    noahmp%config%nmlist%OPT_RSF    = input%OPT_RSFIn
    noahmp%config%nmlist%OPT_SFC    = input%OPT_SFCIn
    noahmp%config%nmlist%OPT_CRS    = input%OPT_CRSIn
    noahmp%config%nmlist%OPT_ALB    = input%OPT_ALBIn
    noahmp%config%nmlist%OPT_RAD    = input%OPT_RADIn
    noahmp%config%nmlist%OPT_STC    = input%OPT_STCIn
    noahmp%config%nmlist%OPT_TKSNO  = input%OPT_TKSNOIn
    noahmp%config%nmlist%OPT_TBOT   = input%OPT_TBOTIn
    noahmp%config%nmlist%OPT_FRZ    = input%OPT_FRZIn
    noahmp%config%nmlist%OPT_INF    = input%OPT_INFIn
    noahmp%config%nmlist%OPT_INFDV  = input%OPT_INFDVIn
    noahmp%config%nmlist%OPT_TDRN   = input%OPT_TDRNIn
    noahmp%config%nmlist%OPT_IRR    = input%OPT_IRRIn
    noahmp%config%nmlist%OPT_IRRM   = input%OPT_IRRMIn
    noahmp%config%nmlist%OPT_CROP   = input%OPT_CROPIn
    noahmp%config%nmlist%OPT_SOIL   = input%OPT_SOILIn
    noahmp%config%nmlist%OPT_PEDO   = input%OPT_PEDOIn
    noahmp%config%nmlist%OPT_RUNSRF = input%OPT_RUNSRFIn
    noahmp%config%nmlist%OPT_RUNSUB = input%OPT_RUNSUBIn
    if ( input%OPT_RUNSUBIn /= input%OPT_RUNSRFIn ) then
       noahmp%config%nmlist%OPT_RUNSUB = input%OPT_RUNSRFIn
       print*,'reset OPT_RUNSUB to be the same as OPT_RUNSRF for now ...'
    endif

    ! config domain variable
    ILOC                            = input%ILOCIn
    JLOC                            = input%JLOCIn
    NSNOW                           = input%NSNOWIn
    NSOIL                           = input%NSOILIn
    noahmp%config%domain%NBAND      = input%NBANDIn    
    noahmp%config%domain%DT         = input%DTIn
    noahmp%config%domain%DX         = input%DXIn
    noahmp%config%domain%LLANDUSE   = input%LLANDUSEIn
    noahmp%config%domain%VEGTYP     = input%VEGTYPEIn
    noahmp%config%domain%CROPTYP    = input%CROPTYPEIn
    noahmp%config%domain%SOILCOLOR  = input%SOILCOLORIn
    !noahmp%config%domain%ICE        = input%ICEIn
    noahmp%config%domain%JULIAN     = input%JULIANIn
    noahmp%config%domain%ZREF       = input%ZLVLIn
    !noahmp%config%domain%DZ8W      = input%DZ8WIn
    noahmp%config%domain%COSZ       = input%COSZIn
    !noahmp%config%domain%ISNOW      = input%ISNOWIn
    !noahmp%config%domain%IST        = input%ISTIn
    !noahmp%config%domain%URBAN_FLAG = input%URBAN_FLAGIn
    noahmp%config%domain%ISWATER    = input%ISWATER_TABLE
    noahmp%config%domain%ISBARREN   = input%ISBARREN_TABLE
    noahmp%config%domain%ISICE      = input%ISICE_TABLE
    noahmp%config%domain%ISCROP     = input%ISCROP_TABLE
    noahmp%config%domain%EBLFOREST  = input%EBLFOREST_TABLE
    noahmp%config%domain%YEARLEN    = input%YEARLENIn
    noahmp%config%domain%LAT        = input%LATIn
 

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
    noahmp%config%domain%ZSNSO  (-NSNOW+1:NSOIL) = input%ZSNSOIn(-NSNOW+1:NSOIL)

    if ( (input%VEGTYPEIn == input%ISURBAN_TABLE) .or. (input%VEGTYPEIn == input%LCZ_1_TABLE) .or. &
         (input%VEGTYPEIn == input%LCZ_2_TABLE)   .or. (input%VEGTYPEIn == input%LCZ_3_TABLE) .or. &
         (input%VEGTYPEIn == input%LCZ_4_TABLE)   .or. (input%VEGTYPEIn == input%LCZ_5_TABLE) .or. &
         (input%VEGTYPEIn == input%LCZ_6_TABLE)   .or. (input%VEGTYPEIn == input%LCZ_7_TABLE) .or. &
         (input%VEGTYPEIn == input%LCZ_8_TABLE)   .or. (input%VEGTYPEIn == input%LCZ_9_TABLE) .or. &
         (input%VEGTYPEIn == input%LCZ_10_TABLE)  .or. (input%VEGTYPEIn == input%LCZ_11_TABLE) ) then
       noahmp%config%domain%URBAN_FLAG = .true.
    endif

    end associate

  end subroutine ConfigVarInitTransfer

end module ConfigVarInitMod
