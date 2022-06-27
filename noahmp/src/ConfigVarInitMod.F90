module ConfigVarInitMod

!!! Initialize column (1-D) Noah-MP configuration variables
!!! Configuration variables should be first defined in ConfigVarType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use NoahmpVarType

  implicit none

contains

!=== initialize with default values
  subroutine ConfigVarInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    ! config namelist variable
    noahmp%config%nmlist%OPT_DVEG     = undefined_int
    noahmp%config%nmlist%OPT_SNF      = undefined_int
    noahmp%config%nmlist%OPT_BTR      = undefined_int
    noahmp%config%nmlist%OPT_RSF      = undefined_int
    noahmp%config%nmlist%OPT_SFC      = undefined_int
    noahmp%config%nmlist%OPT_CRS      = undefined_int
    noahmp%config%nmlist%OPT_ALB      = undefined_int
    noahmp%config%nmlist%OPT_RAD      = undefined_int
    noahmp%config%nmlist%OPT_STC      = undefined_int
    noahmp%config%nmlist%OPT_TKSNO    = undefined_int
    noahmp%config%nmlist%OPT_TBOT     = undefined_int
    noahmp%config%nmlist%OPT_FRZ      = undefined_int
    noahmp%config%nmlist%OPT_RUNSRF   = undefined_int
    noahmp%config%nmlist%OPT_RUNSUB   = undefined_int
    noahmp%config%nmlist%OPT_INF      = undefined_int
    noahmp%config%nmlist%OPT_INFDV    = undefined_int
    noahmp%config%nmlist%OPT_TDRN     = undefined_int
    noahmp%config%nmlist%OPT_IRR      = undefined_int
    noahmp%config%nmlist%OPT_IRRM     = undefined_int
    noahmp%config%nmlist%OPT_CROP     = undefined_int
    noahmp%config%nmlist%OPT_SOIL     = undefined_int
    noahmp%config%nmlist%OPT_PEDO     = undefined_int
    noahmp%config%nmlist%OPT_GLA      = undefined_int

    ! config domain variable
    noahmp%config%domain%LLANDUSE     = "MODIFIED_IGBP_MODIS_NOAH"
    noahmp%config%domain%URBAN_FLAG   = .false.
    noahmp%config%domain%CROPLU       = .false.
    noahmp%config%domain%CROP_ACTIVE  = .false.
    noahmp%config%domain%DVEG_ACTIVE  = .false.
    noahmp%config%domain%NSNOW        = undefined_int
    noahmp%config%domain%NSOIL        = undefined_int
    noahmp%config%domain%ILOC         = undefined_int
    noahmp%config%domain%JLOC         = undefined_int
    noahmp%config%domain%VEGTYP       = undefined_int
    noahmp%config%domain%CROPTYP      = undefined_int
    noahmp%config%domain%ISNOW        = undefined_int
    noahmp%config%domain%IST          = undefined_int
    noahmp%config%domain%NBAND        = undefined_int
    noahmp%config%domain%SOILCOLOR    = undefined_int
    noahmp%config%domain%ICE          = undefined_int
    noahmp%config%domain%NSTAGE       = undefined_int
    noahmp%config%domain%ISWATER      = undefined_int
    noahmp%config%domain%ISBARREN     = undefined_int
    noahmp%config%domain%ISICE        = undefined_int
    noahmp%config%domain%ISCROP       = undefined_int
    noahmp%config%domain%EBLFOREST    = undefined_int
    noahmp%config%domain%YEARLEN      = undefined_int
    noahmp%config%domain%SLOPETYP     = undefined_int
    noahmp%config%domain%DT           = undefined_real
    noahmp%config%domain%DX           = undefined_real
    noahmp%config%domain%JULIAN       = undefined_real
    noahmp%config%domain%COSZ         = undefined_real
    noahmp%config%domain%ZREF         = undefined_real
    noahmp%config%domain%DZ8W         = undefined_real
    noahmp%config%domain%LAT          = undefined_real

  end subroutine ConfigVarInitDefault

!=== initialize with input/restart data or table values

  subroutine ConfigVarInitTransfer(noahmp, NoahmpIO)

    implicit none

    type(NoahmpIO_type) , intent(inout) :: NoahmpIO
    type(noahmp_type),    intent(inout) :: noahmp
    integer                             :: IPRINT = .false.   
 
    associate(                                      &
              I     => NoahmpIO%I                  ,&
              J     => NoahmpIO%J                  ,&
              NSNOW => NoahmpIO%NSNOW              ,&
              NSOIL => NoahmpIO%NSOIL               &
             )

    ! config namelist variable
    noahmp%config%nmlist%OPT_DVEG   = NoahmpIO%IDVEG
    noahmp%config%nmlist%OPT_SNF    = NoahmpIO%IOPT_SNF
    noahmp%config%nmlist%OPT_BTR    = NoahmpIO%IOPT_BTR
    noahmp%config%nmlist%OPT_RSF    = NoahmpIO%IOPT_RSF
    noahmp%config%nmlist%OPT_SFC    = NoahmpIO%IOPT_SFC
    noahmp%config%nmlist%OPT_CRS    = NoahmpIO%IOPT_CRS
    noahmp%config%nmlist%OPT_ALB    = NoahmpIO%IOPT_ALB
    noahmp%config%nmlist%OPT_RAD    = NoahmpIO%IOPT_RAD
    noahmp%config%nmlist%OPT_STC    = NoahmpIO%IOPT_STC
    noahmp%config%nmlist%OPT_TKSNO  = NoahmpIO%IOPT_TKSNO
    noahmp%config%nmlist%OPT_TBOT   = NoahmpIO%IOPT_TBOT
    noahmp%config%nmlist%OPT_FRZ    = NoahmpIO%IOPT_FRZ
    noahmp%config%nmlist%OPT_INF    = NoahmpIO%IOPT_INF
    noahmp%config%nmlist%OPT_INFDV  = NoahmpIO%IOPT_INFDV
    noahmp%config%nmlist%OPT_TDRN   = NoahmpIO%IOPT_TDRN
    noahmp%config%nmlist%OPT_IRR    = NoahmpIO%IOPT_IRR
    noahmp%config%nmlist%OPT_IRRM   = NoahmpIO%IOPT_IRRM
    noahmp%config%nmlist%OPT_CROP   = NoahmpIO%IOPT_CROP
    noahmp%config%nmlist%OPT_SOIL   = NoahmpIO%IOPT_SOIL
    noahmp%config%nmlist%OPT_PEDO   = NoahmpIO%IOPT_PEDO
    noahmp%config%nmlist%OPT_RUNSRF = NoahmpIO%IOPT_RUNSRF
    noahmp%config%nmlist%OPT_RUNSUB = NoahmpIO%IOPT_RUNSUB
    noahmp%config%nmlist%OPT_GLA    = NoahmpIO%IOPT_GLA

    if ( NoahmpIO%IOPT_RUNSUB /= NoahmpIO%IOPT_RUNSRF ) then
       noahmp%config%nmlist%OPT_RUNSUB = NoahmpIO%IOPT_RUNSRF
       print*,'reset OPT_RUNSUB to be the same as OPT_RUNSRF for now ...'
    endif

    ! config domain variable
    noahmp%config%domain%NSNOW      = NoahmpIO%NSNOW
    noahmp%config%domain%NSOIL      = NoahmpIO%NSOIL
    noahmp%config%domain%ILOC       = NoahmpIO%I
    noahmp%config%domain%JLOC       = NoahmpIO%J
    noahmp%config%domain%IST        = 1
    noahmp%config%domain%NBAND      = 2 
    noahmp%config%domain%SOILCOLOR  = 4
    noahmp%config%domain%DT         = NoahmpIO%DTBL
    noahmp%config%domain%DX         = NoahmpIO%DX
    noahmp%config%domain%LLANDUSE   = NoahmpIO%LLANDUSE
    noahmp%config%domain%VEGTYP     = NoahmpIO%IVGTYP   (I,  J)
    noahmp%config%domain%CROPTYP    = NoahmpIO%CROPCAT  (I,  J)
    noahmp%config%domain%ICE        = NoahmpIO%ICE
    noahmp%config%domain%JULIAN     = NoahmpIO%JULIAN
    noahmp%config%domain%ZREF       = 0.5*NoahmpIO%DZ8W (I,1,J) 
    noahmp%config%domain%DZ8W       = NoahmpIO%DZ8W     (I,1,J)
    noahmp%config%domain%COSZ       = NoahmpIO%COSZEN   (I,  J)
    noahmp%config%domain%ISNOW      = NoahmpIO%ISNOWXY  (I,  J)
  
    noahmp%config%domain%URBAN_FLAG = .false.
    noahmp%config%domain%ISWATER    = NoahmpIO%ISWATER_TABLE
    noahmp%config%domain%ISBARREN   = NoahmpIO%ISBARREN_TABLE
    noahmp%config%domain%ISICE      = NoahmpIO%ISICE_TABLE
    noahmp%config%domain%ISCROP     = NoahmpIO%ISCROP_TABLE
    noahmp%config%domain%EBLFOREST  = NoahmpIO%EBLFOREST_TABLE
    noahmp%config%domain%YEARLEN    = NoahmpIO%YEARLEN
    noahmp%config%domain%SLOPETYP   = NoahmpIO%SLOPETYP
    noahmp%config%domain%LAT        = NoahmpIO%XLAT(I,J)
    noahmp%config%domain%NSTAGE     = 8

    if( .not. allocated( noahmp%config%domain%ZSOIL   ) ) allocate( noahmp%config%domain%ZSOIL  (       1:NSOIL) )
    if( .not. allocated( noahmp%config%domain%ZLAYER  ) ) allocate( noahmp%config%domain%ZLAYER (       1:NSOIL) )
    if( .not. allocated( noahmp%config%domain%SOILTYP ) ) allocate( noahmp%config%domain%SOILTYP(       1:NSOIL) )
    if( .not. allocated( noahmp%config%domain%DZSNSO  ) ) allocate( noahmp%config%domain%DZSNSO (-NSNOW+1:NSOIL) )
    if( .not. allocated( noahmp%config%domain%ZSNSO   ) ) allocate( noahmp%config%domain%ZSNSO  (-NSNOW+1:NSOIL) )
    
    noahmp%config%domain%SOILTYP(       1:NSOIL) = undefined_int
    noahmp%config%domain%ZSOIL  (       1:NSOIL) = undefined_real
    noahmp%config%domain%ZLAYER (       1:NSOIL) = undefined_real
    noahmp%config%domain%DZSNSO (-NSNOW+1:NSOIL) = undefined_real
    noahmp%config%domain%ZSNSO  (-NSNOW+1:NSOIL) = undefined_real

    if(NoahmpIO%iopt_soil == 1) then
       noahmp%config%domain%SOILTYP(1:NSOIL) = NoahmpIO%ISLTYP(I,J)      ! soil type same in all layers
    elseif(NoahmpIO%iopt_soil == 2) then
       noahmp%config%domain%SOILTYP(1) = nint(NoahmpIO%SOILCL1(I,J))     ! soil type in layer1
       noahmp%config%domain%SOILTYP(2) = nint(NoahmpIO%SOILCL2(I,J))     ! soil type in layer2
       noahmp%config%domain%SOILTYP(3) = nint(NoahmpIO%SOILCL3(I,J))     ! soil type in layer3
       noahmp%config%domain%SOILTYP(4) = nint(NoahmpIO%SOILCL4(I,J))     ! soil type in layer4
    elseif(NoahmpIO%iopt_soil == 3) then
       noahmp%config%domain%SOILTYP(1:NSOIL) = NoahmpIO%ISLTYP(I,J)      ! to initialize with default
    end if 
       
    noahmp%config%domain%ZSOIL  (       1:NSOIL) = NoahmpIO%ZSOIL  (     1:NSOIL      )
    noahmp%config%domain%ZSNSO  (-NSNOW+1:NSOIL) = NoahmpIO%ZSNSOXY(I,-NSNOW+1:NSOIL,J)

    if ( (NoahmpIO%IVGTYP(I,J) == NoahmpIO%ISURBAN_TABLE) .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_1_TABLE) .or. &
         (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_2_TABLE)   .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_3_TABLE) .or. &
         (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_4_TABLE)   .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_5_TABLE) .or. &
         (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_6_TABLE)   .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_7_TABLE) .or. &
         (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_8_TABLE)   .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_9_TABLE) .or. &
         (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_10_TABLE)  .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_11_TABLE) ) then
 
       noahmp%config%domain%URBAN_FLAG = .true.
       
       if(NoahmpIO%SF_URBAN_PHYSICS == 0 ) then
           noahmp%config%domain%VEGTYP = NoahmpIO%ISURBAN_TABLE
       else
           noahmp%config%domain%VEGTYP = NoahmpIO%NATURAL_TABLE  ! set urban vegetation type based on table natural
           NoahmpIO%GVFMAX(I,J)        = 0.96 * 100.0            ! in %
       endif
         
    endif

    noahmp%config%domain%CROPTYP = 0
    if (NoahmpIO%IOPT_CROP > 0 .and. NoahmpIO%IVGTYP (I,  J) == NoahmpIO%ISCROP_TABLE) &
       noahmp%config%domain%CROPTYP = NoahmpIO%DEFAULT_CROP_TABLE   
       
    if (NoahmpIO%IOPT_CROP > 0 .and. NoahmpIO%CROPCAT(I,J) > 0) then
       noahmp%config%domain%CROPTYP  = NoahmpIO%CROPCAT(I,J)             
       noahmp%config%domain%VEGTYP   = NoahmpIO%ISCROP_TABLE
       NoahmpIO%VEGFRA(I,J)          = 0.95 * 100.0              ! in %
       NoahmpIO%GVFMAX(I,J)          = 0.95 * 100.0              ! in %
    endif

    if(any(noahmp%config%domain%SOILTYP == 14) .AND. NoahmpIO%XICE(I,J) == 0.) then
      if(IPRINT) print *, ' SOIL TYPE FOUND TO BE WATER AT A LAND-POINT'
      if(IPRINT) print *, I,J,'RESET SOIL in surfce.F'
      noahmp%config%domain%SOILTYP = 7
    endif

    end associate

  end subroutine ConfigVarInitTransfer

end module ConfigVarInitMod
