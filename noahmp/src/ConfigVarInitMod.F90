module ConfigVarInitMod

!!! Initialize column (1-D) Noah-MP configuration variables
!!! Configuration variables should be first defined in ConfigVarType.f90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (Oct 27, 2021)
! -------------------------------------------------------------------------

  use NoahmpIOVarType
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
    noahmp%config%nmlist%OPT_GLA      = huge(1)

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
    noahmp%config%domain%SLOPETYP     = huge(1)
    noahmp%config%domain%DT           = huge(1.0)
    noahmp%config%domain%DX           = huge(1.0)
    noahmp%config%domain%JULIAN       = huge(1.0)
    noahmp%config%domain%COSZ         = huge(1.0)
    noahmp%config%domain%ZREF         = huge(1.0)
    noahmp%config%domain%DZ8W         = huge(1.0)
    noahmp%config%domain%LAT          = huge(1.0)

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
    
    noahmp%config%domain%SOILTYP(       1:NSOIL) = huge(1)
    noahmp%config%domain%ZSOIL  (       1:NSOIL) = huge(1.0)
    noahmp%config%domain%ZLAYER (       1:NSOIL) = huge(1.0)
    noahmp%config%domain%DZSNSO (-NSNOW+1:NSOIL) = huge(1.0)
    noahmp%config%domain%ZSNSO  (-NSNOW+1:NSOIL) = huge(1.0)

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
