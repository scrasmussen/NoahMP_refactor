module ConfigVarInitMod

!!! Initialize column (1-D) Noah-MP configuration variables
!!! Configuration variables should be first defined in ConfigVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (July, 2022)
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
    noahmp%config%nmlist%OptDynamicVeg              = undefined_int
    noahmp%config%nmlist%OptRainSnowPartition       = undefined_int
    noahmp%config%nmlist%OptSoilWaterTranspiration  = undefined_int
    noahmp%config%nmlist%OptGroundResistanceEvap    = undefined_int
    noahmp%config%nmlist%OptSurfaceDrag             = undefined_int
    noahmp%config%nmlist%OptStomataResistance       = undefined_int
    noahmp%config%nmlist%OptSnowAlbedo              = undefined_int
    noahmp%config%nmlist%OptCanopyRadiationTransfer = undefined_int
    noahmp%config%nmlist%OptSnowSoilTempTime        = undefined_int
    noahmp%config%nmlist%OptSnowThermConduct        = undefined_int
    noahmp%config%nmlist%OptSoilTemperatureBottom   = undefined_int
    noahmp%config%nmlist%OptSoilSupercoolWater      = undefined_int
    noahmp%config%nmlist%OptRunoffSurface           = undefined_int
    noahmp%config%nmlist%OptRunoffSubsurface        = undefined_int
    noahmp%config%nmlist%OptSoilPermeabilityFrozen  = undefined_int
    noahmp%config%nmlist%OptDynVicInfiltration      = undefined_int
    noahmp%config%nmlist%OptTileDrainage            = undefined_int
    noahmp%config%nmlist%OptIrrigation              = undefined_int
    noahmp%config%nmlist%OptIrrigationMethod        = undefined_int
    noahmp%config%nmlist%OptCropModel               = undefined_int
    noahmp%config%nmlist%OptSoilProperty            = undefined_int
    noahmp%config%nmlist%OptPedotransfer            = undefined_int
    noahmp%config%nmlist%OptGlacierTreatment        = undefined_int

    ! config domain variable
    noahmp%config%domain%LLANDUSE     = "MODIFIED_IGBP_MODIS_NOAH"
    noahmp%config%domain%URBAN_FLAG   = .false.
    noahmp%config%domain%CROPLU       = .false.
    noahmp%config%domain%CROP_ACTIVE  = .false.
    noahmp%config%domain%DVEG_ACTIVE  = .false.
    noahmp%config%domain%NumSnowLayerMax = undefined_int
    noahmp%config%domain%NumSnowLayerNeg = undefined_int
    noahmp%config%domain%NumSoilLayer    = undefined_int
    noahmp%config%domain%GridIndexI   = undefined_int
    noahmp%config%domain%GridIndexJ   = undefined_int
    noahmp%config%domain%VegType      = undefined_int
    noahmp%config%domain%CropType     = undefined_int
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
    noahmp%config%domain%MainTimeStep = undefined_real
    noahmp%config%domain%GridSize     = undefined_real
    noahmp%config%domain%JULIAN       = undefined_real
    noahmp%config%domain%CosSolarZenithAngle    = undefined_real
    noahmp%config%domain%ZREF         = undefined_real
    noahmp%config%domain%ThicknessAtmosBotLayer = undefined_real
    noahmp%config%domain%Latitude               = undefined_real

  end subroutine ConfigVarInitDefault

!=== initialize with input/restart data or table values

  subroutine ConfigVarInitTransfer(noahmp, NoahmpIO)

    implicit none

    type(NoahmpIO_type) , intent(inout) :: NoahmpIO
    type(noahmp_type),    intent(inout) :: noahmp
 
    associate(                                      &
              I               => NoahmpIO%I        ,&
              J               => NoahmpIO%J        ,&
              NumSnowLayerMax => NoahmpIO%NSNOW    ,&
              NNumSoilLayer   => NoahmpIO%NSOIL     &
             )

    ! config namelist variable
    noahmp%config%nmlist%OptDynamicVeg              = NoahmpIO%IOPT_DVEG
    noahmp%config%nmlist%OptRainSnowPartition       = NoahmpIO%IOPT_SNF
    noahmp%config%nmlist%OptSoilWaterTranspiration  = NoahmpIO%IOPT_BTR
    noahmp%config%nmlist%OptGroundResistanceEvap    = NoahmpIO%IOPT_RSF
    noahmp%config%nmlist%OptSurfaceDrag             = NoahmpIO%IOPT_SFC
    noahmp%config%nmlist%OptStomataResistance       = NoahmpIO%IOPT_CRS
    noahmp%config%nmlist%OptSnowAlbedo              = NoahmpIO%IOPT_ALB
    noahmp%config%nmlist%OptCanopyRadiationTransfer = NoahmpIO%IOPT_RAD
    noahmp%config%nmlist%OptSnowSoilTempTime        = NoahmpIO%IOPT_STC
    noahmp%config%nmlist%OptSnowThermConduct        = NoahmpIO%IOPT_TKSNO
    noahmp%config%nmlist%OptSoilTemperatureBottom   = NoahmpIO%IOPT_TBOT
    noahmp%config%nmlist%OptSoilSupercoolWater      = NoahmpIO%IOPT_FRZ
    noahmp%config%nmlist%OptSoilPermeabilityFrozen  = NoahmpIO%IOPT_INF
    noahmp%config%nmlist%OptDynVicInfiltration      = NoahmpIO%IOPT_INFDV
    noahmp%config%nmlist%OptTileDrainage            = NoahmpIO%IOPT_TDRN
    noahmp%config%nmlist%OptIrrigation              = NoahmpIO%IOPT_IRR
    noahmp%config%nmlist%OptIrrigationMethod        = NoahmpIO%IOPT_IRRM
    noahmp%config%nmlist%OptCropModel               = NoahmpIO%IOPT_CROP
    noahmp%config%nmlist%OptSoilProperty            = NoahmpIO%IOPT_SOIL
    noahmp%config%nmlist%OptPedotransfer            = NoahmpIO%IOPT_PEDO
    noahmp%config%nmlist%OptRunoffSurface           = NoahmpIO%IOPT_RUNSRF
    noahmp%config%nmlist%OptRunoffSubsurface        = NoahmpIO%IOPT_RUNSUB
    noahmp%config%nmlist%OptGlacierTreatment        = NoahmpIO%IOPT_GLA

    ! config domain variable
    noahmp%config%domain%NumSnowLayerMax = NoahmpIO%NSNOW
    noahmp%config%domain%NumSnowLayerNeg = NoahmpIO%ISNOWXY(I,J)
    noahmp%config%domain%NumSoilLayer    = NoahmpIO%NSOIL
    noahmp%config%domain%GridIndexI = NoahmpIO%I
    noahmp%config%domain%GridIndexJ = NoahmpIO%J
    noahmp%config%domain%IST        = 1
    noahmp%config%domain%NBAND      = 2 
    noahmp%config%domain%SOILCOLOR  = 4
    noahmp%config%domain%MainTimeStep = NoahmpIO%DTBL
    noahmp%config%domain%GridSize     = NoahmpIO%DX
    noahmp%config%domain%LLANDUSE   = NoahmpIO%LLANDUSE
    noahmp%config%domain%VegType      = NoahmpIO%IVGTYP(I,J)
    noahmp%config%domain%CropType     = NoahmpIO%CROPCAT  (I,  J)
    noahmp%config%domain%ICE        = NoahmpIO%ICE
    noahmp%config%domain%JULIAN     = NoahmpIO%JULIAN
    noahmp%config%domain%ZREF       = 0.5*NoahmpIO%DZ8W (I,1,J) 
    noahmp%config%domain%ThicknessAtmosBotLayer = NoahmpIO%DZ8W(I,1,J)
    noahmp%config%domain%CosSolarZenithAngle    = NoahmpIO%COSZEN(I,J)
  
    noahmp%config%domain%URBAN_FLAG = .false.
    noahmp%config%domain%ISWATER    = NoahmpIO%ISWATER_TABLE
    noahmp%config%domain%ISBARREN   = NoahmpIO%ISBARREN_TABLE
    noahmp%config%domain%ISICE      = NoahmpIO%ISICE_TABLE
    noahmp%config%domain%ISCROP     = NoahmpIO%ISCROP_TABLE
    noahmp%config%domain%EBLFOREST  = NoahmpIO%EBLFOREST_TABLE
    noahmp%config%domain%YEARLEN    = NoahmpIO%YEARLEN
    noahmp%config%domain%SLOPETYP   = NoahmpIO%SLOPETYP
    noahmp%config%domain%Latitude   = NoahmpIO%XLAT(I,J)
    noahmp%config%domain%NSTAGE     = 8

    if( .not. allocated(noahmp%config%domain%DepthSoilLayer) ) allocate( noahmp%config%domain%DepthSoilLayer(1:NumSoilLayer) )
    if( .not. allocated( noahmp%config%domain%ZLAYER  ) ) allocate( noahmp%config%domain%ZLAYER (1:NumSoilLayer) )
    if( .not. allocated( noahmp%config%domain%SOILTYP ) ) allocate( noahmp%config%domain%SOILTYP(1:NumSoilLayer) )
    if( .not. allocated( noahmp%config%domain%DZSNSO  ) ) allocate( noahmp%config%domain%DZSNSO (-NumSnowLayerMax+1:NumSoilLayer) )
    if( .not. allocated( noahmp%config%domain%ZSNSO   ) ) allocate( noahmp%config%domain%ZSNSO  (-NumSnowLayerMax+1:NumSoilLayer) )
    
    noahmp%config%domain%SOILTYP(1:NumSoilLayer) = undefined_int
    noahmp%config%domain%DepthSoilLayer(1:NumSoilLayer) = undefined_real
    noahmp%config%domain%ZLAYER (1:NumSoilLayer) = undefined_real
    noahmp%config%domain%DZSNSO (-NumSnowLayerMax+1:NumSoilLayer) = undefined_real
    noahmp%config%domain%ZSNSO  (-NumSnowLayerMax+1:NumSoilLayer) = undefined_real

    if(noahmp%config%nmlist%OptSoilProperty == 1) then
       noahmp%config%domain%SOILTYP(1:NumSoilLayer) = NoahmpIO%ISLTYP(I,J)      ! soil type same in all layers
    elseif(noahmp%config%nmlist%OptSoilProperty == 2) then
       noahmp%config%domain%SOILTYP(1) = nint(NoahmpIO%SOILCL1(I,J))     ! soil type in layer1
       noahmp%config%domain%SOILTYP(2) = nint(NoahmpIO%SOILCL2(I,J))     ! soil type in layer2
       noahmp%config%domain%SOILTYP(3) = nint(NoahmpIO%SOILCL3(I,J))     ! soil type in layer3
       noahmp%config%domain%SOILTYP(4) = nint(NoahmpIO%SOILCL4(I,J))     ! soil type in layer4
    elseif(noahmp%config%nmlist%OptSoilProperty == 3) then
       noahmp%config%domain%SOILTYP(1:NumSoilLayer) = NoahmpIO%ISLTYP(I,J)      ! to initialize with default
    endif 
       
    noahmp%config%domain%DepthSoilLayer(1:NumSoilLayer) = NoahmpIO%ZSOIL(1:NumSoilLayer)
    noahmp%config%domain%ZSNSO  (-NumSnowLayerMax+1:NumSoilLayer) = NoahmpIO%ZSNSOXY(I,-NumSnowLayerMax+1:NumSoilLayer,J)

    if ( (NoahmpIO%IVGTYP(I,J) == NoahmpIO%ISURBAN_TABLE) .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_1_TABLE) .or. &
         (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_2_TABLE)   .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_3_TABLE) .or. &
         (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_4_TABLE)   .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_5_TABLE) .or. &
         (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_6_TABLE)   .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_7_TABLE) .or. &
         (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_8_TABLE)   .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_9_TABLE) .or. &
         (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_10_TABLE)  .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_11_TABLE) ) then
 
       noahmp%config%domain%URBAN_FLAG = .true.
       
       if(NoahmpIO%SF_URBAN_PHYSICS == 0 ) then
           noahmp%config%domain%VegType = NoahmpIO%ISURBAN_TABLE
       else
           noahmp%config%domain%VegType = NoahmpIO%NATURAL_TABLE  ! set urban vegetation type based on table natural
           NoahmpIO%GVFMAX(I,J)        = 0.96 * 100.0            ! in %
       endif
         
    endif

    noahmp%config%domain%CropType = 0
    if ((NoahmpIO%IOPT_CROP > 0) .and. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%ISCROP_TABLE)) &
       noahmp%config%domain%CropType = NoahmpIO%DEFAULT_CROP_TABLE   
       
    if (NoahmpIO%IOPT_CROP > 0 .and. NoahmpIO%CROPCAT(I,J) > 0) then
       noahmp%config%domain%CropType  = NoahmpIO%CROPCAT(I,J)             
       noahmp%config%domain%VegType   = NoahmpIO%ISCROP_TABLE
       NoahmpIO%VEGFRA(I,J)          = 0.95 * 100.0              ! in %
       NoahmpIO%GVFMAX(I,J)          = 0.95 * 100.0              ! in %
    endif

    if (any(noahmp%config%domain%SOILTYP == 14) .and. (NoahmpIO%XICE(I,J) == 0.0) ) then
       print *, ' SOIL TYPE FOUND TO BE WATER AT A LAND-POINT'
       print *, I,J,'RESET SOIL in surfce.F'
       noahmp%config%domain%SOILTYP = 7
    endif

    end associate

  end subroutine ConfigVarInitTransfer

end module ConfigVarInitMod
