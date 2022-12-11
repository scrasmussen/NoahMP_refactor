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
    noahmp%config%nmlist%OptDynamicVeg               = undefined_int
    noahmp%config%nmlist%OptRainSnowPartition        = undefined_int
    noahmp%config%nmlist%OptSoilWaterTranspiration   = undefined_int
    noahmp%config%nmlist%OptGroundResistanceEvap     = undefined_int
    noahmp%config%nmlist%OptSurfaceDrag              = undefined_int
    noahmp%config%nmlist%OptStomataResistance        = undefined_int
    noahmp%config%nmlist%OptSnowAlbedo               = undefined_int
    noahmp%config%nmlist%OptCanopyRadiationTransfer  = undefined_int
    noahmp%config%nmlist%OptSnowSoilTempTime         = undefined_int
    noahmp%config%nmlist%OptSnowThermConduct         = undefined_int
    noahmp%config%nmlist%OptSoilTemperatureBottom    = undefined_int
    noahmp%config%nmlist%OptSoilSupercoolWater       = undefined_int
    noahmp%config%nmlist%OptRunoffSurface            = undefined_int
    noahmp%config%nmlist%OptRunoffSubsurface         = undefined_int
    noahmp%config%nmlist%OptSoilPermeabilityFrozen   = undefined_int
    noahmp%config%nmlist%OptDynVicInfiltration       = undefined_int
    noahmp%config%nmlist%OptTileDrainage             = undefined_int
    noahmp%config%nmlist%OptIrrigation               = undefined_int
    noahmp%config%nmlist%OptIrrigationMethod         = undefined_int
    noahmp%config%nmlist%OptCropModel                = undefined_int
    noahmp%config%nmlist%OptSoilProperty             = undefined_int
    noahmp%config%nmlist%OptPedotransfer             = undefined_int
    noahmp%config%nmlist%OptGlacierTreatment         = undefined_int

    ! config domain variable
    noahmp%config%domain%LandUseDataName             = "MODIFIED_IGBP_MODIS_NOAH"
    noahmp%config%domain%FlagUrban                   = .false.
    noahmp%config%domain%FlagCropland                = .false.
    noahmp%config%domain%FlagDynamicCrop             = .false.
    noahmp%config%domain%FlagDynamicVeg              = .false.
    noahmp%config%domain%NumSnowLayerMax             = undefined_int
    noahmp%config%domain%NumSnowLayerNeg             = undefined_int
    noahmp%config%domain%NumSoilLayer                = undefined_int
    noahmp%config%domain%GridIndexI                  = undefined_int
    noahmp%config%domain%GridIndexJ                  = undefined_int
    noahmp%config%domain%VegType                     = undefined_int
    noahmp%config%domain%CropType                    = undefined_int
    noahmp%config%domain%SurfaceType                 = undefined_int
    noahmp%config%domain%NumSWRadBand                = undefined_int
    noahmp%config%domain%SoilColor                   = undefined_int
    noahmp%config%domain%IndicatorIceSfc             = undefined_int
    noahmp%config%domain%NumCropGrowStage            = undefined_int
    noahmp%config%domain%IndexWaterPoint             = undefined_int
    noahmp%config%domain%IndexBarrenPoint            = undefined_int
    noahmp%config%domain%IndexIcePoint               = undefined_int
    noahmp%config%domain%IndexCropPoint              = undefined_int
    noahmp%config%domain%IndexEBLForest              = undefined_int
    noahmp%config%domain%NumDayInYear                = undefined_int
    noahmp%config%domain%RunoffSlopeType             = undefined_int
    noahmp%config%domain%MainTimeStep                = undefined_real
    noahmp%config%domain%SoilTimeStep                = undefined_real
    noahmp%config%domain%GridSize                    = undefined_real
    noahmp%config%domain%DayJulianInYear             = undefined_real
    noahmp%config%domain%CosSolarZenithAngle         = undefined_real
    noahmp%config%domain%RefHeightAboveSfc           = undefined_real
    noahmp%config%domain%ThicknessAtmosBotLayer      = undefined_real
    noahmp%config%domain%Latitude                    = undefined_real
    noahmp%config%domain%DepthSoilTempBottom         = undefined_real

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
              NumSoilLayer    => NoahmpIO%NSOIL     &
             )

    ! config namelist variable
    noahmp%config%nmlist%OptDynamicVeg               = NoahmpIO%IOPT_DVEG
    noahmp%config%nmlist%OptRainSnowPartition        = NoahmpIO%IOPT_SNF
    noahmp%config%nmlist%OptSoilWaterTranspiration   = NoahmpIO%IOPT_BTR
    noahmp%config%nmlist%OptGroundResistanceEvap     = NoahmpIO%IOPT_RSF
    noahmp%config%nmlist%OptSurfaceDrag              = NoahmpIO%IOPT_SFC
    noahmp%config%nmlist%OptStomataResistance        = NoahmpIO%IOPT_CRS
    noahmp%config%nmlist%OptSnowAlbedo               = NoahmpIO%IOPT_ALB
    noahmp%config%nmlist%OptCanopyRadiationTransfer  = NoahmpIO%IOPT_RAD
    noahmp%config%nmlist%OptSnowSoilTempTime         = NoahmpIO%IOPT_STC
    noahmp%config%nmlist%OptSnowThermConduct         = NoahmpIO%IOPT_TKSNO
    noahmp%config%nmlist%OptSoilTemperatureBottom    = NoahmpIO%IOPT_TBOT
    noahmp%config%nmlist%OptSoilSupercoolWater       = NoahmpIO%IOPT_FRZ
    noahmp%config%nmlist%OptSoilPermeabilityFrozen   = NoahmpIO%IOPT_INF
    noahmp%config%nmlist%OptDynVicInfiltration       = NoahmpIO%IOPT_INFDV
    noahmp%config%nmlist%OptTileDrainage             = NoahmpIO%IOPT_TDRN
    noahmp%config%nmlist%OptIrrigation               = NoahmpIO%IOPT_IRR
    noahmp%config%nmlist%OptIrrigationMethod         = NoahmpIO%IOPT_IRRM
    noahmp%config%nmlist%OptCropModel                = NoahmpIO%IOPT_CROP
    noahmp%config%nmlist%OptSoilProperty             = NoahmpIO%IOPT_SOIL
    noahmp%config%nmlist%OptPedotransfer             = NoahmpIO%IOPT_PEDO
    noahmp%config%nmlist%OptRunoffSurface            = NoahmpIO%IOPT_RUNSRF
    noahmp%config%nmlist%OptRunoffSubsurface         = NoahmpIO%IOPT_RUNSUB
    noahmp%config%nmlist%OptGlacierTreatment         = NoahmpIO%IOPT_GLA

    ! config domain variable
    noahmp%config%domain%SurfaceType                 = 1
    noahmp%config%domain%NumSWRadBand                = 2
    noahmp%config%domain%SoilColor                   = 4
    noahmp%config%domain%NumCropGrowStage            = 8
    noahmp%config%domain%NumSnowLayerMax             = NoahmpIO%NSNOW
    noahmp%config%domain%NumSnowLayerNeg             = NoahmpIO%ISNOWXY(I,J)
    noahmp%config%domain%NumSoilLayer                = NoahmpIO%NSOIL
    noahmp%config%domain%GridIndexI                  = NoahmpIO%I
    noahmp%config%domain%GridIndexJ                  = NoahmpIO%J
    noahmp%config%domain%MainTimeStep                = NoahmpIO%DTBL
    noahmp%config%domain%SoilTimeStep                = NoahmpIO%soiltstep
    noahmp%config%domain%GridSize                    = NoahmpIO%DX
    noahmp%config%domain%LandUseDataName             = NoahmpIO%LLANDUSE
    noahmp%config%domain%VegType                     = NoahmpIO%IVGTYP(I,J)
    noahmp%config%domain%CropType                    = NoahmpIO%CROPCAT(I,J)
    noahmp%config%domain%IndicatorIceSfc             = NoahmpIO%ICE
    noahmp%config%domain%DayJulianInYear             = NoahmpIO%JULIAN
    noahmp%config%domain%NumDayInYear                = NoahmpIO%YEARLEN
    noahmp%config%domain%Latitude                    = NoahmpIO%XLAT(I,J)
    noahmp%config%domain%RefHeightAboveSfc           = NoahmpIO%DZ8W(I,1,J)*0.5
    noahmp%config%domain%ThicknessAtmosBotLayer      = NoahmpIO%DZ8W(I,1,J)
    noahmp%config%domain%CosSolarZenithAngle         = NoahmpIO%COSZEN(I,J) 
    noahmp%config%domain%IndexWaterPoint             = NoahmpIO%ISWATER_TABLE
    noahmp%config%domain%IndexBarrenPoint            = NoahmpIO%ISBARREN_TABLE
    noahmp%config%domain%IndexIcePoint               = NoahmpIO%ISICE_TABLE
    noahmp%config%domain%IndexCropPoint              = NoahmpIO%ISCROP_TABLE
    noahmp%config%domain%IndexEBLForest              = NoahmpIO%EBLFOREST_TABLE
    noahmp%config%domain%RunoffSlopeType             = NoahmpIO%SLOPETYP
    noahmp%config%domain%DepthSoilTempBottom         = NoahmpIO%ZBOT_TABLE

    if ( .not. allocated(noahmp%config%domain%DepthSoilLayer) )          &
       allocate( noahmp%config%domain%DepthSoilLayer(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%config%domain%ThicknessSoilLayer) )      &
       allocate( noahmp%config%domain%ThicknessSoilLayer(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%config%domain%SoilType) )                &
       allocate( noahmp%config%domain%SoilType(1:NumSoilLayer) )
    if ( .not. allocated(noahmp%config%domain%ThicknessSnowSoilLayer) )  &
       allocate( noahmp%config%domain%ThicknessSnowSoilLayer(-NumSnowLayerMax+1:NumSoilLayer) )
    if ( .not. allocated(noahmp%config%domain%DepthSnowSoilLayer) )      &
       allocate( noahmp%config%domain%DepthSnowSoilLayer(-NumSnowLayerMax+1:NumSoilLayer) )
    
    noahmp%config%domain%SoilType              (:)   = undefined_int
    noahmp%config%domain%DepthSoilLayer        (:)   = undefined_real
    noahmp%config%domain%ThicknessSoilLayer    (:)   = undefined_real
    noahmp%config%domain%ThicknessSnowSoilLayer(:)   = undefined_real
    noahmp%config%domain%DepthSnowSoilLayer    (:)   = undefined_real

    if ( noahmp%config%nmlist%OptSoilProperty == 1 ) then
       noahmp%config%domain%SoilType(1:NumSoilLayer) = NoahmpIO%ISLTYP(I,J)  ! soil type same in all layers
    elseif ( noahmp%config%nmlist%OptSoilProperty == 2 ) then
       noahmp%config%domain%SoilType(1) = nint(NoahmpIO%SOILCL1(I,J))        ! soil type in layer1
       noahmp%config%domain%SoilType(2) = nint(NoahmpIO%SOILCL2(I,J))        ! soil type in layer2
       noahmp%config%domain%SoilType(3) = nint(NoahmpIO%SOILCL3(I,J))        ! soil type in layer3
       noahmp%config%domain%SoilType(4) = nint(NoahmpIO%SOILCL4(I,J))        ! soil type in layer4
    elseif ( noahmp%config%nmlist%OptSoilProperty == 3 ) then
       noahmp%config%domain%SoilType(1:NumSoilLayer) = NoahmpIO%ISLTYP(I,J)  ! to initialize with default
    endif 
       
    noahmp%config%domain%DepthSoilLayer(1:NumSoilLayer) = NoahmpIO%ZSOIL(1:NumSoilLayer)
    noahmp%config%domain%DepthSnowSoilLayer(-NumSnowLayerMax+1:NumSoilLayer) = &
                         NoahmpIO%ZSNSOXY(I,-NumSnowLayerMax+1:NumSoilLayer,J)

    ! treatment for urban point
    if ( (NoahmpIO%IVGTYP(I,J) == NoahmpIO%ISURBAN_TABLE) .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_1_TABLE) .or. &
         (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_2_TABLE)   .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_3_TABLE) .or. &
         (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_4_TABLE)   .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_5_TABLE) .or. &
         (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_6_TABLE)   .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_7_TABLE) .or. &
         (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_8_TABLE)   .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_9_TABLE) .or. &
         (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_10_TABLE)  .or. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%LCZ_11_TABLE) ) then
       noahmp%config%domain%FlagUrban = .true. 
       if(NoahmpIO%SF_URBAN_PHYSICS == 0 ) then
           noahmp%config%domain%VegType = NoahmpIO%ISURBAN_TABLE
       else
           noahmp%config%domain%VegType = NoahmpIO%NATURAL_TABLE  ! set urban vegetation type based on table natural
           NoahmpIO%GVFMAX(I,J)         = 0.96 * 100.0            ! unit: %
       endif         
    endif

    ! treatment for crop point
    noahmp%config%domain%CropType = 0
    if ( (NoahmpIO%IOPT_CROP > 0) .and. (NoahmpIO%IVGTYP(I,J) == NoahmpIO%ISCROP_TABLE) ) &
       noahmp%config%domain%CropType = NoahmpIO%DEFAULT_CROP_TABLE   
       
    if ( (NoahmpIO%IOPT_CROP > 0) .and. (NoahmpIO%CROPCAT(I,J) > 0) ) then
       noahmp%config%domain%CropType = NoahmpIO%CROPCAT(I,J)
       noahmp%config%domain%VegType  = NoahmpIO%ISCROP_TABLE
       NoahmpIO%VEGFRA(I,J)          = 0.95 * 100.0              ! unit: %
       NoahmpIO%GVFMAX(I,J)          = 0.95 * 100.0              ! unit: %
    endif

    ! correct inconsistent soil type
    if ( any(noahmp%config%domain%SoilType == 14) .and. (NoahmpIO%XICE(I,J) == 0.0) ) then
       write(*,*) "SOIL TYPE FOUND TO BE WATER AT A LAND-POINT"
       write(*,*) "RESET SOIL type to SANDY CLAY LOAM at grid = ", I, J
       noahmp%config%domain%SoilType = 7
    endif

    end associate

  end subroutine ConfigVarInitTransfer

end module ConfigVarInitMod
