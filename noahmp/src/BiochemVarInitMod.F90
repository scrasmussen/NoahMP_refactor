module BiochemVarInitMod

!!! Initialize column (1-D) Noah-MP biochemistry (carbon,nitrogen,etc) variables
!!! Biochemistry variables should be first defined in BiochemVarType.F90

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
  subroutine BiochemVarInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    associate( NumCropGrowStage => noahmp%config%domain%NumCropGrowStage )

    ! biochem state variables
    noahmp%biochem%state%PlantGrowStage             = undefined_int
    noahmp%biochem%state%IndexPlanting              = undefined_int
    noahmp%biochem%state%IndexHarvest               = undefined_int
    noahmp%biochem%state%IndexGrowSeason            = undefined_real
    noahmp%biochem%state%NitrogenConcFoliage        = undefined_real
    noahmp%biochem%state%LeafMass                   = undefined_real
    noahmp%biochem%state%RootMass                   = undefined_real
    noahmp%biochem%state%StemMass                   = undefined_real
    noahmp%biochem%state%WoodMass                   = undefined_real
    noahmp%biochem%state%CarbonMassDeepSoil         = undefined_real
    noahmp%biochem%state%CarbonMassShallowSoil      = undefined_real
    noahmp%biochem%state%CarbonMassSoilTot          = undefined_real
    noahmp%biochem%state%CarbonMassLiveTot          = undefined_real
    noahmp%biochem%state%LeafAreaPerMass            = undefined_real
    noahmp%biochem%state%StemAreaPerMass            = undefined_real
    noahmp%biochem%state%LeafMassMin                = undefined_real
    noahmp%biochem%state%StemMassMin                = undefined_real
    noahmp%biochem%state%CarbonFracToLeaf           = undefined_real
    noahmp%biochem%state%CarbonFracToRoot           = undefined_real
    noahmp%biochem%state%CarbonFracToWood           = undefined_real
    noahmp%biochem%state%CarbonFracToStem           = undefined_real
    noahmp%biochem%state%WoodCarbonFrac             = undefined_real
    noahmp%biochem%state%CarbonFracToWoodRoot       = undefined_real
    noahmp%biochem%state%MicroRespFactorSoilWater   = undefined_real
    noahmp%biochem%state%MicroRespFactorSoilTemp    = undefined_real
    noahmp%biochem%state%RespFacNitrogenFoliage     = undefined_real
    noahmp%biochem%state%RespFacTemperature         = undefined_real
    noahmp%biochem%state%RespReductionFac           = undefined_real
    noahmp%biochem%state%GrainMass                  = undefined_real
    noahmp%biochem%state%GrowDegreeDay              = undefined_real

    ! biochem flux variables
    noahmp%biochem%flux%PhotosynLeafSunlit          = undefined_real
    noahmp%biochem%flux%PhotosynLeafShade           = undefined_real
    noahmp%biochem%flux%PhotosynCrop                = undefined_real
    noahmp%biochem%flux%PhotosynTotal               = undefined_real
    noahmp%biochem%flux%GrossPriProduction          = undefined_real
    noahmp%biochem%flux%NetPriProductionTot         = undefined_real
    noahmp%biochem%flux%NetEcoExchange              = undefined_real
    noahmp%biochem%flux%RespirationPlantTot         = undefined_real
    noahmp%biochem%flux%RespirationSoilOrg          = undefined_real
    noahmp%biochem%flux%CarbonToAtmos               = undefined_real
    noahmp%biochem%flux%NetPriProductionLeaf        = undefined_real
    noahmp%biochem%flux%NetPriProductionRoot        = undefined_real
    noahmp%biochem%flux%NetPriProductionWood        = undefined_real
    noahmp%biochem%flux%NetPriProductionStem        = undefined_real
    noahmp%biochem%flux%GrowthRespLeaf              = undefined_real
    noahmp%biochem%flux%GrowthRespRoot              = undefined_real
    noahmp%biochem%flux%GrowthRespWood              = undefined_real
    noahmp%biochem%flux%GrowthRespStem              = undefined_real
    noahmp%biochem%flux%LeafMassMaxChg              = undefined_real
    noahmp%biochem%flux%StemMassMaxChg              = undefined_real
    noahmp%biochem%flux%CarbonDecayToStable         = undefined_real
    noahmp%biochem%flux%RespirationLeaf             = undefined_real
    noahmp%biochem%flux%RespirationStem             = undefined_real
    noahmp%biochem%flux%GrowthRespGrain             = undefined_real
    noahmp%biochem%flux%NetPriProductionGrain       = undefined_real
    noahmp%biochem%flux%ConvRootToGrain             = undefined_real
    noahmp%biochem%flux%ConvStemToGrain             = undefined_real
    noahmp%biochem%flux%RespirationWood             = undefined_real
    noahmp%biochem%flux%RespirationLeafMaint        = undefined_real
    noahmp%biochem%flux%RespirationRoot             = undefined_real
    noahmp%biochem%flux%DeathLeaf                   = undefined_real
    noahmp%biochem%flux%DeathStem                   = undefined_real
    noahmp%biochem%flux%CarbonAssim                 = undefined_real
    noahmp%biochem%flux%TurnoverLeaf                = undefined_real
    noahmp%biochem%flux%TurnoverStem                = undefined_real
    noahmp%biochem%flux%TurnoverWood                = undefined_real
    noahmp%biochem%flux%RespirationSoil             = undefined_real
    noahmp%biochem%flux%TurnoverRoot                = undefined_real
    noahmp%biochem%flux%CarbohydrAssim              = undefined_real
    noahmp%biochem%flux%TurnoverGrain               = undefined_real
    noahmp%biochem%flux%ConvLeafToGrain             = undefined_real
    noahmp%biochem%flux%RespirationGrain            = undefined_real
 
    ! biochem parameter variables
    noahmp%biochem%param%DatePlanting               = undefined_int
    noahmp%biochem%param%DateHarvest                = undefined_int
    noahmp%biochem%param%QuantumEfficiency25C       = undefined_real
    noahmp%biochem%param%CarboxylRateMax25C         = undefined_real
    noahmp%biochem%param%CarboxylRateMaxQ10         = undefined_real
    noahmp%biochem%param%PhotosynPathC3             = undefined_real
    noahmp%biochem%param%SlopeConductToPhotosyn     = undefined_real
    noahmp%biochem%param%TemperatureMinPhotosyn     = undefined_real
    noahmp%biochem%param%LeafAreaPerMass1side       = undefined_real
    noahmp%biochem%param%NitrogenConcFoliageMax     = undefined_real
    noahmp%biochem%param%WoodToRootRatio            = undefined_real
    noahmp%biochem%param%WoodPoolIndex              = undefined_real
    noahmp%biochem%param%TurnoverCoeffLeafVeg       = undefined_real
    noahmp%biochem%param%LeafDeathWaterCoeffVeg     = undefined_real
    noahmp%biochem%param%LeafDeathTempCoeffVeg      = undefined_real
    noahmp%biochem%param%MicroRespCoeff             = undefined_real
    noahmp%biochem%param%RespMaintQ10               = undefined_real
    noahmp%biochem%param%RespMaintLeaf25C           = undefined_real
    noahmp%biochem%param%RespMaintStem25C           = undefined_real
    noahmp%biochem%param%RespMaintRoot25C           = undefined_real
    noahmp%biochem%param%RespMaintGrain25C          = undefined_real
    noahmp%biochem%param%GrowthRespFrac             = undefined_real
    noahmp%biochem%param%TemperaureLeafFreeze       = undefined_real
    noahmp%biochem%param%LeafAreaPerBiomass         = undefined_real
    noahmp%biochem%param%TempBaseGrowDegDay         = undefined_real
    noahmp%biochem%param%TempMaxGrowDegDay          = undefined_real
    noahmp%biochem%param%GrowDegDayEmerg            = undefined_real
    noahmp%biochem%param%GrowDegDayInitVeg          = undefined_real
    noahmp%biochem%param%GrowDegDayPostVeg          = undefined_real
    noahmp%biochem%param%GrowDegDayInitReprod       = undefined_real
    noahmp%biochem%param%GrowDegDayMature           = undefined_real
    noahmp%biochem%param%PhotosynRadFrac            = undefined_real
    noahmp%biochem%param%TempMinCarbonAssim         = undefined_real
    noahmp%biochem%param%TempMaxCarbonAssim         = undefined_real
    noahmp%biochem%param%TempMaxCarbonAssimMax      = undefined_real
    noahmp%biochem%param%CarbonAssimRefMax          = undefined_real
    noahmp%biochem%param%LightExtCoeff              = undefined_real
    noahmp%biochem%param%LighUseEfficiency          = undefined_real
    noahmp%biochem%param%CarbonAssimReducFac        = undefined_real
    noahmp%biochem%param%StemAreaIndexMin           = undefined_real
    noahmp%biochem%param%WoodAllocFac               = undefined_real
    noahmp%biochem%param%WaterStressCoeff           = undefined_real
    noahmp%biochem%param%LeafAreaIndexMin           = undefined_real
    noahmp%biochem%param%TurnoverCoeffRootVeg       = undefined_real
    noahmp%biochem%param%WoodRespCoeff              = undefined_real

    if ( .not. allocated(noahmp%biochem%param%LeafDeathTempCoeffCrop) )  &
       allocate( noahmp%biochem%param%LeafDeathTempCoeffCrop(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%LeafDeathWaterCoeffCrop) ) &
       allocate( noahmp%biochem%param%LeafDeathWaterCoeffCrop(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%CarbohydrLeafToGrain) )    &
       allocate( noahmp%biochem%param%CarbohydrLeafToGrain(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%CarbohydrStemToGrain) )    &
       allocate( noahmp%biochem%param%CarbohydrStemToGrain(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%CarbohydrRootToGrain) )    &
       allocate( noahmp%biochem%param%CarbohydrRootToGrain(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%CarbohydrFracToLeaf) )     &
       allocate( noahmp%biochem%param%CarbohydrFracToLeaf(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%CarbohydrFracToStem) )     &
       allocate( noahmp%biochem%param%CarbohydrFracToStem(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%CarbohydrFracToRoot) )     &
       allocate( noahmp%biochem%param%CarbohydrFracToRoot(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%CarbohydrFracToGrain) )    &
       allocate( noahmp%biochem%param%CarbohydrFracToGrain(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%TurnoverCoeffLeafCrop) )   &
       allocate( noahmp%biochem%param%TurnoverCoeffLeafCrop(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%TurnoverCoeffStemCrop) )   &
       allocate( noahmp%biochem%param%TurnoverCoeffStemCrop(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%TurnoverCoeffRootCrop) )   &
       allocate( noahmp%biochem%param%TurnoverCoeffRootCrop(1:NumCropGrowStage) )

    noahmp%biochem%param%LeafDeathTempCoeffCrop (:) = undefined_real
    noahmp%biochem%param%LeafDeathWaterCoeffCrop(:) = undefined_real
    noahmp%biochem%param%CarbohydrLeafToGrain   (:) = undefined_real
    noahmp%biochem%param%CarbohydrStemToGrain   (:) = undefined_real
    noahmp%biochem%param%CarbohydrRootToGrain   (:) = undefined_real
    noahmp%biochem%param%CarbohydrFracToLeaf    (:) = undefined_real
    noahmp%biochem%param%CarbohydrFracToStem    (:) = undefined_real
    noahmp%biochem%param%CarbohydrFracToRoot    (:) = undefined_real
    noahmp%biochem%param%CarbohydrFracToGrain   (:) = undefined_real
    noahmp%biochem%param%TurnoverCoeffLeafCrop  (:) = undefined_real
    noahmp%biochem%param%TurnoverCoeffStemCrop  (:) = undefined_real
    noahmp%biochem%param%TurnoverCoeffRootCrop  (:) = undefined_real

    end associate

  end subroutine BiochemVarInitDefault


!=== initialize with input data or table values
  subroutine BiochemVarInitTransfer(noahmp, NoahmpIO)

    implicit none

    type(noahmp_type),   intent(inout) :: noahmp
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

    associate(                                                   &
              I            => noahmp%config%domain%GridIndexI   ,&
              J            => noahmp%config%domain%GridIndexJ   ,&
              VegType      => noahmp%config%domain%VegType      ,&
              CropType     => noahmp%config%domain%CropType     ,&
              OptCropModel => noahmp%config%nmlist%OptCropModel  &
             )

    ! biochem state variables
    noahmp%biochem%state%PlantGrowStage             = NoahmpIO%PGSXY   (I,J)   
    noahmp%biochem%state%LeafMass                   = NoahmpIO%LFMASSXY(I,J)
    noahmp%biochem%state%RootMass                   = NoahmpIO%RTMASSXY(I,J)
    noahmp%biochem%state%StemMass                   = NoahmpIO%STMASSXY(I,J) 
    noahmp%biochem%state%WoodMass                   = NoahmpIO%WOODXY  (I,J) 
    noahmp%biochem%state%CarbonMassDeepSoil         = NoahmpIO%STBLCPXY(I,J) 
    noahmp%biochem%state%CarbonMassShallowSoil      = NoahmpIO%FASTCPXY(I,J)
    noahmp%biochem%state%GrainMass                  = NoahmpIO%GRAINXY (I,J)  
    noahmp%biochem%state%GrowDegreeDay              = NoahmpIO%GDDXY   (I,J)  
    noahmp%biochem%state%NitrogenConcFoliage        = 1.0  ! for now, set to nitrogen saturation

    ! biochem parameter variables
    noahmp%biochem%param%NitrogenConcFoliageMax     = NoahmpIO%FOLNMX_TABLE (VegType)
    noahmp%biochem%param%QuantumEfficiency25C       = NoahmpIO%QE25_TABLE   (VegType)
    noahmp%biochem%param%CarboxylRateMax25C         = NoahmpIO%VCMX25_TABLE (VegType)
    noahmp%biochem%param%CarboxylRateMaxQ10         = NoahmpIO%AVCMX_TABLE  (VegType)
    noahmp%biochem%param%PhotosynPathC3             = NoahmpIO%C3PSN_TABLE  (VegType)
    noahmp%biochem%param%SlopeConductToPhotosyn     = NoahmpIO%MP_TABLE     (VegType)
    noahmp%biochem%param%RespMaintQ10               = NoahmpIO%ARM_TABLE    (VegType)
    noahmp%biochem%param%RespMaintLeaf25C           = NoahmpIO%RMF25_TABLE  (VegType)
    noahmp%biochem%param%RespMaintStem25C           = NoahmpIO%RMS25_TABLE  (VegType)
    noahmp%biochem%param%RespMaintRoot25C           = NoahmpIO%RMR25_TABLE  (VegType)
    noahmp%biochem%param%WoodToRootRatio            = NoahmpIO%WRRAT_TABLE  (VegType)
    noahmp%biochem%param%WoodPoolIndex              = NoahmpIO%WDPOOL_TABLE (VegType)
    noahmp%biochem%param%TurnoverCoeffLeafVeg       = NoahmpIO%LTOVRC_TABLE (VegType)
    noahmp%biochem%param%TemperaureLeafFreeze       = NoahmpIO%TDLEF_TABLE  (VegType)
    noahmp%biochem%param%LeafDeathWaterCoeffVeg     = NoahmpIO%DILEFW_TABLE (VegType)
    noahmp%biochem%param%LeafDeathTempCoeffVeg      = NoahmpIO%DILEFC_TABLE (VegType)
    noahmp%biochem%param%GrowthRespFrac             = NoahmpIO%FRAGR_TABLE  (VegType)
    noahmp%biochem%param%MicroRespCoeff             = NoahmpIO%MRP_TABLE    (VegType)
    noahmp%biochem%param%TemperatureMinPhotosyn     = NoahmpIO%TMIN_TABLE   (VegType)
    noahmp%biochem%param%LeafAreaPerMass1side       = NoahmpIO%SLA_TABLE    (VegType)
    noahmp%biochem%param%StemAreaIndexMin           = NoahmpIO%XSAMIN_TABLE (VegType)
    noahmp%biochem%param%WoodAllocFac               = NoahmpIO%BF_TABLE     (VegType)
    noahmp%biochem%param%WaterStressCoeff           = NoahmpIO%WSTRC_TABLE  (VegType)
    noahmp%biochem%param%LeafAreaIndexMin           = NoahmpIO%LAIMIN_TABLE (VegType)
    noahmp%biochem%param%TurnoverCoeffRootVeg       = NoahmpIO%RTOVRC_TABLE (VegType)
    noahmp%biochem%param%WoodRespCoeff              = NoahmpIO%RSWOODC_TABLE(VegType)

    if ( (OptCropModel > 0) .and. (CropType > 0) ) then
       noahmp%biochem%param%DatePlanting            = NoahmpIO%PLTDAY_TABLE   (CropType)
       noahmp%biochem%param%DateHarvest             = NoahmpIO%HSDAY_TABLE    (CropType)
       noahmp%biochem%param%NitrogenConcFoliageMax  = NoahmpIO%FOLNMXI_TABLE  (CropType)
       noahmp%biochem%param%QuantumEfficiency25C    = NoahmpIO%QE25I_TABLE    (CropType)
       noahmp%biochem%param%CarboxylRateMax25C      = NoahmpIO%VCMX25I_TABLE  (CropType)
       noahmp%biochem%param%CarboxylRateMaxQ10      = NoahmpIO%AVCMXI_TABLE   (CropType)
       noahmp%biochem%param%PhotosynPathC3          = NoahmpIO%C3PSNI_TABLE   (CropType)
       noahmp%biochem%param%SlopeConductToPhotosyn  = NoahmpIO%MPI_TABLE      (CropType)
       noahmp%biochem%param%RespMaintQ10            = NoahmpIO%Q10MR_TABLE    (CropType)
       noahmp%biochem%param%RespMaintLeaf25C        = NoahmpIO%LFMR25_TABLE   (CropType)
       noahmp%biochem%param%RespMaintStem25C        = NoahmpIO%STMR25_TABLE   (CropType)
       noahmp%biochem%param%RespMaintRoot25C        = NoahmpIO%RTMR25_TABLE   (CropType)
       noahmp%biochem%param%GrowthRespFrac          = NoahmpIO%FRA_GR_TABLE   (CropType)
       noahmp%biochem%param%TemperaureLeafFreeze    = NoahmpIO%LEFREEZ_TABLE  (CropType)
       noahmp%biochem%param%LeafAreaPerBiomass      = NoahmpIO%BIO2LAI_TABLE  (CropType)
       noahmp%biochem%param%TempBaseGrowDegDay      = NoahmpIO%GDDTBASE_TABLE (CropType)
       noahmp%biochem%param%TempMaxGrowDegDay       = NoahmpIO%GDDTCUT_TABLE  (CropType)
       noahmp%biochem%param%GrowDegDayEmerg         = NoahmpIO%GDDS1_TABLE    (CropType)
       noahmp%biochem%param%GrowDegDayInitVeg       = NoahmpIO%GDDS2_TABLE    (CropType)
       noahmp%biochem%param%GrowDegDayPostVeg       = NoahmpIO%GDDS3_TABLE    (CropType)
       noahmp%biochem%param%GrowDegDayInitReprod    = NoahmpIO%GDDS4_TABLE    (CropType)
       noahmp%biochem%param%GrowDegDayMature        = NoahmpIO%GDDS5_TABLE    (CropType)
       noahmp%biochem%param%PhotosynRadFrac         = NoahmpIO%I2PAR_TABLE    (CropType)
       noahmp%biochem%param%TempMinCarbonAssim      = NoahmpIO%TASSIM0_TABLE  (CropType)
       noahmp%biochem%param%TempMaxCarbonAssim      = NoahmpIO%TASSIM1_TABLE  (CropType)
       noahmp%biochem%param%TempMaxCarbonAssimMax   = NoahmpIO%TASSIM2_TABLE  (CropType)
       noahmp%biochem%param%CarbonAssimRefMax       = NoahmpIO%AREF_TABLE     (CropType)
       noahmp%biochem%param%LightExtCoeff           = NoahmpIO%K_TABLE        (CropType)
       noahmp%biochem%param%LighUseEfficiency       = NoahmpIO%EPSI_TABLE     (CropType)
       noahmp%biochem%param%CarbonAssimReducFac     = NoahmpIO%PSNRF_TABLE    (CropType)
       noahmp%biochem%param%RespMaintGrain25C       = NoahmpIO%GRAINMR25_TABLE(CropType)
       noahmp%biochem%param%LeafDeathTempCoeffCrop  = NoahmpIO%DILE_FC_TABLE  (CropType,:)
       noahmp%biochem%param%LeafDeathWaterCoeffCrop = NoahmpIO%DILE_FW_TABLE  (CropType,:)
       noahmp%biochem%param%CarbohydrLeafToGrain    = NoahmpIO%LFCT_TABLE     (CropType,:)
       noahmp%biochem%param%CarbohydrStemToGrain    = NoahmpIO%STCT_TABLE     (CropType,:)
       noahmp%biochem%param%CarbohydrRootToGrain    = NoahmpIO%RTCT_TABLE     (CropType,:)
       noahmp%biochem%param%CarbohydrFracToLeaf     = NoahmpIO%LFPT_TABLE     (CropType,:)
       noahmp%biochem%param%CarbohydrFracToStem     = NoahmpIO%STPT_TABLE     (CropType,:)
       noahmp%biochem%param%CarbohydrFracToRoot     = NoahmpIO%RTPT_TABLE     (CropType,:)
       noahmp%biochem%param%CarbohydrFracToGrain    = NoahmpIO%GRAINPT_TABLE  (CropType,:)
       noahmp%biochem%param%TurnoverCoeffLeafCrop   = NoahmpIO%LF_OVRC_TABLE  (CropType,:)
       noahmp%biochem%param%TurnoverCoeffStemCrop   = NoahmpIO%ST_OVRC_TABLE  (CropType,:)
       noahmp%biochem%param%TurnoverCoeffRootCrop   = NoahmpIO%RT_OVRC_TABLE  (CropType,:)

       if ( OptCropModel == 1 ) then
          noahmp%biochem%param%DatePlanting         = NoahmpIO%PLANTING(I,J)
          noahmp%biochem%param%DateHarvest          = NoahmpIO%HARVEST(I,J)
          noahmp%biochem%param%GrowDegDayEmerg      = NoahmpIO%SEASON_GDD(I,J) / 1770.0 * &
                                                      noahmp%biochem%param%GrowDegDayEmerg
          noahmp%biochem%param%GrowDegDayInitVeg    = NoahmpIO%SEASON_GDD(I,J) / 1770.0 * &
                                                      noahmp%biochem%param%GrowDegDayInitVeg
          noahmp%biochem%param%GrowDegDayPostVeg    = NoahmpIO%SEASON_GDD(I,J) / 1770.0 * &
                                                      noahmp%biochem%param%GrowDegDayPostVeg
          noahmp%biochem%param%GrowDegDayInitReprod = NoahmpIO%SEASON_GDD(I,J) / 1770.0 * &
                                                      noahmp%biochem%param%GrowDegDayInitReprod
          noahmp%biochem%param%GrowDegDayMature     = NoahmpIO%SEASON_GDD(I,J) / 1770.0 * &
                                                      noahmp%biochem%param%GrowDegDayMature
        endif
    endif ! activate crop parameters

    if ( noahmp%config%nmlist%OptIrrigation == 2 ) then
       noahmp%biochem%param%DatePlanting = NoahmpIO%PLANTING(I,J)
       noahmp%biochem%param%DateHarvest  = NoahmpIO%HARVEST (I,J)
    endif
    
    end associate

  end subroutine BiochemVarInitTransfer

end module BiochemVarInitMod
