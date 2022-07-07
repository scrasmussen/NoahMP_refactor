module BiochemVarInitMod

!!! Initialize column (1-D) Noah-MP biochemistry (carbon,nitrogen,etc) variables
!!! Biochemistry variables should be first defined in BiochemVarType.F90

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
  subroutine BiochemVarInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    associate( NumCropGrowStage => noahmp%config%domain%NumCropGrowStage )

    ! biochem state variables
    noahmp%biochem%state%PlantGrowStage            = undefined_int
    noahmp%biochem%state%IndexPlanting             = undefined_int
    noahmp%biochem%state%IndexHarvest              = undefined_int
    noahmp%biochem%state%IndexGrowSeason           = undefined_real
    noahmp%biochem%state%NitrogenConcFoliage       = undefined_real
    noahmp%biochem%state%LeafMass                  = undefined_real
    noahmp%biochem%state%RootMass                  = undefined_real
    noahmp%biochem%state%StemMass                  = undefined_real
    noahmp%biochem%state%WoodMass                  = undefined_real
    noahmp%biochem%state%CarbonMassDeepSoil        = undefined_real
    noahmp%biochem%state%CarbonMassShallowSoil     = undefined_real
    noahmp%biochem%state%CarbonMassSoilTot         = undefined_real
    noahmp%biochem%state%CarbonMassLiveTot         = undefined_real
    noahmp%biochem%state%LeafAreaPerMass           = undefined_real
    noahmp%biochem%state%StemAreaPerMass           = undefined_real
    noahmp%biochem%state%LeafMassMin               = undefined_real
    noahmp%biochem%state%StemMassMin               = undefined_real
    noahmp%biochem%state%CarbonFracToLeaf          = undefined_real
    noahmp%biochem%state%CarbonFracToRoot          = undefined_real
    noahmp%biochem%state%CarbonFracToWood          = undefined_real
    noahmp%biochem%state%CarbonFracToStem          = undefined_real
    noahmp%biochem%state%WoodCarbonFrac            = undefined_real
    noahmp%biochem%state%CarbonFracToWoodRoot      = undefined_real
    noahmp%biochem%state%MicroRespFactorSoilWater  = undefined_real
    noahmp%biochem%state%MicroRespFactorSoilTemp   = undefined_real
    noahmp%biochem%state%RespFacNitrogenFoliage    = undefined_real
    noahmp%biochem%state%RespFacTemperature        = undefined_real
    noahmp%biochem%state%RespReductionFac           = undefined_real
    noahmp%biochem%state%GrainMass                 = undefined_real
    noahmp%biochem%state%GrowDegreeDay             = undefined_real

    ! biochem flux variables
    noahmp%biochem%flux%PhotosynLeafSunlit         = undefined_real
    noahmp%biochem%flux%PhotosynLeafShade          = undefined_real
    noahmp%biochem%flux%PhotosynCrop               = undefined_real
    noahmp%biochem%flux%PhotosynTotal              = undefined_real
    noahmp%biochem%flux%GrossPriProduction         = undefined_real
    noahmp%biochem%flux%NetPriProductionTot        = undefined_real
    noahmp%biochem%flux%NetEcoExchange             = undefined_real
    noahmp%biochem%flux%RespirationPlantTot        = undefined_real
    noahmp%biochem%flux%RespirationSoilOrg         = undefined_real
    noahmp%biochem%flux%CarbonToAtmos              = undefined_real
    noahmp%biochem%flux%NetPriProductionLeaf       = undefined_real
    noahmp%biochem%flux%NetPriProductionRoot       = undefined_real
    noahmp%biochem%flux%NetPriProductionWood       = undefined_real
    noahmp%biochem%flux%NetPriProductionStem       = undefined_real
    noahmp%biochem%flux%GrowthRespLeaf             = undefined_real
    noahmp%biochem%flux%GrowthRespRoot             = undefined_real
    noahmp%biochem%flux%GrowthRespWood             = undefined_real
    noahmp%biochem%flux%GrowthRespStem             = undefined_real
    noahmp%biochem%flux%LeafMassMaxChg             = undefined_real
    noahmp%biochem%flux%StemMassMaxChg             = undefined_real
    noahmp%biochem%flux%CarbonDecayToStable        = undefined_real
    noahmp%biochem%flux%RespirationLeaf            = undefined_real
    noahmp%biochem%flux%RespirationStem            = undefined_real
    noahmp%biochem%flux%GrowthRespGrain            = undefined_real
    noahmp%biochem%flux%NetPriProductionGrain      = undefined_real
    noahmp%biochem%flux%ConvRootToGrain            = undefined_real
    noahmp%biochem%flux%ConvStemToGrain            = undefined_real
    noahmp%biochem%flux%RespirationWood            = undefined_real
    noahmp%biochem%flux%RespirationLeafMaint       = undefined_real
    noahmp%biochem%flux%RespirationRoot            = undefined_real
    noahmp%biochem%flux%DeathLeaf                  = undefined_real
    noahmp%biochem%flux%DeathStem                  = undefined_real
    noahmp%biochem%flux%CarbonAssim                = undefined_real
    noahmp%biochem%flux%TurnoverLeaf               = undefined_real
    noahmp%biochem%flux%TurnoverStem               = undefined_real
    noahmp%biochem%flux%TurnoverWood               = undefined_real
    noahmp%biochem%flux%RespirationSoil            = undefined_real
    noahmp%biochem%flux%TurnoverRoot               = undefined_real
    noahmp%biochem%flux%CarbonHydrateAssim         = undefined_real
    noahmp%biochem%flux%TurnoverGrain              = undefined_real
    noahmp%biochem%flux%ConvLeafToGrain            = undefined_real
    noahmp%biochem%flux%RespirationGrain           = undefined_real
 
    ! biochem parameter variables
    noahmp%biochem%param%DatePlanting    = undefined_int
    noahmp%biochem%param%DateHarvest    = undefined_int
    noahmp%biochem%param%PhotosynPath   = undefined_int
    noahmp%biochem%param%FOLNMX         = undefined_real
    noahmp%biochem%param%QE25           = undefined_real
    noahmp%biochem%param%VCMX25         = undefined_real
    noahmp%biochem%param%AVCMX          = undefined_real
    noahmp%biochem%param%C3PSN          = undefined_real
    noahmp%biochem%param%MP             = undefined_real
    noahmp%biochem%param%TMIN           = undefined_real
    noahmp%biochem%param%SLA            = undefined_real
    noahmp%biochem%param%FOLN_MX        = undefined_real
    noahmp%biochem%param%ARM            = undefined_real
    noahmp%biochem%param%RMF25          = undefined_real
    noahmp%biochem%param%RMS25          = undefined_real
    noahmp%biochem%param%RMR25          = undefined_real
    noahmp%biochem%param%WRRAT          = undefined_real
    noahmp%biochem%param%WDPOOL         = undefined_real
    noahmp%biochem%param%LTOVRC         = undefined_real
    noahmp%biochem%param%TDLEF          = undefined_real
    noahmp%biochem%param%DILEFW         = undefined_real
    noahmp%biochem%param%DILEFC         = undefined_real
    noahmp%biochem%param%FRAGR          = undefined_real
    noahmp%biochem%param%MRP            = undefined_real
    noahmp%biochem%param%Q10MR          = undefined_real
    noahmp%biochem%param%LFMR25         = undefined_real
    noahmp%biochem%param%STMR25         = undefined_real
    noahmp%biochem%param%RTMR25         = undefined_real
    noahmp%biochem%param%GRAINMR25      = undefined_real
    noahmp%biochem%param%FRA_GR         = undefined_real
    noahmp%biochem%param%LEFREEZ        = undefined_real
    noahmp%biochem%param%BIO2LAI        = undefined_real
    noahmp%biochem%param%TempBaseGrowDegDay = undefined_real
    noahmp%biochem%param%TempMaxGrowDegDay = undefined_real
    noahmp%biochem%param%GrowDegDayEmerg = undefined_real
    noahmp%biochem%param%GrowDegDayInitVeg   = undefined_real
    noahmp%biochem%param%GrowDegDayPostVeg  = undefined_real
    noahmp%biochem%param%GrowDegDayInitReprod = undefined_real
    noahmp%biochem%param%GrowDegDayMature = undefined_real
    noahmp%biochem%param%I2PAR          = undefined_real
    noahmp%biochem%param%TASSIM0        = undefined_real
    noahmp%biochem%param%TASSIM1        = undefined_real
    noahmp%biochem%param%TASSIM2        = undefined_real
    noahmp%biochem%param%CarbonAssimRefMax   = undefined_real
    noahmp%biochem%param%K              = undefined_real
    noahmp%biochem%param%EPSI           = undefined_real
    noahmp%biochem%param%CarbonAssimReducFac = undefined_real
    noahmp%biochem%param%SLAREA         = undefined_real
    noahmp%biochem%param%XSAMIN         = undefined_real
    noahmp%biochem%param%BF             = undefined_real
    noahmp%biochem%param%WSTRC          = undefined_real
    noahmp%biochem%param%LAIMIN         = undefined_real
    noahmp%biochem%param%RTOVRC         = undefined_real
    noahmp%biochem%param%RSDRYC         = undefined_real
    noahmp%biochem%param%RSWOODC        = undefined_real

    if( .not. allocated( noahmp%biochem%param%DILE_FC ) ) allocate( noahmp%biochem%param%DILE_FC (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%DILE_FW ) ) allocate( noahmp%biochem%param%DILE_FW (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%LFCT    ) ) allocate( noahmp%biochem%param%LFCT    (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%STCT    ) ) allocate( noahmp%biochem%param%STCT    (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%RTCT    ) ) allocate( noahmp%biochem%param%RTCT    (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%LFPT    ) ) allocate( noahmp%biochem%param%LFPT    (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%STPT    ) ) allocate( noahmp%biochem%param%STPT    (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%RTPT    ) ) allocate( noahmp%biochem%param%RTPT    (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%GRAINPT ) ) allocate( noahmp%biochem%param%GRAINPT (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%LF_OVRC ) ) allocate( noahmp%biochem%param%LF_OVRC (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%ST_OVRC ) ) allocate( noahmp%biochem%param%ST_OVRC (1:NumCropGrowStage) )
    if( .not. allocated( noahmp%biochem%param%RT_OVRC ) ) allocate( noahmp%biochem%param%RT_OVRC (1:NumCropGrowStage) )

    noahmp%biochem%param%DILE_FC(:)     = undefined_real
    noahmp%biochem%param%DILE_FW(:)     = undefined_real
    noahmp%biochem%param%LFCT(:)        = undefined_real
    noahmp%biochem%param%STCT(:)        = undefined_real
    noahmp%biochem%param%RTCT(:)        = undefined_real
    noahmp%biochem%param%LFPT(:)        = undefined_real
    noahmp%biochem%param%STPT(:)        = undefined_real
    noahmp%biochem%param%RTPT(:)        = undefined_real
    noahmp%biochem%param%GRAINPT(:)     = undefined_real
    noahmp%biochem%param%LF_OVRC(:)     = undefined_real
    noahmp%biochem%param%ST_OVRC(:)     = undefined_real
    noahmp%biochem%param%RT_OVRC(:)     = undefined_real

    end associate

  end subroutine BiochemVarInitDefault


!=== initialize with input data or table values
  subroutine BiochemVarInitTransfer(noahmp, NoahmpIO)

    implicit none

    type(noahmp_type),   intent(inout) :: noahmp
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

    associate(                                                  &
              I           => noahmp%config%domain%GridIndexI   ,&
              J           => noahmp%config%domain%GridIndexJ   ,&
              VegType     => noahmp%config%domain%VegType      ,&
              CropType    => noahmp%config%domain%CropType      &
             )

    ! biochem state variables
    noahmp%biochem%state%PlantGrowStage        = NoahmpIO%PGSXY   (I,J)   
    noahmp%biochem%state%LeafMass              = NoahmpIO%LFMASSXY(I,J)
    noahmp%biochem%state%RootMass              = NoahmpIO%RTMASSXY(I,J)
    noahmp%biochem%state%StemMass              = NoahmpIO%STMASSXY(I,J) 
    noahmp%biochem%state%WoodMass              = NoahmpIO%WOODXY  (I,J) 
    noahmp%biochem%state%CarbonMassDeepSoil    = NoahmpIO%STBLCPXY(I,J) 
    noahmp%biochem%state%CarbonMassShallowSoil = NoahmpIO%FASTCPXY(I,J)
    noahmp%biochem%state%GrainMass             = NoahmpIO%GRAINXY (I,J)  
    noahmp%biochem%state%GrowDegreeDay         = NoahmpIO%GDDXY   (I,J)  
    noahmp%biochem%state%NitrogenConcFoliage   = 1.0                    ! for now, set to nitrogen saturation

    ! biochem parameter variables
    noahmp%biochem%param%FOLNMX         = NoahmpIO%FOLNMX_TABLE (VegType)
    noahmp%biochem%param%QE25           = NoahmpIO%QE25_TABLE   (VegType)
    noahmp%biochem%param%VCMX25         = NoahmpIO%VCMX25_TABLE (VegType)
    noahmp%biochem%param%AVCMX          = NoahmpIO%AVCMX_TABLE  (VegType)
    noahmp%biochem%param%C3PSN          = NoahmpIO%C3PSN_TABLE  (VegType)
    noahmp%biochem%param%MP             = NoahmpIO%MP_TABLE     (VegType)
    noahmp%biochem%param%ARM            = NoahmpIO%ARM_TABLE    (VegType)
    noahmp%biochem%param%RMF25          = NoahmpIO%RMF25_TABLE  (VegType)
    noahmp%biochem%param%RMS25          = NoahmpIO%RMS25_TABLE  (VegType)
    noahmp%biochem%param%RMR25          = NoahmpIO%RMR25_TABLE  (VegType)
    noahmp%biochem%param%WRRAT          = NoahmpIO%WRRAT_TABLE  (VegType)
    noahmp%biochem%param%WDPOOL         = NoahmpIO%WDPOOL_TABLE (VegType)
    noahmp%biochem%param%LTOVRC         = NoahmpIO%LTOVRC_TABLE (VegType)
    noahmp%biochem%param%TDLEF          = NoahmpIO%TDLEF_TABLE  (VegType)
    noahmp%biochem%param%DILEFW         = NoahmpIO%DILEFW_TABLE (VegType)
    noahmp%biochem%param%DILEFC         = NoahmpIO%DILEFC_TABLE (VegType)
    noahmp%biochem%param%FRAGR          = NoahmpIO%FRAGR_TABLE  (VegType)
    noahmp%biochem%param%MRP            = NoahmpIO%MRP_TABLE    (VegType)
    noahmp%biochem%param%TMIN           = NoahmpIO%TMIN_TABLE   (VegType)
    noahmp%biochem%param%SLA            = NoahmpIO%SLA_TABLE    (VegType)
    noahmp%biochem%param%XSAMIN         = NoahmpIO%XSAMIN_TABLE (VegType)
    noahmp%biochem%param%BF             = NoahmpIO%BF_TABLE     (VegType)
    noahmp%biochem%param%WSTRC          = NoahmpIO%WSTRC_TABLE  (VegType)
    noahmp%biochem%param%LAIMIN         = NoahmpIO%LAIMIN_TABLE (VegType)
    noahmp%biochem%param%RTOVRC         = NoahmpIO%RTOVRC_TABLE (VegType)
    noahmp%biochem%param%RSDRYC         = NoahmpIO%RSDRYC_TABLE (VegType)
    noahmp%biochem%param%RSWOODC        = NoahmpIO%RSWOODC_TABLE(VegType)

    if ( CropType > 0 ) then
       noahmp%biochem%param%DatePlanting = NoahmpIO%PLTDAY_TABLE   (CropType)
       noahmp%biochem%param%DateHarvest  = NoahmpIO%HSDAY_TABLE    (CropType)
       noahmp%biochem%param%PhotosynPath = NoahmpIO%C3C4_TABLE     (CropType)
       noahmp%biochem%param%FOLNMX      = NoahmpIO%FOLNMXI_TABLE  (CropType)
       noahmp%biochem%param%QE25        = NoahmpIO%QE25I_TABLE    (CropType)
       noahmp%biochem%param%VCMX25      = NoahmpIO%VCMX25I_TABLE  (CropType)
       noahmp%biochem%param%AVCMX       = NoahmpIO%AVCMXI_TABLE   (CropType)
       noahmp%biochem%param%C3PSN       = NoahmpIO%C3PSNI_TABLE   (CropType)
       noahmp%biochem%param%MP          = NoahmpIO%MPI_TABLE      (CropType)
       noahmp%biochem%param%FOLN_MX     = NoahmpIO%FOLN_MX_TABLE  (CropType)
       noahmp%biochem%param%Q10MR       = NoahmpIO%Q10MR_TABLE    (CropType)
       noahmp%biochem%param%LFMR25      = NoahmpIO%LFMR25_TABLE   (CropType)
       noahmp%biochem%param%STMR25      = NoahmpIO%STMR25_TABLE   (CropType)
       noahmp%biochem%param%RTMR25      = NoahmpIO%RTMR25_TABLE   (CropType)
       noahmp%biochem%param%FRA_GR      = NoahmpIO%FRA_GR_TABLE   (CropType)
       noahmp%biochem%param%LEFREEZ     = NoahmpIO%LEFREEZ_TABLE  (CropType)
       noahmp%biochem%param%BIO2LAI     = NoahmpIO%BIO2LAI_TABLE  (CropType)
       noahmp%biochem%param%TempBaseGrowDegDay    = NoahmpIO%GDDTBASE_TABLE (CropType)
       noahmp%biochem%param%TempMaxGrowDegDay     = NoahmpIO%GDDTCUT_TABLE  (CropType)
       noahmp%biochem%param%GrowDegDayEmerg  = NoahmpIO%GDDS1_TABLE    (CropType)
       noahmp%biochem%param%GrowDegDayInitVeg = NoahmpIO%GDDS2_TABLE    (CropType)
       noahmp%biochem%param%GrowDegDayPostVeg = NoahmpIO%GDDS3_TABLE    (CropType)
       noahmp%biochem%param%GrowDegDayInitReprod = NoahmpIO%GDDS4_TABLE    (CropType)
       noahmp%biochem%param%GrowDegDayMature = NoahmpIO%GDDS5_TABLE    (CropType)
       noahmp%biochem%param%I2PAR       = NoahmpIO%I2PAR_TABLE    (CropType)
       noahmp%biochem%param%TASSIM0     = NoahmpIO%TASSIM0_TABLE  (CropType)
       noahmp%biochem%param%TASSIM1     = NoahmpIO%TASSIM1_TABLE  (CropType)
       noahmp%biochem%param%TASSIM2     = NoahmpIO%TASSIM2_TABLE  (CropType)
       noahmp%biochem%param%CarbonAssimRefMax = NoahmpIO%AREF_TABLE     (CropType)
       noahmp%biochem%param%K           = NoahmpIO%K_TABLE        (CropType)
       noahmp%biochem%param%EPSI        = NoahmpIO%EPSI_TABLE     (CropType)
       noahmp%biochem%param%CarbonAssimReducFac = NoahmpIO%PSNRF_TABLE    (CropType)
       noahmp%biochem%param%GRAINMR25   = NoahmpIO%GRAINMR25_TABLE(CropType)
       noahmp%biochem%param%DILE_FC     = NoahmpIO%DILE_FC_TABLE  (CropType,:)
       noahmp%biochem%param%DILE_FW     = NoahmpIO%DILE_FW_TABLE  (CropType,:)
       noahmp%biochem%param%LFCT        = NoahmpIO%LFCT_TABLE     (CropType,:)
       noahmp%biochem%param%STCT        = NoahmpIO%STCT_TABLE     (CropType,:)
       noahmp%biochem%param%RTCT        = NoahmpIO%RTCT_TABLE     (CropType,:)
       noahmp%biochem%param%LFPT        = NoahmpIO%LFPT_TABLE     (CropType,:)
       noahmp%biochem%param%STPT        = NoahmpIO%STPT_TABLE     (CropType,:)
       noahmp%biochem%param%RTPT        = NoahmpIO%RTPT_TABLE     (CropType,:)
       noahmp%biochem%param%GRAINPT     = NoahmpIO%GRAINPT_TABLE  (CropType,:)
       noahmp%biochem%param%LF_OVRC     = NoahmpIO%LF_OVRC_TABLE  (CropType,:)
       noahmp%biochem%param%ST_OVRC     = NoahmpIO%ST_OVRC_TABLE  (CropType,:)
       noahmp%biochem%param%RT_OVRC     = NoahmpIO%RT_OVRC_TABLE  (CropType,:)
    endif

    if((noahmp%config%nmlist%OptCropModel == 1) .and. (noahmp%config%domain%CropType > 0)) then
       noahmp%biochem%param%DatePlanting = NoahmpIO%PLANTING(I,J)
       noahmp%biochem%param%DateHarvest  = NoahmpIO%HARVEST (I,J)
       noahmp%biochem%param%GrowDegDayEmerg = NoahmpIO%SEASON_GDD(I,J) / 1770.0 * &
                                     noahmp%biochem%param%GrowDegDayEmerg
       noahmp%biochem%param%GrowDegDayInitVeg = NoahmpIO%SEASON_GDD(I,J) / 1770.0 * &
                                     noahmp%biochem%param%GrowDegDayInitVeg
       noahmp%biochem%param%GrowDegDayPostVeg = NoahmpIO%SEASON_GDD(I,J) / 1770.0 * &
                                     noahmp%biochem%param%GrowDegDayPostVeg
       noahmp%biochem%param%GrowDegDayInitReprod = NoahmpIO%SEASON_GDD(I,J) / 1770.0 * &
                                     noahmp%biochem%param%GrowDegDayInitReprod
       noahmp%biochem%param%GrowDegDayMature = NoahmpIO%SEASON_GDD(I,J) / 1770.0 * &
                                     noahmp%biochem%param%GrowDegDayMature
    end if

    if(noahmp%config%nmlist%OptIrrigation == 2) then
       noahmp%biochem%param%DatePlanting = NoahmpIO%PLANTING(I,J)
       noahmp%biochem%param%DateHarvest  = NoahmpIO%HARVEST (I,J)
    end if
    
    end associate

  end subroutine BiochemVarInitTransfer

end module BiochemVarInitMod
