module BiochemCropMainMod

!!! Main Biogeochemistry module for dynamic crop (as opposed to natural vegetation)
!!! currently only include carbon processes (RE Dickinson et al.(1998) and Guo-Yue Niu(2004))
 
  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use CarbonFluxCropMod,     only : CarbonFluxCrop
  use CropGrowDegreeDayMod,  only : CropGrowDegreeDay
  use CropPhotosynthesisMod, only : CropPhotosynthesis
        
  implicit none
        
contains
     
  subroutine BiochemCropMain(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CARBON_CROP
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Modified by Xing Liu, 2014
! Refactered code: P. Valayamkunnath, C. He & refactor team (Nov 17, 2021)
! -------------------------------------------------------------------------
        
    implicit none
        
    type(noahmp_type), intent(inout) :: noahmp
    
! local variables
    integer                          :: J      ! loop index
    real(kind=kind_noahmp)           :: DB     ! thickness of canopy buried by snow (m) 
    
!-------------------------------------------------------------------------
    associate(                                                        &
              VegType          => noahmp%config%domain%VegType       ,& ! in,    vegetation type
              DepthSoilLayer            => noahmp%config%domain%DepthSoilLayer         ,& ! in,    depth [m] of layer-bottom from soil surface
              ThicknessSnowSoilLayer           => noahmp%config%domain%ThicknessSnowSoilLayer        ,& ! in,    snow/soil layer thickness [m]
              IndexWaterPoint          => noahmp%config%domain%IndexWaterPoint       ,& ! in,    water point flag
              IndexIcePoint            => noahmp%config%domain%IndexIcePoint         ,& ! in,    land ice flag
              IndexBarrenPoint         => noahmp%config%domain%IndexBarrenPoint      ,& ! in,    bare soil flag
              FlagUrban       => noahmp%config%domain%FlagUrban    ,& ! in,    urban point flag
              NROOT            => noahmp%water%param%NROOT           ,& ! in,    number of soil layers with root present
              SMCMAX           => noahmp%water%param%SMCMAX          ,& ! in,    saturated value of soil moisture [m3/m3]
              SoilMoisture      => noahmp%water%state%SoilMoisture   ,& ! in,    soil moisture (ice + liq.) [m3/m3]
              SoilTranspFacAcc            => noahmp%water%state%SoilTranspFacAcc           ,& ! in,    accumulated soil water transpiration factor (0 to 1)
              LeafMass           => noahmp%biochem%state%LeafMass        ,& ! inout, leaf mass [g/m2]
              RootMass           => noahmp%biochem%state%RootMass        ,& ! inout, mass of fine roots [g/m2]
              StemMass           => noahmp%biochem%state%StemMass        ,& ! inout, stem mass [g/m2]
              WoodMass             => noahmp%biochem%state%WoodMass          ,& ! inout, mass of wood (incl. woody roots) [g/m2]
              CarbonMassDeepSoil           => noahmp%biochem%state%CarbonMassDeepSoil        ,& ! inout, stable carbon in deep soil [g/m2]
              CarbonMassShallowSoil           => noahmp%biochem%state%CarbonMassShallowSoil        ,& ! inout, short-lived carbon in shallow soil [g/m2]
              XLAI             => noahmp%energy%state%LAI            ,& ! inout, leaf area index [-]
              XSAI             => noahmp%energy%state%SAI            ,& ! inout, stem area index [-]
              GrossPriProduction              => noahmp%biochem%flux%GrossPriProduction            ,& ! out,   net instantaneous assimilation [g/m2/s C]
              NetPriProductionTot              => noahmp%biochem%flux%NetPriProductionTot            ,& ! out,   net primary productivity [g/m2/s C]
              NetEcoExchange              => noahmp%biochem%flux%NetEcoExchange            ,& ! out,   net ecosystem exchange [g/m2/s CO2]
              RespirationPlantTot           => noahmp%biochem%flux%RespirationPlantTot         ,& ! out,   total plant respiration [g/m2/s C]
              RespirationSoilOrg           => noahmp%biochem%flux%RespirationSoilOrg         ,& ! out,   soil organic respiration [g/m2/s C]
              CarbonMassSoilTot            => noahmp%biochem%state%CarbonMassSoilTot         ,& ! out,   total soil carbon [g/m2 C]
              CarbonMassLiveTot            => noahmp%biochem%state%CarbonMassLiveTot         ,& ! out,   total living carbon ([g/m2 C]
              GrainMass            => noahmp%biochem%state%GrainMass         ,& ! out,   mass of grain [g/m2] 
              SoilWaterRootZone            => noahmp%water%state%SoilWaterRootZone           ,& ! out,   root zone soil water [-]
              SoilWaterStress           => noahmp%water%state%SoilWaterStress           & ! out,   water stress coeficient [-]  (1. for wilting)
             ) 
!------------------------------------------------------------------------

    ! initialize
    NetEcoExchange = 0.0
    NetPriProductionTot = 0.0
    GrossPriProduction = 0.0

    ! no biogeochemistry in non-vegetated points
    if ( (VegType == IndexWaterPoint) .or. (VegType == IndexBarrenPoint) .or. &
         (VegType == IndexIcePoint  ) .or. (FlagUrban .eqv. .true.) ) then
       XLAI   = 0.0
       XSAI   = 0.0
       GrossPriProduction    = 0.0
       NetPriProductionTot    = 0.0
       NetEcoExchange    = 0.0
       RespirationPlantTot = 0.0
       RespirationSoilOrg = 0.0
       CarbonMassSoilTot  = 0.0
       CarbonMassLiveTot  = 0.0
       LeafMass = 0.0
       RootMass = 0.0
       StemMass = 0.0
       WoodMass   = 0.0
       CarbonMassDeepSoil = 0.0
       CarbonMassShallowSoil = 0.0
       GrainMass  = 0.0
       return
    endif
    
    ! start biogeochemistry process

    ! water stress
    SoilWaterStress = 1.0 - SoilTranspFacAcc
    SoilWaterRootZone  = 0.0
    do J = 1, NROOT
       SoilWaterRootZone = SoilWaterRootZone + SoilMoisture(J) / SMCMAX(J) * ThicknessSnowSoilLayer(J) / (-DepthSoilLayer(NROOT))
    enddo

    ! start crop carbon process
    call CropPhotosynthesis(noahmp)
    call CropGrowDegreeDay(noahmp)
    call CarbonFluxCrop(noahmp)
    
    end associate
    
  end subroutine BiochemCropMain
    
end module BiochemCropMainMod
