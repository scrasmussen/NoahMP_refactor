# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../user_build_options

OBJS = ../driver/NoahmpDriverMod.o \
       ../driver/NoahmpOutputMod.o \
       ../utility/Machine.o \
       ../utility/CheckNanMod.o \
       ../src/ConstantDefineMod.o \
       ../src/InputVarType.o \
       ../src/ConfigVarType.o \
       ../src/ForcingVarType.o \
       ../src/EnergyVarType.o \
       ../src/WaterVarType.o \
       ../src/BiochemVarType.o \
       ../src/NoahmpVarType.o \
       ../src/InputVarInitMod.o \
       ../src/ConfigVarInitMod.o \
       ../src/ForcingVarInitMod.o \
       ../src/EnergyVarInitMod.o \
       ../src/WaterVarInitMod.o \
       ../src/BiochemVarInitMod.o \
       ../src/CanopyHydrologyMod.o \
       ../src/GroundWaterTopModelMod.o \
       ../src/IrrigationFloodMod.o \
       ../src/IrrigationInfilPhilipMod.o \
       ../src/IrrigationMicroMod.o \
       ../src/MatrixSolverTriDiagonalMod.o \
       ../src/RunoffSubSurfaceDrainageMod.o \
       ../src/RunoffSubSurfaceEquiWaterTableMod.o \
       ../src/RunoffSubSurfaceGroundWaterMod.o \
       ../src/RunoffSubSurfaceShallowMmfMod.o \
       ../src/RunoffSurfaceBatsMod.o \
       ../src/RunoffSurfaceDynamicVicMod.o \
       ../src/RunoffSurfaceExcessDynamicVicMod.o \
       ../src/RunoffSurfaceFreeDrainMod.o \
       ../src/RunoffSurfaceTopModelEquiMod.o \
       ../src/RunoffSurfaceTopModelGrdMod.o \
       ../src/RunoffSurfaceTopModelMmfMod.o \
       ../src/RunoffSurfaceVicMod.o \
       ../src/RunoffSurfaceXinAnJiangMod.o \
       ../src/ShallowWaterTableMmfMod.o \
       ../src/SnowfallBelowCanopyMod.o \
       ../src/SnowLayerCombineMod.o \
       ../src/SnowLayerDivideMod.o \
       ../src/SnowLayerWaterComboMod.o \
       ../src/SnowpackCompactionMod.o \
       ../src/SnowpackHydrologyMod.o \
       ../src/SnowWaterMainMod.o \
       ../src/SoilHydraulicPropertyMod.o \
       ../src/SoilMoistureSolverMod.o \
       ../src/SoilWaterDiffusionRichardsMod.o \
       ../src/SoilWaterInfilGreenAmptMod.o \
       ../src/SoilWaterInfilPhilipMod.o \
       ../src/SoilWaterInfilSmithParlangeMod.o \
       ../src/SoilWaterMainMod.o \
       ../src/TileDrainageEquiDepthMod.o \
       ../src/TileDrainageHooghoudtMod.o \
       ../src/TileDrainageSimpleMod.o \
       ../src/WaterMainMod.o \
       ../src/WaterTableDepthSearchMod.o \
       ../src/WaterTableEquilibriumMod.o \
       ../src/IrrigationTriggerMod.o \
       ../src/IrrigationSprinklerMod.o \
       ../src/CanopyWaterInterceptMod.o \
       ../src/PrecipitationHeatAdvectMod.o \
       ../src/SnowThermalPropertyMod.o \
       ../src/SoilThermalPropertyMod.o \
       ../src/GroundThermalPropertyMod.o \
       ../src/EnergyMainMod.o \
       ../src/NoahmpMainMod.o \
       ../src/CanopyRadiationTwoStreamMod.o \
       ../src/GroundAlbedoMod.o \
       ../src/RadiationMainMod.o \
       ../src/SnowAgingBatsMod.o \
       ../src/SnowAlbedoBatsMod.o \
       ../src/SnowAlbedoClassMod.o \
       ../src/SurfaceAlbedoMod.o \
       ../src/SurfaceRadiationMod.o \
       ../src/HumiditySaturationMod.o \
       ../src/ResistanceAboveCanopyChen97Mod.o \
       ../src/ResistanceAboveCanopyMostMod.o \
       ../src/ResistanceCanopyStomataBallBerryMod.o \
       ../src/ResistanceCanopyStomataJarvisMod.o \
       ../src/ResistanceLeafToGroundMod.o \
       ../src/VaporPressureSaturationMod.o \
       ../src/SurfaceEnergyFluxVegetatedMod.o \
       ../src/ResistanceBareGroundChen97Mod.o \
       ../src/ResistanceBareGroundMostMod.o \
       ../src/SurfaceEnergyFluxBareGroundMod.o \
       ../src/SoilSnowTemperatureMainMod.o \
       ../src/SoilSnowTemperatureSolverMod.o \
       ../src/SoilSnowThermalDiffusionMod.o \
       ../src/SoilSnowWaterPhaseChangeMod.o \
       ../src/SoilWaterSupercoolLiquidMod.o \
       ../src/SnowCoverGroundNiu07Mod.o \
       ../src/GroundRoughnessPropertyMod.o \
       ../src/SurfaceEmissivityMod.o \
       ../src/PsychrometricVariableMod.o \
       ../src/ResistanceGroundEvaporationMod.o \
       ../src/SoilWaterTranspirationMod.o \
       ../src/AtmosForcingMod.o \
       ../src/PhenologyMainMod.o \
       ../src/BiochemCropMainMod.o \
       ../src/BiochemNatureVegMainMod.o \
       ../src/CarbonFluxCropMod.o \
       ../src/CarbonFluxNatureVegMod.o \
       ../src/CropGrowDegreeDayMod.o \
       ../src/CropPhotosynthesisMod.o \
       ../src/IrrigationPrepareMod.o \
       ../src/BalanceErrorCheckMod.o \
       ../src/GeneralInitMod.o

CMD = noahmp_refac.exe
all:	$(CMD)

noahmp_refac.exe: $(OBJS)
	$(COMPILERF90) -o $(@) $(F90FLAGS) $(FREESOURCE) \
        -I../driver -I../utility -I../src $(NETCDFMOD) $(OBJS) $(NETCDFLIB)

# This command cleans up
clean:
	rm -f *.exe

