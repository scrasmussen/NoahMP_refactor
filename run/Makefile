# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../user_build_options

OBJS = ../driver/WaterDriverMod.o \
       ../driver/WaterOutputMod.o \
       ../utility/Machine.o \
       ../utility/ErrorHandleMod.o \
       ../src/ConstantDefineMod.o \
       ../src/InputVarType.o \
       ../src/ConfigVarType.o \
       ../src/ForcingVarType.o \
       ../src/EnergyVarType.o \
       ../src/WaterVarType.o \
       ../src/NoahmpVarType.o \
       ../src/InputVarInitMod.o \
       ../src/ConfigVarInitMod.o \
       ../src/ForcingVarInitMod.o \
       ../src/EnergyVarInitMod.o \
       ../src/WaterVarInitMod.o \
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
       ../src/WaterTableEquilibriumMod.o


CMD = water_refac.exe
all:	$(CMD)

water_refac.exe: $(OBJS)
	$(COMPILERF90) -o $(@) $(F90FLAGS) $(FREESOURCE) \
        -I../driver -I../utility -I../src $(NETCDFMOD) $(OBJS) $(NETCDFLIB)

# This command cleans up
clean:
	rm -f *.exe

