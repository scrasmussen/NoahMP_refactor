# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../user_build_options

OBJS = ConstantDefineMod.o \
       InputVarType.o \
       ConfigVarType.o \
       ForcingVarType.o \
       EnergyVarType.o \
       WaterVarType.o \
       NoahmpVarType.o \
       InputVarInitMod.o \
       ConfigVarInitMod.o \
       ForcingVarInitMod.o \
       EnergyVarInitMod.o \
       WaterVarInitMod.o \
       CanopyHydrologyMod.o \
       GroundWaterTopModelMod.o \
       IrrigationFloodMod.o \
       IrrigationInfilPhilipMod.o \
       IrrigationMicroMod.o \
       MatrixSolverTriDiagonalMod.o \
       RunoffSubSurfaceDrainageMod.o \
       RunoffSubSurfaceEquiWaterTableMod.o \
       RunoffSubSurfaceGroundWaterMod.o \
       RunoffSubSurfaceShallowMmfMod.o \
       RunoffSurfaceBatsMod.o \
       RunoffSurfaceDynamicVicMod.o \
       RunoffSurfaceExcessDynamicVicMod.o \
       RunoffSurfaceFreeDrainMod.o \
       RunoffSurfaceTopModelEquiMod.o \
       RunoffSurfaceTopModelGrdMod.o \
       RunoffSurfaceTopModelMmfMod.o \
       RunoffSurfaceVicMod.o \
       RunoffSurfaceXinAnJiangMod.o \
       ShallowWaterTableMmfMod.o \
       SnowfallBelowCanopyMod.o \
       SnowLayerCombineMod.o \
       SnowLayerDivideMod.o \
       SnowLayerWaterComboMod.o \
       SnowpackCompactionMod.o \
       SnowpackHydrologyMod.o \
       SnowWaterMainMod.o \
       SoilHydraulicPropertyMod.o \
       SoilMoistureSolverMod.o \
       SoilWaterDiffusionRichardsMod.o \
       SoilWaterInfilGreenAmptMod.o \
       SoilWaterInfilPhilipMod.o \
       SoilWaterInfilSmithParlangeMod.o \
       SoilWaterMainMod.o \
       TileDrainageEquiDepthMod.o \
       TileDrainageHooghoudtMod.o \
       TileDrainageSimpleMod.o \
       WaterMainMod.o \
       WaterTableDepthSearchMod.o \
       WaterTableEquilibriumMod.o

all:	$(OBJS)

.F90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) -I../utility $(NETCDFMOD) $(*).F90

clean:
	rm -f *.o *.mod *.exe

#
# Dependencies:
#

ConstantDefineMod.o:    ../utility/Machine.o
InputVarType.o:         ../utility/Machine.o
ConfigVarType.o:        ../utility/Machine.o
ForcingVarType.o:       ../utility/Machine.o
EnergyVarType.o:        ../utility/Machine.o
WaterVarType.o:         ../utility/Machine.o
NoahmpVarType.o:        ConfigVarType.o ForcingVarType.o EnergyVarType.o WaterVarType.o
InputVarInitMod.o:      ../utility/Machine.o ../utility/ErrorHandleMod InputVarType.o
ConfigVarInitMod.o:     InputVarType.o NoahmpVarType.o
ForcingVarInitMod.o:    InputVarType.o NoahmpVarType.o
EnergyVarInitMod.o:     InputVarType.o NoahmpVarType.o
WaterVarInitMod.o:      InputVarType.o NoahmpVarType.o
CanopyHydrologyMod.o:   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
GroundWaterTopModelMod.o:   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
IrrigationFloodMod.o:   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                        IrrigationInfilPhilipMod.o
IrrigationInfilPhilipMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                             SoilHydraulicPropertyMod.o
IrrigationMicroMod.o:   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                        IrrigationInfilPhilipMod.o
MatrixSolverTriDiagonalMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
RunoffSubSurfaceDrainageMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
RunoffSubSurfaceEquiWaterTableMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                      WaterTableEquilibriumMod.o
RunoffSubSurfaceGroundWaterMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                   GroundWaterTopModelMod.o
RunoffSubSurfaceShallowMmfMod.o:   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                   ShallowWaterTableMmfMod.o
RunoffSurfaceBatsMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
RunoffSurfaceDynamicVicMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                               SoilWaterInfilPhilipMod.o RunoffSurfaceExcessDynamicVicMod.o \
                               SoilWaterInfilSmithParlangeMod.o SoilWaterInfilGreenAmptMod.o
RunoffSurfaceExcessDynamicVicMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
RunoffSurfaceFreeDrainMod.o:   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                               SoilHydraulicPropertyMod.o
RunoffSurfaceTopModelEquiMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
RunoffSurfaceTopModelGrdMod.o:   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
RunoffSurfaceTopModelMmfMod.o:   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
RunoffSurfaceVicMod.o:   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
RunoffSurfaceXinAnJiangMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
ShallowWaterTableMmfMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SnowfallBelowCanopyMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SnowLayerCombineMod.o:   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                         SnowLayerWaterComboMod.o
SnowLayerDivideMod.o:    ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                         SnowLayerWaterComboMod.o
SnowLayerWaterComboMod.o:   ../utility/Machine.o ConstantDefineMod.o
SnowpackCompactionMod.o:   ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SnowpackHydrologyMod.o:    ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                           SnowLayerCombineMod.o
SnowWaterMainMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                     SnowfallBelowCanopyMod.o SnowpackCompactionMod.o SnowLayerDivideMod.o \
                     SnowLayerCombineMod.o SnowpackHydrologyMod.o
SoilHydraulicPropertyMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
SoilMoistureSolverMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                          MatrixSolverTriDiagonalMod.o
SoilWaterDiffusionRichardsMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                  SoilHydraulicPropertyMod.o
SoilWaterInfilGreenAmptMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                               SoilHydraulicPropertyMod.o
SoilWaterInfilPhilipMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                            SoilHydraulicPropertyMod.o
SoilWaterInfilSmithParlangeMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                                   SoilHydraulicPropertyMod.o
SoilWaterMainMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                     RunoffSurfaceTopModelGrdMod.o RunoffSurfaceTopModelEquiMod.o \
                     RunoffSurfaceFreeDrainMod.o RunoffSurfaceBatsMod.o \
                     RunoffSurfaceTopModelMmfMod.o RunoffSurfaceVicMod.o \
                     RunoffSurfaceXinAnJiangMod.o RunoffSurfaceDynamicVicMod.o \
                     RunoffSubSurfaceEquiWaterTableMod.o RunoffSubSurfaceGroundWaterMod.o \
                     RunoffSubSurfaceDrainageMod.o RunoffSubSurfaceShallowMmfMod.o \
                     SoilWaterDiffusionRichardsMod.o SoilMoistureSolverMod.o \
                     TileDrainageSimpleMod.o TileDrainageHooghoudtMod.o
TileDrainageEquiDepthMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
TileDrainageHooghoudtMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                             TileDrainageEquiDepthMod.o WaterTableDepthSearchMod.o \
                             WaterTableEquilibriumMod.o
TileDrainageSimpleMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
WaterMainMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o \
                 CanopyHydrologyMod.o SnowWaterMainMod.o IrrigationFloodMod.o \
                 IrrigationMicroMod.o SoilWaterMainMod.o
WaterTableDepthSearchMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o
WaterTableEquilibriumMod.o:  ../utility/Machine.o NoahmpVarType.o ConstantDefineMod.o

