# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../user_build_options

OBJS = InputVarType.o \
       ConfigVarType.o \
       ForcingVarType.o \
       EnergyVarType.o \
       WaterVarType.o \
       BiochemVarType.o \
       NoahmpVarType.o \
       InputVarInitMod.o \
       ConfigVarInitMod.o \
       ForcingVarInitMod.o \
       EnergyVarInitMod.o \
       WaterVarInitMod.o \
       BiochemVarInitMod.o \
       PhenologyMainMod.o  \
       CarbonAssimilationFluxMod.o \
       CarbonAssimilationFluxCropMod.o \
       PhotosynthesisCropMod.o \
       GrowingDegreeDaysCropMod.o \
       BiochemMainMod.o \
       BiochemCropMainMod.o      

all:	$(OBJS)

.F90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) -I../utility $(NETCDFMOD) $(*).F90

clean:
	rm -f *.o *.mod *.exe

#
# Dependencies:
#

InputVarType.o:         ../utility/Machine.o
ConfigVarType.o:        ../utility/Machine.o
ForcingVarType.o:       ../utility/Machine.o
EnergyVarType.o:        ../utility/Machine.o
WaterVarType.o:         ../utility/Machine.o
BiochemVarType.o:       ../utility/Machine.o
NoahmpVarType.o:        ConfigVarType.o ForcingVarType.o EnergyVarType.o \
                        WaterVarType.o BiochemVarType.o
InputVarInitMod.o:      ../utility/Machine.o InputVarType.o
ConfigVarInitMod.o:     InputVarType.o NoahmpVarType.o
ForcingVarInitMod.o:    InputVarType.o NoahmpVarType.o
EnergyVarInitMod.o:     InputVarType.o NoahmpVarType.o
WaterVarInitMod.o:      InputVarType.o NoahmpVarType.o
BiochemVarInitMod.o:    InputVarType.o NoahmpVarType.o
PhenologyMainMod.o:     ../utility/Machine.o NoahmpVarType.o 
CarbonAssimilationFluxMod.o: ../utility/Machine.o NoahmpVarType.o
CarbonAssimilationFluxCropMod.o: ../utility/Machine.o NoahmpVarType.o
PhotosynthesisCropMod.o: ../utility/Machine.o NoahmpVarType.o
GrowingDegreeDaysCropMod.o: ../utility/Machine.o NoahmpVarType.o
BiochemMainMod.o: ../utility/Machine.o NoahmpVarType.o CarbonAssimilationFluxMod.o
BiochemCropMainMod.o: ../utility/Machine.o NoahmpVarType.o CarbonAssimilationFluxCropMod.o \
                      GrowingDegreeDaysCropMod.o PhotosynthesisCropMod.o

