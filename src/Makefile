# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../user_build_options

OBJS = InputVarType.o \
       ConfigVarType.o \
       EnergyVarType.o \
       WaterVarType.o \
       BiochemVarType.o \
       NoahmpVarType.o \
       InputVarInitMod.o \
       ConfigVarInitMod.o \
       EnergyVarInitMod.o \
       WaterVarInitMod.o \
       BiochemVarInitMod.o \
       PhenologyMainMod.o       

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
EnergyVarType.o:        ../utility/Machine.o
WaterVarType.o:         ../utility/Machine.o
BiochemVarType.o:       ../utility/Machine.o
NoahmpVarType.o:        ConfigVarType.o EnergyVarType.o \
                        WaterVarType.o BiochemVarType.o
InputVarInitMod.o:      ../utility/Machine.o InputVarType.o
ConfigVarInitMod.o:     InputVarType.o NoahmpVarType.o
EnergyVarInitMod.o:     InputVarType.o NoahmpVarType.o
WaterVarInitMod.o:      InputVarType.o NoahmpVarType.o
BiochemVarInitMod.o:    InputVarType.o NoahmpVarType.o
PhenologyMainMod.o:     ../utility/Machine.o NoahmpVarType.o 


