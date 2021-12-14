# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../user_build_options

OBJS = NoahmpDriverMod.o NoahmpOutputMod.o

all:	$(OBJS)

.F90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) -I../src -I../utility $(NETCDFMOD) $(*).F90

clean:
	rm -f *.o *.mod *.exe

#
# Dependencies:
#

NoahmpOutputMod.o: ../utility/Machine.o \
                  ../src/NoahmpVarType.o \
                  ../src/InputVarType.o

NoahmpDriverMod.o: ../utility/Machine.o \
                  ../src/NoahmpVarType.o \
                  ../src/InputVarType.o \
                  ../src/ConstantDefineMod.o \
                  ../src/InputVarInitMod.o \
                  ../src/ConfigVarInitMod.o \
                  ../src/EnergyVarInitMod.o \
                  ../src/ForcingVarInitMod.o \
                  ../src/WaterVarInitMod.o \
                  ../src/WaterMainMod.o \
                  ../src/IrrigationTriggerMod \
                  ../src/IrrigationSprinklerMod \
                  NoahmpOutputMod.o

