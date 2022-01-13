# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../user_build_options

OBJS = BiochemOutput.o BiochemDriver.o

all:	$(OBJS)

.F90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) -I../src -I../utility $(NETCDFMOD) $(*).F90

clean:
	rm -f *.o *.mod *.exe

#
# Dependencies:
#

BiochemOutput.o: ../utility/Machine.o \
		../src/NoahmpVarType.o \
		../src/InputVarType.o

BiochemDriver.o: ../utility/Machine.o \
		   ../src/NoahmpVarType.o \
		   ../src/InputVarType.o \
		   ../src/ConstantDefineMod.o \
		   ../src/InputVarInitMod.o \
                   ../src/ConfigVarInitMod.o \
                   ../src/ForcingVarInitMod.o \
                   ../src/EnergyVarInitMod.o \
                   ../src/WaterVarInitMod.o \
                   ../src/BiochemVarInitMod.o \
                   ../src/PhenologyMainMod.o \
                   BiochemOutput.o

