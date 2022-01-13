# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../user_build_options

OBJS = \
        ../utility/Machine.o \
        ../src/NoahmpVarType.o \
        ../src/InputVarType.o \
        ../src/InputVarInitMod.o \
        ../src/ConfigVarInitMod.o \
        ../src/ForcingVarInitMod.o \
        ../src/EnergyVarInitMod.o \
        ../src/WaterVarInitMod.o \
        ../src/BiochemVarInitMod.o \
        ../src/PhenologyMainMod.o \
        ../src/CarbonAssimilationFluxMod.o \
        ../src/CarbonAssimilationFluxCropMod.o \
        ../src/PhotosynthesisCropMod.o \
        ../src/GrowingDegreeDaysCropMod.o \
        ../src/BiochemMainMod.o \
        ../src/BiochemCropMainMod.o\
	../driver/BiochemOutput.o \
	../driver/BiochemDriver.o

CMD = BiochemDriver.exe
all:	$(CMD)

BiochemDriver.exe: $(OBJS)
	$(COMPILERF90) -o $(@) -I../driver -I../src $(OBJS) $(NETCDFLIB)

# This command cleans up
clean:
	rm -f $(CMD)

