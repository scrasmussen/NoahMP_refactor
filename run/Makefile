# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = \
        ../utility/Machine.o \
        ../src/NoahmpVarType.o \
        ../src/InputVarType.o \
        ../src/InputVarInitMod.o \
        ../src/ConfigVarInitMod.o \
        ../src/EnergyVarInitMod.o \
        ../src/WaterVarInitMod.o \
        ../src/BiochemVarInitMod.o \
        ../src/PhenologyMainMod.o \
	../driver/phenology_output.o \
	../driver/phenology_driver.o

CMD = phenology.exe
all:	$(CMD)

phenology.exe: $(OBJS)
	$(COMPILERF90) -o $(@) -I../driver -I../src $(OBJS) $(NETCDFLIB)

# This command cleans up
clean:
	rm -f $(CMD)

