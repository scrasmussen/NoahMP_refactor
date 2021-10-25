# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../user_build_options

OBJS =\
       ../driver/forcing_driver.o\
       ../driver/forcing_output.o\
       ../src/ConstantDefineMod.o\
       ../src/ConfigType.o\
       ../src/EnergyType.o\
       ../src/ForcingType.o\
       ../src/InputType.o\
       ../src/WaterType.o\
       ../src/NoahmpType.o\
       ../src/ConfigInitMod.o\
       ../src/EnergyInitMod.o\
       ../src/ForcingInitMod.o\
       ../src/InputInitMod.o\
       ../src/WaterInitMod.o\
       ../src/AtmosForcingMod.o\
       ../utility/Machine.o


CMD = forcing.exe
all:	$(CMD)

forcing.exe: $(OBJS)
	$(COMPILERF90) -o $(@) -I../driver -I../src -I../utility $(OBJS) $(NETCDFLIB)

# This command cleans up
clean:
	rm -f $(CMD)

