# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../user_build_options

OBJS = ConstantDefineMod.o\
       ConfigType.o\
       EnergyType.o\
       ForcingType.o\
       InputType.o\
       WaterType.o\
       NoahmpType.o\
       ConfigInitMod.o\
       EnergyInitMod.o\
       ForcingInitMod.o\
       InputInitMod.o\
       WaterInitMod.o\
       AtmosForcingMod.o
               

all:	$(OBJS)

.F90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) -I../utility $(NETCDFMOD) $(*).F90

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod

#
# Dependencies:
#

ConstantDefineMod.o: ../utility/Machine.o
ConfigType.o: ../utility/Machine.o
EnergyType.o: ../utility/Machine.o
ForcingType.o: ../utility/Machine.o
InputType.o: ../utility/Machine.o
WaterType.o: ../utility/Machine.o
NoahmpType.o: ConfigType.o EnergyType.o ForcingType.o InputType.o WaterType.o
ConfigInitMod.o: NoahmpType.o InputType.o
EnergyInitMod.o: NoahmpType.o InputType.o
ForcingInitMod.o: NoahmpType.o InputType.o
InputInitMod.o: ../utility/Machine.o InputType.o
WaterInitMod.o: NoahmpType.o InputType.o
AtmosForcingMod.o: ../utility/Machine.o NoahmpType.o ConstantDefineMod.o
 
