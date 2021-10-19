# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = water_output.o water_driver.o

all:	$(OBJS)

water_driver.o: water_driver.f90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ $(*).f90 

water_output.o: water_output.f90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ $(*).f90 

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod

#
# Dependencies:
#

water_output.o: ../src/WaterType.o \
                ../src/EnergyType.o 
water_driver.o: water_output.o \
                        ../src/NamelistRead.o \
			../src/LevelsType.o \
			../src/DomainType.o \
			../src/OptionsType.o \
			../src/ParametersType.o \
                        ../src/ForcingType.o \
                        ../src/EnergyType.o \
			../src/WaterType.o \
                        ../src/WaterModule.o \
                        ../src/CanopyWaterModule.o \
                        ../src/SnowWaterModule.o   \
                        ../src/SnowWaterRenew.o \
                        ../src/SnowLayerChange.o \
			../src/SoilWaterModule.o \
			../src/SoilWaterRetentionCoeff.o \
                        ../src/SurfaceRunoffInfiltration.o \
			../src/SurfaceRunoffModule.o \
			../src/SoilWaterMovement.o \
			../src/SubsurfaceRunoffModule.o

