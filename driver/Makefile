# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../user_build_options

OBJS = water_output.o water_driver.o

all:	$(OBJS)

water_driver.o: water_driver.F90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ $(*).F90 

water_output.o: water_output.F90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) $(*).F90 

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod

#
# Dependencies:
#

water_driver.o: water_output.o ../src/water_routines.o 

