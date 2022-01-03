# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = phenology_output.o phenology_driver.o

all:	$(OBJS)

phenology_driver.o: phenology_driver.f90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ $(*).f90 

phenology_output.o: phenology_output.f90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) $(*).f90 

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod

#
# Dependencies:
#

phenology_driver.o: phenology_output.o ../src/phenology_routines.o 

