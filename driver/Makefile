# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../user_build_options

OBJS = noahmp_output.o noahmp_driver.o

all:	$(OBJS)

.F90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) -I../src $(NETCDFMOD) $(*).F90

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod *.exe

#
# Dependencies:
#

noahmp_driver.o: noahmp_output.o \
                 ../src/module_sf_noahmplsm.o 

