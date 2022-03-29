# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../user_build_options

OBJS = module_sf_noahmplsm.o \
       module_sf_noahmp_glacier.o

all:	$(OBJS)

.F90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) $(*).F90

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod

#
# Dependencies:
#

