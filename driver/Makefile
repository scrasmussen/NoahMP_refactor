# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../user_build_options

OBJS = forcing_output.o\
       forcing_driver.o

all:	$(OBJS)

.F90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../utility -I../src/ -I./ $(*).F90 

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod

#
# Dependencies:
#

forcing_output.o:../src/NoahmpType.o
forcing_driver.o: forcing_output.o ../src/AtmosForcingMod.o ../utility/Machine.o ../src/NoahmpType.o ../src/ConfigInitMod.o
