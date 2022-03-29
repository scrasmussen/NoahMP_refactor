# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../user_build_options

OBJS = \
	../src/module_sf_noahmplsm.o \
        ../src/module_sf_noahmp_glacier.o \
	../driver/noahmp_output.o \
	../driver/noahmp_driver.o

CMD = noahmp.exe
all:	$(CMD)

noahmp.exe: $(OBJS)
	$(COMPILERF90) -o $(@) $(F90FLAGS) $(FREESOURCE) \
        -I../driver -I../src $(NETCDFMOD) $(OBJS) $(NETCDFLIB)

# This command cleans up
clean:
	rm -f *.exe

