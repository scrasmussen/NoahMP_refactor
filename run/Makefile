# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = \
	../src/phenology_routines.o \
	../driver/phenology_output.o \
	../driver/phenology_driver.o

CMD = phenology.exe
all:	$(CMD)

phenology.exe: $(OBJS)
	$(COMPILERF90) -o $(@) -I../driver -I../src $(OBJS) $(NETCDFLIB)

# This command cleans up
clean:
	rm -f $(CMD)

