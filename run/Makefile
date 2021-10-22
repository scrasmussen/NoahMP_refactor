# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../user_build_options

OBJS = \
	../src/water_routines.o \
	../driver/water_output.o \
	../driver/water_driver.o

CMD = water.exe
all:	$(CMD)

water.exe: $(OBJS)
	$(COMPILERF90) -o $(@) -I../driver -I../src $(OBJS) $(NETCDFLIB)

# This command cleans up
clean:
	rm -f $(CMD)

