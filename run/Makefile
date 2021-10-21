# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = \
	../src/forcing_routines.o \
	../driver/forcing_output.o \
	../driver/forcing_driver.o

CMD = forcing.exe
all:	$(CMD)

forcing.exe: $(OBJS)
	$(COMPILERF90) -o $(@) -I../driver -I../src $(OBJS) $(NETCDFLIB)

# This command cleans up
clean:
	rm -f $(CMD)

