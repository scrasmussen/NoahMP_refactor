# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = \
	../src/carbon_routines.o \
	../driver/carbon_output.o \
	../driver/carbon_driver.o

CMD = carbon.exe
all:	$(CMD)

carbon.exe: $(OBJS)
	$(COMPILERF90) -o $(@) -I../driver -I../src $(OBJS) $(NETCDFLIB)

# This command cleans up
clean:
	rm -f $(CMD)

