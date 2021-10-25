# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

all: user_build_options
	(cd utility;            make)
	(cd src;		make)
	(cd driver;		make)
	(cd run;		make)

clean:
	(cd utility;make clean)
	(cd src;		make clean)
	(cd driver;		make clean)
	(cd run;		make clean)
