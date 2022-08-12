# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../../hrldas/user_build_options

OBJS = NoahmpGroundwaterInitMod.o \
       NoahmpSnowInitMod.o \
       NoahmpInitMainMod.o \
       NoahmpDriverMainMod.o \
       NoahmpIOVarType.o \
       NoahmpIOVarInitMod.o

all:	$(OBJS)

.F90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) -I. -I../src -I../utility -I../../hrldas/Utility_routines \
        $(NETCDFMOD) $(*).F90

clean:
	rm -f *.o *.mod *.exe

#
# Dependencies:
#

NoahmpIOVarType.o:          ../utility/Machine.o
NoahmpIOVarInitMod.o:       ../utility/Machine.o NoahmpIOVarType.o
NoahmpGroundwaterInitMod.o: ../../hrldas/Utility_routines/module_domain.o \
                            ../utility/Machine.o NoahmpIOVarType.o ../src/GroundWaterMmfMod.o 
NoahmpSnowInitMod.o:        ../utility/Machine.o NoahmpIOVarType.o
NoahmpInitMainMod.o:        ../utility/Machine.o NoahmpIOVarType.o NoahmpSnowInitMod.o \
                            ../../hrldas/Utility_routines/module_wrf_utilities.o
NoahmpDriverMainMod.o:      ../utility/Machine.o ../src/NoahmpVarType.o NoahmpIOVarType.o \
                            ../src/ConstantDefineMod.o ../src/ConfigVarInitMod.o \
                            ../src/EnergyVarInitMod.o ../src/ForcingVarInitMod.o \
                            ../src/WaterVarInitMod.o ../src/BiochemVarInitMod.o \
                            ../src/NoahmpMainMod.o ../src/NoahmpMainGlacierMod.o \
                            ../src/ConfigVarOutMod.o ../src/EnergyVarOutMod.o \
                            ../src/WaterVarOutMod.o ../src/BiochemVarOutMod.o \
                            ../../hrldas/Utility_routines/module_ra_gfdleta.o
