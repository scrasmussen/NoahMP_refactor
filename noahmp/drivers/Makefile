# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F90

include ../../hrldas/user_build_options

OBJS = NoahmpGroundwaterInitMod.o \
       NoahmpSnowInitMod.o \
       NoahmpInitMainMod.o \
       NoahmpDriverMainMod.o 

all:	$(OBJS)

.F90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) -I. -I../src -I../utility -I../../hrldas/Utility_routines \
         -I ../../wrf $(NETCDFMOD) $(*).F90

clean:
	rm -f *.o *.mod *.exe

#
# Dependencies:
#
  
NoahmpGroundwaterInitMod.o:../../hrldas/Utility_routines/module_domain.o \
                            ../utility/Machine.o \
                            ../src/NoahmpIOVarType.o \
                            ../src/module_sf_noahmp_groundwater.o 
                             
NoahmpSnowInitMod.o:../utility/Machine.o \
                    ../src/NoahmpIOVarType.o

NoahmpInitMainMod.o:../utility/Machine.o \
                    ../src/NoahmpIOVarType.o \
                    ./NoahmpSnowInitMod.o \
                    ../../hrldas/Utility_routines/module_wrf_utilities.o                      

NoahmpDriverMainMod.o:../utility/Machine.o \
                      ../src/NoahmpVarType.o \
                      ../src/NoahmpIOVarType.o \
                      ../src/ConstantDefineMod.o \
                      ../src/NoahmpIOVarInitMod.o \
                      ../src/ConfigVarInitMod.o \
                      ../src/EnergyVarInitMod.o \
                      ../src/ForcingVarInitMod.o \
                      ../src/WaterVarInitMod.o \
                      ../src/BiochemVarInitMod.o \
                      ../src/NoahmpMainMod.o \
                      ../src/NoahmpMainGlacierMod.o \
                      ../src/ConfigVarOutMod.o \
                      ../src/EnergyVarOutMod.o \
                      ../src/WaterVarOutMod.o \
                      ../src/BiochemVarOutMod.o \
                      ../../hrldas/Utility_routines/module_ra_gfdleta.o \
                      ../../wrf/module_sf_urban.o 
