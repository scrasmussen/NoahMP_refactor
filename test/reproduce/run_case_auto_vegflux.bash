#!/bin/bash
#

 outdir=EnerWat_vegflux_default

 mkdir -p ./results/${outdir}
 rm -rf ./results/${outdir}/*

 mkdir workshop
 cd workshop
 cp ../../../run/GENPARM.TBL .
 cp ../../../run/MPTABLE.TBL .
 cp ../../../run/SOILPARM.TBL .
 cp ../../../run/noahmp.exe .

for snownum in 1 2 ; do
for crstp in 1 2 ; do
for sfctp in 1 2 ; do
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 ; do
       croptp=0
       namelist_output=namelist.input.crs${crstp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}.crop${croptp}
       echo case.crs${crstp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}.crop${croptp}
       cp ../namelists/${outdir}/${namelist_output} namelist.input
       ./noahmp.exe
       mv output.nc ../results/${outdir}/output.nc.crs${crstp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}.crop${croptp}

    done # vegetp
 done # soiltp
done #crstp
done #sfctp
done #snow


for crstp in 1 2 ; do
for sfctp in 1 2 ; do
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for croptp in 1 2 3 4 5 ; do
       vegetp=12
       snowopt=.false.
       snownum=1

       namelist_output=namelist.input.crs${crstp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}.crop${croptp}
       echo case.crs${crstp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}.crop${croptp}
       cp ../namelists/${outdir}/${namelist_output} namelist.input
       ./noahmp.exe
       mv output.nc ../results/${outdir}/output.nc.crs${crstp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}.crop${croptp}

    done # croptp
 done # soiltp
done #crstp
done #sfctp


  cd ..
  rm -r workshop

# End of script
 exit 0

