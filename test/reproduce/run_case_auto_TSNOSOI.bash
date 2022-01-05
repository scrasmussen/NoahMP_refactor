#!/bin/bash
#

 outdir=EnerWat_TSNOSOI_default

 mkdir -p ./results/${outdir}
 rm -rf ./results/${outdir}/*

 mkdir workshop
 cd workshop
 cp ../../../run/GENPARM.TBL .
 cp ../../../run/MPTABLE.TBL .
 cp ../../../run/SOILPARM.TBL .
 cp ../../../run/noahmp.exe .

for snownum in 1 2 ; do
for stctp in 1 3 ; do
for sfctp in 1 2 ; do
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 ; do
       namelist_output=namelist.input.stc${stctp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}
       echo case.stc${stctp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}
       cp ../namelists/${outdir}/${namelist_output} namelist.input
       ./noahmp.exe
       mv output.nc ../results/${outdir}/output.nc.stc${stctp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}

    done # vegetp
 done # soiltp
done #stctp
done #sfctp
done #snow

  cd ..
  rm -r workshop

# End of script
 exit 0

