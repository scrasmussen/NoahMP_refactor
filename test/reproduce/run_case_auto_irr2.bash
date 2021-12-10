#!/bin/bash
#

 outdir=waterall_irri2_default
 runoff=3

 mkdir -p ./results/${outdir}
 rm -rf ./results/${outdir}/*

 mkdir workshop
 cd workshop
 cp ../../../run/GENPARM.TBL .
 cp ../../../run/MPTABLE.TBL .
 cp ../../../run/SOILPARM.TBL .
 cp ../../../run/noahmp.exe .

for irri in 1 2 3; do
for irrim in 0 1 2 3 ; do
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 12 ; do

       namelist_output=namelist.input.irr${irri}.irrm${irrim}.runoff${runoff}.soil${soiltp}.vege${vegetp}
       echo case.irr${irri}.irrm${irrim}.runoff${runoff}.soil${soiltp}.vege${vegetp}
       cp ../namelists/${outdir}/${namelist_output} namelist.input
       ./noahmp.exe
       mv output.nc ../results/${outdir}/output.nc.irr${irri}.irrm${irrim}.runoff${runoff}.soil${soiltp}.vege${vegetp}

    done # vegetp
 done # soiltp
done
done


  cd ..
  rm -r workshop

# End of script
 exit 0

