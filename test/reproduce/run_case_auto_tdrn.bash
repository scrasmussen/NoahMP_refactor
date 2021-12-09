#!/bin/bash
#
#  soiltp= 1~19 no 14
#  vegetp= 1~20 no 17
#  runoff= 1~8 no 5
# infdv=1~3 only for runoff=8

 snowopt=.false.
 irri=0
 irrim=0
 runoff=3

 outdir=waterall_tdrn_refactor

 mkdir -p ./results/${outdir}
 rm -rf ./results/${outdir}/*

 mkdir workshop
 cd workshop
 cp ../../../run/NoahmpTable.TBL .
 cp ../../../run/water_refac.exe .

for tdrn in 1 2 ; do 
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 12 ; do

       namelist_output=namelist.input.tdrn${tdrn}.runoff${runoff}.soil${soiltp}.vege${vegetp}
       echo case.tdrn${tdrn}.runoff${runoff}.soil${soiltp}.vege${vegetp}
       cp ../namelists/${outdir}/${namelist_output} namelist.input
       ./water_refac.exe
       mv output.nc ../results/${outdir}/output.nc.tdrn${tdrn}.runoff${runoff}.soil${soiltp}.vege${vegetp}

    done # vegetp
 done # soiltp
done # runoff

  cd ..
  rm -r workshop

# End of script
 exit 0

