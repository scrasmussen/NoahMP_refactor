#!/bin/bash
#

 snowopt=.false.
#  soiltp= 1~19 no 14
#  vegetp= 1~20 no 17
#  runoff= 1~8 no 5
 irri=0
 irrim=0
# infdv=1~3 only for runoff=8
 tdrn=0
 outdir=waterall_base_default

 mkdir -p ./results/${outdir}
 rm -rf ./results/${outdir}/*

 mkdir workshop
 cd workshop
 cp ../../../run/GENPARM.TBL .
 cp ../../../run/MPTABLE.TBL .
 cp ../../../run/SOILPARM.TBL .
 cp ../../../run/water.exe .

for runoff in 1 2 3 4 6 7 ; do 
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 ; do
       namelist_output=namelist.input.runoff${runoff}.soil${soiltp}.vege${vegetp}

       echo case.runoff${runoff}.soil${soiltp}.vege${vegetp}

       cp ../namelists/${outdir}/${namelist_output} namelist.input
       ./water.exe
       mv output.nc ../results/${outdir}/output.nc.runoff${runoff}.soil${soiltp}.vege${vegetp}

    done # vegetp
 done # soiltp
done # runoff

for runoff in 8 ; do
for infdv in 1 2 3 ; do  
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 ; do
       namelist_output=namelist.input.runoff${runoff}.DV${infdv}.soil${soiltp}.vege${vegetp}

       echo case.runoff${runoff}.DV${infdv}.soil${soiltp}.vege${vegetp}

       cp ../namelists/${outdir}/${namelist_output} namelist.input
       ./water.exe
       mv output.nc ../results/${outdir}/output.nc.runoff${runoff}.DV${infdv}.soil${soiltp}.vege${vegetp}

    done # vegetp
 done # soiltp
done # infdv
done # runoff

#  cd ..
#  rm -rf workshop

# End of script
 exit 0

