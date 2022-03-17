#!/bin/bash
#

dvegtp=1
crstp=1
btrtp=1
runtp=1
infdvtp=1
sfctp=1
frztp=1
inftp=1
radtp=1
albtp=1
snftp=1
tbottp=2
stctp=1
rsftp=1
icroptp=0
irrtp=0
irrmtp=0
tdrntp=0

snowopt=.false.
croptp=0
soiltp=1
vegetp=1

 outdir=Noahmp_all_refactor

 mkdir -p ./results/${outdir}
 rm -rf ./results/${outdir}/*

 mkdir workshop
 cd workshop

 cp ../../../run/NoahmpTable.TBL .
 cp ../../../run/noahmp_refac.exe .


###### 3. test irrigation
dvegtp=4
crstp=1
btrtp=1
runtp=3
sfctp=1
frztp=1
inftp=1
radtp=3
albtp=1
snftp=1
tbottp=2
stctp=1
rsftp=1
icroptp=0
tdrntp=0
snowopt=.false.
for irrtp in 1 2 3 ; do
for irrmtp in 0 1 2 3 ; do
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 12 ; do

       namelist_output=namelist.input.irri.irrtp${irrtp}.irrmtp${irrmtp}.soil${soiltp}.vege${vegetp}
       echo case.irri.irrtp${irrtp}.irrmtp${irrmtp}.soil${soiltp}.vege${vegetp}
       cp ../namelists/${outdir}/${namelist_output} namelist.input
       ./noahmp_refac.exe
       mv output.nc ../results/${outdir}/output.nc.irri.irrtp${irrtp}.irrmtp${irrmtp}.soil${soiltp}.vege${vegetp}

    done # vegetp
 done # soiltp
done
done




  cd ..
  rm -r workshop

# End of script
 exit 0

