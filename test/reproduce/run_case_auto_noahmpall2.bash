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


## 2. test all physics combination for specific veg & soil types
# no snow
for dvegtp in 1 2 3 4 5 6 7 8 9; do
for btrtp in 1 2 3 ; do
for runtp in 1 2 3 4 5 6 7 8 ; do
for sfctp in 1 2 ; do
for frztp in 1 2 ; do
for inftp in 1 2 ; do
for radtp in 1 2 3 ; do
for tbottp in 1 2 ; do
for stctp in 1 2 3 ; do
for rsftp in 1 2 3 4 ; do

 snownum=1
 snowopt=.false.
 crstp=1
 snftp=1
 soiltp=12 #clay
 vegetp=1

 namelist_output=namelist.input.phys.snow${snownum}.dveg${dvegtp}.btrtp${btrtp}.runtp${runtp}.sfctp${sfctp}.frztp${frztp}.inftp${inftp}.radtp${radtp}.tbottp${tbottp}.stctp${stctp}.rsftp${rsftp}.soil${soiltp}.vege${vegetp}

       echo case.phys.snow${snownum}.dveg${dvegtp}.btrtp${btrtp}.runtp${runtp}.sfctp${sfctp}.frztp${frztp}.inftp${inftp}.radtp${radtp}.tbottp${tbottp}.stctp${stctp}.rsftp${rsftp}.soil${soiltp}.vege${vegetp}
       cp /glade/work/cenlinhe/NoahMP_refact/namelists_refactor/${outdir}/${namelist_output} namelist.input
       ./noahmp_refac.exe
       mv output.nc ../results/${outdir}/output.nc.phys.snow${snownum}.dveg${dvegtp}.btrtp${btrtp}.runtp${runtp}.sfctp${sfctp}.frztp${frztp}.inftp${inftp}.radtp${radtp}.tbottp${tbottp}.stctp${stctp}.rsftp${rsftp}.soil${soiltp}.vege${vegetp}

done
done
done
done
done
done
done
done
done
done


  cd ..
  rm -r workshop

# End of script
 exit 0

