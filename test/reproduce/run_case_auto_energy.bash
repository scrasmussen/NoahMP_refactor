#!/bin/bash
#

 outdir=EnerWat_all_refactor

 mkdir -p ./results/${outdir}
 rm -rf ./results/${outdir}/*

 mkdir workshop
 cd workshop

 cp ../../../run/NoahmpTable.TBL .
 cp ../../../run/noahmp_refac.exe .

for snownum in 1 2 ; do
for btrtp in 1 2 3 ; do
for stctp in 1 2 ; do
for rsftp in 1 2 3 4 ; do

 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 ; do
       
       namelist_output=namelist.input.snow${snownum}.btr${btrtp}.stc${stctp}.rsf${rsftp}.soil${soiltp}.vege${vegetp}
       echo case.snow${snownum}.btr${btrtp}.stc${stctp}.rsf${rsftp}.soil${soiltp}.vege${vegetp}
       cp ../namelists/${outdir}/${namelist_output} namelist.input
       ./noahmp_refac.exe
       mv output.nc ../results/${outdir}/output.nc.snow${snownum}.btr${btrtp}.stc${stctp}.rsf${rsftp}.soil${soiltp}.vege${vegetp}

    done # vegetp
 done # soiltp

done #rsftp
done # stctp
done #btrtp
done #snow

  cd ..
  rm -r workshop

# End of script
 exit 0

