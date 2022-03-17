#!/bin/bash
#
#  soiltp= 1~19 no 14
#  vegetp= 1~20 no 17

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

 mkdir -p ./namelists/${outdir}
 rm -rf ./namelists/${outdir}/*


# snow
for dvegtp in 4 5 8 ; do
for radtp in 1 3 ; do
for snftp in 1 2 3 4 5 ; do
for crstp in 1; do

 snownum=2
 snowopt=.true.

 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 ; do
       noahmp_namelist=namelist.build_noahmpall.sh
       namelist_output=namelist.input.regular.snow${snownum}.dveg${dvegtp}.crstp${crstp}.snftp${snftp}.radtp${radtp}.soil${soiltp}.vege${vegetp}
# Build namelist
. ${noahmp_namelist}
cat << EOF > ${namelist_output}
    $namelist_input
EOF
# Move namelist.input to the run directory
     mv -f ${namelist_output} ./namelists/${outdir}/
    done # vegetp
 done # soiltp

done
done
done
done



# End of script
 exit 0

