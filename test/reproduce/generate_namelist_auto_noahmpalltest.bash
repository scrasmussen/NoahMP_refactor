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
 
 outdir=Noahmp_all_refactor2

 mkdir -p ./namelists/${outdir}
 rm -rf ./namelists/${outdir}/*



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

       noahmp_namelist=namelist.build_noahmpall_norain.sh
       namelist_output=namelist.input.irri.irrtp${irrtp}.irrmtp${irrmtp}.soil${soiltp}.vege${vegetp}
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




# End of script
 exit 0

