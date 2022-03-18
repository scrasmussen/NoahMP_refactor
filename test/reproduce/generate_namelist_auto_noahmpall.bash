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


###### test regular run (no crop, no irr, no tiledrain)
## 1. test veg & soil type combine for a few typical physics combination

# no snow
for dvegtp in 4 5 8 ; do
for crstp in 1 2 ; do
for radtp in 1 3 ; do

 snownum=1
 snowopt=.false.
 snftp=1

 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 ; do
       noahmp_namelist=namelist.build_noahmpall.sh
       namelist_output=namelist.input.regular.snow${snownum}.dveg${dvegtp}.crstp${crstp}.radtp${radtp}.soil${soiltp}.vege${vegetp}
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

# snow
for dvegtp in 4 5 8 ; do
for radtp in 1 3 ; do
for snftp in 1 5 ; do
for crstp in 1 2 ; do

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

 noahmp_namelist=namelist.build_noahmpall.sh
 namelist_output=namelist.input.phys.snow${snownum}.dveg${dvegtp}.btrtp${btrtp}.runtp${runtp}.sfctp${sfctp}.frztp${frztp}.inftp${inftp}.radtp${radtp}.tbottp${tbottp}.stctp${stctp}.rsftp${rsftp}.soil${soiltp}.vege${vegetp}
# Build namelist
. ${noahmp_namelist}
cat << EOF > ${namelist_output}
    $namelist_input
EOF
# Move namelist.input to the run directory
     mv -f ${namelist_output} ./namelists/${outdir}/

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


###### 4. test tiledrain
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
irrtp=0
irrmtp=0
snowopt=.false.
for tdrntp in 1 2 ; do
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 12 ; do

       noahmp_namelist=namelist.build_noahmpall.sh
       namelist_output=namelist.input.tdrntp${tdrntp}.soil${soiltp}.vege${vegetp}
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


###### 5. test crop
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
icroptp=1
irrtp=0
irrmtp=0
tdrntp=0
snowopt=.false.
soiltp=12
vegetp=12
for croptp in 1 2 3 4 5 ; do

       noahmp_namelist=namelist.build_noahmpall.sh
       namelist_output=namelist.input.crop.soil${soiltp}.croptp${croptp}
# Build namelist
. ${noahmp_namelist}
cat << EOF > ${namelist_output}
    $namelist_input
EOF
# Move namelist.input to the run directory
     mv -f ${namelist_output} ./namelists/${outdir}/

done


# End of script
 exit 0

