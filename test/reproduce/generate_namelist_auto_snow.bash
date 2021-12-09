#!/bin/bash
#
#  soiltp= 1~19 no 14
#  vegetp= 1~20 no 17
#  runoff= 1~8 no 5
# infdv=1~3 only for runoff=8

 snowopt=.true.
 irri=0
 irrim=0
 tdrn=0

 outdir=waterall_snow_refactor

 mkdir -p ./namelists/${outdir}
 rm -rf ./namelists/${outdir}/*

for runoff in 1 2 3 4 6 7 ; do 
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 ; do
       infdv=1
       noahmp_namelist=namelist.build.sh
       namelist_output=namelist.input.runoff${runoff}.soil${soiltp}.vege${vegetp}

# Build namelist
. ${noahmp_namelist}
cat << EOF > ${namelist_output}
    $namelist_input
EOF
# Move namelist.input to the run directory
     mv -f ${namelist_output} ./namelists/${outdir}/

    done # vegetp
 done # soiltp
done # runoff

for runoff in 8 ; do
for infdv in 1 2 3 ; do  
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 ; do
       noahmp_namelist=namelist.build.sh
       namelist_output=namelist.input.runoff${runoff}.DV${infdv}.soil${soiltp}.vege${vegetp}

# Build namelist
. ${noahmp_namelist}
cat << EOF > ${namelist_output}
    $namelist_input
EOF
# Move namelist.input to the run directory
     mv -f ${namelist_output} ./namelists/${outdir}/

    done # vegetp
 done # soiltp
done # infdv
done # runoff


# End of script
 exit 0

