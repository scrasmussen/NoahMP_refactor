#!/bin/bash
#
#  soiltp= 1~19 no 14
#  vegetp= 1~20 no 17

 snowopt=.false.
 tdrn=0
 runoff=3
 infdv=1
 
 outdir=waterall_irri2_default

 mkdir -p ./namelists/${outdir}
 rm -rf ./namelists/${outdir}/*

for irri in 1 2 3; do
for irrim in 0 1 2 3 ; do
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 12 ; do
       noahmp_namelist=namelist.build_irr_norain.sh
       namelist_output=namelist.input.irr${irri}.irrm${irrim}.runoff${runoff}.soil${soiltp}.vege${vegetp}

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

