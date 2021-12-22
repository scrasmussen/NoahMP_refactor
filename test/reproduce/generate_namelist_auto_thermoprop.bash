#!/bin/bash
#
#  soiltp= 1~19 no 14
#  vegetp= 1~20 no 17

 snowopt=.false.
 
 outdir=EnerWat_thermoprop_refactor

 mkdir -p ./namelists/${outdir}
 rm -rf ./namelists/${outdir}/*

for snownum in 1 2 ; do
 if [ $snownum -eq 2 ]; then
    snowopt=.true.
 fi

 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 ; do
       noahmp_namelist=namelist.build_thermprop.sh
       namelist_output=namelist.input.snow${snownum}.soil${soiltp}.vege${vegetp}

# Build namelist
. ${noahmp_namelist}
cat << EOF > ${namelist_output}
    $namelist_input
EOF

# Move namelist.input to the run directory
     mv -f ${namelist_output} ./namelists/${outdir}/

    done # vegetp
 done # soiltp
done # snow


# End of script
 exit 0

