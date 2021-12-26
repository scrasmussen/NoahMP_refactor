#!/bin/bash
#
#  soiltp= 1~19 no 14
#  vegetp= 1~20 no 17

 snowopt=.false.
 
 outdir=EnerWat_vegflux_default

 mkdir -p ./namelists/${outdir}
 rm -rf ./namelists/${outdir}/*

for snownum in 1 2 ; do
 if [ $snownum -eq 2 ]; then
    snowopt=.true.
 fi

for crstp in 1 2 ; do
for sfctp in 1 2 ; do
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 ; do
       croptp=0
       noahmp_namelist=namelist.build_vegflux.sh
       namelist_output=namelist.input.crs${crstp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}.crop${croptp}

# Build namelist
. ${noahmp_namelist}
cat << EOF > ${namelist_output}
    $namelist_input
EOF

# Move namelist.input to the run directory
     mv -f ${namelist_output} ./namelists/${outdir}/

    done # vegetp
 done # soiltp
done #crstp
done #sfctp
done #snow


for crstp in 1 2 ; do
for sfctp in 1 2 ; do
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for croptp in 1 2 3 4 5 ; do
       vegetp=12
       snowopt=.false.
       snownum=1
       noahmp_namelist=namelist.build_vegflux.sh
       namelist_output=namelist.input.crs${crstp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}.crop${croptp}

# Build namelist
. ${noahmp_namelist}
cat << EOF > ${namelist_output}
    $namelist_input
EOF

# Move namelist.input to the run directory
     mv -f ${namelist_output} ./namelists/${outdir}/

    done # croptp
 done # soiltp
done #crstp
done #sfctp





# End of script
 exit 0

