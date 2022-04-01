#!/bin/bash
#
#  soiltp= 1~19 no 14
#  vegetp= 1~20 no 17

 outdir=Noahmp_glacier_refactor

 mkdir -p ./namelists/${outdir}
 rm -rf ./namelists/${outdir}/*


for albtp in 1 2 ; do
for snftp in 1 2 ; do
for tbottp in 1 2 ; do
for stctp in 1 2 ; do
for glatp in 1 2 ; do


 for soiltp  in 1 ; do
    for vegetp in 1  ; do
       noahmp_namelist=namelist.build_glacier.sh
       namelist_output=namelist.input.glacier.albtp${albtp}.snftp${snftp}.tbottp${tbottp}.stctp${stctp}.glatp${glatp}.soil${soiltp}.vege${vegetp}
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
done




# End of script
 exit 0

