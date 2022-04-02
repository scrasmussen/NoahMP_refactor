#!/bin/bash
#

 test1=Noahmp_glacier_default
 test2=Noahmp_glacier_refactor

 outdir1=/glade/scratch/cenlinhe/NoahMP_refactor/test_results/${test1}
 outdir2=/glade/scratch/cenlinhe/NoahMP_refactor/test_results/${test2}
 report_name=difference_report_${test1}_${test2}

 mkdir -p reports/

 if [ -e reports/$report_name ]; then
    rm -rf reports/$report_name
 fi

 cat > reports/$report_name << EOF
==============================
Output differences from simulation
$test1
compared to:
$test2
==============================

EOF


for albtp in 1 2 ; do
for snftp in 1 2 ; do
for tbottp in 1 2 ; do
for stctp in 1 2 ; do
for glatp in 1 2 ; do


# for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
#    for vegetp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 ; do
  for soiltp in 1 ; do
  for vegetp in 1 ; do

       case_output=case.glacier.albtp${albtp}.snftp${snftp}.tbottp${tbottp}.stctp${stctp}.glatp${glatp}.soil${soiltp}.vege${vegetp}
cat >> reports/$report_name << EOF

=====================================
=== ${case_output} ===
=====================================

EOF

      filename1=${outdir1}/output.nc.glacier.albtp${albtp}.snftp${snftp}.tbottp${tbottp}.stctp${stctp}.glatp${glatp}.soil${soiltp}.vege${vegetp}
      filename2=${outdir2}/output.nc.glacier.albtp${albtp}.snftp${snftp}.tbottp${tbottp}.stctp${stctp}.glatp${glatp}.soil${soiltp}.vege${vegetp}
       nccmp -d -S -f -q -s ${filename1} ${filename2} >& temp.txt
       cat temp.txt >> reports/$report_name

       rm temp.txt

    done # vegetp
 done # soiltp

done
done
done
done
done


# End of script
 exit 0

