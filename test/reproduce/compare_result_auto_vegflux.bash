#!/bin/bash
#

 test1=EnerWat_vegflux_default
 test2=EnerWat_vegflux_refactor

 outdir1=/glade/work/cenlinhe/NoahMP_refact/test_results_new/${test1}
 outdir2=/glade/work/cenlinhe/NoahMP_refact/test_results_new/${test2}
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

for snownum in 1 2 ; do
for crstp in 1 2 ; do
for sfctp in 1 2 ; do
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 ; do
       croptp=0
       case_output=case.crs${crstp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}.crop${croptp}

cat >> reports/$report_name << EOF

=====================================
=== ${case_output} ===
=====================================

EOF

       nccmp -d -S -f -q -s ${outdir1}/output.nc.crs${crstp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}.crop${croptp}  ${outdir2}/output.nc.crs${crstp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}.crop${croptp}  >& temp.txt
       cat temp.txt >> reports/$report_name

       rm temp.txt

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

       case_output=case.crs${crstp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}.crop${croptp}

cat >> reports/$report_name << EOF

=====================================
=== ${case_output} ===
=====================================

EOF

       nccmp -d -S -f -q -s ${outdir1}/output.nc.crs${crstp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}.crop${croptp}  ${outdir2}/output.nc.crs${crstp}.sfc${sfctp}.snow${snownum}.soil${soiltp}.vege${vegetp}.crop${croptp}  >& temp.txt
       cat temp.txt >> reports/$report_name

       rm temp.txt


    done # croptp
 done # soiltp
done #crstp
done #sfctp



# End of script
 exit 0

