#!/bin/bash
#

 test1=EnerWat_phase_default
 test2=EnerWat_phase_refactor

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
for frztp in 1 2 ; do
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 ; do

       case_output=case.frz${frztp}.snow${snownum}.soil${soiltp}.vege${vegetp}

cat >> reports/$report_name << EOF

=====================================
=== ${case_output} ===
=====================================

EOF

       nccmp -d -S -f -q -s ${outdir1}/output.nc.frz${frztp}.snow${snownum}.soil${soiltp}.vege${vegetp}  ${outdir2}/output.nc.frz${frztp}.snow${snownum}.soil${soiltp}.vege${vegetp}  >& temp.txt
       cat temp.txt >> reports/$report_name

       rm temp.txt

    done # vegetp
 done # soiltp
done #frztp
done #snow


# End of script
 exit 0

