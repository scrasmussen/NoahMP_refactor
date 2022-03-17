#!/bin/bash
#

 test1=Noahmp_all_default4
 test2=Noahmp_all_refactor4

 outdir1=/glade/scratch/cenlinhe/NoahMP_refactor/test_results/${test1}
 outdir2=/glade/scratch/cenlinhe/NoahMP_refactor/test_results/${test2}
 report_name=difference_report_${test1}_${test2}_snf

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

# snow
for dvegtp in 4 5 8 ; do
for radtp in 1 3 ; do
for snftp in 1 2 3 4 5 ; do
for crstp in 1 ; do

 snownum=2
 snowopt=.true.

 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 ; do

       case_output=case.regular.snow${snownum}.dveg${dvegtp}.crstp${crstp}.snftp${snftp}.radtp${radtp}.soil${soiltp}.vege${vegetp}

cat >> reports/$report_name << EOF

=====================================
=== ${case_output} ===
=====================================

EOF

      filename1=${outdir1}/output.nc.regular.snow${snownum}.dveg${dvegtp}.crstp${crstp}.snftp${snftp}.radtp${radtp}.soil${soiltp}.vege${vegetp}
      filename2=${outdir2}/output.nc.regular.snow${snownum}.dveg${dvegtp}.crstp${crstp}.snftp${snftp}.radtp${radtp}.soil${soiltp}.vege${vegetp}
       nccmp -d -S -f -q -s ${filename1} ${filename2} >& temp4.txt
       cat temp4.txt >> reports/$report_name

       rm temp4.txt

    done # vegetp
 done # soiltp

done
done
done
done


# End of script
 exit 0

