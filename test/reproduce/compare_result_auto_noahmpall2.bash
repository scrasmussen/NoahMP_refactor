#!/bin/bash
#

 test1=Noahmp_all_default
 test2=Noahmp_all_refactor

 outdir1=/glade/scratch/cenlinhe/NoahMP_refactor/test_results/${test1}
 outdir2=/glade/scratch/cenlinhe/NoahMP_refactor/test_results/${test2}
 report_name=difference_report_${test1}_${test2}

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

       case_output=case.phys.snow${snownum}.dveg${dvegtp}.btrtp${btrtp}.runtp${runtp}.sfctp${sfctp}.frztp${frztp}.inftp${inftp}.radtp${radtp}.tbottp${tbottp}.stctp${stctp}.rsftp${rsftp}.soil${soiltp}.vege${vegetp}

cat >> reports/$report_name << EOF

=====================================
=== ${case_output} ===
=====================================

EOF

      filename1=${outdir1}/output.nc.phys.snow${snownum}.dveg${dvegtp}.btrtp${btrtp}.runtp${runtp}.sfctp${sfctp}.frztp${frztp}.inftp${inftp}.radtp${radtp}.tbottp${tbottp}.stctp${stctp}.rsftp${rsftp}.soil${soiltp}.vege${vegetp}
      filename2=${outdir2}/output.nc.phys.snow${snownum}.dveg${dvegtp}.btrtp${btrtp}.runtp${runtp}.sfctp${sfctp}.frztp${frztp}.inftp${inftp}.radtp${radtp}.tbottp${tbottp}.stctp${stctp}.rsftp${rsftp}.soil${soiltp}.vege${vegetp}
       nccmp -d -S -f -q -s ${filename1} ${filename2} >& temp.txt
       cat temp.txt >> reports/$report_name

       rm temp.txt

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



# End of script
 exit 0

