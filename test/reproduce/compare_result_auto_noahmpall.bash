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

###### test regular run (no crop, no irr, no tiledrain)
## 1. test veg & soil type combine for a few typical physics combination

# no snow
for dvegtp in 4 5 8 ; do
for crstp in 1 2 ; do
for radtp in 1 3 ; do

 snownum=1
 snowopt=.false.
 snftp=1

 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 18 19 20 ; do

       case_output=case.regular.snow${snownum}.dveg${dvegtp}.crstp${crstp}.radtp${radtp}.soil${soiltp}.vege${vegetp}

cat >> reports/$report_name << EOF

=====================================
=== ${case_output} ===
=====================================

EOF

      filename1=${outdir1}/output.nc.regular.snow${snownum}.dveg${dvegtp}.crstp${crstp}.radtp${radtp}.soil${soiltp}.vege${vegetp}
      filename2=${outdir2}/output.nc.regular.snow${snownum}.dveg${dvegtp}.crstp${crstp}.radtp${radtp}.soil${soiltp}.vege${vegetp}
       nccmp -d -S -f -q -s ${filename1} ${filename2} >& temp.txt
       cat temp.txt >> reports/$report_name

       rm temp.txt

    done # vegetp
 done # soiltp

done
done
done

# snow
for dvegtp in 4 5 8 ; do
for radtp in 1 3 ; do
for snftp in 1 5 ; do
for crstp in 1 2 ; do

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
       nccmp -d -S -f -q -s ${filename1} ${filename2} >& temp.txt
       cat temp.txt >> reports/$report_name

       rm temp.txt

    done # vegetp
 done # soiltp

done
done
done
done

###### 3. test irrigation
dvegtp=4
crstp=1
btrtp=1
runtp=3
sfctp=1
frztp=1
inftp=1
radtp=3
albtp=1
snftp=1
tbottp=2
stctp=1
rsftp=1
icroptp=0
tdrntp=0
snowopt=.false.
for irrtp in 1 2 3 ; do
for irrmtp in 0 1 2 3 ; do
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 12 ; do

       case_output=case.irri.irrtp${irrtp}.irrmtp${irrmtp}.soil${soiltp}.vege${vegetp}

cat >> reports/$report_name << EOF

=====================================
=== ${case_output} ===
=====================================

EOF

      filename1=${outdir1}/output.nc.irri.irrtp${irrtp}.irrmtp${irrmtp}.soil${soiltp}.vege${vegetp}
      filename2=${outdir2}/output.nc.irri.irrtp${irrtp}.irrmtp${irrmtp}.soil${soiltp}.vege${vegetp}
       nccmp -d -S -f -q -s ${filename1} ${filename2} >& temp.txt
       cat temp.txt >> reports/$report_name

       rm temp.txt

    done # vegetp
 done # soiltp
done
done

###### 4. test tiledrain
dvegtp=4
crstp=1
btrtp=1
runtp=3
sfctp=1
frztp=1
inftp=1
radtp=3
albtp=1
snftp=1
tbottp=2
stctp=1
rsftp=1
icroptp=0
irrtp=0
irrmtp=0
snowopt=.false.
for tdrntp in 1 2 ; do
 for soiltp  in 1 2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 ; do
    for vegetp in 12 ; do

       case_output=case.tdrntp${tdrntp}.soil${soiltp}.vege${vegetp}

cat >> reports/$report_name << EOF

=====================================
=== ${case_output} ===
=====================================

EOF

      filename1=${outdir1}/output.nc.tdrntp${tdrntp}.soil${soiltp}.vege${vegetp}
      filename2=${outdir2}/output.nc.tdrntp${tdrntp}.soil${soiltp}.vege${vegetp}
       nccmp -d -S -f -q -s ${filename1} ${filename2} >& temp.txt
       cat temp.txt >> reports/$report_name

       rm temp.txt

    done # vegetp
 done # soiltp
done


###### 5. test crop
dvegtp=4
crstp=1
btrtp=1
runtp=3
sfctp=1
frztp=1
inftp=1
radtp=3
albtp=1
snftp=1
tbottp=2
stctp=1
rsftp=1
icroptp=1
irrtp=0
irrmtp=0
tdrntp=0
snowopt=.false.
soiltp=12
vegetp=12
for croptp in 1 2 3 4 5 ; do

       case_output=case.crop.soil${soiltp}.croptp${croptp}

cat >> reports/$report_name << EOF

=====================================
=== ${case_output} ===
=====================================

EOF

      filename1=${outdir1}/output.nc.crop.soil${soiltp}.croptp${croptp}
      filename2=${outdir2}/output.nc.crop.soil${soiltp}.croptp${croptp}
       nccmp -d -S -f -q -s ${filename1} ${filename2} >& temp.txt
       cat temp.txt >> reports/$report_name

       rm temp.txt

done




# End of script
 exit 0

