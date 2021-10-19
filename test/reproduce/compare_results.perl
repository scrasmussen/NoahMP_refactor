#!/usr/bin/perl

$test1  = "waterall_addrunoff_baseline_opt81"    ; # Directory where results are located
$test2  = "waterall_addrunoff_refact_opt81";

$results_dir1 = "/glade/work/cenlinhe/NoahMP_refact/test_results/$test1";
$results_dir2 = "/glade/work/cenlinhe/NoahMP_refact/test_results/$test2";

#system("cp -r ./results/$test2 /glade/work/cenlinhe/NoahMP_refact/test_results/");

# @combos = ("vt01.r3d3","vt02.r3d3","vt03.r3d3",
#           "vt04.r3d3","vt05.r3d3","vt06.r3d3",
#           "vt07.r3d3","vt08.r3d3","vt09.r3d3",
#           "vt10.r3d3","vt11.r3d3","vt12.r3d3",
#           "vt13.r3d3","vt14.r3d3","vt15.r3d3",
#           "vt16.r3d3","vt17.r3d3","vt18.r3d3",
#           "vt19.r3d3","vt20.r3d3");

 @combos1 = ("vt01.r6d1","vt02.r6d1","vt03.r6d1",
            "vt04.r6d1","vt05.r6d1","vt06.r6d1",
            "vt07.r6d1","vt08.r6d1","vt09.r6d1",
            "vt10.r6d1","vt11.r6d1","vt12.r6d1");

 @combos2 = ("vt01.r3d3","vt02.r3d3","vt03.r3d3",
            "vt04.r3d3","vt05.r3d3","vt06.r3d3",
            "vt07.r3d3","vt08.r3d3","vt09.r3d3",
            "vt10.r3d3","vt11.r3d3","vt12.r3d3");


$report_name = "difference_report_${test1}_${test2}";

open (FILE,">$report_name") || die "Can't Open: $!\n";
print FILE "========================================  \n";
print FILE "Output differences from simulation \n\n";
print FILE "$test1 \n\n";
print FILE "compared to: \n\n";
print FILE "$test2 \n\n";
close (FILE);

for($combo=0; $combo<=11; $combo++)
 {
   open (FILE,">>$report_name") || die "Can't Open: $!\n";
   print FILE "  \n";
   print FILE "=================================== \n";
   print FILE "=====    combo: $combos2[$combo]    ====== \n";
   print FILE "=================================== \n\n";
   print FILE "  \n";
   close (FILE);
   
   system ("nccmp -d -S -f -q -s $results_dir1/output.nc.$combos1[$combo] $results_dir2/output.nc.$combos2[$combo] >& temp.txt");
   
   system ("cat temp.txt >> $report_name ");

 }
   
system ("mv $report_name reports ");
