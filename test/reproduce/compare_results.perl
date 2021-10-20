#!/usr/bin/perl

$test1  = "070c68c_soilwater_baseline"    ; # Directory where results are located
$test2  = "070c68c_soilwater_baseline";

$results_dir1 = "results/$test1";
$results_dir2 = "results/$test2";

@combos = ("sl01","sl02","sl03",
           "sl04","sl05","sl06",
           "sl07","sl08","sl09",
	   "sl10","sl11","sl12");

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
   print FILE "============================== \n";
   print FILE "=====    combo: $combos[$combo]    ====== \n";
   print FILE "============================== \n\n";
   print FILE "  \n";
   close (FILE);
   
   system ("nccmp -d -S -f -q $results_dir1/output.nc.$combos[$combo] $results_dir2/output.nc.$combos[$combo] >& temp.txt");
   
   system ("cat temp.txt >> $report_name ");

 }
   
system ("mv $report_name reports ");
