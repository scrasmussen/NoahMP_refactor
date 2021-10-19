#!/usr/bin/perl

# Directory where results will be placed

$testname  = "waterall_addrunoff_refact_opt81";

# @combos = ("vt01.r3d3","vt02.r3d3","vt03.r3d3",
#           "vt04.r3d3","vt05.r3d3","vt06.r3d3",
#           "vt07.r3d3","vt08.r3d3","vt09.r3d3",
#           "vt10.r3d3","vt11.r3d3","vt12.r3d3",
#           "vt13.r3d3","vt14.r3d3","vt15.r3d3",
#           "vt16.r3d3","vt17.r3d3","vt18.r3d3",
#           "vt19.r3d3","vt20.r3d3");

 @combos = ("vt01.r3d3","vt02.r3d3","vt03.r3d3",
            "vt04.r3d3","vt05.r3d3","vt06.r3d3",
            "vt07.r3d3","vt08.r3d3","vt09.r3d3",
            "vt10.r3d3","vt11.r3d3","vt12.r3d3");

system("mkdir -p results/$testname");
system("mkdir workshop");
chdir("workshop");

system ("cp ../../../run/water_refac.exe .");

for($combo=0; $combo<=11; $combo++)
 {
   system ("cp ../namelists/namelist.input.$combos[$combo] namelist.input");
   system ("./water_refac.exe");
   system ("mv output.nc ../results/$testname/output.nc.$combos[$combo]");
 }
   
chdir("..");
 system ("rm -Rf workshop");
