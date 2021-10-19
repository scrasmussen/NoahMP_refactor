#!/usr/bin/perl

# Directory where results will be placed

$testname  = "snow_canopy_soilwater_refact";

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

# @combos = ("vt01.r1d1","vt02.r1d1","vt03.r1d1",
#           "vt04.r1d1","vt05.r1d1","vt06.r1d1",
#           "vt07.r1d1","vt08.r1d1","vt09.r1d1",
#           "vt10.r1d1","vt11.r1d1","vt12.r1d1");

# @combos = ("vt01.r2d2","vt02.r2d2","vt03.r2d2",
#           "vt04.r2d2","vt05.r2d2","vt06.r2d2",
#           "vt07.r2d2","vt08.r2d2","vt09.r2d2",
#           "vt10.r2d2","vt11.r2d2","vt12.r2d2");

# @combos = ("vt01.r4d4","vt02.r4d4","vt03.r4d4",
#           "vt04.r4d4","vt05.r4d4","vt06.r4d4",
#           "vt07.r4d4","vt08.r4d4","vt09.r4d4",
#           "vt10.r4d4","vt11.r4d4","vt12.r4d4");

# @combos = ("vt01.r5d5","vt02.r5d5","vt03.r5d5",
#           "vt04.r5d5","vt05.r5d5","vt06.r5d5",
#           "vt07.r5d5","vt08.r5d5","vt09.r5d5",
#           "vt10.r5d5","vt11.r5d5","vt12.r5d5");	   

chdir("namelists");

for($combo=0; $combo<=11; $combo++)
 {
   system ("cp ../../../run/namelist.input namelist.input.$combos[$combo]");
 }
   
chdir("..");

