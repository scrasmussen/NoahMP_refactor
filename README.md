# NoahMP_refactor
This private repo is used to track the NoahMP model refactor progress

=========================================

Steps to running model:

1. Configure environment and build

  a. (optional) create your own environment file in the config directory, need Fortran compiler and NetCDF library

  b. ./configure

    This will generate the user_build_options file. if you didn't create your own environment file, choose any option and manually set compiler and library paths

  c. make

2. Change setup in namelist.input in run/

3. Run xxx.exe in run/

=========================================

Reproducibility tests (located in test/reproduce):

1. run_namelist_combos.perl

  a. change testname, then run

2. compare_results.perl

  a. change names to compare, then run, will produce report in reports/


