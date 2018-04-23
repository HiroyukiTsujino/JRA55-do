
=========================================================
    Make GrADs file from default output from MRICOM
=========================================================

*** DO FIRST ***

  1) Edit "basin_param.F90" and specify coordinates of the model.

  2) Copy "dz.F90" from your source directory.

  3) Edit "Makefile" and specify Fortran 90 compiler and CPP directives.

    If you chose any of VARIABLE, CYCLIC, SUB, BBL, ICEFULLMONIT, ICECMIP5MONIT, VVDIMP, MELYAM, NOHKIM
     please define CPPFLAGS like the following:
  
    CPPFLAGS = -DOGCM_VARIABLE -DOGCM_CYCLIC -DOGCM_BBL -DOGCM_ICEFULLMONIT -DOGCM_ICECMIP5MONIT

  4) Prepare gridspace definition file "vgrid.d" (for option VARIABLE)
     topography data file "topo.d".
     These files are needed for all operations and should be specified in shell scripts.

*** Compile ***

  $ make converter
  ($ gmake converter for SR16000)

  ... all binaries are created


*** Grid point definition *** 

  Shell script: make_xyzdefs.sh
  F90 Program : mkgrads_xyzdefs.F90

  1) In "make_xyzdefs.sh",
     please specify whether grid increment is linear or not for each direction.
     
  2) Issue the following command

     % make_xyzdefs.sh


*** restart (snap shot) ***

   1) ocean interior 

      Shell Script: mkrestart_ocean.sh
      F90 Program : mkgrads_restart_ocean.F90

      Edit Shell Scripts and issue a command.
  
      % mkrestart_ocean.sh

         Usage: mkrestart_ocean.sh inputfile_extension outputfile_extension number_of_data

   2) ice averaged over thickness-categorized ice

      Shell Script: mkrestart_ice.sh
      F90 Program : mkgrads_ice_restart.F90

      Edit Shell Scripts and issue a command.
  
      % mkrestart_ice.sh

         Usage: mkrestart_ice.sh inputfile_extension outputfile_extension number_of_data

   3) ice categorized by thickness

      Shell Script: mkrestart_icecat.sh
      F90 Program : mkgrads_icecat_restart.F90

      Edit Shell Scripts and issue a command.
  
      % mkrestart_icecat.sh

         Usage: mkrestart_icecat.sh inputfile_extension outputfile_extension

*** history (average) ***

   1) ocean interior (HIST)

      Shell Script: mkhist_ocean.sh*
      F90 Program : mkgrads_ocean_hist.F90

      Edit Shell Scripts and issue a command.
  
      % mkhist_ocean.sh

      Usage: mkhist_ocean.sh inputfile_extension outputfile_extension number_of_data

      Note: Items are written to separate files.

   2) surface fluxes (HISTFLUX)

      Shell Script: mkhist_flux.sh*
      F90 Program : mkgrads_hist_flux.F90
      GrADs control file : hist_flux.ctl

      Edit Shell Scripts and issue a command.
  
      % mkhist_flux.sh

      Usage: mkhist_flux.sh inputfile_extension outputfile_extension number_of_data

   3) ice averaged over thickness-categorized ice (HISTICEORG)

      Shell Script: mkgrads_ice_hist.F90
      F90 Program : mkhist_iceorg.sh*
      GrADs control file : hist_ice.ctl

      Edit Shell Scripts and issue a command.
  
      % mkhist_iceorg.sh

      Usage: mkhist_iceorg.sh inputfile_extension outputfile_extension number_of_data

   4) ice categorized by thickness (HISTICECAT)

      Shell Script: mkhist_icecat.sh
      F90 Program : mkgrads_icecat_hist.F90

      Edit Shell Scripts and issue a command.
  
      % mkhist_icecat.sh

      Usage: mkhist_icecat.sh inputfile_extension outputfile_extension

*** files ***

  basin_param.F90  : specifies the number of grid points
  set_grid.F90     : defines coordinates and mask
  mkgrads_ctl.F90  : write GrADs control file (only for categorized ice)
