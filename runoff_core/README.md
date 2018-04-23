runoff_core/README.md
========

This directory contains shell srcipts/Fortran programs to do
 the following operations

 1. Convert orignal CORE-runoff data format from netCDF to Direct access binary
 2. Manipulate masks of run-off data

Document
--------

 1. Convert orignal CORE runoff data from netCDF to direct access binary

    - nc2grads_ciaf_runoff.F90 : corrected interannal forcing
    - nc2grads_cnyf_runoff.F90 : corrected normal year forcing
    namelist.rg2latlon_template

 2. Globally integrate runoff data

    - integ_ciaf_runoff.F90
 
 3. Manipulation of mask

    - Output text file of mask (divide.F90)
    - manipulate mask on text editor
    - create new mask (unite.F90)


Development
--------

  * Developed at: Oceanography and Geocheminstry Research Department,
                  Meteorological Research Institute,
                  Japan Meteorological Agency
  * Contact: Hiroyuki Tsujino (htsujino@mri-jma.go.jp)
