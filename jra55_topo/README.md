SURF_FLUX/jra55_topo
========

This directory contains shell srcipts/Fortran programs to do
 the following operations

 1. Create Ocean/Land mask of JRA-55 suitable to OGCM use
    (that is, exclude lake and pond)
 2. Give list of JRA-55 grid points that should be filled with
    valid QuikSCAT data over the ocean

Document
--------

 1. Create Ocean/Land mask of JRA-55 suitable to OGCM use
     - divide_to_text.sh
        ---> ocean directory
     - unite_to_binary.sh


 2. Give list of JRA-55 grid points that should be filled with
    valid QuikSCAT data over the ocean
     ---> wind_fill directory


Development
--------

  * Developed at: Oceanography and Geochemistry Research Department,
                  Meteorological Research Institute,
                  Japan Meteorological Agency
  * Contact: Hiroyuki Tsujino (htsujino@mri-jma.go.jp)
