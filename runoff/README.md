runoff/README.md
========

This directory contains shell srcipts/Fortran programs
to evaluate and manipulate daily river runoff data on 0.25 x 0.25.


Description
--------

  * src/total_runoff_noyrev.F90: 
    Generate monthly and annual time series from daily data. 
    This should be useful for evaluating JRA55-do runoff products.
    Example: namelist.total_runoff_noyrev_jra55-do-v1_1.


Note
--------

Following information is obsolete:
 used to handle river run-off data from CaMaFlood.


Data
--------

 /worke/htsujino/RUNOFF_YOSHIMURA:

    - original/rivoutYYYY.bin : original river run-off data provided by Yoshimura-Labo
    - nextxy.bin : downstream grid point
         -9  : end point (point of run off)
    - nextxy_wo_lake.bin : downstream grid point
         -9  : rivermouth of coast
         -99 : lake
    - calib_0/rivoutYYYY.bin : the order of data in Y-direction is reversed (no-yrev)
    - mask.gd: CaMa flood grid point with run-off ("zero" run-off is included)
    - mask_poles.gd: mask CaMa flood region to be filled by CORE run-off ("zero" run-off is included)
    - merge_1/rivoutYYYY.bin : fill Greenland with CORE
    - mask_antactica.gd: mask CaMa flood region to be filled by CORE run-off ("zero" run-off is EXcluded)
    - merge_5/rivoutYYYY.bin : cover entire period 1958-2015 utilizing climatology applied by precipitation on land
    - final_v1/runoff.YYYY : final product in units kg/m2/s


Programs/Scripts
--------

 src: ( following the order of operations )

  1. check_nextxy.F90: check downstream grid point

  2. delete_lake.F90: delete run off to lake from nextxy.bin
                      and produce nextxy_wo_lake.bin

  3. apply_factor.F90: apply user specified factor to the original file
       and output reversing the order of data in the Y-direction
       (original yrev data is converted to data recorded from south to north)

  4. divide.F90/unite.F90: make masks (mask.gd -----> mask_poles.gd)

  5. merge_cama_core.F90: merge CaMa flood and CORE
    (first around Greenland and then around Antarctica)

  6. total_runoff.F90, total_runoff_noyrev.F90, total_runoff_clim.F90:
      compute globally integrated run-offs

  7. apply_factor_noyrev.F90: apply user specified factor to the merged file
      both input and output files is recorded from south to north)

  8. mkclim_runoff_noyrev.F90: compute daily climatology 
  
  9. create_data.F90: create data from climatology applied by 
      precipitation on land

 10. create_final_product.F90: create final product (kg/m2/s)
      along with grid cell area (m2)

 11. create_1x1_monthly.F90: create monthly 1x1 grid data (kg/m2/sec)
      from daily data prepared for OMIP (kg/m2/sec)


Development
--------

  * Developed at: Oceanography and Geochemistry Research Department,
                  Meteorological Research Institute,
                  Japan Meteorological Agency
  * Contact: Hiroyuki Tsujino (htsujino@mri-jma.go.jp)
