MRI-JMA-JRA55-do-1-5-0
========

   Generate CMORized data for input4MIPs


Contents
--------

   * runCmorAllWrite-1-5-0.py: Generate All files for JRA55-do-v1.5.0

   * Before running runCmorAllWrite-1-5-0.py, make symbolic links to 

      - Tables      ---> ../input4MIPs-cmor-tables
      - input_atmos ---> JRA55-do main data  (../../linkdir/products/version_1_5/netCDF)
      - input_suppl ---> JRA55-do supplemental data  (../../linkdir/products/support/netCDF)
      - input_clim  ---> JRA55-do climatological data  (../../linkdir/products/clim/netCDF)
      - input_fx    ---> JRA55-do invariant data  (../../linkdir/products/fx/netCDF)
      - input4MIPs  ---> CMORized data for input4MIPS (../../linkdir/products/version_1_5/input4MIPs)


Usage Note
--------

   * On front, issue the following command:
       $ conda activate mypy37 # environment that contains the cmor 3.5.0
       $ run runCmorAllWrite-1-5-0.py

   * On glb193, issue the following command:
       $ conda activate CMOR # environment that contains the latest cmor
       $ run runCmorAllWrite-1-5-0.py

Contact
--------

   * Hiroyuki Tsujino (JMA-MRI)
