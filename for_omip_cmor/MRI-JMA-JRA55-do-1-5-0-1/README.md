MRI-JMA-JRA55-do-1-5-0-1
========

   Generate CMORized data for input4MIPs


Contents
--------

   * runCmorAllWrite-1-5-0-1-update.py: Generate All files for JRA55-do-v1.5.0.1

   * Before running runCmorAllWrite-1-5-0-1-update.py, make symbolic links to 

      - Tables      ---> ../input4MIPs-cmor-tables
      - input_atmos ---> JRA55-do main data  (../../linkdir/products/version_1_5/netCDF)
      - input_suppl ---> JRA55-do supplemental data  (../../linkdir/products/support/netCDF)
      - input_clim  ---> JRA55-do climatological data  (../../linkdir/products/clim/netCDF)
      - input_fx    ---> JRA55-do invariant data  (../../linkdir/products/fx/netCDF)
      - input4MIPs  ---> CMORized data for input4MIPS (../../linkdir/products/version_1_5/input4MIPs)
 
   * Also, before running, please check <https://github.com/PCMDI/input4MIPs-cmor-tables>
     and update contents of ../input4MIPs-cmor-tables directory with those of 
     Tables directory of the latest <https://github.com/PCMDI/input4MIPs-cmor-tables>.
     Copy "input4MIPs_CV.json" to "input4MIPs_CV.json.trunk".
     Add entry of the latest version of JRA55-do to "input4MIPs_CV.json.MRI-JRA55-do-all-version".
     Create symbolic link from input4MIPs_CV.json.MRI-JRA55-do-all-version to input4MIPs_CV.json.


Usage Note
--------

   * On front, issue the following command:
       $ conda activate mypy37 # environment that contains the cmor 3.5.0
       $ run runCmorAllWrite-1-5-0-1.py

Contact
--------

   * Hiroyuki Tsujino (JMA-MRI)
