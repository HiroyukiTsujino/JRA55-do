MRI-JMA-JRA55-do-v1_3
========

   Produce CMORized data for input4MIPs


Contents
--------

   * runCmorDemo.py: Demo srcipt to generate CMORized files
        provided by Paul Durack of LLNL

   * runCmorCheck-1-3-1.py: Check script to generate CMORized files
        for JRA55-do-v1.3.1

   * runCmorAllWrite-1-3-1.py: Generate All files for JRA55-do-v1.3.1

   * runCmor_clim.py: Generate climatological files for JRA55-do-v1.3.1

   * runCmor_fx.py: Generate cell area / land-sea mask files for JRA55-do-v1.3.1

   * Make symbolic links to 

      - input_atmos ---> JRA55-do main data 
      - input_suppl ---> JRA55-do supplemental data


Usage Note
--------

   * edit and run "make_filelist.sh" to create the list of files to be processed
       then copy and paste to runCmorAllWrite-1-3-1.py

   * issue the following command:
       $ source activate cmot331
       $ run runCmorAllWrite-1-3-1.py


   * edit and run "extract_sample_header.sh" to check
       header of the generated files
       output is written to demo directory



Contact
--------

   * Hiroyuki Tsujino (JMA-MRI)
