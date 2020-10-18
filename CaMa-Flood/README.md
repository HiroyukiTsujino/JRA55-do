CaMa-Flood
========

   Run CaMa-Flood and extract river discharge for JRA55-do.
   Courtesy Tatsuo Suzuki (JAMSTEC) and Dai Yamazaki (Univ. Tokyo).


Usage
--------

   * Process raw JRA-55 data and produce input data for CaMa-Flood

      - ../runoff_mk_input directory (see readme.txt)

      - CONV_GRID2bin1dy_mri.sh : Convert JRA55-raw in GRIB format to flat binary format.
         <------- Input : fcst_phyland (JRA-55 raw data)
         -------> Output: ../linkdir/work/jra55fcst_v1_3_input_runoff_3hr_TL319r

      - CONV_T319toRIV1dy_tfact_mri.sh : Produce input data for CaMaFlood.
         <------- Input : ../linkdir/work/jra55fcst_v1_3_input_runoff_3hr_TL319r
         -------> Output: ../linkdir/work/jra55fcst_v1_3_input_runoff_1dy_025x025


   * Run CaMa-Flood

      - Create symbolic links by editing/executing "make_links.sh".

      - Extract CaMa-Flood_v3.6_JRA55 from a tarball.

      - Change directory to CaMa-Flood_v3.6_JRA55.

      - Edit "adt/Mkinclude" so that your Fortran compiler works properly.

      - Go to "gosh" directory.

      - run compile.sh

      - run JRA55_mri.sh 
         <------- Input  : INPUT (../linkdir/work/jra55fcst_v1_3_input_runoff_1dy_025x025)
         -------> Output : OUTPUT/JRA55_mri (../linkdir/CaMa-Flood/OUTPUT_MRI/JRA55_mri)


   * Process CaMa-Flood output to produce river discharge to the oceans

      - Extract_rivmouth directory

      - compile mk_runoff_v1.0_mri.F 

      - run mk_runoff_v1.0_mri (extract only river mouth data)
         <------- Input : OUTPUT/JRA55_mri (../linkdir/CaMa-Flood/OUTPUT_MRI/JRA55_mri)
         -------> Output: OUTPUT/riv_v1.0_mri (../linkdir/CaMa-Flood/OUTPUT_MRI/riv_v1.0_mri)


Contact
--------

   * Hiroyuki Tsujino (JMA-MRI)
