CaMa-Flood
========

   Run CaMa-Flood for JRA55-do.
   Courtesy Tatsuo Suzuki of JAMSTEC.


Usage
--------

   * Process raw JRA-55 data and produce input data for CaMa-Flood

      - ../runoff_mk_input directory (see readme.txt)

      - CONV_GRID2bin1dy_mri.sh : Convert JRA55-raw in GRIB format to flat binary format
         <------- Input : fcst_phyland
         -------> Output: DATA/T319bin1dy

      - CONV_T319toRIV1dy_tfact_mri.sh : Produce input data for CaMaFlood
         <------- Input : DATA/T319bin1dy
         -------> Output: DATA/RIVGRD_1dy_tfact2


   * Run CaMa-Flood

      - Create symbolic links by editing/executing "make_links.sh".

      - Extract CaMa-Flood_v3.6_JRA55 from a tarball.

      - Change directory to CaMa-Flood_v3.6_JRA55.

      - Edit "adt/Mkinclude" so that your Fortran compiler works properly.

      - Go to "gosh" directory.

      - run compile.sh

      - run JRA55_mri.sh 
         <------- Input : DATA/RIVGRD_1dy_tfact2
         -------> Output: CaMa_out/JRA55_mri


   * Process CaMa-Flood output to produce river discharge to the oceans

     - Extract_rivmouth directory

     - compile mk_runoff_v1.0_mri.F 

     - run mk_runoff_v1.0_mri (extract only river mouth data)
         <------- Input : CaMa_out/JRA55_mri
         -------> Output: CaMa_out/riv_v1.0_mri


Contact
--------

   * Hiroyuki Tsujino (JMA-MRI)
