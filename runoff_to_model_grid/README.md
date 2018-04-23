runoff_to_model_grid
========

  Simple example to find a rivermouth grid (of ocean model grid)
  for runoff data of JRA55-do.

  Courtesy : Hideyuki Nakano (JMA-MRI)


Usage
--------

  % make
  % ./mk_mask         ( create mask (mask.gd) file for 0.25 x 0.25 grid
                        from potential temperature data of WOA13v2)
  % ./rivermouth_ctl  ( find rivemourth grids for WOA13 )

  check.gs  grads script for flow path



The dimension of the input data is (nx_roforg = 4 * 360, ny_roforg = 4 * 180) (0.25 x 0.25 grid).

iuput(model) :: 
  1. file_index (mask.gd created by src/mk_mask.F90)
      rec=1  ineger(4):: grid_index(nx,ny)  coast(ocean) : 1
                                            land (ocean) : 2
                                            ocean(other) : 0

input(data):  
   1.  file_riverindex:
          rec=1 integer(4):: grid_index(nx_roforg,ny_roforg)  
                          if rivermouth grid_index(i,j)=-9
   2.  file_mask_Grn
          rec=1 integer(4):: mask_Green(nx_roforg,ny_roforg)
                         if Greenland mask_Green=1
   3.  file_headxy
          rec=1 integer(4):: rof_headx (nx_roforg,ny_roforg)
          rec=2 integer(4):: rof_heady (nx_roforg,ny_roforg)
          rec=3 real(4)   :: rof_length(nx_roforg,ny_roforg)

         The location of the waterhead for  (i,j) point of rivermouth of the data is 
          (rof_headx(i,j), rof_heady(i,j)) in flowdir grid.

         the length of the river is rof_length (m)

   4. file_flwdir 
         rec=1 integer(1):: flwdir(nx_flwdir,ny_flwdir)
         The dimension of the flow direction is
           (nx_flowdir = 60 * 360, ny_flowdir = 60 * 180).         



output: file_rivermouth
   rec=1: integer(4):: rivermouth_x(nx_roforg,ny_roforg)   
   rec=2: integer(4):: rivermouth_y(nx_roforg,ny_roforg)   

   The (i,j) point of rivermouth of the input data
       is (rivermouth_x(i,j), rivermouth_y(i,j)) in the OGCM grid.
   

Contact
--------

    * Hiroyuki Tsujino (htsujino@mri-jma.go.jp)
