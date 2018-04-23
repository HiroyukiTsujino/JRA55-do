!-*-F90-*-
program mk_AntFreshFlux_Depoorter
  implicit none
  real(4),parameter :: rundef = -9.99e33
  integer(4), parameter :: nx = 360 * 4, ny = 180 * 4
  real(4) :: dx = 0.25e0, dy = 0.25e0
  integer(4) :: id_2d   (nx, ny)
  integer(4) :: scode_2d(nx, ny) 
  integer(4) :: coast_2d(nx, ny)
  integer(4) :: shelf_2d(nx, ny)
  integer(4) :: shelf_other_2d(nx, ny)
!
  real(4) :: CF_2d      (nx, ny) ! Calving Flux
  real(4) :: BMF_2d     (nx, ny) ! Basal Melt Flux
  real(4) :: FWF_2d_cnst(nx, ny) ! Constant Fresh Water flux along the coast (CF + BMB)
  integer(4),parameter :: id_str    = 1, id_end    = 27
  integer(4),parameter :: scode_str = 4, scode_end =  6

  real(8) :: area(ny), ashft(ny+1), anhft(ny+1)  ! m^2
  integer(4) :: list_coast_x(nx*ny), list_coast_y(nx*ny), num_coast

  integer(4),parameter :: shelf_num = 31, upscale_num = 6
  real(4) :: CF_org    ( shelf_num   ) ! Calving Supple. Table 1 of Depoorter et al. 2013 Nature
  real(4) :: CF_org_up ( upscale_num )
  real(4) :: BMF_org   ( shelf_num   ) 
  real(4) :: BMF_org_up( upscale_num )

  call set_grid

  call read_ascii_data
  call fill_in_closed_grids
  call identify_coast
  call identify_iceshelf

  call set_Freshwater_source
  call set_Freshwater_flux

  call write_binary_data

contains
!===============================================================
subroutine set_grid
  integer(4) :: j
  real(8) :: ttng, tanfi, ddyt, cst
  real(8) :: lat, lon
  real(8),parameter :: pi     = 3.1415926535897932384626433832795d0
  real(8),parameter :: radius = 6375.d3, deg1 = radius * pi / 180.d0
  
  do j = 1, ny + 1
    lat  = -90.d0 + (j-1) * dy
    ttng  = dtan(pi / 180.d0 * lat )
    tanfi = dtan(0.25d0 * dy / 180.d0 * pi) 
    cst   = cos(pi / 180.d0 * lat )
    ddyt  = 2.d0*radius*dsin(0.5d0*dy/180.d0*pi)
    ashft(j) = 0.25d0 * (1.d0+ttng*tanfi)*dx*dy*cst * deg1**2
    anhft(j) = 0.25d0 * (1.d0-ttng*tanfi)*dx*dy*cst * deg1**2
  end do
  do j = 1, ny
    area(j) = 2.d0 * (anhft(j) + ashft(j+1))
  end do
  
end subroutine set_grid
!---------------------------------------------------------------  
subroutine read_ascii_data

  integer(4) :: i, j, n
  integer(4) :: maxtmp, sumtmp
  integer(4) :: count_2d_id   (nx,ny,id_str   :id_end   )
  integer(4) :: count_2d_scode(nx,ny,scode_str:scode_end)
  integer(4) :: ix, iy, id, scode
  integer(4) :: num_1kmx1km_per_area
  real(4) :: lat, lon
  integer(4) :: iunit

  id_2d   (:,:) = 0.0
  scode_2d(:,:) = 0.0
  count_2d_id   (:,:,:) = 0
  count_2d_scode(:,:,:) = 0

  open(iunit,file='data_Antarctica/Ant_ICESat_MODIS_Mask_1km.ascii',form='formatted')
  ! from http://icesat4.gsfc.nasa.gov/cryo_data/ant_grn_drainage_systems.php
  !      "Antarctic surface specification and drainage system grids"
  !
  ! Citation
  ! Zwally, H. Jay, Mario B. Giovinetto, Matthew A. Beckley, and Jack L. Saba, 2012,
  ! Antarctic and Greenland Drainage Systems, GSFC Cryospheric Sciences Laboratory, at 
  ! http://icesat4.gsfc.nasa.gov/cryo_data/ant_grn_drainage_systems.php.
  do n = 1, 40
    read(iunit,*)
  end do
  do n = 41, 13522972
    read(iunit,*) ix, iy, lat, lon, id, scode
    i = lon / dx + 1
    j = (lat + 90.e0) / dy + 1
    if ( id >= id_str ) then
      count_2d_id(i,j,id) = count_2d_id(i,j,id) + 1
    end if
    if ( scode >= scode_str ) then
      count_2d_scode(i,j,scode) = count_2d_scode(i,j,scode) + 1
    end if
  end do
  do j = 1, ny
    num_1kmx1km_per_area = area(j) * (1.d-3)**2
    do i = 1, nx
      maxtmp = maxval(count_2d_id   (i,j,:))
      sumtmp = sum   (count_2d_id   (i,j,:))
      do n = id_str, id_end
        if ( count_2d_id(i,j,n) == maxtmp .and. &
             & sumtmp >= num_1kmx1km_per_area /2) then
          id_2d   (i,j) = n
          exit
        end if
      end do
      maxtmp = maxval(count_2d_scode (i,j,:))
      sumtmp = sum   (count_2d_scode (i,j,:))
      do n = scode_str, scode_end
        if ( count_2d_scode(i,j,n) == maxtmp .and. &
             & sumtmp >= num_1kmx1km_per_area /2) then
          scode_2d(i,j) = n
          exit
        end if
      end do
    end do
  end do   


end subroutine read_ascii_data
!---------------------------------------------------------------
subroutine fill_in_closed_grids
  implicit none
  integer(4) :: i, j, n
  integer(4) :: ip1, im1
  real(8) :: neighb_id   (id_str   :id_end   )
  real(8) :: neighb_scode(scode_str:scode_end)
  real(8) :: max_neighb  
  integer(4) :: southern_limit = 13

  do j = 2, ny-1
    do i = 1, nx
      if ( id_2d(i,j) /= 0 ) cycle
      ip1 = i + 1
      im1 = i - 1
      if ( i == 1  ) im1 = nx
      if ( i == nx ) ip1 = 1
      if (   id_2d(im1,j) /= 0 .and. &
           & id_2d(ip1,j) /= 0 .and. &
           & id_2d(i,j+1) /= 0 .and. &
           & id_2d(i,j-1) /= 0 ) then
        neighb_id(:) = 0
        do n = id_str, id_end
          if ( id_2d(im1,j) == n ) neighb_id(n) = neighb_id(n) + area(j)
          if ( id_2d(ip1,j) == n ) neighb_id(n) = neighb_id(n) + area(j)
          if ( id_2d(1,j-1) == n ) neighb_id(n) = neighb_id(n) + area(j-1)
          if ( id_2d(1,j+1) == n ) neighb_id(n) = neighb_id(n) + area(j+1)
        end do
        max_neighb = maxval(neighb_id(:))
        do n = id_str, id_end
          if ( neighb_id(n) == max_neighb ) id_2d(i,j) = n
        end do
      end if
    end do
  end do
  do j = 2, ny-1
    do i = 1, nx
      if ( scode_2d(i,j) /= 0 ) cycle
      ip1 = i + 1
      im1 = i - 1
      if ( i == 1  ) im1 = nx
      if ( i == nx ) ip1 = 1
      if (   (scode_2d(im1,j) /= 0 .and. &
            & scode_2d(ip1,j) /= 0 .and. &
            & scode_2d(i,j+1) /= 0 .and. &
            & scode_2d(i,j-1) /= 0 ) .or. j <= southern_limit ) then
        neighb_scode(:) = 0
        do n = scode_str, scode_end
          if ( scode_2d(im1,j) == n ) neighb_scode(n) = neighb_scode(n) + area(j)
          if ( scode_2d(ip1,j) == n ) neighb_scode(n) = neighb_scode(n) + area(j)
          if ( scode_2d(1,j-1) == n ) neighb_scode(n) = neighb_scode(n) + area(j-1)
          if ( scode_2d(1,j+1) == n ) neighb_scode(n) = neighb_scode(n) + area(j+1)
        end do
        max_neighb = maxval(neighb_scode(:))
        do n = scode_str, scode_end
          if ( neighb_scode(n) == max_neighb ) scode_2d(i,j) = n
        end do
      end if
    end do
  end do  

end subroutine fill_in_closed_grids
!---------------------------------------------------------------
subroutine identify_coast
  implicit none
  integer(4) :: i, j
  integer(4) :: ip1, im1

  num_coast = 0
  do j = 2, ny-1
    do i = 1, nx
      coast_2d(i,j) = 0
      ip1 = i + 1
      im1 = i - 1
      if ( i == 1  ) im1 = nx
      if ( i == nx ) ip1 = 1
      if ( scode_2d(i,j) > 0 ) then
        if ( scode_2d(im1,j  ) == 0 .or. &
          &  scode_2d(ip1,j  ) == 0 .or. &
          &  scode_2d(i,  j+1) == 0 .or. &
          &  scode_2d(i,  j-1) == 0 ) then
          coast_2d(i,j) = 1
          num_coast = num_coast + 1
          list_coast_x(num_coast) = i
          list_coast_y(num_coast) = j
          if ( j <= 8 ) then
            write(*,*) 'error', i, j
            write(*,*) scode_2d(im1,j), scode_2d(ip1,j)
            write(*,*) scode_2d(i,j-1), scode_2d(i,j+1)
            stop
          endif
        end if
      end if
    end do
  end do  
end subroutine identify_coast
!---------------------------------------------------------------
subroutine identify_iceshelf
  implicit none
  integer(4) :: i, j, k
  integer(4) :: id, scode

  shelf_2d(:,:) = 0
  do j = 1, ny
    do i = 1, nx
      id = id_2d(i,j)
      scode = scode_2d(i,j)
      if ( scode == 6 ) then
        if ( id == 6 ) shelf_2d(i,j) = 1 ! AR
        if ( id == 7 .or. id == 8 ) shelf_2d(i,j) = 2 ! NE
        if ( id == 9 .or. id == 10 .or. id == 11) shelf_2d(i,j) = 3 ! AIS
        if ( id == 12) then
          if ( i>= 323 .and. i <= 367 ) shelf_2d(i,j) = 4 ! W
          if ( i>= 379 .and. i <= 420 ) shelf_2d(i,j) = 5 ! SHA
        end if
        if (id == 13 ) then
          if ( i <= 450 ) then
            shelf_2d(i,j) = 6 ! VAN
          else if ( i <= 472 ) then
            shelf_2d(i,j) = 7 ! TOT
          else if ( i <= 493 ) then
            shelf_2d(i,j) = 8 ! MU
          else
            shelf_2d(i,j) = 9 ! POR
          end if
        end if
        if (id == 14 ) then
          if ( i >= 530 .and. i <= 553 ) shelf_2d(i,j) = 10 ! ADE
          if ( i > 572 ) then
            if ( i<= 585 .or. j >= 89) then
              shelf_2d(i,j) = 11 ! Mer
            else if ( i <= 601) then
              shelf_2d(i,j) = 12 ! Nin
            else if ( i >= 608 .and. i <= 620) then
              shelf_2d(i,j) = 13 ! Coo
            end if
          end if
        end if
        if (id == 15) then
          if ( i>=644 .and. i<= 652 ) shelf_2d(i,j) = 14 ! REN
        end if
        if (id == 16) shelf_2d(i,j) = 15 ! DRY
        if (id >= 17 .and. id <= 19) shelf_2d(i,j) = 16 ! RIS
        if (id == 20 ) then
          if ( i> 833 .and. i <= 860 .and. j<=54 ) then
            shelf_2d(i,j) = 17 ! SUL
          else if ( i > 873 .and. i <=  876 ) then
            shelf_2d(i,j) = 18 ! LAND
          else if ( i > 899 ) then
            shelf_2d(i,j) = 19 ! GET
          end if
        end if
        if (id == 21) then
          if ( i <= 1005 ) then
            shelf_2d(i,j) = 20 ! CD
          else 
            shelf_2d(i,j) = 21 ! THW
          end if
        end if
        if (id == 22) shelf_2d (i,j) = 22 ! Pine
        if (id == 23) then
          if ( i <= 1044 .and. j <= 67 ) then
            shelf_2d(i,j) = 23 ! COS
          else if ( i<= 1084 ) then
            shelf_2d(i,j) = 24 ! AAB
          else if ( i<= 1099 ) then
            shelf_2d(i,j) = 25 ! VEN
          end if
        end if
        if (id == 24) shelf_2d(i,j) = 26 ! GEO
        if (id == 25) shelf_2d(i,j) = 27 ! WOR
        if (id == 26) then
          if ( j <= 99 ) shelf_2d(i,j) = 28 ! LBC
        end if
!        if (id == 27) 
        if (id >= 1 .and. id <= 3) shelf_2d(i,j) = 29 ! FRIS
        if (id == 4) shelf_2d(i,j) = 30 ! BRL
        if (id == 5) shelf_2d(i,j) = 31 ! JF
      end if
    end do
  end do  

  do k = 1, num_coast
    i = list_coast_x(k)
    j = list_coast_y(k)
    if ( shelf_2d(i,j) == 0 ) then
      id = id_2d(i,j)
      if ( id >=  6 .and. id <= 12 ) shelf_other_2d(i,j) = 1 ! AC'
      if ( id >= 12 .and. id <= 15 ) shelf_other_2d(i,j) = 2 ! C'D'
      if ( id == 16)                 shelf_other_2d(i,j) = 3 ! D'F'
      if ( id >= 20 .and. id <= 22 ) shelf_other_2d(i,j) = 4 ! F'H
      if ( id == 23 .or.  id == 25 ) shelf_other_2d(i,j) = 5 ! HI'
      if ( id == 26 .or.  id == 27 .or. id == 4 .or. id == 5) shelf_other_2d(i,j) = 6 ! I'K
    end if
  end do
  
end subroutine identify_iceshelf
!---------------------------------------------------------------
subroutine set_Freshwater_source

  integer(4) :: n
  real(4) :: sum_CF, sum_CF_plus, sum_BMF, sum_BMF_plus

! Based on Table 1 of Depoorter et al.(2013, Nature12567)

  CF_org( 1) =  44.e0;    BMF_org( 1) =  39.e0    
  CF_org( 2) =  30.e0;    BMF_org( 2) =  36.e0    
  CF_org( 3) =  50.e0;    BMF_org( 3) =  39.e0    
  CF_org( 4) =  31.e0;    BMF_org( 4) =  26.e0    
  
  CF_org( 5) =  34.e0;    BMF_org( 5) =  76.e0 
  CF_org( 6) =   9.e0;    BMF_org( 6) =   5.e0 
  CF_org( 7) =  28.e0;    BMF_org( 7) =  64.e0 
  CF_org( 8) =  23.e0;    BMF_org( 8) =  28.e0 
  CF_org( 9) =  36.e0;    BMF_org( 9) =  18.e0 
  CF_org(10) =   7.e0;    BMF_org(10) =  13.e0 
  CF_org(11) =  20.e0;    BMF_org(11) =   5.e0 
  CF_org(12) =  23.e0;    BMF_org(12) =   0.e0 
  CF_org(13) =  32.e0;    BMF_org(13) =   3.e0 
  CF_org(14) =   1.e0;    BMF_org(14) =   7.e0 
  
  CF_org(15) =   3.e0;    BMF_org(15) =   5.e0    
  CF_org(16) = 147.e0;    BMF_org(16) =  34.e0    
  CF_org(17) =   3.e0;    BMF_org(17) =  28.e0    
  
  CF_org(18) =  11.e0;    BMF_org(18) =   6.e0    
  CF_org(19) =  56.e0;    BMF_org(19) = 136.e0    
  CF_org(20) =  18.e0;    BMF_org(20) =  78.e0    
  CF_org(21) =  62.e0;    BMF_org(21) =  69.e0    
  CF_org(22) =  50.e0;    BMF_org(22) =  95.e0    
  CF_org(23) =   2.e0;    BMF_org(23) =  11.e0    
  
  CF_org(24) =   4.e0;    BMF_org(24) =  86.e0    
  CF_org(25) =   8.e0;    BMF_org(25) =  15.e0    
  CF_org(26) =  17.e0;    BMF_org(26) = 144.e0    
  CF_org(27) =   2.e0;    BMF_org(27) =  10.e0    
  
  CF_org(28) =  25.e0;    BMF_org(28) =  18.e0    
!  CF_org(29) = 250.e0;    BMF_org(29) =  50.e0    
  CF_org(29) = 249.e0;    BMF_org(29) =  49.e0     ! slightly adjust the number of FRIS
  CF_org(30) =  46.e0;    BMF_org(30) =  26.e0     ! in order to fit total number.
  CF_org(31) =  34.e0;    BMF_org(31) =  24.e0

  CF_org_up(1) = 49.e0;  BMF_org_up(1) = 40.e0
  CF_org_up(2) = 93.e0;  BMF_org_up(2) = 82.e0
  CF_org_up(3) = 14.e0;  BMF_org_up(3) = 12.e0
  CF_org_up(4) = 34.e0;  BMF_org_up(4) = 89.e0
  CF_org_up(5) = 10.e0;  BMF_org_up(5) = 25.e0
  CF_org_up(6) = 16.e0;  BMF_org_up(6) = 13.e0

  sum_CF= 0.0
  sum_BMF = 0.0
  do n = 1, shelf_num
    sum_CF  = sum_CF  + CF_org(n)
    sum_BMF = sum_BMF + BMF_org(n)
  end do

  sum_CF_plus  = sum_CF
  sum_BMF_plus = sum_BMF
  do n = 1, upscale_num
    sum_CF_plus  = sum_CF_plus  + CF_org_up(n)
    sum_BMF_plus = sum_BMF_plus + BMF_org_up(n)
  end do

  write(*,*) 'sum_CF      (Gt/yr)=', sum_CF
  write(*,*) 'sum_BMF     (Gt/yr)=', sum_BMF
  write(*,*) 'sum_CF_plus (Gt/yr)=', sum_CF_plus
  write(*,*) 'sum_BMF_plus(Gt/yr)=', sum_BMF_plus
   
end subroutine set_Freshwater_source
!---------------------------------------------------------------    
subroutine set_Freshwater_flux
  integer(4) :: i, j, k, n
  real(8) :: areasum_shelf(shelf_num  )
  real(8) :: areasum_other(upscale_num)
  real(8) :: areasum_coast, FWFsum
  real(8) :: fct, CFsum, BMFsum

  real(8),parameter :: Gt_p_yr_to_m3_p_sec  = 86400*365.24*1.e-6 ! ton $\simeq$ weight of m^3 of water
  
  areasum_shelf(:) = 0.d0
  areasum_other(:) = 0.d0
  areasum_coast    = 0.d0
  do k = 1, num_coast
    i = list_coast_x(k)
    j = list_coast_y(k)
    areasum_coast = areasum_coast + area(j)
    do n = 1, shelf_num
      if ( shelf_2d(i,j) == n) areasum_shelf(n) = areasum_shelf(n) + area(j)
    end do
    do n = 1, upscale_num
      if ( shelf_other_2d(i,j) == n) areasum_other(n) = areasum_other(n) + area(j)
    end do
  end do

  CF_2d (:,:) = 0.e0
  BMF_2d(:,:) = 0.e0
  do k = 1, num_coast
    i = list_coast_x(k)
    j = list_coast_y(k)
    do n = 1, shelf_num
      if ( shelf_2d(i,j) == n) then
        fct = area(j)/areasum_shelf(n)
        CF_2d (i,j) = CF_org (n) * fct
        BMF_2d(i,j) = BMF_org(n) * fct
      endif
    end do
    do n = 1, upscale_num
      if ( shelf_other_2d(i,j) == n) then
        fct = area(j)/areasum_other(n)
        CF_2d (i,j) = CF_org_up (n) * fct
        BMF_2d(i,j) = BMF_org_up(n) * fct
      end if
    end do
  end do

  FWFsum = 0.d0
  do n = 1, shelf_num
    FWFsum = FWFsum + CF_org(n) + BMF_org(n)
  end do
  do n = 1, upscale_num
    FWFsum = FWFsum + CF_org_up(n) + BMF_org_up(n)    
  end do
  write(*,*) 'FWFsum (Gt/yr)= ', FWFsum 

  do k = 1, num_coast
    i = list_coast_x(k)
    j = list_coast_y(k)
    FWF_2d_cnst(i,j) = FWFsum * area(j)/areasum_coast
  end do


  CFsum  = 0.d0
  BMFsum = 0.d0
  FWFsum = 0.d0
  do j = 1, ny
    do i = 1, nx
      CFsum  = CFsum  + CF_2d (i,j)
      BMFsum = BMFsum + BMF_2d(i,j)
      FWFsum = FWFsum + FWF_2d_cnst(i,j)
    end do
  end do
  write(*,*) 'check: CFsum  (Gt/yr)= ', CFsum 
  write(*,*) 'check: BMFsum (Gt/yr)= ', BMFsum
  write(*,*) 'check: FWFsum (Gt/yr)= ', FWFsum 

  CFsum  = 0.d0
  BMFsum = 0.d0
  FWFsum = 0.d0
  do j = 1, ny
    do i = 1, nx
      CF_2d (i,j) = CF_2d (i,j) * Gt_p_yr_to_m3_p_sec
      BMF_2d(i,j) = BMF_2d(i,j) * Gt_p_yr_to_m3_p_sec
      FWF_2d_cnst(i,j) = FWF_2d_cnst(i,j) * Gt_p_yr_to_m3_p_sec
      CFsum  = CFsum  + CF_2d (i,j)
      BMFsum = BMFsum + BMF_2d(i,j)
      FWFsum = FWFsum + FWF_2d_cnst(i,j)
    end do
  end do
  write(*,*) 'CFsum  (Sv)= ', CFsum  * 1.e-6
  write(*,*) 'BMFsum (Sv)= ', BMFsum * 1.e-6
  write(*,*) 'FWFsum (Sv)= ', FWFsum * 1.e-6
end subroutine set_Freshwater_flux
!---------------------------------------------------------------    
subroutine write_binary_data
  integer(4) :: junit

  open(junit,file='data_Antarctica/Ant_ICESat_MODIS_Mask_025x025.dat'  ,form='unformatted', &
       & access='direct',recl = 4 * nx * ny)
  write(junit,rec=1) id_2d
  write(junit,rec=2) scode_2d
  write(junit,rec=3) coast_2d      
  write(junit,rec=4) shelf_2d
  write(junit,rec=5) shelf_other_2d
  write(junit,rec=6) CF_2d
  write(junit,rec=7) BMF_2d
  close(junit)

  open(junit,file='data_Antarctica/CF_025x025.dat',form='unformatted',access='direct',&
       & recl = 4 * nx * ny)
  write(junit,rec=1) CF_2d
  close(junit)

  open(junit,file='data_Antarctica/BMF_025x025.dat',form='unformatted',access='direct',&
       & recl = 4 * nx * ny)
  write(junit,rec=1) BMF_2d
  close(junit)

!  open(junit,file='data_Antarctica/FWFcnst_025x025.dat',form='unformatted',access='direct',&
!       & recl = 4 * nx * ny)
!  write(junit,rec=1) FWF_2d_cnst
!  close(junit)

end subroutine write_binary_data
!---------------------------------------------------------------
end program mk_AntFreshFlux_Depoorter
