! -*-F90-*-
module rivermouth
  implicit none
  integer(4),parameter :: nx_flwdir = 360 * 60, ny_flwdir = 180 * 60 ! FLOW DIRECTION
  integer(4),parameter :: nx_roforg = 360 *  4, ny_roforg = 180 *  4 ! RUNOFF data
  integer(4),parameter :: nx        = 360 *  4, ny        = 180 *  4 ! WOA025 or OGCM

  integer(4),parameter :: lun = 11

! OGCM or WOA                                     
  character(len=128) :: file_index="mask.gd"      
  integer(4),save :: grid_index(nx, ny)           
  real(8),save :: lon_sw(nx+1,ny+1), lat_sw(nx+1,ny+1)
  real(8),save :: lon_c (nx,ny),     lat_c (nx,ny)
  integer(4),save :: num_coast, coast_x(nx*ny),coast_y(nx*ny)

  integer(4),parameter:: i_coast=1
  integer(4),parameter:: i_land =2
  integer(4),parameter:: i_ocean=0

! runoff data
  character(len=128) :: file_riverindex="data_runoff_in/river_CaMaFlood/nextxy_wo_lake_big_endian_noyrev_lon0strt_rm_GA.bin"
  character(len=128) :: file_mask_Grn  ="data_runoff_in/river_Greenland_Bamber_2012/mask_greenland_bamber_2012.gd"
  integer(4),save :: rof_mask (nx_roforg,ny_roforg)
  integer(4),save :: rof_index(nx_roforg,ny_roforg)
  integer(4) :: rivmouth_val = -9
  integer(4) :: rivmouth_Grn = -888

  character(len=128) :: file_headxy="data_runoff_in/river_CaMaFlood/waterhead_xy_big_endian_noyrev_lon0strt.bin"
  integer(4) :: i2d(nx_roforg,ny_roforg)
  integer(4),save :: rof_headx(nx_roforg,ny_roforg)
  integer(4),save :: rof_heady(nx_roforg,ny_roforg)
  real(4),save :: rof_length(nx_roforg,ny_roforg)

  integer(4),save :: rivermouth_x(nx_roforg,ny_roforg)
  integer(4),save :: rivermouth_y(nx_roforg,ny_roforg)

  real(8),save :: lon_rof(nx_roforg), lat_rof(ny_roforg)

  character(len=128) :: file_rivermouth="rivermouth.bin"

! flwdir
  character(len=128) :: file_flwdir='data_runoff_in/river_CaMaFlood/flwdir_big_endian_noyrev_lon0strt.bin'
  integer(1),save :: flwdir(nx_flwdir,ny_flwdir)  
  real(8),save :: lon_flwdir(nx_flwdir), lat_flwdir(nx_flwdir)
  
contains
!-----------------------------------------------------------------------------
subroutine ini_OGCM
  integer(4) :: i, j
  open(lun,file=file_index,form='unformatted',action='read' &
       &    , access='direct',recl=4*nx*ny)
  read(lun,rec=1) grid_index
  close(lun)  

! Spherical 0.25 x 0.25 grid
  do j = 1, ny+1
    do i = 1, nx+1
      lon_sw(i,j) = 0.25d0*dble(i-1)
      lat_sw(i,j) = -90.d0 + 0.25d0*dble(j-1)
    end do
  end do
  do j = 1, ny
    do i = 1, nx
      lon_c (i,j) = 0.25d0*(dble(i)-0.5d0)
      lat_c (i,j) = -90.d0 + 0.25d0*(dble(j)-0.5d0)
    end do
  end do

  num_coast = 0
  do j = 1, ny
    do i = 1, nx
      if ( grid_index(i,j) == i_coast ) then
        num_coast = num_coast + 1
        coast_x(num_coast) = i
        coast_y(num_coast) = j
      end if
    end do
  end do

end subroutine ini_OGCM
!-----------------------------------------------------------------------------
subroutine ini_river
  integer(4) :: i, j

  open(lun,file=file_riverindex,form='unformatted',action='read' &
       &    , access='direct',recl=4*nx_roforg*ny_roforg)
  read(lun,rec=1) rof_index
  close(lun)  

  open(lun,file=file_mask_Grn,form='unformatted',action='read' &
       &    , access='direct',recl=4*nx_roforg*ny_roforg)
  read(lun,rec=1) i2d
  close(lun)  

  do j = 1, ny_roforg
    do i = 1, nx_roforg
      if ( i2d(i,j) == 1 ) rof_index(i,j) = rivmouth_Grn
    end do
  end do

  do j = 1, ny_roforg
    do i = 1, nx_roforg
      if ( rof_index (i,j) == rivmouth_val ) then
        rof_mask(i,j) = 1
      end if
    end do
  end do

  do i = 1, nx_roforg
    lon_rof(i) = 1.d0/4.d0 * (dble(i)-0.5d0)
  end do
  do j = 1, ny_roforg
    lat_rof(j) = -90.d0 + 1.d0/4.d0 * (dble(j)-0.5d0)
  end do
    
  do i = 1, nx_flwdir
    lon_flwdir(i) = 1.d0/60.d0 * (dble(i)-0.5d0)
  end do
  do j = 1, ny_flwdir
    lat_flwdir(j) = -90.d0 + 1.d0/60.d0 * (dble(j)-0.5d0)
  end do

  open(lun,file=file_headxy,form='unformatted',action='read' &
       &    , access='direct',recl=4*nx_roforg*ny_roforg)
  read(lun,rec=1) rof_headx
  read(lun,rec=2) rof_heady
  read(lun,rec=3) rof_length
  close(lun)

  open(lun,file=file_flwdir,form='unformatted',action='read' &
       &    , access='direct',recl=nx_flwdir*ny_flwdir)
  read(lun,rec=1) flwdir
  close(lun)
  rivermouth_x(:,:) = 0
  rivermouth_y(:,:) = 0
  
end subroutine ini_river
!-----------------------------------------------------------------------------
subroutine main
  use location
  integer(4) :: i, j
  integer(4) :: i_src,  j_src
  integer(4) :: ii_head, jj_head, ii, jj
  real(8) :: lon_now, lat_now, lon_prev, lat_prev
  integer(4) :: initial_ij(2)
  integer(4) :: npas
  integer(4),parameter :: max_npas = 20000


  open(lun,file='flowpath.gs',form='formatted')

  do j_src = 1, ny_roforg
    i_srcloop: do i_src = 1, nx_roforg 
      if ( rof_mask(i_src,j_src) == 0 ) cycle i_srcloop

      ii_head = rof_headx(i_src,j_src)
      jj_head = rof_heady(i_src,j_src)

      if (ii_head == 0 .or. jj_head == 0) then
        call seek_nearest_coast(i_src, j_src)
        cycle i_srcloop
      end if

      ! Start from the waterhead
      ii = ii_head
      jj = jj_head

      lon_now = lon_flwdir(ii)
      lat_now = lat_flwdir(jj)

      call location__seek_grid_curv( nx, ny, lon_sw, lat_sw, lon_now, lat_now, &
           & i, j)

      if ( grid_index(i,j) == i_coast ) then
        rivermouth_x(i_src,j_src) = i
        rivermouth_y(i_src,j_src) = j
        cycle i_srcloop
      else if ( grid_index(i,j) == i_land ) then
!        write(*,*) 'B', i_src, j_src
        initial_ij(1) = i
        initial_ij(2) = j
        do npas = 1, max_npas
          lon_prev = lon_now
          lat_prev = lat_now
          call next_iijj( ii, jj)
          lon_now  = lon_flwdir(ii)
          lat_now  = lat_flwdir(jj)
          call location__seek_grid_curv( nx, ny, lon_sw, lat_sw, lon_now, lat_now, &
           & i, j, initial_ij)
          if ( abs(lon_now - lon_prev) < 180.d0 ) then
!            if ( rof_length(i_src,j_src) >= 1000.d3 ) then
              write(lun, '(A,4F13.6,A)') '"drawline ',lon_prev,lat_prev,lon_now,lat_now,'"'
!            end if
          end if
          if ( grid_index(i,j) == i_coast ) then
            rivermouth_x(i_src,j_src) = i
            rivermouth_y(i_src,j_src) = j          
!            write(*,*) 'B',i_src,j_src,rivermouth_x(i_src,j_src),rivermouth_y(i_src,j_src)
            cycle i_srcloop
          else
            initial_ij(1) = i
            initial_ij(2) = j
          endif
        end do
      else ! The waterhead is not on the land or the coast!
           ! Then seek a coast grid that is closest to the i_src,j_src point.
        call seek_nearest_coast(i_src, j_src)
!        write(*,*) 'C',i_src,j_src,rivermouth_x(i_src,j_src),rivermouth_y(i_src,j_src)
        
        cycle i_srcloop
      end if
      write(*,*) 'ERROR', i_src,j_src 
      stop
    end do i_srcloop
  end do
  
end subroutine main
!-----------------------------------------------------------------------------
subroutine write_result
  open(lun,file='file_rivermouth',form='unformatted',action='write' &
       & ,  access='direct',recl=4*nx_roforg*ny_roforg)
  write(lun,rec=1) rivermouth_x
  write(lun,rec=2) rivermouth_y
  close(lun)
end subroutine write_result
!-----------------------------------------------------------------------------
subroutine seek_nearest_coast (i_arg, j_arg)
  use distance
  integer(4),intent(in) :: i_arg, j_arg
  real(8) :: min_dist, dist
  integer(4) :: n
  min_dist = 999.d0
  do n = 1, num_coast
    dist = distance_rad(lon_rof(i_arg),lat_rof(j_arg), &
         & lon_c(coast_x(n),coast_y(n)), &
         & lat_c(coast_x(n),coast_y(n)))
    if ( dist < min_dist) then
      min_dist = dist
      rivermouth_x(i_arg,j_arg) = coast_x(n)
      rivermouth_y(i_arg,j_arg) = coast_y(n) 
    end if
  end do
    
end subroutine seek_nearest_coast
!-----------------------------------------------------------------------------
subroutine next_iijj ( ii_arg, jj_arg )
  integer(4),intent(inout) :: ii_arg, jj_arg
  
  integer(4) :: iw, ie, jn, js
  call news ( iw, ie, jn, js, ii_arg, jj_arg, nx_flwdir, ny_flwdir)
  
  !  8 1 2 
  !  7 0 3
  !  6 5 4
  if (flwdir(ii_arg,jj_arg) == 1) then  ! 
    jj_arg = jn
  else if (flwdir(ii_arg,jj_arg) == 2) then  ! 
    ii_arg = ie
    jj_arg = jn
  else if (flwdir(ii_arg,jj_arg) == 3) then  ! 
    ii_arg = ie
  else if (flwdir(ii_arg,jj_arg) == 4) then  ! 
    ii_arg = ie
    jj_arg = js
  else if (flwdir(ii_arg,jj_arg) == 5) then  ! 
    jj_arg = js
  else if (flwdir(ii_arg,jj_arg) == 6) then  ! 
    ii_arg = iw
    jj_arg = js
  else if (flwdir(ii_arg,jj_arg) == 7) then  ! 
    ii_arg = iw
  else if (flwdir(ii_arg,jj_arg) == 8) then  ! 
    ii_arg = iw
    jj_arg = jn
  end if
end subroutine next_iijj
!---------------------------------------------------------------------------------
subroutine news ( iw, ie, jn, js, &
                 & i,  j, nx_arg, ny_arg )
  implicit none
  integer, intent(out) :: iw, ie, jn, js
  integer, intent(in)  :: i,  j,  nx_arg, ny_arg

  if (i == 1) then
    iw = nx_arg
  else
    iw = i - 1
  end if

  if (i == nx_arg) then
    ie = 1
  else
    ie = i + 1
  end if

  if (j == 1) then
    js = 1
  else
    js = j - 1
  end if
  if (j == ny_arg) then
    jn = ny_arg
  else
    jn = j + 1
  end if 

  return
end subroutine news
!--------------------------------------------------  
    
end module rivermouth
