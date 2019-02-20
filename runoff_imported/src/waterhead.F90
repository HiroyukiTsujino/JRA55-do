!-*-F90-*-
module mod_waterhead
  implicit none
  integer(4),parameter :: nx  =  1440,  ny =   720
  integer(4),parameter :: nnx = 21600, nny = 10800
  integer(4) :: nextx(nx,ny), nexty(nx,ny)
  integer(4) :: outx (nx,ny), outy (nx,ny)
  integer(4) :: headx(nx,ny), heady(nx,ny)
  real(4) :: length(nx,ny)
  integer(4) :: i, j
  integer(1) :: flwdir(nnx,nny)

  type type_flwdir
    integer(4) :: num
    real(8)    :: len
    integer(4) :: ix
    integer(4) :: jy
  end type type_flwdir

  integer(4),parameter :: num_max = 10000
  integer(4) :: lst_max
  type(type_flwdir),allocatable :: lst ( : )


  real(4) :: flw_lat(nny), flw_lon(nnx)
  integer(4) :: iflow = 99, iflow2 = 98

contains
!--------------------------------------------------
subroutine waterhead__ini
  integer(4) :: i, j
  real(8) :: ddx, ddy
  ddx = 1.d0/60.d0
  ddy = 1.d0/60.d0
  do i = 1, nnx
    flw_lon(i) = ddx * (dble(i) - 0.5d0)
  end do
  do j = 1, nny
    flw_lat(j) = ddy * (dble(j) - 0.5d0) - 90.d0
  end do

  lst_max = num_max
  allocate(lst ( lst_max ) )

  open (iflow ,file='data_etc/flw.gs' ,form='formatted',status='replace',position='append')
  open (iflow2,file='data_etc/flw2.gs',form='formatted',status='replace',position='append')
  
end subroutine waterhead__ini
!--------------------------------------------------
subroutine waterhead__main
  integer(4) :: i_src, j_src
  integer(4) :: ii_src, jj_src, ii_head, jj_head
  real(4) :: length_tmp

!  ii_src = 7305  Changjiang River
!  jj_src = 7279

!  do j_src = 486, 486  ! Changjiang River
!    do i_src = 487, 487

!  do j_src = 486, 486
!    do i_src = 128, 128
!  do j_src = 478,478
!    do i_src = 1075,1075
!  do j_src = 479,479
!    do i_src = 1081,1081
!  do j_src = 476,476
!    do i_src = 1083, 1083

  do j_src = 1, ny
    do i_src = 1, nx
      if ( nextx(i_src,j_src) .ne. -9 ) cycle
      ii_src = outx(i_src,j_src)
      jj_src = outy(i_src,j_src)

!      ii_src = 16227
!      jj_src =  7168
!      ii_src = 16257
!      jj_src =  7151
      write(*,*) 'ii_src=', ii_src
      write(*,*) 'jj_src=', jj_src

      call waterhead__each_rivermouth( ii_src, jj_src, ii_head, jj_head, length_tmp )
      write(*,*) length_tmp
      write(*,*) ii_head, jj_head

      headx (i_src,j_src) = ii_head
      heady (i_src,j_src) = jj_head
      length(i_src,j_src) = length_tmp
    end do
  end do
end subroutine waterhead__main
!--------------------------------------------------p
subroutine waterhead__each_rivermouth ( ii_src, jj_src, ii_head, jj_head, length_arg )
  
  integer(4),intent(in)  :: ii_src , jj_src 
  integer(4),intent(out) :: ii_head, jj_head
  real(4) :: length_arg
  integer(4) :: i_org, j_org, n_org, num_org
  integer(4) :: i_nxt, j_nxt
  integer(4) :: n, idir
  logical :: l_nxt, lfirst, ldown
  integer(4) ::  nmax, nmax_org
  real(8) :: lon_org, lat_org, lon_nxt, lat_nxt
  real(8) :: len_org, len_max

  type(type_flwdir) :: lst_waterhead


!  if ( ii_src == 19127 .and. jj_src == 711 ) return
!  if ( ii_src == 19270 .and. jj_src == 706 ) return
!  if ( ii_src == 18403 .and. jj_src == 813 ) return

  nmax = 1
  len_max = 0.d0
  n = 1


  lst(n)%num = 1
  lst(n)%len = 0.d0
  lst(n)%ix  = ii_src
  lst(n)%jy  = jj_src

  outerloop: do 

    n_org = n
    i_org = lst(n_org)%ix
    j_org = lst(n_org)%jy
    lon_org = flw_lon(i_org)
    lat_org = flw_lat(j_org)
    num_org = lst(n_org)%num
    len_org = lst(n_org)%len

    n = n - 1
    do idir = 1, 8
      ldown = .false.
      call next_flwdir( ldown, i_org, j_org, idir, l_nxt, i_nxt, j_nxt)
      if (l_nxt) then
        n = n + 1
        if (n > lst_max ) then
          write(*,*) 'lst num exceeds ', lst_max          
          stop
        end if
        lst(n)%num = num_org + 1
        lst(n)%ix  = i_nxt
        lst(n)%jy  = j_nxt
        lon_nxt = flw_lon(i_nxt)
        lat_nxt = flw_lat(j_nxt)
        lst(n)%len = len_org + distance_m (lon_org,lat_org,lon_nxt,lat_nxt)

        if ( lst(n)%num > nmax ) then
!          lst_waterhead = lst(n)
          nmax = max(lst(n)%num, nmax)
        end if
        if ( lst(n)%len > len_max ) then
          lst_waterhead = lst(n)
          len_max = max(lst(n)%len, len_max)
        end if
        if ( j_nxt == 1) then
          write(*,*) 'cross south pole'
          lst_waterhead = lst(n)      
          nmax = max(lst(n)%num, nmax)    
          exit outerloop
!          stop
        end if
!        write(*,*) n, lst(n)%num, n_org
      end if
    end do

    if ( nmax > num_max ) then
      write(*,*) 'nmax exceeds ', num_max
      write(*,*) 'ii_src,jj_src', ii_src, jj_src
      stop
    end if

    if (n == 0) then
      write(*,*) 'waterhead:', lst_waterhead%num, lst_waterhead%ix, lst_waterhead%jy
      ii_head = lst_waterhead%ix
      jj_head = lst_waterhead%jy
      length_arg = lst_waterhead%len
      exit
    end if
  end do outerloop


!-------------------- 河道描画
  ldown = .true.
  i_org = lst_waterhead%ix
  j_org = lst_waterhead%jy
!  if ( nmax < 300 ) return
  if (len_max < 1000.d3 ) return ! 1000km 以上の河川のみ描く
!  if (len_max < 500.d3 ) return ! 500km 以上の河川のみ描く
  do
    nmax = nmax - 1
    lon_org = flw_lon(i_org)
    lat_org = flw_lat(j_org)
    do idir = 1, 8
      call next_flwdir ( ldown, i_org, j_org, idir, l_nxt, i_nxt, j_nxt)
      if (l_nxt) then
        lon_nxt = flw_lon(i_nxt)
        lat_nxt = flw_lat(j_nxt)
        i_org = i_nxt
        j_org = j_nxt
        exit
      end if
    end do
    if ( lon_org < 1.d0   .and. lon_nxt > 359.d0 ) cycle
    if ( lon_org > 359.d0 .and. lon_nxt < 1.d0   ) cycle
    if ( nmax >  0 ) then
      write(iflow, '(A,4F13.6,A)') '"drawline', real(lon_org,4), real(lat_org,4), real(lon_nxt,4), real(lat_nxt,4),'"'
    else
      write(iflow2, '(A,4F13.6,A)') '"drawline', real(lon_org,4), real(lat_org,4), real(lon_nxt,4), real(lat_nxt,4),'"'
    end if
!    write(*,*) i_nxt, j_nxt, flwdir(i_nxt,j_nxt)
    i_org = i_nxt
    j_org = j_nxt
!    if ( nmax == 0 .or.  i_org == ii_src .and. j_org == jj_src) exit
    if ( nmax < - 600) exit
  end do

end subroutine waterhead__each_rivermouth
!--------------------------------------------------
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
subroutine next_flwdir ( ldown, i, j, idir, l_next, i_next, j_next)
  logical,intent(in)     :: ldown
  integer(4),intent(in)  :: i, j, idir
  logical,intent(out)    :: l_next
  integer(4),intent(out) :: i_next, j_next

  integer(4) :: iw, ie, jn, js
  call news ( iw, ie, jn, js, i, j, nnx, nny)

  l_next = .false.

  if ( ldown ) then
    if (idir == 1 .and. flwdir(i,j) == idir) then
      i_next = i
      j_next = jn
      l_next = .true.
    else if (idir == 2 .and. flwdir(i,j) == idir) then
      i_next = ie
      j_next = jn
      l_next = .true.
    else if (idir == 3 .and. flwdir(i,j) == idir) then
      i_next = ie
      j_next = j
      l_next = .true.
    else if (idir == 4 .and. flwdir(i,j) == idir) then
      i_next = ie
      j_next = js
      l_next = .true.
    else if (idir == 5 .and. flwdir(i,j) == idir) then
      i_next = i
      j_next = js
      l_next = .true.
    else if (idir == 6 .and. flwdir(i,j) == idir) then
      i_next = iw
      j_next = js
      l_next = .true.
    else if (idir == 7 .and. flwdir(i,j) == idir) then
      i_next = iw
      j_next = j
      l_next = .true.
    else if (idir == 8 .and. flwdir(i,j) == idir) then
      i_next = iw
      j_next = jn
      l_next = .true.
    end if
  else
    if (idir == 1 .and. flwdir(i,js) == idir) then  ! 
      i_next = i
      j_next = js
      l_next = .true.
    else if (idir == 2 .and. flwdir(iw,js) == idir) then  ! 
      i_next = iw
      j_next = js
      l_next = .true.
    else if (idir == 3 .and. flwdir(iw, j) == idir) then  ! 
      i_next = iw
      j_next = j
      l_next = .true. 
    else if (idir == 4 .and. flwdir(iw,jn) == idir) then  ! 
      i_next = iw
      j_next = jn
      l_next = .true. 
    else if (idir == 5 .and. flwdir(i ,jn) == idir) then  ! 
      i_next = i
      j_next = jn
      l_next = .true. 
    else if (idir == 6 .and. flwdir(ie,jn) == idir) then  ! 
      i_next = ie
      j_next = jn
      l_next = .true.
    else if (idir == 7 .and. flwdir(ie,j ) == idir) then  ! 
      i_next = ie
      j_next = j
      l_next = .true.
    else if (idir == 8 .and. flwdir(ie,js) == 8) then  ! 
      i_next = ie
      j_next = js
      l_next = .true.
    end if
  end if
end subroutine next_flwdir
!--------------------------------------------------
subroutine waterhead__read
 open(11,file='data_etc/nextxy_big_endian_noyrev_lon0strt.bin',form='unformatted',access='direct',&
       & recl = 4 * nx * ny)
  read(11,rec=1) nextx
  read(11,rec=2) nexty
  close(11)
  
  open(11,file='data_etc/out_xy_big_endian_noyrev_lon0strt.bin',form='unformatted',access='direct',&
       & recl = 4 * nx * ny)
  read(11,rec=1) outx
  read(11,rec=2) outy


  open(11,file='data_etc/flwdir_big_endian_noyrev_lon0strt.bin',form='unformatted',access='direct',&
       & recl = nnx*nny)
  read(11,rec=1) flwdir
  close(11)
end subroutine waterhead__read
!--------------------------------------------------
subroutine waterhead__write

  open(11,file='data_etc/waterhead_xy_big_endian_noyrev_lon0strt.bin',form='unformatted',access='direct',&
       & recl = 4 * nx * ny)
  write(11,rec=1) headx
  write(11,rec=2) heady
  write(11,rec=3) length
end subroutine waterhead__write
!--------------------------------------------------

real(8) function distance_m (lon0,lat0,lon1,lat1)
  implicit none
  real(8),intent(in) :: lon0,lat0,lon1,lat1
  real(8) :: big_len = 9.99e33
  real(8) :: radius = 6375.d3
  real(8),parameter :: pi = 3.14159265358979323846264338327920d0
  real(8),parameter :: radian_r = pi / 180.d0
  real(8),parameter :: radian   = 180.d0 / pi
  real(8) :: x0,y0,x1,y1,a
  real(8) :: eps = 1.d-8

  if ( lon0 == lon1 .and. lat0 == lat1 ) then
    distance_m = big_len
  else
    !-- degree to radian --
    x0 = lon0 * radian_r
    y0 = lat0 * radian_r
    x1 = lon1 * radian_r
    y1 = lat1 * radian_r

    !-- great circle in radian --
    a =cos(y0)*cos(y1)*cos(x1-x0)+ sin(y0)*sin(y1)
    if ( abs(a-1.d0) <= eps ) then
      distance_m = eps * radius
    else
      distance_m = acos( cos(y0)*cos(y1)*cos(x1-x0) &
         & + sin(y0)*sin(y1) ) * radius
    end if
  end if
end function distance_m
end module mod_waterhead
