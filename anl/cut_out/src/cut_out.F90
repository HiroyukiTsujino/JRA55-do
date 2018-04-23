!-*-F90-*-
!cut_out.F90
!====================================================
!
!  Cut out (x1-x2, y1-y2, z1-z2, var1-var2)
!
!====================================================
program cut_out

  use libmxe_para, only: libmxe_para__register, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register, type_libmxe_grid

  use file_open_close_manager

  implicit none

  integer(4) :: x_num, y_num, z_num, var_num, rec_num
  integer(4) :: x_stt, x_end, y_stt, y_end, z_stt, z_end, var_stt, var_end
  integer(4) :: rec_stt, rec_end

  integer(4) :: x_in, y_in, z_in, var_in
  integer(4) :: x_in_all, iw_core_in, ie_core_in
  integer(4) :: i_half

  character(len=1) :: tuxy_in, tuxy

  real(8) :: lon_stt, lon_end, lat_stt, lat_end, dep_stt, dep_end
  integer(4) :: x_out, y_out, z_out, var_out, rec_out
  integer(4) :: i_append_west, i_append_east

  character(len=256)    :: flin
  character(len=256)    :: flout
  character(len=256)    :: file_namelist_cutout
  character(len=256)    :: fllonlat
  character(len=256)    :: flctl

  real(4), allocatable :: r4in(:, :, :, :)
  real(4), allocatable :: r4in_all(:, :, :, :)
  real(4), allocatable :: r4out(:, :, :, :)
  real(8), allocatable :: lon_out(:)
  real(8), allocatable :: lat_out(:)
  real(8), allocatable :: dep_out(:)

  real(8), allocatable :: lon_org(:)
  real(8), allocatable :: lat_org(:)
  real(8), allocatable :: dep_org(:)

  logical :: l_twolaps
  logical :: l_twolaps_type1
  logical :: l_twolaps_type2
  logical :: l_twolaps_type3
  logical :: l_success

  integer(4) :: mtin, mtot, mtnam, mtlonlat, mtstdout

  integer(4) :: i, j, k, n, t

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid

  real(4) :: undef_in = 0.0e0
  logical :: lmakectl = .true.
  logical :: linear_x = .false.
  logical :: linear_y = .false.
  logical :: linear_z = .false.
  character(len=256) :: varname_tmp(1)
  integer(4) :: ndims_tmp(1)
  real(8),allocatable :: dp_tmp(:)

  character(len=256),parameter :: title = 'MRICOM SNAP SHOT (RESTART) DATA'

  character(len=256),parameter :: fmt_axis_h = 'f10.4'
  character(len=256),parameter :: fmt_axis_v = 'f8.2'
  character(len=256),parameter :: time_dim = 'TDEF 1 LINEAR  15JAN1001 1mo'

  character(len=256),parameter :: opt_general = ' '

  character(len=256),parameter :: opt_hint_ts = ' '
  character(len=256),parameter :: opt_hint_uv = ' '

  character(len=256) :: cundef
  character(len=9)   :: var_short
  character(len=256) :: var_long

  !==========================================

  namelist /nml_cut_out/ &
       &   var_num, rec_num  &
       &   flin,             &
       &   undef_in,         &
       &   tuxy_in,          &
       &   iw_core_in, ie_core_in, &
       &   lon_stt, lon_end, &
       &   lat_stt, lat_end, &
       &   dep_stt, dep_end, & 
       &   var_stt, var_end, &
       &   rec_stt, rec_end, &
       &   i_append_west, i_append_east, &
       &   flout,            &
       &   fllonlat,         &
       &   flctl,            &
       &   lmakectl,         &
       &   var_short, var_long, &
       &   linear_x, linear_y, linear_z

  !------------------------------------------

  call open_file_plain(mtstdout,'cut_out.log')

  call libmxe_para__register(para, file_namelist='NAMELIST.MXE')
  call libmxe_grid__register(grid, para)

  x_num = para%imut
  y_num = para%jmut
  z_num = para%km

  !------------------------------------------

  rec_num = 1
  rec_stt = 1
  rec_end = 1
  i_append_west = 0
  i_append_east = 0

  file_namelist_cutout = 'namelist.cut_out'
  var_short = ''
  var_long = ''
  call open_file_plain(mtnam,file_namelist_cutout)
  read(unit=mtnam, nml=nml_cut_out)
  call close_file(mtnam)

  write(mtstdout,*) ' Input data attributes: '
  write(mtstdout,*) '   x_num    : ', x_num
  write(mtstdout,*) '   y_num    : ', y_num
  write(mtstdout,*) '   z_num    : ', z_num
  write(mtstdout,*) '   var_num  : ', var_num
  write(mtstdout,*) '   rec_num  : ', rec_num
  write(mtstdout,*) '   file in  : ', trim(flin)

  !-------------------------------------------

  write(mtstdout,*) ' Output (cut out) data attributes: '
  write(mtstdout,*) '   x range  : ', lon_stt, '-', lon_end
  write(mtstdout,*) '   y range  : ', lat_stt, '-', lat_end
  if (z_num > 1) then
    write(mtstdout,*) '   z range  :', dep_stt, '-', dep_end
  end if
  write(mtstdout,*) '   i_append_west  :',i_append_west
  write(mtstdout,*) '   i_append_east  :',i_append_east

  if (lon_stt > grid%lonu(ie_core_in)) then
    write(mtstdout,*) ' Error : start longitude ', lon_stt, ' must be smaller than ', grid%lonu(ie_core_in)
    stop
  end if
  if (lon_end < grid%lont(iw_core_in)) then
    write(mtstdout,*) ' Error : end longitude ', lon_end, ' must be larger than ', grid%lont(iw_core_in)
    stop
  end if

  if ((lon_end - lon_stt) > 720.0d0) then
    write(mtstdout,*) ' Error : longitude range ',lon_stt, ' through ', lon_end ,' is wider than the limit 720.0 '
    stop
  end if

  y_in = y_num
  z_in = z_num

  if ((lon_stt < grid%lont(iw_core_in)) .or. (grid%lont(ie_core_in) < lon_end)) then
    l_twolaps = .true.
    x_in = (ie_core_in - iw_core_in + 1)
    x_in_all = (ie_core_in - iw_core_in + 1)*2
    allocate(lon_org(1:x_in_all))

    l_twolaps_type1 = .false.
    l_twolaps_type2 = .false.
    l_twolaps_type3 = .false.

    if ((lon_stt < grid%lonu(iw_core_in)) .and. (lon_end < grid%lonu(ie_core_in))) then
      l_twolaps_type1 = .true.
    end if
    if ((grid%lonu(iw_core_in) < lon_stt) .and. (grid%lonu(ie_core_in) < lon_end)) then
      l_twolaps_type2 = .true.
    end if
    if ((lon_stt < grid%lonu(iw_core_in)) .and. (grid%lonu(ie_core_in) < lon_end)) then
      l_twolaps_type3 = .true.
    end if
  else
    l_twolaps = .false.
    x_in = x_num
    x_in_all = x_num
    allocate(lon_org(1:x_in_all))
  end if


  if (l_twolaps) then
    if (l_twolaps_type1) then
      if ((tuxy_in == 't') .or. (tuxy_in == 'y')) then
        lon_org(1:x_in) = grid%lont(iw_core_in:ie_core_in) - 360.d0
        lon_org(x_in+1:x_in*2) = grid%lont(iw_core_in:ie_core_in)
      else
        lon_org(1:x_in) = grid%lonu(iw_core_in:ie_core_in) - 360.d0
        lon_org(x_in+1:x_in*2) = grid%lonu(iw_core_in:ie_core_in)
      end if
    end if
    if (l_twolaps_type2) then
      if ((tuxy == 't') .or. (tuxy == 'y')) then
        lon_org(1:x_in) = grid%lont(iw_core_in:ie_core_in)
        lon_org(x_in+1:x_in*2) = grid%lont(iw_core_in:ie_core_in) + 360.d0
      else
        lon_org(1:x_in) = grid%lonu(iw_core_in:ie_core_in)
        lon_org(x_in+1:x_in*2) = grid%lonu(iw_core_in:ie_core_in) + 360.d0
      end if
    end if
    if (l_twolaps_type3) then
      i_half = (ie_core_in - iw_core_in + 1) / 2
      if ((tuxy == 't') .or. (tuxy == 'y')) then
        lon_org(1            :i_half     ) = grid%lont(ie_core_in-i_half+1:ie_core_in) - 360.d0
        lon_org(i_half+1     :i_half+x_in) = grid%lont(iw_core_in         :ie_core_in)
        lon_org(i_half+x_in+1:x_in*2     ) = grid%lont(iw_core_in         :iw_core_in+i_half-1) + 360.d0
      else
        lon_org(1            :i_half     ) = grid%lonu(ie_core_in-i_half+1:ie_core_in) - 360.d0
        lon_org(i_half+1     :i_half+x_in) = grid%lonu(iw_core_in         :ie_core_in)
        lon_org(i_half+x_in+1:x_in*2     ) = grid%lonu(iw_core_in         :iw_core_in+i_half-1) + 360.d0
      end if
    end if
  else
    if ((tuxy == 't') .or. (tuxy == 'y')) then
      lon_org(1:x_in_all) = grid%lont(1:x_in_all)
    else
      lon_org(1:x_in_all) = grid%lonu(1:x_in_all)
    end if
  end if

  allocate(lat_org(1:y_num))
  if ((tuxy == 't') .or. (tuxy == 'x')) then
    lat_org(1:y_num) = grid%latt(1:y_num)
  else
    lat_org(1:y_num) = grid%latu(1:y_num)
  end if

  allocate(dep_org(1:z_num))
  dep_org(1:z_num) = grid%depm(1:z_num)

  ! search

  do i = x_in_all, 1, -1
    if (lon_org(i) <= lon_stt) then
      x_stt = i - i_append_west
      exit
    end if
  end do

  do i = 1, x_in_all
    if (lon_end <= lon_org(i)) then
      x_end = i + i_append_east
      exit
    end if
  end do

  do j = y_num, 1, -1
    if (lat_org(j) <= lat_stt) then
      y_stt = j
      exit
    end if
  end do

  do j = 1, y_num
    if (lat_end <= lat_org(j)) then
      y_end = j
      exit
    end if
  end do

  if (z_num > 1) then
    do k = z_num, 1, -1
      if (dep_org(k) <= dep_stt) then
        z_stt = k
        exit
      end if
    end do
    do k = 1, z_num
      if (dep_end <= dep_org(k)) then
        z_end = k
        exit
      end if
    end do
  else
    z_stt=1
    z_end=1
  end if

  if ((x_stt < 1) .or. (x_in_all < x_end)) then
    write(mtstdout,*) ' X range is out of bounds of input data, please check '
    stop
  end if

  if (rec_num < rec_end) then
    write(mtstdout,*) ' Number of records exceeds that of input file, please check '
    stop
  end if

  write(mtstdout,*) '   x range  : ', x_stt, '-', x_end
  write(mtstdout,*) '            : ', lon_org(x_stt), lon_org(x_end)
  write(mtstdout,*) '   y range  : ', y_stt, '-', y_end
  write(mtstdout,*) '            : ', lat_org(y_stt), lat_org(y_end)
  write(mtstdout,*) '   z range  : ', z_stt, '-', z_end
  write(mtstdout,*) '   v range  : ', var_stt, '-', var_end
  write(mtstdout,*) '   file out : ', trim(flout)
  write(mtstdout,*) '   record range  :', rec_stt, '-', rec_end

  x_out = x_end - x_stt + 1
  y_out = y_end - y_stt + 1
  z_out = z_end - z_stt + 1

  var_out = var_end - var_stt + 1
  rec_out = rec_end - rec_stt + 1

  write(mtstdout,*) '   x_out    :', x_out
  write(mtstdout,*) '   y_out    :', y_out
  write(mtstdout,*) '   z_out    :', z_out
  write(mtstdout,*) '   var_out  :', var_out
  write(mtstdout,*) '   rec_out  :', rec_out

  allocate (r4in(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (r4in_all(1:x_in_all, 1:y_num, 1:z_num, 1:var_num))
  allocate (r4out(1:x_out, 1:y_out, 1:z_out, 1:var_out))
  allocate (lon_out(1:x_out))
  allocate (lat_out(1:y_out))
  allocate (dep_out(1:z_out))

  lon_out(1:x_out) = lon_org(x_stt:x_end)
  lat_out(1:y_out) = lat_org(y_stt:y_end)
  dep_out(1:z_out) = dep_org(z_stt:z_end)

  write(mtstdout,'(8f8.3)') (real(lon_out(i),4),i=1,x_out)
  write(mtstdout,'(8f8.3)') (real(lat_out(j),4),j=1,y_out)
  write(mtstdout,'(8f8.3)') (real(dep_out(k),4),k=1,z_out)

  !--------------------------------------------------------------------------------

  call open_file_sequential(mtlonlat, fllonlat, action='write')
  write(mtlonlat) lon_out, lat_out
  if (z_out > 1) then
    write(mtlonlat) dep_out
  end if
  call close_file(mtlonlat)

  call open_file_direct(mtin, flin,  4*x_num*y_num*z_num*var_num, action='read',  l_succeed=l_success)
  if (.not. l_success) stop

  call open_file_direct(mtot, flout, 4*x_out*y_out*z_out*var_out, action='write', l_succeed=l_success)
  if (.not. l_success) stop

  do t = rec_stt, rec_end
  
    read (mtin, rec=t) r4in(:,:,:,:)
  
    if (l_twolaps) then
      if (l_twolaps_type3) then
        r4in_all(1            :i_half     ,:,:,:) = r4in(ie_core_in-i_half+1:ie_core_in         ,:,:,:)
        r4in_all(i_half+1     :i_half+x_in,:,:,:) = r4in(iw_core_in         :ie_core_in         ,:,:,:)
        r4in_all(i_half+x_in+1:x_in*2     ,:,:,:) = r4in(iw_core_in         :iw_core_in+i_half-1,:,:,:)
      else
        r4in_all(1     :x_in,     1:y_num, 1:z_num, 1:var_num) = r4in(iw_core_in:ie_core_in, 1:y_num, 1:z_num, 1:var_num)
        r4in_all(x_in+1:x_in_all, 1:y_num, 1:z_num, 1:var_num) = r4in(iw_core_in:ie_core_in, 1:y_num, 1:z_num, 1:var_num)
      end if
    else
      r4in_all(1:x_in_all, 1:y_num, 1:z_num, 1:var_num) = r4in(1:x_num, 1:y_num, 1:z_num, 1:var_num)
    end if

    r4out(1:x_out, 1:y_out, 1:z_out, 1:var_out) = &
      &  r4in_all(x_stt:x_end, y_stt:y_end, z_stt:z_end, var_stt:var_end)
  
    write(mtot, rec=t-rec_stt+1) r4out(1:x_out, 1:y_out, 1:z_out, 1:var_out)
  
  end do

  call close_file(mtot)
  call close_file(mtin)
  call close_file(mtstdout)

  deallocate(r4in)
  deallocate(r4in_all)
  deallocate(r4out)

  if (lmakectl) then

    write(cundef,'(E12.5)') undef_in

    write(6,*) 'Missing value     = ', trim(cundef)

    write(varname_tmp(1),'(a9,i4,1a,1a)') var_short(1:9), z_out,' 99 ',trim(var_long)
    write(6,'(1a)') trim(varname_tmp(1))

    ndims_tmp(1) = z_out

    call mkgrdctl ( &
         &   flctl, flout, title,                      &
         &   x_out, lon_out, fmt_axis_h, linear_x,     &
         &   y_out, lat_out, fmt_axis_h, linear_y,     &
         &   z_out, dep_out, fmt_axis_v, linear_z,     &
         &   time_dim   ,        1,        1,          &
         &   varname_tmp,  ndims_tmp,                  &
         &   cundef, opt_hint_ts, opt_general          &
         &   )

  end if

end program cut_out
