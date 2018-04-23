! -*-F90-*-
!==================================================================
program mkgrads_icecat_restart

  !----------------------------------------------------------------
  !  GrADS data from OGCM output for snap shot categorized sea ice
  !----------------------------------------------------------------

  use basin_param
  use grid_common

  implicit none

  integer(4), parameter :: ncat = 5

  real(8), parameter :: catdim(ncat+1) = (/ &
       & 0.0, 1.0, 2.0, 3.0, 4.0, 5.0 /)

#include "icecat_restart_item.F90"

  !----------------------------------------------------------------

  integer(4), parameter :: nitem = (ncat + 1) * 9 + 10
  integer(4) :: nstep0, month, iday, ihour, imin
  real(8) :: aihour
  
  real(8) :: r2din (imut,jmut,nitem)
  real(4) :: r2dout(imut,jmut,nitem)

  character(len=256) :: file_in
  character(len=256) :: file_out

  character(len=256) :: file_grid, file_topo

  character(len=256) :: file_ctl_ts, file_ctl_uv

  character(len=256),parameter :: title_ts=" OImon (TS) "
  character(len=256),parameter :: title_uv=" OImon (UV) "

  character(len=256),parameter :: fmt_axis = 'f8.3'
  character(len=256),parameter :: time_dim = 'TDEF 1 LINEAR  15JAN1001 1mo'

  character(len=256),parameter :: opt_general = ' '
!  character(len=256),parameter :: opt_general = 'template'

  character(len=256),parameter :: opt_hint_ts = ' '
  character(len=256),parameter :: opt_hint_uv = ' '
!  character(len=256),parameter :: opt_hint_ts = 'PDEF 364 368 BILIN STREAM BINARY-BIG ^pdef.masku.bilin.gd'
!  character(len=256),parameter :: opt_hint_uv = 'PDEF 364 368 BILIN STREAM BINARY-BIG ^pdef.maskt.bilin.gd'

  character(len=256) :: cundef
  
  integer(4) :: i, j, m, n, k
  integer(4) :: ireco, ios
  integer(4) :: item

  real(4) :: rundef_model = -999.0
!  real(4) :: rundef = 0.0

  logical :: lmakectl
  logical :: linear_x = .false.
  logical :: linear_y = .false.
  logical :: linear_z = .false.

  !----------------------------------------------------------------
  
  namelist /nictrst/ file_grid, file_topo, file_in, &
       & file_out, file_ctl_ts, file_ctl_uv, &
       & lmakectl, linear_x, linear_y, linear_z
  read(5,nictrst) 

  !----------------------------------------------------------------

  write(cundef,'(F10.5)') rundef_model

  write(6,*) 'Grid spacing from = ', trim(file_grid)
  write(6,*) 'input from        = ', trim(file_in)
  write(6,*) 'output to         = ', trim(file_out)
  write(6,*) '    missing value = ', trim(cundef)

  call setgrd(file_topo, file_grid)
  
  !----------------------------------------------------------------

  open(11, file=file_in,form='unformatted',action='read')
  open(12, file=file_out,form='unformatted',access='direct',recl=4*imut*jmut)
  ireco = 0

  !----------------------------------------------------------------

  read(11,end=9999) nstep0, month, iday, ihour, imin, aihour
  write(6,*) nstep0, month, iday, ihour, imin, aihour

  do m = 1, nitem
    write(6,*) ' m = ', m
    read(11) r2din(1:imut,1:jmut,m)
  end do

  do m = 1, nitem
    !r2dout(1:imut,1:jmut,m) = rundef_model
    !where (r2din(1:imut,1:jmut,(ncat+1)*9+1) /= 0.0d0) ! a0iceo /= 0
    r2dout(1:imut,1:jmut,m) = r2din(1:imut,1:jmut,m)
    !end where
  end do

  item = 0

  do k = 1, ncat + 1
    do m = 1, 9
      item = item + 1
      ireco = (ncat + 1) * (m - 1) + k
      write(6,*) ' item = ', item, ' record = ', ireco
      write(12,rec=ireco) r2dout(1:imut,1:jmut,item)
    end do

  end do

  do m = 1, 10
    ireco = m + (ncat + 1) * 9
    write(12,rec=ireco) r2dout(1:imut,1:jmut,ireco)
  end do

9999 continue

  !----------------------

  if (lmakectl) then

    call mkgrdctl ( &
         &   file_ctl_ts, file_out, title_ts,              &
         &   imut, alont, fmt_axis, linear_x,              &
         &   jmut, alatt, fmt_axis, linear_y,              &
         &   ncat+1, catdim, fmt_axis, linear_z,           &
         &   time_dim,   nitems,     nitems,               &
         &   varname,    ndims,                            &
         &   cundef, opt_hint_ts, opt_general              &
         &   )

    call mkgrdctl ( &
         &   file_ctl_uv, file_out, title_uv,              &
         &   imut, alonu, fmt_axis, linear_x,              &
         &   jmut, alatu, fmt_axis, linear_y,              &
         &   ncat+1, catdim, fmt_axis, linear_z,           &
         &   time_dim,   nitems,    nitems,                &
         &   varname,    ndims,                            &
         &   cundef, opt_hint_uv, opt_general              &
         &   )

  end if

end program mkgrads_icecat_restart
