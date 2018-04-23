! -*-F90-*-
!=========================================================================
program mkgrads_ice_restart

  !----------------------------------------------------------
  !  Make GrADS data from OGCM output for snap shot sea ice
  !----------------------------------------------------------

  use basin_param
  use grid_common

  implicit none

  real(8) :: dat2(imut,jmut,10)
  real(8) :: dat8(imut,jmut)
  real(4) :: dat4(imut,jmut)

  real(8) :: dp_dmy(1) = 0.0

  integer(4) :: i, j, m, n, irec, inum

  integer(4) :: NKAI,MONTH,IDAY,IHOUR,IMIN

  integer(4), parameter :: mtin=77, mtout=88

  character(len=256) :: file_in, file_out

#include "ice_restart_item.F90"

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
  
  real(4) :: rundef_model = -999.0
!  real(4) :: rundef = 0.0

  logical :: lmakectl
  logical :: linear_x = .false.
  logical :: linear_y = .false.
  logical :: linear_z = .false.

  !----------------------------------------------------------------
  
  namelist /nicerst/ file_grid, file_topo, file_in, &
       & file_out, file_ctl_ts, file_ctl_uv, &
       & inum, lmakectl, linear_x, linear_y, linear_z
  read(5,nicerst) 

  !----------------------------------------------------------------

  write(cundef,'(F10.5)') rundef_model

  write(6,*) 'Grid spacing from = ', trim(file_grid)
  write(6,*) 'input from        = ', trim(file_in)
  write(6,*) '    missing value = ', trim(cundef)

  call setgrd(file_topo, file_grid)
  
  !----------------------------------------------------------------

  write(*,*) 'reading ', inum ,' data'

  open(mtin, file=file_in,form='unformatted')
  write(*,*) 'reading from ... ', trim(file_in)

  open(mtout, file=file_out,form='unformatted',access='direct',recl=4*imut*jmut)
  write(*,*) 'output to ... ', trim(file_out)
  irec = 0

  !----------------------------------------------------------

  irec = 1

  do m = 1, inum

    read (mtin,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
    write(*,*) NKAI, MONTH, IDAY, IHOUR, IMIN

    read (mtin,end=999) dat2(1:imut,1:jmut,1:10)
    do n = 1, 10
      do j = 1, jmut
        do i = 1, imut
          dat4(i,j) = dat2(i,j,n)
        end do
      end do
      write ( mtout, rec=irec ) dat4
      irec = irec + 1
    end do

    read (mtin,end=999) dat2(1:imut,1:jmut,1:3)
    do n = 1, 3
      do j = 1, jmut
        do i = 1, imut
          dat4(i,j) = dat2(i,j,n)
        end do
      end do
      write ( mtout, rec=irec ) dat4
      irec = irec + 1
    end do

999 continue

  end do

  close ( mtin )
  close ( mtout )

  !----------------------

  if (lmakectl) then

    call mkgrdctl ( &
         &   file_ctl_ts, file_out, title_ts,              &
         &   imut, alont, fmt_axis, linear_x,              &
         &   jmut, alatt, fmt_axis, linear_y,              &
         &     1, dp_dmy, fmt_axis, linear_z,              &
         &   time_dim,   nitems,     nitems,               &
         &   varname,    ndims,                            &
         &   cundef, opt_hint_ts, opt_general              &
         &   )

    call mkgrdctl ( &
         &   file_ctl_uv, file_out, title_uv,              &
         &   imut, alonu, fmt_axis, linear_x,              &
         &   jmut, alatu, fmt_axis, linear_y,              &
         &     1, dp_dmy, fmt_axis, linear_z,              &
         &   time_dim,   nitems,    nitems,                &
         &   varname,    ndims,                            &
         &   cundef, opt_hint_uv, opt_general              &
         &   )

  end if

end program mkgrads_ice_restart
