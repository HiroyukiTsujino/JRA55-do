! -*-F90-*-
!=========================================================================
program mkgrads_hist_ocean
  !-----------------------------------------------------------------------
  !     Make GrADS data from OGCM output
  !-----------------------------------------------------------------------

  use basin_param
  use grid_common

  implicit none

  real(4) :: dat3(imut,jmut,km)
  real(4) :: dat2(imut,jmut)

  integer(4) :: i, j, k, n

  integer(4) :: NKAI, MONTH, IDAY, IHOUR, IMIN

#include "ocean_hist_item.F90"

  integer(4), parameter :: mtin1=71, mtin2=72, mtin3=73, mtin4=74, mtin5=75
  integer(4), parameter :: mtot1=81, mtot2=82, mtot3=83, mtot4=84, mtot5=85

  integer(4) :: irec1, irec2, irec3, irec4, irec5
  integer(4) :: inum

  logical :: lsplit

  character(len=256) :: file_in
  character(len=256) :: file_grid, file_topo

  character(len=256) :: file_in_u, file_in_v, file_in_t, file_in_s, file_in_h
  character(len=256) :: file_out_u, file_out_v, file_out_t, file_out_s, file_out_h
  character(len=256) :: file_ctl_u, file_ctl_v, file_ctl_t, file_ctl_s, file_ctl_h

  real(4) :: undef = 0.0

  logical :: lmakectl
  logical :: linear_x = .false.
  logical :: linear_y = .false.
  logical :: linear_z = .false.

  character(len=256) :: varname_tmp(1)
  integer(4) :: ndims_tmp(1)

  character(len=256),parameter :: title = 'MRICOM MEAN DATA'

  character(len=256),parameter :: fmt_axis_h = 'f8.3'
  character(len=256),parameter :: fmt_axis_v = 'f8.2'
  character(len=256),parameter :: time_dim = 'TDEF 1 LINEAR  15JAN1059 1mo'

  character(len=256),parameter :: opt_general = ' '
!  character(len=256),parameter :: opt_general = 'template'

  character(len=256),parameter :: opt_hint_ts = ' '
  character(len=256),parameter :: opt_hint_uv = ' '
!  character(len=256),parameter :: opt_hint_ts = 'PDEF 364 368 BILIN STREAM BINARY-BIG ^pdef.masku.bilin.gd'
!  character(len=256),parameter :: opt_hint_uv = 'PDEF 364 368 BILIN STREAM BINARY-BIG ^pdef.maskt.bilin.gd'

  character(len=256) :: cundef
  !---------------------------------------------------------------------
  
  namelist /nocean/ file_grid, file_topo, &
       & lsplit, &
       & file_in, &
       & file_in_u, file_in_v, file_in_t, file_in_s, file_in_h, &
       & file_out_u, file_out_v, file_out_t, file_out_s, file_out_h, &
       & file_ctl_u, file_ctl_v, file_ctl_t, file_ctl_s, file_ctl_h, &
       & inum, undef, lmakectl, linear_x, linear_y, linear_z
  read(5,nocean) 

  !---------------------------------------------------------------------

  call setgrd(file_topo, file_grid)

  write(cundef,'(E12.5)') undef

  write(6,*) 'Grid spacing from = ', trim(file_grid)
  write(6,*) 'Topography from   = ', trim(file_topo)
  write(6,*) 'Missing value     = ', trim(cundef)

  !----------------------------------------------------------------

  write(*,*) 'reading ', inum ,' data'

  if (lsplit) then

    open(mtin1, file=file_in_u,form='unformatted',action='read')
    write(*,*) 'input (U) to ... ', trim(file_in_u)

    open(mtin2, file=file_in_v,form='unformatted',action='read')
    write(*,*) 'input (V) to ... ', trim(file_in_v)

    open(mtin3, file=file_in_t,form='unformatted',action='read')
    write(*,*) 'input (T) to ... ', trim(file_in_t)

    open(mtin4, file=file_in_s,form='unformatted',action='read')
    write(*,*) 'input (S) to ... ', trim(file_in_s)

    open(mtin5, file=file_in_h,form='unformatted',action='read')
    write(*,*) 'input (SSH) to ... ', trim(file_in_h)

  else

    open(mtin1, file=file_in,form='unformatted',action='read')
    write(*,*) 'reading from ... ', trim(file_in)

  end if

  !-------

  open(mtot1, file=file_out_u,form='unformatted',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (U) to ... ', trim(file_out_u)
  irec1 = 0

  open(mtot2, file=file_out_v,form='unformatted',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (V) to ... ', trim(file_out_v)
  irec2 = 0

  open(mtot3, file=file_out_t,form='unformatted',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (T) to ... ', trim(file_out_t)
  irec3 = 0

  open(mtot4, file=file_out_s,form='unformatted',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (S) to ... ', trim(file_out_s)
  irec4 = 0

  open(mtot5, file=file_out_h,form='unformatted',access='direct',recl=4*imut*jmut)
  write(*,*) 'output (SSH) to ... ', trim(file_out_h)
  irec5 = 0

  !------------------------------------------------------------------------

  do n = 1, inum

    ! U  

    read (mtin1,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
    write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
    read (mtin1,end=999) dat3

    do k = 1, km

      do j = 1, jmut
        do i = 1, imut
          dat2(i,j) = aexl(i,j,k) * dat3(i,j,k) + (1.0d0 - aexl(i,j,k)) * undef
        end do
      end do

      irec1 = irec1 + 1

      write(mtot1,rec=irec1) dat2

    end do

    ! V

    if (lsplit) then
      read (mtin2,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
      write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
      read (mtin2,end=999) dat3
    else
      read (mtin1,end=999) dat3
    end if

    do k = 1, km

      do j = 1, jmut
        do i = 1, imut
          dat2(i,j) = aexl(i,j,k) * dat3(i,j,k) + (1.0d0 - aexl(i,j,k)) * undef
        end do
      end do

      irec2 = irec2 + 1
      write(mtot2,rec=irec2) dat2

    end do

    ! T

    if (lsplit) then
      read (mtin3,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
      write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
      read (mtin3,end=999) dat3
    else
      read (mtin1,end=999) dat3
    end if

    do k = 1, km

      do j = 1, jmut
        do i = 1, imut
          dat2(i,j) = atexl(i,j,k) * dat3(i,j,k) + (1.0d0 - atexl(i,j,k)) * undef
        end do
      end do

      irec3 = irec3 + 1
      write(mtot3,rec=irec3) dat2

    end do

    ! S

    if (lsplit) then
      read (mtin4,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
      write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
      read (mtin4,end=999) dat3
    else
      read (mtin1,end=999) dat3
    end if

    do k = 1, km

      do j = 1, jmut
        do i = 1, imut
          dat2(i,j) = atexl(i,j,k) * dat3(i,j,k) + (1.0d0 - atexl(i,j,k)) * undef
        end do
      end do

      irec4 = irec4 + 1
      write(mtot4,rec=irec4) dat2
    end do

    ! Surface height

    if (lsplit) then
      read (mtin5,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
      write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
      read(mtin5,end=999) dat2
    else
      read(mtin1,end=999) dat2
    end if

    do j = 1, jmut
      do i = 1, imut
        dat2(i,j) = atexl(i,j,1) * dat2(i,j) + (1.0d0 - atexl(i,j,1)) * undef
      end do
    end do
    
    irec5 = irec5 + 1
    write(mtot5,rec=irec5) dat2

  end do

999 continue

  if (lsplit) then
    close(mtin1)
    close(mtin2)
    close(mtin3)
    close(mtin4)
    close(mtin5)
  else
    close(mtin1)
  end if

  close(mtot5)
  close(mtot4)
  close(mtot3)
  close(mtot2)
  close(mtot1)

  !--------------------------------------------------------------

  if (lmakectl) then

    varname_tmp(1) = varname(1)
    ndims_tmp(1) = ndims(1)

    call mkgrdctl ( &
         &   file_ctl_u, file_out_u,   title,              &
         &   imut, alonu, fmt_axis_h, linear_x,            &
         &   jmut, alatu, fmt_axis_h, linear_y,            &
         &   km  , dp   , fmt_axis_v, linear_z,            &
         &   time_dim   ,        1,        1,              &
         &   varname_tmp,  ndims_tmp,                      &
         &   cundef, opt_hint_uv, opt_general              &
         &   )

    varname_tmp(1) = varname(2)
    ndims_tmp(1) = ndims(2)

    call mkgrdctl ( &
         &   file_ctl_v, file_out_v,   title,              &
         &   imut, alonu, fmt_axis_h, linear_x,            &
         &   jmut, alatu, fmt_axis_h, linear_y,            &
         &   km  , dp   , fmt_axis_v, linear_z,            &
         &   time_dim   ,        1,        1,              &
         &   varname_tmp,  ndims_tmp,                      &
         &   cundef, opt_hint_uv, opt_general              &
         &   )

    varname_tmp(1) = varname(3)
    ndims_tmp(1) = ndims(3)

    call mkgrdctl ( &
         &   file_ctl_t, file_out_t,   title,              &
         &   imut, alont, fmt_axis_h, linear_x,            &
         &   jmut, alatt, fmt_axis_h, linear_y,            &
         &   km  , dp   , fmt_axis_v, linear_z,            &
         &   time_dim   ,        1,        1,              &
         &   varname_tmp,  ndims_tmp,                      &
         &   cundef, opt_hint_ts, opt_general              &
         &   )

    varname_tmp(1) = varname(4)
    ndims_tmp(1) = ndims(4)

    call mkgrdctl ( &
         &   file_ctl_s, file_out_s,   title,              &
         &   imut, alont, fmt_axis_h, linear_x,            &
         &   jmut, alatt, fmt_axis_h, linear_y,            &
         &   km  , dp   , fmt_axis_v, linear_z,            &
         &      time_dim,        1,        1,              &
         &   varname_tmp,  ndims_tmp,                      &
         &        cundef, opt_hint_ts, opt_general         &
         &   )

    varname_tmp(1) = varname(5)
    ndims_tmp(1) = ndims(5)

    call mkgrdctl ( &
         &   file_ctl_h,   file_out_h,    title,           &
         &   imut, alont,   fmt_axis_h,  linear_x,         &
         &   jmut, alatt,   fmt_axis_h,  linear_y,         &
         &     km,    dp,   fmt_axis_v,  linear_z,         &
         &      time_dim,          1,         1,           &
         &   varname_tmp,  ndims_tmp,                      &
         &        cundef, opt_hint_ts, opt_general         &
         &   )

  end if

end program mkgrads_hist_ocean
