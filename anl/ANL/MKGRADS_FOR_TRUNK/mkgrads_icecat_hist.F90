! -*-F90-*-
!==================================================================
program make_grads_sicat_hist

  use basin_param
  use grid_common

  implicit none

  integer(4), parameter :: ncat = 5

  real(8), parameter :: catdim(ncat) = (/ &
       & 1.0, 2.0, 3.0, 4.0, 5.0 /)

#if defined OGCM_ICEFULLMONIT && defined OGCM_ICECMIP5MONIT
  !
#include "icecat_hist_item_all.F90"
  !
#else /* OGCM_ICEFULLMONIT && OGCM_ICECMIP5MONIT */
  !
#if defined OGCM_ICEFULLMONIT || defined OGCM_ICECMIP5MONIT
  !
#ifdef OGCM_ICEFULLMONIT
#include "icecat_hist_item_full.F90"
#endif /* OGCM_ICEFULLMONIT */
  !
#ifdef OGCM_ICECMIP5MONIT
#include "icecat_hist_item_cmip5.F90"
#endif /* OGCM_ICECMIP5MONIT */
  !
#else /* OGCM_ICEFULLMONIT || OGCM_ICECMIP5MONIT */
  !
#include "icecat_hist_item_normal.F90"
  !
#endif /* OGCM_ICEFULLMONIT || OGCM_ICECMIP5MONIT */
#endif /* OGCM_ICEFULLMONIT && OGCM_ICECMIP5MONIT */

  integer(4) :: nstep0, month, iday, ihour, imin, mdays

  integer(4) :: num_recs, num_recs_ts, num_recs_uv
  integer(4) :: irect, irecu

  integer(4) :: num_items_ts, num_items_uv

  real(4), allocatable :: r2din(:,:,:)
  real(4), allocatable :: r2dout_ts(:,:,:)
  real(4), allocatable :: r2dout_uv(:,:,:)

  character(len=256) :: varname_ts(nitems)
  character(len=256) :: varname_uv(nitems)
  integer(4) :: vardims_ts(nitems)
  integer(4) :: vardims_uv(nitems)

  character(len=256) :: file_in, file_grid, file_topo
  character(len=256) :: file_out_ts, file_out_uv
  character(len=256) :: file_ctl_ts, file_ctl_uv

  character(len=256) :: title_ts, title_uv

  character(len=256),parameter :: fmt_axis = 'f8.3'
  character(len=256),parameter :: time_dim = 'TDEF 1 LINEAR  15JAN1059 1mo'

  character(len=256),parameter :: opt_general = ' '
!  character(len=256),parameter :: opt_general = 'template'

  character(len=256),parameter :: opt_hint_ts = ' '
  character(len=256),parameter :: opt_hint_uv = ' '
!  character(len=256),parameter :: opt_hint_ts = 'PDEF 364 368 BILIN STREAM BINARY-BIG ^pdef.masku.bilin.gd'
!  character(len=256),parameter :: opt_hint_uv = 'PDEF 364 368 BILIN STREAM BINARY-BIG ^pdef.maskt.bilin.gd'

  character(len=256) :: cundef
  
  integer(4) :: inum, i, j, m, n, k
  integer(4) :: item, ios

  real(4) :: rundef_model = -999.0
  real(4) :: rundef_grads =    0.0
!  real(4) :: rundef = 0.0

  logical :: lmakectl
  logical :: linear_x = .false.
  logical :: linear_y = .false.
  logical :: linear_z = .false.

  !----------------------------------------------------------------
  
  namelist /nicecat/ file_grid, file_topo, inum, file_in, &
       & file_out_ts, file_out_uv, file_ctl_ts, file_ctl_uv, &
       & title_ts, title_uv, lmakectl, linear_x, linear_y, linear_z, &
       & rundef_grads
  read(5,nicecat) 

  !----------------------------------------------------------------

  write(cundef,'(F10.5)') rundef_grads

  write(6,*) 'Grid spacing from = ', trim(file_grid)
  write(6,*) 'input from        = ', trim(file_in)
  write(6,*) '    missing value = ', trim(cundef)

  open(11, file=file_in    ,form='unformatted')
  open(12, file=file_out_ts,form='unformatted',access='direct',recl=4*imut*jmut)
  open(13, file=file_out_uv,form='unformatted',access='direct',recl=4*imut*jmut)

  irect = 0
  irecu = 0

  num_items_ts = 0
  num_items_uv = 0

  num_recs    = 0
  num_recs_ts = 0
  num_recs_uv = 0

  do m = 1, nitems
    if (nuvts(m) == 0) then
      num_items_ts = num_items_ts + 1
      varname_ts(num_items_ts) = varname(m)
      vardims_ts(num_items_ts) = ndims(m)
      do k = 1, ndims(m)
        num_recs_ts = num_recs_ts + 1
        num_recs    = num_recs    + 1
      end do
    end if
    if (nuvts(m) == 1) then
      num_items_uv = num_items_uv + 1
      varname_uv(num_items_uv) = varname(m)
      vardims_uv(num_items_uv) = ndims(m)
      do k = 1, ndims(m)
        num_recs_uv = num_recs_uv + 1
        num_recs    = num_recs    + 1
      end do
    end if
  end do

  ! add frozen water mass 2010.11.16

  num_items_ts = num_items_ts + 1
  num_recs_ts = num_recs_ts + 1
  varname_ts(num_items_ts) = varname(nitems+1)
  vardims_ts(num_items_ts) = 1

  ! add frozen water mass 2010.11.16

  write(6,*) ' Total record numbers are ', num_recs
  write(6,*) '     recs on T-points are ', num_recs_ts
  write(6,*) '     recs on U-points are ', num_recs_uv

  allocate(r2din    (1:imut,1:jmut,1:num_recs))
  allocate(r2dout_ts(1:imut,1:jmut,1:num_recs_ts))
  allocate(r2dout_uv(1:imut,1:jmut,1:num_recs_uv))

  !----------------------------------------------------------------

  call setgrd(file_topo, file_grid)
  
  !----------------------------------------------------------------

  if (lmakectl) then

    call mkgrdctl ( &
         &   file_ctl_ts, file_out_ts, title_ts,           &
         &   imut, alont, fmt_axis, linear_x,              &
         &   jmut, alatt, fmt_axis, linear_y,              &
         &   ncat, catdim, fmt_axis, linear_z,             &
         &   time_dim, num_items_ts, nitems,               &
         &   varname_ts, vardims_ts,                       &
         &   cundef, opt_hint_ts, opt_general              &
         &   )

    call mkgrdctl ( &
         &   file_ctl_uv, file_out_uv, title_uv,           &
         &   imut, alonu, fmt_axis, linear_x,              &
         &   jmut, alatu, fmt_axis, linear_y,              &
         &   ncat, catdim, fmt_axis, linear_z,             &
         &   time_dim, num_items_uv, nitems,               &
         &   varname_uv, vardims_uv,                       &
         &   cundef, opt_hint_uv, opt_general              &
         &   )
  end if

  !----------------------------------------------------------------

  do n = 1, inum

    read(11,end=9999) nstep0, month, iday, ihour, imin, mdays

    write(6,*)        nstep0, month, iday, ihour, imin
    write(6,*)        '     averaged for ', mdays , ' days'

    ! read data

    do m = 1, num_recs
      read(11) r2din(1:imut,1:jmut,m)
      do j = 1, jmut
        do i = 1, imut
          if (r2din(i,j,m) == rundef_model) then
            r2din(i,j,m) = rundef_grads
          end if
        end do
      end do
    end do

    ! apply mask
    
    num_recs    = 0
    num_recs_ts = 0
    num_recs_uv = 0

    do m = 1, nitems
      if (nuvts(m) == 0) then
        do k = 1, ndims(m)
          num_recs_ts = num_recs_ts + 1
          num_recs    = num_recs    + 1
          !write(6,*) num_recs_ts, num_recs
          r2dout_ts(1:imut,1:jmut,num_recs_ts) = r2din(1:imut,1:jmut,num_recs)
        end do
      end if
      if (nuvts(m) == 1) then
        do k = 1, ndims(m)
          num_recs_uv = num_recs_uv + 1
          num_recs    = num_recs    + 1
          write(6,*) 'U-point', num_recs_uv, num_recs
          r2dout_uv(1:imut,1:jmut,num_recs_uv) = r2din(1:imut,1:jmut,num_recs)
        end do
      end if
    end do

    ! ice concentration

    r2dout_ts(1:imut,1:jmut,3) = r2dout_ts(1:imut,1:jmut,3) * 1.0d2

    ! frozen ice mass

    num_recs_ts = num_recs_ts + 1
    r2dout_ts(1:imut,1:jmut,num_recs_ts) = &
         & r2dout_ts(1:imut,1:jmut,1) * real(rice,4) + r2dout_ts(1:imut,1:jmut,2) * real(rsnow,4)

    ! write TS-point data

    do m = 1, num_recs_ts
      irect = irect + 1
      write(6,*) m, irect
      write(12,rec=irect) r2dout_ts(1:imut,1:jmut,m)
    end do

    ! write UV-point data

    do m = 1, num_recs_uv
      irecu = irecu + 1
      write(6,*) m, irecu
      write(13,rec=irecu) r2dout_uv(1:imut,1:jmut,m)
    end do

  end do

9999 continue

  close(13)
  close(12)

end program make_grads_sicat_hist
