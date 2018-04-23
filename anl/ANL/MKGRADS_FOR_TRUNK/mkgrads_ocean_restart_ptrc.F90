! -*-F90-*-
!=========================================================================
program mkgrads_restart_ocean_passive_tracer
  !-----------------------------------------------------------------------
  !     Make GrADS data from OGCM output
  !-----------------------------------------------------------------------

  use basin_param
  use grid_common

  implicit none

  real(8) :: dat3(imut,jmut,km)
  real(8) :: dat2(imut,jmut)
  real(4) :: dat4(imut,jmut)

  integer(4) :: i, j, k, n
  integer(4) :: nkai, month, iday, ihour, imin

  integer(4), parameter :: mtin1=61, mtin2=62, mtin3=63, mtin4=64, mtin5=65
  integer(4), parameter :: mtin6=66, mtin7=67, mtin8=68, mtin9=69
  integer(4), parameter :: mtina=71, mtinb=72, mtinc=73, mtind=74, mtine=75

  integer(4), parameter :: mtot1=81, mtot2=82, mtot3=83, mtot4=84, mtot5=85
  integer(4), parameter :: mtot6=86, mtot7=87, mtot8=88, mtot9=89
  integer(4), parameter :: mtota=91, mtotb=92, mtotc=93, mtotd=94, mtote=75

  integer(4) :: irec1, irec2, irec3, irec4, irec5
  integer(4) :: irec6, irec7, irec8, irec9
  integer(4) :: ireca, irecb, irecc, irecd, irece

  integer(4) :: inum

  logical :: lsplit

  character(len=256) :: file_grid, file_topo

  character(len=256) :: file_in
  character(len=256) :: file_out
  character(len=256) :: file_ctl

  real(4) :: undef = 0.0
  real(4) :: undef8 = 0.0d0

  logical :: lmakectl
  logical :: linear_x = .false.
  logical :: linear_y = .false.
  logical :: linear_z = .false.

  character(len=256) :: varname_tmp(1)
  integer(4) :: ndims_tmp(1)
  real(8) :: dep_tmp(km)

  character(len=256),parameter :: title = 'MRICOM SNAP SHOT (RESTART) DATA'

  character(len=256),parameter :: fmt_axis_h = 'f8.3'
  character(len=256),parameter :: fmt_axis_v = 'f8.2'
  character(len=256),parameter :: time_dim = 'TDEF 1 LINEAR  15JAN1001 1mo'

  character(len=256),parameter :: opt_general = ' '
!  character(len=256),parameter :: opt_general = 'template'

  character(len=256),parameter :: opt_hint_ts = ' '
  character(len=256),parameter :: opt_hint_uv = ' '
!  character(len=256),parameter :: opt_hint_ts = 'PDEF 364 368 BILIN STREAM BINARY-BIG ^pdef.masku.bilin.gd'
!  character(len=256),parameter :: opt_hint_uv = 'PDEF 364 368 BILIN STREAM BINARY-BIG ^pdef.maskt.bilin.gd'

  character(len=256) :: cundef

  !---------------------------------------------------------------------
  
  namelist /nocrst/ file_grid, file_topo, &
       & file_in,  &
       & file_out, &
       & file_ctl, &
       & inum, undef, lmakectl, linear_x, linear_y, linear_z

  read(5,nocrst) 

  !---------------------------------------------------------------------

  call setgrd(file_topo, file_grid)

  write(cundef,'(E12.5)') undef
  undef8 = undef

  write(6,*) 'Grid spacing from = ', trim(file_grid)
  write(6,*) 'Topography from   = ', trim(file_topo)
  write(6,*) 'Missing value     = ', trim(cundef)

  !----------------------------------------------------------------

  write(*,*) 'reading ', inum ,' data'

  !------------------------------------------------------------------------------

  open(mtin1, file=file_in,form='unformatted',action='read')
  write(*,*) 'input from ... ', trim(file_in)


  !-------

  open(mtot1, file=file_out,form='unformatted',access='direct',recl=4*imut*jmut)
  write(*,*) 'output to  ... ', trim(file_out)
  irec1 = 0

  !------------------------------------------------------------------------

  do n = 1, inum

    read (mtin1,end=999) NKAI,MONTH,IDAY,IHOUR,IMIN
    write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN
    read (mtin1,end=999) dat3

    do k = 1, km
      do j = 1, jmut
        do i = 1, imut
          dat4(i,j) = atexl(i,j,k) * dat3(i,j,k) + (1.0d0 - atexl(i,j,k)) * undef8
        end do
      end do

      irec1 = irec1 + 1

      write(mtot1,rec=irec1) dat4

    end do

  end do

999 continue

  close(mtin1)

  close(mtot1)

  !--------------------------------------------------------------

  if (lmakectl) then

    varname_tmp(1) = "t         km 99 Passive tracer                                       "
    ndims_tmp(1) = km

    call mkgrdctl ( &
         &   file_ctl   , file_out  , title   ,            &
         &   imut, alont, fmt_axis_h, linear_x,            &
         &   jmut, alatt, fmt_axis_h, linear_y,            &
         &   km  , dp   , fmt_axis_v, linear_z,            &
         &   time_dim   ,        1,        1,              &
         &   varname_tmp,  ndims_tmp,                      &
         &   cundef, opt_hint_ts, opt_general              &
         &   )

  end if

end program mkgrads_restart_ocean_passive_tracer
