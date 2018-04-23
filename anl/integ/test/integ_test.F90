! -*-F90-*-
module integ_test
  use integ
  use libmxe_ut
  use fruit, only: assert_equals
  use libmxe_para, only: type_libmxe_para, clen
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io,   only: type_libmxe_io
  implicit none
  private


  character(*),parameter,public :: testname='integ'
  public :: ini_test
  public :: test_area_t_2d
  public :: test_area_t_zero
  public :: test_area_t_one
  public :: test_area_u_2d
  public :: test_area_u_zero
  public :: test_area_u_one
  public :: test_vol_u
  public :: test_vert_t
  public :: test_section_u_zonal

  character(clen),save :: file_base_2d_t  !- input 2D file (T-grid)
  character(clen),save :: file_base_2d_u  !-  (U-grid)
  character(clen),save :: file_base_3d_t  !-  3D file (T-grid)
  character(clen),save :: file_base_3d_u  !-          (U-grid)
  character(clen),save :: file_base_ssh   !-  SSH

  integer,parameter           :: lun_ut = 20
  integer,parameter           :: lun    = 21
  integer                     :: ios

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save   :: io

  integer,save                :: im, jm, km

contains


subroutine ini_test
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register &
                   & , libmxe_topo__aexl, libmxe_topo__dz3d
  use libmxe_io, only: libmxe_io__register
  implicit none

  namelist /integ_lst/ file_base_2d_t, file_base_2d_u &
                   & , file_base_3d_u , file_base_3d_t, &
                     & file_base_ssh

  read( 5, nml=integ_lst, iostat=ios )
  if ( ios /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)
  call libmxe_topo__dz3d(topo,para)
  call libmxe_io__register(io,para)

  im = para%imut
  jm = para%jmut
  km = para%km

  call libmxe_ut__open_namelist( lun_ut )

end subroutine ini_test


subroutine test_area_t_2d
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer :: reclen
  real(8) :: f
  real,allocatable :: r(:,:)
  real(8),allocatable :: d(:,:)

  namelist /integ_area_t_2d/ dval, drange

  call libmxe_ut__clear
  rewind( lun_ut )
  read( lun_ut, nml=integ_area_t_2d, iostat=ios )
  if ( ios/=0 ) return

  allocate( r(im,jm), d(im,jm) )
  reclen = im*jm*4
  call libmxe_io__open(io,trim(file_base_2d_t),1,reclen &
       & ,lun,action='read')
  read(lun,rec=1) r
  close(lun)

  d(1:im,1:jm) = dble( r(1:im,1:jm) )
  f =  integ__area(d,1,'T',para,grid,topo)

  call assert_equals( dval, f, drange, 'area T 2D (read file)' )

  deallocate(r,d)

end subroutine test_area_t_2d


subroutine test_area_t_zero
  implicit none

  real(8) :: f
  real(8),allocatable :: d(:,:)

  allocate( d(para%imut,para%jmut) )
  d = 0.d0
  f =  integ__area(d,1,'T',para,grid,topo)
  call assert_equals( 0.d0, f, 'T (zero)' )

  deallocate(d)

end subroutine test_area_t_zero


subroutine test_area_t_one
  implicit none

  real(8) :: f
  real(8),allocatable :: d(:,:)

  namelist /integ_area_t_one/ dval, drange

  call libmxe_ut__clear
  rewind( lun_ut )
  read( lun_ut, nml=integ_area_t_one, iostat=ios )
  if ( ios/=0 ) return

  allocate( d(para%imut,para%jmut) )
  d = 1.d0
  f = integ__area(d,1,'T',para,grid,topo)
  call assert_equals( dval, f, drange, 'T (one)' )

  deallocate(d)

end subroutine test_area_t_one


subroutine test_area_u_2d
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer,parameter :: lun = 21
  integer :: reclen
  real(8) :: f
  real,allocatable :: r(:,:)
  real(8),allocatable :: d(:,:)

  namelist /integ_area_u_2d/ dval, drange

  call libmxe_ut__clear
  rewind( lun_ut )
  read( lun_ut, nml=integ_area_u_2d, iostat=ios )
  if ( ios/=0 ) return

  allocate( r(im,jm), d(im,jm) )
  reclen = im*jm*4
  call libmxe_io__open(io,trim(file_base_2d_u),1,reclen &
       & ,lun,action='read')
  read(lun,rec=1) r
  close(lun)

  d(1:im,1:jm) = dble( r(1:im,1:jm) )
  f =  integ__area(d,1,'U',para,grid,topo)

  call assert_equals( dval, f, drange, 'U 2D (read file)' )

  deallocate(r,d)

end subroutine test_area_u_2d


subroutine test_area_u_zero
  implicit none

  real(8) :: f
  real(8),allocatable :: d(:,:)

  allocate( d(para%imut,para%jmut) )
  d = 0.d0
  f =  integ__area(d,1,'U',para,grid,topo)
  call assert_equals( 0.d0, f, 'U (zero)' )

  deallocate(d)

end subroutine test_area_u_zero


subroutine test_area_u_one
  implicit none

  real(8) :: f
  real(8),allocatable :: d(:,:)

  namelist /integ_area_u_one/ dval, drange

  call libmxe_ut__clear
  rewind( lun_ut )
  read( lun_ut, nml=integ_area_u_one, iostat=ios )
  if ( ios/=0 ) return

  allocate( d(para%imut,para%jmut) )
  d = 1.d0
  f = integ__area(d,1,'U',para,grid,topo)
  call assert_equals( dval, f, drange, 'U (one)' )

  deallocate(d)

end subroutine test_area_u_one


subroutine test_vol_u
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer :: reclen
  real(8) :: f
  real,allocatable :: r(:,:,:)
  real(8),allocatable :: d(:,:,:)

  namelist /integ_vol_u/ dval, drange

  call libmxe_ut__clear
  rewind( lun_ut )
  read( lun_ut, nml=integ_vol_u, iostat=ios )
  if ( ios/=0 ) return

  allocate( r(im,jm,km), d(im,jm,km) )
  reclen = im*jm*km*4
  call libmxe_io__open(io,trim(file_base_3d_u),1,reclen &
       & ,lun,action='read')
  read(lun,rec=1) r
  close(lun)

  d(:,:,:) = dble( r(:,:,:) )
  f =  integ__vol(d,'U',para,grid,topo)

  call assert_equals( dval, f, drange, 'vol U-grid' )

  deallocate(r,d)

end subroutine test_vol_u


subroutine test_vert_t
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer :: reclen
  real,allocatable :: r(:,:,:)
  real(8),allocatable :: f(:,:)

  namelist /integ_vert_t/ dval, drange, nx, ny

  call libmxe_ut__clear
  rewind( lun_ut )
  read( lun_ut, nml=integ_vert_t, iostat=ios )
  if ( ios/=0 ) return

  allocate( r(im,jm,km), f(im,jm) )
  reclen = im*jm*km*4
  call libmxe_io__open( io, trim(file_base_3d_t), 1, &
                      & reclen, lun, action='read' )
  read(lun,rec=1) r
  close(lun)

  call integ__vert( dble(r(:,:,:)), 'T', para, topo, f(:,:) )

  do while ( ios==0 )
    call assert_equals( dval, f(nx,ny), drange, 'vert T-grid' )
    read( lun_ut, nml=integ_vert_t, iostat=ios )
  enddo

  deallocate(r,f)

end subroutine test_vert_t


subroutine test_section_u_zonal
  use libmxe_io,   only: libmxe_io__open
  use libmxe_topo, only: libmxe_topo__updatedz
  implicit none

  real(4),allocatable :: ssh(:,:)
  real(4),allocatable :: d(:,:,:)
  real(8)             :: f_cm2

  namelist /integ_section_u_zonal/ dval, drange, nx

  call libmxe_ut__clear
  rewind( lun_ut )
  read( lun_ut, nml=integ_section_u_zonal, iostat=ios )
  if ( ios/=0 ) return

  allocate( ssh(im,jm), d(im,jm,km) )
  call libmxe_io__open( io, trim(file_base_3d_u), 1, &
                      & im*jm*km*4, lun, action='read' )
  read(lun,rec=1) d
  close(lun)
  call libmxe_io__open( io, trim(file_base_ssh), 1, &
                      & im*jm*4, lun, action='read' )
  read(lun,rec=1) ssh
  close(lun)

  call libmxe_topo__updatedz( dble(ssh), topo, para, grid )

  f_cm2 = integ__section( dble(d),para,grid,topo,'U',.true.,1,im,nx)

  call assert_equals( dval, f_cm2, drange, 'section(u,zonal)' )

  deallocate( ssh,d )

end subroutine test_section_u_zonal



end module integ_test
