! -*-F90-*-
module regrid_test
  use regrid
  use libmxe_ut
  use fruit, only: assert_equals
  use libmxe_para, only: type_libmxe_para, clen
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io
  implicit none
  private


  character(*),parameter,public :: testname='regrid'
  public :: ini_test
  public :: test_h2d_t2u
  public :: test_h2d_t2u_zero
  public :: test_h2d_t2u_one

  character(clen),save :: file_base_2d_t  !- input 2D file (T-grid)

  integer,parameter :: lun=20
  integer :: i

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save :: io

  integer :: im,jm
  real(8),allocatable :: d(:,:),f(:,:)

contains


subroutine ini_test
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register
  use libmxe_io, only: libmxe_io__register
  implicit none

  namelist /regrid_lst/ file_base_2d_t

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_io__register(io,para)

  read(5,nml=regrid_lst,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  im = para%imut
  jm = para%jmut

end subroutine ini_test


subroutine test_h2d_t2u
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer :: reclen
  real,allocatable :: r(:,:)

  namelist /regrid_h2d_t2u/ rval, rrange, nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=regrid_h2d_t2u,iostat=i)
  close(lun)
  if ( i/=0 ) return

  allocate( r(im,jm), d(im,jm), f(im,jm) )
  reclen = im*jm*4
  call libmxe_io__open(io,trim(file_base_2d_t),1,reclen &
       & ,lun,action='read')
  read(lun,rec=1) r
  close(lun)

  d(1:im,1:jm) = dble( r(1:im,1:jm) )
  call regrid__h2d(d,1,'T',para,grid,topo,f)
  call assert_equals( rval, real(f(nx,ny)), rrange &
                  & , 'h2d T2U (read file)')
  deallocate(r,d,f)

end subroutine test_h2d_t2u


subroutine test_h2d_t2u_zero
  implicit none

  namelist /regrid_h2d_t2u_zero/ nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=regrid_h2d_t2u_zero,iostat=i)
  close(lun)
  if ( i/=0 ) return

  allocate( d(im,jm), f(im,jm) )
  d = 0.d0
  call regrid__h2d(d,1,'T',para,grid,topo,f)
  call assert_equals( 0.d0, f(nx,ny), 'h2d T2U (zero)' )
  deallocate(d,f)

end subroutine test_h2d_t2u_zero


subroutine test_h2d_t2u_one
  use libmxe_io, only: libmxe_io__open
  implicit none

  namelist /regrid_h2d_t2u_one/ nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=regrid_h2d_t2u_one,iostat=i)
  close(lun)
  if ( i/=0 ) return

  allocate( d(im,jm), f(im,jm) )
  d = 1.d0
  call regrid__h2d(d,1,'T',para,grid,topo,f)
  call assert_equals( 1.d0, f(nx,ny), 1.d-12, 'h2d T2U (1.0)' )
  deallocate(d,f)

end subroutine test_h2d_t2u_one


end module regrid_test
