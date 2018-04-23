! -*-F90-*-
module gradient_test
  use gradient
  use libmxe_ut
  use fruit, only: assert_equals
  use libmxe_para, only: type_libmxe_para, clen
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io
  implicit none
  private


  character(*),parameter,public :: testname='gradient'
  public :: ini_test
  public :: test_h2d_dtdxu
  public :: test_h2d_dtdxu_zero
  public :: test_h2d_dtdxu_one
  public :: test_h2d_dudxu
  public :: test_h2d_dudyu

  character(clen),save :: file_base_2d_t  !- input 2D file (T-grid)
  character(clen),save :: file_base_2d_u  !-  (U-grid)

  integer,parameter :: lun=20
  integer :: i

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save :: io

  integer :: im,jm
  real(8),allocatable :: d(:,:), f(:,:)


contains


subroutine ini_test
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register
  use libmxe_io, only: libmxe_io__register
  implicit none

  namelist /gradient_lst/ file_base_2d_t, file_base_2d_u

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_io__register(io,para)

  read(5,nml=gradient_lst,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  im = para%imut
  jm = para%jmut

end subroutine ini_test


subroutine test_h2d_dtdxu
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer :: reclen
  real,allocatable :: r(:,:)

  namelist /gradient_h2d_dtdxu/ rval, rrange, nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=gradient_h2d_dtdxu,iostat=i)
  close(lun)
  if ( i/=0 ) return

  allocate( r(im,jm), d(im,jm), f(im,jm) )
  reclen = im*jm*4
  call libmxe_io__open(io,trim(file_base_2d_t),1,reclen &
       & ,lun,action='read')
  read(lun,rec=1) r
  close(lun)

  d(1:im,1:jm) = dble( r(1:im,1:jm) )
  call gradient__h2d(d,1,'T','U','x',para,grid,topo,f)
  call assert_equals( rval, real(f(nx,ny)), rrange &
                  & , 'h2d dtdxu (read file)')

  deallocate(r,d,f)

end subroutine test_h2d_dtdxu


subroutine test_h2d_dtdxu_zero
  implicit none

  namelist /gradient_h2d_dtdxu_zero/ nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=gradient_h2d_dtdxu_zero,iostat=i)
  close(lun)
  if ( i/=0 ) return

  allocate( d(im,jm), f(im,jm) )
  d = 0.d0
  call gradient__h2d(d,1,'T','U','x',para,grid,topo,f)
  call assert_equals( 0.d0, f(nx,ny), 'h2d T2U (zero)' )
  deallocate(d,f)

end subroutine test_h2d_dtdxu_zero


subroutine test_h2d_dtdxu_one
  use libmxe_io, only: libmxe_io__open
  implicit none

  namelist /gradient_h2d_dtdxu_one/ nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=gradient_h2d_dtdxu_one,iostat=i)
  close(lun)
  if ( i/=0 ) return

  allocate( d(im,jm), f(im,jm) )
  d = 1.d0
  call gradient__h2d(d,1,'T','U','x',para,grid,topo,f)
  call assert_equals( 0.d0, f(nx,ny), 1.d-12, 'h2d dTdxU (1.0)' )
  deallocate(d,f)

end subroutine test_h2d_dtdxu_one


subroutine test_h2d_dudxu
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer :: reclen
  real,allocatable :: r(:,:)

  namelist /gradient_h2d_dudxu/ rval, rrange, nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=gradient_h2d_dudxu,iostat=i)
  close(lun)
  if ( i/=0 ) return

  allocate( r(im,jm), d(im,jm), f(im,jm) )
  reclen = im*jm*4
  call libmxe_io__open(io,trim(file_base_2d_u),1,reclen &
       & ,lun,action='read')
  read(lun,rec=1) r
  close(lun)
  d(1:im,1:jm) = dble( r(1:im,1:jm) )
  call gradient__h2d(d,1,'U','U','x',para,grid,topo,f)
  call assert_equals( rval, real(f(nx,ny)), rrange &
                  & , 'h2d dudxu (read file)')
  deallocate(r,d,f)

end subroutine test_h2d_dudxu


subroutine test_h2d_dudyu
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer :: reclen
  real,allocatable :: r(:,:)

  namelist /gradient_h2d_dudyu/ rval, rrange, nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=gradient_h2d_dudyu,iostat=i)
  close(lun)
  if ( i/=0 ) return

  allocate( r(im,jm), d(im,jm), f(im,jm) )
  reclen = im*jm*4
  call libmxe_io__open(io,trim(file_base_2d_u),1,reclen &
       & ,lun,action='read')
  read(lun,rec=1) r
  close(lun)
  d(1:im,1:jm) = dble( r(1:im,1:jm) )
  call gradient__h2d(d,1,'U','U','y',para,grid,topo,f)
  call assert_equals( rval, real(f(nx,ny)), rrange &
                  & , 'h2d dudyu (read file)')
  deallocate(r,d,f)

end subroutine test_h2d_dudyu


end module gradient_test
