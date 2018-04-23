! -*-F90-*-
module libmxe_topo_test
  use libmxe_para
  use libmxe_topo
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='libmxe_topo'
  public :: ini_test
  public :: test_ho4
  public :: test_exnn
  public :: test_depth_t_cm
  public :: write_topo_grads


  type(type_libmxe_para),save :: para
  type(type_libmxe_topo),save :: topo

  integer,parameter :: lun=20
  integer :: i


contains


subroutine ini_test
  implicit none

  call libmxe_para__register(para)
  call libmxe_topo__register(topo,para)

end subroutine ini_test


subroutine test_ho4
  implicit none

  namelist /topo_ho4/ ival, nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=topo_ho4,iostat=i)
  close(lun)
  if ( i/=0 ) return

  call assert_equals( ival, topo%ho4(nx,ny), 'ho4' )

end subroutine test_ho4


subroutine test_exnn
  implicit none

  namelist /topo_exnn/ ival, nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=topo_exnn,iostat=i)
  close(lun)
  if ( i/=0 ) return

  call assert_equals( ival, topo%exnn(nx,ny), 'exnn' )

end subroutine test_exnn


subroutine test_depth_t_cm
  implicit none

  namelist /topo__depth_t_cm/ ival, nx, ny

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )

  read( lun, nml=topo__depth_t_cm, iostat=i )

  do while ( i==0 )
    call assert_equals( ival, topo%depth_t_cm(nx,ny), 'depth_t_cm' )
    read( lun, nml=topo__depth_t_cm, iostat=i )
  enddo
  close(lun)

end subroutine test_depth_t_cm


subroutine write_topo_grads
  use libmxe_grid, only: libmxe_grid__register, type_libmxe_grid
  use libmxe_io, only: libmxe_io__register, type_libmxe_io
  use libmxe_grads, only: libmxe_grads__make, type_grads
  implicit none

  integer,parameter :: lun=10

  type(type_libmxe_grid) :: grid
  type(type_libmxe_io) :: io
  type(type_grads) :: grads

  open(lun,file='topog.d',form='unformatted',access='direct' &
      , recl=para%imut*para%jmut*4)
    write(lun,rec=1) real(topo%ho4(1:para%imut,1:para%jmut))
    write(lun,rec=2) real(topo%exnn(1:para%imut,1:para%jmut))
  close(lun)

  grads%file_base='topog'
  grads%title='topography'
  grads%cgrid='U'
  grads%ztype='surface'
  grads%timemode='stationary'
  grads%nvar=2
  grads%var(1)='ho4 0 99 depth [cm]'
  grads%var(2)='exnn 0 99 bottom layer number'
  call libmxe_grid__register(grid,para)
  call libmxe_io__register(io,para)
  call libmxe_grads__make(grads,para,grid,io)

end subroutine write_topo_grads


end module libmxe_topo_test
