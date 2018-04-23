! -*-F90-*-
module map_test
  use map
  use libmxe_ut
  use fruit, only: assert_equals
  use libmxe_para, only: type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  implicit none
  private


  character(*),parameter,public :: testname='map'
  public :: ini_test
  public :: test_seek_nearest_sea_igrid

  real(8),save :: lon, lat    !- target location

  integer,parameter :: lun=20
  integer :: i

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo


contains


subroutine ini_test
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register
  use libmxe_io, only: libmxe_io__register
  implicit none

  namelist /map_test_lst/ lon, lat

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)

  read(5,nml=map_test_lst,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

end subroutine ini_test


subroutine test_seek_nearest_sea_igrid
  implicit none

  integer :: igrid, jgrid
  real(8) :: distance

  namelist /map_seek_nearest_sea_igrid/ ival

  call libmxe_ut__clear
  call libmxe_ut__open_namelist( lun )
    read(lun,nml=map_seek_nearest_sea_igrid,iostat=i)
  close(lun)
  if ( i/=0 ) return

  call map__seek_nearest_sea(para,grid,topo,lon,lat,'T' &
                          & ,igrid,jgrid,distance)
  call assert_equals( ival, igrid, 'seek_nearest_sea(igrid)')

end subroutine test_seek_nearest_sea_igrid


end module map_test
