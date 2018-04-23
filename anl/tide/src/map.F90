! -*-F90-*-
!- Map.
module map
  use libmxe_para, only: type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  implicit none
  private


  public :: map__seek_nearest_sea  !- Seek sea grid.
  public :: map__hubeny            !- Calc distance [m] between 2 points
                                   !-   after Hubeny's Eq.

contains 


!-----------------------------------------------------------------
subroutine map__seek_nearest_sea(para,grid,topo,lon,lat,cgrid &
                          & ,igrid,jgrid,distance)
  implicit none

  type(type_libmxe_para),intent(in) :: para
  type(type_libmxe_grid),intent(in) :: grid
  type(type_libmxe_topo),intent(in) :: topo
  real(8),intent(in) :: lon, lat            !- target location
  character(1),intent(in) :: cgrid          !- 'T':T-grid, 'U':U-grid
  integer,intent(out) :: igrid, jgrid       !- nearest grid
  real(8),intent(out),optional :: distance  !- distance [km]

  real(8),parameter :: dist_default = 9.99d33
  integer :: i,j,im,jm,indx(2)
  real(8),allocatable :: dist(:,:)

  !-- check --
  if ( .not. grid%ldef )  then
    write(*,*) 'Error at map__seek_nearest_sea'
    write(*,*) '  grid is not registered.'
    stop
  endif
  if ( .not. topo%ldef )  then
    write(*,*) 'Error at map__seek_nearest_sea'
    write(*,*) '  topo is not registered.'
    stop
  endif
  if ( cgrid=='U' ) then
    write(*,*) 'Error at map__seek_nearest_sea'
    write(*,*) '  Under construction: cgrid = U'
    stop
  endif

  igrid = 0
  jgrid = 0
  distance = 0.d0
  im = para%imut
  jm = para%jmut

  !-- distance --
  allocate(dist(im,jm))
  dist(:,:) = dist_default
  do j = 1, jm
    do i = 1, im
      if ( topo%texnn(i,j) > 0 ) then
        dist(i,j) = map__hubeny( grid%glont(i,j),grid%glatt(i,j),lon,lat )
      endif
    enddo
  enddo

  !-- seek nearest sea --
  indx(1:2) = minloc( dist(:,:) ) 
  igrid = indx(1)
  jgrid = indx(2)
  distance = dist( igrid, jgrid )

end subroutine map__seek_nearest_sea


real(8) function map__hubeny( lon1, lat1, lon2, lat2 )
  use libmxe_para, only : pi
  implicit none

  real(8),intent(in) :: lon1, lat1, lon2, lat2

  real(8),parameter :: a = 6377397.d0, b = 6356079.d0 ! [m]
  real(8),parameter :: e2 = 0.00667436061028297d0
  real(8),parameter :: rad = pi / 180.d0

  real(8) :: d, dx, dy, yy, w, n, m

  d = 0.d0

  dx = rad * ( lon1 - lon2 )
  dy = rad * ( lat1 - lat2 )
  yy = 0.5d0 * rad * ( lat1 + lat2 )

  w = sqrt( 1.d0 - e2*sin(yy)**2 )
  n = a / w
  m = a * ( 1.d0 - e2 ) / w**3

  d = sqrt( ( dy*m )**2 + ( dx*n*cos(yy) )**2 )

  map__hubeny = d

end function map__hubeny


end module map
