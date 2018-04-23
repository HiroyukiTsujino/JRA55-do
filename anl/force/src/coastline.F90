! -*-F90-*-
!- Regrid an array.
module coastline
  use libmxe_para, only: type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  implicit none
  private


  public :: coastline__seek_nearest_sea
  public :: coastline__seek_nearest_sea2


contains 


!-----------------------------------------------------------------
subroutine coastline__seek_nearest_sea( para, grid, topo, lon, lat, k_arg, cgrid, sea_i, sea_j )
  implicit none

  type(type_libmxe_para),intent(in) :: para
  type(type_libmxe_grid),intent(in) :: grid
  type(type_libmxe_topo),intent(in) :: topo
  real(8),intent(in) :: lon, lat
  integer,intent(in) :: k_arg
  character(1),intent(in) :: cgrid !- 'T' or 'U'
  integer,intent(out) :: sea_i, sea_j

  real(8),pointer :: x(:,:), y(:,:)
  real(8) :: distance(para%imut,para%jmut), sea_index(para%imut,para%jmut)
  integer :: ij(2)

  !-- check --
  if ( .not. grid%ldef )  then
    write(6,*) 'STOP: Error at coastline__seek_nearest_sea'
    write(6,*) '  grid is not registered.'
    stop
  endif
  if ( .not. topo%ldef )  then
    write(6,*) 'STOP: Error at coastline__seek_nearest_sea'
    write(6,*) '  topo is not registered.'
    stop
  endif
  if ( ( k_arg < 1 ).or.( k_arg > para%km ) )  then
    write(6,*) 'STOP: Error at coastline__seek_nearest_sea'
    write(6,*) '  wrong k_arg=',k_arg
    stop
  endif

  if ( cgrid == 'T' ) then
    x => grid%glont
    y => grid%glatt
    sea_index(:,:) = topo%atexl(:,:,k_arg)
  else
    x => grid%glonu
    y => grid%glatu
    sea_index(:,:) = topo%aexl(:,:,k_arg)
  endif
  !- TODO: replace copy by save

  distance = (x-lon)**2 + (y-lat)**2  !- TODO: precise distance

  ij = minloc( distance, sea_index == 1.d0 )
  sea_i = ij(1)
  sea_j = ij(2)
  !- TODO: very heavy

end subroutine coastline__seek_nearest_sea
!-----------------------------------------------------------------


subroutine coastline__seek_nearest_sea2( im, jm, x, y, sea_index, lon, lat, sea_i, sea_j )
  implicit none

  integer,intent(in)  :: im, jm
  real(8),intent(in)  :: x(im), y(jm)
  integer,intent(in)  :: sea_index(im,jm)
  real(8),intent(in)  :: lon, lat
  integer,intent(out) :: sea_i, sea_j

!  real(8),pointer :: x(:,:), y(:,:)
  real(8) :: distance, distance_min
  integer :: start_i(1), start_j(1)
  integer :: n, i0, i1, j0, j1, ii(1), is, js

  start_i = minloc( abs(x-lon) )
  start_j = minloc( abs(y-lat) )

  sea_i = 0
  sea_j = 0
  distance_min = 9.99d33

  do n = 0, min( im/2, jm/2 )

    j0 = max( start_j(1) - n, 1 )
    j1 = min( start_j(1) + n, jm )
    i0 = max( start_i(1) - n, 1 )
    i1 = min( start_i(1) + n, im )

    !- west
    ii = minloc( abs(y(j0:j1)-lat), sea_index(i0,j0:j1) == 1.d0 )
    if ( ii(1) > 0 ) then
      is = i0
      js = ii(1) + j0 - 1
      distance = (x(is)-lon)**2 + (y(js)-lat)**2
      if ( distance < distance_min ) then
        distance_min = distance
        sea_i = is
        sea_j = js
      endif
    endif

    !- east
    ii = minloc( abs(y(j0:j1)-lat), sea_index(i1,j0:j1) == 1.d0 )
    if ( ii(1) > 0 ) then
      is = i1
      js = ii(1) + j0 - 1
      distance = (x(is)-lon)**2 + (y(js)-lat)**2
      if ( distance < distance_min ) then
        distance_min = distance
        sea_i = is
        sea_j = js
      endif
    endif

    !- south
    ii = minloc( abs(x(i0:i1)-lon), sea_index(i0:i1,j0) == 1.d0 )
    if ( ii(1) > 0 ) then
      is = ii(1) + i0 - 1
      js = j0
      distance = (x(is)-lon)**2 + (y(js)-lat)**2
      if ( distance < distance_min ) then
        distance_min = distance
        sea_i = is
        sea_j = js
      endif
    endif

    !- north
    ii = minloc( abs(x(i0:i1)-lon), sea_index(i0:i1,j1) == 1.d0 )
    if ( ii(1) > 0 ) then
      is = ii(1) + i0 - 1
      js = j1
      distance = (x(is)-lon)**2 + (y(js)-lat)**2
      if ( distance < distance_min ) then
        distance_min = distance
        sea_i = is
        sea_j = js
      endif
    endif

    if ( distance_min < 9.99e33 ) exit

  enddo


end subroutine coastline__seek_nearest_sea2


end module coastline
