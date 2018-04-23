program mk_mask
  implicit none
  integer(4),parameter :: nx = 360 * 4, ny = 180 * 4
  real(4),parameter :: rundef = -9.99e33
  real(4) :: th(nx,ny)

  integer(4) :: mask(nx,ny)  !      coast(ocean): 1, land: 2, ocean(other):0
  integer(4) :: i, j, iw, ie, jn, js

  open(11,file='woa13_decav_th00.gd', form='unformatted',access='direct', &
       & recl = 4 * nx * ny )
  read(11,rec=1) th
  close(11)

  mask(:,:) = 0
  do j = 1, ny
    do i = 1, nx
      call news (iw, ie, jn, js, i, j, nx, ny)
      if ( th(i, j) /= rundef ) then
        if ( th(iw,j ) == rundef .or. &
           & th(ie,j ) == rundef .or. &
           & th(i ,js) == rundef .or. &
           & th(i ,jn) == rundef .or. &
           & th(iw,jn) == rundef .or. &
           & th(ie,jn) == rundef .or. &
           & th(iw,js) == rundef .or. &
           & th(ie,js) == rundef ) then
          mask(i,j) = 1
        else
          mask(i,j) = 0
        end if
      else
        mask(i,j) = 2
      end if
    end do
  end do
  open(11,file='mask.gd', form='unformatted',access='direct', &
       & recl = 4 * nx * ny )
  write(11,rec=1) mask
  close(11)
end program mk_mask

subroutine news ( iw,  ie,  jn,  js,  & 
     &             i,   j,  nx,  ny   )
  implicit none
  integer, intent(out) :: iw, ie, jn, js
  integer, intent(in)  :: i,  j,  nx, ny

  if (i == 1) then
    iw = nx
  else
    iw = i - 1
  end if

  if (i == nx) then
    ie = 1
  else
    ie = i + 1
  end if

  if (j == 1) then
    js = 1
  else
    js = j - 1
  end if
  if (j == ny) then
    jn = ny
  else
    jn = j + 1
  end if 

  return
end subroutine news
