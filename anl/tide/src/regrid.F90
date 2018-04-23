! -*-F90-*-
!- Regrid an array.
module regrid
  use libmxe_para, only: type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  implicit none
  private


  !-- subroutine --
  public :: regrid__h2d  !- horizontal 2D array (T=>U, U=>T)


contains 


!-----------------------------------------------------------------
subroutine regrid__h2d(d,k,cgrid_org,para,grid,topo,f,missing)
  implicit none

  type(type_libmxe_para),intent(in) :: para
  type(type_libmxe_grid),intent(in) :: grid
  type(type_libmxe_topo),intent(in) :: topo
  real(8),intent(in) :: d(para%imut,para%jmut)  !- input data
  integer,intent(in) :: k          !- vertical level
  character(1),intent(in) :: cgrid_org !- 'T':T-grid, 'U':U-grid
  real(8),intent(out) :: f(para%imut,para%jmut) !- output
  real(8),intent(in),optional :: missing !- missing value

  integer :: i,j,im,jm

  !-- check --
  if ( .not. grid%ldef )  then
    write(*,*) 'Error at regrid__h2d'
    write(*,*) '  grid is not registered.'
    stop
  endif
  if ( .not. topo%ldef )  then
    write(*,*) 'Error at regrid__h2d'
    write(*,*) '  topo is not registered.'
    stop
  endif
  if ( ( k < 1 ).or.( k > para%km ) )  then
    write(*,*) 'Error at regrid__h2d'
    write(*,*) '  wrong k=',k
    stop
  endif

  !--  missing value in the ground --
  if (present(missing)) then
    f(:,:) = missing
  else
    f(:,:) = dble( para%rundefout )
  endif

  im = para%imut
  jm = para%jmut

  !-- interpolation --
  if ( cgrid_org=='T' ) then   !- T-grid => U-grid

    do j = 1, jm -1
      do i = 1, im -1
        if ( k <= topo%exnn(i,j) ) then
          f(i,j) = ( grid%a_bl(i,j) * d(i,j) &
               & + grid%a_br(i,j) * d(i+1,j) &
               & + grid%a_tl(i,j) * d(i,j+1) &
               & + grid%a_tr(i,j) * d(i+1,j+1) ) &
               &  / grid%areau(i,j)
        endif
      enddo
    enddo

  else
    write(*,*) 'Error at regrid__h2d'
    write(*,*) '  Under construction: U-grid => T-grid'
    stop
  endif

end subroutine regrid__h2d


end module regrid
