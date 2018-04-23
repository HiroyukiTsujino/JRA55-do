! -*-F90-*-
!- Calc gradient of an array.
module gradient
  use libmxe_para, only: type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  implicit none
  private


  !-- subroutine --
  public :: gradient__h2d  !- horizontal 2D array


contains 


!-----------------------------------------------------------------
subroutine gradient__h2d(d,k,cgrid_org,cgrid_new,cxy &
                       & ,para,grid,topo,f,missing)
  implicit none

  type(type_libmxe_para),intent(in) :: para
  type(type_libmxe_grid),intent(in) :: grid
  type(type_libmxe_topo),intent(in) :: topo
  real(8),intent(in) :: d(para%imut,para%jmut)  !- input data
  integer,intent(in) :: k          !- vertical level
  character(1),intent(in) :: cgrid_org !- 'T':T-grid, 'U':U-grid
  character(1),intent(in) :: cgrid_new !-   (new array)
  character(1),intent(in) :: cxy       !- 'x':X-direction, 'y':Y-
  real(8),intent(out) :: f(para%imut,para%jmut) !- output
  real(8),intent(in),optional :: missing !- missing value

  integer :: i,j,im,jm,fdm(para%imut,para%jmut)
  logical :: lmiss

  !-- check --
  if ( .not. grid%ldef )  then
    write(*,*) 'Error at gradient__h2d'
    write(*,*) '  grid is not registered.'
    stop
  endif
  if ( .not. topo%ldef )  then
    write(*,*) 'Error at gradient__h2d'
    write(*,*) '  topo is not registered.'
    stop
  endif
  if ( ( k < 1 ).or.( k > para%km ) )  then
    write(*,*) 'Error at gradient__h2d'
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

  !-- gradient --
  lmiss = .true.

  if ((cxy=='x').and.(cgrid_org=='T').and.(cgrid_new=='U')) then
    do j = 1, jm -1
      do i = 1, im -1
        if ( k <= topo%exnn(i,j) ) then
          f(i,j) = ( &
               &   ( grid%dy_tl(i+1,j) * d(i+1,j) &
               &     + grid%dy_bl(i+1,j) * d(i+1,j+1) ) &
               &       / ( grid%dy_tl(i+1,j) + grid%dy_bl(i+1,j) ) &
               & - ( grid%dy_tl(i,j) * d(i,j) &
               &     + grid%dy_bl(i,j) * d(i,j+1) ) &
               &       / ( grid%dy_tl(i,j) + grid%dy_bl(i,j) ) ) &
               & / ( grid%dx_tl(i,j) + grid%dx_tr(i,j) )
        endif
      enddo
    enddo
    lmiss = .false.
  endif

  if ((cxy=='x').and.(cgrid_org=='U').and.(cgrid_new=='U')) then
    fdm(:,:) = 0  !- finite difference method
    !-    0: land, 1: center, 2: forward, 3: backward
    do j = 1, jm
      i = 1
      if ( ( k <= topo%exnn(i,j) ).and.( k <= topo%exnn(i+1,j) ) ) then
        fdm(i,j) = 2
      endif
      do i = 2, im-1
        if ( k <= topo%exnn(i,j) ) then
          if ( ( k <= topo%exnn(i+1,j) ) &
               & .and.( k <= topo%exnn(i-1,j) ) ) then
            fdm(i,j) = 1
          endif
          if ( ( k <= topo%exnn(i+1,j) ) &
               & .and.( k > topo%exnn(i-1,j) ) ) then
            fdm(i,j) = 2
          endif
          if ( ( k > topo%exnn(i+1,j) ) &
               & .and.( k <= topo%exnn(i-1,j) ) ) then
            fdm(i,j) = 3
          endif
        endif
      enddo
      i = im
      if ( ( k <= topo%exnn(i,j) ).and.( k <= topo%exnn(i-1,j) ) ) then
        fdm(i,j) = 3
      endif
    enddo
    do j = 1, jm
      do i = 1, im
        select case(fdm(i,j))
        case(1)
          f(i,j) = ( d(i+1,j) - d(i-1,j) ) &
               & / ( grid%dx_tr(i-1,j) + grid%dx_tl(i,j) &
               &    + grid%dx_tr(i,j) + grid%dx_tl(i+1,j) )
        case(2)
          f(i,j) = ( d(i+1,j) - d(i,j) ) &
               & / ( grid%dx_tr(i,j)+grid%dx_tl(i+1,j) )
        case(3)
          f(i,j) = ( d(i,j) - d(i-1,j) ) &
               & / ( grid%dx_tr(i-1,j)+grid%dx_tl(i,j) )
        endselect
      enddo
    enddo
    lmiss = .false.
  endif

  if ((cxy=='y').and.(cgrid_org=='T').and.(cgrid_new=='U')) then
    do j = 1, jm -1
      do i = 1, im -1
        if ( k <= topo%exnn(i,j) ) then
          f(i,j) = ( &
               &   ( grid%dx_br(i,j+1) * d(i,j+1) &
               &     + grid%dx_bl(i,j+1) * d(i+1,j+1) ) &
               &       / ( grid%dx_bl(i,j+1) + grid%dx_br(i,j+1) ) &
               & - ( grid%dx_br(i,j) * d(i,j) &
               &     + grid%dx_bl(i,j) * d(i+1,j) ) &
               &       / ( grid%dx_bl(i,j) + grid%dx_br(i,j) ) ) &
               & / ( grid%dy_tr(i,j) + grid%dy_br(i,j) )
        endif
      enddo
    enddo
    lmiss = .false.
  endif

  if ((cxy=='y').and.(cgrid_org=='U').and.(cgrid_new=='U')) then
    fdm(:,:) = 0  !- finite difference method
    !-    0: land, 1: center, 2: forward, 3: backward
    do i = 1, im
      j = 1
      if ( ( k <= topo%exnn(i,j) ).and.( k <= topo%exnn(i,j+1) ) ) then
        fdm(i,j) = 2
      endif
      do j = 2, jm-1
        if ( k <= topo%exnn(i,j) ) then
          if ( ( k <= topo%exnn(i,j+1) ) &
               & .and.( k <= topo%exnn(i,j-1) ) ) then
            fdm(i,j) = 1
          endif
          if ( ( k <= topo%exnn(i,j+1) ) &
               & .and.( k > topo%exnn(i,j-1) ) ) then
            fdm(i,j) = 2
          endif
          if ( ( k > topo%exnn(i,j+1) ) &
               & .and.( k <= topo%exnn(i,j-1) ) ) then
            fdm(i,j) = 3
          endif
        endif
      enddo
      j = jm
      if ( ( k <= topo%exnn(i,j) ).and.( k <= topo%exnn(i,j-1) ) ) then
        fdm(i,j) = 3
      endif
    enddo
    do j = 1, jm
      do i = 1, im
        select case(fdm(i,j))
        case(1)
          f(i,j) = ( d(i,j+1) - d(i,j-1) ) &
               & / ( grid%dy_tr(i,j-1) + grid%dy_br(i,j) &
               &    + grid%dy_tr(i,j) + grid%dy_br(i,j+1) )
        case(2)
          f(i,j) = ( d(i,j+1) - d(i,j) ) &
               & / ( grid%dy_tr(i,j)+grid%dy_br(i,j+1) )
        case(3)
          f(i,j) = ( d(i,j) - d(i,j-1) ) &
               & / ( grid%dy_tr(i,j-1)+grid%dy_br(i,j) )
        endselect
      enddo
    enddo
    lmiss = .false.
  endif

  !-- note --
  if ( lmiss ) then
    write(*,*) 'Error at gradient__h2d'
    write(*,*) '  Under construction: ',cgrid_org,cgrid_new,cxy
    stop
  endif

end subroutine gradient__h2d


end module gradient
