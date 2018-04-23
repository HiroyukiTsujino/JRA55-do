! -*-F90-*-
!- Subroutines of space integration
module integ
  use libmxe_para, only: type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  implicit none
  private


  !-- subroutine --


  !-- function --
  public :: integ__area  !- area integration
  public :: integ__vol   !- volume integration
  public :: integ__vert
  public :: integ__section


contains 


function integ__area(d,k,cgrid,para,grid,topo,istr,iend,jstr,jend)
  implicit none

  real(8) :: integ__area

  type(type_libmxe_para),intent(in) :: para
  type(type_libmxe_grid),intent(in) :: grid
  type(type_libmxe_topo),intent(in) :: topo
  real(8),intent(in) :: d(para%imut,para%jmut)  !- input data
  integer,intent(in) :: k          !- vertical level
  character(1),intent(in) :: cgrid !- 'T':T-grid, 'U':U-grid
  integer,intent(in),optional :: istr,iend,jstr,jend
               !- array range: [istr:iend,jstr:jend,kstr:kend]

  real(8) :: f
  integer :: i,j,i0,i1,j0,j1
  real(8),allocatable :: area(:,:)


  !-- check --
  if ( .not. grid%ldef )  then
    write(*,*) 'Error at integ__area'
    write(*,*) '  grid is not registered.'
    stop
  endif
  if ( .not. topo%ldef )  then
    write(*,*) 'Error at integ__area'
    write(*,*) '  topo is not registered.'
    stop
  endif
  if ( ( k < 1 ).or.( k > para%km ) )  then
    write(*,*) 'Error at integ__area'
    write(*,*) '  wrong k=',k
    stop
  endif


  !-- arguments (grid ranges) --
  i0 = 1
  i1 = para%imut
  j0 = 1
  j1 = para%jmut
  if (present(istr)) i0 = istr
  if (present(iend)) i1 = iend
  if (present(jstr)) j0 = jstr
  if (present(jend)) j1 = jend


  !-- area of T or U box --
  allocate(area(i0:i1,j0:j1))
  area = 0.d0

  if ( cgrid=='U' ) then
    do j = j0, j1
      do i = i0, i1
        area(i,j) = topo%aexl(i,j,k) * grid%areau(i,j) 
      enddo
    enddo
  else
    do j = j0, j1
      do i = i0, i1
        if ( ( i == 1 ).and.( j == 1 ) ) then
          area(i,j) = topo%aexl(i,j,k) * grid%a_bl(i,j) 
        else if ( i == 1 ) then
          area(i,j) = topo%aexl(i,j,k) * grid%a_bl(i,j) &
               & + topo%aexl(i,j-1,k) * grid%a_tl(i,j-1) 
        else if ( j == 1 ) then
          area(i,j) = topo%aexl(i,j,k) * grid%a_bl(i,j) &
               & + topo%aexl(i-1,j,k) * grid%a_br(i-1,j) 
        else
          area(i,j) = topo%aexl(i,j,k) * grid%a_bl(i,j) &
               & + topo%aexl(i-1,j,k) * grid%a_br(i-1,j) &
               & + topo%aexl(i,j-1,k) * grid%a_tl(i,j-1) &
               & + topo%aexl(i-1,j-1,k) * grid%a_tr(i-1,j-1) 
        endif
      enddo
    enddo
  endif


  !-- integrate --
  f = 0.d0
  do j = j0, j1
    do i = i0, i1
      f = f + d(i,j) * area(i,j)
    enddo
  enddo
  deallocate(area)

  integ__area = f


end function integ__area
!--------------------------------------------------------------


function integ__vol(d,cgrid,para,grid,topo,istr,iend,jstr,jend,kstr,kend)
  implicit none

  real(8) :: integ__vol

  type(type_libmxe_para),intent(in) :: para
  type(type_libmxe_grid),intent(in) :: grid
  type(type_libmxe_topo),intent(in) :: topo
  real(8),intent(in) :: d(para%imut,para%jmut,para%km)
  character(1),intent(in) :: cgrid !- 'T':T-grid, 'U':U-grid
  integer,intent(in),optional :: istr,iend,jstr,jend,kstr,kend
               !- array range: [istr:iend,jstr:jend,kstr:kend]


  real(8) :: f
  integer :: i,j,k,i0,i1,j0,j1,k0,k1


  !-- check --
  if ( .not. grid%ldef )  then
    write(*,*) 'Error at integ__vol'
    write(*,*) '  grid is not registered.'
    stop
  endif
  if ( .not. topo%ldef )  then
    write(*,*) 'Error at integ__vol'
    write(*,*) '  topo is not registered.'
    stop
  endif

  if ( cgrid /= 'U' ) then
    write(*,*) ' Error : only U-grid can be used now.'
    stop
  endif


  !-- arguments (grid ranges) --
  i0 = 1
  i1 = para%imut
  j0 = 1
  j1 = para%jmut
  k0 = 1
  k1 = para%km
  if (present(istr)) i0 = istr
  if (present(iend)) i1 = iend
  if (present(jstr)) j0 = jstr
  if (present(jend)) j1 = jend
  if (present(kstr)) k0 = kstr
  if (present(kend)) k1 = kend


  !-- integrate --
  f = 0.d0
  do k = k0, k1
    do j = j0, j1
      do i = i0, i1
        f = f + d(i,j,k) * topo%dzu(i,j,k) * grid%areau(i,j)
      enddo
    enddo
  enddo
    !- dzu=0 in land


  integ__vol = f


end function integ__vol
!--------------------------------------------------------------


subroutine integ__vert( d, cgrid, para, topo, f, k_first, k_last)
  implicit none

  type(type_libmxe_para),intent(in) :: para
  character(1),          intent(in) :: cgrid !- 'T':T-grid, 'U':U-grid
  type(type_libmxe_topo),intent(in) :: topo
  real(8),               intent(in) :: d(para%imut,para%jmut,para%km)
  real(8),               intent(out):: f(para%imut,para%jmut)
  integer,      intent(in),optional :: k_first, k_last
                                                !- integration range

  real(8),pointer :: dz(:,:,:)
  integer         :: i, j, k, k0, k1

  if ( .not. topo%ldef )  then
    write(*,*) 'Error at integ__vert'
    write(*,*) '  topo is not registered.'
    stop
  endif

  if ( cgrid=='T' ) then
    dz => topo%dzt
  else
    dz => topo%dzu
  endif

  k0 = 1
  k1 = para%km
  if (present(k_first)) k0 = k_first
  if (present(k_last))  k1 = k_last

  f(:,:) = 0.d0
  do k = k0, k1
    do j = 1, para%jmut
      do i = 1, para%imut
        f(i,j) = f(i,j) + d(i,j,k) * dz(i,j,k)
      enddo
    enddo
  enddo
    !- dz=0 in land

  nullify( dz )

end subroutine integ__vert
!--------------------------------------------------------------


function integ__section( d, para, grid, topo, cgrid, l_zonal, &
                      &  i_first, i_last, j_arg, k_first, k_last)
  implicit none

  real(8)                           :: integ__section

  type(type_libmxe_para),intent(in) :: para
  type(type_libmxe_grid),intent(in) :: grid
  type(type_libmxe_topo),intent(in) :: topo
  character(1),          intent(in) :: cgrid !- 'T':T-grid, 'U':U-grid
  logical,               intent(in) :: l_zonal
                                        !- T: zonal, F:meridional
  real(8),               intent(in) :: d(para%imut,para%jmut,para%km)
  integer,               intent(in) :: i_first, i_last !- i range
  integer,               intent(in) :: j_arg
  integer,intent(in),optional       :: k_first, k_last
                                                !- layer

  real(8),pointer :: dz(:,:,:)
  integer         :: i, j, k, k0, k1
  real(8)         :: f_cm2
  real(8),allocatable :: dx_cm(:)

  if ( .not. grid%ldef )  then
    write(*,*) 'Error at integ__section'
    write(*,*) '  grid is not registered.'
    stop
  endif
  if ( .not. topo%ldef )  then
    write(*,*) 'Error at integ__section'
    write(*,*) '  topo is not registered.'
    stop
  endif
  if ( l_zonal ) then
    if ( ( j_arg < 1 ).or.( j_arg > para%jmut ) ) then
      write(*,*) 'Wrong args at integ__section'
      write(*,*) 'j_arg =', j_arg
      stop
    endif
  else
    if ( ( j_arg < 1 ).or.( j_arg > para%imut ) ) then
      write(*,*) 'Wrong args at integ__section'
      write(*,*) 'j_arg =', j_arg
      stop
    endif
  endif

  allocate( dx_cm(i_first:i_last) )
  if ( cgrid=='T' ) then
    dz => topo%dzt
    if ( l_zonal ) then
      do i = i_first, i_last
        dx_cm(i) = grid%dx_bl(i,j_arg) + grid%dx_br(max(i-1,1),j_arg)
      enddo
    else
      do j = i_first, i_last
        dx_cm(j) = grid%dy_bl(j_arg,j) + grid%dy_tl(j_arg,max(j-1,1))
      enddo
    endif
  else
    dz => topo%dzu
    if ( l_zonal ) then
      do i = i_first, i_last
        dx_cm(i) = grid%dx_tl(i,j_arg) + grid%dx_tr(i,j_arg)
      enddo
    else
      do j = i_first, i_last
        dx_cm(j) = grid%dy_tr(j_arg,j) + grid%dy_br(j_arg,j)
      enddo
    endif
  endif

  k0 = 1
  k1 = para%km
  if (present(k_first)) k0 = k_first
  if (present(k_last))  k1 = k_last

  f_cm2 = 0.d0
  do k = k0, k1

    if ( l_zonal ) then
      j = j_arg
      do i = i_first, i_last
        f_cm2 = f_cm2 + d(i,j,k) * dz(i,j,k) * dx_cm(i)
      enddo
    else
      i = j_arg
      do j = i_first, i_last
        f_cm2 = f_cm2 + d(i,j,k) * dz(i,j,k) * dx_cm(j)
      enddo
    endif
  enddo

  integ__section = f_cm2

  nullify( dz )

end function integ__section


end module integ
