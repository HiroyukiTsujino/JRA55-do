! -*-F90-*-
!- horizontal average
module have_on_ice
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io
  use libmxe_grads, only: type_grads
  implicit none
  private

  character(*),parameter :: file_mask_default = 'no'


  !-- arguments --
  character(clen),save :: file_base      !- input data (file base)
  character(clen),save :: file_base_ice  !- input data (file base)
  character(clen),save :: fileo  !- output file name
  logical :: l2d  !- .true.: 2D data / .false.: 3D data
  character(1),save :: cgrid  !- "T": T-grid / "U": U-grid
  character(clen) :: file_mask = file_mask_default
                           !- mask array (1.e0 or 0.e0)
  character(clen),save :: operate = ''
                           !- 'square': average square of input

  public :: ini
  public :: calc
  public :: get_result
  public :: write_result
  public :: next
  public :: has_next


  real(8),allocatable,save :: fi(:,:)  ! over ice
  real(8),allocatable,save :: fo(:,:)  ! over sea
  integer(4),save :: n    !- count record loop
  integer(4),save :: im,jm,km,nm,reclen
  real(8),allocatable,save :: rarea(:)
  real(8),allocatable,save :: mask(:,:,:)
  real(8),allocatable,save :: mask_valid(:,:,:)

  integer,parameter :: lun = 80

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save :: io
  type(type_grads),save :: grads

  real(8),allocatable :: d(:,:)
  real(8),allocatable :: ice(:,:)
  real(4),allocatable :: r4(:,:,:)

  real(8) :: def_ice = 0.15d0

contains


subroutine next
  implicit none

  n = n + 1
 
end subroutine next


logical function has_next
  implicit none

  if ( n > io%nm ) then
    has_next = .false.
  else
    has_next = .true.
  endif

end function has_next


subroutine ini
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register &
                      & , libmxe_topo__aexl
  use libmxe_io, only: libmxe_io__register
  use integ, only: integ__area
  implicit none

  integer :: i,k
  character(3) :: ctemp

  !-----

  namelist /have_ice_lst/ file_base, file_base_ice, fileo, def_ice, &
       & cgrid, file_mask, operate

  !-----

  l2d = .true.

  read(5,nml=have_ice_lst,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)
  call libmxe_io__register(io,para)
  im = para%imut
  jm = para%jmut
  km = para%km
  if ( l2d ) km = 1
  nm = io%nm
  reclen = 4*im*jm*km

  grads%file_base = trim(fileo)
  grads%title = 'Horizontal average : '//trim(fileo) & 
               & //' '//trim(operate)
  grads%cgrid = cgrid
  grads%istr=1
  grads%iend=1
  grads%jstr=1
  grads%jend=1
  grads%ztype = 'center'
  if ( l2d ) grads%ztype = 'surface'
  grads%nvar = 2
  write(ctemp,'(i3)') km
  grads%var(1) = 'fi '//trim(ctemp)//' 99 Horizontal ave. over sea ice'
  grads%var(2) = 'fo '//trim(ctemp)//' 99 Horizontal ave. over ocean'

  allocate(r4(im,jm,km))
  allocate(mask(im,jm,km),mask_valid(im,jm,km))

  mask(:,:,:) = 1.d0
  if ( file_mask /= file_mask_default ) then
    open(lun,file=trim(file_mask),form='unformatted' &
         & ,access='direct',recl=reclen,action='read')
    write(6,*) ' reading mask from ', trim(file_mask)
    read(lun,rec=1) r4
    close(lun)
    mask(:,:,:) = dble( r4(:,:,:) ) 
  endif

  allocate(d(im,jm),ice(im,jm),rarea(km),fi(km,nm),fo(km,nm))

  fi(1:km,1:nm)=0.d0
  fo(1:km,1:nm)=0.d0
  rarea(1:km)=0.d0

  do k = 1, km
    d(1:im,1:jm) = mask(1:im,1:jm,k)
    rarea(k) = integ__area(d,k,cgrid,para,grid,topo)
    write(6,*) 'target area: ',rarea(k),' at k=',k
    if ( rarea(k) > 0.d0 ) rarea(k) = 1.d0 / rarea(k)
  enddo

  n = 1

end subroutine ini


subroutine calc
  use libmxe_io, only: libmxe_io__open
  use integ, only: integ__area
  implicit none

  integer :: i, j, k

  if ( .not. para%ldef ) then
    write(*,*) 'Error: calc is called before ini.'
    stop
  endif

  ! sea ice

  call libmxe_io__open(io,trim(file_base_ice),n,reclen,lun,action='read')
  read(lun,rec=1) r4
  close(lun)

  do j = 1, jm
    do i = 1, im
      ice(i,j) = real(r4(i,j,1),8)
    end do
  end do

  ! target data

  call libmxe_io__open(io,trim(file_base),n,reclen,lun,action='read')
  read(lun,rec=1) r4
  close(lun)

  rarea(:) = 0.0d0

!  where ( r4 == para%rundefin ) r4 = 0.e0  !- missing value!

  !-----------------------
  ! ice covered region

  mask_valid(:,:,:) = mask(:,:,:)

  do k = 1, km

    do j = 1, jm
      do i = 1, im
        if ((ice(i,j) == real(para%rundefin,8)) .or. (ice(i,j) < def_ice) .or. (r4(i,j,k) == para%rundefin)) then
          mask_valid(i,j,k) = 0.0d0
        end if
      end do
    end do

    d(1:im,1:jm) = mask_valid(1:im,1:jm,k)

    rarea(k) = integ__area(d,k,cgrid,para,grid,topo)
    if ( rarea(k) > 0.d0 ) rarea(k) = 1.d0 / rarea(k)

    if ( operate == 'square' ) then
      d(1:im,1:jm) = mask_valid(1:im,1:jm,k) * dble(r4(1:im,1:jm,k))**2
    else
      d(1:im,1:jm) = mask_valid(1:im,1:jm,k) * dble(r4(1:im,1:jm,k))
    end if

    fi(k,n) = integ__area(d,k,cgrid,para,grid,topo) * rarea(k)

  end do

  !-----------------------
  ! ice free region

  mask_valid(:,:,:) = mask(:,:,:)

  do k = 1, km

    do j = 1, jm
      do i = 1, im
        if ((ice(i,j) == real(para%rundefin,8)) .or. (ice(i,j) > 0.0d0) .or. (r4(i,j,k) == para%rundefin)) then
          mask_valid(i,j,k) = 0.0d0
        end if
      end do
    end do

    d(1:im,1:jm) = mask_valid(1:im,1:jm,k)

    rarea(k) = integ__area(d,k,cgrid,para,grid,topo)
    if ( rarea(k) > 0.d0 ) rarea(k) = 1.d0 / rarea(k)

    if ( operate == 'square' ) then
      d(1:im,1:jm) = mask_valid(1:im,1:jm,k) * dble(r4(1:im,1:jm,k))**2
    else
      d(1:im,1:jm) = mask_valid(1:im,1:jm,k) * dble(r4(1:im,1:jm,k))
    endif

    fo(k,n) = integ__area(d,k,cgrid,para,grid,topo) * rarea(k)

  end do

  write(*,*) n,fi(1,n),fo(1,n)

end subroutine calc


subroutine write_result
  use libmxe_grads, only: libmxe_grads__make
  implicit none
  integer(4) :: n, irec

  call libmxe_grads__make(grads,para,grid,io)
  open(lun,file=trim(fileo)//'.gd',form='unformatted' &
       & ,access='direct',recl=4*km,action='write')
  irec = 0
  do n = 1, nm
    write(*,*) 'write', n,fi(1,n),fo(1,n)
    irec = irec + 1
    write(lun,rec=irec) real(fi(1,n),4)
    irec = irec + 1
    write(lun,rec=irec) real(fo(1,n),4)
  end do
  close(lun)

end subroutine write_result


real(8) function get_result(k,nn)
  implicit none

  integer,intent(in) :: k,nn

  get_result = fi(k,nn)

end function get_result


end module have_on_ice
