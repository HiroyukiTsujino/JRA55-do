! -*-F90-*-
!- horizontal average
module have
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io
  use libmxe_grads, only: type_grads
  implicit none
  private

  character(*),parameter :: file_mask_default = 'no'


  !-- arguments --
  character(clen),save :: file_base  !- input data (file base)
  character(clen),save :: fileo  !- output file name
  logical :: l2d  !- .true.: 2D data / .false.: 3D data
  character(1),save :: cgrid  !- "T": T-grid / "U": U-grid
  character(clen) :: file_mask = file_mask_default
                           !- mask array (1.e0 or 0.e0)
  character(clen),save :: operate = ''
                           !- 'square': average square of input
  integer(4) :: i_region_number 

  public :: ini
  public :: calc
  public :: get_result
  public :: write_result
  public :: next
  public :: has_next

  real(8),allocatable,save :: f(:,:)
  integer,save :: n    !- count record loop
  integer,save :: im,jm,km,nm,reclen
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
  real,allocatable :: r4(:,:,:)

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
  logical :: l_area_write

  namelist /have_lst/ file_base, fileo, l2d, cgrid, file_mask &
                    & , i_region_number, operate, l_area_write

  i_region_number = 1
  l_area_write = .false.
  read(5,nml=have_lst,iostat=i)
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
  grads%nvar = 1
  write(ctemp,'(i3)') km
  grads%var(1) = 'f '//trim(ctemp)//' 99 Horizontal ave.'

  allocate(r4(im,jm,km))
  allocate(mask(im,jm,km),mask_valid(im,jm,km))

  mask(:,:,:) = 1.d0
  if ( file_mask /= file_mask_default ) then
    open(lun,file=trim(file_mask),form='unformatted' &
         & ,access='direct',recl=reclen,action='read')
    write(6,*) ' reading mask from ', trim(file_mask)
    read(lun,rec=1) r4
    close(lun)
    where(r4(:,:,:) /= real(i_region_number,4))
      mask(:,:,:) = 0.d0
    end where
  endif


  if (l_area_write) then
    open(lun,file='areau.gd',form='unformatted' &
         & ,access='direct',recl=4*im*jm,action='write')
    write(lun,rec=1) real(grid%areau(1:im,1:jm),4)
    close(lun)
  end if

  allocate(d(im,jm),rarea(km),f(km,nm))

  f(1:km,1:nm)=0.d0
  rarea(1:km)=0.d0

  do k = 1, km
    d(1:im,1:jm) = mask(1:im,1:jm,k)
    rarea(k) = integ__area(d,k,cgrid,para,grid,topo)
    write(6,*) 'area: ',rarea(k),' [cm^2] at k=',k
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

  call libmxe_io__open(io,trim(file_base),n &
       & ,reclen,lun,action='read')
  read(lun,rec=1) r4
  close(lun)

  rarea(:) = 0.0d0

!  where ( r4 == para%rundefin ) r4 = 0.e0  !- missing value!

  mask_valid(:,:,:) = mask(:,:,:)

  do k = 1, km

    do j = 1, jm
      do i = 1, im
        if (r4(i,j,k) == para%rundefin) then
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

    f(k,n) = integ__area(d,k,cgrid,para,grid,topo) * rarea(k)
  enddo

  write(*,*) n,f(1,n)

end subroutine calc


subroutine write_result
  use libmxe_grads, only: libmxe_grads__make
  implicit none

  call libmxe_grads__make(grads,para,grid,io)
  open(lun,file=trim(fileo)//'.gd',form='unformatted' &
       & ,access='direct',recl=4*km*nm,action='write')
    write(lun,rec=1) real(f(1:km,1:nm))
  close(lun)

end subroutine write_result


real(8) function get_result(k,nn)
  implicit none

  integer,intent(in) :: k,nn

  get_result = f(k,nn)

end function get_result


end module have
