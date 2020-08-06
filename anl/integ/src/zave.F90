! -*-F90-*-
!- zonal average
module zave
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
  character(clen),save :: fileo_base  !- output file name
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


  real(8),allocatable,save :: f(:,:)
  integer,save :: n    !- count record loop
  integer,save :: im,jm,km,nm,reclen,reclen_out
  real(8),allocatable,save :: rarea(:,:)
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
  integer(4) :: i_region_number 
  integer(4) :: ex_region_number 

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

  integer :: i,j,k
  character(3) :: ctemp
  real(8) :: area, area_ocean

  namelist /zave_lst/ file_base, fileo_base, l2d, cgrid, file_mask &
                    & , i_region_number, ex_region_number, operate

  i_region_number = 1
  ex_region_number = -999

  read(5,nml=zave_lst,iostat=i)

  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  else
    write(6,*) ' Region number = ', i_region_number
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
  reclen_out = 4*jm*km

  grads%file_base = trim(fileo_base)
  grads%title = 'Zonal average : '//trim(fileo_base) & 
               & //' '//trim(operate)
  grads%cgrid = cgrid
  grads%istr=1
  grads%iend=1
  grads%jstr=1
  grads%jend=jm
  grads%ztype = 'center'
  if ( l2d ) grads%ztype = 'surface'
  grads%nvar = 1
  write(ctemp,'(i3)') km
  grads%var(1) = 'f '//trim(ctemp)//' 99 Zonal ave.'

  allocate(r4(im,jm,km))
  allocate(mask(im,jm,km),mask_valid(im,jm,km))

  mask(:,:,:) = 1.d0
  if ( file_mask /= file_mask_default ) then
    write(6,*) ' read mask from ... ', trim(file_mask)
    open(lun,file=trim(file_mask),form='unformatted' &
         & ,access='direct',recl=reclen,action='read')
    read(lun,rec=1) r4
    close(lun)
    if (i_region_number > 0) then
      where(r4(:,:,:) /= real(i_region_number,4))
        mask(:,:,:) = 0.d0
      end where
    else
      where(r4(:,:,:) <= 0.0)
        mask(:,:,:) = 0.d0
      end where
    end if
  endif

  if (ex_region_number > 0) then
    where(r4(:,:,:) == real(ex_region_number,4))
      mask(:,:,:) = 0.d0
    end where
  end if

  allocate(d(im,jm),rarea(jm,km),f(jm,km))

  f(1:jm,1:km)=0.d0
  rarea(1:jm,1:km)=0.d0

  area_ocean = 0.0d0

  k = 1
  d(1:im,1:jm) = mask(1:im,1:jm,k)
  do j = 1, jm
    area = integ__area(d,k,cgrid,para,grid,topo,&
         & istr=1,iend=im,jstr=j,jend=j)
    area_ocean = area_ocean + area
    write(6,*) j, area
  end do

  write(6,*) ' AREA of the Ocean = ', area_ocean * 1.0d-10, ' km^2 '

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

  call libmxe_io__open(io,trim(file_base),n,reclen,lun,action='read')
  read(lun,rec=1) r4
  close(lun)

  rarea(:,:) = 0.0d0
  f(:,:) = 0.0d0

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

    do j = 1, jm
      rarea(j,k) = integ__area(d,k,cgrid,para,grid,topo,&
           & istr=1,iend=im,jstr=j,jend=j)
      if ( rarea(j,k) > 0.d0 ) rarea(j,k) = 1.d0 / rarea(j,k)
    end do

    if ( operate == 'square' ) then
      d(1:im,1:jm) = mask_valid(1:im,1:jm,k) * dble(r4(1:im,1:jm,k))**2
    else
      d(1:im,1:jm) = mask_valid(1:im,1:jm,k) * dble(r4(1:im,1:jm,k))
    endif

    do j = 1, jm
      f(j,k) = integ__area(d,k,cgrid,para,grid,topo,&
           & istr=1,iend=im,jstr=j,jend=j)
      f(j,k) = f(j,k) * rarea(j,k)
      if (rarea(j,k) == 0.0d0) then
        f(j,k) = para%rundefout
      end if
    end do

  end do

end subroutine calc


subroutine write_result
  use libmxe_io, only: libmxe_io__open
  use libmxe_grads, only: libmxe_grads__make
  implicit none

  if (n == io%nm) then
    call libmxe_grads__make(grads,para,grid,io)
  end if

  call libmxe_io__open(io, trim(fileo_base), n, reclen_out, lun, action='write')
  write(lun,rec=1) real(f(1:jm,1:km))
  close(lun)

end subroutine write_result


real(8) function get_result(j,k)
  implicit none

  integer,intent(in) :: j,k

  get_result = f(j,k)

end function get_result


end module zave
