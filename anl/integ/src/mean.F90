! -*-F90-*-
!-- Time mean
module mean
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io
  use libmxe_grads, only: type_grads
  implicit none
  private


  !-- arguments --
  character(clen),save :: file_base  !- input data (file base)
  character(clen),save :: fileo      !- output file name
  logical :: l2d                     !- T: 2D data / F: 3D
  character(1) :: cgrid              !- "T": T-grid / "U": U
  real,save :: missing               !- missing value
  integer,save :: nstr = 1           !- start N  [default: 1]
  integer,save :: nend = 0           !- end N [default: nm]
  character(clen),save :: operate = ''
                           !- 'square': average square of input
  logical :: lsuccessive = .false.   !- T: successive mode
                                     !-    low cost


  public :: ini
  public :: calc
  public :: get_result
  public :: write_result
  public :: write_ave


  integer, parameter :: lun = 77
  real, parameter :: missing_default = -999.999e10
  real,allocatable,save :: r(:,:,:)
  integer,allocatable,save :: exnn(:,:)
  integer,save :: im, jm, km, nm, reclen

  real,allocatable :: r1(:,:,:)
  real(8),allocatable :: d(:,:,:)

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save :: io
  type(type_grads),save :: grads


contains


subroutine ini
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register
  use libmxe_io, only: libmxe_io__register
  implicit none

  integer :: i
  character(3) :: ctemp

  namelist /mean_lst/ file_base &
            & , fileo, l2d, cgrid, missing, nstr, nend &
            & , operate, lsuccessive

  !---- arguments ----
  missing = missing_default
  read(5,nml=mean_lst,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  !---- experiment settings ----
  call libmxe_para__register(para)
  if ( .not. lsuccessive ) then
    call libmxe_grid__register(grid,para)
    call libmxe_topo__register(topo,para)
  endif
  call libmxe_io__register(io,para)
  im = para%imut
  jm = para%jmut
  km = para%km
  if ( l2d ) km = 1
  reclen = im*jm*km*4
  if ( nend == 0 ) nend = io%nm
  if ( ( nstr < 1).or.( nstr > io%nm ) &
       & .or.( nend < 1).or.( nend > io%nm ) ) then
    write(*,*) 'Error at ini'
    write(*,*) '  Wrong nstr, nend',nstr,nend
    stop
  endif

  !-- allocate --
  allocate(r(im,jm,km))
  allocate(r1(im,jm,km))
  allocate(d(im,jm,km))

  if ( lsuccessive ) return

  !-- topography --
  allocate( exnn(im,jm) )
  if ( cgrid=='U' ) then
    exnn(1:im,1:jm) = topo%exnn(1:im,1:jm)
  else
    exnn(1:im,1:jm) = topo%texnn(1:im,1:jm)
  endif

  !-- grads control file --
  grads%file_base = trim(fileo)
  grads%title = 'Mean: '//trim(fileo)//' '//trim(operate)
  grads%cgrid = cgrid
  grads%ztype = 'center'
  if ( l2d ) grads%ztype = 'surface'
  grads%timemode = 'stationary'
  grads%nvar = 1
  write(ctemp,'(i3)') km
  grads%var(1) = 'f '//trim(ctemp)//' 99 Mean'

end subroutine ini


subroutine calc
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer :: i,j,k,n

  d(:,:,:) = 0.d0

  do n = nstr, nend

    if ( mod(n,100) == 0 ) write(*,*) n

    !-- input --
    call libmxe_io__open(io,trim(file_base),n,reclen &
         & ,lun,action='read')
    read(lun,rec=1) r1
    close(lun)

    if ( operate == 'square' ) then
      d = d + dble( r1 )**2
    else
      d = d + dble( r1 )
    endif

  enddo

  r(:,:,:) = real( d(:,:,:) / dble( nend - nstr + 1 ) )

  if ( lsuccessive ) return

  !-- land --
  do j = 1, jm
    do i = 1, im
      do k = 1, km
        if ( exnn(i,j) < k ) r(i,j,k) = para%rundefout
      enddo
    enddo
  enddo

  !-- missing --
  if ( missing /= missing_default ) then
    do k = 1, km
      do j = 1, jm
        do i = 1, im
          if ( r1(i,j,k) == missing ) then
            r(i,j,k) = para%rundefout
          endif
        enddo
      enddo
    enddo
  endif

end subroutine calc


subroutine write_result
  use libmxe_grads, only: libmxe_grads__make
  use libmxe_io, only: libmxe_io__open
  implicit none


  if ( lsuccessive ) then
    call libmxe_io__open(io,trim(fileo),nend,reclen &
          & ,lun,action='write')
  else
    open(lun,file=trim(fileo)//'.gd',form='unformatted' &
       & , access='direct',recl=reclen,action='write')
  endif

    write(lun,rec=1) r
  close(lun)

  if ( lsuccessive ) return

  call libmxe_grads__make(grads,para,grid,io)

end subroutine write_result


real function get_result(i,j,k)
  implicit none

  integer,intent(in) :: i,j,k

  get_result = r(i,j,k)

end function get_result


subroutine write_ave
  use libmxe_topo, only: libmxe_topo__aexl
  use integ, only: integ__area
  implicit none

  real(8),allocatable :: d(:,:)
  real(8) :: area
  integer :: i,j,k

  if ( lsuccessive ) return

  call libmxe_topo__aexl(topo,para)
  allocate(d(im,jm))

  do k = 1, km

    d(1:im,1:jm) = 1.d0
    do j = 1, jm
      do i = 1, im
        if ( r(i,j,k) == para%rundefout ) d(i,j) = 0.d0
      enddo
    enddo
    area = integ__area(d,k,cgrid,para,grid,topo)

    d(1:im,1:jm) = 0.d0
    do j = 1, jm
      do i = 1, im
        if ( r(i,j,k) /= para%rundefout ) then
          d(i,j) = dble( r(i,j,k) )
        endif
      enddo
    enddo
    write(*,*) k, integ__area(d,k,cgrid,para,grid,topo) / area 
  enddo

  deallocate(d)

end subroutine write_ave


end module mean
