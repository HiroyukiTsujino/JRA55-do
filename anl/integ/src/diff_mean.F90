! -*-F90-*-
!-- Mean difference between 2 outputs
module diff_mean
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io
  use libmxe_grads, only: type_grads
  implicit none
  private


  !-- arguments --
  character(clen) :: file_namelist1  !- EXP1 namelist
  character(clen) :: file_namelist2  !- EXP2 namelist
  character(clen),save :: file_base1  !- EXP1 input file base
  character(clen),save :: file_base2  !- EXP2 input file base
  character(clen),save :: fileo  !- output file name
  logical :: l2d  !- .true.: 2D data / .false.: 3D data
  character(1) :: cgrid  !- "T": T-grid / "U": U-grid
  real,save :: missing  !- missing value
  integer,save :: nstr = 1  !- start N  [default: 1]
  integer,save :: nend = 0  !- end N [default: nm]
  character(clen),save :: operate = ''
                           !- 'square': average square of input


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

  type(type_libmxe_para),save :: para1,para2
  type(type_libmxe_grid),save :: grid1
  type(type_libmxe_topo),save :: topo1
  type(type_libmxe_io),save :: io1,io2
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

  namelist /diff_mean_lst/ file_namelist1, file_namelist2 &
            & , file_base1, file_base2 &
            & , fileo, l2d, cgrid, missing, nstr, nend &
            & , operate

  !---- arguments ----
  missing = missing_default
  read(5,nml=diff_mean_lst,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  !---- experiment settings ----
  call libmxe_para__register(para1 &
             & ,file_namelist=trim(file_namelist1))
  call libmxe_grid__register(grid1,para1)
  call libmxe_topo__register(topo1,para1)
  call libmxe_io__register(io1,para1)
  im = para1%imut
  jm = para1%jmut
  km = para1%km
  if ( l2d ) km = 1
  reclen = im*jm*km*4
  if ( nend == 0 ) nend = io1%nm
  if ( ( nstr < 1).or.( nstr > io1%nm ) &
       & .or.( nend < 1).or.( nend > io1%nm ) ) then
    write(*,*) 'Error at ini'
    write(*,*) '  Wrong nstr, nend',nstr,nend
    stop
  endif

  call libmxe_para__register(para2 &
             & ,file_namelist=trim(file_namelist2))
  call libmxe_io__register(io2,para2)

  !-- topography --
  allocate( exnn(im,jm) )
  if ( cgrid=='U' ) then
    exnn(1:im,1:jm) = topo1%exnn(1:im,1:jm)
  else
    exnn(1:im,1:jm) = topo1%texnn(1:im,1:jm)
  endif

  !-- grads control file --
  grads%file_base = trim(fileo)
  grads%title = 'Mean anomaly: '//trim(fileo) & 
               & //' '//trim(operate)
  grads%cgrid = cgrid
  grads%ztype = 'center'
  if ( l2d ) grads%ztype = 'surface'
  grads%timemode = 'stationary'
  grads%nvar = 1
  write(ctemp,'(i3)') km
  grads%var(1) = 'f '//trim(ctemp)//' 99 Mean anomaly'

  allocate(r(im,jm,km))

end subroutine ini


subroutine calc
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer :: i,j,k,n
  real,allocatable :: r1(:,:,:), r2(:,:,:)
  real(8),allocatable :: d(:,:,:)

  allocate(r1(im,jm,km),r2(im,jm,km))
  allocate(d(im,jm,km))
  d = 0.d0

  do n = nstr, nend

    if ( mod(n,100) == 0 ) write(*,*) n

    !-- input --
    call libmxe_io__open(io1,trim(file_base1),n,reclen &
         & ,lun,action='read')
    read(lun,rec=1) r1
    close(lun)
    call libmxe_io__open(io2,trim(file_base2),n,reclen &
         & ,lun,action='read')
    read(lun,rec=1) r2
    close(lun)

    if ( operate == 'square' ) then
      d = d + ( dble( r1 ) - dble( r2 ) )**2
    else
      d = d + ( dble( r1 ) - dble( r2 ) )
    endif

  enddo

  r = real( d / dble( nend - nstr + 1 ) )

  !-- land --
  do j = 1, jm
    do i = 1, im
      do k = 1, km
        if ( exnn(i,j) < k ) r(i,j,k) = para1%rundefout
      enddo
    enddo
  enddo

  !-- missing --
  if ( missing /= missing_default ) then
    do k = 1, km
      do j = 1, jm
        do i = 1, im
          if ( ( r1(i,j,k) == missing ) &
               & .or.( r2(i,j,k) == missing )) then
            r(i,j,k) = para1%rundefout
          endif
        enddo
      enddo
    enddo
  endif

  deallocate(r1,r2,d)

end subroutine calc


subroutine write_result
  use libmxe_grads, only: libmxe_grads__make
  implicit none

  call libmxe_grads__make(grads,para1,grid1,io1)
  open(lun,file=trim(fileo)//'.gd',form='unformatted' &
     & , access='direct',recl=reclen,action='write')
    write(lun,rec=1) r
  close(lun)

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

  call libmxe_topo__aexl(topo1,para1)
  allocate(d(im,jm))

  do k = 1, km

    d(1:im,1:jm) = 1.d0
    do j = 1, jm
      do i = 1, im
        if ( r(i,j,k) == para1%rundefout ) d(i,j) = 0.d0
      enddo
    enddo
    area = integ__area(d,k,cgrid,para1,grid1,topo1)

    d(1:im,1:jm) = 0.d0
    do j = 1, jm
      do i = 1, im
        if ( r(i,j,k) /= para1%rundefout ) then
          d(i,j) = dble( r(i,j,k) )
        endif
      enddo
    enddo
    write(*,*) k, integ__area(d,k,cgrid,para1,grid1,topo1) / area 
  enddo

  deallocate(d)

end subroutine write_ave


end module diff_mean
