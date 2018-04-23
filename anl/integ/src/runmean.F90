! -*-F90-*-
!-- Calc running mean
module runmean
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io
  use libmxe_grads, only: type_grads
  implicit none
  private


  !-- arguments --
  character(clen),save :: file_base  !- input data (file base)
  character(clen),save :: diro       !- output directory
  character(clen),save :: fileo      !- output file name
  logical :: l2d                !- .true.: 2D data / .false.: 3D data
  character(1) :: cgrid              !- "T": T-grid / "U": U-grid
  integer,save :: width              !- average sample width
  character(clen),save :: width_unit = 'record' !- width unit:
                                     !- 'record', 'minute'
  integer,save :: nstr = 1  !- start N  [default: 1]
  integer,save :: nend = 0  !- end N [default: nm]


  public :: ini
  public :: calc
  public :: get_result
  public :: write_result
  public :: next
  public :: has_next


  integer, parameter :: lun = 77
  real,allocatable,save :: r(:,:,:)
  real(8),allocatable,save :: d(:,:,:),w(:,:),wsum(:)
  integer,allocatable,save :: nrec_str(:),nrec_end(:),nrec_sum(:)
  integer,allocatable,save :: exnn(:,:)
  integer,save :: im, jm, km, nm, reclen
  integer,save :: nrec    !- count record loop

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save :: io
  type(type_grads),save :: grads


contains


subroutine next
  implicit none

  nrec = nrec + 1
 
end subroutine next


logical function has_next
  implicit none

  if ( nrec > nend ) then
    has_next = .false.
  else
    has_next = .true.
  endif

end function has_next


subroutine ini
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register
  use libmxe_io, only: libmxe_io__register
  use libmxe_grads, only: libmxe_grads__make
  implicit none

  integer :: i, n, nwidth, nwidth_half
  character(3) :: ctemp

  namelist /runmean_lst/ file_base, width, width_unit &
            & , diro, fileo, l2d, cgrid, nstr, nend

  !---- arguments ----
  read(5,nml=runmean_lst,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist'
    stop
  endif

  !---- experiment settings ----
  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_io__register(io,para)
  im = para%imut
  jm = para%jmut
  km = para%km
  nm = io%nm
  if ( l2d ) km = 1
  reclen = im*jm*km*4
  if ( nend == 0 ) nend = nm
  if ( ( nstr < 1).or.( nstr > nm ) &
       & .or.( nend < 1).or.( nend > nm ) ) then
    write(*,*) 'Error at ini'
    write(*,*) '  Wrong nstr, nend',nstr,nend
    stop
  endif

  !-- topography --
  allocate( exnn(im,jm) )
  if ( cgrid=='U' ) then
    exnn(1:im,1:jm) = topo%exnn(1:im,1:jm)
  else
    exnn(1:im,1:jm) = topo%texnn(1:im,1:jm)
  endif

  !-- prepare averaging --

  !- max record width
  if ( width_unit == 'record' ) then

    if ( width <= 1 ) then
      write(*,*) 'Error at ini'
      write(*,*) '  Wrong width:',width
      stop
    endif
    nwidth_half = width / 2

  else
    write(*,*) 'Error at ini'
    write(*,*) '  Wrong width_unit:',trim(width_unit)
    stop
  endif

  !- record number of start and end
  allocate(nrec_str(nm),nrec_end(nm),nrec_sum(nm))
  do n = 1, nm
    nrec_str(n) = max( n - nwidth_half , 1 )
    nrec_end(n) = min( n + nwidth_half , nm )
    nrec_sum(n) = nrec_end(n) - nrec_str(n) + 1
  enddo

  !- weight factor
  nwidth = nwidth_half * 2 + 1
  allocate(w(nwidth,nm),wsum(nm))
  w(:,:) = 0.d0
  if ( width_unit == 'record' ) then

    do n = 1, nm
      i = nrec_sum(n)
      w(1:i,n) = 1.d0
    enddo
    if ( mod( nwidth , 2 ) == 0 ) then
      do n = 1, nm
        if ( nrec_str(n) == ( n - nwidth_half ) ) w(1,n) = 0.5d0
        if ( nrec_end(n) == ( n + nwidth_half ) ) w(nrec_sum(n),n) = 0.5d0
      enddo
    endif

  endif

  do n = 1, nm    
    wsum(n) = sum( w(:,n) )
  enddo

  !-- grads control file --
  grads%file_base = trim(fileo)
  grads%title = 'Runmean: '//trim(fileo)
  grads%cgrid = cgrid
  grads%ztype = 'center'
  if ( l2d ) grads%ztype = 'surface'
  grads%nvar = 1
  write(ctemp,'(i3)') km
  grads%var(1) = 'f '//trim(ctemp)//' 99 running mean'
  call libmxe_grads__make(grads,para,grid,io)

  allocate(r(im,jm,km),d(im,jm,km))

  nrec = nstr

end subroutine ini


subroutine calc
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer :: i,j,k,n


  if ( mod(nrec,100) == 0 ) write(*,*) nrec
  d(:,:,:) = 0.d0

  !-- average subset --
  i = 0
  do n = nrec_str(nrec), nrec_end(nrec)
    call libmxe_io__open(io,trim(file_base),n,reclen &
       & ,lun,action='read')
      read(lun,rec=1) r
    close(lun)
    i = i + 1
    d(:,:,:) = d(:,:,:) + w(i,nrec) * dble( r(:,:,:) )
  enddo

  r(:,:,:) = real( d(:,:,:) / wsum(nrec) )

  !-- land --
  do j = 1, jm
    do i = 1, im
      do k = 1, km
        if ( exnn(i,j) < k ) r(i,j,k) = para%rundefout
      enddo
    enddo
  enddo

end subroutine calc


subroutine write_result
  use libmxe_io, only: libmxe_io__open
  implicit none

  call libmxe_io__open(io,trim(diro)//'/'//trim(fileo) &
                  & , nrec, reclen, lun, action='write')
    write(lun,rec=1) r(:,:,:)
  close(lun)

end subroutine write_result


real function get_result(i,j,k)
  implicit none

  integer,intent(in) :: i,j,k

  get_result = r(i,j,k)

end function get_result


end module runmean
