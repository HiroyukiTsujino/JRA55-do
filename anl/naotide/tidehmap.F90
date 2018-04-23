! -*-F90-*-
!- Make tidal height fields using naotide.f .
module tidehmap
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io
  implicit none
  private


  !-- arguments --
  character(clen) :: diro !- output directory
  logical :: ltopo        !- .true.: use topography


  public :: init
  public :: calc
  public :: get_result
  public :: write_result
  public :: next
  public :: has_next


  character(*),parameter :: fileo='naoh'
  real,parameter :: rundef_nao = 9999.99e0
  integer,parameter :: lun=10
  real,allocatable,save :: h(:,:)
  integer,allocatable,save :: texnn(:,:)
  integer,save :: n    !- count record loop
  integer,save :: imut,jmut

  type(type_libmxe_para),save :: para
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save :: io


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


subroutine init
  use libmxe_para, only: libmxe_para__register
  use libmxe_grid, only: libmxe_grid__register
  use libmxe_topo, only: libmxe_topo__register
  use libmxe_io, only: libmxe_io__register
  use libmxe_grads, only: libmxe_grads__make, type_grads
  implicit none

  integer :: i
  type(type_grads) :: grads

  namelist /tidehmap_lst/ diro, ltopo

  !---- arguments ----
  read(5,nml=tidehmap_lst,iostat=i)
  if (i/=0) then
    write(*,*) 'Error: no namelist'
    stop
  endif

  !---- experiment settings ----
  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_io__register(io,para)
  imut = para%imut
  jmut = para%jmut

  !---- topography ----
  allocate(texnn(imut,jmut))
  texnn(1:imut,1:jmut) = 1
  if ( ltopo ) then
    call libmxe_topo__register(topo,para)
    texnn(1:imut,1:jmut) = topo%texnn(1:imut,1:jmut)
  endif

  !---- make grads control file ----
  grads%file_base = fileo
  grads%title = 'nao99b tidal height'
  grads%cgrid = 'T'
  grads%ztype = 'surface'
  grads%nvar = 1
  grads%var(1) = 'h 1 99 tidal height [cm]'
  grads%undef = rundef_nao
  call libmxe_grads__make(grads,para,grid,io)

  allocate(h(imut,jmut))
  n = 1

end subroutine init


subroutine calc
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer,parameter :: itmode=2, lpmode=2
  real(8) :: time, height, hsp, hlp, x, y
  integer :: i,j, iy,imon,id,ih,imin,isec
  logical :: Ldata

  write(*,*) 'nrec, date, Julian date'

  !-- get modified Julian date (time) [day] --
  iy=io%calrec(n)%year
  imon=io%calrec(n)%month
  id=io%calrec(n)%day
  ih=io%calrec(n)%hour
  imin=io%calrec(n)%minute
  isec=io%calrec(n)%second
  call mjdymd(time, iy, imon, id, ih, imin, isec, 1)
  write(*,'(i4,i5,i2,i2,i3,i2.2,f12.4)') &
       &  n,iy, imon, id, ih, imin, time

  !-- get tidal height at model grid (height) --
  h(1:imut,1:jmut) = rundef_nao
  do j = 1, jmut
    do i = 1, imut
      if ( texnn(i,j) /= 0 ) then  !- sea
        x = grid%glont(i,j)
        y = grid%glatt(i,j)
        call naotide(x,y,time,itmode,lpmode &
             & ,height,hsp,hlp,Ldata)
        !          if ( .not.Ldata ) then
        !            write(*,*) 'Error at i,j,x,y :',i,j,real(x),real(y)
        !          endif
        h(i,j) = real(height)
      endif
    enddo
  enddo

end subroutine calc


subroutine write_result
  use libmxe_io, only: libmxe_io__open
  implicit none

  call libmxe_io__open( io, trim(diro)//'/'//fileo, n &
       & ,imut*jmut*4, lun, action='write' )
    write(lun,rec=1) h
  close(lun)

end subroutine write_result


real function get_result(i,j)
  implicit none

  integer,intent(in) :: i,j

  get_result = h(i,j)

end function get_result


end module tidehmap
