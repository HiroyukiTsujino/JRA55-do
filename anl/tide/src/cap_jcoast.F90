! -*-F90-*-
!-- Calc RMSE and capture ratio of tidal height.
module cap_jcoast
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo
  use libmxe_io, only: type_libmxe_io
  use jcoast, only: njcoast
  implicit none
  private


  !-- arguments --
  character(clen),save :: file_base  !- SSH data (file base)
  character(clen),save :: file_base_ave !- SSH data (average)
  integer,save :: nstn               !- number of station
  integer,save :: nstr = 1           !- start N  [default: 1]
  integer,save :: nend = 0           !- end N [default: nm]

  character(*),parameter :: file_table = 'stationf.tbl'
  character(*),parameter :: fileo = 'cap.tbl'
  character(*),parameter :: file_namelist_ave='namelist.configure.in.ave'
  character(*),parameter :: dir_jcoast = 'japan_coast/'
  character(*),parameter :: cyear = '.2001'

  public :: ini
  public :: calc
  public :: get_result
  public :: write_result
  public :: next
  public :: has_next


  integer, parameter :: lun = 77
  character(20),allocatable,save :: cstn(:)
  real,allocatable,save :: lons(:),lats(:)
  real(8),allocatable,save :: rms(:),rmse(:),cap(:),ht(:),hta(:)
  integer,allocatable,save :: indx(:)
  integer,save :: im, jm, nm, reclen, mstr, mend, mindx
  integer,save :: nrec    !- count station loop

  real,allocatable :: r(:,:),ra(:,:)
  real :: h(njcoast),ha(njcoast)

  type(type_libmxe_para),save :: para, paraa
  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo
  type(type_libmxe_io),save :: io, ioa


contains


subroutine next
  implicit none

  nrec = nrec + 1
 
end subroutine next


logical function has_next
  implicit none

  if ( nrec > nstn ) then
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
  use libmxe_calendar, only: type_calendar, libmxe_calendar__diffsec
  implicit none

  integer :: i, m, n, indx_intv
  type(type_calendar) :: cal

  namelist /cap_jcoast_lst/ file_base, file_base_ave, nstn &
            & , nstr, nend

  !---- arguments ----
  read(5,nml=cap_jcoast_lst,iostat=i)
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
  nm = io%nm
  reclen = im*jm*4

  if ( nend == 0 ) nend = nm
  if ( ( nstr < 1).or.( nstr > nm ) &
       & .or.( nend < 1).or.( nend > nm ) ) then
    write(*,*) 'Error at ini'
    write(*,*) '  Wrong nstr, nend',nstr,nend
    stop
  endif

  if ( ( ( io%calint%hour == 0 ).and.(io%calint%minute == 30 ) ) &
   & .or.( ( io%calint%hour == 1 ).and.(io%calint%minute == 0 ) ) ) then
  else
    write(*,*) 'Error at ini'
    write(*,*) '  Wrong record interval (1 hour or 30 minute only)'
    stop
  endif

  call libmxe_para__register(paraa,file_namelist=file_namelist_ave)
  call libmxe_io__register(ioa,paraa)

  !---- Read station table. ----
  allocate(lons(nstn),lats(nstn),cstn(nstn))

  open( lun, file=file_table, form='formatted', action='read')
    do n = 1, nstn
      read(lun,'(f7.3,1x,f6.3,1x,a)') lons(n), lats(n),cstn(n)
    enddo
  close(lun)

  !---- Seek record in Japan_coast data. ----
  if ( io%calrec(nstr)%minute == 30 ) nstr = nstr + 1
  cal = type_calendar( io%calrec(nstr)%year, 1, 1, 0, 0, 0 )
  i = libmxe_calendar__diffsec( cal, io%calrec(nstr) )
  mstr = i / 3600 + 1  !- second => hour 

  i = libmxe_calendar__diffsec( cal, io%calrec(nend) )
  mend = i / 3600 + 1

  !---- file index to be read ----
  mindx = mend - mstr + 1
  allocate(indx(mindx))

  indx_intv = 1
  if ( io%calint%minute == 30 ) indx_intv = 2
  do m = 1, mindx
    indx(m) = nstr + indx_intv*( m - 1 )
  enddo

  allocate(rms(nstn),rmse(nstn),cap(nstn))
  allocate(ht(mindx),hta(mindx))
  allocate(r(im,jm),ra(im,jm))

  nrec = 1

end subroutine ini


subroutine calc
  use libmxe_io, only: libmxe_io__open
  use map, only: map__seek_nearest_sea
  use jcoast, only: jcoast__read, jcoast__mean_25h
  implicit none

  integer :: i,j,k,m,n
  real(8) :: lon, lat, distance
  character(clen) :: filej

  !-- Read japan_coast data. --
  lon = lons(nrec)
  lat = lats(nrec)
  filej = dir_jcoast//trim(cstn(nrec))//cyear
  call jcoast__read(trim(filej),h)

  !-- RMS --
  call jcoast__mean_25h( h, ha )
  hta(1:mindx) = dble( h(mstr:mend) - ha(mstr:mend) )
  rms(nrec) = sqrt( sum( hta(1:mindx)**2 ) / dble(mindx) )

  !-- Seek grid. --
  call map__seek_nearest_sea(para,grid,topo,lon,lat,'T',i,j,distance)

  !-- RMSE --
  do m = 1, mindx
    n = indx(m)
    call libmxe_io__open(io,trim(file_base),n,reclen,lun,action='read')
      read(lun,rec=1) r
    close(lun)
    call libmxe_io__open(ioa,trim(file_base_ave),n,reclen,lun,action='read')
      read(lun,rec=1) ra
    close(lun)
    ht(m) = dble( r(i,j) - ra(i,j) )
  enddo

  rmse(nrec) = sqrt( sum( ( ht(1:mindx) - hta(1:mindx) )**2 ) / dble(mindx) )

  !-- Capture ratio --
  cap(nrec) = 1.d0 - rmse(nrec)**2 / rms(nrec)**2

  write(*,'(i3,x,f8.3,x,a)') nrec, cap(nrec), trim(cstn(nrec))

end subroutine calc


subroutine write_result
  use libmxe_io, only: libmxe_io__open
  implicit none

  integer :: n

  open( lun, file=fileo, form='formatted', action='write')
    write(lun,'(i5)') nstn
    do n = 1, nstn
      write(lun,'(5(f8.3,x),a)') lons(n), lats(n) &
                             & , rms(n), rmse(n), cap(n), cstn(n)
    enddo
  close(lun)

end subroutine write_result


real function get_result(i)
  implicit none

  integer,intent(in) :: i

  get_result = real( cap(i) )

end function get_result


end module cap_jcoast
