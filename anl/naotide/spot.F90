! -*-F90-*-
!- Calc tidal height variation at 1 spot using naotide.f.
module spot
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_io, only: type_libmxe_io
  implicit none
  private


  !-- arguments --
  real(8) :: lon, lat          !- location
  character(clen) :: fileo  !- output file name


  public :: ini
  public :: calc
  public :: get_result
  public :: write_result


  real,parameter :: rundef_nao = 9999.99e0
  integer,parameter :: lun=10
  integer,save :: nm
  real,allocatable,save :: h(:)

  type(type_libmxe_para),save :: para
  type(type_libmxe_io),save :: io


contains


subroutine ini
  use libmxe_para, only: libmxe_para__register
  use libmxe_io, only: libmxe_io__register
  implicit none

  integer :: i

  namelist /spot_lst/ lon, lat, fileo

  !---- arguments ----
  read(5,nml=spot_lst,iostat=i)
  if (i/=0) then
    write(*,*) 'Error: no namelist'
    stop
  endif

  !---- experiment settings ----
  call libmxe_para__register(para)
  call libmxe_io__register(io,para)
  nm = io%nm

  allocate(h(nm))
  h(:) = rundef_nao

end subroutine ini


subroutine calc
  implicit none

  integer,parameter :: itmode=2, lpmode=2
  real(8) :: time, height, hsp, hlp, x, y
  integer :: iy,imon,id,ih,imin,isec, n
  logical :: Ldata

  write(*,*) 'nrec, date, Julian date'

  do n = 1, nm

    !-- get modified Julian date (time) [day] --
    iy=io%calrec(n)%year
    imon=io%calrec(n)%month
    id=io%calrec(n)%day
    ih=io%calrec(n)%hour
    imin=io%calrec(n)%minute
    isec=io%calrec(n)%second
    call mjdymd(time, iy, imon, id, ih, imin, isec, 1)

    if ( mod( n,100) == 0 ) then
      write(*,'(i4,i5,i2,i2,i3,i2.2,f12.4)') &
           &  n,iy, imon, id, ih, imin, time
    endif

    !-- get tidal height at model grid (height) --
    call naotide(lon,lat,time,itmode,lpmode &
             & ,height,hsp,hlp,Ldata)
    h(n) = real(height)

  enddo

end subroutine calc


subroutine write_result
  use libmxe_calendar, only: libmxe_calendar__cdate
  implicit none

  character(15) :: cdate

  open(lun, file=trim(fileo)//'.gd', form='unformatted' &
       & , access='direct', recl=nm*4, action='write' )
    write(lun,rec=1) h(1:nm)
  close(lun)

  !-- grads control file --
  open(lun,file=trim(fileo)//'.ctl',form='formatted')
    write(lun,'(a)') 'DSET ^'//trim(fileo)//'.gd'
    write(lun,'(a)') 'OPTIONS big_endian 365_day_calendar'
    write(lun,'(a)') 'TITLE naotide'
    write(lun,'(a)') 'UNDEF 9999.99E+0'
    write(lun,'(a,f11.5,a)') 'XDEF 1 LINEAR ',lon,' 1.0'
    write(lun,'(a,f11.5,a)') 'YDEF 1 LINEAR ',lat,' 1.0'
    write(lun,'(a)') 'ZDEF 1 LINEAR 2.0 1.0'
    cdate = libmxe_calendar__cdate(io%calrec(1))
    write(lun,'(a,i5,a,a)') 'TDEF ',nm,' LINEAR ',trim(cdate)
    write(lun,'(a)') 'VARS 1'
    write(lun,'(a)') ' hm 1 99 tidal height [cm]'
    write(lun,'(a)') 'ENDVARS'
  close(lun)


end subroutine write_result


real function get_result(i)
  implicit none

  integer,intent(in) :: i

  get_result = h(i)

end function get_result


end module spot
