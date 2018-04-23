! -*-F90-*-
!- Control file input/output.
module libmxe_io
  use libmxe_para, only: clen, type_libmxe_para
  use libmxe_calendar, only: type_calendar
  implicit none
  private


  type,public :: type_libmxe_io
    logical :: ldef=.false. !- .true. : this object is registered
    integer :: nm           !- number of record
    integer :: suf_digit    !- digit of suffix (like year:4)
    character(6) :: timemode  !- time unit of records:
                            !- year,month,day,hour,minute,second
    type(type_calendar) :: calint !- time interval of record
    type(type_calendar),pointer :: calrec(:)
                            !- calrec(1:nm): calendar of record
    character(len=clen) :: namelist !- namelist file
    logical :: l_leap_year = .false.
    integer :: dt_sec
  end type type_libmxe_io


  !-- subroutine --
  public :: libmxe_io__register
    !- Register the IO object.

  public :: libmxe_io__set_calendar

  public :: libmxe_io__open
    !- Open a file to write or read data by direct access.


  !-- function --
  public :: libmxe_io__suffix
    !- Return a file name with a suffix indicating time.

  integer,parameter,private :: lun=89


contains 
!-----------------------------------------------------------------


subroutine libmxe_io__register( io, para )
  use libmxe_calendar, only: libmxe_calendar__intarr2cal
  implicit none

  type(type_libmxe_io),intent(out)  :: io
  type(type_libmxe_para),intent(in) :: para

  integer             :: rec_first_date(6)   !- Y/M/D/H/M/S
  integer             :: rec_last_date(6)
  integer             :: rec_interval_date(6)
  logical             :: l_leap_year

  integer             :: ios
  type(type_calendar) :: first_cal, last_cal, interval_cal

  namelist /nml_record_date/ rec_first_date, rec_last_date, &
                           & rec_interval_date, l_leap_year


  if ( .not. para%ldef )  then
    write(*,*) 'Error at libmxe_io__register'
    write(*,*) '  para is not registered.'
    stop
  endif

  !-- read namelist --
  rec_first_date(:)    = 0
  rec_last_date(:)     = 0
  rec_interval_date(:) = 0
  l_leap_year          = .false.

  open( lun, file=trim(para%namelist), status='old' )
  read( lun, nml=nml_record_date )
  close(lun)

  io%l_leap_year = l_leap_year
  io%namelist    = para%namelist

  first_cal    = libmxe_calendar__intarr2cal( rec_first_date    )
  last_cal     = libmxe_calendar__intarr2cal( rec_last_date     )
  interval_cal = libmxe_calendar__intarr2cal( rec_interval_date )

  call libmxe_io__set_calendar( io, first_cal, last_cal, &
                              & interval_cal )

  io%ldef        = .true.

end subroutine libmxe_io__register
!-----------------------------------------------------------------


subroutine libmxe_io__set_calendar( io, first_cal, last_cal, interval_cal )
  use libmxe_calendar, only: type_calendar, &
                          &  libmxe_calendar__diffsec, &
                          &  libmxe_calendar__addcal
  implicit none

  type(type_libmxe_io),intent(inout) :: io
  type(type_calendar),intent(in)   :: first_cal, last_cal, interval_cal

  integer                          :: n

  io%calint = interval_cal

  !-- time mode and digit of records suffix --
  if ( io%calint%second /= 0 ) then
    io%timemode = 'second'
    io%suf_digit = 14
  else if ( io%calint%minute /= 0 ) then
    io%timemode = 'minute'
    io%suf_digit = 12
  else if ( io%calint%hour /= 0 ) then
    io%timemode = 'hour'
    io%suf_digit = 10
  else if ( io%calint%day /= 0 ) then
    io%timemode = 'day'
    io%suf_digit = 8
  else if ( io%calint%month /= 0 ) then
    io%timemode = 'month'
    io%suf_digit = 6
  else if ( io%calint%year /= 0 ) then
    io%timemode = 'year'
    io%suf_digit = 4
  else
    write(*,*) 'Error at libmxe_io__set_calendar'
    write(*,*) ' Wrong interval_cal:',interval_cal
    stop
  endif

  !-- nm (number of data records) --
  io%dt_sec = 0
  if ( ( io%timemode == 'day' ) &
        .or. ( io%timemode == 'hour' ) &
        .or. ( io%timemode == 'minute' ) &
        .or. ( io%timemode == 'second' ) ) then
    io%dt_sec = io%calint%day * 24 * 3600 + io%calint%hour * 3600 &
        & + io%calint%minute * 60 + io%calint%second 
    io%nm = libmxe_calendar__diffsec( first_cal, last_cal, &
          &  l_leap=io%l_leap_year ) / io%dt_sec + 1
  endif
  if ( io%timemode == 'month' ) then
    io%nm = ( last_cal%year - first_cal%year )*12 &
           & + last_cal%month - first_cal%month + 1
  endif
  if ( io%timemode =='year' ) then
    io%nm = last_cal%year - first_cal%year + 1
  endif
  if ( io%nm <= 0 ) then
    write(*,*) 'Error at libmxe_io__set_calendar: nm=',io%nm
    stop
  endif

  !-- date of each record --
  allocate( io%calrec(io%nm) )
  io%calrec(1) = first_cal
  do n = 2, io%nm
    io%calrec(n) = libmxe_calendar__addcal( io%calrec(n-1), &
                 &   io%calint, l_leap=io%l_leap_year )
  enddo

end subroutine libmxe_io__set_calendar
!-----------------------------------------------------------------


function libmxe_io__suffix( io, file_base, n )
  implicit none

  character(clen) :: libmxe_io__suffix

  type(type_libmxe_io),intent(in) :: io
  character(*),intent(in) :: file_base
                 !- base name of input/output file
  integer,intent(in) :: n  !- record number

  character(15) :: csuffix
  character(4) :: cyear
  character(2) :: cmonth, cday, chour, cminute, csecond


  !-- check --
  if ( .not. io%ldef )  then
    write(*,*) 'Error at libmxe_io__suffix'
    write(*,*) '  io is not registered.'
    stop
  endif
  if ( n > io%nm )  then
    write(*,*) 'Error at libmxe_io__suffix'
    write(*,*) '  n=',n,' > io%nm=',io%nm
    stop
  endif


  !-- date --    
  write(cyear,'(i4.4)') io%calrec(n)%year
  write(cmonth,'(i2.2)') io%calrec(n)%month
  write(cday,'(i2.2)') io%calrec(n)%day
  write(chour,'(i2.2)') io%calrec(n)%hour
  write(cminute,'(i2.2)') io%calrec(n)%minute
  write(csecond,'(i2.2)') io%calrec(n)%second

  csuffix = cyear//cmonth//cday//chour//cminute//csecond

  libmxe_io__suffix = trim(file_base)//'.'//trim(csuffix(1:io%suf_digit))


end function libmxe_io__suffix
!-----------------------------------------------------------------


subroutine libmxe_io__open( io, file_base, n, reclen, lun, action )
  implicit none

  type(type_libmxe_io),intent(in) :: io
  character(*),intent(in) :: file_base  !- file name (base)
  integer,intent(in) :: lun    !- logical unit number to open
  integer,intent(in) :: n      !- record number
  integer,intent(in) :: reclen !- record length [byte]
  character(*),intent(in),optional :: action

  character(clen) :: cfile


  !-- check --
  if ( .not. io%ldef )  then
    write(*,*) 'Error at libmxe_io__open'
    write(*,*) '  io is not registered.'
    stop
  endif
  if ( n > io%nm )  then
    write(*,*) 'Error at libmxe_io__open'
    write(*,*) '  n=',n,' > io%nm=',io%nm
    stop
  endif
  if ( len(file_base) == 0 )  then
    write(*,*) 'Error at libmxe_io__open: empty file_base'
    stop
  endif


  !-- Add suffix --    
  cfile = libmxe_io__suffix(io,file_base,n)

  !-- open --
  if (present(action)) then
    open(lun, file=cfile, recl=reclen &
        & , access='direct', form='unformatted', action=action )
  else
    open(lun, file=cfile, recl=reclen &
        & , access='direct', form='unformatted' )
  endif


end subroutine libmxe_io__open


end module libmxe_io
