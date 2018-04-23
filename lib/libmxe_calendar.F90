! -*-F90-*-
module libmxe_calendar
 implicit none
 private


  !-- structure --
  type,public :: type_calendar
    integer :: year, month, day, hour, minute, second 
      !- year >= 0
  end type type_calendar


  !-- function --
  public :: libmxe_calendar__addcal
    !- get a calendar date elapsed by CalAdd since Cal 
    !-  Usage: CalNew = libmxe_calendar__addcal(Cal,CalAdd) 

  public :: libmxe_calendar__addsec
    !- get a calendar date elapsed by sec [second] since Cal 
    !-  Usage: CalNew = libmxe_calendar__addsec(Cal,sec) 

  public :: libmxe_calendar__addmonth
    !- get a calendar date elapsed by m [month] since Cal 
    !-  Usage: CalNew = libmxe_calendar__addmonth(Cal,m) 

  public :: libmxe_calendar__diffsec
    !- get time interval [second]
    !-  Usage: second = libmxe_calendar__diffsec(cal1,cal2) 
  public :: libmxe_calendar__diffsec_long

  private :: libmxe_calendar__cal2sec
    !- get total second from 1jan0001 0:00
    !-  Usage: second = libmxe_calendar__cal2sec(cal) 
    !-  Notice!!: Function is integer(8).

  private :: libmxe_calendar__sec2cal
    !- convert second from 1jan0001 0:00 to calendar
    !-  Usage: cal = libmxe_calendar__sec2cal(sec) 
    !-  Notice!!: Argument SEC is integer(8).

  public :: libmxe_calendar__cdate
    !- get character(clen=15) like 10:30Z12JAN2000
    !-  Usage: cdate = libmxe_calendar__cdate(cal) 

  public :: libmxe_calendar__intarr2cal
    !- integer :: intarr(6)=/(year, month, day, hour, minute, second)/
    !- cal = libmxe_calendar__intarr2cal( intarr(6) )

  public :: libmxe_calendar__l_leap_year
    !- Usage: l_leap_year = libmxe_calendar__l_leap_year( year )
    !-  .true. : leap year


  !-- private variable --
  integer :: month(12) =(/31,28,31,30,31,30,31,31,30,31,30,31/) 

contains 
!---------------------------------------------------------------------


function libmxe_calendar__addcal( cal, caladd, l_leap )
  implicit none

  type(type_calendar)            :: libmxe_calendar__addcal
  type(type_calendar),intent(in) :: cal, caladd
  logical,optional,intent(in)    :: l_leap

  type(type_calendar) :: caltemp
  integer :: i
  logical :: lleap

  lleap = .false.
  if ( present(l_leap) ) lleap = l_leap

  !-- + day, hour, minute, second
  i = caladd%day * 24 * 3600 + caladd%hour * 3600 &
              & + caladd%minute * 60  + caladd%second
  caltemp = libmxe_calendar__addsec( cal, i, l_leap=lleap )

  !-- + month
  caltemp = libmxe_calendar__addmonth( caltemp, caladd%month )

  !-- + year
  caltemp%year = caltemp%year + caladd%year

  libmxe_calendar__addcal = caltemp
    

end function libmxe_calendar__addcal
!---------------------------------------------------------------------


function libmxe_calendar__addsec( cal, sec, l_leap )
  implicit none

  type(type_calendar)            :: libmxe_calendar__addsec
  type(type_calendar),intent(in) :: cal
  integer,intent(in)             :: sec
  logical,optional,intent(in)    :: l_leap

  integer(8) :: i
  logical :: lleap

  lleap = .false.
  if ( present(l_leap) ) lleap = l_leap

  i = libmxe_calendar__cal2sec( cal, lleap )
  i = i + sec
  libmxe_calendar__addsec = libmxe_calendar__sec2cal( i, lleap )

end function libmxe_calendar__addsec
!---------------------------------------------------------------------


function libmxe_calendar__addmonth(cal, m)
  implicit none

  type(type_calendar) :: libmxe_calendar__addmonth

  type(type_calendar),intent(in)  :: cal
  integer,intent(in) :: m
  type(type_calendar) :: calnew
  integer :: year_change, new_month

  calnew = cal
  new_month = cal%month + m

  year_change = floor( ( dble( new_month - 1 ) + 1.d-10 ) / 12.d0 )
  calnew%year = cal%year + year_change
  calnew%month = new_month - year_change * 12

  libmxe_calendar__addmonth = calnew

!- This function fails when
!-   FFLAGS = -fastsse, and
!-     year_change = ( new_month - 1 ) / 12

end function libmxe_calendar__addmonth
!---------------------------------------------------------------------


function libmxe_calendar__cal2sec( cal, l_leap )
  implicit none

  integer(8)                     :: libmxe_calendar__cal2sec
  type(type_calendar),intent(in) :: cal
  logical,            intent(in) :: l_leap

  integer :: n, nsum(0:11)
  integer(8) :: nday
  logical :: lleap

  nsum(0) = 0
  do n = 1, 11
    nsum(n) = nsum(n-1) + month(n)
  enddo

  if ( cal%year < 0 ) then
    write(*,*) 'Error at libmxe_calendar__cal2sec: negative value of year ' &
            & ,cal%year
    stop
  endif

  !-- day number (except for today) since 1JAN0001 --
  nday = ( cal%year -1 )* 365 + nsum(cal%month-1) + cal%day - 1


  !-- additional days of leap years --
  if (l_leap) then

    nday = nday + ( cal%year - 1 ) / 4  - ( cal%year - 1 ) / 100  &
          &  + ( cal%year - 1 ) / 400 
    !- Count leap year from 0001 to cal%year-1

    if ( ( mod( cal%year , 4 ) == 0 ) &
      & .and. ( ( mod( cal%year , 100 ) /= 0 ) &
      &           .or. ( mod( cal%year , 400 ) == 0 ) ) &
      & .and. ( cal%month >= 3 ) ) then
      nday = nday + 1
    endif
    !- Add 1 day in MAR - DEC of leap yaer

  endif


  libmxe_calendar__cal2sec = nday * 24 * 3600 + cal%hour * 3600 &
                           & + cal%minute * 60  + cal%second

end function libmxe_calendar__cal2sec
!---------------------------------------------------------------------


function libmxe_calendar__sec2cal( sec, l_leap )
  implicit none

  !- time[sec] start from 0:00 1JAN0001 to calendar[ymdhms]

  type(type_calendar)   :: libmxe_calendar__sec2cal
  integer(8),intent(in) :: sec
  logical,intent(in)    :: l_leap

  integer :: i, n, nsum(0:12), n400y, nday, nsec
  type(type_calendar) :: cal
  logical :: lleap
  integer :: nsum_yr(0:400)

  nsum(0) = 0
  do n = 1, 12
    nsum(n) = nsum(n-1) + month(n)
  enddo

  !---- year ----

  if (l_leap) then

    nday = sec / ( 24 * 3600 )
    n400y = nday / ( 400 * 365 + 97)   !- cycle of leap year: 400yr
    nday = mod( nday, ( 400 * 365 + 97 ) ) + 1

    nsum_yr(0) = 0
    do n = 1, 400
      i = 0
      if ( libmxe_calendar__l_leap_year( n ) ) i = 1
      nsum_yr(n) = nsum_yr(n-1) + 365 + i
    enddo 

    do n = 1, 400
      if ( nday <= nsum_yr(n) ) then
        cal%year = n
        nday = nday - nsum_yr( n-1 )  !- ordinal day of year
        exit
      endif
    enddo
    cal%year = cal%year + n400y * 400

    if ( libmxe_calendar__l_leap_year( cal%year ) ) then
      nsum(2:12) = nsum(2:12) + 1
    endif

  else

    nday = sec / ( 24 * 3600 )
    cal%year = nday / 365 + 1     !- start: B.C. 0001
    nday = mod( nday, 365 ) + 1   !- ordinal day of year
    
  endif


  !---- month, day ----
  do n = 1, 12
    if (  nday <= nsum(n) ) then
      cal%month = n
      exit
    endif
  enddo
  cal%day = nday - nsum(cal%month-1)


  !---- hour, minute, second ----
  nsec = mod( sec, 24*3600 )
  cal%hour = nsec / 3600
  cal%minute = mod( nsec, 3600) / 60
  cal%second = mod( nsec, 60 )


  !---- check ----
  if ( ( cal%month < 1 ).or.(cal%month > 12) ) then
    write(*,*) 'Error at libmxe_calendar__sec2cal, cal%month:',cal%month
    stop
  endif

  libmxe_calendar__sec2cal = cal


end function libmxe_calendar__sec2cal
!---------------------------------------------------------------------


function libmxe_calendar__diffsec( cal1, cal2, l_leap )
  implicit none

  integer :: libmxe_calendar__diffsec
  type(type_calendar),intent(in)  :: cal1, cal2 
  logical,optional,intent(in)   :: l_leap

  integer(8) :: i1, i2
  logical :: lleap

  lleap = .false.
  if ( present(l_leap) ) lleap = l_leap

  i1 = libmxe_calendar__cal2sec( cal1, lleap )
  i2 = libmxe_calendar__cal2sec( cal2, lleap )
  libmxe_calendar__diffsec = i2 - i1
    
end function libmxe_calendar__diffsec
!---------------------------------------------------------------------


function libmxe_calendar__diffsec_long( cal1, cal2, l_leap )
  implicit none

  integer(8) :: libmxe_calendar__diffsec_long
  type(type_calendar),intent(in)  :: cal1, cal2 
  logical,optional,intent(in)   :: l_leap

  integer(8) :: i1, i2
  logical :: lleap

  lleap = .false.
  if ( present(l_leap) ) lleap = l_leap

  i1 = libmxe_calendar__cal2sec( cal1, lleap )
  i2 = libmxe_calendar__cal2sec( cal2, lleap )
  libmxe_calendar__diffsec_long = i2 - i1
    
end function libmxe_calendar__diffsec_long
!---------------------------------------------------------------------


function libmxe_calendar__cdate(cal)
  implicit none

  character(15) :: libmxe_calendar__cdate
  type(type_calendar),intent(in)  :: cal

  integer,parameter :: clen=15
  character(3) :: cmon(12) =(/'JAN','FEB','MAR','APR','MAY' &
                    & ,'JUN','JUL','AUG','SEP','OCT','NOV','DEC'/) 
  character(15) :: ctemp


  write(ctemp,'(i2,a1,i2.2,a1,i2.2,a3,i4.4)') &
   &  cal%hour, ':' ,cal%minute, 'Z', cal%day &
   & ,cmon(cal%month),cal%year

  libmxe_calendar__cdate = ctemp
    
end function libmxe_calendar__cdate
!---------------------------------------------------------------------


function libmxe_calendar__intarr2cal( intarr )
  implicit none

  type(type_calendar) :: libmxe_calendar__intarr2cal
  integer,intent(in)  :: intarr(6)

  libmxe_calendar__intarr2cal%year   = intarr(1)
  libmxe_calendar__intarr2cal%month  = intarr(2)
  libmxe_calendar__intarr2cal%day    = intarr(3)
  libmxe_calendar__intarr2cal%hour   = intarr(4)
  libmxe_calendar__intarr2cal%minute = intarr(5)
  libmxe_calendar__intarr2cal%second = intarr(6)

end function libmxe_calendar__intarr2cal
!---------------------------------------------------------------------


function libmxe_calendar__l_leap_year( year )
  implicit none

  logical            :: libmxe_calendar__l_leap_year
  integer,intent(in) :: year

  logical :: l_leap

  l_leap = .false.
  if ( mod(year,4  )==0 ) l_leap = .true.
  if ( mod(year,100)==0 ) l_leap = .false.
  if ( mod(year,400)==0 ) l_leap = .true.

  libmxe_calendar__l_leap_year = l_leap
 
end function libmxe_calendar__l_leap_year


end module
