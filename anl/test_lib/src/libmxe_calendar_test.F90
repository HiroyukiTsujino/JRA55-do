! -*-F90-*-
module libmxe_calendar_test
  use libmxe_calendar
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='libmxe_calendar'
  public :: test_addmonth_plus_0
  public :: test_addmonth_plus_1
  public :: test_addmonth_plus_14
  public :: test_addmonth_minus_1
  public :: test_l_leap_year
  public :: test_diffsec
  public :: test_addcal

contains


subroutine test_addmonth_plus_0
  implicit none

  type(type_calendar) :: cal, cal2

  cal = type_calendar( 2000, 1, 1, 12, 30, 45 )
  cal2 = libmxe_calendar__addmonth( cal, 0 )

  call assert_equals( 1, cal2%month, 'addmonth_plus_0' )

end subroutine test_addmonth_plus_0


subroutine test_addmonth_plus_1
  implicit none

  type(type_calendar) :: cal, cal2

  cal = type_calendar( 2000, 1, 1, 12, 30, 45 )
  cal2 = libmxe_calendar__addmonth( cal, 1 )

  call assert_equals( 2, cal2%month, 'addmonth_plus_1' )

end subroutine test_addmonth_plus_1


subroutine test_addmonth_plus_14
  implicit none

  type(type_calendar) :: cal, cal2

  cal = type_calendar( 2000, 1, 1, 12, 30, 45 )
  cal2 = libmxe_calendar__addmonth( cal, 14 )

  call assert_equals( 3, cal2%month, 'addmonth_plus_14(month)' )
  call assert_equals( 2001, cal2%year, 'addmonth_plus_14(year)' )

end subroutine test_addmonth_plus_14


subroutine test_addmonth_minus_1
  implicit none

  type(type_calendar) :: cal, cal2

  cal = type_calendar( 2000, 1, 1, 12, 30, 45 )
  cal2 = libmxe_calendar__addmonth( cal, -1 )

  call assert_equals( 12, cal2%month, 'addmonth_minus_1(month)' )
  call assert_equals( 1999, cal2%year, 'addmonth_minus_1(year)' )

end subroutine test_addmonth_minus_1


subroutine test_l_leap_year
  implicit none

  logical :: l_leap_year

  l_leap_year = libmxe_calendar__l_leap_year( 1999 )
  call assert_equals( .false., l_leap_year, 'l_leap_year(1999)' )

  l_leap_year = libmxe_calendar__l_leap_year( 2000 )
  call assert_equals( .true., l_leap_year, 'l_leap_year(2000)' )

  l_leap_year = libmxe_calendar__l_leap_year( 2004 )
  call assert_equals( .true., l_leap_year, 'l_leap_year(2004)' )

  l_leap_year = libmxe_calendar__l_leap_year( 2100 )
  call assert_equals( .false., l_leap_year, 'l_leap_year(2100)' )

end subroutine test_l_leap_year


subroutine test_diffsec
  implicit none

  type(type_calendar) :: cal1, cal2
  integer             :: isec

  cal1 = type_calendar( 2000, 1, 1, 12, 0, 0 )
  cal2 = type_calendar( 2004, 3, 1, 12, 30, 45 )

  isec = libmxe_calendar__diffsec( cal1, cal2 )
  call assert_equals( 131243445, isec, 'diffsec' )

  isec = libmxe_calendar__diffsec( cal1, cal2, l_leap=.true. )
  call assert_equals( 131416245, isec, 'diffsec' )

end subroutine test_diffsec


subroutine test_addcal
  implicit none

  type(type_calendar) :: cal, caladd, cal2
  integer             :: isec

  cal = type_calendar( 2000, 2, 28, 0, 0, 0 )
  caladd = type_calendar( 0, 0, 3, 0, 0, 0 )

  cal2 = libmxe_calendar__addcal( cal, caladd )
  call assert_equals( 2000, cal2%year, 'addcal(year)' )
  call assert_equals( 3, cal2%month, 'addcal(month)' )
  call assert_equals( 3, cal2%day, 'addcal(day)' )

  cal2 = libmxe_calendar__addcal( cal, caladd, l_leap=.true. )
  call assert_equals( 2000, cal2%year, 'addcal(year)' )
  call assert_equals( 3, cal2%month, 'addcal(month)' )
  call assert_equals( 2, cal2%day, 'addcal(day)' )

end subroutine test_addcal


end module libmxe_calendar_test
