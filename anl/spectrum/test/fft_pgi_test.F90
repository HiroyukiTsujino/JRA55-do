! -*-F90-*-
module fft_pgi_test
  use fft_pgi
  use libmxe_ut
  use fruit, only: assert_equals
  implicit none
  private


  character(*),parameter,public :: testname='fft_pgi'
  public :: ini_test
  public :: test_power
  public :: test_lowpass

  integer,parameter :: lun=20
  integer :: i

  real(8),parameter :: pi=3.14159265358979323846
  integer,parameter :: nm = 8
  real(8),save      :: timeseries(nm)

contains


subroutine ini_test
  implicit none

  do i = 1, nm
    timeseries(i) =  7.d0 &
         &  + 3.d0 * sin( 1.d0 *dble(i)/dble(nm)*2.d0*pi + 1.d0 ) &
         &  + 1.d0 * sin( 2.d0 *dble(i)/dble(nm)*2.d0*pi ) &
         &  + 5.d0 * sin( 3.d0 *dble(i)/dble(nm)*2.d0*pi )
  enddo

end subroutine ini_test


subroutine test_power
  implicit none

  integer,parameter :: nm2 = nm / 2
  real(8) :: power(0:nm2)
  real(8) :: power_answer(0:nm2)
  character(1) :: ctemp

  power_answer(0) = 49.00000378120287d0
  power_answer(1) = 4.500001404843554d0
  power_answer(2) = 0.5000004135294873d0
  power_answer(3) = 12.50000051246683d0
  power_answer(4) = 0.d0

  power(:) = 0.d0
  call fft__power( nm, timeseries(:), power(:) )

  do i = 0, nm2
    write(ctemp,'(i1)') i
    call assert_equals( power_answer(i), power(i), 1.d-10, 'power:'//ctemp )
  enddo

end subroutine test_power


subroutine test_lowpass
  implicit none

  integer,parameter :: nfreq_max = 1
  real(8) :: d(nm)
  real(8) :: danswer(nm)
  character(1) :: ctemp

  danswer(1) = 9.931184447502853d0
  danswer(2) = 8.620907156915196d0

  d(:) = 0.d0
  call fft__lowpass( nm, timeseries(:), nfreq_max, d(:) )

  do i = 1, 2
    write(ctemp,'(i1)') i
    call assert_equals( danswer(i), d(i), 1.d-10, 'lowpass:'//ctemp )
  enddo

end subroutine test_lowpass


end module fft_pgi_test
