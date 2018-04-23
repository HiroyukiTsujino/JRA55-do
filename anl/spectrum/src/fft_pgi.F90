! -*-F90-*-
!- Wrapper of PGIFortran FFT routines
module fft_pgi
  implicit none
  private


  public :: fft__power
  public :: fft__lowpass


contains 


subroutine fft__power( nm, timeseries, power, real_part, imagin_part )
  implicit none

  integer,intent(in)  :: nm
  real(8),intent(in)  :: timeseries(nm)
  real(8),intent(out) :: power(0:nm/2)
                                    !- power(0) : constant
                                    !- power(1) : frequency 1/nm
                                    !-            (1 per cycle)
                                    !- power(2) : frequency 2/nm ...
  real(8),intent(out),optional :: real_part(0:nm/2)
  real(8),intent(out),optional :: imagin_part(0:nm/2)

  real(8) :: comm(3*nm+100),x(0:nm-1),dr(0:nm/2),di(0:nm/2)
  integer :: i, info, nm2

  nm2 = nm/2

  x(:) = timeseries(:)
  call dzfft(2,nm,x,comm,info)

  dr(:) = 0.d0
  di(:) = 0.d0
  do i = 0, nm2
    dr(i) = x(i)
  enddo
  do i = 1, nm2 - 1
    di(i) = x(nm-i)
  enddo

  power(0) = dr(0)**2
  do i = 1, nm2 - 1
    power(i) = 2.d0*( dr(i)**2 + di(i)**2 )  !- double:plus and minus
  enddo
  power(nm2) = dr(nm2)**2

  power(:) = power(:) / dble(nm)  !- mean 

  if ( present(real_part) ) real_part = dr
  if ( present(imagin_part) ) imagin_part = di

end subroutine fft__power


subroutine fft__lowpass( nm, timeseries, nfreq_max, lowpass )
  implicit none

  integer,intent(in)  :: nm
  real(8),intent(in)  :: timeseries(nm)
  integer,intent(in)  :: nfreq_max  !- max frequency [ 0 - nm/2-1 ]
                                    !-   0 : constant
                                    !-   1 : 1 cycle   ...
  real(8),intent(out) :: lowpass(nm)

  real(8) :: comm(3*nm+100),x(0:nm-1)
  integer :: i, info

  x(:) = timeseries(:)
  call dzfft(2,nm,x,comm,info)

  do i = nm/2+1, nm-1
    x(i) = -x(i)
  enddo

  do i = nfreq_max + 1, nm/2
    x(i) = 0.0d0                     !- real part
    if ( i < nm/2 ) x(nm-i) = 0.0d0  !- imaginary part
  enddo

  call zdfft(2,nm,x,comm,info)
  lowpass(:) = x(:)

end subroutine fft__lowpass


end module fft_pgi
