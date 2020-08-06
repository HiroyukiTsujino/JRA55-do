!-*-F90-*-
program linear_regression_of_two_data

  use libmxe_para, only: libmxe_para__register, clen, rho, omega &
                     & , pi, radius, radian, radian_r, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io
  use libmxe_grads, only: libmxe_grads__make, type_grads

  implicit none

  integer(4) :: kmx

  integer(4) :: mtin1 = 31, mtin2 = 32
  integer(4) :: iflnm = 11, iflout = 20

  real(8) :: mean1, mean2
  real(8) :: stdv1, stdv2, corr

  real(4), allocatable :: ind1(:), ind2(:)
  real(8), allocatable :: x(:), y(:)

  real(4) :: a, b

  integer(4) :: nsum

  integer(4) :: i, j, k, m, n, nd
  integer(4) :: ii, nn, kk
  integer(4) :: n1, n2, inum
  integer(4) :: iundef=-99999
  integer(4) :: ireco

  character(len=256) :: dirname
  character(len=256) :: flnin1, flnin2
  character(len=256) :: fnin
  character(len=256) :: fnout

  real(4) :: rmiss1, rmiss2

  integer(4) :: irec1, irec2

  logical :: lconv1, lconv2

  real(4), allocatable :: raw1(:), raw2(:)
  integer(4) :: n_available
  real(8) :: mean_m1, mean_p1
  real(8) :: stdv_m1, stdv_p1
  real(8) :: corr_lag1a, corr_lag1b 

  real(8) :: mean_m2, mean_p2
  real(8) :: stdv_m2, stdv_p2
  real(8) :: corr_lag2a, corr_lag2b

  real(8) :: eff_a, eff_b

  ! MXE structure variables

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo
  type(type_libmxe_io) :: io

  real(8) :: w_lon, e_lon
  real(8) :: s_lat, n_lat
  real(4) :: uvc_min

  !-----------------------------------------------------------------------

  namelist /nml_reg/ flnin1, flnin2, rmiss1, rmiss2, &
       & w_lon, e_lon, s_lat, n_lat, &
       & lconv1, lconv2, uvc_min

  open (iflnm,file='namelist_reg.dat')
  read (iflnm,nml=nml_reg)

  close(iflnm)

  !-----------------------------------------------------------------------

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)

  kmx = para%imut * para%jmut

  write(6,*) ' max number of data = ', kmx

  allocate(ind1(kmx),ind2(kmx))
  allocate(raw1(kmx),raw2(kmx))
  allocate(x(kmx),y(kmx))

  raw1(1:kmx) = 0.0
  raw2(1:kmx) = 0.0

  ! ===============================================
  !  Loop Start
  ! ===============================================
  ! -----------------------------------------------
  !   Initial Settings
  ! -----------------------------------------------

  nsum = 0

  mean1 = 0.0d0
  mean2 = 0.0d0

  ! -----------------------------------------------
  !   Read CORE Historical Data
  ! -----------------------------------------------
  
  if (lconv1) then
    open(mtin1,file=flnin1,form='unformatted',access='direct',convert='big_endian',recl=4*kmx)
  else
    open(mtin1,file=flnin1,form='unformatted',access='direct',convert='little_endian',recl=4*kmx)
  endif

  if (lconv2) then
    open(mtin2,file=flnin2,form='unformatted',access='direct',convert='big_endian',recl=4*kmx)
  else
    open(mtin2,file=flnin2,form='unformatted',access='direct',convert='little_endian',recl=4*kmx)
  end if

  read (mtin1,rec=1) raw1
  read (mtin2,rec=1) raw2

  close(mtin2)
  close(mtin1)

  write(6,*) ' analyzing ... ', s_lat, n_lat, w_lon, e_lon

  do j = 1, para%jmut
    do i = 1, para%imut

      if (grid%glatt(i,j) > s_lat .and. grid%glatt(i,j) < n_lat &
           & .and. grid%glont(i,j) > w_lon .and. grid%glont(i,j) < e_lon) then

        k = para%imut * (j - 1) + i
          
!        if ((raw1(k) /= rmiss1) .and. (raw2(k) /= rmiss2) .and. (abs(raw1(k)) > 1.0e-6) .and. (abs(raw2(k)) > 1.0e-6)) then
        if ((raw1(k) /= rmiss1) .and. (raw2(k) /= rmiss2) .and. (abs(raw1(k)) > uvc_min)) then

          nsum = nsum + 1

          mean1 = mean1 + real(raw1(k),8)
          mean2 = mean2 + real(raw2(k),8)

        end if

      end if
    end do
  end do

  write(6,*) ' There are ', nsum , 'data / maximum ' , kmx

  ! -----------------------------------------------
  !   Calculate Mean
  ! -----------------------------------------------

  write(6,*) 'Calculate Mean'

  if (nsum > 0) then
    mean1 = mean1 / dble(nsum)
    mean2 = mean2 / dble(nsum)
    write(6,*) '   mean1 = ',mean1, ' mean2 = ', mean2
  end if

  stdv1 = 0.0d0
  stdv2 = 0.0d0
  corr  = 0.0d0
  inum = 0

  do j = 1, para%jmut
    do i = 1, para%imut
      if (grid%glatt(i,j) > s_lat .and. grid%glatt(i,j) < n_lat &
           & .and. grid%glont(i,j) > w_lon .and. grid%glont(i,j) < e_lon) then

        k = para%imut * (j - 1) + i

!        if ((raw1(k) /= rmiss1) .and. (raw2(k) /= rmiss2) .and. (abs(raw1(k)) > 1.0e-6) .and. (abs(raw2(k)) > 1.0e-6)) then
        if ((raw1(k) /= rmiss1) .and. (raw2(k) /= rmiss2) .and. (abs(raw1(k)) > uvc_min)) then

          inum = inum + 1

          ind1(inum) = raw1(k) - real(mean1,4)
          ind2(inum) = raw2(k) - real(mean2,4)

          stdv1 = stdv1 + real(ind1(inum),8)**2
          stdv2 = stdv2 + real(ind2(inum),8)**2
          corr = corr + real(ind1(inum),8) * real(ind2(inum),8)
          
        end if
      end if
    end do
  end do

  write(6,*) ' There are ', inum , 'data / maximum ' , kmx
      
  ! -----------------------------------------------
  !   Calculate Linear Trend
  ! -----------------------------------------------

  do j = 1, inum
    x(j) = real(ind1(j),8)
    y(j) = real(ind2(j),8)
  end do

  call lsr( x, y, inum, kmx, a, b )

  write(6,*) 'slope        ', a
  write(6,*) 'intersection ', b
  write(6,*) 'mean (1)     ', mean1
  write(6,*) 'mean (2)     ', mean2
  write(6,*) 'intersection + mean(1) ', b + mean1

  stdv1 = sqrt(stdv1 / dble(inum))
  stdv2 = sqrt(stdv2 / dble(inum))
  corr = corr / stdv1 / stdv2 / dble(inum)

  write(6,'(1a,E20.10)') 'Std of data1: ', stdv1
  write(6,'(1a,E20.10)') 'Std of data2: ', stdv2
  write(6,'(1a,F10.5)')  ' Correlation: ', corr

end program linear_regression_of_two_data
