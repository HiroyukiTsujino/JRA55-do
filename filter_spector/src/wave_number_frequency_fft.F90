! -*-F90-*-
program wave_number_frequency_fft

  implicit none

  integer(4) :: num_grid_in
  integer(4) :: num_data_st
  integer(4) :: num_data_ed
  real(4),allocatable :: data_in(:)
  real(4) :: undef_in

  integer(4) :: i, j, m, n, info, ntime

  character(256) :: flnin1_dir
  character(256) :: flnin1_base, flnin2_base
  character(256) :: flnin1, flnin2
  character(256) :: flout1_base, flout2_base
  character(256) :: flout1, flout2

  integer(4), parameter :: mtin1 = 77, mtin2 = 78
  integer(4), parameter :: mtot1 = 87, mtot2 = 88

  integer(4) :: num_used, num_used_half

  !---------------------
  ! wave number

  real(8),allocatable :: raw(:), pow(:), pow_chk(:)
  real(8),allocatable :: comm(:)

  !---------------------
  ! frequency domain

  integer(4) :: ibyr, ibmn, ibdy, idint, num_time
  integer(4) :: nyear, month, nday

  complex(8),allocatable :: time_wavn(:,:)
  complex(8),allocatable :: time_wavn_raw(:,:)
  complex(8),allocatable :: time_wavn_work(:)
  complex(8),allocatable :: comm_cmplx(:)

  real(8),allocatable :: power_frq_wavn(:,:)
  real(8),allocatable :: phase_frq_wavn(:,:)

  real(8),allocatable :: wave_length(:)
  real(8),allocatable :: wave_period(:)

  !---------------------

  real(8) :: hl1, hl2

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  logical :: l_check = .true.

  real(8) :: dist_intv, length
  real(8) :: time_intv, period

  !--------------------------------------------------------------------

  namelist /ioinfwavfrq/  &
       & flnin1_dir,      &
       & flnin1_base,     &
       & num_grid_in, num_data_st, num_data_ed, &
       & dist_intv,       &
       & ibyr, ibmn, ibdy, idint, num_time, &
       & time_intv,       &
       & flout1_base,     &
       & l_check

  open (11,file='namelist.wavfrq')
  read (11,nml=ioinfwavfrq)
  close(11)

  !-----------------------------------------------------------

  allocate(data_in(1:num_grid_in))

  num_used = num_data_ed - num_data_st + 1

  if (mod(num_used,2) /= 0) then
    write(6,*) ' Please set (num_data_ed - num_data_st + 1) to be even number '
    write(6,*) '   terminating......... '
    stop
  end if

  num_used_half = num_used / 2 !!!!! assuming num_used to be even number !!!!!

  allocate(raw(1:num_used), pow(1:num_used))

  allocate(comm(3*num_used+100))

  !-----

  allocate(time_wavn(1:num_time,1:num_used))
  allocate(time_wavn_raw(1:num_time,1:num_used))

  !-----

  nyear = ibyr
  month = ibmn
  nday  = ibdy

  do n = 1, num_time

    if (nday > ndmon(month)) then
      nday = nday - ndmon(month)
      month = month + 1
    end if

    if (month > 12) then
      month = 1
      nyear = nyear + 1
    end if

    write(6,*) 'Year = ', nyear, ' Month = ', month, ' Day = ', nday

    write(flnin1,'(1a,1a,i4.4,1a,1a,i4.4,i2.2,i2.2)') &
         & trim(flnin1_dir),'/y',nyear,'/',trim(flnin1_base),nyear,month,nday
    write(*,*) ' DATA READ from..... ', trim(flnin1)

    open(mtin1,file=flnin1,form='unformatted',access='direct', &
         & action='read',recl=4*num_grid_in)

    read(mtin1,rec=1) data_in

    close(mtin1)

    nday = nday + idint

    raw(1:num_used) = real(data_in(num_data_st:num_data_ed),8)
    pow(1:num_used) = real(data_in(num_data_st:num_data_ed),8)

    call dzfft(0, num_used, pow, comm, info) ! preparation
    call dzfft(1, num_used, pow, comm, info) ! forward transfomation

!    write(6,*) 'fft (wavenumber) done'
!
!    i = 1
!    write(*,'(i8,8x,f12.5)') i, pow(i)
!
!    do i = 2, (num_used-1)/2 + 1
!      write(*,'(i8,i8,f12.5,f12.5)') i, num_used-i+2, pow(i), pow(num_used-i+2)
!    end do
!
!    i = num_used/2 + 1
!    write(*,'(i8,8x,f12.5)') i, pow(i)
!    
    if (l_check) then

      allocate(pow_chk(1:num_used))

      pow_chk(1:num_used) = pow(1:num_used)

      do i = num_used/2 + 2, num_used
        pow_chk(i) = - pow_chk(i)
      end do

      call zdfft(2, num_used, pow_chk, comm, info)

      do i = 1, num_used
        write(*,*) i, raw(i), pow_chk(i)
      end do

      deallocate(pow_chk)

    end if

    do m = 1, num_used_half - 1 ! m = 1 for n/2 - 1
      hl1 = pow(num_used_half-m+1)
      hl2 = pow(num_used_half+m+1)
      time_wavn_raw(n,m) = cmplx(hl1,-hl2,8)
      time_wavn_raw(n,num_used-m) = cmplx(hl1,hl2,8)
    end do

    time_wavn_raw(n,num_used_half) = cmplx(pow(1),0.0d0,8)
    time_wavn_raw(n,num_used) = cmplx(pow(num_used_half+1),0.0d0,8)

!    do m = 1, num_used
!      write(6,'(i4,f12.5,f12.5)') m, time_wavn_raw(n,m)
!    end do

  end do

  time_wavn(1:num_time,1:num_used) = time_wavn_raw(1:num_time,1:num_used)

  allocate (time_wavn_work(1:num_time))
  allocate (comm_cmplx(1:3*num_time+100))
  allocate (power_frq_wavn(1:num_time,1:num_used))
  allocate (phase_frq_wavn(1:num_time,1:num_used))

  allocate (wave_length(1:num_used))
  allocate (wave_period(1:num_time))

  do m = 1, num_used
    time_wavn_work(1:num_time) = time_wavn(1:num_time,m)
    call zfft1d( 0,num_time,time_wavn_work,comm_cmplx,info)
    call zfft1d(-1,num_time,time_wavn_work,comm_cmplx,info)
    do n = 1, num_time
      power_frq_wavn(n,m) = time_wavn_work(n) * conjg(time_wavn_work(n))
!      write(6,*) n,m,frq_wavn(n,m)
    end do
  end do

  !-----

  phase_frq_wavn(1:num_time,1:num_used) = 0.0d0

  do n = 2, num_time
    period = time_intv * num_time / real(n-1,8)
    do m = 1, num_used_half - 1 
      length = dist_intv * num_used / (num_used_half - m)
      phase_frq_wavn(n,m) = length / period
    end do
    do m = num_used_half + 1, num_used
      length = - dist_intv * num_used / (m - num_used_half)
      phase_frq_wavn(n,m) = length / period
    end do
  end do

  !-----

  wave_period(1) = 0.0d0
  do n = 2, num_time
    wave_period(n) = time_intv * num_time / real(n-1,8)
  end do

  do m = 1, num_used_half - 1 
    wave_length(m) = dist_intv * num_used / (num_used_half - m)
  end do
  wave_length(num_used_half) = 0.0d0
  do m = num_used_half + 1, num_used
    wave_length(m) = - dist_intv * num_used / (m - num_used_half)
  end do

  !------

  do n = 1, num_time
    write(11,'(i8,e20.10)') n, wave_period(n)/86400.d0
  end do

  do m = 1, num_used
    write(12,'(i8,e20.10)') m, wave_length(m)*1.0d-3
  end do

  !------

  write(flout1,'(1A)') trim(flout1_base)
  write(*,*) ' Spectorum Written to..... ', trim(flout1)

  open (mtot1,file=flout1,form='unformatted',access='direct',recl=4*num_time*num_used)
  write(mtot1,rec=1) real(power_frq_wavn(1:num_time,1:num_used),4)
  write(mtot1,rec=2) real(phase_frq_wavn(1:num_time,1:num_used),4)
  close(mtot1)

end program wave_number_frequency_fft
