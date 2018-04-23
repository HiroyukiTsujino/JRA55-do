! -*-F90-*-
program power

  implicit none

  integer(4) :: num_grid_in
  integer(4) :: num_data_st
  integer(4) :: num_data_ed
  real(4),allocatable :: data_in(:)
  real(4) :: undef_in

  integer(4) :: i, j, info, ntime

  character(256) :: flnin1_base, flnin2_base
  character(256) :: flnin1, flnin2
  character(256) :: flout1_base, flout2_base
  character(256) :: flout1, flout2

  integer(4), parameter :: mtin1 = 77, mtin2 = 78
  integer(4), parameter :: mtot1 = 87, mtot2 = 88

  integer(4) :: num_used, num_rec, num_out
  real(8),allocatable :: raw(:), pow(:), pow_chk(:)
  real(8),allocatable :: comm(:)
  real(8),allocatable :: energy(:)
  real(8),allocatable :: wave_length(:)
  real(8),allocatable :: wave_num(:)
  real(8),allocatable :: wave_num_log(:)
  real(8) :: delta_x

  logical :: l_check = .true.

  !--------------------------------------------------------------------

  namelist /nml_spector/ &
       & flnin1_base,  num_rec,   &
       & num_grid_in, num_data_st, num_data_ed, delta_x, &
       & flout1_base

  delta_x=1.0d0
  open (11,file='namelist.spector')
  read (11,nml=nml_spector)
  close(11)

  !-----------------------------------------------------------

  allocate(data_in(1:num_grid_in))
  write(flnin1,'(1A)') trim(flnin1_base)
  write(*,*) ' DATA READ from..... ', trim(flnin1)
  open(mtin1,file=flnin1,form='unformatted',access='direct',recl=4*num_grid_in)
  read(mtin1,rec=num_rec) data_in
  close(mtin1)

  !--------------------------------------------------------------------

  num_used = num_data_ed - num_data_st + 1

  if (mod(num_used,2) /= 0) then
    write(6,*) ' Please set (num_data_ed - num_data_st + 1) to be even number '
    write(6,*) '   terminating......... '
    stop
  end if

  allocate(raw(1:num_used), pow(1:num_used))
  allocate(comm(3*num_used+100))
  allocate(energy(1:num_used))
  allocate(wave_length(1:num_used))
  allocate(wave_num(1:num_used))
  allocate(wave_num_log(1:num_used))

  raw(1:num_used) = real(data_in(num_data_st:num_data_ed),8)
  pow(1:num_used) = real(data_in(num_data_st:num_data_ed),8)

  call dzfft(0, num_used, pow, comm, info)
  call dzfft(1, num_used, pow, comm, info)

  energy(:) = 0.0d0

  i = 1
  write(*,'(i8,8x,f12.5)') i, pow(i)
  energy(i) = real(num_used,8) * pow(i)**2
  wave_length(i) = real(num_used,8) * 2.0d0 * delta_x
  wave_num(i) = 0.0d0
  wave_num_log(i) = 0.0d0

  do i = 2, (num_used-1)/2 + 1
    write(*,'(i8,i8,f12.5,f12.5)') i, num_used-i+2, pow(i), pow(num_used-i+2)
    energy(i) = real(num_used,8) * (pow(i)**2 + pow(num_used-i+2)**2)
    wave_length(i) = real(num_used,8) / real(i-1,8) * delta_x
    wave_num(i) = real(i-1,8) / real(num_used,8) / delta_x
    wave_num_log(i) = log10(wave_num(i))
  end do

  i = num_used/2 + 1
  write(*,'(i8,8x,f12.5)') i, pow(i)
  energy(i) = real(num_used,8) * pow(i)**2
  wave_length(i) = real(num_used,8)/real(i-1,8) * delta_x
  wave_num(i) = real(i-1,8) / real(num_used,8) / delta_x
  wave_num_log(i) = log10(wave_num(i))

  write(flout1,'(1a)') trim(flout1_base)


  open (10,file='x-axis.txt')
  write(10,'(5f15.8)') -wave_length(1:num_used/2+1)
  write(10,'(5f15.8)') wave_num(1:num_used/2+1)
  write(10,'(5f15.8)') wave_num_log(1:num_used/2+1)
  close(10)

  num_out = num_used/2+1
  open (mtot1,file=flout1,form='unformatted',access='direct',recl=4*num_out)
  write(mtot1,rec=1) real(energy(1:num_out),4)
  close(mtot1)

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
  end if

end program power
