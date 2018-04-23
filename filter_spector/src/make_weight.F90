! -*-F90-*-
!
!==============================================================
program make_filter_weight

  use file_open_close_manager

  implicit none


  real(8), parameter :: eps = 1.0d-6

  real(8) :: hl1, hl2, hl3, hl4, hl5, hl6

  character(255) :: infile, infile_base
  character(255) :: file_namelist
  character(255) :: file_filter_weight

  integer(4) :: mtot1, mtot2, mtot3, mtot4

  integer(4) :: lrec

  integer(4) :: mtnam

  integer(4) :: num_buff
  integer(4) :: num_filter, iside, icenter
  
  integer(4) :: m, n, iter

  real(8) :: left, right, center
  real(8),allocatable :: fd_org(:), fd_new(:), fd_tmp(:)

  !-----------------------------------------------------------------

  namelist /nml_filter_weight/ num_buff, num_filter, &
       &  iside, icenter, file_filter_weight

  !-----------------------------------------------------------------

  file_namelist='namelist.filter_weight'
  call open_file_plain(mtnam,file_namelist)
  read(mtnam,nml=nml_filter_weight) 
  call close_file(mtnam)

  !-----------------------------------------------------------------


  allocate(fd_org(-num_buff : num_buff))
  allocate(fd_new(-num_buff : num_buff))
  allocate(fd_tmp(-num_buff : num_buff))

  fd_tmp(:) = 0.0d0

  fd_org(:) = 0.0d0
  fd_org(0) = 1.0d0

  left   = real(iside,8) / (real(iside+iside+icenter,8))
  right  = real(iside,8) / (real(iside+iside+icenter,8))
  center = real(icenter,8) / (real(iside+iside+icenter,8))

  write(6,*) 'Sum of weights = ', left + right + center


  lrec = 4 * (num_buff * 2 + 1)

  call open_file_direct(mtot1,file_filter_weight,lrec)
  write(mtot1,rec=1) real(fd_org(-num_buff:num_buff),4)

  fd_new(:) = fd_org(:)
  do iter = 1, num_filter
    fd_tmp(:) = fd_new(:)
    fd_new(:) = 0.0d0
    do n = -num_buff+1, num_buff-1
      fd_new(n) = left * fd_tmp(n-1) + center * fd_tmp(n) + right * fd_tmp(n+1)
    end do
    write(mtot1,rec=iter+1) real(fd_new(-num_buff:num_buff),4)
  end do

  call close_file(mtot1)

  if ((fd_new(-num_buff) /= 0.0d0) .or. (fd_new(num_buff) /= 0.0d0)) then
    write(6,*) ' You should widen the array size, current size = ', num_buff
  end if

  deallocate(fd_tmp,fd_org,fd_new)

end program make_filter_weight
