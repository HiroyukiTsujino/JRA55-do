!p2a_g.F90
!====================================================
!
!  Pentad to Annual Generalized version
!
!====================================================
program p2a_g
  !
  implicit none
  !
  integer(4) :: x_num, y_num, z_num, var_num
  !
  real(4), save  :: undef  =  0.0e0
  integer(4)     :: iyear
  !
  logical, save         :: l_leap
  !
  character(len=256)    :: fpathin
  character(len=256)    :: fpathout
  !
  namelist /nml_p2a_g/ x_num, y_num, z_num, var_num, undef, &
    &                  iyear, l_leap, fpathin, fpathout
  !
  character(len=256)    :: flin
  character(len=256)    :: flout
  !
  integer(4), save :: daymonth(12)
  integer(4), save :: day_year
  integer(4), parameter :: num_data = 73 ! = 365 / 5
  integer(4), save :: day_pen(num_data)
  integer(4) :: nday, month, nd
  !
  real(4), allocatable :: datr4in(:, :, :, :)
  real(8), allocatable :: data_r8(:, :, :, :)
  real(4), allocatable :: datr4out(:, :, :, :)
  !
  integer(4), parameter :: mtin  = 81
  integer(4), parameter :: mtout = 83
  !
  !==========================================
  !
  read(unit=5, nml=nml_p2a_g)
  print *,'x_num    :', x_num
  print *,'y_num    :', y_num
  print *,'z_num    :', z_num
  print *,'var_num  :', var_num
  print *,'UNDEF    :', undef
  print *,'year     :', iyear
  print *,'l_leap   :', l_leap
  print *,'fpath in :', trim(fpathin)
  print *,'fpath out:', trim(fpathout)
  !
  allocate (datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  !
  data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 0.0d0
  datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 0.0e0
  !
  !-------------------------------------
  !
  daymonth( 1)=31
  daymonth( 2)=28
  daymonth( 3)=31
  daymonth( 4)=30
  daymonth( 5)=31
  daymonth( 6)=30
  daymonth( 7)=31
  daymonth( 8)=31
  daymonth( 9)=30
  daymonth(10)=31
  daymonth(11)=30
  daymonth(12)=31
  !
  day_year = 365
  !
  day_pen(1:num_data) = 5
  !
  if (l_leap) then
    if(mod(iyear,4) == 0) then
      if(mod(iyear,100) == 0) then
        if(mod(iyear,400) == 0) then
          daymonth(2) = 29
          day_pen(12) = 6
          day_year = 366
        else
          daymonth(2) = 28
          day_pen(12) = 5
          day_year = 365
        end if
      else
        daymonth(2) = 29
        day_pen(12) = 6
        day_year = 366
      end if
    else
      daymonth(2) = 28
      day_pen(12) = 5
      day_year = 365
    end if
  end if
  !
  !-------------------------------------
  !
  nday = 0
  month = 1
  !
  do nd = 1, num_data
    nday = nday + day_pen(nd)
    if (nday > daymonth(month)) then
      nday = nday - daymonth(month)
      month = month + 1
    end if
    write(6,*) 'Year = ', iyear, ' Month = ', month, ' Day = ', nday
    !
    write(flin, '(a, a, i4.4, i2.2, i2.2)' ) trim(fpathin), '.', iyear, month, nday
    write(*,'(a, a)') 'file in :', trim(flin)
    open(mtin, file=flin, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
    read (mtin, rec=1) datr4in(:,:,:,:)
    !
    where(datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num) == undef)
      datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 0.0e0
    end where
    data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) = data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) &
      &             + real(day_pen(nd), 8) * real(datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num), 8)
    close(mtin)
  end do
  !
  data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) = data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num)  &
    &                                            / real(day_year, 8)
  datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num) = real(data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num), 4)
  !
  where(data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) == 0.0d0)
    datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num) = undef
  end where
  !
  write(flout, '(a, a, i4.4)' ) trim(fpathout), '.', iyear
  write(*,'(a, a)') 'file out :', trim(flout)
  open(mtout, file=flout, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
  write(mtout, rec=1) datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num)
  close(mtout)
  !
!====================================================
end program p2a_g
