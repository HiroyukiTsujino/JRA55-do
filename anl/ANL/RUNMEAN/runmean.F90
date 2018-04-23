!runmean.F90
!====================================================
!
!  Running Mean
!
!====================================================
program runmean
  !
  implicit none
  !
  integer(4) :: x_num, y_num, z_num, var_num
  !
  real(4), save  :: undef  =  0.0e0
  integer(4)     :: year_stt, year_end, y_range
  !
  logical, save         :: l_leap
  !
  character(len=256)    :: fpathin
  character(len=256)    :: fpathout
  !
  namelist /nml_runmean/ x_num, y_num, z_num, var_num, &
    &                    undef, l_leap,                &
    &                    year_stt, year_end, y_range,   &
    &                    fpathin, fpathout
  !
  character(len=256)    :: flin
  character(len=256)    :: flout
  !
  integer(4), parameter :: mtin  = 81
  integer(4), parameter :: mtout = 83
  !
  integer(4) :: year0, iyear
  integer(4) :: day_year, day_sum
  integer(4) :: nday, month, nd
  !
  real(4), allocatable :: datr4in(:, :, :, :)
  real(8), allocatable :: data_r8(:, :, :, :)
  real(4), allocatable :: datr4out(:, :, :, :)
  !
  !==========================================
  !
  read(unit=5, nml=nml_runmean)
  print *,'x_num    :', x_num
  print *,'y_num    :', y_num
  print *,'z_num    :', z_num
  print *,'var_num  :', var_num
  print *,'UNDEF    :', undef
  print *,'l_leap   :', l_leap
  print *,'y_range  :', y_range
  print *,'year_stt :', year_stt
  print *,'year_end :', year_end
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
  do year0 = year_stt, year_end
    day_sum = 0
    do iyear = year0 - y_range, year0 + y_range
      day_year = 365
      if(l_leap) call num_day_year
      day_sum = day_sum + day_year
      write(flin, '(a, a, i4.4)' ) trim(fpathin), '.', iyear
      write(*,'(a, a)') 'file in :', trim(flin)
      !
      open(mtin, file=flin, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
      read (mtin, rec=1) datr4in(:,:,:,:)
      where(datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num) == undef)
        datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 0.0e0
      end where
      data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) =                                    &
        &                               data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num)    &
        &    + real(day_year, 8) * real(datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num), 8)
      close(mtin)
    end do
    !
    data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) =                          &
      &      data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) / real(day_sum, 8)
    datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num) = real(data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num), 4)
    where(data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) == 0.0d0)
      datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num) = undef
    end where
    !
    write(flout, '(a, a, i4.4)' ) trim(fpathout), '.', year0
    write(*,'(a, a)') 'file out :', trim(flout)
    open(mtout, file=flout, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
    write(mtout, rec=1) datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num)
    close(mtout)
  end do
  !
contains
!====================================================
subroutine num_day_year
  !
  day_year = 365
  !
  if (l_leap) then
    if(mod(iyear,4) == 0) then
      if(mod(iyear,100) == 0) then
        if(mod(iyear,400) == 0) then
          day_year = 366
        else
          day_year = 365
        end if
      else
        day_year = 366
      end if
    else
      day_year = 365
    end if
  end if
  !
end subroutine num_day_year
!====================================================
end program runmean
