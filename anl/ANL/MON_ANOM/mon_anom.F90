!-*-F90-*-
!mon_anom.F90
!====================================================
!
!  Monthly Anomaly from Climatology
!
!====================================================
program mon_anom
  !
  implicit none
  !
  integer(4) :: x_num, y_num, z_num, var_num
  integer(4), save :: xwrite = 1
  integer(4), save :: ywrite = 1
  integer(4), save :: zwrite = 1
  integer(4), save :: vwrite = 1
  !
  real(4), save  :: undef  =  0.0e0
  integer(4)     :: yearstt, yearend
  !
  character(len=256)    :: fpathin0
  character(len=256)    :: fpathin
  character(len=256)    :: fpathout
  !
  namelist /nml_mon_anom/ x_num, y_num, z_num, var_num, undef, &
    &                     xwrite, ywrite, zwrite, vwrite,      &
    &                     yearstt, yearend, fpathin, fpathout, &
    &                     fpathin0
  !
  character(len=256)    :: flin
  character(len=256)    :: flout
  !
  integer(4)       :: nday, month, nvar
  integer(4)       :: iyr, k
  !
  real(4), allocatable :: datr4in(:, :, :, :)
  real(8), allocatable :: data_r8(:, :, :, :)
  real(8), allocatable :: clim_r8(:, :, :, :, :)
  real(4), allocatable :: datr4out(:, :, :, :)
  !
  integer(4), parameter :: mtin  = 81
  integer(4), parameter :: mtout = 83
  !
  !==========================================
  !
  read(unit=5, nml=nml_mon_anom)
  print *,'x_num    :', x_num
  print *,'y_num    :', y_num
  print *,'z_num    :', z_num
  print *,'var_num  :', var_num
  print *,'UNDEF    :', undef
  print *,'xwrite   :', xwrite
  print *,'ywrite   :', ywrite
  print *,'zwrite   :', zwrite
  print *,'vwrite   :', vwrite
  print *,'year stt :', yearstt
  print *,'year end :', yearend
  print *,'fpath in :', trim(fpathin)
  print *,'fpath in0:', trim(fpathin0)
  print *,'fpath out:', trim(fpathout)
  !
  allocate (datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (clim_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num, 1:12))
  allocate (datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  !
  data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 0.0d0
  clim_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num, 1:12) = 0.0d0
  datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 0.0e0
  !
  !-------------------------------------
  !
  do month = 1, 12
    data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 0.0d0
    do iyr = yearstt, yearend
      write(flin, '(a, a, i4.4, i2.2)' ) trim(fpathin0), '.', iyr, month
      write(*,'(a, a)') 'file in :', trim(flin)
      open(mtin, file=flin, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
      read (mtin, rec=1) datr4in(:,:,:,:)
      !
      do nvar = 1, var_num
        do k = 1, z_num
          where(datr4in(1:x_num, 1:y_num, k, nvar) == undef)
            datr4in(1:x_num, 1:y_num, k, nvar) = 0.0e0
          end where
        end do
      end do
      !
      data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) = data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) &
        &                                      + real(datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num), 8)
      close(mtin)
    end do
    !
    clim_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num, month) = data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num)  &
      &                                            / real(yearend -yearstt +1, 8)
  end do
  !
  do iyr = yearstt, yearend
    do month = 1, 12
      write(flin, '(a, a, i4.4, i2.2)' ) trim(fpathin), '.', iyr, month
      write(*,'(a, a)') 'file in :', trim(flin)
      open(mtin, file=flin, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
      read (mtin, rec=1) datr4in(:,:,:,:)
      !
      do nvar = 1, var_num
        do k = 1, z_num
          where(datr4in(1:x_num, 1:y_num, k, nvar) == undef)
            datr4in(1:x_num, 1:y_num, k, nvar) = 0.0e0
          end where
        end do
      end do
      !
      datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num) = datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num)               &
        &                                            - real(clim_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num, month), 4)
      close(mtin)
      !
      do nvar = 1, var_num
        do k = 1, z_num
          where(datr4in(1:x_num, 1:y_num, k, nvar) == undef)
            datr4out(1:x_num, 1:y_num, k, nvar) = undef
          end where
        end do
      end do
      !
      write(flout, '(a, a, i2.2, a, i4.4, i2.2)' ) trim(fpathout), '_', vwrite, '.anom.', iyr, month
      write(*,'(a, a)') 'file out :', trim(flout)
      open(mtout, file=flout, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
      write(mtout, rec=1) datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num)
      close(mtout)
      !
      write(flout, '(a, a, i2.2, a, i4.4, i2.2, a)' ) trim(fpathout), '_', vwrite, '.anom.', iyr, month, '.txt'
      write(*,'(a, a)') 'file out :', trim(flout)
      open(mtout, file=flout)
      write(mtout, *) datr4out(xwrite, ywrite, zwrite, vwrite)
      close(mtout)
    end do
  end do
  !
!====================================================
end program mon_anom
