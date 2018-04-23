!-*-F90-*-
!mon_anom.F90
!====================================================
!
!  Monthly Anomaly from Climatology
!
!====================================================
program mon_eke
  !
  implicit none
  !
  integer(4) :: x_num, y_num, z_num

  real(4), save  :: undef  =  0.0e0
  integer(4)     :: yearstt, yearend
  !
  character(len=256)    :: fpathinu, fpathinv
  character(len=256)    :: fpathout
  !
  namelist /nml_mon_eke/ x_num, y_num, z_num, undef, &
    &                     yearstt, yearend, &
    &                     fpathinu, fpathinv, &
    &                     fpathout
  !
  character(len=256)    :: flinu, flinv
  character(len=256)    :: flout
  !
  integer(4)       :: nday, month, nvar
  integer(4)       :: iyr, i, j, k
  !
  real(4), allocatable :: datr4in(:, :, :)
  real(8), allocatable :: u_r8(:, :, :)
  real(8), allocatable :: v_r8(:, :, :)
  real(8), allocatable :: ucl_r8(:, :, :, :)
  real(8), allocatable :: vcl_r8(:, :, :, :)
  real(4), allocatable :: datr4out(:, :, :)
  !
  integer(4), parameter :: mtinu = 81, mtinv = 82
  integer(4), parameter :: mtout = 83
  !
  !==========================================
  !
  read(unit=5, nml=nml_mon_eke)
  print *,'x_num    :', x_num
  print *,'y_num    :', y_num
  print *,'z_num    :', z_num
  print *,'UNDEF    :', undef
  print *,'year stt :', yearstt
  print *,'year end :', yearend
  print *,'fpath inu :', trim(fpathinu)
  print *,'fpath inv :', trim(fpathinv)
  print *,'fpath out :', trim(fpathout)
  !
  allocate (datr4in(1:x_num, 1:y_num, 1:z_num))
  allocate (datr4out(1:x_num, 1:y_num, 1:z_num))

  allocate (u_r8(1:x_num, 1:y_num, 1:z_num))
  allocate (ucl_r8(1:x_num, 1:y_num, 1:z_num, 1:12))
  allocate (v_r8(1:x_num, 1:y_num, 1:z_num))
  allocate (vcl_r8(1:x_num, 1:y_num, 1:z_num, 1:12))
  !
  ucl_r8(1:x_num, 1:y_num, 1:z_num, 1:12) = 0.0d0
  vcl_r8(1:x_num, 1:y_num, 1:z_num, 1:12) = 0.0d0
  datr4out(1:x_num, 1:y_num, 1:z_num) = 0.0e0
  !
  !-------------------------------------
  !
  do month = 1, 12
    do iyr = yearstt, yearend

      write(flinu, '(a, a, i4.4, i2.2)' ) trim(fpathinu), '.', iyr, month
      write(*,'(a, a)') 'file in :', trim(flinu)
      open(mtinu, file=flinu, form='unformatted', access='direct', recl=4*x_num*y_num*z_num)
      read (mtinu, rec=1) datr4in(:,:,:)
      close(mtinu)

      do k = 1, z_num
        where(datr4in(1:x_num, 1:y_num, k) == undef)
          datr4in(1:x_num, 1:y_num, k) = 0.0e0
        end where
      end do

      ucl_r8(1:x_num, 1:y_num, 1:z_num, month) = ucl_r8(1:x_num, 1:y_num, 1:z_num, month) &
           &                              + real(datr4in(1:x_num, 1:y_num, 1:z_num), 8) * 1.0d-2

      !----

      write(flinv, '(a, a, i4.4, i2.2)' ) trim(fpathinv), '.', iyr, month
      write(*,'(a, a)') 'file in :', trim(flinv)
      open(mtinv, file=flinv, form='unformatted', access='direct', recl=4*x_num*y_num*z_num)
      read (mtinv, rec=1) datr4in(:,:,:)
      close(mtinv)

      do k = 1, z_num
        where(datr4in(1:x_num, 1:y_num, k) == undef)
          datr4in(1:x_num, 1:y_num, k) = 0.0e0
        end where
      end do

      vcl_r8(1:x_num, 1:y_num, 1:z_num, month) = vcl_r8(1:x_num, 1:y_num, 1:z_num, month) &
           &                              + real(datr4in(1:x_num, 1:y_num, 1:z_num), 8) * 1.0d-2
    end do
    !
    ucl_r8(1:x_num, 1:y_num, 1:z_num, month) = ucl_r8(1:x_num, 1:y_num, 1:z_num, month)  &
      &                                            / real(yearend -yearstt +1, 8)

    vcl_r8(1:x_num, 1:y_num, 1:z_num, month) = vcl_r8(1:x_num, 1:y_num, 1:z_num, month)  &
      &                                            / real(yearend -yearstt +1, 8)

  end do
  !
  do iyr = yearstt, yearend
    do month = 1, 12

      write(flinu, '(a, a, i4.4, i2.2)' ) trim(fpathinu), '.', iyr, month
      write(*,'(a, a)') 'file in :', trim(flinu)
      open (mtinu, file=flinu, form='unformatted', access='direct', recl=4*x_num*y_num*z_num)
      read (mtinu, rec=1) datr4in(:,:,:)
      close(mtinu)
      !
      do k = 1, z_num
        where(datr4in(1:x_num, 1:y_num, k) == undef)
          datr4in(1:x_num, 1:y_num, k) = 0.0e0
        end where
      end do

      u_r8(1:x_num, 1:y_num, 1:z_num) = real(datr4in(1:x_num, 1:y_num, 1:z_num),8) * 1.0d-2

      !---

      write(flinv, '(a, a, i4.4, i2.2)' ) trim(fpathinv), '.', iyr, month
      write(*,'(a, a)') 'file in :', trim(flinv)
      open (mtinv, file=flinv, form='unformatted', access='direct', recl=4*x_num*y_num*z_num)
      read (mtinv, rec=1) datr4in(:,:,:)
      close(mtinv)
      !
      do k = 1, z_num
        where(datr4in(1:x_num, 1:y_num, k) == undef)
          datr4in(1:x_num, 1:y_num, k) = 0.0e0
        end where
      end do

      v_r8(1:x_num, 1:y_num, 1:z_num) = real(datr4in(1:x_num, 1:y_num, 1:z_num),8) * 1.0d-2

      !---

      do k = 1, z_num
        do j = 1, y_num
          do i = 1, x_num
            datr4out(i,j,k) = 0.5 * real((u_r8(i,j,k) - ucl_r8(i,j,k,month))**2 + (v_r8(i,j,k) - vcl_r8(i,j,k,month))**2 ,4)
          end do
        end do
      end do

      do k = 1, z_num
        where(datr4in(1:x_num, 1:y_num, k) == 0.0e0)
          datr4out(1:x_num, 1:y_num, k) = undef
        end where
      end do

      write(flout, '(a, a, i4.4, i2.2)' ) trim(fpathout), '.', iyr, month
      write(*,'(a, a)') 'file out :', trim(flout)
      open (mtout, file=flout, form='unformatted', access='direct', recl=4*x_num*y_num*z_num)
      write(mtout, rec=1) datr4out(1:x_num, 1:y_num, 1:z_num)
      close(mtout)

    end do
  end do
  !
!====================================================
end program mon_eke
