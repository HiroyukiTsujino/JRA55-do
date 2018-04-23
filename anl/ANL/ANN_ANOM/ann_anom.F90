!-*-F90-*-
!ann_anom.F90
!====================================================
!
!  Annual anomaly from Climatology
!
!====================================================
program ann_anom
  !
  implicit none
  !
  integer(4) :: x_num, y_num, z_num, var_num
  !
  real(4), save  :: undef  =  0.0e0
  integer(4)     :: yearstt, yearend
  !
  character(len=256)    :: fpathin0
  character(len=256)    :: fpathin
  character(len=256)    :: fpathout
  !
  namelist /nml_ann_anom/ x_num, y_num, z_num, var_num, undef, &
    &                     yearstt, yearend, fpathin, fpathout, &
    &                     fpathin0, l_readclim, flclm
  !
  logical :: l_readclim = .false.

  character(len=256)    :: flin
  character(len=256)    :: flout
  character(len=256)    :: flclm='dummy.dat'
  !
  integer(4)       :: nvar
  integer(4)       :: iyr, k
  !
  real(4), allocatable :: datr4in(:, :, :, :)
  real(8), allocatable :: data_r8(:, :, :, :)
  real(8), allocatable :: clim_r8(:, :, :, :)
  real(4), allocatable :: datr4out(:, :, :, :)
  !
  integer(4), parameter :: mtin  = 81
  integer(4), parameter :: mtout = 83
  !
  !==========================================
  !
  read(unit=5, nml=nml_ann_anom)
  print *,'x_num     :', x_num
  print *,'y_num     :', y_num
  print *,'z_num     :', z_num
  print *,'var_num   :', var_num
  print *,'UNDEF     :', undef
  print *,'year stt  :', yearstt
  print *,'year end  :', yearend
  print *,'fpath in  :', trim(fpathin)
  print *,'fpath in0 :', trim(fpathin0)
  print *,'fpath out :', trim(fpathout)
  print *,'l_readclim:', l_readclim
  print *,'file clim :', trim(flclm)
  !
  allocate (datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (clim_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  !
  data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 0.0d0
  clim_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 0.0d0
  datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 0.0e0
  !
  !-------------------------------------
  !
  if (l_readclim) then

    open(mtin, file=flclm, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
    write(*,'(a, a)') 'climatology read from :', trim(flclm)
    read (mtin, rec=1) datr4in(:,:,:,:)
    close(mtin)
    do nvar = 1, var_num
      do k = 1, z_num
        where(datr4in(1:x_num, 1:y_num, k, nvar) == undef)
          datr4in(1:x_num, 1:y_num, k, nvar) = 0.0e0
        end where
      end do
    end do
    clim_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) = real(datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num), 8)

  else

    data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 0.0d0
    do iyr = yearstt, yearend
      write(flin, '(a, a, i4.4)' ) trim(fpathin0), '.', iyr
      write(*,'(a, a)') 'file in :', trim(flin)
      open(mtin, file=flin, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
      read(mtin, rec=1) datr4in(:,:,:,:)
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
    clim_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num) = data_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num)  &
      &                                            / real(yearend -yearstt +1, 8)
  end if
  !
  do iyr = yearstt, yearend
    write(flin, '(a, a, i4.4)' ) trim(fpathin), '.', iyr
    write(*,'(a, a)') 'file in :', trim(flin)
    open(mtin, file=flin, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
    read (mtin, rec=1) datr4in(:,:,:,:)
    close(mtin)
    !
    do nvar = 1, var_num
      do k = 1, z_num
        where(datr4in(1:x_num, 1:y_num, k, nvar) == undef)
          datr4in(1:x_num, 1:y_num, k, nvar) = 0.0e0
        end where
      end do
    end do
    !
    datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num) = datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num)   &
         &                                    - real(clim_r8(1:x_num, 1:y_num, 1:z_num, 1:var_num), 4)

    do nvar = 1, var_num
      do k = 1, z_num
        where(datr4in(1:x_num, 1:y_num, k, nvar) == 0.0e0)
          datr4out(1:x_num, 1:y_num, k, nvar) = undef
        end where
      end do
    end do

    write(flout, '(a, a, i4.4)' ) trim(fpathout),'.anom.', iyr
    write(*,'(a, a)') 'file out :', trim(flout)
    open(mtout, file=flout, form='unformatted', access='direct', recl=4*x_num*y_num*z_num)
    do nvar = 1, var_num
      write(mtout, rec=nvar) datr4out(1:x_num, 1:y_num, 1:z_num, nvar)
    end do
    close(mtout)

  end do

!====================================================
end program ann_anom
