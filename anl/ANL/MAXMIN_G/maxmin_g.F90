!maxmin_g.F90
!====================================================
!
!  Pentad to Annual Generalized version
!
!====================================================
program maxmin_g
  !
  implicit none
  !
  integer(4) :: x_num, y_num, z_num, var_num
  integer(4) :: x0, x1, y0, y1, z0, z1
  !
  real(4), save  :: undef  =  0.0e0
  !
  character(len=256)    :: fpathin
  character(len=256)    :: fpathout
  !
  namelist /nml_maxmin_g/ x_num, y_num, z_num, var_num, undef, &
    &                  x0, x1, y0, y1, z0, z1,                 &
    &                  fpathin, fpathout
  !
  character(len=256)    :: flin
  character(len=256)    :: flout
  character(len=256)    :: fotxt
  !
  real(4), allocatable :: datr4in(:, :, :, :)
  real(4), allocatable :: datr4out(:, :)
  !
  integer(4), parameter :: mtin  = 81
  integer(4), parameter :: mtout = 83
  !
  integer(4) :: n
  !
  !==========================================
  !
  read(unit=5, nml=nml_maxmin_g)
  print *,'x_num    :', x_num
  print *,'y_num    :', y_num
  print *,'z_num    :', z_num
  print *,'var_num  :', var_num
  print *,'UNDEF    :', undef
  print *,'fpath in :', trim(fpathin)
  print *,'fpath out:', trim(fpathout)
  !
  allocate (datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (datr4out(1:2, 1:var_num))
  !
  datr4out(1:2, 1:var_num) = 0.0e0
  !
  !-------------------------------------
  !
  write(flin, '(a)' ) trim(fpathin)
  write(*,'(a, a)') 'file in :', trim(flin)
  open(mtin, file=flin, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
  read (mtin, rec=1) datr4in(:,:,:,:)
  !
  where(datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num) == undef)
    datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num) = -1.e33
  end where
  do n = 1, var_num
    datr4out(1, n) = maxval(datr4in(x0:x1, y0:y1, z0:z1, n))    
  end do
  !
  where(datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num) == undef)
    datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 1.e33
  end where
  do n = 1, var_num
    datr4out(2, n) = minval(datr4in(x0:x1, y0:y1, z0:z1, n))    
  end do
  !
  write(*,'(a)') trim(fpathout)
  do n = 1, var_num
    write(*, *) 'var: ', n
    write(flout, '(a, a, i2.2, a)' ) trim(fpathout), '_', n, 'max'
    open(mtout, file=flout, form='unformatted', access='direct', recl=4)
    write(mtout, rec=1) datr4out(1, n)
    close(mtout)
    write(flout, '(a, a, i2.2, a)' ) trim(fpathout), '_', n, 'min'
    open(mtout, file=flout, form='unformatted', access='direct', recl=4)
    write(mtout, rec=1) datr4out(2, n)
    close(mtout)
    !
    write(fotxt, '(a, a, i2.2, a)' ) trim(fpathout), '_', n, 'max.txt'
    open(mtout, file=fotxt)
    write(*, *) 'max: ', datr4out(1, n)
    write(mtout, *) datr4out(1, n)
    close(mtout)
    write(fotxt, '(a, a, i2.2, a)' ) trim(fpathout), '_', n, 'min.txt'
    write(*, *) 'min: ', datr4out(2, n)
    open(mtout, file=fotxt)
    write(mtout, *) datr4out(2, n)
    close(mtout)
  end do
  !
!====================================================
end program maxmin_g
