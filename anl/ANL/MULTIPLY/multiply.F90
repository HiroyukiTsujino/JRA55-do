!-*-F90-*-
!multiply.F90
!====================================================
!
!  Multiply
!
!====================================================
program multiply
  !
  implicit none
  !
  integer(4) :: x_num, y_num, z_num, var_num
  !
  real(4), save  :: UNDEF  =  0.0e0
  !
  character(len=256)    :: flin1
  character(len=256)    :: flin2
  character(len=256)    :: flout
  !
  namelist /nml_multiply/ x_num, y_num, z_num, var_num, undef, &
    &                       flin1, flin2, flout
  !
  real(4), allocatable :: datr4in1(:, :, :, :)
  real(4), allocatable :: datr4in2(:, :, :, :)
  real(4), allocatable :: datr4out(:, :, :, :)
  real(4), allocatable :: flag1(:, :, :, :)
  real(4), allocatable :: flag2(:, :, :, :)
  !
  integer(4), parameter :: mtin1  = 81
  integer(4), parameter :: mtin2  = 82
  integer(4), parameter :: mtout  = 83
  !
  !==========================================
  !
  read(unit=5, nml=nml_multiply)
  print *,'x_num    :', x_num
  print *,'y_num    :', y_num
  print *,'z_num    :', z_num
  print *,'var_num  :', var_num
  print *,'UNDEF    :', undef
  print *,'file in 1:', trim(flin1)
  print *,'file in 2:', trim(flin2)
  print *,'file out :', trim(flout)
  !
  allocate (datr4in1(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (datr4in2(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (flag1(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (flag2(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  !
  !
  datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 0.0e0
  flag1(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 1.0e0
  flag2(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 1.0e0
  !
  open(mtin1, file=flin1, form='unformatted', access='direct', &
    &         recl=4*x_num*y_num*z_num*var_num)
  read (mtin1, rec=1) datr4in1(:,:,:,:)
  where(datr4in1(1:x_num, 1:y_num, 1:z_num, 1:var_num) == undef)
    datr4in1(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 0.0e0
    flag1(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 0.0e0
  end where
  close(mtin1)
  !
  open(mtin2, file=flin2, form='unformatted', access='direct', &
    &         recl=4*x_num*y_num*z_num*var_num)
  read (mtin2, rec=1) datr4in2(:,:,:,:)
  where(datr4in2(1:x_num, 1:y_num, 1:z_num, 1:var_num) == undef)
    datr4in2(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 0.0e0
    flag2(1:x_num, 1:y_num, 1:z_num, 1:var_num) = 0.0e0
  end where
  close(mtin2)
  !
  datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num) =        &
    &     datr4in1(1:x_num, 1:y_num, 1:z_num, 1:var_num)  &
    &   * datr4in2(1:x_num, 1:y_num, 1:z_num, 1:var_num)
  !
  where(max(flag1(1:x_num, 1:y_num, 1:z_num, 1:var_num),          &
    &       flag2(1:x_num, 1:y_num, 1:z_num, 1:var_num)) < 1.0e0)
    datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num) = undef
  end where
  !
  open(mtout, file=flout, form='unformatted', access='direct', &
    &         recl=4*x_num*y_num*z_num*var_num)
  write(mtout, rec=1) datr4out(1:x_num, 1:y_num, 1:z_num, 1:var_num)
  close(mtout)
  !
!====================================================
end program multiply
