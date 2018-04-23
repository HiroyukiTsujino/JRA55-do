!-*-F90-*-
!r4_to_r8.F90
!====================================================
!
!  convert real(4) to real(8) 
!
!====================================================
program r4_to_r8
  !
  implicit none
  !
  integer(4) :: x_num, y_num, z_num, var_num
  !
  real(4), save  :: undefin =  0.0e0
  real(8), save  :: undefout=  0.0d0
  !
  character(len=256)    :: flin
  character(len=256)    :: flout
  !
  namelist /nml_r4_to_r8/ x_num, y_num, z_num, var_num, &
    &                     undefin, flin, undefout, flout
  !
  real(4), allocatable :: datr4in(:, :, :, :)
  real(8), allocatable :: datr8out(:, :, :, :)
  !
  integer(4), parameter :: mtin  = 81
  integer(4), parameter :: mtout  = 82
  !
  integer(4) :: k, n
  !
  !==========================================
  !
  read(unit=5, nml=nml_r4_to_r8)
  print *,'x_num    :', x_num
  print *,'y_num    :', y_num
  print *,'z_num    :', z_num
  print *,'var_num  :', var_num
  print *,'UNDEF in :', undefin
  print *,'file  in :', trim(flin)
  print *,'UNDEF out:', undefout
  print *,'file out :', trim(flout)
  !
  allocate (datr4in(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (datr8out(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  !
  open(mtin, file=flin, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
  read (mtin, rec=1) datr4in(:,:,:,:)
  close(mtin)
  !
  datr8out(:,:,:,:) = real( datr4in(:,:,:,:), 8)
  !
  do n = 1, var_num
    do k = 1, z_num
      where(datr4in(1:x_num, 1:y_num, k, n) == undefin)
        datr8out(1:x_num, 1:y_num, k, n) = undefout
      end where
    end do
  end do
  !
  open(mtout, file=flout, form='unformatted', access='direct', recl=8*x_num*y_num*z_num*var_num)
  write(mtout, rec=1) datr8out(1:x_num, 1:y_num, 1:z_num, 1:var_num)
  close(mtout)
  !
!====================================================
end program r4_to_r8
