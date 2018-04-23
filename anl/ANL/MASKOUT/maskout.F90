!-*-F90-*-
!maskout.F90
!====================================================
!
!  mask out data
!
!====================================================
program maskout
  !
  implicit none
  !
  integer(4) :: x_num, y_num, z_num, var_num
  !
  real(4), save  :: lwlim =  0.0e0
  real(4), save  :: uplim =  1.0e0
  real(4), save  :: undef =  0.0e0
  !
  character(len=256)    :: flin
  character(len=256)    :: flout
  !
  namelist /nml_maskout/ x_num, y_num, z_num, var_num,    &
    &                    lwlim, uplim, flin, undef, flout
  !
  real(4), allocatable :: datr4(:, :, :, :)
  !
  integer(4), parameter :: mtin  = 81
  integer(4), parameter :: mtout = 82
  !
  integer(4) :: k, n
  !
  !==========================================
  !
  read(unit=5, nml=nml_maskout)
  print *,'x_num    :', x_num
  print *,'y_num    :', y_num
  print *,'z_num    :', z_num
  print *,'var_num  :', var_num
  print *,'lower lim:', lwlim
  print *,'upper lim:', uplim
  print *,'file  in :', trim(flin)
  print *,'UNDEF out:', undef
  print *,'file out :', trim(flout)
  !
  allocate (datr4(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  !
  open(mtin, file=flin, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
  read (mtin, rec=1) datr4(:,:,:,:)
  close(mtin)
  !
  do n = 1, var_num
    do k = 1, z_num
      where(datr4(1:x_num, 1:y_num, k, n) < lwlim)
        datr4(1:x_num, 1:y_num, k, n) = undef
      end where
      where(datr4(1:x_num, 1:y_num, k, n) > uplim)
        datr4(1:x_num, 1:y_num, k, n) = undef
      end where
    end do
  end do
  !
  open(mtout, file=flout, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
  write(mtout, rec=1) datr4(1:x_num, 1:y_num, 1:z_num, 1:var_num)
  close(mtout)
  !
!====================================================
end program maskout
