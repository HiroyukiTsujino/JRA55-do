!-*-F90-*-
!txt_to_r4.F90
!====================================================
!
!  Text file to real(4) file
!
!====================================================
program txt_to_r4
  !
  implicit none
  !
  integer(4), save :: x_num = 1
  integer(4), save :: y_num = 1
  integer(4), save :: z_num = 1
  integer(4), save :: var_num = 1
  integer(4), save :: t_num = 1
  !
  real(4), save  :: UNDEF  =  -9.99e33
  !
  character(len=256)    :: flin
  character(len=256)    :: flout
  !
  !
  namelist /nml_txt_to_r4/ x_num, y_num, z_num, var_num, t_num,  &
    &                      undef, flin, flout
  !
  real(4), allocatable :: dtin(:, :, :, :, :)
  !
  integer(4), parameter :: mtin  = 81
  integer(4), parameter :: mtout  = 83
  !
  integer(4) :: i, j, k, v, t
  !
  !==========================================
  !
  read(unit=5, nml=nml_txt_to_r4)
  print *,'x_num    :', x_num
  print *,'y_num    :', y_num
  print *,'z_num    :', z_num
  print *,'var_num  :', var_num
  print *,'t_num    :', t_num
  print *,'UNDEF    :', undef
  print *,'file in  :', trim(flin)
  print *,'file out :', trim(flout)
  !
  allocate (dtin(1:x_num, 1:y_num, 1:z_num, 1:var_num, 1:t_num))
  !
  open(mtin, file=flin)
  do t = 1, t_num
    do v = 1, var_num
      do k = 1, z_num
        do j = 1, y_num
          do i = 1, x_num
            !read (mtin, '(f)') dtin(i, j, k, v, t)
            read (mtin, * ) dtin(i, j, k, v, t)
write(*,*) dtin(i, j, k, v, t)
          end do
        end do
      end do
    end do
  end do
  !
  open(mtout, file=flout, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
  do t = 1, t_num
    write(mtout, rec=t) dtin(1:x_num, 1:y_num, 1:z_num, 1:var_num, t)
  end do
  close(mtout)
  !
!====================================================
end program txt_to_r4
