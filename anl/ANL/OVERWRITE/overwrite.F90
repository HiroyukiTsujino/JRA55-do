!overwrite.F90
!====================================================
!
!  Overwrite  data1 with data2 where data2 is defined
!
!====================================================
program overwrite
  !
  implicit none
  !
  integer(4) :: x_num, y_num, z_num, var_num
  !
  real(4), save  :: undef =  -9.99e33
  !
  character(len=256)    :: flin1
  character(len=256)    :: flin2
  character(len=256)    :: flout
  !
  namelist /nml_overwrite/ x_num, y_num, z_num, var_num, undef, &
    &                      flin1, flin2, flout
  !
  real(4), allocatable :: r4in1(:, :, :, :)
  real(4), allocatable :: r4in2(:, :, :, :)
  real(4), allocatable :: r4out(:, :, :, :)
  !
  integer(4), parameter :: mtin1  = 81
  integer(4), parameter :: mtin2  = 82
  integer(4), parameter :: mtout  = 83
  !
  integer(4) :: i, j, k, n
  !
  !==========================================
  !
  read(unit=5, nml=nml_overwrite)
  print *,'x_num    :', x_num
  print *,'y_num    :', y_num
  print *,'z_num    :', z_num
  print *,'var_num  :', var_num
  print *,'UNDEF    :', undef
  print *,'file in 1:', trim(flin1)
  print *,'file in 2:', trim(flin2)
  print *,'file out :', trim(flout)
  !
  allocate (r4in1(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (r4in2(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (r4out(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  !
  open(mtin1, file=flin1, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
  read (mtin1, rec=1) r4in1(:,:,:,:)
  close(mtin1)
  !
  !r4out(:,:,:,:) = r4in1(:,:,:,:)
  !
  open(mtin2, file=flin2, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
  read (mtin2, rec=1) r4in2(:,:,:,:)
  close(mtin2)
  !
  do n = 1, var_num
    do k = 1, z_num
      do j = 1, y_num
        do i = 1, x_num
          if(r4in2(i,j,k,n) == undef) then
            r4out(i,j,k,n) = r4in1(i,j,k,n)
          else
            r4out(i,j,k,n) = r4in2(i,j,k,n)
          end if
        end do
      end do
    end do
  end do
  !
  !where(r4in1(:,:,:,:) == undef)
  !  r4out(:,:,:,:) = undef
  !end where
  !
  open(mtout, file=flout, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
  write(mtout, rec=1) r4out(1:x_num, 1:y_num, 1:z_num, 1:var_num)
  close(mtout)
  !
!====================================================
end program overwrite
