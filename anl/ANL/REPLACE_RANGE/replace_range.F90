!replace_range.F90
!====================================================
!
!  Replace data in range (x1-x2, y1-y2, z1-z2, var1-var2)
!
!====================================================
program replace_range
  !
  implicit none
  !
  integer(4) :: x_num, y_num, z_num, var_num
  integer(4) :: x_stt, x_end, y_stt, y_end, z_stt, z_end, var_stt, var_end
  real(4)    :: add, factor
  !
  real(4), save  :: undef =  -9.99e33
  !
  character(len=256)    :: flin
  character(len=256)    :: flout
  !
  namelist /nml_replace_range/ x_num, y_num, z_num, var_num, undef, &
    &                          flin,                                &
    &   x_stt, x_end, y_stt, y_end, z_stt, z_end, var_stt, var_end, &
    &                          add, factor,                         &
    &                          flout
  !
  real(4), allocatable :: r4in(:, :, :, :)
  real(4), allocatable :: r4out(:, :, :, :)
  !
  integer(4), parameter :: mtin1  = 81
  integer(4), parameter :: mtout  = 83
  !
  integer(4) :: i, j, k, n
  !
  !==========================================
  !
  read(unit=5, nml=nml_replace_range)
  print *,'x_num    :', x_num
  print *,'y_num    :', y_num
  print *,'z_num    :', z_num
  print *,'var_num  :', var_num
  print *,'UNDEF    :', undef
  print *,'file in  :', trim(flin)
  print *,'x range  :', x_stt, '-', x_end
  print *,'x range  :', y_stt, '-', y_end
  print *,'x range  :', z_stt, '-', z_end
  print *,'x range  :', var_stt, '-', var_end
  print *,'add      :', add
  print *,'factor   :', factor
  print *,'file out :', trim(flout)
  !
  allocate (r4in(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (r4out(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  !
  open(mtin1, file=flin, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
  read (mtin1, rec=1) r4in(:,:,:,:)
  close(mtin1)
  !
  r4out(:,:,:,:) = r4in(:,:,:,:)
  !
  do n = var_stt, var_end
    do k = z_stt, z_end
      do j = y_stt, y_end
        do i = x_stt, x_end
          if(r4out(i,j,k,n) /= undef) then
            r4out(i,j,k,n) = (r4in(i,j,k,n) + add) * factor
          end if
        end do
      end do
    end do
  end do
  !
  deallocate(r4in)
  !
  open(mtout, file=flout, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
  write(mtout, rec=1) r4out(1:x_num, 1:y_num, 1:z_num, 1:var_num)
  close(mtout)
  !
  deallocate(r4out)
!====================================================
end program replace_range
