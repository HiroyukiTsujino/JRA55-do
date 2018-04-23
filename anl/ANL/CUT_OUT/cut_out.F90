!cut_out.F90
!====================================================
!
!  Cut out (x1-x2, y1-y2, z1-z2, var1-var2)
!
!====================================================
program cut_out
  !
  implicit none
  !
  integer(4) :: x_num, y_num, z_num, var_num, rec_num
  integer(4) :: x_stt, x_end, y_stt, y_end, z_stt, z_end, var_stt, var_end
  integer(4) :: rec_stt, rec_end
  integer(4) :: x_out, y_out, z_out, var_out, rec_out
  !
  character(len=256)    :: flin
  character(len=256)    :: flout
  !
  namelist /nml_cut_out/ x_num, y_num, z_num, var_num, rec_num  &
    &                          flin,                                &
    &   x_stt, x_end, y_stt, y_end, z_stt, z_end, var_stt, var_end, &
    &   rec_stt, rec_end,      flout
  !
  real(4), allocatable :: r4in(:, :, :, :)
  real(4), allocatable :: r4out(:, :, :, :)
  !
  integer(4), parameter :: mtin1  = 81
  integer(4), parameter :: mtout  = 83
  !
  integer(4) :: i, j, k, n, t
  !
  !==========================================
  !
  rec_num = 1
  rec_stt = 1
  rec_end = 1
  !
  read(unit=5, nml=nml_cut_out)
  print *,'x_num    :', x_num
  print *,'y_num    :', y_num
  print *,'z_num    :', z_num
  print *,'var_num  :', var_num
  print *,'rec_num  :', rec_num
  print *,'file in  :', trim(flin)
  print *,'x range  :', x_stt, '-', x_end
  print *,'y range  :', y_stt, '-', y_end
  print *,'z range  :', z_stt, '-', z_end
  print *,'v range  :', var_stt, '-', var_end
  print *,'t range  :', rec_stt, '-', rec_end
  print *,'file out :', trim(flout)
  x_out = x_end -x_stt +1
  y_out = y_end -y_stt +1
  z_out = z_end -z_stt +1
  var_out = var_end -var_stt +1
  rec_out = rec_end -rec_stt +1
  print *,'x_out    :', x_out
  print *,'y_out    :', y_out
  print *,'z_out    :', z_out
  print *,'var_out  :', var_out
  print *,'rec_out  :', rec_out
  !
  allocate (r4in(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (r4out(1:x_out, 1:y_out, 1:z_out, 1:var_out))
  !
  open(mtin1, file=flin, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
  do t = rec_stt, rec_end
    read (mtin1, rec=t) r4in(:,:,:,:)
    !
    r4out(1:x_out, 1:y_out, 1:z_out, 1:var_out) = &
      &  r4in(x_stt:x_end, y_stt:y_end, z_stt:z_end, var_stt:var_end)
    !
    !
    open(mtout, file=flout, form='unformatted', access='direct', recl=4*x_out*y_out*z_out*var_out)
    write(mtout, rec=t-rec_stt+1) r4out(1:x_out, 1:y_out, 1:z_out, 1:var_out)
    close(mtout)
  end do
  close(mtin1)
  deallocate(r4in)
  deallocate(r4out)
!====================================================
end program cut_out
