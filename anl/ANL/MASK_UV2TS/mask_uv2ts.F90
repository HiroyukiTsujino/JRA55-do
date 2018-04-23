!-*-F90-*-
!mask_uv2ts.F90
!====================================================
!
!  make TS-point mask data from UV-point mask data
!
!====================================================
program mask_uv2ts
  !
  implicit none
  !
  integer(4) :: x_num, y_num, z_num, var_num
  !
  character(len=256)    :: flin
  character(len=256)    :: flout
  !
  namelist /nml_mask_uv2ts/ x_num, y_num, z_num, var_num,    &
    &                       flin, flout
  !
  real(4), allocatable :: datr4i(:, :, :, :)
  real(4), allocatable :: datr4o(:, :, :, :)
  !
  integer(4), parameter :: mtin  = 81
  integer(4), parameter :: mtout = 82
  !
  integer(4) :: i, j, k, n
  !
  !==========================================
  !
  read(unit=5, nml=nml_mask_uv2ts)
  print *,'x_num    :', x_num
  print *,'y_num    :', y_num
  print *,'z_num    :', z_num
  print *,'var_num  :', var_num
  print *,'file  in :', trim(flin)
  print *,'file out :', trim(flout)
  !
  allocate (datr4i(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  allocate (datr4o(1:x_num, 1:y_num, 1:z_num, 1:var_num))
  !
  open(mtin, file=flin, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
  read (mtin, rec=1) datr4i(:,:,:,:)
  close(mtin)
  !
  datr4o(:,:,:,:) = 0.0e0
  do n = 1, var_num
    do k = 1, z_num
      do j = 2, y_num
        do i = 2, x_num
          datr4o(i,j,k,n) = max(datr4i(i-1,j  ,k,n), datr4i(i,j  ,k,n), &
                                datr4i(i-1,j-1,k,n), datr4i(i,j-1,k,n) )
        end do
      end do
    end do
  end do
#ifdef OGCM_CYCLIC
  datr4o(1,:,:,:) = datr4o(x_num-4,:,:,:) 
#endif /* OGCM_CYCLIC */
  !
  open(mtout, file=flout, form='unformatted', access='direct', recl=4*x_num*y_num*z_num*var_num)
  write(mtout, rec=1) datr4o(1:x_num, 1:y_num, 1:z_num, 1:var_num)
  close(mtout)
  !
!====================================================
end program mask_uv2ts
