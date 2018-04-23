!xfactor.F90
!====================================================
!
!  x factor
!
!====================================================
program xfactor
  !
  implicit none
  !
  integer(4) :: x_num, y_num, z_num_1, z_num_2, var_num_1, var_num_2
  !
  real(4), save  :: UNDEF  =  0.0e0
  !
  character(len=256)    :: flin1
  character(len=256)    :: flin2
  character(len=256)    :: flout
  !
  real(4) :: factor1, factor2
  !
  namelist /nml_xfactor/ x_num, y_num, undef,     &
    &                 flin1, z_num_1, var_num_1,  &
    &                 flin2, z_num_2, var_num_2,  &
    &                 flout
  !
  real(4), allocatable :: datr4in1(:, :, :, :)
  real(4), allocatable :: datr4in2(:, :, :, :)
  real(4), allocatable :: datr4out(:, :, :, :)
  real(4), allocatable :: flag(:, :, :, :)
  !
  integer(4), parameter :: mtin1  = 81
  integer(4), parameter :: mtin2  = 82
  integer(4), parameter :: mtout  = 83
  !
  integer(4) :: k, n
  !
  !==========================================
  !
  read(unit=5, nml=nml_xfactor)
  print *,'x_num    :', x_num
  print *,'y_num    :', y_num
  print *,'UNDEF    :', undef
  print *,'file in 1:', trim(flin1)
  print *,'z_num_1  :', z_num_1
  print *,'var_num_1:', var_num_1
  print *,'file in 2:', trim(flin2)
  print *,'z_num_2  :', z_num_2
  print *,'var_num_2:', var_num_2
  print *,'file out :', trim(flout)
  !
  allocate (datr4in1(1:x_num, 1:y_num, 1:z_num_1, 1:var_num_1))
  allocate (datr4in2(1:x_num, 1:y_num, 1:z_num_2, 1:var_num_2))
  allocate (datr4out(1:x_num, 1:y_num, 1:z_num_1, 1:var_num_1))
  allocate (flag(1:x_num, 1:y_num, 1:z_num_1, 1:var_num_1))
  !
  !
  datr4out(1:x_num, 1:y_num, 1:z_num_1, 1:var_num_1) = 0.0e0
  flag(1:x_num, 1:y_num, 1:z_num_1, 1:var_num_1) = 1.0e0
  !
  open(mtin1, file=flin1, form='unformatted', access='direct', recl=4*x_num*y_num*z_num_1*var_num_1)
  read (mtin1, rec=1) datr4in1(:,:,:,:)
  where(datr4in1(1:x_num, 1:y_num, 1:z_num_1, 1:var_num_1) == undef)
    datr4in1(1:x_num, 1:y_num, 1:z_num_1, 1:var_num_1) = 0.0e0
    flag(1:x_num, 1:y_num, 1:z_num_1, 1:var_num_1) = 0.0e0
  end where
  close(mtin1)
  !
  open(mtin2, file=flin2, form='unformatted', access='direct', recl=4*x_num*y_num*z_num_2*var_num_2)
  read (mtin2, rec=1) datr4in2(:,:,:,:)
  where(datr4in2(1:x_num, 1:y_num, 1:z_num_2, 1:var_num_2) == undef)
    datr4in2(1:x_num, 1:y_num, 1:z_num_2, 1:var_num_2) = 0.0e0
  end where
  close(mtin2)
  !
  if(var_num_2 == var_num_1) then
    if(z_num_2 == z_num_1) then
      datr4out(1:x_num, 1:y_num, 1:z_num_1, 1:var_num_1) =                &
        &             datr4in1(1:x_num, 1:y_num, 1:z_num_1, 1:var_num_1)  &
        &           * datr4in2(1:x_num, 1:y_num, 1:z_num_1, 1:var_num_1)
    else if(z_num_2 == 1) then
      do k = 1, z_num_1
        datr4out(1:x_num, 1:y_num, k, 1:var_num_1) =                &
          &             datr4in1(1:x_num, 1:y_num, k, 1:var_num_1)  &
          &           * datr4in2(1:x_num, 1:y_num, 1, 1:var_num_1)
      end do
    else
      write(*,*) 'incorrect z_num_2 var_num_2 !'
      write(*,*) 'z_num_1:', z_num_1
      write(*,*) 'z_num_2:', z_num_2
      write(*,*) 'var_num_1:', var_num_1
      write(*,*) 'var_num_2:', var_num_2
      stop
    end if
  else if(var_num_2 == 1) then
    if(z_num_2 == z_num_1) then
      do n = 1, var_num_1
        datr4out(1:x_num, 1:y_num, 1:z_num_1, n) =                &
          &             datr4in1(1:x_num, 1:y_num, 1:z_num_1, n)  &
          &           * datr4in2(1:x_num, 1:y_num, 1:z_num_1, 1)
      end do
    else if(z_num_2 == 1) then
      do n = 1, var_num_1
        do k = 1, z_num_1
          write(*,*) 'k=', k
          datr4out(1:x_num, 1:y_num, k, n) =                &
            &             datr4in1(1:x_num, 1:y_num, k, n)  &
            &           * datr4in2(1:x_num, 1:y_num, 1, 1)
        end do
      end do
    else
      write(*,*) 'incorrect z_num_2 var_num_2 !'
      write(*,*) 'z_num_1:', z_num_1
      write(*,*) 'z_num_2:', z_num_2
      write(*,*) 'var_num_1:', var_num_1
      write(*,*) 'var_num_2:', var_num_2
      stop
    end if
  end if
  !
  where(flag(1:x_num, 1:y_num, 1:z_num_1, 1:var_num_1) < 1.0e0)
    datr4out(1:x_num, 1:y_num, 1:z_num_1, 1:var_num_1) = undef
  end where
  !
  open(mtout, file=flout, form='unformatted', access='direct', recl=4*x_num*y_num*z_num_1*var_num_1)
  write(mtout, rec=1) datr4out(1:x_num, 1:y_num, 1:z_num_1, 1:var_num_1)
  close(mtout)
  !
!====================================================
end program xfactor
