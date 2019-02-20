! -*-F90-*-
!
!---------------------- globcurrent_smooth.F90 ------------------------
!
!  Information:
!      Apply smoothing filter to Glob Current product.
!
!-------------------------------------------------------------------
program globcurrent_smooth

  use file_open_close_manager

  implicit none

  integer(4), parameter :: imf = 1440, jmf = 720
  real(4),allocatable :: dat4_org(:,:)

  ! original data

  real(8),allocatable :: datu_org(:,:)
  real(8),allocatable :: datv_org(:,:)
  real(8),allocatable :: mask_org(:,:)

  ! smoothed data

  real(8),allocatable :: datu_tmp(:,:)
  real(8),allocatable :: datv_tmp(:,:)
  real(8),allocatable :: datu_smt(:,:)
  real(8),allocatable :: datv_smt(:,:)

  integer(4), parameter :: mtin1 = 61
  integer(4), parameter :: mtot1 = 70
  integer(4) :: mtnam

  character(len=256) :: fl_in
  character(len=256) :: fl_ot
  character(len=256) :: file_namelist_smooth

  integer(4) :: i, j, n
  integer(4) :: iter, iter_max

  real(4) :: undef_out, undef_in
  real(8) :: hl1, hl2, hl3, hl4

  !-----------------------------------------------------------------------

  namelist /nml_smooth/ fl_in, fl_ot, iter_max, &
       & undef_in, undef_out

  !-----------------------------------------------------------------------

  file_namelist_smooth='namelist.smooth'
  call open_file_plain(mtnam,file_namelist_smooth)
  read(mtnam,nml=nml_smooth)
  close(mtnam)

  allocate(dat4_org(1:imf,1:jmf))
  allocate(datu_org(0:imf+1,0:jmf+1))
  allocate(datv_org(0:imf+1,0:jmf+1))
  allocate(mask_org(0:imf+1,0:jmf+1))

  dat4_org(:,:) = 0.0e0
  datu_org(:,:) = 0.0d0
  datv_org(:,:) = 0.0d0
  mask_org(:,:) = 0.0d0

  allocate(datu_tmp(0:imf+1,0:jmf+1))
  allocate(datv_tmp(0:imf+1,0:jmf+1))
  allocate(datu_smt(0:imf+1,0:jmf+1))
  allocate(datv_smt(0:imf+1,0:jmf+1))

  datu_tmp(:,:) = 0.0d0
  datv_tmp(:,:) = 0.0d0
  datu_smt(:,:) = 0.0d0
  datv_smt(:,:) = 0.0d0

  !--------------------------------------------------------------------

  write(6,*) 'DATA read from ',trim(fl_in)
  open(mtin1,file=trim(fl_in),form='unformatted',access='direct',recl=4*imf*jmf)

  read(mtin1,rec=1) dat4_org(1:imf,1:jmf)
  datu_org(1:imf,1:jmf) = real(dat4_org(1:imf,1:jmf),8)
  datu_org(    0,1:jmf) = datu_org(imf,1:jmf)
  datu_org(imf+1,1:jmf) = datu_org(  1,1:jmf)

  read(mtin1,rec=2) dat4_org(1:imf,1:jmf)
  datv_org(1:imf,1:jmf) = real(dat4_org(1:imf,1:jmf),8)
  datv_org(    0,1:jmf) = datv_org(imf,1:jmf)
  datv_org(imf+1,1:jmf) = datv_org(  1,1:jmf)

  close(mtin1)

  where(dat4_org(1:imf,1:jmf)/=undef_in) mask_org(1:imf,1:jmf) = 1.d0
  mask_org(    0,1:jmf) = mask_org(imf,1:jmf)
  mask_org(imf+1,1:jmf) = mask_org(  1,1:jmf)

  !------

  datu_smt(:,:) = datu_org(:,:)
  datv_smt(:,:) = datv_org(:,:)

  LOOP_ITER: do iter = 1, iter_max

    do j = 1, jmf
      do i = 1, imf
        datu_tmp(i,j) = datu_smt(i,j)
        datv_tmp(i,j) = datv_smt(i,j)
      end do
    end do

    datu_tmp(    0,1:jmf) = datu_tmp(imf,1:jmf)
    datu_tmp(imf+1,1:jmf) = datu_tmp(  1,1:jmf)

    do j = 1, jmf
      do i = 1, imf
        if (mask_org(i,j) > 0.0d0) then
          hl1 =   12.0d0 * mask_org(i  ,j  ) &
               & + 2.0d0 * mask_org(i+1,j  ) &
               & + 2.0d0 * mask_org(i  ,j+1) &
               & + 2.0d0 * mask_org(i-1,j  ) &
               & + 2.0d0 * mask_org(i  ,j-1) &
               & + 1.0d0 * mask_org(i+1,j+1) &
               & + 1.0d0 * mask_org(i-1,j+1) &
               & + 1.0d0 * mask_org(i+1,j-1) &
               & + 1.0d0 * mask_org(i-1,j-1) 
          hl2 =   12.0d0 * mask_org(i  ,j  ) * datu_tmp(i  ,j  ) &
               & + 2.0d0 * mask_org(i+1,j  ) * datu_tmp(i+1,j  ) &
               & + 2.0d0 * mask_org(i  ,j+1) * datu_tmp(i  ,j+1) &
               & + 2.0d0 * mask_org(i-1,j  ) * datu_tmp(i-1,j  ) &
               & + 2.0d0 * mask_org(i  ,j-1) * datu_tmp(i  ,j-1) &
               & + 1.0d0 * mask_org(i+1,j+1) * datu_tmp(i+1,j+1) &
               & + 1.0d0 * mask_org(i-1,j+1) * datu_tmp(i-1,j+1) &
               & + 1.0d0 * mask_org(i+1,j-1) * datu_tmp(i+1,j-1) &
               & + 1.0d0 * mask_org(i-1,j-1) * datu_tmp(i-1,j-1)
          hl3 =   12.0d0 * mask_org(i  ,j  ) * datv_tmp(i  ,j  ) &
               & + 2.0d0 * mask_org(i+1,j  ) * datv_tmp(i+1,j  ) &
               & + 2.0d0 * mask_org(i  ,j+1) * datv_tmp(i  ,j+1) &
               & + 2.0d0 * mask_org(i-1,j  ) * datv_tmp(i-1,j  ) &
               & + 2.0d0 * mask_org(i  ,j-1) * datv_tmp(i  ,j-1) &
               & + 1.0d0 * mask_org(i+1,j+1) * datv_tmp(i+1,j+1) &
               & + 1.0d0 * mask_org(i-1,j+1) * datv_tmp(i-1,j+1) &
               & + 1.0d0 * mask_org(i+1,j-1) * datv_tmp(i+1,j-1) &
               & + 1.0d0 * mask_org(i-1,j-1) * datv_tmp(i-1,j-1)
          if (hl1 > 0.0d0) then
            datu_smt(i,j) = hl2 / hl1
            datv_smt(i,j) = hl3 / hl1
          end if
        end if
      end do
    end do
    
  end do LOOP_ITER

  where(mask_org(1:imf,1:jmf)==0.d0) datu_smt(1:imf,1:jmf) = real(undef_out,4)
  where(mask_org(1:imf,1:jmf)==0.d0) datv_smt(1:imf,1:jmf) = real(undef_out,4)

  write(6,*) 'DATA written to ',trim(fl_ot)
  open(mtot1,file=trim(fl_ot),form='unformatted',access='direct',recl=4*imf*jmf)
  write(mtot1,rec=1) real(datu_smt(1:imf,1:jmf),4)
  write(mtot1,rec=2) real(datv_smt(1:imf,1:jmf),4)
  close(mtot1)

  !--------------------------------------------------------------------

  deallocate(datu_org, datv_org, dat4_org, mask_org)

end program globcurrent_smooth
