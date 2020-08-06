!-*-F90-*-
!curltau.F90
!====================================================
!
! calculate curl tau
!
!====================================================
program curltau
  !
  use oc_mod_param, only : &
       &   imut, jmut, km, &
       &   ksgm, dz,       &
       &   para,           &
       &   param_mxe__ini
  !
  use oc_structure, only : &
       &   aexl, atexl,    &
       &   a_tl,   a_tr,   &
       &   a_bl,   a_br,   &
       &   dx_tl,  dx_tr,  &
       &   dx_bl,  dx_br,  &
       &   dy_tl,  dy_tr,  &
       &   dy_bl,  dy_br,  &
       &   areat,          &     
       &   alont,  alatt,  &     
       &   grid, topo,     &
       &   set_area_t,     &
       &   structure_mxe__ini

  use file_open_close_manager

  !----------------------------------------------
  !
  implicit none
  !
  real(8),allocatable :: taux(:,:)
  real(8),allocatable :: tauy(:,:)
  real(4),allocatable :: taux4(:,:)
  real(4),allocatable :: tauy4(:,:)
  !
  real(8),allocatable :: curl(:,:)
  real(4),allocatable :: curl4(:,:)

  real(8),allocatable :: mask_coast(:,:)
  real(8),allocatable :: mask_miss(:,:)
  !
  real(4), parameter :: UNDEF = -9.99e33
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flinx    ! 入力ファイル
  character(len=256)    :: fliny    ! 入力ファイル
  character(len=256)    :: flout    ! 出力ファイル
  character(len=256)    :: file_namelist_curltau
  integer(4) :: irecx
  integer(4) :: irecy
  !
  logical :: l_units_cgs
  logical :: l_nohalo_cyclic
  logical :: l_out_latlon
  logical :: l_mask_coast
  logical :: l_mask_miss

  integer(4) :: mtin, mtnam
  integer(4) :: mtot
  integer(4) :: lreclen

  integer(4) :: ios       !  入出力エラーチェック用
  integer(4) :: i, j
  !
  !==============================================

  namelist /nml_curltau/ flinx, fliny, irecx, irecy, &
       & l_units_cgs, l_mask_coast, l_mask_miss, flout, l_nohalo_cyclic, l_out_latlon

  !---------------------------------------------
  ! 入力パラメタ既定値
  !
  flinx = 'hs_taux.d'
  fliny = 'hs_tauy.d'
  flout = 'curl_tau.d'
  l_units_cgs = .false.
  l_nohalo_cyclic = .false.
  l_mask_coast = .true.
  l_mask_miss = .true.

  ! 標準入力から読み込み

  file_namelist_curltau = 'namelist.curl_tau'
  l_out_latlon = .false.

  call open_file_plain(mtnam,file_namelist_curltau)
  read(unit=mtnam, nml=nml_curltau)
  call close_file(mtnam)

  write(*,*) 'flinx    :', trim(flinx)
  write(*,*) 'fliny    :', trim(fliny)
  write(*,*) 'flout    :', trim(flout)
  write(*,*) 'l_nohalo_cyclic    :', l_nohalo_cyclic

  !----------------------------------------------
  ! 海洋モデル格子情報等の準備

  call param_mxe__ini
  call structure_mxe__ini
  call set_area_t

  allocate(taux(1:imut,1:jmut))
  allocate(tauy(1:imut,1:jmut))
  allocate(taux4(1:imut,1:jmut))
  allocate(tauy4(1:imut,1:jmut))

  allocate(curl(1:imut,1:jmut))
  allocate(curl4(1:imut,1:jmut))

  allocate(mask_coast(1:imut,1:jmut))
  allocate(mask_miss(1:imut,1:jmut))

  !----------------------------------------------

  mask_coast(1:imut,1:jmut) = 0.0d0

  do j = 2, jmut
    do i = 2, imut
      mask_coast(i,j) = aexl(i,j,1) * aexl(i-1,j,1) * aexl(i,j-1,1) * aexl(i-1,j-1,1)
    end do
  end do

  !==============================================
  ! 入力

  lreclen = 4*imut*jmut
  
  call open_file_direct(mtin, flinx, lreclen, action='read')
  read (mtin, rec=irecx, iostat=ios) taux4
  if(ios /= 0) write(*, *) 'reading error in file:', trim(flinx)
  call close_file(mtin)

  mask_miss(:,:) = 0.d0
  do j = 2, jmut
    do i = 2, imut
      if ((taux4(i,j) /= undef) .and. (taux4(i-1,j) /= undef) &
           & .and. (taux4(i,j-1) /= undef) .and. (taux4(i-1,j-1) /= undef)) then
        mask_miss(i,j) = 1.d0
      end if
    end do
  end do
  if (l_nohalo_cyclic) then
    do j = 2, jmut
      if ((taux4(1,j) /= undef) .and. (taux4(imut,j) /= undef) &
           & .and. (taux4(1,j-1) /= undef) .and. (taux4(imut,j-1) /= undef)) then
        mask_miss(1,j) = 1.d0
      end if
    end do
  end if

  where (taux4(:,:) == undef) 
    taux4(:,:) = 0.d0
  end where

  if (l_units_cgs) then
    taux(:,:) = aexl(:,:,1)*real(taux4(:,:),8) * 1.0d-1 ! ---> MKS
  else
    taux(:,:) = aexl(:,:,1)*real(taux4(:,:),8)
  end if
  if (para%lcyclic) then
    taux(1:2, 1:jmut)=taux(imut-3:imut-2, 1:jmut)
  end if
  !
  call open_file_direct(mtin, fliny, lreclen, action='read')
  read (mtin, rec=irecy, iostat=ios) tauy4
  if(ios /= 0) write(*, *) 'reading error in file:', mtin
  call close_file(mtin)

  where (tauy4(:,:) == undef) 
    tauy4(:,:) = 0.d0
  end where

  if (l_units_cgs) then
    tauy(:,:) = aexl(:,:,1)*real(tauy4(:,:),8) * 1.0d-1 ! ----> MKS
  else
    tauy(:,:) = aexl(:,:,1)*real(tauy4(:,:),8)
  end if

  if (para%lcyclic) then
    tauy(1:2, 1:jmut)=tauy(imut-3:imut-2, 1:jmut)
  end if

  !---------------------------------------------------
  !
  ! calculate curl tau
  !
  curl(1:imut,1:jmut) = 0.0d0

  call cal_curl_tau
  !
  !---------------------------------------------------
  !
  ! 出力
  !
  curl4(:,:) = real(curl(:,:),4)
  if (l_mask_coast) then
    where(mask_coast(:,:) == 0.0d0)
      curl4(:,:) = UNDEF
    end where
  else
    where(atexl(:,:,1) == 0.0d0)
      curl4(:,:) = UNDEF
    end where
  end if
  !
  if (l_mask_miss) then
    where(mask_miss(:,:) == 0.0d0)
      curl4(:,:) = UNDEF
    end where
  end if

  call open_file_direct(mtot, flout, lreclen, action='write')
  write (mtot, rec=1, iostat=ios) curl4
  if(ios /= 0) write(*, *) 'writing error in file:', trim(flout)
  !
  call close_file(mtot)
  !
  if (l_out_latlon) then
    write(6,*) ' Write latitude-longitude to unit 11'
    write(11,'(8f10.3)') (alont(i),i=1,jmut)
    write(11,'(8f10.3)') (alatt(j),j=1,jmut)
  end if

contains
!====================================================
!
!  calculate curl tau
!
!====================================================
subroutine cal_curl_tau
  !
  integer(4) :: i, j
  !
  do j = 2, jmut
    do i = 2, imut
      curl(i, j) = atexl(i, j, 1) * 1.0d2 *                             &
        & (-taux(i-1, j  )*dx_tr(i-1, j  ) -taux(i, j  )*dx_tl(i, j  )  &
        &  -tauy(i-1, j  )*dy_br(i-1, j  ) +tauy(i, j  )*dy_br(i, j  )  &
        &  -tauy(i-1, j-1)*dy_tr(i-1, j-1) +tauy(i, j-1)*dy_tr(i, j-1)  &
        &  +taux(i-1, j-1)*dx_tr(i-1, j-1) +taux(i, j-1)*dx_tl(i, j-1)) &
        & / (areat(i,j,1) +1.0d0 -atexl(i,j,1))
    end do
  end do

  if (l_nohalo_cyclic) then
    do j = 2, jmut
      atexl(1,j,1) = max(aexl(imut,j,1), aexl(1,j,1), aexl(imut,j-1,1), aexl(1,j-1,1))
    end do
    do j = 2, jmut
      areat(1, j, 1) = atexl(1, j, 1) *                                &
           & ( aexl(imut,j  ,1) *a_br(imut,j  ) +aexl(1,j  ,1) *a_bl(1,j  ) &
           &  +aexl(imut,j-1,1) *a_tr(imut,j-1) +aexl(1,j-1,1) *a_tl(1,j-1))
    end do
    do j = 2, jmut
      curl(1,j) = atexl(1, j, 1) * 1.0d2 *                             &
        & (-taux(imut, j  )*dx_tr(imut, j  ) -taux(1, j  )*dx_tl(1, j  )  &
        &  -tauy(imut, j  )*dy_br(imut, j  ) +tauy(1, j  )*dy_br(1, j  )  &
        &  -tauy(imut, j-1)*dy_tr(imut, j-1) +tauy(1, j-1)*dy_tr(1, j-1)  &
        &  +taux(imut, j-1)*dx_tr(imut, j-1) +taux(1, j-1)*dx_tl(1, j-1)) &
        & / (areat(1,j,1) +1.0d0 -atexl(1,j,1))
    end do
  end if
  !
end subroutine cal_curl_tau
!====================================================
end program curltau
