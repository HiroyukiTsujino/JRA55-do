!-*-F90-*-
!divice.F90
!====================================================
!
! calculate ice divergence in MKS units
!
!====================================================
program divergence_ice
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ksgm, dz
  !
  use oc_structure, only :    &
  &   read_topo,              & !-----------------------
  &   aexl, atexl,            & ! 海陸インデックス
  &   read_scale,             & !-----------------------
  &   a_tl,   a_tr,           & ! 格子面積
  &   a_bl,   a_br,           &
  &   dx_tl,  dx_tr,          & ! 東西長
  &   dx_bl,  dx_br,          &
  &   dy_tl,  dy_tr,          & ! 南北長
  &   dy_bl,  dy_br,          &
  &   set_area,               & !-----------------------
  &   areat
  !
  !----------------------------------------------
  !
  implicit none
  !
  real(8) :: massfx (1:imut, 1:jmut)
  real(8) :: massfy (1:imut, 1:jmut)
  real(4) :: massfx4(1:imut, 1:jmut)
  real(4) :: massfy4(1:imut, 1:jmut)
  !
  integer(4) :: irecu, irecv

  real(8) :: divg (1:imut, 1:jmut)
  real(4) :: divg4(1:imut, 1:jmut)
  !
  real(4), parameter :: UNDEF = -9.99e33
  !
  ! 入出力ファイル
  !
  character(len=256)    :: fltopo   ! 海底地形ファイル
  character(len=256)    :: flsclf   ! スケールファクター
  character(len=256)    :: flin     ! 入力ファイル
  character(len=256)    :: flout    ! 出力ファイル
  !
  namelist /nml_divice/ fltopo, flsclf, flin, flout, irecu, irecv
  !
  integer(4), parameter :: mttmp    = 80
  integer(4), parameter :: mtin     = 81
  integer(4), parameter :: mtin_ssh = 82
  integer(4), parameter :: mtout    = 83
  integer(4) :: ios       !  入出力エラーチェック用
  !
  integer(4) :: i, j

  !==============================================
  !
  ! 入力パラメタ既定値
  !
  fltopo = 'topo.d'
  flsclf = 'scale_factor.d'
  flin   = 'hs_icecat_uv.d'
  flout  = 'div_ice.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml_divice)
  write(*,*) 'fltopo   :', trim(fltopo)
  write(*,*) 'flsclf   :', trim(flsclf)
  write(*,*) 'flin     :', trim(flin)
  write(*,*) 'flout    :', trim(flout)
  write(*,*) 'irecu    :', irecu
  write(*,*) 'irecv    :', irecv
  !
  !----------------------------------------------
  !
  !  地形の読み込み、海陸インデックス、層厚設定
  !
  call read_topo(fltopo)
  !
  !----------------------------------------------
  !
  !  スケールファクタの読み込み
  !
  call read_scale(flsclf)
  !
  call set_area
  !
  !==============================================
  !
  ! 入力
  !
  open (mtin, file=flin, form='unformatted', &
    &     access='direct', recl=4*imut*jmut)
  !
  read (mtin, rec=irecu, iostat=ios) massfx4
  if(ios /= 0) write(*, *) 'reading error in file:', mtin
  do j = 1, jmut
    do i = 1, imut
      massfx(i,j) = aexl(i,j,1) * real(massfx4(i,j),8) &
           & / (dy_br(i,j) + dy_tr(i,j)) * 1.0d2 ! MKS
    end do
  end do

  massfx(1:2, 1:jmut) = massfx(imut-3:imut-2, 1:jmut)
  !
  read (mtin, rec=irecv, iostat=ios) massfy4
  if(ios /= 0) write(*, *) 'reading error in file:', mtin
  do j = 1, jmut
    do i = 1, imut
      massfy(i,j) = aexl(i,j,1) * real(massfy4(i,j),8) &
           & / (dx_tl(i,j) + dx_tr(i,j)) * 1.0d2 ! MKS
    end do
  end do

  massfy(1:2, 1:jmut) = massfy(imut-3:imut-2, 1:jmut)
  !
  close (mtin)
  !
  !---------------------------------------------------
  !
  ! calculate ice divergence
  !
  call cal_div_ice
  !
  !---------------------------------------------------
  !
  ! 出力
  !
  divg4(:,:) = real(divg(:,:),4)
  where(atexl(:,:,1) == 0.0d0)
    divg4(:,:) = UNDEF
  end where
  !
  open (mtout, file=flout, form='unformatted', &
    &     access='direct', recl=4*imut*jmut)
  !
  write (mtout, rec=1, iostat=ios) divg4
  if(ios /= 0) write(*, *) 'writing error in file:', mtout
  !
  close ( mtout )
  !
contains
!====================================================
!
!  calculate divergence ice
!
!====================================================
subroutine cal_div_ice
  !
  integer(4) :: i, j
  !
  do j = 2, jmut
    do i = 2, imut
      divg(i, j) = atexl(i, j, 1) * 1.0d2 *                             &     ! MKS
        & ( massfy(i-1, j  )*dx_tr(i-1, j  ) +massfy(i, j  )*dx_tl(i, j  )  &
        &  -massfx(i-1, j  )*dy_br(i-1, j  ) +massfx(i, j  )*dy_br(i, j  )  &
        &  -massfx(i-1, j-1)*dy_tr(i-1, j-1) +massfx(i, j-1)*dy_tr(i, j-1)  &
        &  -massfy(i-1, j-1)*dx_tr(i-1, j-1) -massfy(i, j-1)*dx_tl(i, j-1)) &
        & / (areat(i,j,1) + 1.0d0 - atexl(i,j,1))
    end do
  end do
  !
end subroutine cal_div_ice
!====================================================
end program divergence_ice
