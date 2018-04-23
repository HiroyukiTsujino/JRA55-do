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
  real(8) :: taux(1:imut, 1:jmut)
  real(8) :: tauy(1:imut, 1:jmut)
  real(4) :: taux4(1:imut, 1:jmut)
  real(4) :: tauy4(1:imut, 1:jmut)
  !
  real(8) :: curl(1:imut, 1:jmut)
  real(4) :: curl4(1:imut, 1:jmut)
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
  namelist /nml_curltau/ fltopo, flsclf, flin, flout
  !
  integer(4), parameter :: mttmp    = 80
  integer(4), parameter :: mtin     = 81
  integer(4), parameter :: mtin_ssh = 82
  integer(4), parameter :: mtout    = 83
  integer(4) :: ios       !  入出力エラーチェック用
  !
  !==============================================
  !
  ! 入力パラメタ既定値
  !
  fltopo = 'topo.d'
  flsclf = 'scale_factor.d'
  flin   = 'hs_tau_sfc.d'
  flout  = 'curl_tau.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml_curltau)
  write(*,*) 'fltopo   :', trim(fltopo)
  write(*,*) 'flsclf   :', trim(flsclf)
  write(*,*) 'flin     :', trim(flin)
  write(*,*) 'flout    :', trim(flout)
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
  read (mtin, rec=1, iostat=ios) taux4
  if(ios /= 0) write(*, *) 'reading error in file:', mtin
  taux(:,:) = aexl(:,:,1)*real(taux4(:,:),8)
  taux(1:2, 1:jmut)=taux(imut-3:imut-2, 1:jmut)
  !
  read (mtin, rec=2, iostat=ios) tauy4
  if(ios /= 0) write(*, *) 'reading error in file:', mtin
  tauy(:,:) = aexl(:,:,1)*real(tauy4(:,:),8)
  tauy(1:2, 1:jmut)=tauy(imut-3:imut-2, 1:jmut)
  !
  close (mtin)
  !
  !---------------------------------------------------
  !
  ! calculate curl tau
  !
  call cal_curl_tau
  !
  !---------------------------------------------------
  !
  ! 出力
  !
  curl4(:,:) = real(curl(:,:),4)
  where(atexl(:,:,1) == 0.0d0)
    curl4(:,:) = UNDEF
  end where
  !
  open (mtout, file=flout, form='unformatted', &
    &     access='direct', recl=4*imut*jmut)
  !
  write (mtout, rec=1, iostat=ios) curl4
  if(ios /= 0) write(*, *) 'writing error in file:', mtout
  !
  close ( mtout )
  !
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
  !
end subroutine cal_curl_tau
!====================================================
end program curltau
