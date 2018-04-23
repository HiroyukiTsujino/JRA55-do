!uv2zeta.F90
!====================================================
!
! calculate relative vorticity
!
!====================================================
program uv2zeta
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ksgm, dz
  !
  use oc_structure, only :    &
  &   read_topo,              & !-----------------------
  &   aexl, atexl,            & ! 海陸インデックス
#ifdef OGCM_SPHERICAL
  &   set_hgrids,             &
  &   calc_scale,             &
#else /* OGCM_SPHERICAL */
  &   read_scale,             &
#endif /* OGCM_SPHERICAL */
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
  real(8) :: u(imut, jmut, km)
  real(8) :: v(imut, jmut, km)
  real(4) :: u4(imut, jmut, km)
  real(4) :: v4(imut, jmut, km)
  !
  real(8) :: zeta(imut, jmut, km)
  real(4) :: zeta4(imut, jmut, km)
  !
  real(4), save :: undef = -9.99e33
  !
  ! 入出力ファイル
  !
  character(len=256)    :: fltopo   ! 海底地形ファイル
  character(len=256)    :: flsclf   ! スケールファクター
  character(len=256)    :: fluin
  character(len=256)    :: flvin
  character(len=256)    :: flout    ! 出力ファイル
  !
  namelist /nml_uv2zeta/ fltopo, flsclf, undef, fluin, flvin, flout
  !
  integer(4), parameter :: mttmp    = 80
  integer(4), parameter :: mtuin     = 81
  integer(4), parameter :: mtvin     = 82
  integer(4), parameter :: mtout    = 84
  integer(4) :: ios       !  入出力エラーチェック用
  !
  !==============================================
  !
  ! 入力パラメタ既定値
  !
  fltopo = 'topo.d'
  flsclf = 'scale_factor.d'
  fluin  = 'hs_u.d'
  flvin  = 'hs_v.d'
  flout  = 'zeta.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml_uv2zeta)
  write(*,*) 'fltopo   :', trim(fltopo)
#ifndef OGCM_SPHERICAL
  write(*,*) 'flsclf   :', trim(flsclf)
#endif /* OGCM_SPHERICAL */
  write(*,*) 'UNDEF    :', undef
  write(*,*) 'fluin    :', trim(fluin)
  write(*,*) 'flvin    :', trim(flvin)
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
  !  スケールファクタ
  !
#ifdef OGCM_SPHERICAL
  call set_hgrids
  call calc_scale
#else /* OGCM_SPHERICAL */
  call read_scale(flsclf)
#endif /* OGCM_SPHERICAL */
  !
  call set_area
  !
  !==============================================
  !
  ! 入力
  !
  open (mtuin, file=fluin, form='unformatted', &
    &     access='direct', recl=4*imut*jmut*km)
  read (mtuin, rec=1, iostat=ios) u4
  if(ios /= 0) write(*, *) 'reading error in file:', mtuin
  u(:,:,:) = aexl(:,:,:)*real(u4(:,:,:),8)
  u(1:2, 1:jmut, 1:km)=u(imut-3:imut-2, 1:jmut, 1:km)
  close (mtuin)
  !
  open (mtvin, file=flvin, form='unformatted', &
    &     access='direct', recl=4*imut*jmut*km)
  read (mtvin, rec=1, iostat=ios) v4
  if(ios /= 0) write(*, *) 'reading error in file:', mtvin
  v(:,:,:) = aexl(:,:,:)*real(v4(:,:,:),8)
  v(1:2, 1:jmut, 1:km)=v(imut-3:imut-2, 1:jmut, 1:km)
  close (mtvin)
  !
  !---------------------------------------------------
  !
  ! calculate relative vorticity
  !
  call cal_zeta
  !
  !---------------------------------------------------
  !
  ! 出力
  !
  zeta4(1:imut, 1:jmut, 1:km) = real(zeta(1:imut, 1:jmut, 1:km),4)
  where(atexl(1:imut, 1:jmut, 1:km) == 0.0d0)
    zeta4(1:imut, 1:jmut, 1:km) = undef
  end where
  !
  open (mtout, file=flout, form='unformatted', &
    &     access='direct', recl=4*imut*jmut*km)
  !
  write (mtout, rec=1, iostat=ios) zeta4
  if(ios /= 0) write(*, *) 'writing error in file:', mtout
  !
  close ( mtout )
  !
contains
!====================================================
!
!  calculate relative vorticity
!
!====================================================
subroutine cal_zeta
  !
  integer(4) :: i, j, k
  !
  zeta(1:imut, 1:jmut, 1:km) =0.0d0
  !
  do k = 1, km
    do j = 2, jmut
      do i = 2, imut
        zeta(i, j, k) = atexl(i, j, k) *                                  &
          & (-u(i-1, j  , k)*dx_tr(i-1, j  ) -u(i, j  , k)*dx_tl(i, j  )  &
          &  -v(i-1, j  , k)*dy_br(i-1, j  ) +v(i, j  , k)*dy_br(i, j  )  &
          &  -v(i-1, j-1, k)*dy_tr(i-1, j-1) +v(i, j-1, k)*dy_tr(i, j-1)  &
          &  +u(i-1, j-1, k)*dx_tr(i-1, j-1) +u(i, j-1, k)*dx_tl(i, j-1)) &
          & / (areat(i, j, k) +1.0d0 -atexl(i, j, k))
      end do
    end do
  end do
#ifdef OGCM_CYCLIC
  zeta(1,1:jmut,1:km) =  zeta(imut-4,1:jmut,1:km)
#endif /* OGCM_CYCLIC */
  !
end subroutine cal_zeta
!====================================================
end program uv2zeta
