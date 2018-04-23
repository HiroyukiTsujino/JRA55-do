!-*-F90-*-
!uv2w.F90
!====================================================
!
! Diagnose W at TS-Grid from u, v, and ssh
!
!====================================================
program uv2w
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ksgm, dz
  !
  use oc_structure, only : &
  &   read_topo,           & !------------------
  &   dep,                 & ! UVTSボックス境界（上面）の深さ
  &   aexl, atexl,         & ! 海陸インデックス
  &   coefx, coefy,        &
  &   dzu,                 & ! UVボックスの厚さ
  &   thcksgm, thcksgmr,   &
  &   ktbtm,               &
#ifdef OGCM_BBL
  &   aexlbbl, atexlbbl,   & ! BBLインデックス
#endif /* OGCM_BBL */
#ifdef OGCM_SPHERICAL
  &   set_hgrids,          &
  &   calc_scale,          & !------------------
#else /* OGCM_SPHERICAL */
  &   read_scale,          & !------------------
#endif /* OGCM_SPHERICAL */
  &   a_tl  , a_tr  ,      & ! 格子面積
  &   a_bl  , a_br  ,      &
  &   dx_tl , dx_tr ,      & ! 東西長
  &   dx_bl , dx_br ,      &
  &   dy_tl , dy_tr ,      & ! 南北長
  &   dy_bl , dy_br ,      &
  &   set_area,            & !------------------
  &   areat
  !
  !----------------------------------------------
  !
  implicit none
  !
  real(4), save :: UNDEF =  0.0e0
  !
  ! 海洋モデル地形
  !
  real(8)    :: u(imut, jmut, km)
  real(8)    :: v(imut, jmut, km)
  real(8)    :: ssh(imut, jmut)
  !
  real(8)    :: umo(imut, jmut, km)
  real(8)    :: vmo(imut, jmut, km)
  real(8)    :: wlwl(imut, jmut, km)
  !
  real(4)    :: w(imut, jmut, km)
  !
  real(4)    :: d3_r4(imut,jmut,km)
  real(4)    :: d2_r4(imut,jmut)
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flin_u ! 入力ファイル
  character(len=256)    :: flin_v ! 入力ファイル
  character(len=256)    :: flin_ssh  ! 入力ファイル
  character(len=256)    :: fltopo ! 海底地形ファイル
  character(len=256)    :: flsclf ! スケールファクター・ファイル
  character(len=256)    :: flout_w ! 出力ファイル
  !
  namelist /nml_uv2w/ flin_u, flin_v, flin_ssh, flout_w, &
    &                 undef, fltopo, flsclf
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mtin_u   = 80
  integer(4), parameter :: mtin_v   = 81
  integer(4), parameter :: mtin_ssh = 83
  integer(4), parameter :: mtout_w  = 84
  !
#ifdef OGCM_BBL
  integer(4), parameter :: kmax = km-1
#else /* OGCM_BBL */
  integer(4), parameter :: kmax = km
#endif /* OGCM_BBL */
  !
  real(8)    :: ustar, vstar
  real(8)    :: hl1
  !
  integer(4) :: i, j, k
  integer(4) :: k0
  !
  !==============================================
  !
  !----------------------------------------------
  ! 入力パラメタ既定値
  !----------------------------------------------
  flin_u  = 'hs_u.d'
  flin_v  = 'hs_v.d'
  flin_ssh= 'hs_ssh.d'
  flout_w = 'w.d'
  fltopo = 'topo.d'
  flsclf = 'scale_factor.d'
  !
  !----------------------------------------------
  ! 標準入力から読み込み
  !----------------------------------------------
  read(unit=5, nml_uv2w)
  print *,'flin_u   :', trim(flin_u)
  print *,'flin_v   :', trim(flin_v)
  print *,'flin_ssh :', trim(flin_ssh)
  print *,'flout_w  :', trim(flout_w)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  print *,'UNDEF    :', undef
  !
  !----------------------------------------------
  !  地形の読み込み
  !----------------------------------------------
  !
  call read_topo(fltopo)
  !
  !----------------------------------------------
  !  スケールファクタの読み込み
  !----------------------------------------------
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
  ! 入出力ファイルオープン
  !----------------------------------------------
  open (mtin_u, file=flin_u, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_v, file=flin_v, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_ssh, file=flin_ssh, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)
  !
  !----------------------------------------------
  ! 読み込み、倍精度実数への変換
  !----------------------------------------------
  read (mtin_u, rec=1) d3_r4
  u(:,:,:) = aexl(:,:,:)*real(d3_r4(:,:,:),8)
  !
  u(1:2,         1:jmut, 1:km)=u(imut-3:imut-2, 1:jmut, 1:km)
  u(imut-1:imut, 1:jmut, 1:km)=u(3:4,           1:jmut, 1:km)
  !
  ! V
  read (mtin_v, rec=1) d3_r4
  v(:,:,:) = aexl(:,:,:)*real(d3_r4(:,:,:),8)
  !
  v(1:2,         1:jmut, 1:km)=v(imut-3:imut-2, 1:jmut, 1:km)
  v(imut-1:imut, 1:jmut, 1:km)=v(3:4,           1:jmut, 1:km)
  !-------------------------
  !
  !  SSH
  read (mtin_ssh, rec=1) d2_r4
  ssh(:,:) = atexl(:,:,1)*real(d2_r4(:,:),8)
  !
  ssh(1:2,         1:jmut)=ssh(imut-3:imut-2, 1:jmut)
  ssh(imut-1:imut, 1:jmut)=ssh(3:4,           1:jmut)
  !
  !----------------------------------------------
  close (mtin_u)
  close (mtin_v)
  close (mtin_ssh)
  !
  !----------------------------------------------
  do j = 1, jmut-1
    do i = 1, imut-1
      hl1 = aexl(i,j,1)*( a_tl(i,j)*ssh(i,j+1) + a_tr(i,j)*ssh(i+1,j+1)  &
        &                +a_bl(i,j)*ssh(i,j  ) + a_br(i,j)*ssh(i+1,j  )) &
        &   / (1.d0 -aexl(i,j,1) +a_tl(i,j) +a_tr(i,j)                   &
        &                        +a_bl(i,j) +a_br(i,j) )
      do k = 1, ksgm
        dzu(i,j,k) = dzu(i,j,k) * (thcksgm +hl1) * thcksgmr
      end do
    end do
  end do
#ifdef OGCM_CYCLIC
  dzu(imut, 1:jmut, 1:ksgm) = v(4, 1:jmut, 1:ksgm)
#endif /* OGCM_CYCLIC */
  !----------------------------------------------
  ! x-ward transport at X-point
  !----------------------------------------------
  do j = 2, jmut
    do i = 1, imut-1
      do k = 1, kmax
        ustar = dy_br(i, j  )*dzu(i, j  , k)*u(i, j  , k) &
          &    +dy_tr(i, j-1)*dzu(i, j-1, k)*u(i, j-1, k)
        umo(i,j,k) = ustar
      end do
#ifdef OGCM_BBL
      ustar = dy_br(i, j  )*dzu(i, j  , km)*u(i, j  , km) &
        &    +dy_tr(i, j-1)*dzu(i, j-1, km)*u(i, j-1, km)
      umo(i,j,km) = ustar
#endif /* OGCM_BBL */
    end do
#ifdef OGCM_CYCLIC
    umo(imut,j,1:km) = umo(4,j,1:km)
#endif /* OGCM_CYCLIC */
  end do
  !
  !----------------------------------------------
  ! y-ward transport at Y-point
  !----------------------------------------------
  do j = 1, jmut-1
    do i = 2, imut
      do k = 1, kmax
        vstar = dx_tr(i-1,j)*dzu(i-1,j,k)*v(i-1,j,k) &
          &    +dx_tl(i  ,j)*dzu(i  ,j,k)*v(i  ,j,k)
        vmo(i,j,k) = vstar
      end do
#ifdef OGCM_BBL
      vstar = dx_tr(i-1,j)*dzu(i-1,j,km)*v(i-1,j,km) &
        &    +dx_tl(i  ,j)*dzu(i  ,j,km)*v(i  ,j,km)
      vmo(i,j,km) = vstar
#endif /* OGCM_BBL */
    end do
  end do
  !
  !----------------------------------------------
  !
  wlwl(1:imut, 1:jmut, 1:km) = 0.d0
  ! upward transport across upper surface of box(k)
  do j = 2, jmut
    do i = 2, imut
      k0 = ktbtm(i, j)
      if(k0 > 1) then
#ifdef OGCM_BBL
        wlwl(i, j, k0-1) =  atexlbbl(i, j, 1) *                                &
          & ( umo(i-1, j, km)*coefx(i-1, j, km) -umo(i, j, km)*coefx(i, j, km) &
          &  +vmo(i, j-1, km)*coefy(i, j-1, km) -vmo(i, j, km)*coefy(i, j, km))&
          &              +  atexl(i, j, k0) *                                  &
          & ( umo(i-1, j, k0)*coefx(i-1, j, k0) -umo(i, j, km)*coefx(i, j, k0) &
          &  +vmo(i, j-1, k0)*coefy(i, j-1, k0) -vmo(i, j, km)*coefy(i, j, k0))
#else /* OGCM_BBL */
        wlwl(i, j, k0-1) =  atexl(i, j, k0) *                                  &
          & ( umo(i-1, j, k0)*coefx(i-1, j, k0) -umo(i, j, k0)*coefx(i, j, k0) &
          &  +vmo(i, j-1, k0)*coefy(i, j-1, k0) -vmo(i, j, k0)*coefy(i, j, k0))
#endif /* OGCM_BBL */
        if(k0 > 2) then
          !
          do k = k0-1, 2, -1
            wlwl(i, j, k-1) =  wlwl(i, j, k) + atexl(i, j, k) *                &
              & ( umo(i-1, j, k)*coefx(i-1, j, k) -umo(i, j, k)*coefx(i, j, k) &
              &  +vmo(i, j-1, k)*coefy(i, j-1, k) -vmo(i, j, k)*coefy(i, j, k))
          end do
        end if
      end if
    end do
  end do
  !
  w(1:imut, 1:jmut, 1:km) = 0.e0
  !
  do j = 2, jmut
    do i = 2, imut
      do k = 2, kmax
        hl1 = 0.5d0 * atexl(i, j, k) * (wlwl(i, j, k-1) + wlwl(i, j, k))  &
          &                     / (areat(i, j, k) +1.d0 -atexl(i, j, k))
        w(i, j, k) = real(hl1, 4)
      end do
#ifdef OGCM_BBL
      k0 = ktbtm(i, j)
      hl1 = 0.5d0 * atexl(i, j, km) * wlwl(i, j, k0-1)     &
          &      / (areat(i, j, km) +1.d0 -atexl(i, j, km))
      w(i, j, km) = real(hl1, 4)
#endif /* OGCM_BBL */
    end do
  end do
  !
  w(1:2,         1:jmut, 1:km)=w(imut-3:imut-2, 1:jmut, 1:km)
  w(imut-1:imut, 1:jmut, 1:km)=w(3:4,           1:jmut, 1:km)
  !----------------------------------------------
  where(atexl(1:imut, 1:jmut, 1:km) == 0.d0)
    w(1:imut, 1:jmut, 1:km) = undef
  end where
  !
  !==============================================
  open (mtout_w, file=flout_w, form='unformatted',&
    &  access='direct', recl=4*imut*jmut*km)
  write(mtout_w, rec=1) w(1:imut, 1:jmut, 1:km)
  close (mtout_w)
  !
!====================================================
end program uv2w
