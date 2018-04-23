!hs2uvmo.F90
!====================================================
!
! Vertically Integrated X-ward and Y-ward Heat Transport
!
!====================================================
program uvmo
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ksgm, dz,            &
  &   ro
  !
  use oc_structure, only : &
  &   read_topo,           & !------------------
  &   dep,                 & ! UVTSボックス境界（上面）の深さ
  &   ho4, exnn,           & ! 水深、層数
  &   aexl, atexl,         & ! 海陸インデックス
  &   coefx, coefy,        &
  &   dzu,                 & ! UVボックスの厚さ
  &   thcksgm, thcksgmr,   &
#ifdef OGCM_BBL
  &   ho4bbl, exnnbbl,     & ! BBL層厚、層数
  &   aexlbbl, atexlbbl,   & ! BBLインデックス
  &   kbtm,                &
#endif /* OGCM_BBL */
  &   read_scale,          & !------------------
  &   a_tl  , a_tr  ,      & ! 格子面積
  &   a_bl  , a_br  ,      &
  &   dx_tl , dx_tr ,      & ! 東西長
  &   dx_bl , dx_br ,      &
  &   dy_tl , dy_tr ,      & ! 南北長
  &   dy_bl , dy_br
  !
  !----------------------------------------------
  !
  implicit none
  !
  real(8), parameter :: unit_kgs = ro * 1.d-3 ! [cm^3/s] > [kg/s]
  real(4), parameter :: UNDEF0 =  0.0e0
  !
  ! 海洋モデル地形
  !
  real(8)    :: u(imut, jmut, km)
  real(8)    :: v(imut, jmut, km)
  real(8)    :: ssh(imut, jmut)
  !
  real(8)    :: umo(imut, jmut, km)
  real(8)    :: vmo(imut, jmut, km)
  real(4)    :: umo4(imut, jmut, km)
  real(4)    :: vmo4(imut, jmut, km)
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
  character(len=256)    :: flout_umo ! 出力ファイル
  character(len=256)    :: flout_vmo ! 出力ファイル
  !
  namelist /nml_uvmo/ flin_u, flin_v, flin_ssh, &
    &                 fltopo, flsclf,           &
    &                 flout_umo, flout_vmo
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mttmp    = 79
  integer(4), parameter :: mtin_u   = 80
  integer(4), parameter :: mtin_v   = 81
  integer(4), parameter :: mtin_ssh = 83
  integer(4), parameter :: mtout_umo= 84
  integer(4), parameter :: mtout_vmo= 85
  !
  real(8)    :: wrk1, wrk2
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
  !
  !==============================================
  !
  !----------------------------------------------
  ! 入力パラメタ既定値
  !----------------------------------------------
  flin_u  = 'hs_u.d'
  flin_v  = 'hs_v.d'
  flin_ssh= 'hs_ssh.d'
  fltopo = 'topo.d'
  flsclf = 'scale_factor.d'
  flout_umo = 'umo.d'
  flout_umo = 'vmo.d'
  !
  !----------------------------------------------
  ! 標準入力から読み込み
  !----------------------------------------------
  read(unit=5, nml_uvmo)
  print *,'flin_u   :', trim(flin_u)
  print *,'flin_v   :', trim(flin_v)
  print *,'flin_ssh :', trim(flin_ssh)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  print *,'flout_umo:', trim(flout_umo)
  print *,'flout_vmo:', trim(flout_vmo)
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
  call read_scale(flsclf)
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
  ! x方向の熱輸送を鉛直積算
  !----------------------------------------------
  do j = 2, jmut
    do i = 1, imut-1
      hl1 = 1.d0 /(dx_bl(i, j)+dx_br(i, j))
      do k = 1, ksgm
        ustar = dy_br(i, j  )*dzu(i, j  , k)*u(i, j  , k) &
          &    +dy_tr(i, j-1)*dzu(i, j-1, k)*u(i, j-1, k)
        ustar = ustar*(thcksgm+hl1*(dx_br(i, j)*ssh(i, j)+dx_bl(i, j)*ssh(i+1, j)))*thcksgmr
        umo(i,j,k) = ustar
      end do
      do k = ksgm+1, kmax
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
  ! y方向の熱輸送を鉛直積算
  !----------------------------------------------
  do j = 1, jmut-1
    do i = 2, imut
      hl1 = 1.d0/(dy_bl(i, j)+dy_tl(i, j))
      do k = 1, ksgm
        vstar = dx_tr(i-1,j)*dzu(i-1,j,k)*v(i-1,j,k) &
          &    +dx_tl(i  ,j)*dzu(i  ,j,k)*v(i  ,j,k)
        vstar = vstar*(thcksgm+hl1*(dy_tl(i, j)*ssh(i, j)+dy_bl(i, j)*ssh(i, j+1)))*thcksgmr
        vmo(i,j,k) = vstar
      end do
      do k = ksgm+1, kmax
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
  ! 単位変換、単精度変換して書き込み
  !----------------------------------------------
  umo(1:imut, 1:jmut, 1:km) = coefx(1:imut, 1:jmut, 1:km) * unit_kgs  &
    &                         * umo(1:imut, 1:jmut, 1:km)
  umo4(1:imut, 1:jmut, 1:km) = real(umo(1:imut, 1:jmut, 1:km), 4)
  !
  do i = 3, imut-2
    umo4(i, jmut  , 1:km) = - umo4(imut-i, jmut-4, 1:km)
    umo4(i, jmut-1, 1:km) = - umo4(imut-i, jmut-3, 1:km)
  end do
  !
  vmo(1:imut, 1:jmut, 1:km) = coefy(1:imut, 1:jmut, 1:km) * unit_kgs  &
    &                         * vmo(1:imut, 1:jmut, 1:km)
  vmo4(1:imut, 1:jmut, 1:km) = real(vmo(1:imut, 1:jmut, 1:km), 4)
  !
  do i = 3, imut-2
    vmo4(i, jmut  , 1:km) = - vmo4(imut-i+2, jmut-5, 1:km)
    vmo4(i, jmut-1, 1:km) = - vmo4(imut-i+2, jmut-4, 1:km)
  end do
  !
  !============================================
  open (mtout_umo, file=flout_umo, form='unformatted',&
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtout_vmo, file=flout_vmo, form='unformatted',&
    &  access='direct', recl=4*imut*jmut*km)
  !
  !--------------------------------------------
  write(mtout_umo, rec=1) umo4(1:imut, 1:jmut, 1:km)
  write(mtout_vmo, rec=1) vmo4(1:imut, 1:jmut, 1:km)
  !----------------------------------------------
  close (mtout_umo)
  close (mtout_vmo)
  !
!====================================================
end program uvmo
