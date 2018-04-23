!btsf.F90
!====================================================
!
! BaroTropic StreamFunction
!
!====================================================
program btsf
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ksgm, dz,            &
  &   ro,    cp
  !
  use oc_structure, only :    &
  &   dep,                    & ! UVTSボックス境界（上面）の深さ
  &   thcksgm,                & ! 初期のsigma layer 全体の厚さ 
  &   thcksgmr,               & ! 1/thcksgm
  &   aexl, atexl,            & ! 海陸インデックス
  &   dzu,                    & ! UVボックスの厚さ
  &   dx_bl , dx_br ,         &
  &   dy_tr , dy_br ,         &
  &   read_topo,              &
  &   read_scale
  !
  !----------------------------------------------
  !
  implicit none
  !
  real(8), parameter :: unit_cgs2sv = 1.d-12 ! [  cm^3 / s]   -> [Sv]
  real(4), parameter :: UNDEF = -9.99e33
  !
  real(8)    :: u(imut, jmut, km)
  real(8)    :: ssh(imut, jmut)
  !
  real(8)    :: um(imut, jmut)  ! x-ward transport at ustar-point
  real(8)    :: usf(imut, jmut) ! streamfunction at u-point
  real(4)    :: usf4(imut, jmut)
  !
  real(4)    :: d3_r4(imut,jmut,km)
  real(4)    :: d2_r4(imut,jmut)
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flin_u ! 入力ファイル
  character(len=256)    :: flin_ssh  ! 入力ファイル
  character(len=256)    :: fltopo ! 海底地形ファイル
  character(len=256)    :: flsclf ! スケールファクター・ファイル
  character(len=256)    :: flout  ! 出力ファイル
  !
  namelist /nml_btsf/ flin_u, flin_ssh, fltopo, flsclf, flout
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mtin_u   = 80
  integer(4), parameter :: mtin_ssh = 83
  integer(4), parameter :: mtout    = 84
  !
#ifdef OGCM_BBL
  integer(4) :: kbtm
  integer(4), parameter :: kmax = km-1
#else /* OGCM_BBL */
  integer(4), parameter :: kmax = km
#endif /* OGCM_BBL */
  !
  real(8)    :: ustar
  real(8)    :: wrk1, wrk2
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
  flin_ssh= 'hs_ssh.d'
  fltopo  = 'topo.d'
  flsclf  = 'scale_factor.d'
  flout   = 'btsf.d'
  !
  !----------------------------------------------
  ! 標準入力から読み込み
  !----------------------------------------------
  read(unit=5, nml_btsf)
  print *,'flin_u   :', trim(flin_u)
  print *,'flin_ssh :', trim(flin_ssh)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  print *,'flout    :', trim(flout)
  !
  !----------------------------------------------
  !  地形の読み込み, 層厚関連定数, 海陸インデックス
  !----------------------------------------------
  call read_topo(fltopo)
  !
  !----------------------------------------------
  !  スケールファクタの読み込み
  !----------------------------------------------
  call read_scale(flsclf)
  !
  !==============================================
  ! 入出力ファイルオープン
  !----------------------------------------------
  open (mtin_u, file=flin_u, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_ssh, file=flin_ssh, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)
  !
  open (mtout, file=flout, form='unformatted',&
    &  access='direct', recl=4*imut*jmut)
  !
  !----------------------------------------------
  ! 読み込み、倍精度実数への変換
  !----------------------------------------------
  read (mtin_u, rec=1,iostat=ios) d3_r4
  if(ios /= 0) write(*, *) 'reading error in file:', trim(flin_u)
  u(:,:,:) = aexl(:,:,:)*real(d3_r4(:,:,:),8)
  !
  !-------------------------
  read (mtin_ssh, rec=1,iostat=ios) d2_r4
  ssh(:,:) = atexl(:,:,1)*real(d2_r4(:,:),8)
  if(ios /= 0) write(*, *) 'reading error in file:', trim(flin_ssh)
  !
  !----------------------------------------------
  ! x方向の輸送を鉛直積算, 順圧流線関数を計算
  !----------------------------------------------
  usf(1:imut,1) = 0.0d0
  do j = 2, jmut
    do i = 1, imut-1
      hl1 = 1.d0 /(dx_bl(i, j)+dx_br(i, j))
      do k = 1, ksgm
        ustar = dy_br(i, j  )*dzu(i, j  , k)*u(i, j  , k) &
          &    +dy_tr(i, j-1)*dzu(i, j-1, k)*u(i, j-1, k)
        ustar = ustar*(thcksgm+hl1*(dx_br(i, j)*ssh(i, j)+dx_bl(i, j)*ssh(i+1, j)))*thcksgmr
        !
        um(i,j) = um(i,j) +ustar
      end do
      do k = ksgm+1, kmax
        ustar = dy_br(i, j  )*dzu(i, j  , k)*u(i, j  , k) &
          &    +dy_tr(i, j-1)*dzu(i, j-1, k)*u(i, j-1, k)
        !
        um(i,j) = um(i,j) +ustar
      end do
#ifdef OGCM_BBL
      ustar = dy_br(i, j  )*dzu(i, j  , km)*u(i, j  , km) &
        &    +dy_tr(i, j-1)*dzu(i, j-1, km)*u(i, j-1, km)
      !
      um(i,j) = um(i,j) +ustar
#endif /* OGCM_BBL */
      usf(i,j) = usf(i,j-1) -um(i,j)
    end do
    usf(imut,j) = usf(4,j)
  end do
  !----------------------------------------------
  ! 単位変換、単精度変換して書き込み
  !----------------------------------------------
  usf4(1:imut,1:jmut) = real(usf(1:imut,1:jmut)*unit_cgs2sv, 4)
  where(aexl(1:imut, 1:jmut, 1) == 0.d0)
    usf4(1:imut, 1:jmut) = UNDEF
  end where
  !
  !--------------------------------------------
  write(mtout, rec=1,iostat=ios) usf4(1:imut, 1:jmut)
  if(ios /= 0) write(*, *) 'writing error in file:', trim(flout)
  !----------------------------------------------
  close (mtin_u)
  close (mtin_ssh)
  close (mtout)
  !
!====================================================
end program btsf
