!-*-F90-*-
!globalmean.F90
!====================================================
!
! Global Average of Variables at TS-Grid
!
!====================================================
program globalmean
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ksgm, dz
  !
  use oc_structure, only :  &
  &   read_topo,            & !--------
  &   dep,                  & ! UVTSボックス境界（上面）の深さ
  &   ho4, exnn ,           & ! 水深、層数
  &   aexl, atexl,          & ! 海陸インデックス
  &   dzu,                  & ! UVボックスの厚さ
#ifdef OGCM_BBL
  &   ho4bbl, exnnbbl,      & ! BBL層厚、層数
  &   aexlbbl, atexlbbl,    & ! BBLインデックス
#endif /* OGCM_BBL */
  &   thcksgm, thcksgmr ,   & ! σ層の厚さ、逆数
  &   read_scale,           & !--------
  &   a_tl  , a_tr  ,       & ! 格子面積
  &   a_bl  , a_br  ,       &
  &   dx_tl , dx_tr ,       & ! 東西長
  &   dx_bl , dx_br ,       &
  &   dy_tl , dy_tr ,       & ! 南北長
  &   dy_bl , dy_br
  !
  !----------------------------------------------
  !
  implicit none
  !
  real(4), parameter :: UNDEF = -9.99e33
  !
  ! 海洋モデル地形
  !
  real(8)    :: t(imut, jmut, km)
  real(8)    :: ssh(imut, jmut)
  !
  real(4)    :: d3_r4(imut,jmut,km)
  real(4)    :: d2_r4(imut,jmut)
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flin   ! 入力ファイル
  character(len=256)    :: flin_ssh  ! 入力ファイル
  character(len=256)    :: fltopo ! 海底地形ファイル
  character(len=256)    :: flsclf ! スケールファクター・ファイル
  character(len=256)    :: flout0 ! 出力ファイル
  !
  namelist /nml_glbmean/ flin, flin_ssh, fltopo, flsclf, flout0
  !
  character(len=256)    :: flout1
  character(len=256)    :: flout2 ! 出力ファイル
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mttmp  = 79
  integer(4), parameter :: mtin   = 80
  integer(4), parameter :: mtin_ssh = 83
  integer(4), parameter :: mtout0 = 84
  integer(4), parameter :: mtout1 = 85
  integer(4), parameter :: mtout2 = 86
  !
#ifdef OGCM_BBL
  integer(4) :: kbtm
  integer(4), parameter :: kmax = km-1
#else /* OGCM_BBL */
  integer(4), parameter :: kmax = km
#endif /* OGCM_BBL */
  !
  real(8)    :: vol, tvol, tvol_k(km)
  real(8)    :: sumt, sumt_k(km)
  real(8)    :: meant
  real(8) :: nendidx(jmut)
  !
  real(4)    :: meant4
  !
  integer(4) :: i, j, k
  !
  !==============================================
  !
  !----------------------------------------------
  ! 入力パラメタ既定値
  !----------------------------------------------
  flin    = 'hs_t.d'
  flin_ssh= 'hs_ssh.d'
  fltopo  = 'topo.d'
  flsclf  = 'scale_factor.d'
  flout0  = 'thetaoga.d'
  flout1  = 'thetaoga.d.lev'
  flout2  = 'thetaoga.d.txt'
  !
  !----------------------------------------------
  ! 標準入力から読み込み
  !----------------------------------------------
  read(unit=5, nml_glbmean)
  print *,'flin     :', trim(flin)
  print *,'flin_ssh :', trim(flin_ssh)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  print *,'flout0   :', trim(flout0)
  !
  write(flout1, *) trim(flout0), '.lev'
  print *,'flout1   :', trim(flout1)
  write(flout2, *) trim(flout0), '.txt'
  print *,'flout2   :', trim(flout2)
  !----------------------------------------------
  !  地形の読み込み
  !----------------------------------------------
  call read_topo(fltopo)
  !
  !----------------------------------------------
  !  スケールファクタの読み込み
  !----------------------------------------------
  call read_scale(flsclf)
  !
  !==============================================
  !
  !----------------------------------------------
  ! 入出力ファイルオープン
  !----------------------------------------------
  open (mtin, file=flin, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  write(*, *) 'file in :', trim(flin)
  !
  open (mtin_ssh, file=flin_ssh, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)
  write(*, *) 'ssh in  :', trim(flin_ssh)
  !
  !----------------------------------------------
  ! 読み込み、倍精度実数への変換
  !----------------------------------------------
  read (mtin, rec=1) d3_r4(1:imut,1:jmut,1:km)
  t(1:imut,1:jmut,1:km) = atexl(1:imut,1:jmut,1:km)*real(d3_r4(1:imut,1:jmut,1:km),8)
  !
  !  SSH
  read (mtin_ssh, rec=1) d2_r4
  ssh(:,:) = atexl(:,:,1)*real(d2_r4(:,:),8)
  !
  close (mtin)
  close (mtin_ssh)
  !
  !----------------------------------------------
  ! 体積重みをつけて全球で積算
  !----------------------------------------------
  nendidx(1:jmut) = 1.d0
#if defined OGCM_JOT || defined OGCM_TRIPOLAR
  nendidx(jmut-2) = 0.5d0
  nendidx(jmut-1:jmut) = 0.d0
#endif /* OGCM_JOT || OGCM_TRIPOLAR */
  tvol = 0.d0
  sumt = 0.d0
  tvol_k(1:km) = 0.d0
  sumt_k(1:km) = 0.d0
  !
  do j = 2, jmut-2
    do i = 3, imut-2
      do k = 1, ksgm
        vol = dz(k) * (thcksgm+ssh(i, j)) * thcksgmr * nendidx(j) &
          &   * (aexl(i-1, j  , k)* a_br(i-1, j  ) +aexl(i, j  , k)*a_bl(i, j  ) &
          &     +aexl(i-1, j-1, k)* a_tr(i-1, j-1) +aexl(i, j-1, k)*a_tl(i, j-1))
        tvol_k(k) = tvol_k(k) + vol
        sumt_k(k) = sumt_k(k) + t(i, j, k) * vol
        tvol = tvol + vol
        sumt = sumt + t(i, j, k) * vol
      end do
      do k = ksgm+1, kmax
        vol = nendidx(j) *                                                       &
          &   ( dzu(i-1, j  , k) * a_br(i-1, j  ) +dzu(i, j  , k) * a_bl(i, j  ) &
          &    +dzu(i-1, j-1, k) * a_tr(i-1, j-1) +dzu(i, j-1, k) * a_tl(i, j-1))
        tvol_k(k) = tvol_k(k) + vol
        sumt_k(k) = sumt_k(k) + t(i, j, k) * vol
        tvol = tvol + vol
        sumt = sumt + t(i, j, k) * vol
      end do
#ifdef OGCM_BBL
      vol = nendidx(j) *                                                         &
        &   ( dzu(i-1, j  , km) * a_br(i-1, j  ) +dzu(i, j  , km) * a_bl(i, j  ) &
        &    +dzu(i-1, j-1, km) * a_tr(i-1, j-1) +dzu(i, j-1, km) * a_tl(i, j-1))
      tvol_k(km) = tvol_k(km) + vol
      sumt_k(km) = sumt_k(km) + t(i, j, k) * vol
      tvol = tvol + vol
      sumt = sumt + t(i, j, km) * vol
#endif /* OGCM_BBL */
    end do
  end do
  !
  !----------------------------------------------
  ! 単精度に変換して書き込み
  !----------------------------------------------
  open (mtout0, file=flout0, form='unformatted',&
    &  access='direct', recl=4)
  !
  open (mtout2, file=flout2)
  !
  meant = sumt / tvol
  meant4 = real(meant, 4)
  write(*,*) 'Total Volume :', tvol*1.d-6, '[m^3]'
  write(*,*) 'Global Average :', meant
  !
  write(mtout0, rec=1) meant4
  write(mtout2, *)    meant
  !
  close (mtout0)
  close (mtout2)
  !-------------------
  open (mtout1, file=flout1, form='unformatted',&
    &  access='direct', recl=4)
  !
  do k = 1, km
    meant = sumt_k(k) / tvol_k(k)
    meant4 = real(meant, 4)
    write(*,'(i2.2, a,          e12.5,           a,                   f8.5)'),  &
      &       k,    ' Volume:', tvol_k(k)*1.d-6, ' [m^3]; Average :', meant
    !
    write(mtout1, rec=k) meant4
  end do
  !
  close (mtout1)
  !----------------------------------------------
  !
!====================================================
end program globalmean
