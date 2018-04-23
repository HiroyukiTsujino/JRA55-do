!passage.F90
!====================================================
!
! Calculate Transport through a Passage
!
!====================================================
program passage_t
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ksgm, dz,            &
  &   pi, radian, radian_r, &
  &   slat0, slon0,        &
  &   nplat, nplon,        &
  &   splat, splon,        &
  &   ro
#ifndef OGCM_VARIABLE
  use oc_mod_param, only : &
  &   dxtdgc, dytdgc
#endif /* ! OGCM_VARIABLE */
  !
  use oc_mod_trnsfrm, only  : &
  &   set_abc, mp2lp,         &
  &   length_on_sphere
  !
  use oc_structure, only  : &
  &   read_topo,            & !--------
  &   aexl, atexl,          & ! 海陸インデックス
  &   coefx, coefy,         &
  &   dzu,                  & ! UVボックスの厚さ
  &   thcksgm, thcksgmr,    & ! sigma 層の厚さ、逆数
  &   read_scale,           & !--------
  &   a_tl  , a_tr  ,       & ! 格子面積
  &   a_bl  , a_br  ,       &
  &   dx_tl , dx_tr ,       & ! 東西長
  &   dx_bl , dx_br ,       &
  &   dy_tl , dy_tr ,       & ! 南北長
  &   dy_bl , dy_br ,       &
  &   set_hgrids,           & !--------
  &   dxtdeg, dytdeg,       & ! T点を中心とするモデル格子間隔(度)
  &   dxudeg, dyudeg,       & ! U点を中心とするモデル格子間隔(度)
  &   slat, slon,           & ! 
  &   alatt, alatu,         & ! モデル座標上緯度(psi)経度(mu)
  &   alont, alonu
  !
#ifdef OGCM_BBL
  use oc_structure, only  : &
  &   ho4bbl, exnnbbl,      & ! BBL層厚、層数
  &   aexlbbl, atexlbbl       ! BBLインデックス
#endif /* OGCM_BBL */
  !
  !----------------------------------------------
  !
  implicit none
  !
  real(8)    :: u(imut, jmut)
  real(8)    :: v(imut, jmut)
  !
  real(4)    :: d2_r4(imut,jmut)
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flin_ice_uv ! 入力ファイル
  character(len=256)    :: flout  ! 出力ファイル
  character(len=256)    :: fltopo ! 海底地形ファイル
  character(len=256)    :: flsclf ! スケールファクター・ファイル
  character(len=256)    :: flibas ! basinインデックスファイル
#ifdef OGCM_VARIABLE
  character(len=256)    :: file_vgrid ! 可変格子定義ファイル
#endif /* OGCM_VARIABLE */
  !
  integer(4) :: irecu, irecv

  real(8) :: lat_right, lat_left
  real(8) :: lon_left, lon_right
  !
  namelist /nml_passi/  flin_ice_uv, flout, irecu, irecv,         &
    &                   lat_right, lat_left, lon_left, lon_right, &
    &                   fltopo, flsclf, flibas
#ifdef OGCM_VARIABLE
  namelist /inflg/ file_vgrid
#endif /* OGCM_VARIABLE */
  !
  character(len=256)    :: flout2
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mttmp    = 79
  integer(4), parameter :: mtin_ice_uv = 80
  integer(4), parameter :: mtout    = 83
  integer(4), parameter :: mtout2   = 84
  !
  integer(4) :: i, j, k, m, jj
  real(8)    :: wrk8
  real(4)    :: wrk4
  !
  !==============================================
  !
  ! 入力パラメタ規定値
  !
  flin_ice_uv  = 'hs_icecat_uv.185101'
  irecu = 3
  irecv = 4
  lon_left = 290.d0
  lon_right = 295.d0
  lat_left = -50.d0
  lat_right = -70.d0
  flout  = 'passage.d'
  flout2 = 'passage.d.txt'
  fltopo = 'topo.d'
  flsclf = 'scale_factor.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml_passi)
  print *,'flin_ice_uv :', trim(flin_ice_uv)
  print *,'icecu    :', irecu
  print *,'icecv    :', irecv
  print *,'lon left :', lon_left, ' lon right:', lon_right
  print *,'lat left :', lat_left, ' lat right:', lat_right
  print *,'flout    :', trim(flout)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  !
  write(flout2, *) trim(flout), '.txt'
  print *,'flout2   :', trim(flout2)
#ifdef OGCM_VARIABLE
  read(unit=5, inflg) ! file_vgrid
#endif /* OGCM_VARIABLE */
  !
  !----------------------------------------------
  ! 海洋モデル格子情報等の準備
  !
  call read_topo(fltopo)
  !
  ! 座標変換のパラメータを決める
  call set_abc ( nplat, nplon, splat, splon )
  !
  ! モデル水平格子情報定義
  !
#ifdef OGCM_VARIABLE
  call set_hgrids(file_vgrid)
#else /* OGCM_VARIABLE */
  call set_hgrids
#endif /* OGCM_VARIABLE */
  !
  !----------------------------------------------
  !
  !  スケールファクタの読み込み
  !
  call read_scale(flsclf)
  !
  !==============================================
  !
  ! 入出力ファイルオープン
  !
  open (mtin_ice_uv, file=flin_ice_uv, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)

  !-------------------------
  !
  ! ICE_UV
  !
  read (mtin_ice_uv, rec=irecu) d2_r4
  u(:,:) = aexl(:,:,1)*real(d2_r4(:,:),8)
  u(1:2,         1:jmut)=u(imut-3:imut-2, 1:jmut)
  u(imut-1:imut, 1:jmut)=u(3:4,           1:jmut)

  read (mtin_ice_uv, rec=irecv) d2_r4
  v(:,:) = aexl(:,:,1)*real(d2_r4(:,:),8)
  v(1:2,         1:jmut) = v(imut-3:imut-2, 1:jmut)
  v(imut-1:imut, 1:jmut) = v(3:4,           1:jmut)

  close ( mtin_ice_uv )
  !
  !---------------------------------------------------
  ! Transport through important Passages
  !    (positive : northward)
  !---------------------------------------------------
  !
  call passage_transport_ice(wrk8)
  !
  write(*, '(e10.4e1, a)') , wrk8,   ' [kg/s]'
  wrk4 = real(wrk8, 4)
  !
  open (mtout, file=flout, form='unformatted',&
    &  access='direct', recl=4 )
  write(mtout, rec=1) wrk4
  close ( mtout )
  !
  open (mtout2, file=flout2)
  write(mtout2, *) wrk8
  close ( mtout2 )
  !
contains
!====================================================
!
!  地理座標で指定した断面を通過する流量を計算
!
!====================================================
subroutine passage_transport_ice(trnsprt)
  !
  real(8), intent(inout) :: trnsprt
  !
  !                          /\
  !                         /||\
  ! (lon_left, lat_right) ---++--- (lon_right, lat_right)
  !                          ||
  !
  integer(4) :: istt, jstt
  integer(4) :: iend, jend
  real(8)    :: lambda0, phi0
  real(8)    :: lambda, phi
  real(8)    :: mu, psi
  !
  real(8)    :: ustar, vstar
  real(8)    :: tmpratio
  real(8)    :: hl0, hl1
  !
  integer(4) :: i, j, k
#ifdef OGCM_BBL
  integer(4), parameter :: kmax = km-1
#else /* OGCM_BBL */
  integer(4), parameter :: kmax = km
#endif /* OGCM_BBL */
  !
  ! starting point (UV-point)
  !
  hl0 = 1.d9
  !
  lambda0 = lon_left * radian_r
  phi0    = lat_left * radian_r
  do j = 3, jmut-2
    do i = 3, imut-2
      mu  = alonu(i)*radian_r
      psi = alatu(j)*radian_r
      call mp2lp(lambda, phi, mu, psi)
      hl1 = length_on_sphere(lambda, phi, lambda0, phi0)
      if(hl1 < hl0) then
        hl0 = hl1
        istt = i
        jstt = j
      end if
    end do
  end do
  !
  ! ending point (UV-point)
  !
  hl0 = 1.d9
  !
  lambda0 = lon_right * radian_r
  phi0    = lat_right * radian_r
  do j = 3, jmut-2
    do i = 3, imut-2
      mu  = alonu(i)*radian_r
      psi = alatu(j)*radian_r
      call mp2lp(lambda, phi, mu, psi)
      hl1 = length_on_sphere(lambda, phi, lambda0, phi0)
      if(hl1 < hl0) then
        hl0 = hl1
        iend = i
        jend = j
      end if
    end do
  end do
  !
  write(*, '(a, i4, a, i4)') 'left  point: i=', istt, ', j=', jstt
  write(*,'(1a)') '   SW      SE      NW      NE   '
  write(*,'(4F8.3)') aexl(istt-1,jstt-1,1),  aexl(istt,jstt-1,1),  aexl(istt-1,jstt,1),  aexl(istt,jstt,1)
  write(*, '(a, i4, a, i4)') 'right point: i=', iend, ', j=', jend
  write(*,'(1a)') '   SW      SE      NW      NE   '
  write(*,'(4F8.3)') aexl(iend-1,jend-1,1),  aexl(iend,jend-1,1),  aexl(iend-1,jend,1),  aexl(iend,jend,1)

  if(istt > iend) then
    write(*, *) 'start i > end i'
    write(*, *) 'Please Check'
    stop
  end if
  !
  !---------------------
  trnsprt = 0.d0
  !
  if(jstt > jend) then ! \

    write(*, '(a,         i3,   a,    i3,   a,         i3,   a,   i3,   a  )') &
      &        'from U(', istt, ', ', jend, ') to U(', istt, ', ', jstt, ')'

    do j = jend+1, jstt
      ustar =  0.5d0 * (aexl(istt,j,1)*u(istt,j) + aexl(istt,j-1,1)*u(istt,j-1))
      trnsprt = trnsprt + ustar
    end do
    write(*,*) 'Eastward Transport:', trnsprt, '[Kg/s]'

    !-------------------------------------------------------

    if(istt < iend) then

      write(*, '(a,         i3,   a,    i3,     a,         i3,   a,   i3,   a  )') &
        &        'from U(', istt, ', ', jend, ') to U(', iend, ', ', jend, ')'

      do i = istt+1, iend
        vstar = 0.5d0 * (aexl(i-1,jend,1)*v(i-1,jend) + aexl(i,jend,1)*v(i,jend))
        trnsprt = trnsprt + vstar
      end do

      write(*,*) 'Northward and Eastward Transport:', trnsprt, '[Kg/s]'
    end if
  !=========================================================
  else if(jstt < jend) then ! /

    write(*, '(a,         i3,   a,    i3,     a,         i3,   a,   i3,   a  )') &
      &        'from U(', istt, ', ', jstt+1, ') to U(', istt, ', ', jend, ')'

    do j = jstt+1, jend
      ustar = 0.5d0 * (aexl(istt,j,1) * u(istt,j) + aexl(istt,j-1,1)*u(istt, j-1))
      trnsprt = trnsprt - ustar
    end do

    write(*,*) 'Westward Transport:', trnsprt, '[Kg/s]'
    !-------------------------------------------------------
    if(istt < iend) then

      write(*, '(a,         i3,   a,    i3,     a,         i3,   a,   i3,   a  )') &
        &        'from U(', istt, ', ', jend, ') to U(', iend, ', ', jend, ')'

      do i = istt+1, iend
        vstar = 0.5d0 * (aexl(i-1,jend,1)*v(i-1, jend) + aexl(i,jend,1)*v(i,jend))
        trnsprt = trnsprt + vstar
      end do

      write(*,*) 'Northward and Westward Transport:', trnsprt, '[Kg/s]'

    end if
  !=========================================================
  else   !   jstt == jend

    write(*, '(a,         i3,   a,    i3,     a,         i3,   a,   i3,   a  )') &
      &        'from U(', istt, ', ', jstt, ') to U(', iend, ', ', jstt, ')'

    do i = istt+1, iend
      vstar = 0.5d0 * (aexl(i-1,jstt,1) * v(i-1,jstt) + aexl(i,jstt,1) * v(i,jstt))
      trnsprt = trnsprt + vstar
    end do

    write(*,*) 'Northward Transport:', trnsprt, '[Kg/s]'

  end if
  !
end subroutine passage_transport_ice
!====================================================
end program passage_t
