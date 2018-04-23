!-*-F90-*-
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
  &   dep,                  &
  &   kbtm,                 &
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
  !real(8)    :: mu_u, psiu, lambdau, phiu
  !
  real(8)    :: u(imut, jmut, km)
  real(8)    :: v(imut, jmut, km)
  real(8)    :: ssh(imut, jmut)
  !
  real(8), parameter :: cnv_Sv   = 1.d-12     !  cm^3/s > Sv
  real(8), parameter :: cnv_kgs  = ro * 1.d-3 !  cm^3/s > kg/s
  real(8), save      :: cnv_unit
  character(len=8), parameter :: str_Sv   = ' [Sv]'
  character(len=8), parameter :: str_kgs  = ' [kg/s]'
  character(len=8), save      :: str_unit
  !
  real(4)    :: d3_r4(imut,jmut,km)
  real(4)    :: d2_r4(imut,jmut)
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flin_u ! 入力ファイル
  character(len=256)    :: flin_v ! 入力ファイル
  character(len=256)    :: flin_ssh ! 入力ファイル
  character(len=256)    :: flout  ! 出力ファイル
  character(len=256)    :: fltopo ! 海底地形ファイル
  character(len=256)    :: flsclf ! スケールファクター・ファイル
#ifdef OGCM_VARIABLE
  character(len=256)    :: file_vgrid ! 可変格子定義ファイル
#endif /* OGCM_VARIABLE */
  !
  real(8) :: lat_right, lat_left
  real(8) :: lon_left, lon_right
  !
  logical, save :: l_cmip5 = .false.
  real(8), save :: max_dep = -1.d0
  real(8), save :: min_dep = -1.d0
  !
  namelist /nml_pass/   flin_u, flin_v, flin_ssh, flout,          &
    &                   lat_right, lat_left, lon_left, lon_right, &
    &                   fltopo, flsclf, l_cmip5,                  &
    &                   max_dep, min_dep
#ifdef OGCM_VARIABLE
  namelist /inflg/ file_vgrid
#endif /* OGCM_VARIABLE */
  !
  character(len=256)    :: flout2
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mtin_u   = 80
  integer(4), parameter :: mtin_v   = 81
  integer(4), parameter :: mtin_ssh = 82
  integer(4), parameter :: mtout    = 83
  integer(4), parameter :: mtout2   = 84
  !
  integer(4), save  :: kmax
  integer(4), save  :: kmin
  !
  integer(4) :: i, j, k, m, jj
  real(8)    :: wrk8
  real(4)    :: wrk4
  !
  !==============================================
  !
  ! 入力パラメタ規定値
  !
  flin_u  = 'hs_u.d'
  flin_v  = 'hs_v.d'
  flin_ssh= 'hs_ssh.d'
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
  read(unit=5, nml_pass)
  print *,'flin_u   :', trim(flin_u)
  print *,'flin_v   :', trim(flin_v)
  print *,'flin_ssh :', trim(flin_ssh)
  print *,'lon left :', lon_left, ' lon right:', lon_right
  print *,'lat left :', lat_left, ' lat right:', lat_right
  print *,'flout    :', trim(flout)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  print *,'l_cmip5  :', l_cmip5
  print *,'max_dep  :', max_dep, ' [m]'
  print *,'min_dep  :', min_dep, ' [m]'
  !
  write(flout2, *) trim(flout), '.txt'
  print *,'flout2   :', trim(flout2)
#ifdef OGCM_VARIABLE
  read(unit=5, inflg) ! file_vgrid
#endif /* OGCM_VARIABLE */
  if(l_cmip5) then
    cnv_unit = cnv_kgs
    str_unit = str_kgs
    print *,'unit     : [kg/s]'
  else
    cnv_unit = cnv_Sv
    str_unit = str_Sv
    print *,'unit     : [Sv]'
  end if
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
  if(min_dep <= 0.d0) then
    kmin = 1
  else
    do k = 1, km-2
      kmin = k
      if(dep(k) >= min_dep * 1.d2) exit
    end do
    write(*,*) 'dep(kmin) = ', dep(kmin) * 1.d-2, '[m]'
  end if
  write(*,*) 'kmin = ', kmin
  !---------------
  if(max_dep <= 0.d0) then
#ifdef OGCM_BBL
    kmax = km-1
#else /* OGCM_BBL */
    kmax = km
#endif /* OGCM_BBL */
  else
    do k = 1, km-1
      kmax = k
      if(dep(k+1) >= max_dep * 1.d2) exit
    end do
    write(*,*) 'dep(kmax+1) = ', dep(kmax+1) * 1.d-2, '[m]'
  end if
  write(*,*) 'kmax = ', kmax
  !
  ! 入出力ファイルオープン
  !
  open (mtin_u, file=flin_u, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_v, file=flin_v, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_ssh, file=flin_ssh, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)
  !
  !-------------------------
  !
  ! U
  !
  read (mtin_u, rec=1) d3_r4
  u(:,:,:) = aexl(:,:,:)*real(d3_r4(:,:,:),8)
  !
  u(1:2,         1:jmut, 1:km)=u(imut-3:imut-2, 1:jmut, 1:km)
  u(imut-1:imut, 1:jmut, 1:km)=u(3:4,           1:jmut, 1:km)
  !
  ! V
  !
  read (mtin_v, rec=1) d3_r4
  v(:,:,:) = aexl(:,:,:)*real(d3_r4(:,:,:),8)
  !
  v(1:2,         1:jmut, 1:km)=v(imut-3:imut-2, 1:jmut, 1:km)
  v(imut-1:imut, 1:jmut, 1:km)=v(3:4,           1:jmut, 1:km)
  !-------------------------
  !
  !  SSH
  !
  read (mtin_ssh, rec=1) d2_r4
  ssh(:,:) = atexl(:,:,1)*real(d2_r4(:,:),8)
  !
  ssh(1:2,         1:jmut)=ssh(imut-3:imut-2, 1:jmut)
  ssh(imut-1:imut, 1:jmut)=ssh(3:4,           1:jmut)
  !
  close ( mtin_u )
  close ( mtin_v )
  close ( mtin_ssh)
  !
  !---------------------------------------------------
  ! Transport through important Passages
  !    (positive : northward)
  !---------------------------------------------------
  !
  call passage_transport(wrk8)
  !
  wrk8 = wrk8 * cnv_unit
  if(l_cmip5) then
    write(*, '(e, a)') , wrk8, trim(str_unit)
  else
    write(*, '(f9.4, a)') , wrk8, trim(str_unit)
  end if
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
subroutine passage_transport(trnsprt)
  !
  real(8), intent(inout) :: trnsprt
  !
  !                          /\
  !                         /||\
  ! (lon_left, lat_left)  ---++--- (lon_right, lat_right)
  !                          ||
  !
  integer(4) :: istt, jstt
  integer(4) :: iend, jend
  real(8)    :: lambda0, phi0
  real(8)    :: lambda, phi
  real(8)    :: mu, psi
  !
  real(8)    :: trnsprt_x, trnsprt_y
  real(8)    :: ustar, vstar
  real(8)    :: tmpratio
  real(8)    :: hl0, hl1
  !
  integer(4) :: i, j, k, kstt, ktmp
  !
  ! starting point (UV-point)
  !
  hl0 = 1.d9
  !
  lambda0 = lon_left * radian_r
  phi0    = lat_left * radian_r
  do j = 3, jmut-2
    !do i = 3, imut-2
    do i = 2, imut-2
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
    !do i = 3, imut-2
    do i = 2, imut-2
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
  if(jstt == jend .and. iend == 2) iend = imut-2
  !
  write(*, '(a, i4, a, i4)') 'left  point: i=', istt, ', j=', jstt
  write(*, '(a, i4, a, i4, a, f)') 'aexl(', istt, ',', jstt, ', kmin) = ', aexl(istt, jstt, kmin)
  write(*, '(a, i4, a, i4)') 'right point: i=', iend, ', j=', jend
  write(*, '(a, i4, a, i4, a, f)') 'aexl(', iend, ',', jend, ', kmin) = ', aexl(iend, jend, kmin)
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
    !
    trnsprt_x = 0.d0
    trnsprt_y = 0.d0
    do j = jend+1, jstt
      hl1 = 1.d0 /(dx_bl(istt, j)+dx_br(istt, j))
      if(kmin <= ksgm) then
        do k=kmin, ksgm
          ustar =  dy_br(istt, j  )*dzu(istt, j  , k)*u(istt, j  , k) &
            &     +dy_tr(istt, j-1)*dzu(istt, j-1, k)*u(istt, j-1, k)
          ustar = ustar * thcksgmr * (thcksgm + hl1 *  &
            &    ( dx_bl(istt, j)*ssh(istt  , j)       &
            &     +dx_br(istt, j)*ssh(istt+1, j)) )
          trnsprt_x = trnsprt_x + ustar
        end do
        !
        kstt = ksgm+1
      end if
      !
      if(kmin > ksgm) kstt = kmin
      do k=kstt, kmax
        ustar = dy_br(istt, j  )*dzu(istt, j  , k)*u(istt, j  , k) &
          &    +dy_tr(istt, j-1)*dzu(istt, j-1, k)*u(istt, j-1, k)
        trnsprt_x = trnsprt_x + ustar
      end do
#ifdef OGCM_BBL
      hl1 = 0.d0
      ktmp = kbtm(istt,j)
      if(ktmp >= kstt .and. kstt > ksgm .and. ktmp <= kmax) hl1 = 1.d0
      hl0 = 0.d0
      ktmp = kbtm(istt,j-1)
      if(ktmp >= kstt .and. kstt > ksgm .and. ktmp <= kmax) hl0 = 1.d0
      !
      ustar = dy_br(istt, j  )*dzu(istt, j  , km)*u(istt, j  , km)*hl1 &
        &    +dy_tr(istt, j-1)*dzu(istt, j-1, km)*u(istt, j-1, km)*hl0
      trnsprt_x = trnsprt_x + ustar
#endif /* OGCM_BBL */
    end do
    write(*,*) 'X-ward Transport:', trnsprt_x*1.d-12, '[Sv]'
    !-------------------------------------------------------
    if(istt < iend) then
      write(*, '(a,         i3,   a,    i3,     a,         i3,   a,   i3,   a  )') &
        &        'from U(', istt, ', ', jend, ') to U(', iend, ', ', jend, ')'
      !
      do i = istt+1, iend
        hl1 = 1.d0 /(dy_bl(i, jend)+dy_tl(i, jend))
        if(kmin <= ksgm) then
          do k=kmin, ksgm
            vstar = dx_tr(i-1, jend)*dzu(i-1, jend, k)*v(i-1, jend, k) &
              &    +dx_tl(i  , jend)*dzu(i  , jend, k)*v(i  , jend, k)
            vstar = vstar * thcksgmr * (thcksgm + hl1 *  &
              &    ( dy_tl(i, jend)*ssh(i, jend+1)       &
              &     +dy_bl(i, jend)*ssh(i, jend  )) )
            trnsprt_y = trnsprt_y + vstar
          end do
          !
          kstt = ksgm+1
        end if
        !
        if(kmin > ksgm) kstt = kmin
        do k=kstt, kmax
          vstar = dx_tr(i-1, jend)*dzu(i-1, jend, k)*v(i-1, jend, k) &
            &    +dx_tl(i  , jend)*dzu(i  , jend, k)*v(i  , jend, k)
          trnsprt_y = trnsprt_y + vstar
        end do
#ifdef OGCM_BBL
        hl1 = 0.d0
        ktmp = kbtm(i,jend)
        if(ktmp >= kstt .and. kstt > ksgm .and. ktmp <= kmax) hl1 = 1.d0
        hl0 = 0.d0
        ktmp = kbtm(i-1,jend)
        if(ktmp >= kstt .and. kstt > ksgm .and. ktmp <= kmax) hl0 = 1.d0
        !
        vstar = dx_tr(i-1, jend)*dzu(i-1, jend, km)*v(i-1, jend, km)*hl0 &
          &    +dx_tl(i  , jend)*dzu(i  , jend, km)*v(i  , jend, km)*hl1
        trnsprt_y = trnsprt_y + vstar
#endif /* OGCM_BBL */
      end do
      !
      write(*,*) 'Y-ward Transport:', trnsprt_y*1.d-12, '[Sv]'
    end if
    trnsprt = trnsprt_x + trnsprt_y
    write(*,*) 'Y-ward and X-ward Transport:', trnsprt*1.d-12, '[Sv]'
  !=========================================================
  else if(jstt < jend) then ! /
    write(*, '(a,         i3,   a,    i3,     a,         i3,   a,   i3,   a  )') &
      &        'from U(', istt, ', ', jstt, ') to U(', istt, ', ', jend, ')'
    !
    trnsprt_x = 0.d0
    trnsprt_y = 0.d0
    do j = jstt+1, jend
      hl1 = 1.d0 /(dx_bl(istt, j)+dx_br(istt, j))
      if(kmin <= ksgm) then
        do k=kmin, ksgm
          ustar =  dy_br(istt, j  )*dzu(istt, j  , k)*u(istt, j  , k) &
            &     +dy_tr(istt, j-1)*dzu(istt, j-1, k)*u(istt, j-1, k)
          ustar = ustar * thcksgmr * (thcksgm + hl1 *  &
            &    ( dx_bl(istt, j)*ssh(istt  , j)       &
            &     +dx_br(istt, j)*ssh(istt+1, j)) )
          trnsprt_x = trnsprt_x + ustar
        end do
        !
        kstt = ksgm+1
      end if
      !
      if(kmin > ksgm) kstt = kmin
      !
      do k=kstt, kmax
        ustar = dy_br(istt, j  )*dzu(istt, j  , k)*u(istt, j  , k) &
          &    +dy_tr(istt, j-1)*dzu(istt, j-1, k)*u(istt, j-1, k)
        trnsprt_x = trnsprt_x + ustar
      end do
#ifdef OGCM_BBL
        hl1 = 0.d0
        ktmp = kbtm(istt,j)
        if(ktmp >= kstt .and. kstt > ksgm .and. ktmp <= kmax) hl1 = 1.d0
        hl0 = 0.d0
        ktmp = kbtm(istt,j-1)
        if(ktmp >= kstt .and. kstt > ksgm .and. ktmp <= kmax) hl0 = 1.d0
        !
        ustar = dy_br(istt, j  )*dzu(istt, j  , km)*u(istt, j  , km)*hl1 &
          &    +dy_tr(istt, j-1)*dzu(istt, j-1, km)*u(istt, j-1, km)*hl0
        trnsprt_x = trnsprt_x + ustar
#endif /* OGCM_BBL */
    end do
    !
    write(*,*) 'minus X-ward Transport:', -trnsprt_x*1.d-12, '[Sv]'
    !-------------------------------------------------------
    if(istt < iend) then
      write(*, '(a,         i3,   a,    i3,     a,         i3,   a,   i3,   a  )') &
        &        'from U(', istt, ', ', jend, ') to U(', iend, ', ', jend, ')'
      !
      do i = istt+1, iend
        hl1 = 1.d0 /(dy_bl(i, jend)+dy_tl(i, jend))
        if(kmin <= ksgm) then
          do k=kmin, ksgm
            vstar = dx_tr(i-1, jend)*dzu(i-1, jend, k)*v(i-1, jend, k) &
              &    +dx_tl(i  , jend)*dzu(i  , jend, k)*v(i  , jend, k)
            vstar = vstar * thcksgmr * (thcksgm + hl1 *  &
              &    ( dy_tl(i, jend)*ssh(i, jend+1)       &
              &     +dy_bl(i, jend)*ssh(i, jend  )) )
            trnsprt_y = trnsprt_y + vstar
          end do
          !
          kstt = ksgm+1
        end if
        !
        if(kmin > ksgm) kstt = kmin
        !
        do k=kstt, kmax
          vstar = dx_tr(i-1, jend)*dzu(i-1, jend, k)*v(i-1, jend, k) &
            &    +dx_tl(i  , jend)*dzu(i  , jend, k)*v(i  , jend, k)
          trnsprt_y = trnsprt_y + vstar
        end do
#ifdef OGCM_BBL
        hl0 = 0.d0
        ktmp = kbtm(i-1,jend)
        if(ktmp >= kstt .and. kstt > ksgm .and. ktmp <= kmax) hl0 = 1.d0
        hl1 = 0.d0
        ktmp = kbtm(i,jend)
        if(ktmp >= kstt .and. kstt > ksgm .and. ktmp <= kmax) hl1 = 1.d0
        !
        vstar = dx_tr(i-1, jend)*dzu(i-1, jend, km)*v(i-1, jend, km)*hl0 &
          &    +dx_tl(i  , jend)*dzu(i  , jend, km)*v(i  , jend, km)*hl1
        trnsprt_y = trnsprt_y + vstar
#endif /* OGCM_BBL */
      end do
      !
      write(*,*) 'Y-ward Transport:', trnsprt_y*1.d-12, '[Sv]'
    end if
    trnsprt = - trnsprt_x + trnsprt_y
    write(*,*) 'Y-ward and minus X-ward Transport:', trnsprt*1.d-12, '[Sv]'
  !=========================================================
  else   !   jstt == jend
    write(*, '(a,         i3,   a,    i3,     a,         i3,   a,   i3,   a  )') &
      &        'from U(', istt, ', ', jstt, ') to U(', iend, ', ', jstt, ')'
    !
    trnsprt_x = 0.d0
    trnsprt_y = 0.d0
    do i = istt+1, iend
      hl1 = 1.d0 /(dy_bl(i, jstt)+dy_tl(i, jstt))
      if(kmin <= ksgm) then
        do k=kmin, ksgm
          vstar = dx_tr(i-1, jstt)*dzu(i-1, jstt, k)*v(i-1, jstt, k) &
            &    +dx_tl(i  , jstt)*dzu(i  , jstt, k)*v(i  , jstt, k)
          vstar = vstar * thcksgmr * (thcksgm + hl1 *  &
            &    ( dy_tl(i, jstt)*ssh(i, jstt+1)       &
            &     +dy_bl(i, jstt)*ssh(i, jstt  )) )
          trnsprt = trnsprt + vstar
        end do
        !
        kstt = ksgm+1
      end if
      !
      if(kmin > ksgm) kstt = kmin
      !
      do k=kstt, kmax
        vstar = dx_tr(i-1, jstt)*dzu(i-1, jstt, k)*v(i-1, jstt, k) &
          &    +dx_tl(i  , jstt)*dzu(i  , jstt, k)*v(i  , jstt, k)
        trnsprt = trnsprt + vstar
      end do
#ifdef OGCM_BBL
      hl0 = 0.d0
      ktmp = kbtm(i-1,jstt)
      if(ktmp >= kstt .and. kstt > ksgm .and. ktmp <= kmax) hl0 = 1.d0
      hl1 = 0.d0
      ktmp = kbtm(i,jstt)
      if(ktmp >= kstt .and. kstt > ksgm .and. ktmp <= kmax) hl1 = 1.d0
      !
      if(max(kbtm(i-1,jstt), kbtm(i,jstt)) > kstt .and. kstt > ksgm) then
        vstar = dx_tr(i-1, jstt)*dzu(i-1, jstt, km)*v(i-1, jstt, km)*hl0 &
          &    +dx_tl(i  , jstt)*dzu(i  , jstt, km)*v(i  , jstt, km)*hl1
        trnsprt = trnsprt + vstar
      end if
#endif /* OGCM_BBL */
    end do
    !
    write(*,*) 'Y-ward Transport:', trnsprt*1.d-12, '[Sv]'
  end if
  !
end subroutine passage_transport
!====================================================
end program passage_t
