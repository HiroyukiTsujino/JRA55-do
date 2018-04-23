!-*-F90-*-
!volo.F90
!====================================================
!
! Global Sea Water Volume and Mass 
!
!====================================================
program volo
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ksgm, dz,            &
  &   rho0,                &
  &   slat0, slon0,        &
  &   nplat, nplon,        &
  &   splat, splon,        &
  &   pi, radian, radian_r
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
  &   ktbtm,                &
#endif /* OGCM_BBL */
  &   thcksgm, thcksgmr ,   & ! σ層の厚さ、逆数
  &   read_scale,           & !--------
  &   set_hgrids,           &
  &   a_tl  , a_tr  ,       & ! 格子面積
  &   a_bl  , a_br  ,       &
  &   dx_tl , dx_tr ,       & ! 東西長
  &   dx_bl , dx_br ,       &
  &   dy_tl , dy_tr ,       & ! 南北長
  &   dy_bl , dy_br,        &
  &   alont, alonu,         &
  &   alatt, alatu,         &
  &   slon0, slat0,         &
  &   dxtdeg, dytdeg
  !
  use oc_mod_trnsfrm, only  : &
  &   set_abc, mp2lp

  !----------------------------------------------
  !
  implicit none
  !
  real(4), parameter :: UNDEF = 0.0e0
  !
  ! 海洋モデル地形
  !
  real(8)    :: ssh(imut, jmut)

  real(8)    :: area_t(imut, jmut, km)
  real(8)    :: volumt(imut, jmut, km)
  real(8)    :: deptht(imut, jmut, km)
  real(8)    :: glatt(imut, jmut)
  real(8)    :: glatu(imut, jmut)
  real(8)    :: glont(imut, jmut)
  real(8)    :: glonu(imut, jmut)

  real(8)    :: glatt_corner(4, imut, jmut)
  real(8)    :: glatu_corner(4, imut, jmut)
  real(8)    :: glont_corner(4, imut, jmut)
  real(8)    :: glonu_corner(4, imut, jmut)
  !
  real(8)    :: grp_dxtdeg(imut)
  real(8)    :: grp_dytdeg(jmut)
  real(8)    :: grp_dxudeg(imut)
  real(8)    :: grp_dyudeg(jmut)
  real(8)    :: grp_lont(imut)
  real(8)    :: grp_lonu(imut)
  real(8)    :: grp_latt(jmut)
  real(8)    :: grp_latu(jmut)

  real(8)    :: grp_alatt(imut,jmut)
  real(8)    :: grp_alatu(imut,jmut)
  real(8)    :: grp_alont(imut,jmut)
  real(8)    :: grp_alonu(imut,jmut)
  !
  real(8)    :: nat_alatt(imut,jmut)
  real(8)    :: nat_alatu(imut,jmut)
  real(8)    :: nat_alont(imut,jmut)
  real(8)    :: nat_alonu(imut,jmut)
  !
  real(4)    :: d2_r4(imut,jmut)
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flin_ssh  ! 入力ファイル
  character(len=256)    :: fltopo ! 海底地形ファイル
  character(len=256)    :: flvgrd ! 
  character(len=256)    :: flsclf ! スケールファクター・ファイル
  character(len=256)    :: floutv
  character(len=256)    :: floutm

  logical :: l_gridinfo
  character(len=256)    :: floutare
  character(len=256)    :: floutvol
  character(len=256)    :: floutdep

  character(len=256)    :: flouttp
  character(len=256)    :: floutup

  character(len=256)    :: flouttp_corner
  character(len=256)    :: floutup_corner

  character(len=256)    :: flout_lattg
  character(len=256)    :: flout_latug
  character(len=256)    :: flout_lontg
  character(len=256)    :: flout_lonug
  character(len=256)    :: flout_lonlat_t
  character(len=256)    :: flout_lonlat_u
  character(len=256)    :: flout_nat_lonlat_t
  character(len=256)    :: flout_nat_lonlat_u
  !
  namelist /nml_volo/  flin_ssh, fltopo, flvgrd, flsclf, floutv, floutm, &
       & l_gridinfo, floutare, floutvol, floutdep, &
       & flouttp, floutup, flouttp_corner, floutup_corner, &
       & flout_lattg, flout_latug, flout_lontg, flout_lonug, &
       & flout_lonlat_t, flout_lonlat_u, &
       & flout_nat_lonlat_t, flout_nat_lonlat_u
  !
  character(len=256)    :: floutv_txt
  character(len=256)    :: floutm_txt
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mtin_ssh = 80
  integer(4), parameter :: mtoutv = 82
  integer(4), parameter :: mtoutm = 83
  integer(4), parameter :: mtoutv_txt = 84
  integer(4), parameter :: mtoutm_txt = 85

  integer(4), parameter :: mtoutar = 86
  integer(4), parameter :: mtoutvl = 87
  integer(4), parameter :: mtoutdp = 88
  !
  integer(4), parameter :: mtouttp = 91
  integer(4), parameter :: mtoutup = 92
  integer(4), parameter :: mtout_tmp = 93

#ifdef OGCM_BBL
  integer(4), parameter :: kmax = km-1
#else /* OGCM_BBL */
  integer(4), parameter :: kmax = km
#endif /* OGCM_BBL */
  !
  real(8)    :: vol, tvol, tmass
  real(8)    :: dztu, dztl
  real(8) :: nendidx(jmut)
  !
  integer(4) :: i, j, k, ku
  !
  real(8) :: mu_u, psi_u, lambdau, phiu
  real(8) :: lon_trn, lat_trn

  !==============================================
  !
  !----------------------------------------------
  ! 入力パラメタ既定値
  !----------------------------------------------
  flin_ssh= 'hs_ssh.d'
  fltopo  = 'topo.d'
  flvgrd  = 'vgrid.d'
  flsclf  = 'scale_factor.d'
  floutv  = 'volo.d'
  floutm  = 'masso.d'
  floutv_txt  = 'volo.txt'
  floutm_txt  = 'masso.txt'
  floutare = 'area_t.d'
  floutvol = 'volume_t.d'
  floutdep = 'depth_t.d'

  flouttp = 'glatlon_t.d'
  floutup = 'glatlon_u.d'

  !----------------------------------------------
  ! 標準入力から読み込み
  !----------------------------------------------

  read(unit=5, nml_volo)

  print *,'flin_ssh :', trim(flin_ssh)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  print *,'flvgrd   :', trim(flvgrd)
  print *,'floutv   :', trim(floutv)
  print *,'floutm   :', trim(floutm)

  write(floutv_txt, *) trim(floutv), '.txt'
  write(floutm_txt, *) trim(floutm), '.txt'

  print *,'floutv_txt: ', trim(floutv_txt)
  print *,'floutm_txt: ', trim(floutm_txt)

  print *,'floutare :', trim(floutare)
  print *,'floutvol :', trim(floutvol)
  print *,'floutdep :', trim(floutdep)

  print *,'flouttp  :', trim(flouttp)
  print *,'floutup  :', trim(floutup)

  print *,'flouttp_corner  :', trim(flouttp_corner)
  print *,'floutup_corner  :', trim(floutup_corner)

  !----------------------------------------------
  !  地形の読み込み
  !----------------------------------------------
  call read_topo(fltopo)
  !
  !----------------------------------------------
  ! 座標変換のパラメータを決める
  !----------------------------------------------
  call set_abc ( nplat, nplon, splat, splon )
  !
  !----------------------------------------------
  ! モデル水平格子情報定義
  !----------------------------------------------
  !
#ifdef OGCM_VARIABLE
  call set_hgrids(flvgrd)
#else /* OGCM_VARIABLE */
  call set_hgrids
#endif /* OGCM_VARIABLE */

  !----------------------------------------------
  !  スケールファクタの読み込み
  !----------------------------------------------
  call read_scale(flsclf)
  !
  !==============================================
  !
  open (mtin_ssh, file=flin_ssh, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)
  write(*, *) 'ssh in  :', trim(flin_ssh)
  !
  !  SSH
  read (mtin_ssh, rec=1) d2_r4
  ssh(:,:) = atexl(:,:,1)*real(d2_r4(:,:),8)
  !
  close (mtin_ssh)
  !
  !----------------------------------------------
  ! 体積重みをつけて全球で積算
  !----------------------------------------------

  area_t(1:imut, 1:jmut, 1:km) = 0.0d0
  volumt(1:imut, 1:jmut, 1:km) = 0.0d0
  deptht(1:imut, 1:jmut, 1:km) = 0.0d0

  nendidx(1:jmut) = 1.d0
#if defined OGCM_JOT || defined OGCM_TRIPOLAR
  nendidx(jmut-2) = 0.5d0
  nendidx(jmut-1:jmut) = 0.d0
#endif /* OGCM_JOT || OGCM_TRIPOLAR */
  tvol = 0.d0
  !
  do j = 2, jmut-2
    do i = 3, imut-2
      dztu = 0.0d0
      do k = 1, ksgm
        vol = dz(k) * (thcksgm+ssh(i, j)) * thcksgmr * nendidx(j) &
             &   * (aexl(i-1, j  , k)* a_br(i-1, j  ) +aexl(i, j  , k)*a_bl(i, j  ) &
             &     +aexl(i-1, j-1, k)* a_tr(i-1, j-1) +aexl(i, j-1, k)*a_tl(i, j-1))
        tvol = tvol + vol
        volumt(i, j, k) = vol

        dztl = atexl(i,j,k) * dz(k) * (thcksgm+ssh(i, j)) * thcksgmr
        if (k == 1) then
          deptht(i,j,k) = 0.5d0 * dztl
        else
          deptht(i,j,k) = atexl(i,j,k) * (deptht(i,j,k-1) + 0.5d0 * (dztu + dztl))
        end if
        dztu = dztl

        area_t(i, j, k) = nendidx(j) &
             &   * (aexl(i-1, j  , k)* a_br(i-1, j  ) +aexl(i, j  , k)*a_bl(i, j  ) &
             &     +aexl(i-1, j-1, k)* a_tr(i-1, j-1) +aexl(i, j-1, k)*a_tl(i, j-1))
      end do

      do k = ksgm + 1, kmax
        vol = nendidx(j) *                                                       &
          &   ( dzu(i-1, j  , k) * a_br(i-1, j  ) +dzu(i, j  , k) * a_bl(i, j  ) &
          &    +dzu(i-1, j-1, k) * a_tr(i-1, j-1) +dzu(i, j-1, k) * a_tl(i, j-1))
        tvol = tvol + vol
        volumt(i, j, k) = vol

        dztl = max(dzu(i-1,j,k),dzu(i,j,k),dzu(i-1,j-1,k),dzu(i,j-1,k))
        deptht(i,j,k) = atexl(i,j,k) * (deptht(i,j,k-1) + 0.5d0 * (dztu + dztl))
        dztu = dztl

        area_t(i, j, k) = nendidx(j) &
          &   * (aexl(i-1, j  , k)* a_br(i-1, j  ) +aexl(i, j  , k)*a_bl(i, j  ) &
          &     +aexl(i-1, j-1, k)* a_tr(i-1, j-1) +aexl(i, j-1, k)*a_tl(i, j-1))

      end do
#ifdef OGCM_BBL
      if ((ktbtm(i,j) >= ksgm) .and. (atexlbbl(i,j,1) == 1.0d0)) then

        ku = ktbtm(i,j)-1
        dztl = max(dzu(i-1,j,km),dzu(i,j,km),dzu(i-1,j-1,km),dzu(i,j-1,km))
        dztu = max(dzu(i-1,j,ku),dzu(i,j,ku),dzu(i-1,j-1,ku),dzu(i,j-1,ku))
        deptht(i,j,ktbtm(i,j)) = deptht(i,j,ku) + 0.5d0 * (dztu + dztl)

        if (volumt(i,j,ktbtm(i,j)) == 0.0d0) then
          vol = nendidx(j) *                                                         &
               &   ( dzu(i-1, j  , km) * a_br(i-1, j  ) +dzu(i, j  , km) * a_bl(i, j  ) &
               &    +dzu(i-1, j-1, km) * a_tr(i-1, j-1) +dzu(i, j-1, km) * a_tl(i, j-1))
          volumt(i, j, ktbtm(i,j)) = vol
          tvol = tvol + vol
        else
          write(6,*) 'inconsistency ', i,j,ktbtm(i,j)
        end if

        if (area_t(i,j,ktbtm(i,j)) == 0.0d0) then
          area_t(i, j, ktbtm(i,j)) = nendidx(j) &
               &   * (aexl(i-1, j  , km)* a_br(i-1, j  ) +aexl(i, j  , km)*a_bl(i, j  ) &
               &     +aexl(i-1, j-1, km)* a_tr(i-1, j-1) +aexl(i, j-1, km)*a_tl(i, j-1))
        else
          write(6,*) 'inconsistency ', i,j,ktbtm(i,j)
        end if

      end if
#endif /* OGCM_BBL */
    end do
  end do
  !
  !----------------------------------------------
  !
  tvol = tvol * 1.d-6  !  [cm^3] to [m^3]
  write(*,*) 'Sea Water Total Volume :', tvol, '[m^3]'
  !
  tmass = tvol * rho0 * 1.d3
  write(*,*) 'Sea Water Total Mass :', tmass, '[kg]'
  !
  open (mtoutv, file=floutv, form='unformatted', access='direct', recl=4)
  write(mtoutv, rec=1) real(tvol, 4)
  close (mtoutv)
  !
  open (mtoutm, file=floutm, form='unformatted', access='direct', recl=4)
  write(mtoutm, rec=1) real(tmass, 4)
  close (mtoutm)
  !
  open (mtoutv_txt, file=floutv_txt)
  write(mtoutv_txt, *)    tvol
  close (mtoutv_txt)
  !
  open (mtoutm_txt, file=floutm_txt)
  write(mtoutm_txt, *)    tmass
  close (mtoutm_txt)
  !----------------------------------------------

  do i = 1, imut
    nat_alatt(i,1:jmut) = alatt(1:jmut)
    nat_alatu(i,1:jmut) = alatu(1:jmut)
  end do

  do j = 1, jmut
    nat_alont(1:imut,j) = alont(1:imut)
    nat_alonu(1:imut,j) = alonu(1:imut)
  end do

  !----------------------------------------------
  ! geographical lon/lat

  do j = 1, jmut
    do i = 1, imut
      mu_u = alonu(i)*radian_r
      psi_u = alatu(j)*radian_r
      call mp2lp(lambdau, phiu, mu_u, psi_u)
      lon_trn = lambdau * radian
      if (lon_trn < 0.0d0) then
        lon_trn = lon_trn + 360.d0
      end if
      if (lon_trn >= 360.0) then
        lon_trn = lon_trn - 360.d0
      end if
      lat_trn = phiu * radian
      glonu(i,j) = lon_trn
      glatu(i,j) = lat_trn
!      write(6,*) lon_trn, lat_trn
    end do
  end do

  do j = 1, jmut
    do i = 1, imut
      mu_u = alont(i)*radian_r
      psi_u = alatt(j)*radian_r
      call mp2lp(lambdau, phiu, mu_u, psi_u)
      lon_trn = lambdau * radian
      if (lon_trn < 0.0d0) then
        lon_trn = lon_trn + 360.d0
      end if
      if (lon_trn >= 360.0) then
        lon_trn = lon_trn - 360.d0
      end if
      lat_trn = phiu * radian
      glont(i,j) = lon_trn
      glatt(i,j) = lat_trn
!      write(6,*) lon_trn, lat_trn
    end do
  end do

  !-----

  do j = 2, jmut
    do i = 2, imut
      glatt_corner(1,i,j) = glatu(i-1,j-1)
      glatt_corner(2,i,j) = glatu(i  ,j-1)
      glatt_corner(3,i,j) = glatu(i-1,j)
      glatt_corner(4,i,j) = glatu(i  ,j)
      glont_corner(1,i,j) = glonu(i-1,j-1)
      glont_corner(2,i,j) = glonu(i  ,j-1)
      glont_corner(3,i,j) = glonu(i-1,j)
      glont_corner(4,i,j) = glonu(i  ,j)
    end do
  end do

  j = 1
  do i = 2, imut
    glatt_corner(1,i,j) = glatu(i-1,j) - (glatu(i-1,j+1) - glatu(i-1,j))
    glatt_corner(2,i,j) = glatu(i  ,j) - (glatu(i  ,j+1) - glatu(i  ,j))
    glatt_corner(3,i,j) = glatu(i-1,j)
    glatt_corner(4,i,j) = glatu(i  ,j)

    glont_corner(1,i,j) = glonu(i-1,j)
    glont_corner(2,i,j) = glonu(i  ,j)
    glont_corner(3,i,j) = glonu(i-1,j)
    glont_corner(4,i,j) = glonu(i  ,j)
  end do

  i = 1
  do j = 1, jmut
    glatt_corner(1,i,j) = glatt_corner(1,imut-3,j)
    glatt_corner(2,i,j) = glatt_corner(2,imut-3,j)
    glatt_corner(3,i,j) = glatt_corner(3,imut-3,j)
    glatt_corner(4,i,j) = glatt_corner(4,imut-3,j)

    glont_corner(1,i,j) = glont_corner(1,imut-3,j)
    glont_corner(2,i,j) = glont_corner(2,imut-3,j)
    glont_corner(3,i,j) = glont_corner(3,imut-3,j)
    glont_corner(4,i,j) = glont_corner(4,imut-3,j)
  end do

  !-----

  do j = 1, jmut - 1
    do i = 1, imut - 1
      glatu_corner(1,i,j) = glatt(i  ,j  )
      glatu_corner(2,i,j) = glatt(i+1,j  )
      glatu_corner(3,i,j) = glatt(i  ,j+1)
      glatu_corner(4,i,j) = glatt(i+1,j+1)
      glonu_corner(1,i,j) = glont(i  ,j  )
      glonu_corner(2,i,j) = glont(i+1,j  )
      glonu_corner(3,i,j) = glont(i  ,j+1)
      glonu_corner(4,i,j) = glont(i+1,j+1)
    end do
  end do

  j = jmut
  do i = 2, imut - 1
    glatu_corner(1,i,j) = glatt(i  ,j)
    glatu_corner(2,i,j) = glatt(i+1,j)
    glatu_corner(3,i,j) = glatt(imut-i+2,jmut-5)
    glatu_corner(4,i,j) = glatt(imut-i+1,jmut-5)

    glonu_corner(1,i,j) = glont(i  ,j)
    glonu_corner(2,i,j) = glont(i+1,j)
    glonu_corner(3,i,j) = glont(imut-i+2,jmut-5)
    glonu_corner(4,i,j) = glont(imut-i+1,jmut-5)
  end do

  i = 1
  do j = 1, jmut
    glatu_corner(1,i,j) = glatu_corner(1,imut-3,j)
    glatu_corner(2,i,j) = glatu_corner(2,imut-3,j)
    glatu_corner(3,i,j) = glatu_corner(3,imut-3,j)
    glatu_corner(4,i,j) = glatu_corner(4,imut-3,j)

    glonu_corner(1,i,j) = glonu_corner(1,imut-3,j)
    glonu_corner(2,i,j) = glonu_corner(2,imut-3,j)
    glonu_corner(3,i,j) = glonu_corner(3,imut-3,j)
    glonu_corner(4,i,j) = glonu_corner(4,imut-3,j)
  end do

  i = imut
  do j = 1, jmut
    glatu_corner(1,i,j) = glatu_corner(1,4,j)
    glatu_corner(2,i,j) = glatu_corner(2,4,j)
    glatu_corner(3,i,j) = glatu_corner(3,4,j)
    glatu_corner(4,i,j) = glatu_corner(4,4,j)

    glonu_corner(1,i,j) = glonu_corner(1,4,j)
    glonu_corner(2,i,j) = glonu_corner(2,4,j)
    glonu_corner(3,i,j) = glonu_corner(3,4,j)
    glonu_corner(4,i,j) = glonu_corner(4,4,j)
  end do

  !----------------------------------------------
  ! lon/lat for graphics

#ifdef OGCM_TRIPOLAR

  grp_dxtdeg(1:imut) = dxtdeg(1:imut)
  do i = 1, imut - 1
    grp_dxudeg(i) = 0.5d0*(grp_dxtdeg(i)+grp_dxtdeg(i+1))
  end do
  grp_dxudeg(imut) = grp_dxudeg(4) ! CYCLIC

  grp_lont(1) = 80.0d0 - 2.0d0 * grp_dxtdeg(1)
  grp_lonu(1) = grp_lont(1) + 0.5d0 * grp_dxtdeg(1)
  do i = 2, imut
    grp_lont(i) = grp_lont(i-1) + grp_dxudeg(i-1)
    grp_lonu(i) = grp_lonu(i-1) + grp_dxtdeg(i)
  end do
  do i = 1, imut
    write(6,*) i, grp_lont(i), grp_lonu(i)
    grp_alont(i,1:jmut) = grp_lont(i)
    grp_alonu(i,1:jmut) = grp_lonu(i)
  end do

  !----------

  do j = 1, jmut
    if (j >= 287) then
      grp_dytdeg(j) = 0.325d0
    else if (j < 286) then
      grp_dytdeg(j) = dytdeg(j)
    else
      grp_dytdeg(j) = 0.25d0 + 0.5d0 * 0.325d0
    end if
  end do
!  do j = 1, jmut
!    write(6,*) j, dytdeg(j), grp_dytdeg(j)
!  end do

  do j = 1, jmut - 1
    grp_dyudeg(j) = 0.5d0*(grp_dytdeg(j)+grp_dytdeg(j+1))
  end do
  grp_dyudeg(jmut) = grp_dytdeg(jmut)

  grp_latt(1) = slat0 - grp_dytdeg(1)
  grp_latu(1) = grp_latt(1) + 0.5d0 * grp_dytdeg(1)
  do j = 2, jmut
    grp_latt(j) = grp_latt(j-1) + grp_dyudeg(j-1)
    grp_latu(j) = grp_latu(j-1) + grp_dytdeg(j)
  end do
  do j = 1, jmut
    write(6,*) j, grp_latt(j), grp_latu(j)
    grp_alatt(1:imut,j) = grp_latt(j)
    grp_alatu(1:imut,j) = grp_latu(j)
  end do
#endif /* OGCM_TRIPOLAR */

  if (l_gridinfo) then

    deptht(1:imut,1:jmut,1:km) = deptht(1:imut,1:jmut,1:km) * 1.0d-2
    area_t(1:imut,1:jmut,1:km) = area_t(1:imut,1:jmut,1:km) * 1.0d-4
    volumt(1:imut,1:jmut,1:km) = volumt(1:imut,1:jmut,1:km) * 1.0d-6

    open (mtoutar, file=floutare, form='unformatted', access='direct', recl=4*imut*jmut)
    do k = 1, km
      write(mtoutar, rec=k) real(area_t(1:imut,1:jmut,k), 4)
    end do
    close (mtoutar)

    open (mtoutvl, file=floutvol, form='unformatted', access='direct', recl=4*imut*jmut)
    do k = 1, km
      write(mtoutvl, rec=k) real(volumt(1:imut,1:jmut,k), 4)
    end do
    close (mtoutvl)

    open (mtoutdp, file=floutdep, form='unformatted', access='direct', recl=4*imut*jmut)
    do k = 1, km
      write(mtoutdp, rec=k) real(deptht(1:imut,1:jmut,k), 4)
    end do
    close (mtoutdp)
    
    open (mtouttp, file=flouttp, form='unformatted', access='direct', recl=4*imut*jmut)
    write(mtouttp, rec=1) real(glont(1:imut,1:jmut), 4)
    write(mtouttp, rec=2) real(glatt(1:imut,1:jmut), 4)
    close(mtouttp)

    open (mtoutup, file=floutup, form='unformatted', access='direct', recl=4*imut*jmut)
    write(mtoutup, rec=1) real(glonu(1:imut,1:jmut), 4)
    write(mtoutup, rec=2) real(glatu(1:imut,1:jmut), 4)
    close(mtoutup)

    open (mtouttp, file=flouttp_corner, form='unformatted', access='direct', recl=4*4*imut*jmut)
    write(mtouttp, rec=1) real(glont_corner(1:4,1:imut,1:jmut), 4)
    write(mtouttp, rec=2) real(glatt_corner(1:4,1:imut,1:jmut), 4)
!    d2_r4(1:imut,1:jmut) = real(glont_corner(1,1:imut,1:jmut), 4)
!    write(mtouttp, rec=1) d2_r4
!    d2_r4(1:imut,1:jmut) = real(glont_corner(2,1:imut,1:jmut), 4)
!    write(mtouttp, rec=2) d2_r4
!    d2_r4(1:imut,1:jmut) = real(glont_corner(3,1:imut,1:jmut), 4)
!    write(mtouttp, rec=3) d2_r4
!    d2_r4(1:imut,1:jmut) = real(glont_corner(4,1:imut,1:jmut), 4)
!    write(mtouttp, rec=4) d2_r4
!    d2_r4(1:imut,1:jmut) = real(glatt_corner(1,1:imut,1:jmut), 4)
!    write(mtouttp, rec=5) d2_r4
!    d2_r4(1:imut,1:jmut) = real(glatt_corner(2,1:imut,1:jmut), 4)
!    write(mtouttp, rec=6) d2_r4
!    d2_r4(1:imut,1:jmut) = real(glatt_corner(3,1:imut,1:jmut), 4)
!    write(mtouttp, rec=7) d2_r4
!    d2_r4(1:imut,1:jmut) = real(glatt_corner(4,1:imut,1:jmut), 4)
!    write(mtouttp, rec=8) d2_r4
    close(mtouttp)

    open (mtoutup, file=floutup_corner, form='unformatted', access='direct', recl=4*4*imut*jmut)
    write(mtoutup, rec=1) real(glonu_corner(1:4,1:imut,1:jmut), 4)
    write(mtoutup, rec=2) real(glatu_corner(1:4,1:imut,1:jmut), 4)
!    d2_r4(1:imut,1:jmut) = real(glonu_corner(1,1:imut,1:jmut), 4)
!    write(mtoutup, rec=1) d2_r4
!    d2_r4(1:imut,1:jmut) = real(glonu_corner(2,1:imut,1:jmut), 4)
!    write(mtoutup, rec=2) d2_r4
!    d2_r4(1:imut,1:jmut) = real(glonu_corner(3,1:imut,1:jmut), 4)
!    write(mtoutup, rec=3) d2_r4
!    d2_r4(1:imut,1:jmut) = real(glonu_corner(4,1:imut,1:jmut), 4)
!    write(mtoutup, rec=4) d2_r4
!    d2_r4(1:imut,1:jmut) = real(glatu_corner(1,1:imut,1:jmut), 4)
!    write(mtoutup, rec=5) d2_r4
!    d2_r4(1:imut,1:jmut) = real(glatu_corner(2,1:imut,1:jmut), 4)
!    write(mtoutup, rec=6) d2_r4
!    d2_r4(1:imut,1:jmut) = real(glatu_corner(3,1:imut,1:jmut), 4)
!    write(mtoutup, rec=7) d2_r4
!    d2_r4(1:imut,1:jmut) = real(glatu_corner(4,1:imut,1:jmut), 4)
!    write(mtoutup, rec=8) d2_r4
    close(mtoutup)

    open (mtout_tmp, file=flout_lattg, form='unformatted', access='direct', recl=4*jmut)
    write(mtout_tmp, rec=1) real(grp_latt(1:jmut), 4)
    close(mtout_tmp)

    open (mtout_tmp, file=flout_lontg, form='unformatted', access='direct', recl=4*imut)
    write(mtout_tmp, rec=1) real(grp_lont(1:imut), 4)
    close(mtout_tmp)

    open (mtout_tmp, file=flout_latug, form='unformatted', access='direct', recl=4*jmut)
    write(mtout_tmp, rec=1) real(grp_latu(1:jmut), 4)
    close(mtout_tmp)

    open (mtout_tmp, file=flout_lonug, form='unformatted', access='direct', recl=4*imut)
    write(mtout_tmp, rec=1) real(grp_lonu(1:imut), 4)
    close(mtout_tmp)

    open (mtout_tmp, file=flout_lonlat_t, form='unformatted', access='direct', recl=4*imut*jmut)
    write(mtout_tmp, rec=1) real(grp_alont(1:imut,1:jmut), 4)
    write(mtout_tmp, rec=2) real(grp_alatt(1:imut,1:jmut), 4)
    close(mtout_tmp)

    open (mtout_tmp, file=flout_lonlat_u, form='unformatted', access='direct', recl=4*imut*jmut)
    write(mtout_tmp, rec=1) real(grp_alonu(1:imut,1:jmut), 4)
    write(mtout_tmp, rec=2) real(grp_alatu(1:imut,1:jmut), 4)
    close(mtout_tmp)

    open (mtout_tmp, file=flout_nat_lonlat_t, form='unformatted', access='direct', recl=4*imut*jmut)
    write(mtout_tmp, rec=1) real(nat_alont(1:imut,1:jmut), 4)
    write(mtout_tmp, rec=2) real(nat_alatt(1:imut,1:jmut), 4)
    close(mtout_tmp)

    open (mtout_tmp, file=flout_nat_lonlat_u, form='unformatted', access='direct', recl=4*imut*jmut)
    write(mtout_tmp, rec=1) real(nat_alonu(1:imut,1:jmut), 4)
    write(mtout_tmp, rec=2) real(nat_alatu(1:imut,1:jmut), 4)
    close(mtout_tmp)

  end if

!====================================================
end program volo
