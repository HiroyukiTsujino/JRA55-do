! -*-F90-*-
!hs2zm_w.F90
!====================================================
!
! Make Zonal Mean of W-points
!
!====================================================
program zonal_mean_w
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ksgm, dz,            &
  &   pi, radian, radian_r, &
  &   slat0, slon0,        &
  &   nplat, nplon,        &
  &   splat, splon,        &
  &   ro,    cp
#ifndef OGCM_VARIABLE
  use oc_mod_param, only : &
  &   dxtdgc, dytdgc
#endif /* ! OGCM_VARIABLE */
  !
  use oc_mod_trnsfrm, only  : &
  &   set_abc, mp2lp,         &
  &   length_on_sphere
  !
  use oc_structure,    only : &
  &   read_topo,              & !----------------------------
  &   aexl, atexl,            & ! 海陸インデックス
  &   dzu,                    & ! UVボックスの厚さ
  &   dep,                    & ! UVTSボックス境界（上面）の深さ
  &   thcksgm, thcksgmr,      & ! σ層の厚さ、逆数
#ifdef OGCM_BBL
  &   aexlbbl, atexlbbl,      & ! BBLインデックス
  &   kbtm, ktbtm,            &
#endif /* OGCM_BBL */
  &   set_hgrids,             & !----------------------------
  &   dxtdeg, dytdeg,         & ! T点を中心とするモデル格子間隔(度)
  &   dxudeg, dyudeg,         & ! U点を中心とするモデル格子間隔(度)
  &   slat, slon,             & ! 
  &   alatt, alatu,           &
  &   alont, alonu,           &
  &   read_scale,             & !----------------------------
  &   a_tl  , a_tr  ,         & ! 格子面積
  &   a_bl  , a_br  ,         &
  &   dx_tl , dx_tr ,         & ! 東西長
  &   dx_bl , dx_br ,         &
  &   dy_tl , dy_tr ,         & ! 南北長
  &   dy_bl , dy_br
  !
  !----------------------------------------------
  !
  implicit none
  !
  ! 地理座標パラメタ
  ! 
  real(8), parameter :: dlatg =   1.0d0  ! 緯度格子間隔
  real(8), parameter :: slatg  = real(slat0, 8)
  integer(4), parameter :: jmgeo = 75 + 1 - slat0 ! 1 degree interval -78 to 75
  !
  ! 海洋モデル地形
  !
  integer(4) :: ibas(imut, jmut)      ! basin区分, UV-Grid
  !
  !             0:LAND, 1:ATL, 2:PAC, 3:IND, 4:MED, 9:SO
  !
  integer(4) :: mask_glb(imut, jmut) ! 全球海洋      1 or -1
  integer(4) :: mask_atl(imut, jmut) ! 大西洋+地中海 1 or -1
  integer(4) :: mask_pac(imut, jmut) ! 太平洋        1 or -1
  integer(4) :: mask_ind(imut, jmut)
  !
  real(8)    :: ibm(imut, jmut)      ! 積算で使用する海盆マスク(1 or 0)
  real(8)    :: mu_u, psiu, lambdau, phiu
  !
  real(8)    :: t(imut, jmut, km)
  real(8)    :: ssh(imut, jmut)
  !
  !
  real(8)    :: phi_std_degree ! この緯度に沿って地球を一周
  !
  real(8)    :: vol_tstotal(jmgeo, km)
  real(8)    :: mask_jk(jmgeo, km)
  real(8)    :: t_v(jmgeo, km)
  real(8)    :: tzmg(jmgeo, km)  ! Global
  real(8)    :: tzma(jmgeo, km)  ! Atlantic
  real(8)    :: tzmp(jmgeo, km)  ! Pacific
  real(8)    :: tzmi(jmgeo, km)  ! Indian
  real(4)    :: tzmgf(jmgeo, km)  ! Global
  real(4)    :: tzmaf(jmgeo, km)  ! Atlantic
  real(4)    :: tzmpf(jmgeo, km)  ! Pacific
  real(4)    :: tzmif(jmgeo, km)  ! Indian
  !
  !
  real(4) :: undef_in
  real(4) :: undef8_in
  real(4), parameter :: undefgd = -9.99e33
  real(4)    :: d3_r4(imut,jmut,km)
  real(4)    :: d2_r4(imut,jmut)
  !
  ! 入出力ファイル
  !
  integer(4) :: nstep             ! 読み出しセット数
  character(len=256)    :: flin ! 入力ファイル
  character(len=256)    :: flin_ssh ! 入力ファイル
  character(len=256)    :: flout  ! 出力ファイル
  character(len=256)    :: fltopo ! 海底地形ファイル
  character(len=256)    :: flsclf ! スケールファクター・ファイル
  character(len=256)    :: flibas ! basinインデックスファイル
#ifdef OGCM_VARIABLE
  character(len=256)    :: file_vgrid ! 可変格子定義ファイル
#endif /* OGCM_VARIABLE */
  !
  namelist /nml_zm/  flin, flin_ssh, undef_in, flout, fltopo, flsclf, flibas
#ifdef OGCM_VARIABLE
  namelist /inflg/ file_vgrid
#endif /* OGCM_VARIABLE */
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mttmp    = 79
  integer(4), parameter :: mtin   = 82
  integer(4), parameter :: mtin_ssh = 84
  integer(4), parameter :: mtout    = 86
  !
  integer(4), parameter :: lrec_out = jmgeo*km*4
  !
  integer(4) :: irecw
  integer(4) :: i, j, k, m, jj
  integer(4) :: jg
  character(80) :: fmt_ibas
  real(8)    :: wrk1, wrk2
  !
  !==============================================
  !
  ! 入力パラメタ規定値
  !
  flin    = 'hs_t.d'
  flin_ssh= 'hs_ssh.d'
  flout   = 'zonalmean.gd'
  fltopo  = 'topo.d'
  flsclf  = 'scale_factor.d'
  flibas  = 'basin_map.txt'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml_zm)
  print *,'flin     :', trim(flin)
  print *,'flin_ssh :', trim(flin_ssh)
  print *,'flout    :', trim(flout)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  print *,'flibas   :', trim(flibas)
  print *,'undef_in :', undef_in
  undef8_in = real(undef_in,8)
#ifdef OGCM_VARIABLE
  read(unit=5, inflg) ! file_vgrid
#endif /* OGCM_VARIABLE */
  !
  !----------------------------------------------
  ! 海洋モデル格子情報等の準備
  !
  ! 座標変換のパラメータを決める
  call set_abc ( nplat, nplon, splat, splon )
  !
  !----------------------------------------------
#ifdef OGCM_VARIABLE
  call set_hgrids(file_vgrid)
#else /* OGCM_VARIABLE */
  call set_hgrids
#endif /* OGCM_VARIABLE */
  !
  !----------------------------------------------
  call read_topo(fltopo)
  !
  !----------------------------------------------
  call read_scale(flsclf)
  !
  !----------------------------------------------
  !  basinインデックス読み込み
  !
  write(fmt_ibas,"(a,i4,a)") "(i6,",imut,"i1)"
  open(mttmp, file=flibas, form="formatted")  
  do j = jmut, 1, -1
  ! read(mttmp, fmt="(i6,364i1)") jj,(ibas(i,j), i=1, imut)
    read(mttmp, fmt=fmt_ibas,iostat=ios) jj,(ibas(i,j), i=1, imut)
    if(ios /= 0) write(*, *) 'reading error in file:', flibas
    if ( jj /= j ) then
      print *,' error in ',trim(flibas)
      stop 999
    endif
  ! write(6,fmt=fmt_ibas) jj,(ibas(i,j), i=1, imut)
  end do
  close(mttmp)
  !
  mask_glb(1:imut, 1:jmut) = -1
  mask_atl(1:imut, 1:jmut) = -1
  mask_pac(1:imut, 1:jmut) = -1
  mask_ind(1:imut, 1:jmut) = -1
  !
  do j = 1, jmut
    do i = 1, imut
      m = ibas(i, j)
      if(m > 0) then
        mask_glb(i, j) = 1           ! Ocean: 1, Land: -1
!        if(m == 1 .or. m == 4) mask_atl(i, j) = 1 ! Atlantic or Mediterranean
        if(m == 1)             mask_atl(i, j) = 1 ! Atlantic
        if(m == 2)             mask_pac(i, j) = 1 ! Pacific
        if(m == 3)             mask_ind(i, j) = 1 ! Indian
      end if
    end do
  end do
  !
  !  Pacific side half of the Arctic Sea is excluded from ATL
  !
  do j = 1, jmut
    do i = 1, imut
      mu_u = alonu(i)*radian_r
      psiu = alatu(j)*radian_r
      call mp2lp(lambdau, phiu, mu_u, psiu)
      if(phiu > 60.d0*radian_r .and. cos(lambdau) < 0.d0) mask_atl(i, j) = -1
    end do
  end do
  !
  !==============================================
  !
  ! 入出力ファイルオープン
  !
  open (mtin, file=flin, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_ssh, file=flin_ssh, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)
  !
  open (mtout, file=flout, form='unformatted',&
    &  access='direct', recl=lrec_out )
  irecw=1
  !
  m = 1
    !
    !
    read (mtin, rec=1) d3_r4
    t(:,:,:) = atexl(:,:,:)*real(d3_r4(:,:,:),8)
    !
    t(1:2,         1:jmut, 1:km)=t(imut-3:imut-2, 1:jmut, 1:km)
    t(imut-1:imut, 1:jmut, 1:km)=t(3:4,           1:jmut, 1:km)
    !
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
    !
    !--------------------
    !  Global
    !--------------------
    print *, '       Global '
    !
    do j = 1, jmut
      do i = 1, imut
        ibm(i, j) = 0.5d0 + 0.5d0*real(mask_glb(i, j), 8)
      end do
    end do
    !
    vol_tstotal(1:jmgeo, 1:km)=0.d0
    t_v(1:jmgeo, 1:km)=0.d0
    !
    do jg = 1, jmgeo
      !
      phi_std_degree = slatg + dlatg*(jg-1)
      call around_the_world_wp(jg, phi_std_degree)
      !
    end do
    !
    do k = 1, km
      do j = 1, jmgeo
        mask_jk(j, k) = 0.5d0 + sign(0.5d0, vol_tstotal(j, k) - 1.d-30)
      end do
    end do
    !
    do k=1, km
      do j=1, jmgeo
        tzmg(j, k) = t_v(j, k) * mask_jk(j, k) / (vol_tstotal(j, k) +1.d0 -mask_jk(j, k))
      end do
    end do
    !
    where(mask_jk(1:jmgeo, 1:km) < 0.5d0)
      tzmg(1:jmgeo, 1:km) = undefgd
    end where
    !
    do k = 1, km
      tzmgf(1:jmgeo, km+1-k) = real(tzmg(1:jmgeo, k), 4)
    end do
    !
    !--------------------
    !  Atlantic
    !--------------------
    print *, '       Atlantic '
    !
    do j = 1, jmut
      do i= 1, imut
        ibm(i, j) = 0.5d0 + 0.5d0*real(mask_atl(i, j), 8)
      end do
    end do
    !
    vol_tstotal(1:jmgeo, 1:km)=0.d0
    t_v(1:jmgeo, 1:km)=0.d0
    !
    do jg=1, jmgeo
      !
      phi_std_degree = slatg + dlatg*(jg-1)
      call around_the_world_wp(jg, phi_std_degree)
      !
    end do
    !
    do k=1, km
      do j=1, jmgeo
        mask_jk(j, k) = 0.5d0 + sign(0.5d0, vol_tstotal(j, k) - 1.d-30)
      end do
    end do
    !
    do k=1, km
      do j=1, jmgeo
        tzma(j, k) = t_v(j, k) * mask_jk(j, k) / (vol_tstotal(j, k) +1.d0 -mask_jk(j, k))
      end do
    end do
    !
    where(mask_jk(1:jmgeo, 1:km) < 0.5d0)
      tzma(1:jmgeo, 1:km) = undefgd
    end where
    !
    do k=1, km
      tzmaf(1:jmgeo, km+1-k) = real(tzma(1:jmgeo, k), 4)
    end do
    !
    !--------------------
    !  Pacific
    !--------------------
    print *, '       Pacific '
    !
    do j = 1, jmut
      do i= 1, imut
        ibm(i, j) = 0.5d0 + 0.5d0*real(mask_pac(i, j), 8)
      end do
    end do
    !
    vol_tstotal(1:jmgeo, 1:km)=0.d0
    t_v(1:jmgeo, 1:km)=0.d0
    !
    do jg=1, jmgeo
      !
      phi_std_degree = slatg + dlatg*(jg-1)
      call around_the_world_wp(jg, phi_std_degree)
      !
    end do
    !
    do k=1, km
      do j=1, jmgeo
        mask_jk(j, k) = 0.5d0 + sign(0.5d0, vol_tstotal(j, k) - 1.d-30)
      end do
    end do
    !
    do k=1, km
      do j=1, jmgeo
        tzmp(j, k) = t_v(j, k) * mask_jk(j, k) / (vol_tstotal(j, k) +1.d0 -mask_jk(j, k))
      end do
    end do
    !
    where(mask_jk(1:jmgeo, 1:km) < 0.5d0)
      tzmp(1:jmgeo, 1:km) = undefgd
    end where
    !
    do k=1, km
      tzmpf(1:jmgeo, km+1-k) = real(tzmp(1:jmgeo, k), 4)
    end do
    !
    !--------------------
    !  Indian Ocean
    !--------------------
    print *, '       Indian '
    !
    do j = 1, jmut
      do i= 1, imut
        ibm(i, j) = 0.5d0 + 0.5d0*real(mask_ind(i, j), 8)
      end do
    end do
    !
    vol_tstotal(1:jmgeo, 1:km)=0.d0
    t_v(1:jmgeo, 1:km)=0.d0
    !
    do jg=1, jmgeo
      !
      phi_std_degree = slatg + dlatg*(jg-1)
      call around_the_world_wp(jg, phi_std_degree)
      !
    end do
    !
    do k=1, km
      do j=1, jmgeo
        mask_jk(j, k) = 0.5d0 + sign(0.5d0, vol_tstotal(j, k) - 1.d-30)
      end do
    end do
    !
    do k=1, km
      do j=1, jmgeo
        tzmi(j, k) = t_v(j, k) * mask_jk(j, k) / (vol_tstotal(j, k) +1.d0 -mask_jk(j, k))
      end do
    end do
    !
    where(mask_jk(1:jmgeo, 1:km) < 0.5d0)
      tzmi(1:jmgeo, 1:km) = undefgd
    end where
    !
    do k=1, km
      tzmif(1:jmgeo, km+1-k) = real(tzmi(1:jmgeo, k), 4)
    end do
    !
    !--------------------------------------------
    write ( mtout, rec=irecw )  tzmgf(1:jmgeo, 1:km)
    irecw=irecw+1
    !
    write ( mtout, rec=irecw )  tzmaf(1:jmgeo, 1:km)
    irecw=irecw+1
    !
    write ( mtout, rec=irecw )  tzmpf(1:jmgeo, 1:km)
    irecw=irecw+1
    !
    write ( mtout, rec=irecw )  tzmif(1:jmgeo, 1:km)
    irecw=irecw+1
    !
    !---------------------------------------------------
  !
  close ( mtin )
  close ( mtin_ssh)
  close ( mtout )
  !
contains
!====================================================
!
!  指定緯度で世界一周
!
!====================================================
subroutine around_the_world_wp(jg, phi_std_degree)
  !
  ! summing values on w-points
  !
  integer(4), intent(IN) :: jg
  real(8),    intent(IN) :: phi_std_degree
  !
  integer(4), parameter :: kmax = km-1
  !
  integer(4) :: i00, j00, ic, jc, jend
  !
  real(8)    :: phi_std
  real(8)    :: mu0,  mu2
  real(8)    :: psi0, psi1, psi2
  !
  real(8)    :: lambda00,  lambda20,  lambda01,  lambda02
  real(8)    :: phi00, phi20, phi01, phi02
  !
  real(8)    :: sqphi00, sqphi20, sqphi01, sqphi02
  real(8)    :: sqphimin
  real(8)    :: ustar, vstar
  real(8)    :: vol_ts
  real(8)    :: hl1, hl2, hl3, hl4
  !
  real(8)    :: t_done(imut, jmut)
  !
  integer(4) :: j, k
  !
  phi_std = phi_std_degree*radian_r
  !
  t_done(1:imut, 1:jmut) = 0.d0
  !
  t_done(1:2,         1:jmut) = 1.d0
  t_done(imut-1:imut, 1:jmut) = 1.d0
  t_done(1:imut, 1)      = 1.d0
  t_done(1:imut, jmut-1:jmut)   = 1.d0
  !
  i00 = 3
  mu0 = alont(i00)*radian_r
  sqphimin=pi*pi

  do j = 3, jmut-1
    ! TSレンジで探索
    psi0 = alatt(j)*radian_r
    call mp2lp(lambda00, phi00, mu0, psi0)
    sqphi00 = (phi00-phi_std)*(phi00-phi_std)
    if(sqphi00 < sqphimin) then
      sqphimin = sqphi00
      j00 = j
    end if
  end do
  !
  mu0  = alont(i00)*radian_r
  !mu2  = alont(i00+1)*radian_r ! mu2 is not used here
  psi0 = alatt(j00)*radian_r
  psi2 = alatt(j00+1)*radian_r
  !
  call mp2lp(lambda00, phi00, mu0, psi0)
  call mp2lp(lambda02, phi02, mu0, psi2)
  !
  sqphi00 = (phi00-phi_std)*(phi00-phi_std)
  sqphi02 = (phi02-phi_std)*(phi02-phi_std)
  !
  if (sqphi00 > sqphi02) then
    j00 = j00 + 1
    psi0     = psi2
    lambda00 = lambda02
    phi00    = phi02
  end if
  !
  t_done(i00, j00) = 1.d0
  !
  write(*,*) ' start point = ',i00,j00,lambda00*radian,phi00*radian
  !
  ic = i00 + 1  !  移動先
  jc = j00
  !
  t_done(ic, jc) = 1.d0
  !
  !
  ! ustar, vstar の定義
  !
  !                i,j
  !       i-1,j uv--V*--uv i,j
  !              |      |
  !       i-1,j U*  TS  U* i,j
  !              |      |
  !     i-1,j-1 uv--V*--uv i,j-1
  !                i,j-1
  !
  do k = 1, kmax

    hl1 = a_br(i00-1,j00  )*dzu(i00-1,j00  ,k)*ibm(i00-1,j00  )  &
      & + a_bl(i00  ,j00  )*dzu(i00  ,j00  ,k)*ibm(i00  ,j00  )  &
      & + a_tr(i00-1,j00-1)*dzu(i00-1,j00-1,k)*ibm(i00-1,j00-1)  &
      & + a_tl(i00  ,j00-1)*dzu(i00  ,j00-1,k)*ibm(i00  ,j00-1)

#ifdef OGCM_BBL
    if (k+1 == ktbtm(i00,j00) .and. atexlbbl(i,j,1) == 1.0d0) then
      hl2 = a_br(i00-1,j00  )*dzu(i00-1,j00  ,km)*ibm(i00-1,j00  )  &
        & + a_bl(i00  ,j00  )*dzu(i00  ,j00  ,km)*ibm(i00  ,j00  )  &
        & + a_tr(i00-1,j00-1)*dzu(i00-1,j00-1,km)*ibm(i00-1,j00-1)  &
        & + a_tl(i00  ,j00-1)*dzu(i00  ,j00-1,km)*ibm(i00  ,j00-1)
    else
#endif /* OGCM_BBL */
      hl2 = a_br(i00-1,j00  )*dzu(i00-1,j00  ,k+1)*ibm(i00-1,j00  )  &
        & + a_bl(i00  ,j00  )*dzu(i00  ,j00  ,k+1)*ibm(i00  ,j00  )  &
        & + a_tr(i00-1,j00-1)*dzu(i00-1,j00-1,k+1)*ibm(i00-1,j00-1)  &
        & + a_tl(i00  ,j00-1)*dzu(i00  ,j00-1,k+1)*ibm(i00  ,j00-1)
#ifdef OGCM_BBL
    end if
#endif /* OGCM_BBL */

    if (k <= ksgm-1) then
      vol_ts = 0.5d0 * (hl1 + hl2) * (thcksgm + ssh(i00, j00)) * thcksgmr
    else if (k == ksgm) then
      vol_ts = 0.5d0 * (hl1 * (thcksgm + ssh(i00, j00)) * thcksgmr + hl2)
    else
      vol_ts = 0.5d0 * (hl1 + hl2)
    end if

    if (t(i00,j00,k) /= undef8_in) then
      vol_tstotal(jg,k) = vol_tstotal(jg,k) + vol_ts
      t_v(jg,k) = t_v(jg,k) + t(i00,j00,k) * vol_ts
    end if

  end do

  jend = j00  !  terminater

  LOOP_AROUND_THE_WORLD : do while (ic < imut)

    i00 = ic
    j00 = jc

    mu0  = alont(i00)*radian_r
    mu2  = alont(i00+1)*radian_r
    psi0 = alatt(j00)*radian_r
    psi1 = alatt(j00-1)*radian_r
    psi2 = alatt(j00+1)*radian_r

    call mp2lp(lambda00, phi00, mu0, psi0)
    call mp2lp(lambda20, phi20, mu2, psi0)
    call mp2lp(lambda02, phi02, mu0, psi2)
    call mp2lp(lambda01, phi01, mu0, psi1)

    sqphi20 = (phi20-phi_std)*(phi20-phi_std) +t_done(i00+1,j00  )
    sqphi02 = (phi02-phi_std)*(phi02-phi_std) +t_done(i00,  j00+1)
    sqphi01 = (phi01-phi_std)*(phi01-phi_std) +t_done(i00  ,j00-1)

    if(min(sqphi20, sqphi02, sqphi01) >= 1.d0) then
      exit LOOP_AROUND_THE_WORLD
    end if

    if(min(sqphi02, sqphi01) >= sqphi20) then ! 等号が必要

      ic = i00+1
      jc = j00

      do k = 1, kmax
        hl1 = a_br(ic-1,jc  )*dzu(ic-1,jc  ,k)*ibm(ic-1,jc  )  &
             & + a_bl(ic  ,jc  )*dzu(ic  ,jc  ,k)*ibm(ic  ,jc  )  &
             & + a_tr(ic-1,jc-1)*dzu(ic-1,jc-1,k)*ibm(ic-1,jc-1)  &
             & + a_tl(ic  ,jc-1)*dzu(ic  ,jc-1,k)*ibm(ic  ,jc-1)

#ifdef OGCM_BBL
        if (k+1 == ktbtm(ic,jc) .and. atexlbbl(i,j,1) == 1.0d0) then
          hl2 = a_br(ic-1,jc  )*dzu(ic-1,jc  ,km)*ibm(ic-1,jc  )  &
               & + a_bl(ic  ,jc  )*dzu(ic  ,jc  ,km)*ibm(ic  ,jc  )  &
               & + a_tr(ic-1,jc-1)*dzu(ic-1,jc-1,km)*ibm(ic-1,jc-1)  &
               & + a_tl(ic  ,jc-1)*dzu(ic  ,jc-1,km)*ibm(ic  ,jc-1)
        else
#endif /* OGCM_BBL */
          hl2 = a_br(ic-1,jc  )*dzu(ic-1,jc  ,k+1)*ibm(ic-1,jc  )  &
               & + a_bl(ic  ,jc  )*dzu(ic  ,jc  ,k+1)*ibm(ic  ,jc  )  &
               & + a_tr(ic-1,jc-1)*dzu(ic-1,jc-1,k+1)*ibm(ic-1,jc-1)  &
               & + a_tl(ic  ,jc-1)*dzu(ic  ,jc-1,k+1)*ibm(ic  ,jc-1)
#ifdef OGCM_BBL
        end if
#endif /* OGCM_BBL */

        if (k <= ksgm-1) then
          vol_ts = 0.5d0 * (hl1 + hl2) * (thcksgm + ssh(ic, jc)) * thcksgmr
        else if (k == ksgm) then
          vol_ts = 0.5d0 * (hl1 * (thcksgm + ssh(ic, jc)) * thcksgmr + hl2)
        else
          vol_ts = 0.5d0 * (hl1 + hl2)
        end if

        if (t(ic,jc,k) /= undef8_in) then
          vol_tstotal(jg,k) = vol_tstotal(jg,k) + vol_ts
          t_v(jg,k) = t_v(jg,k) + t(ic,jc,k) * vol_ts
        end if
      end do

    else if (min(sqphi20, sqphi01) >= sqphi02) then
      ic = i00
      jc = j00+1

      do k = 1, kmax
        hl1 = a_br(ic-1,jc  )*dzu(ic-1,jc  ,k)*ibm(ic-1,jc  )  &
             & + a_bl(ic  ,jc  )*dzu(ic  ,jc  ,k)*ibm(ic  ,jc  )  &
             & + a_tr(ic-1,jc-1)*dzu(ic-1,jc-1,k)*ibm(ic-1,jc-1)  &
             & + a_tl(ic  ,jc-1)*dzu(ic  ,jc-1,k)*ibm(ic  ,jc-1)

#ifdef OGCM_BBL
        if (k+1 == ktbtm(ic,jc) .and. atexlbbl(i,j,1) == 1.0d0) then
          hl2 = a_br(ic-1,jc  )*dzu(ic-1,jc  ,km)*ibm(ic-1,jc  )  &
               & + a_bl(ic  ,jc  )*dzu(ic  ,jc  ,km)*ibm(ic  ,jc  )  &
               & + a_tr(ic-1,jc-1)*dzu(ic-1,jc-1,km)*ibm(ic-1,jc-1)  &
               & + a_tl(ic  ,jc-1)*dzu(ic  ,jc-1,km)*ibm(ic  ,jc-1)
        else
#endif /* OGCM_BBL */
          hl2 = a_br(ic-1,jc  )*dzu(ic-1,jc  ,k+1)*ibm(ic-1,jc  )  &
               & + a_bl(ic  ,jc  )*dzu(ic  ,jc  ,k+1)*ibm(ic  ,jc  )  &
               & + a_tr(ic-1,jc-1)*dzu(ic-1,jc-1,k+1)*ibm(ic-1,jc-1)  &
               & + a_tl(ic  ,jc-1)*dzu(ic  ,jc-1,k+1)*ibm(ic  ,jc-1)
#ifdef OGCM_BBL
        end if
#endif /* OGCM_BBL */

        if (k <= ksgm-1) then
          vol_ts = 0.5d0 * (hl1 + hl2) * (thcksgm + ssh(ic, jc)) * thcksgmr
        else if (k == ksgm) then
          vol_ts = 0.5d0 * (hl1 * (thcksgm + ssh(ic, jc)) * thcksgmr + hl2)
        else
          vol_ts = 0.5d0 * (hl1 + hl2)
        end if

        if (t(ic,jc,k) /= undef8_in) then
          vol_tstotal(jg,k) = vol_tstotal(jg,k) + vol_ts
          t_v(jg,k) = t_v(jg,k) + t(ic,jc,k) * vol_ts
        end if
      end do

    else
      ic = i00
      jc = j00-1

      do k = 1, kmax
        hl1 = a_br(ic-1,jc  )*dzu(ic-1,jc  ,k)*ibm(ic-1,jc  )  &
             & + a_bl(ic  ,jc  )*dzu(ic  ,jc  ,k)*ibm(ic  ,jc  )  &
             & + a_tr(ic-1,jc-1)*dzu(ic-1,jc-1,k)*ibm(ic-1,jc-1)  &
             & + a_tl(ic  ,jc-1)*dzu(ic  ,jc-1,k)*ibm(ic  ,jc-1)

#ifdef OGCM_BBL
        if (k+1 == ktbtm(ic,jc) .and. atexlbbl(i,j,1) == 1.0d0) then
          hl2 = a_br(ic-1,jc  )*dzu(ic-1,jc  ,km)*ibm(ic-1,jc  )  &
               & + a_bl(ic  ,jc  )*dzu(ic  ,jc  ,km)*ibm(ic  ,jc  )  &
               & + a_tr(ic-1,jc-1)*dzu(ic-1,jc-1,km)*ibm(ic-1,jc-1)  &
               & + a_tl(ic  ,jc-1)*dzu(ic  ,jc-1,km)*ibm(ic  ,jc-1)
        else
#endif /* OGCM_BBL */
          hl2 = a_br(ic-1,jc  )*dzu(ic-1,jc  ,k+1)*ibm(ic-1,jc  )  &
               & + a_bl(ic  ,jc  )*dzu(ic  ,jc  ,k+1)*ibm(ic  ,jc  )  &
               & + a_tr(ic-1,jc-1)*dzu(ic-1,jc-1,k+1)*ibm(ic-1,jc-1)  &
               & + a_tl(ic  ,jc-1)*dzu(ic  ,jc-1,k+1)*ibm(ic  ,jc-1)
#ifdef OGCM_BBL
        end if
#endif /* OGCM_BBL */

        if (k <= ksgm-1) then
          vol_ts = 0.5d0 * (hl1 + hl2) * (thcksgm + ssh(ic, jc)) * thcksgmr
        else if (k == ksgm) then
          vol_ts = 0.5d0 * (hl1 * (thcksgm + ssh(ic, jc)) * thcksgmr + hl2)
        else
          vol_ts = 0.5d0 * (hl1 + hl2)
        end if

        if (t(ic,jc,k) /= undef8_in) then
          vol_tstotal(jg,k) = vol_tstotal(jg,k) + vol_ts
          t_v(jg,k) = t_v(jg,k) + t(ic,jc,k) * vol_ts
        end if

      end do

    end if
    !
    t_done(ic, jc)=1.d0
    !
!!!!if(ic == imut-1 .and. jc == jend) then
    if(ic == imut-2) then
      exit LOOP_AROUND_THE_WORLD
    end if
  end do LOOP_AROUND_THE_WORLD
  !
end subroutine around_the_world_wp
!====================================================
!====================================================
end program zonal_mean_w
