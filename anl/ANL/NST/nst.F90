!-*-F90-*-
!nst.F90
!====================================================
!
! Calculate Northward Salt Transport
!
! model salinity is assumed to have a unit of [kg/m^3]
!====================================================
program nward_salt_transport
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
  &   dep,                  & ! UVTSボックス境界（上面）の深さ
  &   ho4, exnn ,           & ! 水深、層数
  &   aexl, atexl,          & ! 海陸インデックス
  &   dzu,                  & ! UVボックスの厚さ
  &   thcksgm, thcksgmr,    & ! sigma 層の厚さ、逆数
  &   coefx, coefy,         &
  &   read_scale,           & !--------
  &   a_tl  , a_tr  ,       & ! 格子面積
  &   a_bl  , a_br  ,       &
  &   dx_tl , dx_tr ,       & ! 東西長
  &   dx_bl , dx_br ,       &
  &   dy_tl , dy_tr ,       & ! 南北長
  &   dy_bl , dy_br ,       &
  &   set_hgrids,            & !--------
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
  !                                        salinity
  real(8), parameter :: unitcnv = 1.d-6 ! [kg / m^3 * cm^3 / s] -> [kg/s]
  !
  ! 地理座標パラメタ
  ! 
  real(8), parameter :: dlatg =   1.0d0  ! 緯度格子間隔
  !
  ! 海面フラックス積算用緯度区切り
  !
  integer(4), parameter :: k_2000m = 15  ! k about 2000m depth
  !                                 note !! k is counted from the bottom !!
  !
  real(8), parameter    :: slatg  = real(slat0, 8)
  integer(4), parameter :: jmgeo = 75 +1 -slat0
  integer(4), parameter :: j_45N = 45 +1 -slat0    ! 45N
  integer(4), parameter :: j_30N = 30 +1 -slat0    ! 30N
  integer(4), parameter :: j_20S = -20 +1 -slat0   ! 20S
  integer(4), parameter :: j_SOmax = -65 +1 -slat0 ! 65S
  integer(4), parameter :: j_SOmin = -70 +1 -slat0 ! 70S
  !
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
  real(8)    :: ibm(imut, jmut)      ! 積算で使用する海盆マスク(1 or 0)
  real(8)    :: mu_u, psiu, lambdau, phiu
  !
  real(8)    :: ut(imut, jmut, km)
  real(8)    :: vt(imut, jmut, km)
  !
  real(8)    :: ht_pw(jmgeo)
  !
  real(4)    :: ht_pw_glb(jmgeo)  ! Global
  real(4)    :: ht_pw_atl(jmgeo)  ! Atlantic
  real(4)    :: ht_pw_pac(jmgeo)  ! Pacific
  real(4)    :: ht_pw_ind(jmgeo)  ! Indian
  !
  !
  real(8)    :: phi_std_degree ! この緯度に沿って地球を一周
  !
  ! 子午面循環流線関数 [Sv]
  !     鉛直流量(WLWL)と同じところで定義される
  !     最下層T-boxの下面でk=1, 海面でk=km+1
  !     海面淡水フラックスがあるとき、海面で0にならない。
  !
  real(4)    :: d3_r4(imut,jmut,km)
  real(4)    :: d2_r4(imut,jmut)
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flin_ut ! 入力ファイル
  character(len=256)    :: flin_vt ! 入力ファイル
  character(len=256)    :: flout  ! 出力ファイル
  character(len=256)    :: fltopo ! 海底地形ファイル
  character(len=256)    :: flsclf ! スケールファクター・ファイル
  character(len=256)    :: flibas ! basinインデックスファイル
#ifdef OGCM_VARIABLE
  character(len=256)    :: file_vgrid ! 可変格子定義ファイル
#endif /* OGCM_VARIABLE */
  !
  namelist /nml_nst/   flin_ut, flin_vt,                        &
    &                  flout, fltopo, flsclf, flibas
#ifdef OGCM_VARIABLE
  namelist /inflg/ file_vgrid
#endif /* OGCM_VARIABLE */
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mttmp    = 79
  integer(4), parameter :: mtin_ut   = 80
  integer(4), parameter :: mtin_vt   = 81
  integer(4), parameter :: mtout    = 86
  !
  integer(4) :: irecw
  integer(4) :: i, j, k, m, jj
  integer(4) :: nkai
  integer(4) :: jg
  character(80) :: fmt_ibas
  real(8)    :: wrk1, wrk2
#ifdef OGCM_BBL
  integer(4) :: kbtm
#endif /* OGCM_BBL */
  !
  !==============================================
  write(*,*) 'slat0', slat0
  write(*,*) 'slatg', slatg
  write(*,*) 'jmgeo', jmgeo
  write(*,*) 'j_45N', j_45N
  write(*,*) 'j_30N', j_30N
  write(*,*) 'j_20S', j_20S
  write(*,*) 'j_SOmax', j_SOmax
  write(*,*) 'j_SOmin', j_SOmin
  !
  !---------------------------------------------
  !
  ! 入力パラメタ規定値
  !
  flin_ut  = 'hs_ut.d'
  flin_vt  = 'hs_vt.d'
  flout  = 'nst.d'
  fltopo = 'topo.d'
  flsclf = 'scale_factor.d'
  flibas = 'basin_map.txt'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml=nml_nst)
  print *,'flin_ut   :', trim(flin_ut)
  print *,'flin_vt   :', trim(flin_vt)
  print *,'flout    :', trim(flout)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  print *,'flibas   :', trim(flibas)
#ifdef OGCM_VARIABLE
  read(unit=5, inflg) ! file_vgrid
#endif /* OGCM_VARIABLE */
  !
  !----------------------------------------------
  ! 海洋モデル格子情報等の準備
  !
  call read_topo(fltopo)
  !
  write(*,"(a)")                'XDEF       1  LINEAR    0.0000   1.0'
  write(*,"(a,i5,a,f8.4,f8.4)") 'YDEF   ', jmgeo, '  LINEAR  ', slatg, dlatg
  !
  ! 座標変換のパラメータを決める
  call set_abc ( nplat, nplon, splat, splon )
  !print *, '@@@ after set_abc '
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
  !  地形の読み込み
  !
  call read_topo(fltopo)
  !----------------------------------------------
  !
  !  スケールファクタの読み込み
  !
  call read_scale(flsclf)
  !---------------------------------------------
  !
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
  do j= 1, jmut
    do i = 1, imut
      m = ibas(i, j)
      if(m > 0) then
        mask_glb(i, j) = 1           ! Ocean: 1, Land: -1
        !if(m == 1 .or. m == 4) mask_atl(i, j) = 1 ! Atlantic or Mediterranean
        if(m == 1)             mask_atl(i, j) = 1 ! Atlantic
        if(m == 2)             mask_pac(i, j) = 1 ! Pacific
        if(m == 3)             mask_ind(i, j) = 1 ! Indian
      end if
    end do
  end do
  !
  !  Pacific side half of the Arctic Sea is excluded from ATL
  !
  do j= 1, jmut
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
  open (mtin_ut, file=flin_ut, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_vt, file=flin_vt, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtout, file=flout, form='unformatted',&
    &  access='direct', recl=4*jmgeo)
  irecw=1
  !
  !-------------------------
  !
  ! UT
  !
  read (mtin_ut, rec=1) d3_r4
  ut(:,:,:) = coefx(:,:,:)*real(d3_r4(:,:,:),8)
  !
  ut(1:2,         1:jmut, 1:km)=ut(imut-3:imut-2, 1:jmut, 1:km)
  ut(imut-1:imut, 1:jmut, 1:km)=ut(3:4,           1:jmut, 1:km)
  !
  ! VT
  !
  read (mtin_vt, rec=1) d3_r4
  vt(:,:,:) = coefy(:,:,:)*real(d3_r4(:,:,:),8)
  !
  vt(1:2,         1:jmut, 1:km)=vt(imut-3:imut-2, 1:jmut, 1:km)
  vt(imut-1:imut, 1:jmut, 1:km)=vt(3:4,           1:jmut, 1:km)
  !-------------------------
  !
  !--------------------
  !  Global
  !--------------------
  print *, '       Global '
  !
  do j = 1, jmut
    do i= 1, imut
      ibm(i, j) = 0.5d0 + 0.5d0*real(mask_glb(i, j), 8)
    end do
  end do
  !
  ht_pw(1:jmgeo)=0.d0
  !
  do jg=1, jmgeo
    phi_std_degree = slatg + dlatg*(jg-1)
    call around_the_world(jg, phi_std_degree)
  end do
  ht_pw_glb(1:jmgeo) = real(ht_pw(1:jmgeo)*unitcnv, 4)
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
  ht_pw(1:jmgeo)=0.d0
  !
  do jg=1, jmgeo
    phi_std_degree = slatg + dlatg*(jg-1)
    call around_the_world(jg, phi_std_degree)
  end do
  ht_pw_atl(1:jmgeo) = real(ht_pw(1:jmgeo)*unitcnv, 4)
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
  ht_pw(1:jmgeo)=0.d0
  !
  do jg=1, jmgeo
    phi_std_degree = slatg + dlatg*(jg-1)
    call around_the_world(jg, phi_std_degree)
  end do
  ht_pw_pac(1:jmgeo) = real(ht_pw(1:jmgeo)*unitcnv, 4)
  !
  !
  !--------------------
  !  Indian
  !--------------------
  print *, '       Indian  '
  !
  do j = 1, jmut
    do i= 1, imut
      ibm(i, j) = 0.5d0 + 0.5d0*real(mask_ind(i, j), 8)
    end do
  end do
  !
  ht_pw(1:jmgeo)=0.d0
  !
  do jg=1, jmgeo
    phi_std_degree = slatg + dlatg*(jg-1)
    call around_the_world(jg, phi_std_degree)
  end do
  ht_pw_ind(1:jmgeo) = real(ht_pw(1:jmgeo)*unitcnv, 4)
  !
  !--------------------------------------------
  write ( mtout, rec=irecw )  ht_pw_glb(1:jmgeo)
  irecw=irecw+1
  !
  write ( mtout, rec=irecw )  ht_pw_atl(1:jmgeo)
  irecw=irecw+1
  !
  write ( mtout, rec=irecw )  ht_pw_pac(1:jmgeo)
  irecw=irecw+1
  !
  write ( mtout, rec=irecw )  ht_pw_ind(1:jmgeo)
  irecw=irecw+1
  !----------------------------
  !
  close ( mtin_ut )
  close ( mtin_vt )
  close ( mtout )
  !
contains
!====================================================
!
!  指定緯度で世界一周
!
!====================================================
subroutine around_the_world(jg, phi_std_degree)
  !
  integer(4), intent(IN) :: jg
  real(8),    intent(IN) :: phi_std_degree
  !
  !-------------------------
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
  real(8)    :: hl1
  !
  real(8)    :: u_done(imut, jmut)
  !
  integer(4) :: j, k
  !
  phi_std = phi_std_degree*radian_r
  !
  u_done(1:imut, 1:jmut) = 0.d0
  !
  u_done(1:2,    1:jmut) = 1.d0
  u_done(imut,   1:jmut) = 1.d0
  u_done(1:imut, 1)      = 1.d0
  u_done(1:imut, jmut-2:jmut)   = 1.d0
  !
  i00=4
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
  i00=i00-1  !  UV格子
  j00=j00-1
  !
  u_done(i00, j00)  = 1.d0
  u_done(i00, j00+1)= 1.d0
  !
  mu0  = alonu(i00)*radian_r
  !mu2  = alonu(i00+1)*radian_r ! mu2 is not used here
  psi0 = alatu(j00)*radian_r
  psi2 = alatu(j00+1)*radian_r
  !
  call mp2lp(lambda00, phi00, mu0, psi0)
  call mp2lp(lambda02, phi02, mu0, psi2)
  !
  sqphi00 = (phi00-phi_std)*(phi00-phi_std)
  sqphi02 = (phi02-phi_std)*(phi02-phi_std)
  !
  if (sqphi00 > sqphi02) then
    j00 = j00+1
    psi0     = psi2
    lambda00 = lambda02
    phi00    = phi02
  end if
  !
  !print *,' start point = ',i00,j00,lambda00*radian,phi00*radian
  !
  ic = i00+1  !  移動先
  jc = j00
  !
  u_done(ic, jc)=1.d0
  !
  !
  do k = 1, km
    ht_pw(jg) = ht_pw(jg) +coefy(ic, jc, k) * max(ibm(i00, j00), ibm(ic, jc)) &
      &                                * vt(ic, jc, k)
  end do
  !
  jend = j00  !  terminater
  !
  !
  LOOP_AROUND_THE_WORLD : do while (ic < imut)
    i00 = ic
    j00 = jc
    !
    mu0  = alonu(i00)*radian_r
    mu2  = alonu(i00+1)*radian_r
    psi0 = alatu(j00)*radian_r
    psi1 = alatu(j00-1)*radian_r
    psi2 = alatu(j00+1)*radian_r
    !
    call mp2lp(lambda00, phi00, mu0, psi0)
    call mp2lp(lambda20, phi20, mu2, psi0)
    call mp2lp(lambda02, phi02, mu0, psi2)
    call mp2lp(lambda01, phi01, mu0, psi1)
    !
    sqphi20 = (phi20-phi_std)*(phi20-phi_std) +u_done(i00+1,j00  )
    sqphi02 = (phi02-phi_std)*(phi02-phi_std) +u_done(i00,  j00+1)
    sqphi01 = (phi01-phi_std)*(phi01-phi_std) +u_done(i00  ,j00-1)
    !
    !
    if(min(sqphi20, sqphi02, sqphi01) > 1.d0) then
      exit LOOP_AROUND_THE_WORLD
    end if
    !
    if(min(sqphi02, sqphi01) >= sqphi20) then ! 等号が必要
      ic = i00+1
      jc = j00
      !
      do k = 1, km
        ht_pw(jg) = ht_pw(jg) +coefy(ic, jc, k) * max(ibm(i00, j00), ibm(ic, jc)) &
        &                                * vt(ic, jc, k)
      end do
      !
    else if (min(sqphi20, sqphi01) >= sqphi02) then
      ic = i00
      jc = j00+1
      !
      do k = 1, km
        ht_pw(jg) = ht_pw(jg) -coefx(ic, jc, k) * max(ibm(i00, j00), ibm(ic, jc)) &
        &                                * ut(ic, jc, k)
      end do
      !
    else
      ic = i00
      jc = j00-1
      !
      do k = 1, km
        ht_pw(jg) = ht_pw(jg) +coefx(i00, j00, k) * max(ibm(i00, j00), ibm(ic, jc)) &
        &                                * ut(i00, j00, k)
      end do
      !
    end if
    !
    u_done(ic, jc)=1.d0
    !
    if(ic == imut-1 .and. jc == jend) then
      exit LOOP_AROUND_THE_WORLD
    end if
  end do LOOP_AROUND_THE_WORLD
  !
end subroutine around_the_world
!====================================================
!
!====================================================
end program nward_salt_transport
