!-*-F90-*-
!hs2moc.F90
!====================================================
!
! Make Meridional Overturning Streamfunction
!
!             original version written by M.Hirabara
!  modified for variable grid  @040720 by S.Yukimoto
!  make using the model's modules @060927 by S.Yukimoto
!  modified for readability  @20070416 by M.Hirabara
!====================================================
program meridional_streamfunction
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
  use oc_structure, only  : &
  &   read_topo,            & !--------
  &   dep,                  & ! UVTSボックス境界（上面）の深さ
  &   ho4, exnn ,           & ! 水深、層数
  &   aexl, atexl,          & ! 海陸インデックス
  &   dzu,                  & ! UVボックスの厚さ
  &   thcksgm, thcksgmr,    & ! sigma 層の厚さ、逆数
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
  real(8)    :: u(imut, jmut, km)
  real(8)    :: v(imut, jmut, km)
  real(8)    :: ssh(imut, jmut)
  !
  real(8)    :: sv(jmgeo, km)  ! 緯度ごと層ごとの北向き流量     [cgs]
  !
  real(8)    :: phi_std_degree ! この緯度に沿って地球を一周
  !
  ! 子午面循環流線関数 [Sv]
  !     鉛直流量(WLWL)と同じところで定義される
  !     最下層T-boxの下面でk=1, 海面でk=km+1
  !     海面淡水フラックスがあるとき、海面で0にならない。
  !
  real(4)    :: strmf_g(jmgeo, km+1)  ! Global
  real(4)    :: strmf_a(jmgeo, km+1)  ! Atlantic
  real(4)    :: strmf_p(jmgeo, km+1)  ! Pacific
  real(4)    :: strmf_i(jmgeo, km+1)  ! Pacific
  !
  real(4), parameter :: cgs2Sv  = 1.0e-12      ! cm^3/s => Sv   ( 10^6 m3/s )
  real(4), parameter :: cgs2kgs = ro * 1.0e-3  ! cm^3/s => kg/s
  real(4), save      :: conv_unit = cgs2Sv     ! default
  !
  character(len=8), parameter :: str_Sv   = ' [Sv]'
  character(len=8), parameter :: str_kgs  = ' [kg/s]'
  character(len=8), save      :: str_unit = str_Sv
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
  character(len=256)    :: flibas ! basinインデックスファイル
  character(len=256)    :: flstdout ! 診断量の出力ファイル
  logical, save      :: l_cmip5 = .false.  ! default
  !
  namelist /nmlmoc/   flin_u, flin_v, flin_ssh, flout,         &
    &                 fltopo, flsclf, flibas, flstdout, l_cmip5
#ifdef OGCM_VARIABLE
  character(len=256)    :: file_vgrid ! 可変格子定義ファイル
  !
  namelist /inflg/ file_vgrid
#endif /* OGCM_VARIABLE */
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mttmp    = 79
  integer(4), parameter :: mtin_u   = 80
  integer(4), parameter :: mtin_v   = 81
  integer(4), parameter :: mtin_ssh = 84
  integer(4), parameter :: mtout    = 86
  integer(4), parameter :: mtstdout = 87
  !
  integer(4), save :: lrec_out
  !
  integer(4) :: irecw
  integer(4) :: i, j, k, m, jj
  integer(4) :: nkai
  integer(4) :: jg
  character(80) :: fmt_ibas
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
  !
  lrec_out = jmgeo*(km+1)*4
  !
  !
  !---------------------------------------------
  !
  ! 入力パラメタ規定値
  !
  flin_u  = 'hs_u.d'
  flin_v  = 'hs_v.d'
  flin_ssh= 'hs_ssh.d'
  flout  = 'moc.d'
  fltopo = 'topo.d'
  flsclf = 'scale_factor.d'
  flibas = 'basin_map.txt'
  flstdout = 'ocdgout.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nmlmoc)
  print *,'flin_u   :', trim(flin_u)
  print *,'flin_v   :', trim(flin_v)
  print *,'flin_ssh :', trim(flin_ssh)
  print *,'flout    :', trim(flout)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  print *,'flibas   :', trim(flibas)
  print *,'flstdout :', trim(flstdout)
  print *,'l_cmip5  :', l_cmip5
  !
  if(l_cmip5) then
    conv_unit = cgs2kgs
    str_unit  = str_kgs
  end if
  !
#ifdef OGCM_VARIABLE
  read(unit=5, inflg) ! file_vgrid
#endif /* OGCM_VARIABLE */
  !
  !----------------------------------------------
  ! 海洋モデル格子情報等の準備
  !
  call read_topo(fltopo)
  !
  write(*,"(a)")                'XDEF      1  LINEAR    0.0000   1.0'
  write(*,"(a,i5,a,f8.4,f8.4)") 'YDEF   ', jmgeo, '  LINEAR  ', slatg, dlatg
  write(*,"(a,i5,a)")           'ZDEF   ', km+1,  '  LEVELS'
  write(*,"(f8.2)") (dep(km)+dz(km)) * 1.d-2     !  cm => m
  do k = km, 1, -1
    write(*,"(f8.2)") dep(k) * 1.d-2     !  cm => m
  end do
  !
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
  open (mtin_u, file=flin_u, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_v, file=flin_v, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_ssh, file=flin_ssh, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)
  !
  open (mtout, file=flout, form='unformatted',&
    &  access='direct', recl=lrec_out )
  irecw=1
  !
  open (mtstdout, file=flstdout, form='formatted')
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
  sv(1:jmgeo, 1:km)=0.d0
  !
  do jg=1, jmgeo
    !
    phi_std_degree = slatg + dlatg*(jg-1)
    call around_the_world(jg, phi_std_degree)
    !
  end do
  !
  strmf_g(1:jmgeo, 1)=0.0
  do k=2, km+1
    do j=2, jmgeo
      strmf_g(j,k)=strmf_g(j,k-1)-real(sv(j,km+2-k), 4) * conv_unit
    end do
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
  sv(1:jmgeo, 1:km)=0.d0
  !
  do jg=1, jmgeo
    !
    phi_std_degree = slatg + dlatg*(jg-1)
    call around_the_world(jg, phi_std_degree)
    !
  end do
  !
  strmf_a(1:jmgeo, 1)=0.0
  do k=2, km+1
    do j=2, jmgeo
      strmf_a(j,k)=strmf_a(j,k-1)-real(sv(j,km+2-k), 4) * conv_unit
    end do
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
  sv(1:jmgeo, 1:km)=0.d0
  !
  do jg=1, jmgeo
    !
    phi_std_degree = slatg + dlatg*(jg-1)
    call around_the_world(jg, phi_std_degree)
    !
  end do
  !
  strmf_p(1:jmgeo, 1)=0.0
  do k=2, km+1
    do j=2, jmgeo
      strmf_p(j,k)=strmf_p(j,k-1)-real(sv(j,km+2-k), 4) * conv_unit
    end do
  end do
  !
  !--------------------
  !  Indian
  !--------------------
  print *, '       Indian '
  !
  do j = 1, jmut
    do i= 1, imut
      ibm(i, j) = 0.5d0 + 0.5d0*real(mask_ind(i, j), 8)
    end do
  end do
  !
  sv(1:jmgeo, 1:km)=0.d0
  !
  do jg=1, jmgeo
    !
    phi_std_degree = slatg + dlatg*(jg-1)
    call around_the_world(jg, phi_std_degree)
    !
  end do
  !
  strmf_i(1:jmgeo, 1)=0.0
  do k=2, km+1
    do j=2, jmgeo
      strmf_i(j,k)=strmf_i(j,k-1)-real(sv(j,km+2-k), 4) * conv_unit
    end do
  end do
  !
  !--------------------------------------------
  write ( mtout, rec=irecw )  strmf_g(1:jmgeo, 1:km+1)
  irecw=irecw+1
  !
  write ( mtout, rec=irecw )  strmf_a(1:jmgeo, 1:km+1)
  irecw=irecw+1
  !
  write ( mtout, rec=irecw )  strmf_p(1:jmgeo, 1:km+1)
  irecw=irecw+1
  !
  write ( mtout, rec=irecw )  strmf_i(1:jmgeo, 1:km+1)  &
    &                        +strmf_p(1:jmgeo, 1:km+1)
  irecw=irecw+1
  !
  !---------------------------------------------------
  ! Meridional Overturning
  !    (maximum or minimum at the latitude in the basin)
  !---------------------------------------------------
  write(mtstdout, '(a)') ' Meridional Overturning StreamFunction'
  write(mtstdout, '(a)') '   maximum magnitude at the latitude'
  write(mtstdout, '(a)') '     in the basin'
  !
  if(l_cmip5) then
    write(mtstdout, '(a, e, a)') ' Global 70S - 65S:', &
      &  minval(strmf_g(j_SOmin:j_SOmax, 1:km)), trim(str_unit)
    !
    write(mtstdout, '(a, e, a)') ' Atlantic 45N max:', &
      &  maxval(strmf_a(j_45N, 1:km)), trim(str_unit)
    !
    write(mtstdout, '(a, e, a)') ' Atlantic 30N max:', &
      &  maxval(strmf_a(j_30N, 1:km)), trim(str_unit)
    !
    write(mtstdout, '(a, e, a)') ' Atlantic 20S min:', &
      &  minval(strmf_a(j_20S, 1:k_2000m)), trim(str_unit)
    !
    write(mtstdout, '(a, e, a)') ' Pacific  20S min:', &
      &  minval(strmf_p(j_20S, 1:k_2000m)), trim(str_unit)
    !
    write(mtstdout, '(a, e, a)') ' Indian   20S min:', &
      &  minval(strmf_i(j_20S, 1:k_2000m)), trim(str_unit)
  else
    write(mtstdout, '(a, f9.2, a)') ' Global 70S - 65S:', &
      &  minval(strmf_g(j_SOmin:j_SOmax, 1:km)), trim(str_unit)
    !
    write(mtstdout, '(a, f9.2, a)') ' Atlantic 45N max:', &
      &  maxval(strmf_a(j_45N, 1:km)), trim(str_unit)
    !
    write(mtstdout, '(a, f9.2, a)') ' Atlantic 30N max:', &
      &  maxval(strmf_a(j_30N, 1:km)), trim(str_unit)
    !
    write(mtstdout, '(a, f9.2, a)') ' Atlantic 20S min:', &
      &  minval(strmf_a(j_20S, 1:k_2000m)), trim(str_unit)
    !
    write(mtstdout, '(a, f9.2, a)') ' Pacific  20S min:', &
      &  minval(strmf_p(j_20S, 1:k_2000m)), trim(str_unit)
    !
    write(mtstdout, '(a, f9.2, a)') ' Indian   20S min:', &
      &  minval(strmf_i(j_20S, 1:k_2000m)), trim(str_unit)
  end if
  !
  !
  close ( mtin_u )
  close ( mtin_v )
  close ( mtin_ssh)
  close ( mtout )
  close ( mtstdout )
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
  !
#ifdef OGCM_BBL
  integer(4), parameter :: kmax = km-1
#else /* OGCM_BBL */
  integer(4), parameter :: kmax = km
#endif /* OGCM_BBL */
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
  !
  hl1 = max(dy_bl(ic, jc)+dy_tl(ic, jc), 1.d0)
  hl1 = 1.d0/hl1
  do k = 1, ksgm
    vstar = dx_tr(i00,j00)*dzu(i00,j00,k)*v(i00,j00,k)*ibm(i00,j00) &
      &    +dx_tl(ic ,jc )*dzu(ic ,jc ,k)*v(ic ,jc ,k)*ibm(ic ,jc )
    vstar = vstar*(thcksgm+hl1*(dy_bl(ic, jc)*ssh(i00+1, j00)+dy_tl(ic, jc)*ssh(i00+1, j00+1)))*thcksgmr
    sv(jg,k)=sv(jg,k) +vstar
  end do
  do k = ksgm+1, kmax
    vstar = dx_tr(i00,j00)*dzu(i00,j00,k)*v(i00,j00,k)*ibm(i00,j00) &
      &    +dx_tl(ic ,jc )*dzu(ic ,jc ,k)*v(ic ,jc ,k)*ibm(ic ,jc )
    sv(jg,k)=sv(jg,k) +vstar
  end do
#ifdef OGCM_BBL
  k=max(exnn(i00,j00), exnn(ic,jc))
  vstar = dx_tr(i00,j00)*dzu(i00,j00,km)*v(i00,j00,km)*ibm(i00,j00) &
    &    +dx_tl(ic ,jc )*dzu(ic ,jc ,km)*v(ic ,jc ,km)*ibm(ic ,jc )
  sv(jg,k)=sv(jg,k)+vstar
#endif /* OGCM_BBL */
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
      hl1 = max(dy_bl(ic, jc)+dy_tl(ic, jc), 1.d0)
      hl1 = 1.d0/hl1
      do k = 1, ksgm
        vstar = dx_tr(i00,j00)*dzu(i00,j00,k)*v(i00,j00,k)*ibm(i00,j00) &
          &    +dx_tl(ic ,jc )*dzu(ic ,jc ,k)*v(ic ,jc ,k)*ibm(ic ,jc )
        vstar = vstar*(thcksgm+hl1*(dy_bl(ic, jc)*ssh(i00+1, j00)+dy_tl(ic, jc)*ssh(i00+1, j00+1)))*thcksgmr
        sv(jg,k) = sv(jg,k) +vstar
      end do
      do k = ksgm+1, kmax
        vstar = dx_tr(i00, j00)*dzu(i00, j00, k)*v(i00, j00, k)*ibm(i00,j00) &
          &    +dx_tl(ic , jc )*dzu(ic , jc , k)*v(ic , jc , k)*ibm(ic ,jc )
        sv(jg,k) = sv(jg,k) +vstar
      end do
#ifdef OGCM_BBL
      k=max(exnn(i00, j00), exnn(ic, jc))
      vstar = dx_tr(i00, j00)*dzu(i00, j00, km)*v(i00, j00, km)*ibm(i00, j00) &
        &    +dx_tl(ic , jc )*dzu(ic , jc , km)*v(ic , jc , km)*ibm(ic , jc )
      sv(jg,k)=sv(jg,k) +vstar
#endif /* OGCM_BBL */
      !
    else if (min(sqphi20, sqphi01) >= sqphi02) then
      ic = i00
      jc = j00+1
      !
      hl1 = 1.d0 /(dx_bl(ic, jc)+dx_br(ic, jc))
      do k = 1, ksgm
        ustar = dy_tr(i00, j00)*dzu(i00, j00, k)*u(i00, j00, k)*ibm(i00,j00) &
          &    +dy_br(ic , jc )*dzu(ic , jc , k)*u(ic , jc , k)*ibm(ic ,jc )
        ustar = ustar*(thcksgm+hl1*(dx_bl(ic, jc)*ssh(i00, j00+1)+dx_br(ic, jc)*ssh(i00+1, j00+1)))*thcksgmr
        sv(jg,k) = sv(jg,k) -ustar
      end do
      do k = ksgm+1, kmax
        ustar = dy_tr(i00, j00)*dzu(i00, j00, k)*u(i00, j00, k)*ibm(i00,j00) &
          &    +dy_br(ic , jc )*dzu(ic , jc , k)*u(ic , jc , k)*ibm(ic ,jc )
        sv(jg,k) = sv(jg,k) -ustar
      end do
#ifdef OGCM_BBL
      k=max(exnn(i00, j00), exnn(ic, jc))
      ustar = dy_tr(i00, j00)*dzu(i00, j00, km)*u(i00, j00, km)*ibm(i00,j00) &
        &    +dy_br(ic , jc )*dzu(ic , jc , km)*u(ic , jc , km)*ibm(ic ,jc )
      sv(jg,k) = sv(jg,k) -ustar
#endif /* OGCM_BBL */
      !
    else
      ic = i00
      jc = j00-1
      !
      hl1 = 1.d0 /(dx_bl(i00, j00)+dx_br(i00, j00))
      do k = 1, ksgm
        ustar = dy_br(i00, j00)*dzu(i00, j00, k)*u(i00, j00, k)*ibm(i00,j00) &
          &    +dy_tr(ic , jc )*dzu(ic , jc , k)*u(ic , jc , k)*ibm(ic ,jc )
        ustar = ustar*(thcksgm+hl1*(dx_bl(i00, j00)*ssh(i00, j00)+dx_br(i00, j00)*ssh(i00+1, j00)))*thcksgmr
        sv(jg,k) = sv(jg,k) +ustar
      end do
      do k = ksgm+1, kmax
        ustar = dy_br(i00, j00)*dzu(i00, j00, k)*u(i00, j00, k)*ibm(i00,j00) &
          &    +dy_tr(ic , jc )*dzu(ic , jc , k)*u(ic , jc , k)*ibm(ic ,jc )
        sv(jg,k) = sv(jg,k) +ustar
      end do
#ifdef OGCM_BBL
      k=max(exnn(i00, j00), exnn(ic, jc))
      ustar = dy_tr(i00, j00)*dzu(i00, j00, km)*u(i00, j00, km)*ibm(i00,j00) &
        &    +dy_br(ic , jc )*dzu(ic , jc , km)*u(ic , jc , km)*ibm(ic ,jc )
      sv(jg,k) = sv(jg,k) +ustar
#endif /* OGCM_BBL */
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
end program meridional_streamfunction
