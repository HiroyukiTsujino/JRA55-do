!-*-F90-*-
!mocip2trn.F90
!====================================================
!
! Make Meridional Overturning Streamfunction
!     Potential Density - Latitude
!
!====================================================
program meridional_transport_ip
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   imx, jmx,            &
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
  &   dep,                  &
  &   ho4, exnn ,           &
  &   aexl, atexl,          &
#ifdef OGCM_BBL
  &   ho4bbl, exnnbbl,      &
  &   aexlbbl, atexlbbl,    &
#endif /* OGCM_BBL */
  &   dzu,                  &
  &   thcksgm, thcksgmr,    &
  &   read_scale,           & !--------
  &   a_tl  , a_tr  ,       &
  &   a_bl  , a_br  ,       &
  &   dx_tl , dx_tr ,       &
  &   dx_bl , dx_br ,       &
  &   dy_tl , dy_tr ,       &
  &   dy_bl , dy_br ,       &
  &   set_hgrids,            & !--------
  &   dxtdeg, dytdeg,       &
  &   dxudeg, dyudeg,       &
  &   slat, slon,           &
  &   alatt, alatu,         &
  &   alont, alonu
  !
  !
  use oc_mod_density, only : &
  &   dens
  !
  !----------------------------------------------
  !
  implicit none
  !
  !
  real(8), parameter    :: dlatg =   1.0d0
  real(8), parameter    :: slatg  = real(slat0, 8)
  integer(4), parameter :: jmgeo = 75 + 1 -slat0
  !
  ! 海面フラックス積算用緯度区切り
  !
  real(8)    :: rho_sep(4) ! (1) surface, (2) upper thermocline (3) lower thermocline (4) bottom
  real(8)    :: rho_int(3) ! (1) seasonal thermocline (2) main thermocline (3) deep layer
  integer(4) :: rl_sep(4)
  integer(4) :: RLMAX
  real(8),allocatable :: rholev(:)
  real(8),allocatable :: rholev_half(:)
  real(8),parameter :: dr_eps = 1.0d-6
  !
  ! k=1    :                   rholatbnd >= rholev(1)
  ! k=RLMAX: rholev(RLMAX-1) > rholatbnd 
  !
  real(4),allocatable    :: sv(:,:)
  !
  ! k=1       : bottom
  ! k=RLMAX+1 : sea surface
  !
  real(4),allocatable    :: strmf_g(:,:)  ! Global
  real(4),allocatable    :: strmf_a(:,:)  ! Atlantic
  real(4),allocatable    :: strmf_p(:,:)  ! Pacific
  real(4),allocatable    :: strmf_i(:,:)  ! Indian
  !
  !real(4), parameter :: undefgd = -9.99e33
  real(4)    :: d3_r4(imut,jmut,km)
  real(4)    :: d2_r4(imut,jmut)
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flin
  character(len=256)    :: flout
  character(len=256)    :: fltopo
  character(len=256)    :: flsclf
#ifdef OGCM_VARIABLE
  character(len=256)    :: file_vgrid ! 可変格子定義ファイル
#endif /* OGCM_VARIABLE */
  !
  namelist /nml_mocip2trn/ rho_sep, rho_int,           &
    &               flin, flout, fltopo, flsclf
#ifdef OGCM_VARIABLE
  namelist /inflg/ file_vgrid
#endif /* OGCM_VARIABLE */
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mttmp   = 79
  integer(4), parameter :: mtin    = 80
  integer(4), parameter :: mtout   = 85
  !
  integer(4) :: lrec_in, lrec_out
  !
  integer(4) :: ireci, irecw
  integer(4) :: i, j, k, m, jj
  integer(4) :: jg
  !
  !==============================================
  !
  ! 入力パラメタ規定値
  !
  flin   = 'hs_u.d'
  flout  = 'mrdip.d'
  fltopo = 'topo.d'
  flsclf = 'scale_factor.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml=nml_mocip2trn)
  print *,'flin     :', trim(flin)
  print *,'flout    :', trim(flout)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
#ifdef OGCM_VARIABLE
  read(unit=5, nml=inflg) ! file_vgrid
#endif /* OGCM_VARIABLE */

  !----------------------------------------------

  rl_sep(1) = 1
  do m = 2, 4
    rl_sep(m) = rl_sep(m-1) + int(-(rho_sep(m)-rho_sep(m-1)) / rho_int(m-1) + dr_eps)
  end do

  write(6,*) ' density separated at '
  write(6,'(4f10.4)') (rho_sep(m),m=1,4)
  write(6,'(4i10)') (rl_sep(m),m=1,4)
  write(6,*) ' with interval '
  write(6,'(3f10.4)') (rho_int(m),m=1,3)

  rlmax = rl_sep(4)
  write(6,*) ' RLMAX = ', RLMAX

  lrec_in  = jmgeo * (RLMAX+1) * 4
  lrec_out = jmgeo * RLMAX * 4
  allocate(rholev(0:RLMAX))
  allocate(rholev_half(1:RLMAX))
  allocate(sv(1:jmgeo,1:RLMAX))

  allocate(strmf_g(1:jmgeo,1:RLMAX+1))  ! Global
  allocate(strmf_a(1:jmgeo,1:RLMAX+1))  ! Atlantic
  allocate(strmf_p(1:jmgeo,1:RLMAX+1))  ! Pacific
  allocate(strmf_i(1:jmgeo,1:RLMAX+1))  ! Indian

  rholev(0) = rho_sep(1) + rho_int(1)
  rholev(1) = rho_sep(1)

  do m = 2, 4
    do k = rl_sep(m-1) + 1,  rl_sep(m)
      rholev(k) = rholev(k-1) - rho_int(m-1)
    end do
  end do

  do k = 1, rlmax
    rholev_half(k) = 0.5d0 * (rholev(k-1) + rholev(k))
  end do

  write(*,*) '------'
  write(*,'(a, i4, a, f8.3)') 'ZDEF ', RLMAX+1, ' LEVELS ', rholev(0)
  write(*,'(7f8.3)') (rholev(k),k=1,rlmax)
  write(*,*) '------'

  write(*,*) '------'
  write(*,'(a, i4, a)') 'ZDEF ', RLMAX, ' LEVELS '
  write(*,'(7f8.3)') (rholev_half(k),k=1,rlmax)
  write(*,*) '------'

  !----------------------------------------------
  ! 海洋モデル格子情報等の準備
  !
  ! モデル水平格子情報定義
  !
  call read_topo(fltopo)
  !
  call read_scale(flsclf)
  !
#ifdef OGCM_VARIABLE
  call set_hgrids(file_vgrid)
#else /* OGCM_VARIABLE */
  call set_hgrids
#endif /* OGCM_VARIABLE */
  !
  !==============================================
  !
  ! 入出力ファイルオープン
  !
  open (mtin, file=flin, form='unformatted', &
    &  access='direct', recl=lrec_in)
  ireci = 1
  !
  open (mtout, file=flout, form='unformatted',&
    &  access='direct', recl=lrec_out )
  irecw = 1
  !
  !-------------------------

  read ( mtin, rec=ireci )  strmf_g(1:jmgeo, 1:RLMAX+1)
  ireci=ireci+1

  read ( mtin, rec=ireci )  strmf_a(1:jmgeo, 1:RLMAX+1)
  ireci=ireci+1

  read ( mtin, rec=ireci )  strmf_p(1:jmgeo, 1:RLMAX+1)
  ireci=ireci+1

  read ( mtin, rec=ireci )  strmf_i(1:jmgeo, 1:RLMAX+1)
  ireci=ireci+1

  strmf_i(1:jmgeo, 1:RLMAX+1) = strmf_i(1:jmgeo, 1:RLMAX+1) - strmf_p(1:jmgeo, 1:RLMAX+1)

  close ( mtin )

  !--------------------
  !  Global
  !--------------------

  print *, '       Global '

  sv(1:jmgeo,1:rlmax) = 0.0

  do j = 1, jmgeo
    do k = 1, rlmax
      sv(j,k) = -(strmf_g(j,k+1) - strmf_g(j,k))
    end do
  end do

  write (mtout, rec=irecw)  sv(1:jmgeo, 1:rlmax)
  irecw = irecw + 1

  !--------------------
  !  Atlantic
  !--------------------
  print *, '       Atlantic '

  sv(1:jmgeo,1:rlmax) = 0.0

  do j = 1, jmgeo
    do k = 1, rlmax
      sv(j,k) = -(strmf_a(j,k+1) - strmf_a(j,k))
    end do
  end do

  write (mtout, rec=irecw)  sv(1:jmgeo, 1:rlmax)
  irecw = irecw + 1

  !--------------------
  !  Pacific
  !--------------------
  print *, '       Pacific '

  sv(1:jmgeo,1:rlmax) = 0.0

  do j = 1, jmgeo
    do k = 1, rlmax
      sv(j,k) = -(strmf_p(j,k+1) - strmf_p(j,k))
    end do
  end do

  write (mtout, rec=irecw)  sv(1:jmgeo, 1:rlmax)
  irecw = irecw + 1

  !--------------------
  !  Indian
  !--------------------
  print *, '       Indian '
  !
  sv(1:jmgeo,1:rlmax) = 0.0

  do j = 1, jmgeo
    do k = 1, rlmax
      sv(j,k) = -(strmf_i(j,k+1) - strmf_i(j,k))
    end do
  end do

  write (mtout, rec=irecw)  sv(1:jmgeo, 1:rlmax)
  irecw = irecw + 1

  close ( mtout )

end program meridional_transport_ip
