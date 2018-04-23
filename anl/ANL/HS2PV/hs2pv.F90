!-*-F90-*-
!hs2pv.F90
!====================================================
!
! Potential Vorticity
!
!====================================================
program potential_vorticity
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   imx, jmx,            &
  &   ksgm, dz,            &
  &   pi, radian, radian_r, &
  &   slat0, slon0,        &
  &   nplat, nplon,        &
  &   splat, splon,        &
  &   ro, omega
#ifndef OGCM_VARIABLE
  use oc_mod_param, only : &
  &   dxtdgc, dytdgc
#endif /* ! OGCM_VARIABLE */
  !
  use oc_mod_trnsfrm, only  : &
  &   set_abc, mp2lp
  !
  use oc_structure, only  : &
  &   read_topo,            & !--------
  &   dep,                  &
  &   ho4, exnn ,           &
  &   aexl, atexl,          &
  &   ktbtm,                &
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
  real(8)    :: mu_u, psiu, lambdau, phiu
  !
  real(8)    :: u(imut, jmut, km)
  real(8)    :: v(imut, jmut, km)
  real(8)    :: t(imut, jmut, km)
  real(8)    :: s(imut, jmut, km)
  real(8)    :: rho(imut, jmut, km) ! [g/cm^3] => rho-1000 [kg/m^3]
  real(8)    :: ssh(imut, jmut)
  !
  real(8)    :: refpress
  real(8)    :: pressure(km)
  !
  real(8),parameter :: dr_eps = 1.0d-6
  !
  real(8) :: pv(imut, jmut, km) ! bottom of each cell
  !
  real(4), parameter :: undef = -9.99e33
  real(4)    :: d3_r4(imut,jmut,km)
  real(4)    :: d2_r4(imut,jmut)
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flin_u
  character(len=256)    :: flin_v
  character(len=256)    :: flin_t
  character(len=256)    :: flin_s
  character(len=256)    :: flinssh
  character(len=256)    :: flout
  character(len=256)    :: fltopo
  character(len=256)    :: flsclf
  character(len=256)    :: flibas
#ifdef OGCM_VARIABLE
  character(len=256)    :: file_vgrid ! 可変格子定義ファイル
#endif /* OGCM_VARIABLE */
  !
  namelist /nml_pv/ refpress, &
    &               flin_u, flin_v, flin_t, flin_s, flinssh, &
    &               flout, fltopo, flsclf
#ifdef OGCM_VARIABLE
  namelist /inflg/ file_vgrid
#endif /* OGCM_VARIABLE */
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mttmp   = 79
  integer(4), parameter :: mtin_u  = 80
  integer(4), parameter :: mtin_v  = 81
  integer(4), parameter :: mtin_t  = 82
  integer(4), parameter :: mtin_s  = 83
  integer(4), parameter :: mtinssh = 84
  integer(4), parameter :: mtout   = 85
  !
  integer(4) :: lrec_out
  !
  integer(4) :: irecw
  integer(4) :: i, j, k, m, jj
  integer(4) :: jg
  !
  !==============================================
  !
  ! 入力パラメタ規定値
  !
  flin_u = 'hs_u.d'
  flin_v = 'hs_v.d'
  flin_t = 'hs_t.d'
  flin_s = 'hs_s.d'
  flinssh= 'hs_ssh.d'
  flout  = 'mrdip.d'
  fltopo = 'topo.d'
  flsclf = 'scale_factor.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml=nml_pv)
  print *,'ref. pressure :', refpress, ' [bar]'
  print *,'flin_u  :', trim(flin_u)
  print *,'flin_v  :', trim(flin_v)
  print *,'flin_t  :', trim(flin_t)
  print *,'flin_s  :', trim(flin_s)
  print *,'flinssh :', trim(flinssh)
  print *,'flout    :', trim(flout)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
#ifdef OGCM_VARIABLE
  read(unit=5, nml=inflg) ! file_vgrid
#endif /* OGCM_VARIABLE */
  !
  pressure(1:km) = refpress ! [bar]
  !
  !----------------------------------------------
  ! 海洋モデル格子情報等の準備
  !
  ! 座標変換のパラメータを決める
  call set_abc ( nplat, nplon, splat, splon )
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
  open (mtin_u, file=flin_u, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_v, file=flin_v, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_t, file=flin_t, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_s, file=flin_s, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtinssh, file=flinssh, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)
  !
  open (mtout, file=flout, form='unformatted',&
    &  access='direct', recl=4*imut*jmut*km )

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
  ! T
  !
  read (mtin_t, rec=1) d3_r4
  t(:,:,:) = atexl(:,:,:)*real(d3_r4(:,:,:),8)
  !
  t(1:2,         1:jmut, 1:km)=t(imut-3:imut-2, 1:jmut, 1:km)
  t(imut-1:imut, 1:jmut, 1:km)=t(3:4,           1:jmut, 1:km)
  !
  ! S
  !
  read (mtin_s, rec=1) d3_r4
  s(:,:,:) = atexl(:,:,:)*real(d3_r4(:,:,:),8)
  !
  s(1:2,         1:jmut, 1:km)=s(imut-3:imut-2, 1:jmut, 1:km)
  s(imut-1:imut, 1:jmut, 1:km)=s(3:4,           1:jmut, 1:km)
  !-------------------------
  !
  !  SSH
  !
  read (mtinssh, rec=1) d2_r4
  ssh(:,:) = atexl(:,:,1)*real(d2_r4(:,:),8)
  !
  ssh(1:2,         1:jmut) = ssh(imut-3:imut-2, 1:jmut)
  ssh(imut-1:imut, 1:jmut) = ssh(3:4,           1:jmut)
  !
  close ( mtin_u )
  close ( mtin_v )
  close ( mtin_t )
  close ( mtin_s )
  close ( mtinssh)
  !
  !-------------------------
  ! sigma
  !
  call dens(imut, jmut, km, t, s, pressure, rho)
  rho(1:imut, 1:jmut, 1:km) = atexl(1:imut, 1:jmut, 1:km)* rho(1:imut, 1:jmut, 1:km) * 1.d3
  !
  !-------------------------

  call calc_pv

  write ( mtout, rec=1 )  d3_r4
  close ( mtout )
  !
contains
  !--------------------------------------------------------------------------
  subroutine calc_pv

    implicit none

    real(8) :: hl1, hl2, hl3
    real(8) :: cor

    pv(1:imut, 1:jmut, 1:km) = real(undef,8)
    d3_r4(1:imut, 1:jmut, 1:km+1) = 0.0e0

    do j = 2, jmut
      do i = 2, imut

        mu_u = alont(i) * radian_r
        psiu = alatt(j) * radian_r

        call mp2lp(lambdau, phiu, mu_u, psiu)
        cor = 2.d0 * omega * sin(phiu)

        do k = 1, ksgm - 1
          hl1 = 0.5d-2 * (dz(k) + dz(k+1)) * (thcksgm + ssh(i,j)) * thcksgmr
          pv(i,j,k) = atexl(i,j,k+1) * cor &
               & * (rho(i,j,k+1) - rho(i,j,k)) / hl1 / ro * 1.0d-3
        end do

        do k = ksgm, ksgm
          hl2 = max(dzu(i-1,j,k+1),dzu(i,j,k+1),dzu(i-1,j-1,k+1),dzu(i,j-1,k+1))
          hl1 = 0.5d-2 * (dz(k) * (thcksgm + ssh(i,j)) * thcksgmr + hl2)
          pv(i,j,k) = atexl(i,j,k+1) * cor &
               & * (rho(i,j,k+1) - rho(i,j,k)) / hl1 / ro * 1.0d-3
        end do

        do k = ksgm + 1, km - 1
          hl2 = max(dzu(i-1,j,k+1),dzu(i,j,k+1),dzu(i-1,j-1,k+1),dzu(i,j-1,k+1))
          hl1 = 0.5d-2 * (dz(k) + hl2)
          pv(i,j,k) = atexl(i,j,k+1) * cor &
               & * (rho(i,j,k+1) - rho(i,j,k)) / hl1 / ro * 1.0d-3
        end do

#ifdef OGCM_BBL
        k = ktbtm(i,j)
        if (k > ksgm .and. atexlbbl(i,j,1) == 1.d0) then
          hl3 = max(dzu(i-1,j,k-1),dzu(i,j,k-1),dzu(i-1,j-1,k-1),dzu(i,j-1,k-1))
          hl2 = dz(km)
          hl1 = 0.5d-2 * (hl3 + hl2)
          pv(i,j,k-1) = atexlbbl(i,j,1) * cor &
               & * (rho(i,j,km) - rho(i,j,k-1)) / hl1 / ro * 1.0d-3
        end if
#endif /* OGCM_BBL */

      end do
    end do

    pv(1:2,         1:jmut, 1:km) = pv(imut-3:imut-2, 1:jmut, 1:km)
    pv(imut-1:imut, 1:jmut, 1:km) = pv(3:4,           1:jmut, 1:km)

    do k = 1, km
      d3_r4(1:imut, 1:jmut, k) = pv(1:imut, 1:jmut, k)
      where(pv(1:imut, 1:jmut, k) == real(undef,8))  
        d3_r4(1:imut, 1:jmut, k) = UNDEF
      end where
    end do

  end subroutine calc_pv
!====================================================
end program potential_vorticity
