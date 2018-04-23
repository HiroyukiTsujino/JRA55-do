!hs2biv.F90
!====================================================
!
! Buoyancy Integrated Vertically
!
!====================================================
program buoyancy_vertically_integrated
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ksgm, dz,            &
  &   rho0,                &
  &   grav, unit_bar
  !
  use oc_structure, only  : &
  &   read_topo,            & !--------
  &   dep, dp,              &
  &   ho4, exnn, texnn,     &
  &   aexl, atexl,          &
#ifdef OGCM_BBL
  &   atexlbbl,             &
#endif /* OGCM_BBL */
  &   dzu,                  &
  &   thcksgm, thcksgmr,    &
  &   ktbtm,                &
  &   read_scale,           & !--------
  &   a_tl  , a_tr  ,       &
  &   a_bl  , a_br  ,       &
  &   dx_tl , dx_tr ,       &
  &   dx_bl , dx_br ,       &
  &   dy_tl , dy_tr ,       &
  &   dy_bl , dy_br ,       &
  &   set_area,             & !--------
  &   areat,                &
  &   nendidx,              &
  &   set_volt,             & !--------
  &   volt
  !
  use oc_mod_density, only : &
  &   dens
  !
  !----------------------------------------------
  !
  implicit none
  !
  real(8)    :: pd(km)     ! Pressure [bar]
  !
  real(8)    :: t(imut, jmut, km)
  real(8)    :: s(imut, jmut, km)
  real(8)    :: ssh(imut, jmut)
  real(8)    :: rho(imut, jmut, km)
  real(8)    :: rholevmean(km)
  real(8)    :: thcksgmave(km)
  !
  real(4)    :: bivf(imut, jmut)
  real(4)    :: bivuf(imut, jmut)
  real(4)    :: bivlf(imut, jmut)
  !
  real(4), parameter :: UNDEF = -9.99e33
  real(4)    :: d3_r4(imut,jmut,km)
  real(4)    :: d2_r4(imut,jmut)
  !
  integer(4), save :: k_bound = 41
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flin_t
  character(len=256)    :: flin_s
  character(len=256)    :: flin_ssh ! 入力ファイル
  character(len=256)    :: flout  ! 出力ファイル
  character(len=256)    :: fltopo ! 海底地形ファイル
  character(len=256)    :: flsclf ! スケールファクター・ファイル
  !
  namelist /nml_biv/ flin_t, flin_s, flin_ssh, flout, fltopo, flsclf, k_bound
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mttmp    = 79
  integer(4), parameter :: mtin_t   = 80
  integer(4), parameter :: mtin_s   = 81
  integer(4), parameter :: mtin_ssh = 82
  integer(4), parameter :: mtout    = 84
  !
  integer(4) :: irecw
  integer(4) :: i, j, k, m, jj
  integer(4) :: nkai
  character(80) :: fmt_ibas
  real(8)    :: wrk1, wrk2
#ifdef OGCM_BBL
  integer(4) :: kbtm
#endif /* OGCM_BBL */
  !
  !==============================================
  !
  ! 入力パラメタ規定値
  !
  flin_t  = 'hs_t.d'
  flin_s  = 'hs_s.d'
  flin_ssh= 'hs_ssh.d'
  flout   = 'biv.gd'
  fltopo  = 'topo.d'
  flsclf  = 'scale_factor.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml_biv)
  print *,'flin_t   :', trim(flin_t)
  print *,'flin_s   :', trim(flin_s)
  print *,'flin_ssh :', trim(flin_ssh)
  print *,'flout    :', trim(flout)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  print *,'k_bound  :', k_bound
  !
  !----------------------------------------------
  !
  call read_topo(fltopo)
  !
  write(*,*) 'upper: Surface - ', dep(k_bound+1) * 1.d-2, '[m]'
  write(*,*) 'lower: ', dep(k_bound+1) * 1.d-2, '[m]', ' - Bottom'
  !
  pd(1:km) = grav * unit_bar * dp(1:km) * rho0
  !
  call read_scale(flsclf)
  !
  call set_area
  call set_volt
  !
#if defined OGCM_JOT || defined OGCM_TRIPOLAR
  areat(1:imut, jmut-2, 1:km) = 0.5d0 * areat(1:imut, jmut-2, 1:km)
  volt(1:imut, jmut-2, 1:km)  = 0.5d0 * volt(1:imut, jmut-2, 1:km)
#endif /* OGCM_JOT || OGCM_TRIPOLAR */
  !
  !==============================================
  !
  ! 入出力ファイルオープン
  !
  open (mtin_t, file=flin_t, form='unformatted', access='direct', recl=4*imut*jmut*km)
  open (mtin_s, file=flin_s, form='unformatted', access='direct', recl=4*imut*jmut*km)
  open (mtin_ssh, file=flin_ssh, form='unformatted', access='direct', recl=4*imut*jmut)
  !
  open (mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut)
  irecw=1
  !
  !-------------------------
  !
  ! T
  !
  read (mtin_t, rec=1) d3_r4(:,:,:)
  t(:,:,:) = atexl(:,:,:)*real(d3_r4(:,:,:),8)
  !
  ! S
  !
  read (mtin_s, rec=1) d3_r4(:,:,:)
  s(:,:,:) = atexl(:,:,:)*real(d3_r4(:,:,:),8)
#ifdef OGCM_BBL
  do j = 1, jmut
    do i = 1, imut
      k = ktbtm(i,j)
      if(atexl(i,j,1) > 0.d0 .and. atexl(i,j,k) == 0.d0) then
        t(i, j, k) = atexlbbl(i, j, 1) * t(i, j, km)
        s(i, j, k) = atexlbbl(i, j, 1) * s(i, j, km)
      end if
    end do
  end do
#endif /* OGCM_BBL */
  !
  t(1:2,         1:jmut, 1:km)=t(imut-3:imut-2, 1:jmut, 1:km)
  t(imut-1:imut, 1:jmut, 1:km)=t(3:4,           1:jmut, 1:km)
  !
  s(1:2,         1:jmut, 1:km)=s(imut-3:imut-2, 1:jmut, 1:km)
  s(imut-1:imut, 1:jmut, 1:km)=s(3:4,           1:jmut, 1:km)
  !-------------------------
  !
  rho(1:imut, 1:jmut, 1:km) = 0.d0
  call dens(imut, jmut, km, t, s, pd, rho)
  rho(1:imut, 1:jmut, 1:km) = rho(1:imut, 1:jmut, 1:km) + 1.d0
  !
  !-------------------------
  !  SSH
  !
  read (mtin_ssh, rec=1) d2_r4
  ssh(:,:) = atexl(:,:,1)*real(d2_r4(:,:),8)
  ssh(1:2,         1:jmut)=ssh(imut-3:imut-2, 1:jmut)
  ssh(imut-1:imut, 1:jmut)=ssh(3:4,           1:jmut)
  !
  !=========================
  !
  call rhoave
  !
  call b_vint
  !
  write(mtout, rec=irecw) bivf(1:imut, 1:jmut)
  write(mtout, rec=irecw+1) bivuf(1:imut, 1:jmut)
  write(mtout, rec=irecw+2) bivlf(1:imut, 1:jmut)
  !
  close ( mtin_t )
  close ( mtin_s )
  close ( mtin_ssh)
  close ( mtout )
  !
contains
!====================================================
!
!  各鉛直レベルの平均現場密度を計算
!
!====================================================
subroutine rhoave
  !
  implicit none
  !
  real(8) :: hl1
  real(8) :: voltsgm, tvolt(km), trhovol(km)
  !
  tvolt(1:km) = 0.d0
  trhovol(1:km) = 0.d0
  !
  do k = 1, ksgm
    hl1 = dz(k) * thcksgmr ! ratio
    do j = 1, jmut-2
      do i = 3, imut-2
        voltsgm = hl1 * (thcksgm +ssh(i, j)) * areat(i, j, k) * nendidx(j)
        tvolt(k) = tvolt(k) + voltsgm
        trhovol(k) = trhovol(k) + atexl(i,j,k) * rho(i,j,k) * voltsgm
      end do
    end do
  end do
  !
  do k = ksgm+1, km-1
    do j = 1, jmut-2
      do i = 3, imut-2
        tvolt(k)= tvolt(k) + volt(i, j, k) * nendidx(j)
        trhovol(k) = trhovol(k) + atexl(i,j,k) * rho(i,j,k) * volt(i, j, k) * nendidx(j)
      end do
    end do
  end do
#ifdef OGCM_BBL
  do j = 1, jmut-2
    do i = 3, imut-2
      if(atexl(i,j,km) > 0.d0) then
        k = ktbtm(i,j)
        tvolt(k)= tvolt(k) + volt(i, j, km) * nendidx(j)
        trhovol(k) = trhovol(k) + rho(i,j,k) * volt(i, j, km) * nendidx(j)
      end if
    end do
  end do
#endif /* OGCM_BBL */
  !
  rholevmean(km)     = 0.d0
  do k = 1, km-1
    rholevmean(k) = trhovol(k) / tvolt(k)
  end do
  !
end subroutine rhoave
!====================================================
subroutine b_vint
  !
  implicit none
  !
  real(8) :: hl1, hl2
  real(8) :: biv, biv_u, biv_l
  !
  bivf(:, :)  = 0.e0
  bivuf(:, :) = 0.e0
  bivlf(:, :) = 0.e0
  !
  do j = 1, jmut
    do i = 2, imut
      biv = 0.d0
      biv_u = 0.d0
      biv_l = 0.d0
      !
      do k = 1, ksgm
        hl2 = atexl(i,j,k) * grav * dz(k)               &
          &        * (thcksgm + ssh(i,j)) * thcksgmr    &
          &        * (rho(i,j,k) -rholevmean(k))
        biv = biv +  hl2
        biv_u = biv_u +  hl2
      end do
      !
      do k = ksgm+1, k_bound
        hl2 = atexl(i,j,k) * grav                       &
          &        * (rho(i,j,k) -rholevmean(k))        &
          &        * max(dzu(i-1,j  ,k), dzu(i,j  ,k),  &
          &              dzu(i-1,j-1,k), dzu(i,j-1,k) )
        biv = biv +  hl2
        biv_u = biv_u +  hl2
      end do
      !
      do k = k_bound+1, km-1
        hl2 = atexl(i,j,k) * grav                &
          &        * (rho(i,j,k) -rholevmean(k))        &
          &        * max(dzu(i-1,j  ,k), dzu(i,j  ,k),  &
          &              dzu(i-1,j-1,k), dzu(i,j-1,k) )
        biv = biv +  hl2
        biv_l = biv_l +  hl2
      end do
#ifdef OGCM_BBL
      k = ktbtm(i,j)
      if(k > ksgm) then
        hl2 = atexl(i,j,km) * grav * dz(km)             &
          &        * (rho(i,j,k) -rholevmean(k))
        biv = biv +  hl2
        if(k <= k_bound) then
          biv_u = biv_u +  hl2
        else if(k < km) then
          biv_l = biv_l +  hl2
        end if
      end if
#endif /* OGCM_BBL */
      !
      bivf(i, j)  = - real(biv, 4)   * 1.e-1  !  buoyancy x dz : cgs -> MKS
      bivuf(i, j) = - real(biv_u, 4) * 1.e-1  !  buoyancy x dz : cgs -> MKS
      bivlf(i, j) = - real(biv_l, 4) * 1.e-1  !  buoyancy x dz : cgs -> MKS
    end do
  end do
  !
  where(atexl(2:imut, 2:jmut, 1) == 0.d0)  
    bivf(2:imut, 2:jmut) = UNDEF
    bivuf(2:imut, 2:jmut) = UNDEF
  end where
  !
  where(atexl(2:imut, 2:jmut, k_bound +1) == 0.d0)  
    bivlf(2:imut, 2:jmut) = UNDEF
  end where
  !
  bivf(1, 2:jmut) = bivf(imut-3, 2:jmut)
  bivuf(1, 2:jmut) = bivuf(imut-3, 2:jmut)
  bivlf(1, 2:jmut) = bivlf(imut-3, 2:jmut)
end subroutine b_vint
!====================================================
end program buoyancy_vertically_integrated
