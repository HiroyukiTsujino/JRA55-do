!-*-F90-*-
!hs2prs.F90
!====================================================
!
! Pressure at TS-point
!
!====================================================
program hs2pressure
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
  &   ktbtm
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
  !
  real(8)    :: prs(imut, jmut, 0:km)
  !
  real(4), parameter :: UNDEF =  -9.99e33
  real(4), parameter :: unit_cgs2hPa = 1.0e-3
  !
  real(4)    :: d3_r4(imut,jmut,km)
  real(4)    :: d2_r4(imut,jmut)
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
  namelist /nml_pressure/ flin_t, flin_s, flin_ssh, flout, fltopo, flsclf
  !
  integer(4), parameter :: mtin_t   = 80
  integer(4), parameter :: mtin_s   = 81
  integer(4), parameter :: mtin_ssh = 82
  integer(4), parameter :: mtout    = 84
  !
  integer(4) :: i, j, k, m
  !
  !==============================================
  !
  ! 入力パラメタ規定値
  !
  flin_t  = 'hs_t.d'
  flin_s  = 'hs_s.d'
  flin_ssh= 'hs_ssh.d'
  flout   = 'pressure.d'
  fltopo  = 'topo.d'
  flsclf  = 'scale_factor.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml_pressure)
  print *,'flin_t   :', trim(flin_t)
  print *,'flin_s   :', trim(flin_s)
  print *,'flin_ssh :', trim(flin_ssh)
  print *,'flout    :', trim(flout)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  !
  !----------------------------------------------
  !
  call read_topo(fltopo)
  !
  pd(1:km) = grav * unit_bar * dp(1:km) * rho0
  !
  !==============================================
  !
  ! 入出力ファイルオープン
  !
  open (mtin_t, file=flin_t, form='unformatted', access='direct', recl=4*imut*jmut*km)
  open (mtin_s, file=flin_s, form='unformatted', access='direct', recl=4*imut*jmut*km)
  open (mtin_ssh, file=flin_ssh, form='unformatted', access='direct', recl=4*imut*jmut)
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
      if(atexl(i,j,k) == 0.d0) then
        t(i, j, k) = atexlbbl(i, j, 1) * t(i, j, km)
        s(i, j, k) = atexlbbl(i, j, 1) * s(i, j, km)
      end if
    end do
  end do
#endif /* OGCM_BBL */
  !
  !
  !-------------------------
  !  SSH
  !
  read (mtin_ssh, rec=1) d2_r4
  ssh(:,:) = atexl(:,:,1)*real(d2_r4(:,:),8)
  !
#ifdef OGCM_CYCLIC
  t(1:2,         1:jmut, 1:km)=t(imut-3:imut-2, 1:jmut, 1:km)
  t(imut-1:imut, 1:jmut, 1:km)=t(3:4,           1:jmut, 1:km)
  !
  s(1:2,         1:jmut, 1:km)=s(imut-3:imut-2, 1:jmut, 1:km)
  s(imut-1:imut, 1:jmut, 1:km)=s(3:4,           1:jmut, 1:km)
  !
  ssh(1:2,         1:jmut)=ssh(imut-3:imut-2, 1:jmut)
  ssh(imut-1:imut, 1:jmut)=ssh(3:4,           1:jmut)
#endif /* OGCM_CYCLIC */
  !
  close ( mtin_t )
  close ( mtin_s )
  close ( mtin_ssh)
  !
  !-------------------------
  !
  rho(1:imut, 1:jmut, 1:km) = 0.d0
  call dens(imut, jmut, km, t, s, pd, rho)
  rho(1:imut, 1:jmut, 1:km) = rho(1:imut, 1:jmut, 1:km) + 1.d0
  !
  !=========================
  !
  call calc_pressure
  !
  open (mtout, file=flout, form='unformatted', access='direct', recl=4*imut*jmut)
  !
  do k = 1, km
    write(mtout, rec=k) d3_r4(1:imut, 1:jmut, k)
  end do
  write(mtout, rec=km+1) d2_r4(1:imut, 1:jmut)
  !
  close ( mtout )
  !
contains
!====================================================
!
!
!====================================================
subroutine calc_pressure
  !
  implicit none
  !
  real(8) :: hl1, hl2
  !
  prs(1:imut, 1:jmut, 0:km)   = 0.0d0
  d3_r4(1:imut, 1:jmut, 1:km) = 0.0e0
  d2_r4(1:imut, 1:jmut) = 0.0e0
  !
  do j = 2, jmut
    do i = 2, imut
      do k = 1, ksgm
        hl1 = atexl(i,j,k) * grav * dz(k) * 0.5d0       &
             &        * (thcksgm + ssh(i,j)) * thcksgmr    &
             &        * rho(i,j,k)
        prs(i,j,k) = atexl(i,j,k) * (prs(i,j,k-1) + hl1) ! half level
        d3_r4(i,j,k) = real(prs(i,j,k), 4) * unit_cgs2hPa
        prs(i,j,k) = prs(i,j,k) + hl1 ! full level
      end do
      !
#ifdef OGCM_BBL
      do k = ksgm + 1, km - 1
#else /* OGCM_BBL */
      do k = ksgm + 1, km
#endif /* OGCM_BBL */
        hl2 = max(dzu(i-1,j,k),dzu(i,j,k),dzu(i-1,j-1,k),dzu(i,j-1,k))
        hl1 = atexl(i,j,k) * grav * hl2 * 0.5d0 * rho(i,j,k)
        prs(i,j,k) = atexl(i,j,k) * (prs(i,j,k-1) + hl1)
        d3_r4(i,j,k) = real(prs(i,j,k), 4) * unit_cgs2hPa
        prs(i,j,k) = prs(i,j,k) + hl1
      end do
#ifdef OGCM_BBL
      k = ktbtm(i,j)
      if (k > ksgm .and. atexlbbl(i,j,1) == 1.d0) then
        hl1 = atexlbbl(i,j,1) * grav * dz(km) * 0.5d0 * rho(i,j,k)
        prs(i,j,k)  = atexlbbl(i,j,1) * (prs(i,j,k-1) + hl1) ! half level
        d3_r4(i,j,k)  = real(prs(i,j,k), 4) * unit_cgs2hPa
        d3_r4(i,j,km) = real(prs(i,j,k), 4) * unit_cgs2hPa
        prs(i,j,k)  = prs(i,j,k) + hl1 ! full level
      end if
#endif /* OGCM_BBL */
      d2_r4(i,j)  = real(prs(i,j,ktbtm(i,j)), 4) * unit_cgs2hPa
    end do
  end do
  !
  do k = 1, km
    where(d3_r4(1:imut, 1:jmut, k) == 0.0e0)  
      d3_r4(1:imut, 1:jmut, k) = UNDEF
    end where
  end do

  where(d2_r4(1:imut, 1:jmut) == 0.0e0)  
    d2_r4(1:imut, 1:jmut) = UNDEF
  end where
  !
end subroutine calc_pressure
!====================================================
end program hs2pressure
