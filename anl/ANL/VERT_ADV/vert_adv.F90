!-*-F90-*-
!vert_adv.F90
!====================================================
!
! Diagnose vertical tracer transport
!    at the bottom of TS-Cell from u, v, and ssh
!
!====================================================
program vert_adv
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ksgm, dz
  !
  use oc_structure, only : &
  &   read_topo,           & !------------------
  &   aexl, atexl,         & ! 海陸インデックス
  &   dzu,                 & ! UVボックスの厚さ
  &   dsgm,                &
  &   thcksgm, thcksgmr,   &
  &   ktbtm,               &
#ifdef OGCM_BBL
  &   aexlbbl, atexlbbl,   & ! BBLインデックス
  &   texnnbbl,            &
#endif /* OGCM_BBL */
  &   read_scale,          & !------------------
  &   a_tl  , a_tr  ,      & ! 格子面積
  &   a_bl  , a_br  ,      &
  &   set_area,            & !------------------
  &   areat
  !
  !----------------------------------------------
  !
  implicit none
  !
  real(4), save :: UNDEF =  0.0e0
  !
  real(8)    :: trcr(imut, jmut, km)
  real(8)    :: wlwl(imut, jmut, km)
  real(8)    :: wtrc(imut, jmut, km)
  real(8)    :: dzt (imut, jmut, km)
  real(8)    :: ssh (imut, jmut)
  real(8)    :: hu  (imut, jmut)
  !
  real(4)    :: d3_r4(imut,jmut,km)
  real(4)    :: d2_r4(imut,jmut)
  !
  ! 入出力ファイル
  !
  character(len=256) :: flin_tr  ! 入力ファイル
  character(len=256) :: flin_wl  ! 入力ファイル
  character(len=256) :: flin_ssh ! 入力ファイル
  character(len=256) :: fltopo   ! 海底地形ファイル
  character(len=256) :: flsclf   ! スケールファクター・ファイル
  character(len=256) :: flout_wt ! 出力ファイル

  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mtin_tr = 81
  integer(4), parameter :: mtin_wl = 82
  integer(4), parameter :: mtin_ssh = 83
  integer(4), parameter :: mtout_wt = 86

#ifdef OGCM_BBL
  integer(4), parameter :: kmax = km-1
#else /* OGCM_BBL */
  integer(4), parameter :: kmax = km
#endif /* OGCM_BBL */
  !
  real(8)    :: hl1, hl2, hl3, hl4
  !
  integer(4) :: i, j, k
  integer(4) :: ktmp
  !
  !==============================================

  namelist /nml_vertadv/ flin_tr, flin_wl, flin_ssh, &
       & flout_wt, undef, &
       & fltopo, flsclf

  !----------------------------------------------
  ! 入力パラメタ既定値
  !----------------------------------------------

  flin_tr  = 'hs_t.gd'
  flin_wl  = 'hs_wlwl.gd'
  flin_ssh  = 'hs_ssh.gd'
  flout_wt = 'hs_wt.gd'
  fltopo = 'topo.d'
  flsclf = 'scale_factor.d'

  !----------------------------------------------
  ! 標準入力から読み込み
  !----------------------------------------------

  read(unit=5, nml_vertadv)

  print *,'flin_tr  :', trim(flin_tr)
  print *,'flin_wl  :', trim(flin_wl)
  print *,'flin_ssh :', trim(flin_ssh)
  print *,'flout_wt :', trim(flout_wt)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  print *,'UNDEF    :', undef

  !----------------------------------------------
  !  地形の読み込み
  !----------------------------------------------
  !
  call read_topo(fltopo)
  !
  !----------------------------------------------
  !  スケールファクタの読み込み
  !----------------------------------------------
  !
  call read_scale(flsclf)
  !
  call set_area
  !

  !==============================================
  ! 入出力ファイルオープン
  !----------------------------------------------
  open (mtin_tr, file=flin_tr, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_wl, file=flin_wl, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_ssh, file=flin_ssh, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)
  !----------------------------------------------
  ! 読み込み、倍精度実数への変換
  !----------------------------------------------
  !
  ! tracer
  read (mtin_tr, rec=1) d3_r4
  trcr(:,:,:) = atexl(:,:,:)*real(d3_r4(:,:,:),8)
  !
  trcr(1:2,         1:jmut, 1:km) = trcr(imut-3:imut-2, 1:jmut, 1:km)
  trcr(imut-1:imut, 1:jmut, 1:km) = trcr(3:4,           1:jmut, 1:km)
  !
  ! vertical transport
  read (mtin_wl, rec=1) d3_r4
  do k = 1, km - 1
    wlwl(1:imut,1:jmut,k) = atexl(1:imut,1:jmut,k)*real(d3_r4(1:imut,1:jmut,k),8)
  end do
  !
  wlwl(1:imut,1:jmut,km) = 0.0d0
  !
  wlwl(1:2,         1:jmut, 1:km) = wlwl(imut-3:imut-2, 1:jmut, 1:km)
  wlwl(imut-1:imut, 1:jmut, 1:km) = wlwl(3:4,           1:jmut, 1:km)
  !
  !  SSH
  read (mtin_ssh, rec=1) d2_r4
  ssh(:,:) = atexl(:,:,1)*real(d2_r4(:,:),8)
  !
  ssh(1:2,         1:jmut)=ssh(imut-3:imut-2, 1:jmut)
  ssh(imut-1:imut, 1:jmut)=ssh(3:4,           1:jmut)
  !----------------------------------------------

  close (mtin_tr)
  close (mtin_wl)
  close (mtin_ssh)

  !----------------------------------------------

  hu(1:imut,1:jmut) = 0.0d0

  do j = 1, jmut - 1
    do i = 1, imut - 1
      hl1 = a_bl(i,j) + a_br(i,j) + a_tl(i,j) + a_tr(i,j)
      hu(i,j) = a_bl(i,j) * ssh(i,j  ) + a_br(i,j) * ssh(i+1,j  ) &
           &  + a_tl(i,j) * ssh(i,j+1) + a_tr(i,j) * ssh(i+1,j+1)
      hu(i,j) = aexl(i,j,1) * hu(i,j) / hl1
    end do
  end do

  do k = 1, ksgm
    do j = 2, jmut - 1
      do i = 2, imut - 1
        dzu(i,j,k) = aexl(i,j,k) * (dzu(i,j,k) + hu(i,j) * dsgm(k))
      end do
    end do
  end do

  dzt(:,:,:) = 0.0d0
  do k = 1, km
    do j = 2, jmut - 1
      do i = 2, imut - 1
        dzt(i,j,k) = max(dzu(i,j,k),dzu(i-1,j,k),dzu(i,j-1,k),dzu(i-1,j-1,k))
      end do
    end do
  end do

  wtrc(1:imut, 1:jmut, 1:km) = 0.d0

  !----------------------------------------------

  do j = 2, jmut
    do i = 2, imut
      do k = 1, ktbtm(i,j) - 2
        wtrc(i,j,k) = wlwl(i,j,k) * atexl(i,j,k+1) &
             & * (dzt(i,j,k+1) * trcr(i,j,k) + dzt(i,j,k) * trcr(i,j,k+1)) &
             & / (dzt(i,j,k+1) + dzt(i,j,k) + 1.0d0 - atexl(i,j,k+1))
      end do
      if (ktbtm(i,j) > 1) then
#ifdef OGCM_BBL
        if (atexlbbl(i,j,1) == 1.0d0) then ! ktbtm is BBL
          ktmp = ktbtm(i,j) - 1
          wtrc(i,j,ktmp) = wlwl(i,j,ktmp) * atexl(i,j,km) &
               & * (dzt(i,j,km) * trcr(i,j,ktmp) + dzt(i,j,ktmp) * trcr(i,j,km)) &
               & / (dzt(i,j,km) + dzt(i,j,ktmp) + 1.0d0 - atexl(i,j,km))
          wtrc(i,j,km-1) = wtrc(i,j,ktmp)
        else ! ktbtm is normal ocean grid
          ktmp = ktbtm(i,j) - 1
          wtrc(i,j,ktmp) = wlwl(i,j,ktmp) * atexl(i,j,ktmp+1) &
               & * (dzt(i,j,ktmp+1) * trcr(i,j,ktmp) + dzt(i,j,ktmp) * trcr(i,j,ktmp+1)) &
               & / (dzt(i,j,ktmp+1) + dzt(i,j,ktmp) + 1.0d0 - atexl(i,j,ktmp+1))
        end if
#else /* OGCM_BBL */
        ktmp = ktbtm(i,j) - 1
        wtrc(i,j,ktmp) = wlwl(i,j,ktmp) * atexl(i,j,ktmp+1) &
             & * (dzt(i,j,ktmp+1) * trcr(i,j,ktmp) + dzt(i,j,ktmp) * trcr(i,j,ktmp+1)) &
             & / (dzt(i,j,ktmp+1) + dzt(i,j,ktmp) + 1.0d0 - atexl(i,j,ktmp+1))
#endif /* OGCM_BBL */
      end if
    end do
  end do

  !==============================================
  ! apply mask (undef value)

  wtrc(1:imut,1:jmut,km) = undef
  do k = 1, km - 1
    do j = 1, jmut
      do i = 1, imut
        if (atexl(i,j,k+1) == 0.0d0) then
          wtrc(i,j,k) = undef
        end if
      end do
    end do
  end do

#ifdef OGCM_BBL
  do j = 1, jmut
    do i = 1, imut
      if (atexlbbl(i,j,1) == 1.0d0) then
        wtrc(i,j,ktbtm(i,j)-1) = wtrc(i,j,km-1)
      end if
    end do
  end do
#endif /* OGCM_BBL */

  open (mtout_wt, file=flout_wt, form='unformatted',&
       &  access='direct', recl=4*imut*jmut*km)
  write(mtout_wt, rec=1) real(wtrc(1:imut, 1:jmut, 1:km),4)
  close(mtout_wt)

!====================================================
end program vert_adv
