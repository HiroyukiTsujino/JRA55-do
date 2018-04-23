!-*-F90-*-
!gm2wlwl.F90
!====================================================
!
! Diagnose WLWL (vertical volume flux)
!    at the top of TS-Cell from u, v, and ssh
!
!====================================================
program gm2wlwl
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ksgm, dz
  !
  use oc_structure, only : &
  &   read_topo,           & !------------------
  &   dep,                 & ! UVTSボックス境界（上面）の深さ
  &   aexl, atexl,         & ! 海陸インデックス
  &   coefx, coefy,        &
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
  &   dx_tl , dx_tr ,      & ! 東西長
  &   dx_bl , dx_br ,      &
  &   dy_tl , dy_tr ,      & ! 南北長
  &   dy_bl , dy_br ,      &
  &   set_area,            & !------------------
  &   areat
  !
  !----------------------------------------------
  !
  implicit none
  !
  real(4), save :: UNDEF =  0.0e0
  !
  ! 海洋モデル地形
  !
  real(8)    :: u(imut, jmut, km)
  real(8)    :: v(imut, jmut, km)
  real(8)    :: ssh(imut, jmut)
  real(8)    :: hu (imut, jmut)
  !
  real(8)    :: umo(imut, jmut, km)
  real(8)    :: vmo(imut, jmut, km)
  real(8)    :: wlwl(imut, jmut, km)
  !
  real(4)    :: w(imut, jmut, km)
  !
  real(4)    :: d3_r4(imut,jmut,km)
  real(4)    :: d2_r4(imut,jmut)
  !
  ! 入出力ファイル
  !
  character(len=256) :: flin_u ! 入力ファイル
  character(len=256) :: flin_v ! 入力ファイル
  character(len=256) :: flin_ssh  ! 入力ファイル
  character(len=256) :: fltopo ! 海底地形ファイル
  character(len=256) :: flsclf ! スケールファクター・ファイル
  character(len=256) :: flout_ul ! 出力ファイル
  character(len=256) :: flout_vl ! 出力ファイル
  character(len=256) :: flout_wl ! 出力ファイル

  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mtin_u   = 80
  integer(4), parameter :: mtin_v   = 81
  integer(4), parameter :: mtin_ssh = 83
  integer(4), parameter :: mtout_ul = 84
  integer(4), parameter :: mtout_vl = 85
  integer(4), parameter :: mtout_wl = 86

  logical :: l_out_ul, l_out_vl, l_out_wl

  logical :: l_upward

#ifdef OGCM_BBL
  integer(4), parameter :: kmax = km-1
#else /* OGCM_BBL */
  integer(4), parameter :: kmax = km
#endif /* OGCM_BBL */
  !
  real(8)    :: ustar, vstar
  real(8)    :: hl1, hl2, hl3, hl4
  !
  integer(4) :: i, j, k
  integer(4) :: k0
  !
  !==============================================

  namelist /nml_gm2wlwl/ flin_u, flin_v, flin_ssh, &
       & l_out_ul, l_out_vl, l_out_wl, &
       & flout_ul, flout_vl, flout_wl, &
       & undef, fltopo, flsclf, l_upward

  !----------------------------------------------
  ! 入力パラメタ既定値
  !----------------------------------------------

  flin_u  = 'hs_u.d'
  flin_v  = 'hs_v.d'
  flin_ssh= 'hs_ssh.d'
  l_out_ul = .true.
  l_out_vl = .true.
  l_out_wl = .true.
  l_upward = .true.
  flout_ul = 'ul.d'
  flout_vl = 'vl.d'
  flout_wl = 'wl.d'
  fltopo = 'topo.d'
  flsclf = 'scale_factor.d'

  !----------------------------------------------
  ! 標準入力から読み込み
  !----------------------------------------------

  read(unit=5, nml_gm2wlwl)

  print *,'flin_u   :', trim(flin_u)
  print *,'flin_v   :', trim(flin_v)
  print *,'flin_ssh :', trim(flin_ssh)
  print *,'flout_ul :', trim(flout_ul)
  print *,'flout_vl :', trim(flout_vl)
  print *,'flout_wl :', trim(flout_wl)
  print *,'l_out_ul :', l_out_ul
  print *,'l_out_vl :', l_out_vl
  print *,'l_out_wl :', l_out_wl
  print *,'l_upward :', l_upward
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
  open (mtin_u, file=flin_u, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_v, file=flin_v, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_ssh, file=flin_ssh, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)
  !
  !----------------------------------------------
  ! 読み込み、倍精度実数への変換
  !----------------------------------------------
  read (mtin_u, rec=1) d3_r4
  u(:,:,:) = coefx(:,:,:)*real(d3_r4(:,:,:),8)
  !
  u(1:2,         1:jmut, 1:km)=u(imut-3:imut-2, 1:jmut, 1:km)
  u(imut-1:imut, 1:jmut, 1:km)=u(3:4,           1:jmut, 1:km)
  !
  ! V
  read (mtin_v, rec=1) d3_r4
  v(:,:,:) = coefy(:,:,:)*real(d3_r4(:,:,:),8)
  !
  v(1:2,         1:jmut, 1:km)=v(imut-3:imut-2, 1:jmut, 1:km)
  v(imut-1:imut, 1:jmut, 1:km)=v(3:4,           1:jmut, 1:km)
  !-------------------------
  !
  !  SSH
  read (mtin_ssh, rec=1) d2_r4
  ssh(:,:) = atexl(:,:,1)*real(d2_r4(:,:),8)
  !
  ssh(1:2,         1:jmut)=ssh(imut-3:imut-2, 1:jmut)
  ssh(imut-1:imut, 1:jmut)=ssh(3:4,           1:jmut)
  !
  !----------------------------------------------

  close (mtin_u)
  close (mtin_v)
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

  !----------------------------------------------
  ! X-ward transport
  !----------------------------------------------

  do j = 2, jmut
    do i = 1, imut - 1
      do k = 1, ksgm
        hl1 = aexl(i,j,k) * (dzu(i, j  , k) + hu(i,j  ) * dsgm(k)) * dy_br(i,j)
        hl2 = aexl(i,j,k) * (dzu(i, j-1, k) + hu(i,j-1) * dsgm(k)) * dy_tr(i,j-1)
        umo(i,j,k) = u(i,j,k) * (hl1 + hl2)
      end do
      do k = ksgm+1, kmax
        hl1 = dzu(i, j  , k) * dy_br(i,j)
        hl2 = dzu(i, j-1, k) * dy_tr(i,j-1)
        umo(i,j,k) = u(i,j,k) * (hl1 + hl2)
      end do
#ifdef OGCM_BBL
      hl1 = dzu(i, j  , km) * dy_br(i,j)
      hl2 = dzu(i, j-1, km) * dy_tr(i,j-1) 
      umo(i,j,km) = u(i,j,km) * (hl1 + hl2)
#endif /* OGCM_BBL */
    end do
#ifdef OGCM_CYCLIC
    umo(imut,j,1:km) = umo(4,j,1:km)
#endif /* OGCM_CYCLIC */
  end do
  !
  !----------------------------------------------
  ! Y-ward transport
  !----------------------------------------------

  do j = 1, jmut - 1
    do i = 2, imut
      do k = 1, ksgm
        hl1 = (dzu(i  ,j,k) + hu(i  ,j) * dsgm(k)) * dx_tl(i,j)
        hl2 = (dzu(i-1,j,k) + hu(i-1,j) * dsgm(k)) * dx_tr(i-1,j)
        vmo(i,j,k) = v(i,j,k) * (hl1 + hl2)
      end do
      do k = ksgm + 1, kmax
        hl1 = dzu(i  ,j,k) * dx_tl(i,j)
        hl2 = dzu(i-1,j,k) * dx_tr(i-1,j)
        vmo(i,j,k) = v(i,j,k) * (hl1 + hl2)
      end do
#ifdef OGCM_BBL
      hl1 = dzu(i  ,j,km) * dx_tl(i,j)
      hl2 = dzu(i-1,j,km) * dx_tr(i-1,j)
      vmo(i,j,km) = v(i,j,km) * (hl1 + hl2)
#endif /* OGCM_BBL */
    end do
  end do
  !
  !----------------------------------------------
  !
  wlwl(1:imut, 1:jmut, 1:km) = 0.d0
  !
  if (l_upward) then

    do j = 2, jmut
      do i = 2, imut

        k0 = ktbtm(i, j)

        if (k0 > 1) then
#ifdef OGCM_BBL
          if (atexlbbl(i,j,1) == 1.0d0) then
            wlwl(i, j, km-texnnbbl(i,j)) = atexlbbl(i, j, 1) *                        &
                 & ( umo(i-1, j, km)*coefx(i-1, j, km) -umo(i, j, km)*coefx(i, j, km) &
                 &  +vmo(i, j-1, km)*coefy(i, j-1, km) -vmo(i, j, km)*coefy(i, j, km))
            wlwl(i,j,ktbtm(i,j)-texnnbbl(i,j)) = wlwl(i,j,km-texnnbbl(i,j))
          else
            wlwl(i, j, k0-1) = atexl(i, j, k0) *                                  &
                 & ( umo(i-1, j, k0)*coefx(i-1, j, k0) -umo(i, j, k0)*coefx(i, j, k0) &
                 &  +vmo(i, j-1, k0)*coefy(i, j-1, k0) -vmo(i, j, k0)*coefy(i, j, k0))
          end if
#else /* OGCM_BBL */
          wlwl(i, j, k0-1) = atexl(i, j, k0) *                                  &
               & ( umo(i-1, j, k0)*coefx(i-1, j, k0) -umo(i, j, k0)*coefx(i, j, k0) &
               &  +vmo(i, j-1, k0)*coefy(i, j-1, k0) -vmo(i, j, k0)*coefy(i, j, k0))
#endif /* OGCM_BBL */
          if (k0 > 2) then
            do k = k0-1, 2, -1
              wlwl(i, j, k-1) = wlwl(i, j, k) + atexl(i, j, k) *                 &
                   & ( umo(i-1, j, k)*coefx(i-1, j, k) -umo(i, j, k)*coefx(i, j, k) &
                   &  +vmo(i, j-1, k)*coefy(i, j-1, k) -vmo(i, j, k)*coefy(i, j, k))
            end do
          end if
        end if
      end do
    end do

  else

    do j = 2, jmut
      do i = 2, imut

        k0 = ktbtm(i, j)

        if (k0 > 1) then
          wlwl(i, j, 1) = - atexl(i, j, 2) *                                    &
               & ( umo(i-1, j, 1)*coefx(i-1, j, 1) -umo(i, j, 1)*coefx(i, j, 1) &
               &  +vmo(i, j-1, 1)*coefy(i, j-1, 1) -vmo(i, j, 1)*coefy(i, j, 1))
          do k = 2, k0 - 1
            wlwl(i, j, k) = wlwl(i, j, k-1) - atexl(i, j, k) *                 &
                 & ( umo(i-1, j, k)*coefx(i-1, j, k) -umo(i, j, k)*coefx(i, j, k) &
                 &  +vmo(i, j-1, k)*coefy(i, j-1, k) -vmo(i, j, k)*coefy(i, j, k))
          end do
          
#ifdef OGCM_BBL
          if (atexlbbl(i,j,1) == 1.0d0) then
            wlwl(i, j, km-texnnbbl(i,j)) = atexlbbl(i, j, 1) *                        &
                 & ( umo(i-1, j, km)*coefx(i-1, j, km) -umo(i, j, km)*coefx(i, j, km) &
                 &  +vmo(i, j-1, km)*coefy(i, j-1, km) -vmo(i, j, km)*coefy(i, j, km))
            wlwl(i,j,ktbtm(i,j)-texnnbbl(i,j)) = wlwl(i,j,km-texnnbbl(i,j))
          end if
#endif /* OGCM_BBL */

        end if
      end do
    end do
  end if

  !==============================================

  if (l_out_ul) then
    do k = 1, km
      do j = 1, jmut
        do i = 1, imut
          if (coefx(i,j,k) == 0.0d0) then
            umo(i,j,k) = undef
          end if
        end do
      end do
    end do

    open (mtout_ul, file=flout_ul, form='unformatted',&
         &  access='direct', recl=4*imut*jmut*km)
    write(mtout_ul, rec=1) real(umo(1:imut, 1:jmut, 1:km),4)
    close(mtout_ul)
  end if

  !==============================================

  if (l_out_vl) then
    do k = 1, km
      do j = 1, jmut
        do i = 1, imut
          if (coefy(i,j,k) == 0.0d0) then
            vmo(i,j,k) = undef
          end if
        end do
      end do
    end do

    open (mtout_vl, file=flout_vl, form='unformatted',&
         &  access='direct', recl=4*imut*jmut*km)
    write(mtout_vl, rec=1) real(vmo(1:imut, 1:jmut, 1:km),4)
    close(mtout_vl)
  end if

  !==============================================

  if (l_out_wl) then

    wlwl(1:imut,1:jmut,km) = undef

    do k = 1, km - 1
      do j = 1, jmut
        do i = 1, imut
          if (atexl(i,j,k+1) == 0.0d0) then
            wlwl(i,j,k) = undef
          end if
        end do
      end do
    end do

#ifdef OGCM_BBL
    do j = 1, jmut
      do i = 1, imut
        if (atexlbbl(i,j,1) > 0.0d0) then
!          write(6,*) 'replace interior'
          wlwl(i,j,ktbtm(i,j)-1) = wlwl(i,j,km-1)
        end if
      end do
    end do
#endif /* OGCM_BBL */


    open (mtout_wl, file=flout_wl, form='unformatted',&
         &  access='direct', recl=4*imut*jmut*km)
    write(mtout_wl, rec=1) real(wlwl(1:imut, 1:jmut, 1:km),4)
    close(mtout_wl)

  end if

!====================================================
end program gm2wlwl
