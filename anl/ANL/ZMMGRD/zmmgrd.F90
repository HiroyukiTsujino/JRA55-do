! -*-F90-*-

!hs2zm.F90
!====================================================
!
! Make Meridional Gradient of Zonal Mean
!
!====================================================
program zonal_mean_meridional_gradient
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ksgm, dz,            &
  &   radius, pi,          &
  &   radian, radian_r,    &
  &   slat0, slon0,        &
  &   nplat, nplon,        &
  &   splat, splon,        &
  &   ro,    cp
#ifndef OGCM_VARIABLE
  use oc_mod_param, only : &
  &   dxtdgc, dytdgc
#endif /* ! OGCM_VARIABLE */
  !
  use oc_structure,    only : &
  &   read_topo,              & !----------------------------
  &   aexl, atexl,            & ! 海陸インデックス
  &   dzu,                    & ! UVボックスの厚さ
  &   dp, dep,                & ! UVTSボックス境界（上面）の深さ
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
  real(8), parameter :: dlatg = 1.0d0  ! 緯度格子間隔
  real(8), parameter :: slatg = real(slat0, 8)
  real(8), parameter :: dy = dlatg * radius * radian_r * 1.0d-2 ! MKS
  integer(4), parameter :: jmgeo = 75 + 1 - slat0 ! 1 degree interval -78 to 75

  real(8)    :: tzmg(jmgeo, km)  ! Global
  real(8)    :: tzma(jmgeo, km)  ! Atlantic
  real(8)    :: tzmp(jmgeo, km)  ! Pacific
  real(8)    :: tzmi(jmgeo, km)  ! Indian

  real(4)    :: dat_zm(jmgeo, km) ! work of IO

  real(8)    :: tzmgy(jmgeo, km)  ! Global
  real(8)    :: tzmay(jmgeo, km)  ! Atlantic
  real(8)    :: tzmpy(jmgeo, km)  ! Pacific
  real(8)    :: tzmiy(jmgeo, km)  ! Indian
  !
  ! depth average
  !
  real(8)    :: tzmgy_dav(jmgeo)  ! Global
  real(8)    :: tzmay_dav(jmgeo)  ! Atlantic
  real(8)    :: tzmpy_dav(jmgeo)  ! Pacific
  real(8)    :: tzmiy_dav(jmgeo)  ! Indian
  logical    :: l_depth_average = .false.
  real(8)    :: dst, ded, depth_integ
  integer(4) :: kst, ked
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flin   ! 入力ファイル
  character(len=256)    :: flout  ! 出力ファイル
  character(len=256)    :: flout2 ! 出力ファイル

  real(4) :: undef4_in = 0.0
  real(4) :: undef4_out = 0.0

  real(8) :: undef8_in
  real(8) :: undef8_out

  character(len=256)    :: fltopo ! 海底地形ファイル
  character(len=256)    :: flsclf ! スケールファクター・ファイル
#ifdef OGCM_VARIABLE
  character(len=256)    :: file_vgrid ! 可変格子定義ファイル
#endif /* OGCM_VARIABLE */
  namelist /nml_zm_mgrd/  flin, flout, flout2, fltopo, flsclf, &
       & undef4_in, undef4_out, dst, ded, l_depth_average
#ifdef OGCM_VARIABLE
  namelist /inflg/ file_vgrid
#endif /* OGCM_VARIABLE */
  !
  integer(4) :: ios          !  入出力エラーチェック用

  integer(4), parameter :: mtin     = 82
  integer(4), parameter :: mtout    = 86
  integer(4), parameter :: mtout2   = 87
  !
  integer(4), parameter :: lrec_in = jmgeo*km*4
  integer(4), parameter :: lrec_out = jmgeo*km*4
  integer(4), parameter :: lrec_out2 = jmgeo*4
  !
  integer(4) :: ireci, irecw, irecw2
  integer(4) :: i, j, k, m, jj
  integer(4) :: jg
  !
  !==============================================
  !
  ! 入力パラメタ規定値
  !
  flin    = 'zm_t.d'
  flout   = 'zm_t_mgrd.d'
  flout2  = 'zm_t_mgrd_dav.d'
  fltopo  = 'topo.d'
  flsclf  = 'scale_factor.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml_zm_mgrd)
  print *,'flin     :', trim(flin)
  print *,'flout    :', trim(flout)
  print *,'flout2   :', trim(flout2)
  print *,'undef_in :', undef4_in
  print *,'undef_out:', undef4_out
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  print *,'depth average :',l_depth_average
#ifdef OGCM_VARIABLE
  read(unit=5, inflg) ! file_vgrid
#endif /* OGCM_VARIABLE */
  !
  undef8_in  = real(undef4_in ,8)
  undef8_out = real(undef4_out,8)

  !----------------------------------------------
#ifdef OGCM_VARIABLE
  call set_hgrids(file_vgrid)
#else /* OGCM_VARIABLE */
  call set_hgrids
#endif /* OGCM_VARIABLE */
  !----------------------------------------------
  !
  call read_topo(fltopo)
  !
  !----------------------------------------------
  !
  call read_scale(flsclf)
  !
  !==============================================
  if (l_depth_average) then
    do k = 1, km
      if (dst < dp(k)) then
        kst = k
        exit
      end if
    end do
    do k = 1, km
      if (ded < dp(k)) then
        ked = k - 1
        exit
      end if
    end do
    print *,'dst - ded:', dep(kst) ,' - ',dep(ked+1)
  end if
  !-----------------------------------------------
  !
  ! 入出力ファイルオープン
  !
  open (mtin, file=flin, form='unformatted', &
    &  access='direct', recl=lrec_in)
  ireci=0

  open (mtout, file=flout, form='unformatted',&
    &  access='direct', recl=lrec_out)
  irecw=0

  if (l_depth_average) then
    open (mtout2, file=flout2, form='unformatted',&
         &  access='direct', recl=lrec_out2)
    irecw2=0
  end if

  !----------------------------------------------

  ireci = ireci + 1
  read (mtin, rec=ireci) dat_zm
  tzmg(:,:) = real(dat_zm(:,:),8)

  ireci = ireci + 1
  read (mtin, rec=ireci) dat_zm
  tzma(:,:) = real(dat_zm(:,:),8)

  ireci = ireci + 1
  read (mtin, rec=ireci) dat_zm
  tzmp(:,:) = real(dat_zm(:,:),8)

  ireci = ireci + 1
  read (mtin, rec=ireci) dat_zm
  tzmi(:,:) = real(dat_zm(:,:),8)

  close ( mtin )

  !--------------------
  !  Global
  !--------------------
  print *, '       Global '

  tzmgy(1:jmgeo, 1:km) = undef8_out

  do k = 1, km
    do j = 1, jmgeo - 1
      if ((tzmg(j,k) /= undef8_in) .and. (tzmg(j+1,k) /= undef8_in)) then
        tzmgy(j,k) = (tzmg(j+1,k) - tzmg(j,k)) / dy
      end if
    end do
  end do

  if (l_depth_average) then

    tzmgy_dav(1:jmgeo) = 0.0d0

    do j = 1, jmgeo
      depth_integ = 0.0d0
      do k = kst, ked
        if (tzmgy(j,km-k+1) /= undef8_out) then
          tzmgy_dav(j) = tzmgy_dav(j) + tzmgy(j,km-k+1) * dz(k)
          depth_integ = depth_integ + dz(k)
        else
          exit
        end if
      end do
      write(6,*) depth_integ
      if (depth_integ > 0.0d0) then
        tzmgy_dav(j) = tzmgy_dav(j) / depth_integ
!        write(6,*) j, tzmgy_dav(j)
      else
        tzmgy_dav(j) = undef8_out
      end if
    end do

  end if

!  where(tzmgy(1:jmgeo,1:km) == 0.0d0)
!    tzmgy(1:jmgeo, 1:km) = undef8_out
!  end where

  !--------------------
  !  Atlantic
  !--------------------
  print *, '       Atlantic '

  tzmay(1:jmgeo, 1:km) = undef8_out

  do k = 1, km
    do j = 1, jmgeo - 1
      if ((tzma(j,km) /= undef8_in) .and. (tzma(j+1,k) /= undef8_in)) then
        tzmay(j,k) = (tzma(j+1,k) - tzma(j,k)) / dy
      end if
    end do
  end do

  if (l_depth_average) then

    tzmay_dav(1:jmgeo) = 0.0d0

    do j = 1, jmgeo
      depth_integ = 0.0d0
      do k = kst, ked
        if (tzmay(j,km-k+1) /= undef8_out) then
          tzmay_dav(j) = tzmay_dav(j) + tzmay(j,km-k+1) * dz(k)
          depth_integ = depth_integ + dz(k)
        else
          exit
        end if
      end do
      if (depth_integ > 0.0d0) then
        tzmay_dav(j) = tzmay_dav(j) / depth_integ
      else
        tzmay_dav(j) = undef8_out
      end if
    end do
  end if

!  where(tzmay(1:jmgeo,1:km) == 0.0d0)
!    tzmay(1:jmgeo, 1:km) = undef8_out
!  end where

  !--------------------
  !  Pacific
  !--------------------
  print *, '       Pacific '

  tzmpy(1:jmgeo, 1:km) = undef8_out

  do k = 1, km
    do j = 1, jmgeo - 1
      if ((tzmp(j,k) /= undef8_in) .and. (tzmp(j+1,k) /= undef8_in)) then
        tzmpy(j,k) = (tzmp(j+1,k) - tzmp(j,k)) / dy
      end if
    end do
  end do

  if (l_depth_average) then
    tzmpy_dav(1:jmgeo) = 0.0d0

    do j = 1, jmgeo
      depth_integ = 0.0d0
      do k = kst, ked
        if (tzmpy(j,km-k+1) /= undef8_out) then
          tzmpy_dav(j) = tzmpy_dav(j) + tzmpy(j,km-k+1) * dz(k)
          depth_integ = depth_integ + dz(k)
        else
          exit
        end if
      end do
      if (depth_integ > 0.0d0) then
        tzmpy_dav(j) = tzmpy_dav(j) / depth_integ
      else
        tzmpy_dav(j) = undef8_out
      end if
    end do
  end if

!  where(tzmpy(1:jmgeo,1:km) == 0.0d0)
!    tzmpy(1:jmgeo, 1:km) = undef8_out
!  end where

  !--------------------
  !  Indian Ocean
  !--------------------
  print *, '       Indian '

  tzmiy(1:jmgeo, 1:km) = undef8_out

  do k = 1, km
    do j = 1, jmgeo - 1
      if ((tzmi(j,k) /= undef8_in) .and. (tzmi(j+1,k) /= undef8_in)) then
        tzmiy(j,k) = (tzmi(j+1,k) - tzmi(j,k)) / dy
      end if
    end do
  end do

  if (l_depth_average) then
    tzmiy_dav(1:jmgeo) = 0.0d0

    do j = 1, jmgeo
      depth_integ = 0.0d0
      do k = kst, ked
        if (tzmiy(j,km-k+1) /= undef8_out) then
          tzmiy_dav(j) = tzmiy_dav(j) + tzmiy(j,km-k+1) * dz(k)
          depth_integ = depth_integ + dz(k)
        else
          exit
        end if
      end do
      if (depth_integ > 0.0d0) then
        tzmiy_dav(j) = tzmiy_dav(j) / depth_integ
      else
        tzmiy_dav(j) = undef8_out
      end if
    end do
  end if

!  where(tzmiy(1:jmgeo,1:km) == 0.0d0)
!    tzmiy(1:jmgeo, 1:km) = undef8_out
!  end where

  !--------------------------------------------

  irecw = irecw + 1
  write ( mtout, rec=irecw ) real(tzmgy(1:jmgeo,1:km),4)

  irecw = irecw + 1
  write ( mtout, rec=irecw ) real(tzmay(1:jmgeo,1:km),4)

  irecw = irecw + 1
  write ( mtout, rec=irecw ) real(tzmpy(1:jmgeo,1:km),4)

  irecw = irecw + 1
  write ( mtout, rec=irecw ) real(tzmiy(1:jmgeo,1:km),4)

  close ( mtout )

  !---------------------------------------------------
  if (l_depth_average) then

    irecw2 = irecw2 + 1
    write ( mtout2, rec=irecw2 ) real(tzmgy_dav(1:jmgeo),4)

    irecw2 = irecw2 + 1
    write ( mtout2, rec=irecw2 ) real(tzmay_dav(1:jmgeo),4)

    irecw2 = irecw2 + 1
    write ( mtout2, rec=irecw2 ) real(tzmpy_dav(1:jmgeo),4)

    irecw2 = irecw2 + 1
    write ( mtout2, rec=irecw2 ) real(tzmiy_dav(1:jmgeo),4)

    close ( mtout2 )

  end if
  !---------------------------------------------------
  !
  !
!====================================================
end program zonal_mean_meridional_gradient
