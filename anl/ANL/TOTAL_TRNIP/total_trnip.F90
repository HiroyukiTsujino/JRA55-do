!-*-F90-*-
!total_moc.F90
!====================================================
!
! Make Meridional Overturning Streamfunction
!
!             original version written by M.Hirabara
!  modified for variable grid  @040720 by S.Yukimoto
!  make using the model's modules @060927 by S.Yukimoto
!  modified for readability  @20070416 by M.Hirabara
!====================================================
program total_transport_on_density_coords

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

  use oc_structure, only  : &
  &   read_topo,            & !--------
  &   dep,                  & ! UVTSボックス境界（上面）の深さ
  &   ho4, exnn ,           & ! 水深、層数
  &   aexl, atexl,          & ! 海陸インデックス
  &   coefx, coefy,         &
  &   dzu,                  & ! UVボックスの厚さ
  &   thcksgm, thcksgmr,    & ! sigma 層の厚さ、逆数
  &   read_scale,           & !--------
  &   a_tl  , a_tr  ,       & ! 格子面積
  &   a_bl  , a_br  ,       &
  &   dx_tl , dx_tr ,       & ! 東西長
  &   dx_bl , dx_br ,       &
  &   dy_tl , dy_tr ,       & ! 南北長
  &   dy_bl , dy_br ,       &
  &   set_hgrids,           & !--------
  &   dxtdeg, dytdeg,       & ! T点を中心とするモデル格子間隔(度)
  &   dxudeg, dyudeg,       & ! U点を中心とするモデル格子間隔(度)
  &   slat, slon,           & ! 
  &   alatt, alatu,         & ! モデル座標上緯度(psi)経度(mu)
  &   alont, alonu

#ifdef OGCM_BBL
  use oc_structure, only  : &
  &   ho4bbl, exnnbbl,      & ! BBL層厚、層数
  &   aexlbbl, atexlbbl       ! BBLインデックス
#endif /* OGCM_BBL */

  !----------------------------------------------

  implicit none

  ! 地理座標パラメタ

  real(8)    :: dlatg = 0.5d0           ! product latitude interval ... namelist
  real(8)    :: slatg = real(slat0, 8)  ! product start latitude    ... namelist
  integer(4) :: jmgeo
  integer(4) :: rlmax

  real(8), parameter :: lat_out = -30.0d0

  !-----

  real(4),allocatable :: trn_g(:,:)  ! Global
  real(4),allocatable :: trn_a(:,:)  ! Atlantic
  real(4),allocatable :: trn_p(:,:)  ! Pacific
  real(4),allocatable :: trn_i(:,:)  ! Pacific

  real(4),allocatable :: trn_gm_g(:,:)  ! Global
  real(4),allocatable :: trn_gm_a(:,:)  ! Atlantic
  real(4),allocatable :: trn_gm_p(:,:)  ! Pacific
  real(4),allocatable :: trn_gm_i(:,:)  ! Pacific

  real(4),allocatable :: trn_out(:,:)  ! Total

  real(8) :: phi_std_degree_s, phi_std_degree_n 
  real(4) :: weight, tmpval
  real(4),allocatable :: trn_tmp(:)

  real(4) :: undef4_in = -9.99e33
  real(4) :: undef4_out = -9.99e33
  real(8) :: undef8_in = -9.99d33
  real(8) :: undef8_out = -9.99d33

  ! 入出力ファイル

  character(len=256)    :: flin_eu     ! 入力ファイル
  character(len=256)    :: flin_gm     ! 入力ファイル
  character(len=256)    :: flout       ! 出力ファイル
  character(len=256)    :: flout_prof  ! 出力ファイル
  character(len=256)    :: fltopo      ! 海底地形ファイル
  character(len=256)    :: flsclf      ! スケールファクター・ファイル
#ifdef OGCM_VARIABLE
  character(len=256)    :: file_vgrid ! 可変格子定義ファイル
#endif /* OGCM_VARIABLE */
  logical :: l_inc_gm
  !
  namelist /nmlttrnip/ flin_eu, flin_gm, l_inc_gm,  &
       &            flout, flout_prof, &
       &            undef4_in, undef4_out, &
       &            fltopo, flsclf,    &
       &            jmgeo, slatg, dlatg, rlmax
#ifdef OGCM_VARIABLE
  namelist /inflg/ file_vgrid
#endif /* OGCM_VARIABLE */
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mtin_eu    = 81
  integer(4), parameter :: mtin_gm    = 82
  integer(4), parameter :: mtout      = 86
  integer(4), parameter :: mtout_prof = 87
  !
  integer(4), save :: lrec_in, lrec_out
  !
  integer(4) :: irecw, irecw2
  integer(4) :: i, j, k, m, jj
  integer(4) :: nkai
  integer(4) :: jg
  !
  !==============================================
  !
  ! 入力パラメタ規定値

  flin_eu     = 'trnip.d'
  flin_gm     = 'gmtrnip.d'
  flout       = 'trn_sum.d'
  flout_prof  = 'trn_sum_prof.d'
  fltopo      = 'topo.d'
  flsclf      = 'scale_factor.d'

  ! 標準入力から読み込み

  read(unit=5, nmlttrnip)
  print *,'flin_eu    :', trim(flin_eu)
  print *,'flin_gm    :', trim(flin_gm)
  print *,'flout      :', trim(flout)
  print *,'flout_prof :', trim(flout_prof)
  print *,'fltopo     :', trim(fltopo)
  print *,'flsclf     :', trim(flsclf)
  print *,'l_inc_gm   :', l_inc_gm
#ifdef OGCM_VARIABLE
  read(unit=5, inflg) ! file_vgrid
#endif /* OGCM_VARIABLE */

  undef8_in  = real(undef4_in,8)
  undef8_out = real(undef4_out,8)

  !----------------------------------------------
  ! 海洋モデル格子情報等の準備
  !
  call read_topo(fltopo)
  !
  write(*,"(a)")                'XDEF      1  LINEAR    0.0000   1.0'
  write(*,"(a,i5,a,f8.4,f8.4)") 'YDEF   ', jmgeo, '  LINEAR  ', slatg, dlatg
!  write(*,"(a,i5,a)")           'ZDEF   ', km+1,  '  LEVELS'
!  write(*,"(f8.2)") (dep(km)+dz(km)) * 1.d-2     !  cm => m
!  write(*,"(10f8.2)") (dep(k) * 1.d-2,k=km,1,-1)     !  cm => m
  !
  !----------------------------------------------

  allocate(trn_g   (1:jmgeo,1:rlmax))
  allocate(trn_gm_g(1:jmgeo,1:rlmax))
  allocate(trn_a   (1:jmgeo,1:rlmax))
  allocate(trn_gm_a(1:jmgeo,1:rlmax))
  allocate(trn_p   (1:jmgeo,1:rlmax))
  allocate(trn_gm_p(1:jmgeo,1:rlmax))
  allocate(trn_i   (1:jmgeo,1:rlmax))
  allocate(trn_gm_i(1:jmgeo,1:rlmax))

  allocate(trn_out (1:jmgeo,1:rlmax))
  allocate(trn_tmp (1:rlmax))

  !----------------------------------------------
  ! モデル水平格子情報定義
  !
#ifdef OGCM_VARIABLE
  call set_hgrids(file_vgrid)
#else /* OGCM_VARIABLE */
  call set_hgrids
#endif /* OGCM_VARIABLE */
  !
  !----------------------------------------------
  lrec_in  = jmgeo*rlmax*4
  lrec_out = jmgeo*rlmax*4
  !----------------------------------------------
  !
  !  スケールファクタの読み込み
  !
  call read_scale(flsclf)

  !==============================================
  !
  ! 入出力ファイルオープン
  !
  open (mtin_eu, file=flin_eu, form='unformatted', &
    &  access='direct', recl=lrec_in)

  if (l_inc_gm) then
    open (mtin_gm, file=flin_gm, form='unformatted', &
         &  access='direct', recl=lrec_in)
  end if

  open (mtout, file=flout, form='unformatted',&
    &  access='direct', recl=lrec_out)
  irecw=1

  open (mtout_prof, file=flout_prof, form='unformatted',&
    &  access='direct', recl=4*rlmax)
  irecw2=1

  !-------------------------

  ! Resolved

  read (mtin_eu, rec=1) trn_g
  read (mtin_eu, rec=2) trn_a
  read (mtin_eu, rec=3) trn_p
  read (mtin_eu, rec=4) trn_i

  close ( mtin_eu )

  !-------------------------

  if (l_inc_gm) then
    read (mtin_gm, rec=1) trn_gm_g
    read (mtin_gm, rec=2) trn_gm_a
    read (mtin_gm, rec=3) trn_gm_p
    read (mtin_gm, rec=4) trn_gm_i
  else
    trn_gm_g(:,:) = 0.0e0
    trn_gm_a(:,:) = 0.0e0
    trn_gm_p(:,:) = 0.0e0
    trn_gm_i(:,:) = 0.0e0
  end if

  close ( mtin_gm )

  !--------------------------------------------------

  trn_out(1:jmgeo,1:rlmax) = trn_g(1:jmgeo, 1:rlmax) + trn_gm_g(1:jmgeo, 1:rlmax)
  write ( mtout, rec=irecw ) trn_out
  irecw=irecw+1

  trn_out(1:jmgeo,1:rlmax) = trn_a(1:jmgeo, 1:rlmax) + trn_gm_a(1:jmgeo, 1:rlmax)
  write ( mtout, rec=irecw ) trn_out
  irecw=irecw+1

  trn_out(1:jmgeo,1:rlmax) = trn_p(1:jmgeo, 1:rlmax) + trn_gm_p(1:jmgeo, 1:rlmax)
  write ( mtout, rec=irecw ) trn_out
  irecw=irecw+1

  trn_out(1:jmgeo,1:rlmax) = trn_i(1:jmgeo, 1:rlmax) + trn_gm_i(1:jmgeo, 1:rlmax)
  write ( mtout, rec=irecw ) trn_out
  irecw=irecw+1

  !---------------------------------------------------
  ! Meridional Overturning
  !    (maximum or minimum at the latitude in the basin)
  !---------------------------------------------------

  do jg = 1, jmgeo - 1
    phi_std_degree_s = slatg + dlatg * (jg-1)
    phi_std_degree_n = slatg + dlatg * jg
    if ((phi_std_degree_s <= lat_out) .and. (lat_out < phi_std_degree_n)) then
      write(6,*) jg, phi_std_degree_s, phi_std_degree_n
      weight = (lat_out - phi_std_degree_s) / dlatg
      trn_tmp(1:rlmax) = (1.0d0 - weight) * (trn_gm_g(jg, 1:rlmax) + trn_g(jg, 1:rlmax)) &
           & + weight * (trn_gm_g(jg+1, 1:rlmax) + trn_g(jg+1, 1:rlmax))
      write(mtout_prof,rec=irecw2) trn_tmp
      irecw2 = irecw2 + 1
      do k = 1, rlmax
        write(6,*) k,trn_tmp(k)
      end do
      exit
    end if
  end do

  deallocate(trn_out,trn_tmp)
  deallocate(trn_i, trn_gm_i)
  deallocate(trn_p, trn_gm_p)
  deallocate(trn_a, trn_gm_a)
  deallocate(trn_g, trn_gm_g)

  close ( mtout_prof )
  close ( mtout )

end program total_transport_on_density_coords
