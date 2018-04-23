!-*-F90-*-
!remap_uv.F90
!====================================================
program remap_vector
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
  &   rot_mp2lp,              &
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
  &   set_hgrids,           & !--------
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

  use mod_gaussgrid

  !----------------------------------------------
  !
  implicit none
  !
  ! divided grid

  integer(4) :: idiv, jdiv    ! divide U-cells, this should be even
  integer(4) :: nxdiv, nydiv  ! model core region divivded into ...
  real(8),    allocatable :: alon_div(:), alat_div(:)
  integer(4), allocatable :: i_div_t(:), j_div_t(:)
  integer(4), allocatable :: i_div_u(:), j_div_u(:)
  integer(4) :: ii, jj
  integer(4) :: iidv, jjdv
  real(8) :: lon_trn, lat_trn
  real(8) :: area_tmp, basin_tmp
  real(8) :: u_tmp, v_tmp
  real(8) :: tran_lat

  ! interpolated grid

  character(len=16) :: sph_grid
  integer(4), save :: lrec_out
  integer(4) :: j_begin_search
  real(8), allocatable :: area_received(:,:)
  real(8), allocatable :: u_received(:,:)
  real(8), allocatable :: v_received(:,:)
  real(8), allocatable :: basin_index(:,:)
  real(8), parameter :: eps = 1.0d-5
  real(4), allocatable :: dat_out(:,:)
  real(8) :: rot_cos, rot_sin
  integer(4) :: j_start

  ! original grid

  integer(4), parameter :: ibu = 3, ieu = imut - 2  ! cyclic
  integer(4), parameter :: jbu = 2, jeu = jmut - 3  ! tripolar

  integer(4), parameter :: ibt = 3, iet = imut - 2  ! cyclic
  integer(4), parameter :: jbt = 2, jet = jmut - 2  ! tripolar

  !
  !             0:LAND, 1:ATL, 2:PAC, 3:IND, 4:MED, 9:SO
  integer(4) :: ibas(imut, jmut)      ! basin区分, UV-Grid
  !
  real(8)    :: mu_u, psi_u, lambdau, phiu
  !
  real(8)          :: u(imut, jmut)
  real(8)          :: v(imut, jmut)
  real(8)          :: undef
  integer(4)       :: m_recu, m_recv
  integer(4)       :: k_mask
  !
  real(4)    :: d3_r4(imut,jmut,km)
  real(4)    :: d2_r4(imut,jmut)
  !
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flinu    ! 入力ファイル
  character(len=256)    :: flinv    ! 入力ファイル
  character(len=256)    :: flout    ! 出力ファイル
  character(len=256)    :: fltopo   ! 海底地形ファイル
  character(len=256)    :: flsclf   ! スケールファクター・ファイル
  character(len=256)    :: flibas   ! basinインデックスファイル
  character(len=256)    :: flstdout ! 診断量の出力ファイル
  !
#ifdef OGCM_VARIABLE
  character(len=256)    :: file_vgrid ! 可変格子定義ファイル
#endif /* OGCM_VARIABLE */
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mttmp    = 79
  integer(4), parameter :: mtinu    = 84
  integer(4), parameter :: mtinv    = 85
  integer(4), parameter :: mtout    = 86
  integer(4), parameter :: mtstdout = 87
  !
  integer(4) :: irecw
  integer(4) :: i, j, k, m
  integer(4) :: nkai
  integer(4) :: jg
  character(80) :: fmt_ibas
#ifdef OGCM_BBL
  integer(4) :: kbtm
#endif /* OGCM_BBL */
  !
#ifdef OGCM_CYCLIC
  logical, parameter :: lcyclic = .true.
#else /* OGCM_CYCLIC */
  logical, parameter :: lcyclic = .false.
#endif /* OGCM_CYCLIC */

  logical :: l_out_area
  logical :: l_out_mask

  namelist /nml_rmp_uv/  idiv, jdiv, &
       &  sph_grid,            &
       &  tran_lat,            &
       &  m_recu,              &
       &  m_recv,              &
       &  undef,               &
       &  k_mask,              &
       &  l_out_area,          &
       &  l_out_mask,          &
       &  flinu, flinv,        &
       &  flout,               &
       &  fltopo, flsclf, flibas, flstdout
#ifdef OGCM_VARIABLE
  namelist /inflg/ file_vgrid
#endif /* OGCM_VARIABLE */
  !==============================================

  write(*,*) 'slat0', slat0
  write(*,*) 'slon0', slon0

  !---------------------------------------------
  !
  ! 入力パラメタ規定値
  !
  flinu  = 'hs_u.d'
  flinv  = 'hs_v.d'
  flout  = 'hs_uv_sph.d'
  fltopo = 'topo.d'
  flsclf = 'scale_factor.d'
  flibas = 'basin_map.txt'
  flstdout = 'remap_uv.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml_rmp_uv)
  print *,'flinu    :', trim(flinu)
  print *,'flinv    :', trim(flinv)
  print *,'flout    :', trim(flout)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  print *,'flibas   :', trim(flibas)
  print *,'flstdout :', trim(flstdout)
  print *,'mrecu    :', m_recu
  print *,'mrecv    :', m_recv

#ifdef OGCM_VARIABLE
  read(unit=5, inflg) ! file_vgrid
#endif /* OGCM_VARIABLE */
  !
  open (mtstdout, file=flstdout, form='formatted')
  !
  !----------------------------------------------
  ! 海洋モデル格子情報等の準備
  !
  call read_topo(fltopo)
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
    read(mttmp, fmt=fmt_ibas,iostat=ios) jj,(ibas(i,j), i=1, imut)
    if(ios /= 0) write(*, *) 'reading error in file:', flibas
    if ( jj /= j ) then
      print *,' error in ',trim(flibas)
      stop 999
    endif
  end do
  close(mttmp)
  !
  !  Pacific side half of the Arctic Sea is excluded from ATL
  !
  !do j = 1, jmut
  !  do i = 1, imut
  !    mu_u = alonu(i)*radian_r
  !    psi_u = alatu(j)*radian_r
  !    call mp2lp(lambdau, phiu, mu_u, psi_u)
  !    if (phiu > 60.d0*radian_r .and. cos(lambdau) < 0.d0) then
  !      if (ibas(i,j) == 1) then
  !        ibas(i,j) = 5 ! Arctic, tentative
  !      end if
  !    end if
  !  end do
  !end do
  !
  !----------------------------------------------
  !  Read data

  open (mtinu, file=flinu, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)

  read (mtinu, rec=m_recu) d2_r4
  u(:,:) = aexl(:,:,k_mask)*real(d2_r4(:,:),8)

  if (lcyclic) then
    u(1:2,         1:jmut)=u(imut-3:imut-2, 1:jmut)
    u(imut-1:imut, 1:jmut)=u(3:4,           1:jmut)
  end if

  close ( mtinu )

  !-----

  open (mtinv, file=flinv, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)

  read (mtinv, rec=m_recv) d2_r4
  v(:,:) = aexl(:,:,k_mask)*real(d2_r4(:,:),8)

  if (lcyclic) then
    v(1:2,         1:jmut)=v(imut-3:imut-2, 1:jmut)
    v(imut-1:imut, 1:jmut)=v(3:4,           1:jmut)
  end if

  close ( mtinv )

  !==============================================

  call set_gaussgrid (sph_grid)

  write(mtstdout,*) 'imaxg = ', imaxg
  write(mtstdout,*) 'jmaxg = ', jmaxg

  allocate(u_received(1:imaxg,1:jmaxg))
  allocate(v_received(1:imaxg,1:jmaxg))
  allocate(area_received(1:imaxg,1:jmaxg))
  allocate(basin_index  (1:imaxg,1:jmaxg))
  allocate(dat_out(1:imaxg,1:jmaxg))
  lrec_out = imaxg*jmaxg*4

  u_received(1:imaxg,1:jmaxg) = 0.0d0
  v_received(1:imaxg,1:jmaxg) = 0.0d0
  area_received(1:imaxg,1:jmaxg) = 0.0d0
  basin_index  (1:imaxg,1:jmaxg) = 0.0d0

  !----------------------------------------------
  ! create divided grid

  nxdiv = (ieu - ibu + 1) * idiv
  nydiv = (jeu - jbu + 1) * jdiv
  allocate(alon_div(1:nxdiv), alat_div(1:nydiv))
  allocate(i_div_t(1:nxdiv), j_div_t(1:nydiv))
  allocate(i_div_u(1:nxdiv), j_div_u(1:nydiv))

  !-----

  ! U-points

  iidv = 1
  alon_div(iidv) = alont(ibt) + 0.5d0 * dxtdeg(ibt) / real(idiv,8)
  i_div_t(iidv) = ibt

  do ii = 2, idiv/2
    iidv = iidv + 1
    alon_div(iidv) = alon_div(iidv-1) + dxtdeg(ibt) / real(idiv,8)
    i_div_t(iidv) = ibt
  end do

  do i = ibt + 1, iet
    iidv = iidv + 1
    alon_div(iidv) = alonu(i-1) + 0.5d0 * dxtdeg(i) / real(idiv,8)
    i_div_t(iidv) = i
    do ii = 2, idiv
      iidv = iidv + 1
      alon_div(iidv) = alon_div(iidv-1) + dxtdeg(i) / real(idiv,8)
      i_div_t(iidv) = i
    end do
  end do

  iidv = iidv + 1
  alon_div(iidv) = alonu(iet) + 0.5d0 * dxtdeg(iet+1) / real(idiv,8)
  i_div_t(iidv) = iet + 1

  do ii = 2, idiv/2
    iidv = iidv + 1
    alon_div(iidv) = alon_div(iidv-1) + dxtdeg(iet+1) / real(idiv,8)
    i_div_t(iidv) = iet + 1
  end do

  iidv = 0
  do i = ibu, ieu
    do ii = 1, idiv
      iidv = iidv + 1
      i_div_u(iidv) = i
    end do
  end do

  do ii = 1, iidv
    write(mtstdout,*) ii, alon_div(ii), i_div_t(ii), i_div_u(ii)
  end do
  write(mtstdout,*) 'iidv = ', iidv, nxdiv

  !-----

  jjdv = 1

  alat_div(jjdv) = alatt(jbt) + 0.5d0 * dytdeg(jbt) / real(jdiv,8)
  j_div_t(jjdv) = jbt

  do jj = 2, jdiv/2
    jjdv = jjdv + 1
    alat_div(jjdv) = alat_div(jjdv-1) + dytdeg(jbt) / real(jdiv,8)
    j_div_t(jjdv) = jbt
  end do

  do j = jbt + 1, jet - 1
    jjdv = jjdv + 1
    alat_div(jjdv) = alatu(j-1) + 0.5d0 * dytdeg(j) / real(jdiv,8)
    j_div_t(jjdv) = j
    do jj = 2, jdiv
      jjdv = jjdv + 1
      alat_div(jjdv) = alat_div(jjdv-1) + dytdeg(j) / real(jdiv,8)
      j_div_t(jjdv) = j
    end do
  end do

  jjdv = jjdv + 1
  alat_div(jjdv) = alatu(jet-1) + 0.5d0 * dytdeg(jet) / real(jdiv,8)
  j_div_t(jjdv) = jet

  do jj = 2, jdiv/2
    jjdv = jjdv + 1
    alat_div(jjdv) = alat_div(jjdv-1) + dytdeg(jet) / real(jdiv,8)
    j_div_t(jjdv) = jet
  end do

  jjdv = 0
  do j = jbu, jeu
    do jj = 1, jdiv
      jjdv = jjdv + 1
      j_div_u(jjdv) = j
    end do
  end do

  do jj = 1, jjdv
    write(mtstdout,*) jj, alat_div(jj), j_div_t(jj), j_div_u(jj)
  end do
  write(mtstdout,*) 'jjdv = ', jjdv, nydiv

!  do jjdv = 1, nydiv
!    do iidv = 1, nxdiv
!      mu_u = alon_div(iidv)*radian_r
!      psi_u = alat_div(jjdv)*radian_r
!      call mp2lp(lambdau, phiu, mu_u, psi_u)
!      if (alat_div(jjdv) > nplat) then
!        write(6,*) iidv, jjdv, lambdau * radian, phiu * radian
!      end if
!    end do
!  end do

  j_start = 1
  do jjdv = 1, nydiv
!    if (mod(jjdv,100) == 0) write(6,*) jjdv, j_start
    do iidv = 1, nxdiv
      mu_u = alon_div(iidv)*radian_r
      psi_u = alat_div(jjdv)*radian_r
      call mp2lp(lambdau, phiu, mu_u, psi_u)
      call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
      lon_trn = lambdau * radian
      if (lon_trn <= long_boundary(0)) then
        lon_trn = lon_trn + 360.d0
      end if
      if (lon_trn > long_boundary(imaxg)) then
        lon_trn = lon_trn - 360.d0
      end if
      lat_trn = phiu * radian
      if (lat_trn > latg_boundary(jmaxg)) then
        write(6,*) 'erroneous transformation'
        write(6,*) lat_trn, alon_div(iidv), alat_div(jjdv)
        stop
      end if

      if (exnn(i_div_u(iidv),j_div_u(jjdv)) >= k_mask) then ! U-point datum is valid
        basin_tmp = real(ibas(i_div_u(iidv),j_div_u(jjdv)))
        if (j_div_t(jjdv) == j_div_u(jjdv)) then   ! lower half
          if (i_div_t(iidv) == i_div_u(iidv)) then ! left half
            area_tmp = a_bl(i_div_u(iidv),j_div_u(jjdv)) / real(idiv,8) / real(jdiv,8) * 4.0d0
          else if (i_div_t(iidv) == i_div_u(iidv)+1) then  ! right half
            area_tmp = a_br(i_div_u(iidv),j_div_u(jjdv)) / real(idiv,8) / real(jdiv,8) * 4.0d0
          else
            write(6,*) 'program error'
          end if
        else                                       ! upper half
          if (i_div_t(iidv) == i_div_u(iidv)) then ! left half
            area_tmp = a_tl(i_div_u(iidv),j_div_u(jjdv)) / real(idiv,8) / real(jdiv,8) * 4.0d0
          else if (i_div_t(iidv) == i_div_u(iidv)+1) then  ! right half
            area_tmp = a_tr(i_div_u(iidv),j_div_u(jjdv)) / real(idiv,8) / real(jdiv,8) * 4.0d0
          else
            write(6,*) 'program error'
          end if
        end if

        u_tmp =   rot_cos * u(i_div_u(iidv),j_div_u(jjdv)) + rot_sin * v(i_div_u(iidv),j_div_u(jjdv))
        v_tmp = - rot_sin * u(i_div_u(iidv),j_div_u(jjdv)) + rot_cos * v(i_div_u(iidv),j_div_u(jjdv))

        do j = j_start, jmaxg
          if ((latg_boundary(j-1) < lat_trn) .and. (lat_trn <= latg_boundary(j))) then
            do i = 1, imaxg + 1
              if ((long_boundary(i-1) < lon_trn) .and. (lon_trn <= long_boundary(i))) then
                area_received(i,j) = area_received(i,j) + area_tmp
                u_received(i,j) = u_received(i,j) + area_tmp * u_tmp
                v_received(i,j) = v_received(i,j) + area_tmp * v_tmp
                if (basin_index(i,j) < basin_tmp) then
                  basin_index(i,j) = basin_tmp
                end if
                exit
              end if
            end do
            if (lat_trn < tran_lat) then
              j_start = j
            end if
            exit
          end if
        end do

      end if

    end do
  end do

  do j = 1, jmaxg
    do i = 1, imaxg
      if (area_received(i,j) > 0.0d0) then
        u_received(i,j) = u_received(i,j) / area_received(i,j)
        v_received(i,j) = v_received(i,j) / area_received(i,j)
      else
        u_received(i,j) = undef
        v_received(i,j) = undef
      end if
    end do
  end do
  
  !----------------------------------------------
  !
  ! 入出力ファイルオープン

  open (mtout, file=flout, form='unformatted',&
    &  access='direct', recl=lrec_out )
  irecw = 1

  !-------------------------

  dat_out(1:imaxg,1:jmaxg) = real(u_received(1:imaxg,1:jmaxg),4)
  write(mtout, rec=1) dat_out
  dat_out(1:imaxg,1:jmaxg) = real(v_received(1:imaxg,1:jmaxg),4)
  write(mtout, rec=2) dat_out

  if (l_out_area) then
    dat_out(1:imaxg,1:jmaxg) = real(area_received(1:imaxg,1:jmaxg),4)
    write(mtout, rec=3) dat_out
  end if

  if (l_out_mask) then
    dat_out(1:imaxg,1:jmaxg) = real(basin_index(1:imaxg,1:jmaxg),4)
    write(mtout, rec=4) dat_out
  end if

  close ( mtout )

end program remap_vector
