!-*-F90-*-
!remap_vector.F90
!====================================================
! %%%%%  From model grid to lat-lon grid %%%%%
!
program remap_vector

  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ksgm, dz,            &
  &   pi, radian, radian_r, &
  &   slat0, slon0,        &
  &   nplat, nplon,        &
  &   splat, splon
#ifndef OGCM_VARIABLE
  use oc_mod_param, only : &
  &   dxtdgc, dytdgc
#endif /* ! OGCM_VARIABLE */

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

#ifdef OGCM_BBL
  use oc_structure, only  : &
  &   ho4bbl, exnnbbl,      & ! BBL層厚、層数
  &   aexlbbl, atexlbbl       ! BBLインデックス
  use oc_structure, only  : &
  &   kbtm
#endif /* OGCM_BBL */

  use oc_mod_trnsfrm, only  : &
  &   set_abc, mp2lp, lp2mp, rot_mp2lp

  use mod_gaussgrid

  !----------------------------------------------

  implicit none

  ! divided grid

  integer(4) :: idiv, jdiv    ! divide T-cells, this should be even
  integer(4) :: nxdiv, nydiv  ! model core region divivded into ...
  real(8),    allocatable :: alon_div(:), alat_div(:)
  integer(4), allocatable :: i_div_t(:), j_div_t(:)
  integer(4), allocatable :: i_div_u(:), j_div_u(:)

  real(8) :: area_tmp

  ! interpolated grid

  character(len=16) :: sph_grid
  integer(4), save :: lrec_out
  integer(4) :: j_begin_search
  real(8), allocatable :: area_received(:,:,:)
  real(8), allocatable :: rotation_cos(:,:)
  real(8), allocatable :: rotation_sin(:,:)
  real(8), allocatable :: u_received(:,:,:)
  real(8), allocatable :: v_received(:,:,:)
  real(8), parameter :: eps = 1.0d-5
  real(4), allocatable :: dat_out(:,:)
  real(8) :: undef_out
  integer(4) :: idst_3d, idst_2d, i_dst, j_dst, k_dst

  ! original grid

  integer(4), parameter :: ibu = 3, ieu = imut - 2  ! cyclic
  integer(4), parameter :: jbu = 2, jeu = jmut - 3  ! tripolar

  integer(4), parameter :: ibt = 3, iet = imut - 2  ! cyclic
  integer(4), parameter :: jbt = 2, jet = jmut - 2  ! tripolar

  real(8), allocatable :: u(:,:,:), v(:,:,:)
  real(8)          :: undef_in
  integer(4)       :: itemu_start   ! start record of the 1st layer of X-vector 
  integer(4)       :: itemv_start   ! start record of the 1st layer of Y-vector 
  integer(4)       :: k_start, k_end
  integer(4) :: isrc_3d, isrc_2d, i_src, j_src, k_src

  real(4)    :: d2_r4(imut,jmut)

  ! 入出力ファイル

  character(len=256)    :: flinu, flinv     ! 入力ファイル
  character(len=256)    :: floutu, floutv   ! 出力ファイル
  character(len=256)    :: fltopo   ! 海底地形ファイル
  character(len=256)    :: flsclf   ! スケールファクター・ファイル
  character(len=256)    :: flstdout ! 診断量の出力ファイル

#ifdef OGCM_VARIABLE
  character(len=256)    :: file_vgrid ! 可変格子定義ファイル
#endif /* OGCM_VARIABLE */

  logical :: l_read_table = .false.
  logical :: l_area_weight = .false.
  logical :: l_bicubic_trn = .true.
  logical :: l_check_table = .true.
  character(len=256) :: file_rmp_table
  character(len=256) :: file_recv_area
  integer(4) :: nlink
  integer(4) :: isrc_max
  integer(4) :: idst_max
  integer(4),allocatable :: isrc(:), idst(:)
  real(8),allocatable :: wgt(:,:)
  integer(4) :: num_wgts_all

  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mttbl    = 78
  integer(4), parameter :: mttmp    = 79
  integer(4), parameter :: mtin     = 84
  integer(4), parameter :: mtout    = 86
  integer(4), parameter :: mtstdout = 87
  !
  integer(4) :: irecw, ireci
  integer(4) :: i, j, k, m, n
  !
#ifdef OGCM_CYCLIC
  logical, parameter :: lcyclic = .true.
#else /* OGCM_CYCLIC */
  logical, parameter :: lcyclic = .false.
#endif /* OGCM_CYCLIC */

  namelist /nml_rmp_vec/       &
       &  idiv, jdiv,          &
       &  sph_grid,            &
       &  undef_in, undef_out, &
       &  itemu_start,         &
       &  itemv_start,         &
       &  k_start, k_end,      &
       &  l_read_table,        &
       &  l_area_weight,       &
       &  l_bicubic_trn,       &
       &  l_check_table,       &
       &  file_rmp_table,      &
       &  file_recv_area,      &
       &  flinu, flinv,        &
       &  floutu, floutv,      &
       &  fltopo, flsclf,      &
       &  flstdout
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
  flinu   = 'hs_u.d'
  flinv   = 'hs_v.d'
  floutu  = 'hs_u_sph.d'
  floutv  = 'hs_v_sph.d'
  fltopo  = 'topo.d'
  flsclf  = 'scale_factor.d'
  flstdout = 'remap_vector.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml_rmp_vec)

  write(6,*) 'sph_grid :', trim(sph_grid)

  write(6,*) 'fltopo   :', trim(fltopo)
  write(6,*) 'flsclf   :', trim(flsclf)

  write(6,*) 'l_read_table   :',l_read_table
  write(6,*) 'l_area_weight  :',l_area_weight
  write(6,*) 'l_bicubic_trn  :',l_bicubic_trn
  write(6,*) 'l_check_table  :',l_check_table
  write(6,*) 'file_rmp_table :',trim(file_rmp_table)
  write(6,*) 'file_recv_area :',trim(file_recv_area)

  if (.not. l_read_table) then
    write(6,*) 'idiv   :', idiv
    write(6,*) 'jdiv   :', jdiv
  end if

  write(6,*) 'flinu    :', trim(flinu)
  write(6,*) 'flinv    :', trim(flinv)
  write(6,*) 'undef_in :', undef_in
  write(6,*) 'itemu_start  :', itemu_start
  write(6,*) 'itemv_start  :', itemv_start
  write(6,*) 'k_start  :', k_start
  write(6,*) 'k_end    :', k_end

  write(6,*) 'floutu   :', trim(floutu)
  write(6,*) 'floutv   :', trim(floutv)
  write(6,*) 'undef_out:', undef_out

  write(6,*) 'flstdout :', trim(flstdout)

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
#ifdef OGCM_BBL
  ! reset aexl
  aexl(1:imut,1:jmut,1:km) = 0.0d0
  do j = 1, jmut
    do i = 1, imut
      if (kbtm(i,j) >= ksgm) then
        do k = 1, kbtm(i,j)
          aexl(i,j,k) = 1.0d0
        end do
      end if
    end do
  end do
#endif /* OGCM_BBL */
  !
  ! 座標変換のパラメータを決める
  call set_abc ( nplat, nplon, splat, splon )
  !
  ! モデル水平格子情報定義
  !
#ifdef OGCM_VARIABLE
  call set_hgrids(file_vgrid)
#else /* OGCM_VARIABLE */
  call set_hgrids
#endif /* OGCM_VARIABLE */

  !----------------------------------------------
  ! Read scaling factor

  call read_scale(flsclf)

  !---------------------------------------------
  ! Set target lat-lon grid lattice

  call set_gaussgrid (sph_grid)

  write(mtstdout,*) 'imaxg = ', imaxg
  write(mtstdout,*) 'jmaxg = ', jmaxg

  allocate(area_received(1:imaxg,1:jmaxg,k_start:k_end))
  allocate(rotation_cos(1:imaxg,1:jmaxg))
  allocate(rotation_sin(1:imaxg,1:jmaxg))
  allocate(u_received(1:imaxg,1:jmaxg,k_start:k_end))
  allocate(v_received(1:imaxg,1:jmaxg,k_start:k_end))
  allocate(dat_out(1:imaxg,1:jmaxg))
  lrec_out = imaxg*jmaxg*4

  !----------------------------------------------

  if (.not. l_read_table) then
    if (l_area_weight) then
      call create_remap_table_vec
    end if
    if (l_bicubic_trn) then
      call create_remap_table_vec_cubic
    end if
  end if

  write(6,*) ' Reading table ...'
  open(mttbl,file=file_rmp_table,form='unformatted',action='read')

  read(mttbl) isrc_max
  read(mttbl)
  read(mttbl)
  read(mttbl) idst_max
  read(mttbl)
  read(mttbl)

  write(6,*) '   isrc_max = ',isrc_max,' / ', imut * jmut * km
  write(6,*) '   idst_max = ',idst_max,' / ', imaxg * jmaxg * km

  read(mttbl) num_wgts_all, nlink

  write(6,'(1a,I8)') 'nlink = ', nlink

  allocate(isrc(1:nlink))
  allocate(idst(1:nlink))
  allocate(wgt(1:num_wgts_all,1:nlink))

  read(mttbl) isrc(1:nlink)
  read(mttbl) idst(1:nlink)
  read(mttbl) wgt(1:num_wgts_all,1:nlink)

  close(mttbl)

  !-------------------------------------------------
  ! Read Scalar data

  allocate(u(1:imut,1:jmut,k_start:k_end))

  open (mtin, file=flinu, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)

  write(6,*) 'reading original data (U) from', trim(flinu)

  do k = k_start, k_end
    ireci = itemu_start - 1 + k
    read (mtin, rec=ireci) d2_r4
    u(:,:,k) = real(d2_r4(:,:),8)
  end do

  close ( mtin )

  !---

  allocate(v(1:imut,1:jmut,k_start:k_end))

  open (mtin, file=flinv, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)

  write(6,*) 'reading original data (V) from', trim(flinv)

  do k = k_start, k_end
    ireci = itemv_start - 1 + k
    read (mtin, rec=ireci) d2_r4
    v(:,:,k) = real(d2_r4(:,:),8)
  end do

  close ( mtin )

#ifdef OGCM_BBL
  if (k_end == km) then
    do j = 1, jmut
      do i = 1, imut
        if (kbtm(i,j) >= ksgm) then
          if (aexlbbl(i,j,1) == 1.0d0) then
            u(i,j,kbtm(i,j)) = u(i,j,km)
            v(i,j,kbtm(i,j)) = v(i,j,km)
            u(i,j,km) = undef_in
            v(i,j,km) = undef_in
          end if
        end if
      end do
    end do
  end if
#endif /* OGCM_BBL */

  u(:,:,:) = u(:,:,:) * aexl(:,:,:)
  v(:,:,:) = v(:,:,:) * aexl(:,:,:)

  !----------------------------------------------

  u_received(1:imaxg,1:jmaxg,k_start:k_end) = 0.0d0
  v_received(1:imaxg,1:jmaxg,k_start:k_end) = 0.0d0
  area_received(1:imaxg,1:jmaxg,k_start:k_end) = 0.0d0
  rotation_cos(1:imaxg,1:jmaxg) = 0.0d0
  rotation_sin(1:imaxg,1:jmaxg) = 0.0d0

  do n = 1, nlink

    idst_3d = idst(n)
    k_dst = int((idst_3d - 1) / (imaxg * jmaxg)) + 1
    idst_2d = idst_3d - (k_dst - 1) * imaxg * jmaxg
    j_dst = jmaxg - int((idst_2d - 1) / imaxg)
    i_dst = idst_2d - (jmaxg - j_dst) * imaxg

#ifdef OGCM_CMIP5
    isrc_3d = isrc(n)
    k_src = int((isrc_3d - 1) / ((ieu - ibu + 1) * (jeu - jbu + 1))) + 1
    isrc_2d = isrc_3d - (k_src - 1) * (ieu - ibu + 1) * (jeu - jbu + 1)
    j_src = int((isrc_2d - 1) / (ieu - ibu + 1)) + jbu
    i_src = isrc_2d - (j_src - jbu) * (ieu - ibu + 1) + ibu - 1
#else /* OGCM_CMIP5 */
    isrc_3d = isrc(n)
    k_src = int((isrc_3d - 1) / (imut * jmut)) + 1
    isrc_2d = isrc_3d - (k_src - 1) * imut * jmut
    j_src = int((isrc_2d - 1) / imut) + 1
    i_src = isrc_2d - (j_src - 1) * imut
#endif /* OGCM_CMIP5 */
    if ((i_src < ibu)  .or. (ieu < i_src)) then
      write(6,'(1a,4i6)') ' i_src is out of core region : ', i_src, ibu, ieu, j_src
    end if
    if ((j_src < jbu)  .or. (jeu < j_src)) then
      write(6,'(1a,4i6)') ' j_src is out of core region : ', j_src, jbu, jeu, i_src
    end if

    if (u(i_src,j_src,k_src) /= undef_in) then

#ifdef OGCM_CMIP5ORG
      if (j_src == jeu) then
        if ((i_src <= ((imut - 4)/4 + 2)) .and. (wgt(3,n) < 0.0d0)) then
          wgt(3,n) = -wgt(3,n)
        end if
        if ((i_src >= (3*(imut - 4)/4 + 3)) .and. (wgt(3,n) > 0.0d0)) then
          wgt(3,n) = -wgt(3,n)
        end if
      end if
#endif /* OGCM_CMIP5ORG */

      u_received(i_dst,j_dst,k_dst) = u_received(i_dst,j_dst,k_dst) &
           & + (wgt(2,n) * u(i_src,j_src,k_src) - wgt(3,n) * v(i_src,j_src,k_src))
      v_received(i_dst,j_dst,k_dst) = v_received(i_dst,j_dst,k_dst) &
           & + (wgt(3,n) * u(i_src,j_src,k_src) + wgt(2,n) * v(i_src,j_src,k_src))
      area_received(i_dst,j_dst,k_dst) = area_received(i_dst,j_dst,k_dst) + wgt(1,n)
      
      if (k_dst==k_start) then
        rotation_cos(i_dst,j_dst) = rotation_cos(i_dst,j_dst) + wgt(2,n)
        rotation_sin(i_dst,j_dst) = rotation_sin(i_dst,j_dst) + wgt(3,n)
      end if

    else

      write(6,*) ' Expect valid data for ', i_src, j_src, k_src
      
    end if

  end do

  !-----------------------------------------------

  do k = k_start, k_end
    do j = 1, jmaxg
      do i = 1, imaxg
        if (area_received(i,j,k) == 0.0d0) then
          u_received(i,j,k) = undef_out
          v_received(i,j,k) = undef_out
        end if
      end do
    end do
  end do

  !----------------------------------------------
  ! 出力ファイルオープン

  open (mtout, file=floutu, form='unformatted', access='direct', recl=lrec_out)
  irecw = 0

  write(6,*) 'writing remapped data (U) to', trim(floutu)

  do k = k_start, k_end
    dat_out(1:imaxg,1:jmaxg) = real(u_received(1:imaxg,1:jmaxg,k),4)
    irecw = irecw + 1
    write(mtout, rec=irecw) dat_out
  end do

  close ( mtout )

  !---

  open (mtout, file=floutv, form='unformatted', access='direct', recl=lrec_out)
  irecw = 0

  write(6,*) 'writing remapped data (V) to', trim(floutv)

  do k = k_start, k_end
    dat_out(1:imaxg,1:jmaxg) = real(v_received(1:imaxg,1:jmaxg,k),4)
    irecw = irecw + 1
    write(mtout, rec=irecw) dat_out
  end do

  close ( mtout )

  !------------------------------------------------

  if (l_check_table) then

    open(mttmp,file=file_recv_area,form='unformatted',access='direct',action='write',recl=lrec_out)
    irecw = 0
    do k = k_start, k_end
      dat_out(1:imaxg,1:jmaxg) = real(area_received(1:imaxg,1:jmaxg,k),4)
      irecw = irecw + 1
      write(mttmp, rec=irecw) dat_out
    end do
    dat_out(1:imaxg,1:jmaxg) = real(rotation_cos(1:imaxg,1:jmaxg),4)
    irecw = irecw + 1
    write(mttmp, rec=irecw) dat_out
    dat_out(1:imaxg,1:jmaxg) = real(rotation_sin(1:imaxg,1:jmaxg),4)
    irecw = irecw + 1
    write(mttmp, rec=irecw) dat_out

    close(mttmp)

  end if

  close ( mtstdout )

contains
  !=====================================================================
  subroutine create_remap_table_vec

    implicit none

    integer(4) :: k
    real(8)    :: mu_u, psi_u, lambdau, phiu
    real(8)    :: lon_trn, lat_trn
    real(8)    :: rot_cos, rot_sin

    integer(4) :: iidv, jjdv
    integer(4) :: ii, jj
    integer(4) :: n

    integer(4) :: nlink, nlink_div, nlink_add
    integer(4) :: nxlink, nylink
    integer(4) :: isrc_div
    integer(4) :: id_3d, id_2d

    integer(4) :: k_levels

    !----------------------------------------------

    ! create divided grid on the basis of U-grid

    nxdiv = (ieu - ibu + 1) * idiv
    nydiv = (jeu - jbu + 1) * jdiv
    allocate(alon_div(1:nxdiv), alat_div(1:nydiv))
    allocate(i_div_t(1:nxdiv), j_div_t(1:nydiv))
    allocate(i_div_u(1:nxdiv), j_div_u(1:nydiv))

    !-------

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

    !---------------------------------------------------------------------

    k_levels = k_end - k_start + 1

    num_wgts_all = 3

!    allocate(isrc(nxdiv*nydiv*k_levels))
!    allocate(idst(nxdiv*nydiv*k_levels))
!    allocate(wgt(1:num_wgts_all,nxdiv*nydiv*k_levels))

    nxlink = (ieu - ibu + 1) * 8
    nylink = (jeu - jbu + 1) * 8

    allocate(isrc(nxlink*nylink*k_levels))
    allocate(idst(nxlink*nylink*k_levels))
    allocate(wgt(1:num_wgts_all,nxlink*nylink*k_levels))

    isrc(:) = 0
    idst(:) = 0
    wgt(:,:) = 0.d0

    nlink = 0
    nlink_add = 0

    area_received(1:imaxg,1:jmaxg,k_start:k_end) = 0.0d0

    do k = k_start, k_end

      nlink_div = nlink

      write(6,*) 'k = ',k, ' nlink_div = ', nlink_div

      do jjdv = 1, nydiv

        if (mod(jjdv,100) == 0) then
          write(6,*) jjdv ,'/', nydiv
          write(6,*) alat_div(jjdv)
        end if

        do iidv = 1, nxdiv

          mu_u = alon_div(iidv) * radian_r
          psi_u = alat_div(jjdv) * radian_r
          call mp2lp(lambdau, phiu, mu_u, psi_u)

          ! lat-lon(labmda, phi) => model (mu, psi)
          call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)

          lon_trn = lambdau * radian
          if (lon_trn <= long_boundary(1)) then
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

          if (aexl(i_div_u(iidv),j_div_u(jjdv),k) /= 0.0d0) then ! U-point datum is valid

            ! determine area

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

            ! create link, mapping table

#ifdef OGCM_CMIP5
            isrc_div = i_div_u(iidv) - ibu + 1 &
                 &  + (j_div_u(jjdv) - jbu) * (ieu - ibu + 1) &
                 &  + (k - 1) * (ieu - ibu + 1) * (jeu - jbu + 1)
#else /* OGCM_CMIP5 */
            isrc_div = i_div_u(iidv) + (j_div_u(jjdv) - 1) * imut + (k - 1) * imut * jmut
#endif /* OGCM_CMIP5 */

            do j = 1, jmaxg

              if ((latg_boundary(j-1) < lat_trn) .and. (lat_trn <= latg_boundary(j))) then

                do i = 1, imaxg

                  if ((long_boundary(i-1) < lon_trn) .and. (lon_trn <= long_boundary(i))) then

                    idst_3d = i + (jmaxg - j) * imaxg + (k - 1) * imaxg * jmaxg
                    area_received(i,j,k) = area_received(i,j,k) + area_tmp

                    ! nlink_div : number of link at the start of the divided data grid loop
                    ! nlink     : number of link at present
                    ! nlink_add : new link ( it not already exist )

                    nlink_add = nlink + 1   !- new link (default)

                    if (nlink > nlink_div) then
                      do n = nlink, nlink_div + 1, -1
                        if (idst(n) == idst_3d .and. isrc(n) == isrc_div) then
                          nlink_add = n
                          exit
                        end if
                      end do
                    end if

                    !-- make new link --
                    if ( nlink_add > nlink ) then
                      nlink = nlink + 1
                      !write(6,*) ' New link (src, dst) ', isrc_div, idst_3d
                      !write(6,*) '                     ', i_div_t(iidv), j_div_t(jjdv), k
                      isrc(nlink) = isrc_div
                      idst(nlink) = idst_3d
                      wgt(2,nlink) = rot_cos
                      wgt(3,nlink) = - rot_sin

                    end if

                    wgt(1,nlink_add) = wgt(1,nlink_add) + area_tmp

                    exit

                  end if

                end do

                exit

              end if

            end do

          end if

        end do
      end do

      do n = nlink_div + 1, nlink
        idst_3d = idst(n)
        idst_2d = idst_3d - (k - 1) * imaxg * jmaxg
        j_dst = jmaxg - int((idst_2d - 1) / imaxg)
        i_dst = idst_2d - (jmaxg - j_dst) * imaxg
        wgt(1,n) = wgt(1,n) / area_received(i_dst,j_dst,k)
        wgt(2,n) = wgt(1,n) * wgt(2,n)
        wgt(3,n) = wgt(1,n) * wgt(3,n)
      end do

    end do

    !------

    write(6,'(1a,I8)') 'nlink = ', nlink
    isrc_max = maxval(isrc(1:nlink))
    idst_max = maxval(idst(1:nlink))
    write(6,*) '   isrc_max = ',isrc_max,' / ', imut * jmut * km
    write(6,*) '   idst_max = ',idst_max,' / ', imaxg * jmaxg * km

    open(mttbl,file=file_rmp_table,form='unformatted',action='write')
    write(mttbl) isrc_max
    write(mttbl)
    write(mttbl)
    write(mttbl) idst_max
    write(mttbl)
    write(mttbl)
    write(mttbl) num_wgts_all, nlink
    write(mttbl) isrc(1:nlink)
    write(mttbl) idst(1:nlink)
    write(mttbl) wgt (1:num_wgts_all,1:nlink)
    close(mttbl)

    deallocate(isrc,idst,wgt)


    deallocate(alon_div, alat_div)
    deallocate(i_div_t, j_div_t)
    deallocate(i_div_u, j_div_u)

  end subroutine create_remap_table_vec
  !=====================================================================
  !=====================================================================
  subroutine create_remap_table_vec_cubic

    implicit none

    real(8)    :: mu_u, psi_u, lambdau, phiu
    real(8)    :: mu_a, psi_a
    real(8)    :: lon_trn, lat_trn
    real(8)    :: rot_cos, rot_sin

    real(8),allocatable :: a_fac(:,:), a_inv(:,:)
    real(8),allocatable :: work(:)
    integer(4),allocatable :: ipiv(:)
    integer(4) :: lda, lwork
    integer(4) :: info

    integer(4) :: ii, jj
    integer(4) :: iii, jjj

    integer(4) :: k
    integer(4) :: n, ndim

    integer(4) :: nlink, num_max_link

    integer(4) :: k_levels

    real(8) :: lambda0, phi0
    real(8) :: x_tmp, y_tmp

    real(8) :: weight
    real(8) :: weightx, weighty
    real(8) :: dx, dy

    real(8) :: tmp, tmp_sign

    integer(4) :: i_base, j_base

    integer(4) :: i_width, j_width

    logical :: l_bilinear, l_land, l_bicubic

    real(8),allocatable :: acexl(:,:)

    integer(4), allocatable :: i_org(:,:)
    integer(4), allocatable :: j_org(:,:)
    real(8), allocatable :: np_sign(:,:)

    !----------------------------------------------

#ifdef OGCM_CMIP5
    i_width = (ieu - ibu + 1)
    j_width = (jeu - jbu + 1)
#else /* OGCM_CMIP5 */
    i_width = imut
    j_width = jmut
#endif /* OGCM_CMIP5 */

    k_levels = k_end - k_start + 1

    num_wgts_all = 3
    num_max_link = 16*imaxg*jmaxg*k_levels 

    allocate(isrc(num_max_link))
    allocate(idst(num_max_link))
    allocate(wgt(1:num_wgts_all,num_max_link))

    isrc(:) = 0
    idst(:) = 0
    wgt(:,:) = 0.d0

    nlink = 0

    allocate(acexl(1:imut,1:jmut))
    acexl(:,:) = 0.0d0

    allocate(i_org(1:imut,1:jmut))
    allocate(j_org(1:imut,1:jmut))
    allocate(np_sign(1:imut,1:jmut))

    do i = 1, imut
      i_org(i,1:jmut) = i
    end do
    do i = 1, imut
      i_org(i,jeu+1:jmut) = imut - i + 1
    end do

    do j = 1, jmut
      j_org(1:imut,j) = j
    end do
    j_org(1:imut,jeu+1) = jeu
    j_org(1:imut,jeu+2) = jeu - 1
    j_org(1:imut,jeu+3) = jeu - 2

    np_sign(1:imut,1:jeu) = 1.0d0
! need check, but the following works well somehow...
!    np_sign(1:imut,jeu+1:jmut) = -1.0d0
    np_sign(1:imut,jeu+1:jmut) = 1.0d0

    do k = k_start, k_end

      do j = 2, jmut
        do i = 2, imut
          acexl(i,j) = aexl(i-1,j-1,k) * aexl(i,j-1,k) * aexl(i-1,j,k) * aexl(i,j,k)
        end do
      end do


      do jj = 1, jmaxg

        write(6,*) jj ,'/', jmaxg
        write(6,*) 'Geographical latitude', latg(jj)

        do ii = 1, imaxg

          lambdau = long(ii) * radian_r
          phiu = latg(jj) * radian_r

!          write(6,*) ii, jj, long(ii), latg(jj)

          ! lon-lat(labmda, phi) => model (mu, psi)
          call lp2mp(mu_a, psi_a, lambdau, phiu)

          lon_trn = mu_a * radian
          lat_trn = psi_a * radian

          if (lon_trn < alonu(ibt-1)) then
            lon_trn = lon_trn + 360.0d0
          end if

!          write(6,*) ii, jj, lon_trn, lat_trn
          if ((lon_trn < alonu(ibt-1)) .or. (lon_trn >= alonu(iet))) cycle
          if ((lat_trn < alatu(jbt-1)) .or. (lat_trn >= alatu(jet))) cycle

          idst_3d = ii + (jmaxg - jj) * imaxg + (k - 1) * imaxg * jmaxg

          do j = jbt, jet

            if ((alatu(j-1) <= lat_trn) .and. (lat_trn < alatu(j))) then

              do i = ibt, iet

                if ((alonu(i-1) <= lon_trn) .and. (lon_trn < alonu(i))) then

                  if (atexl(i,j,k) == 1.0d0) then ! At least one data is available.

                    i_base = i - 1
                    j_base = j - 1

                    l_bicubic = .false.
                    l_bilinear = .false.
                    l_land = .false.

                    if (acexl(i-1,j-1)*acexl(i+1,j-1)*acexl(i-1,j+1)*acexl(i+1,j+1) /= 0.0d0) then
                      l_bicubic = .true.
                    else if (acexl(i,j) /= 0.0d0) then
                      l_bilinear = .true.
                    else
                      l_land = .true.
                    end if

                    write(6,*) ii, jj, i_base, j_base, l_land, l_bilinear, l_bicubic
!                    write(6,*) acexl(i,j), atexl(i,j,k)
!                    write(6,*) aexl(i-1,j-1,k),aexl(i,j-1,k),aexl(i-1,j,k),aexl(i,j,k)

                    if (l_land) then

                      dx = alonu(i_base+1) - alonu(i_base)
                      dy = alatu(j_base+1) - alatu(j_base)
                      weightx = abs(lon_trn - alonu(i_base)) / dx
                      weighty = abs(lat_trn - alatu(j_base)) / dy

                      ! land is southwest

                      if ((1.0d0 - aexl(i-1,j-1,k)) * aexl(i,j-1,k) * aexl(i-1,j,k) * aexl(i,j,k) == 1.0d0) then

                        ! (1) southeast
                        i_src = i_org(i,j-1)
                        j_src = j_org(i,j-1)
                        tmp_sign = np_sign(i,j-1)
                        mu_u  = alonu(i_src) * radian_r
                        psi_u = alatu(j_src) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
#ifdef OGCM_CMIP5
                        isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                        isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */
                        weight = weightx * (1.0d0 - weighty) + (1.0d0 - weightx) * (1.0d0 - weighty) * dy / (dx + dy)

                        nlink = nlink + 1
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = weight
                        wgt(2,nlink) = weight * rot_cos * tmp_sign
                        wgt(3,nlink) = - weight * rot_sin * tmp_sign

                        ! (2) northwest
                        i_src = i_org(i-1,j)
                        j_src = j_org(i-1,j)
                        tmp_sign = np_sign(i-1,j)
                        mu_u  = alonu(i_src) * radian_r
                        psi_u = alatu(j_src) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
#ifdef OGCM_CMIP5
                        isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                        isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */
                        weight = (1.0d0 - weightx) * weighty + (1.0d0 - weightx) * (1.0d0 - weighty) * dx / (dx + dy)

                        nlink = nlink + 1
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = weight
                        wgt(2,nlink) = weight * rot_cos * tmp_sign
                        wgt(3,nlink) = - weight * rot_sin * tmp_sign

                        ! (3) northeast
                        i_src = i_org(i,j)
                        j_src = j_org(i,j)
                        tmp_sign = np_sign(i,j)
                        mu_u  = alonu(i_src) * radian_r
                        psi_u = alatu(j_src) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
#ifdef OGCM_CMIP5
                        isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                        isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */
                        weight = weightx * weighty

                        nlink = nlink + 1
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = weight
                        wgt(2,nlink) = weight * rot_cos * tmp_sign
                        wgt(3,nlink) = - weight * rot_sin * tmp_sign

                      end if

                      !-----------------

                      if (aexl(i-1,j-1,k) * (1.0d0 - aexl(i,j-1,k)) * aexl(i-1,j,k) * aexl(i,j,k) == 1.0d0) then

                        !---
                        i_src = i_org(i-1,j-1)
                        j_src = j_org(i-1,j-1)
                        tmp_sign = np_sign(i-1,j-1)
                        mu_u  = alonu(i_src) * radian_r
                        psi_u = alatu(j_src) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
#ifdef OGCM_CMIP5
                        isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                        isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */
                        weight = (1.0d0 - weightx) * (1.0d0 - weighty) + weightx * (1.0d0 - weighty) * dy / (dx + dy)

                        nlink = nlink + 1
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = weight
                        wgt(2,nlink) = weight * rot_cos * tmp_sign
                        wgt(3,nlink) = - weight * rot_sin * tmp_sign

                        !---
                        i_src = i_org(i-1,j)
                        j_src = j_org(i-1,j)
                        tmp_sign = np_sign(i-1,j)
                        mu_u  = alonu(i_src) * radian_r
                        psi_u = alatu(j_src) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
#ifdef OGCM_CMIP5
                        isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                        isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */
                        weight = (1.0d0 - weightx) * weighty

                        nlink = nlink + 1
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = weight
                        wgt(2,nlink) = weight * rot_cos * tmp_sign
                        wgt(3,nlink) = - weight * rot_sin * tmp_sign

                        !---
                        i_src = i_org(i,j)
                        j_src = j_org(i,j)
                        tmp_sign = np_sign(i,j)
                        mu_u  = alonu(i_src) * radian_r
                        psi_u = alatu(j_src) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
#ifdef OGCM_CMIP5
                        isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                        isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */
                        weight = weightx * weighty + weightx * (1.0d0 - weighty) * dx / (dx + dy)

                        nlink = nlink + 1
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = weight
                        wgt(2,nlink) = weight * rot_cos * tmp_sign
                        wgt(3,nlink) = - weight * rot_sin * tmp_sign

                      end if

                      !------------------------------

                      if (aexl(i-1,j-1,k) * aexl(i,j-1,k) * (1.0d0 - aexl(i-1,j,k)) * aexl(i,j,k) == 1.0d0) then

                        !---
                        i_src = i_org(i-1,j-1)
                        j_src = j_org(i-1,j-1)
                        tmp_sign = np_sign(i-1,j-1)
                        mu_u  = alonu(i_src) * radian_r
                        psi_u = alatu(j_src) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
#ifdef OGCM_CMIP5
                        isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                        isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */
                        weight = (1.0d0 - weightx) * (1.0d0 - weighty) + (1.0d0 - weightx) * weighty * dx / (dx + dy)

                        nlink = nlink + 1
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = weight
                        wgt(2,nlink) = weight * rot_cos * tmp_sign
                        wgt(3,nlink) = - weight * rot_sin * tmp_sign

                        !---
                        i_src = i_org(i,j-1)
                        j_src = j_org(i,j-1)
                        tmp_sign = np_sign(i,j-1)

                        mu_u  = alonu(i_src) * radian_r
                        psi_u = alatu(j_src) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
#ifdef OGCM_CMIP5
                        isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                        isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */
                        weight = weightx * (1.0d0 - weighty)

                        nlink = nlink + 1
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = weight
                        wgt(2,nlink) = weight * rot_cos * tmp_sign
                        wgt(3,nlink) = - weight * rot_sin * tmp_sign

                        !---
                        i_src = i_org(i,j)
                        j_src = j_org(i,j)
                        tmp_sign = np_sign(i,j)

                        mu_u  = alonu(i_src) * radian_r
                        psi_u = alatu(j_src) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
#ifdef OGCM_CMIP5
                        isrc_3d = i_src - ibu + 1 &
                             &  + (j_src - jbu) * (ieu - ibu + 1) &
                             &  + (k - 1) * (ieu - ibu + 1) * (jeu - jbu + 1)
#else /* OGCM_CMIP5 */
                        isrc_3d = i_src + (j_src - 1) * imut + (k - 1) * imut * jmut
#endif /* OGCM_CMIP5 */
                        weight = weightx * weighty + (1.0d0 - weightx) * weighty * dy / (dx + dy)

                        nlink = nlink + 1
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = weight
                        wgt(2,nlink) = weight * rot_cos * tmp_sign
                        wgt(3,nlink) = - weight * rot_sin * tmp_sign

                      end if

                      !------------------

                      if (aexl(i-1,j-1,k) * aexl(i,j-1,k) * aexl(i-1,j,k) * (1.0d0 - aexl(i,j,k)) == 1.0d0) then

                        !---
                        i_src = i_org(i-1,j-1)
                        j_src = j_org(i-1,j-1)
                        tmp_sign = np_sign(i-1,j-1)

                        mu_u  = alonu(i_src) * radian_r
                        psi_u = alatu(j_src) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
#ifdef OGCM_CMIP5
                        isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                        isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */
                        weight = (1.0d0 - weightx) * (1.0d0 - weighty)

                        nlink = nlink + 1
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = weight
                        wgt(2,nlink) = weight * rot_cos * tmp_sign
                        wgt(3,nlink) = - weight * rot_sin * tmp_sign

                        !---
                        i_src = i_org(i,j-1)
                        j_src = j_org(i,j-1)
                        tmp_sign = np_sign(i,j-1)

                        mu_u  = alonu(i_src) * radian_r
                        psi_u = alatu(j_src) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
#ifdef OGCM_CMIP5
                        isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                        isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */
                        weight = weightx * (1.0d0 - weighty) + weightx * weighty * dx / (dx + dy)

                        nlink = nlink + 1
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = weight
                        wgt(2,nlink) = weight * rot_cos * tmp_sign
                        wgt(3,nlink) = - weight * rot_sin * tmp_sign

                        !---
                        i_src = i_org(i-1,j)
                        j_src = j_org(i-1,j)
                        tmp_sign = np_sign(i-1,j)
                        mu_u  = alonu(i_src) * radian_r
                        psi_u = alatu(j_src) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
#ifdef OGCM_CMIP5
                        isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                        isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */
                        weight = (1.0d0 - weightx) * weighty + weightx * weighty * dy / (dx + dy)

                        nlink = nlink + 1
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = weight
                        wgt(2,nlink) = weight * rot_cos * tmp_sign
                        wgt(3,nlink) = - weight * rot_sin * tmp_sign

                      end if

                      ! 2-points

                      if ((1.0d0 - aexl(i-1,j-1,k)) * (1.0d0 - aexl(i,j-1,k)) * aexl(i-1,j,k) * aexl(i,j,k) == 1.0d0) then

                        do iii = i_base, i_base + 1

                          weightx = abs(lon_trn - alonu(iii)) / (alonu(i_base+1) - alonu(i_base))
                          i_src = i_org(iii,j_base+1)
                          j_src = j_org(iii,j_base+1)
                          tmp_sign = np_sign(iii,j_base+1)
#ifdef OGCM_CMIP5
                          isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                          isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */

                          ! rotation: lon-lat(labmda0, phi0) => model (mu_u, psi_u)

                          mu_u  = alonu(i_src) * radian_r
                          psi_u = alatu(j_src) * radian_r
                          call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
                        
                          nlink = nlink + 1
                          isrc(nlink) = isrc_3d
                          idst(nlink) = idst_3d
                          wgt(1,nlink) = (1.0d0 - weightx)
                          wgt(2,nlink) = (1.0d0 - weightx) * rot_cos * tmp_sign
                          wgt(3,nlink) = - (1.0d0 - weightx) * rot_sin * tmp_sign

                        end do

                      end if

                      if (aexl(i-1,j-1,k) * aexl(i,j-1,k) * (1.0d0 - aexl(i-1,j,k)) * (1.0d0 - aexl(i,j,k)) == 1.0d0) then

                        do iii = i_base, i_base + 1

                          weightx = abs(lon_trn - alonu(iii)) / (alonu(i_base+1) - alonu(i_base))
                          i_src = i_org(iii,j_base)
                          j_src = j_org(iii,j_base)
                          tmp_sign = np_sign(iii,j_base)
#ifdef OGCM_CMIP5
                          isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                          isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */

                          ! rotation: lon-lat(labmda0, phi0) => model (mu_u, psi_u)

                          mu_u  = alonu(i_src) * radian_r
                          psi_u = alatu(j_src) * radian_r
                          call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
                        
                          nlink = nlink + 1
                          isrc(nlink) = isrc_3d
                          idst(nlink) = idst_3d
                          wgt(1,nlink) = (1.0d0 - weightx)
                          wgt(2,nlink) = (1.0d0 - weightx) * rot_cos * tmp_sign
                          wgt(3,nlink) = - (1.0d0 - weightx) * rot_sin * tmp_sign

                        end do

                      end if

                      if ((1.0d0 - aexl(i-1,j-1,k)) * aexl(i,j-1,k) * (1.0d0 - aexl(i-1,j,k)) * aexl(i,j,k) == 1.0d0) then

                        do jjj = j_base, j_base + 1

                          weighty = abs(lat_trn - alatu(jjj)) / (alatu(j_base+1) - alatu(j_base))
                          i_src = i_org(i_base+1,jjj)
                          j_src = j_org(i_base+1,jjj)
                          tmp_sign = np_sign(i_base+1,jjj)
#ifdef OGCM_CMIP5
                          isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                          isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */

                          ! rotation: lon-lat(labmda0, phi0) => model (mu_u, psi_u)

                          mu_u  = alonu(i_src) * radian_r
                          psi_u = alatu(j_src) * radian_r
                          call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
                        
                          nlink = nlink + 1
                          isrc(nlink) = isrc_3d
                          idst(nlink) = idst_3d
                          wgt(1,nlink) = (1.0d0 - weighty)
                          wgt(2,nlink) = (1.0d0 - weighty) * rot_cos * tmp_sign
                          wgt(3,nlink) = - (1.0d0 - weighty) * rot_sin * tmp_sign

                        end do

                      end if

                      if (aexl(i-1,j-1,k) * (1.0d0 - aexl(i,j-1,k)) * aexl(i-1,j,k) * (1.0d0 - aexl(i,j,k)) == 1.0d0) then

                        do jjj = j_base, j_base + 1

                          weighty = abs(lat_trn - alatu(jjj)) / (alatu(j_base+1) - alatu(j_base))
                          i_src = i_org(i_base,jjj)
                          j_src = j_org(i_base,jjj)
                          tmp_sign = np_sign(i_base,jjj)
#ifdef OGCM_CMIP5
                          isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                          isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */

                          ! rotation: lon-lat(labmda0, phi0) => model (mu_u, psi_u)

                          mu_u  = alonu(i_src) * radian_r
                          psi_u = alatu(j_src) * radian_r
                          call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
                        
                          nlink = nlink + 1
                          isrc(nlink) = isrc_3d
                          idst(nlink) = idst_3d
                          wgt(1,nlink) = (1.0d0 - weighty)
                          wgt(2,nlink) = (1.0d0 - weighty) * rot_cos * tmp_sign
                          wgt(3,nlink) = - (1.0d0 - weighty) * rot_sin * tmp_sign

                        end do

                      end if

                      ! Theree points are land

                      if ((1.0d0 - aexl(i-1,j-1,k)) * (1.0d0 - aexl(i,j-1,k)) * (1.0d0 - aexl(i-1,j,k)) * aexl(i,j,k) == 1.0d0) then

                        i_src = i_org(i,j)
                        j_src = j_org(i,j)
                        tmp_sign = np_sign(i,j)
                        mu_u  = alonu(i_src) * radian_r
                        psi_u = alatu(j_src) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
#ifdef OGCM_CMIP5
                        isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                        isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */
                        weight = 1.0d0

                        nlink = nlink + 1
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = weight
                        wgt(2,nlink) = weight * rot_cos * tmp_sign
                        wgt(3,nlink) = - weight * rot_sin * tmp_sign

                      end if

                      if ((1.0d0 - aexl(i-1,j-1,k)) * (1.0d0 - aexl(i,j-1,k)) * aexl(i-1,j,k) * (1.0d0 - aexl(i,j,k)) == 1.0d0) then

                        i_src = i_org(i-1,j)
                        j_src = j_org(i-1,j)
                        tmp_sign = np_sign(i-1,j)
                        mu_u  = alonu(i_src) * radian_r
                        psi_u = alatu(j_src) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
#ifdef OGCM_CMIP5
                        isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                        isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */
                        weight = 1.0d0

                        nlink = nlink + 1
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = weight
                        wgt(2,nlink) = weight * rot_cos * tmp_sign
                        wgt(3,nlink) = - weight * rot_sin * tmp_sign

                      end if

                      if ((1.0d0 - aexl(i-1,j-1,k)) * aexl(i,j-1,k) * (1.0d0 - aexl(i-1,j,k)) * (1.0d0 - aexl(i,j,k)) == 1.0d0) then

                        i_src = i_org(i,j-1)
                        j_src = j_org(i,j-1)
                        tmp_sign = np_sign(i,j-1)
                        mu_u  = alonu(i_src) * radian_r
                        psi_u = alatu(j_src) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
#ifdef OGCM_CMIP5
                        isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                        isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */
                        weight = 1.0d0

                        nlink = nlink + 1
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = weight
                        wgt(2,nlink) = weight * rot_cos * tmp_sign
                        wgt(3,nlink) = - weight * rot_sin * tmp_sign

                      end if

                      if (aexl(i-1,j-1,k) * (1.0d0 - aexl(i,j-1,k)) * (1.0d0 - aexl(i-1,j,k)) * (1.0d0 - aexl(i,j,k)) == 1.0d0) then

                        i_src = i_org(i-1,j-1)
                        j_src = j_org(i-1,j-1)
                        tmp_sign = np_sign(i-1,j-1)
                        mu_u  = alonu(i_src) * radian_r
                        psi_u = alatu(j_src) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)
#ifdef OGCM_CMIP5
                        isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                        isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */
                        weight = 1.0d0

                        nlink = nlink + 1
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = weight
                        wgt(2,nlink) = weight * rot_cos * tmp_sign
                        wgt(3,nlink) = - weight * rot_sin * tmp_sign

                      end if

                    end if

                    if (l_bilinear) then ! bilinear

                      do jjj = j_base, j_base + 1
                        do iii = i_base, i_base + 1

                          weightx = abs(lon_trn - alonu(iii)) / (alonu(i_base+1) - alonu(i_base))
                          weighty = abs(lat_trn - alatu(jjj)) / (alatu(j_base+1) - alatu(j_base))

                          i_src = i_org(iii,jjj)
                          j_src = j_org(iii,jjj)
                          tmp_sign = np_sign(iii,jjj)
#ifdef OGCM_CMIP5
                          isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                          isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */

                          ! rotation: lon-lat(labmdau, phiu) => model (mu_u, psi_u)

                          mu_u  = alonu(i_src) * radian_r
                          psi_u = alatu(j_src) * radian_r
                          call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)

                          nlink = nlink + 1
                          !write(6,*) ' New link (src, dst) ', isrc_div, idst_3d
                          !write(6,*) '                     ', i_div_t(iidv), j_div_t(jjdv), k
                          isrc(nlink) = isrc_3d
                          idst(nlink) = idst_3d
                          wgt(1,nlink) = (1.0d0 - weightx) * (1.0d0 - weighty)
                          wgt(2,nlink) = (1.0d0 - weightx) * (1.0d0 - weighty) * rot_cos * tmp_sign
                          wgt(3,nlink) = - (1.0d0 - weightx) * (1.0d0 - weighty) * rot_sin * tmp_sign

                        end do
                      end do

                    end if

                    if (l_bicubic) then

                      ndim = 16

                      allocate(a_fac(ndim,ndim), a_inv(ndim,ndim))
                      allocate(ipiv(ndim))

                      n = 0

                      do jjj = j_base - 1, j_base + 2

                        y_tmp = alatu(jjj) - lat_trn

                        do iii = i_base - 1, i_base + 2

                          x_tmp = alonu(iii) - lon_trn

                          n = n + 1

                          a_fac(n, 1) = (x_tmp ** 3) * (y_tmp ** 3)
                          a_fac(n, 2) = (x_tmp ** 3) * (y_tmp ** 2)
                          a_fac(n, 3) = (x_tmp ** 2) * (y_tmp ** 3)
                          a_fac(n, 4) = (x_tmp ** 3) * y_tmp
                          a_fac(n, 5) = (x_tmp ** 2) * (y_tmp ** 2)
                          a_fac(n, 6) = x_tmp * (y_tmp ** 3)
                          a_fac(n, 7) = x_tmp ** 3
                          a_fac(n, 8) = (x_tmp ** 2) * y_tmp
                          a_fac(n, 9) = x_tmp * (y_tmp ** 2)
                          a_fac(n,10) = y_tmp ** 3
                          a_fac(n,11) = x_tmp ** 2
                          a_fac(n,12) = x_tmp * y_tmp
                          a_fac(n,13) = y_tmp ** 2
                          a_fac(n,14) = x_tmp
                          a_fac(n,15) = y_tmp
                          a_fac(n,16) = 1.0d0

                        end do

                      end do

                      a_inv(1:ndim,1:ndim) = a_fac(1:ndim,1:ndim)

                      lda = ndim
                      call dgetrf(ndim,ndim,a_inv,lda,ipiv,info)
                      if (info /= 0) write(6,*) 'dgetrf info =', info

                      !do j = 1, dim
                      !  write(6,'(16F8.3)') (a(i,j),i=1,ndim)
                      !end do

                      lwork = ndim
                      allocate(work(1:ndim))
                      call dgetri(ndim,a_inv,lda,ipiv,work,lwork,info)
                      deallocate(work)

                      if (info /= 0) then
                        write(6,*) 'dgetri info =', info
                        write(6,*) 'dgetri lwork =', lwork
                      end if

                      tmp = 0.0d0
                      do n = 1, ndim
                        tmp = tmp + a_inv(16,n) * a_fac(n,16)
                      end do

                      if (abs(tmp-1.0d0) > 1.0d-4) write(6,*) tmp

                      n = 0

                      do jjj = j_base - 1, j_base + 2
                        do iii = i_base - 1, i_base + 2

                          n = n + 1

                          i_src = i_org(iii,jjj)
                          j_src = j_org(iii,jjj)
                          tmp_sign = np_sign(iii,jjj)

#ifdef OGCM_CMIP5
                          isrc_3d = i_src - ibu + 1 + (j_src - jbu) * i_width + (k - 1) * i_width * j_width
#else /* OGCM_CMIP5 */
                          isrc_3d = i_src + (j_src - 1) * i_width + (k - 1) * i_width * j_width
#endif /* OGCM_CMIP5 */

                          ! rotation: lon-lat(labmdau, phiu) => model (mu_u, psi_u)

                          mu_u  = alonu(i_src) * radian_r
                          psi_u = alatu(j_src) * radian_r
                          call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)

                          nlink = nlink + 1
                          !write(6,*) ' New link (src, dst) ', isrc_div, idst_3d
                          !write(6,*) '                     ', i_div_t(iidv), j_div_t(jjdv), k
                          isrc(nlink) = isrc_3d
                          idst(nlink) = idst_3d
                          wgt(1,nlink) = a_inv(16,n)
                          wgt(2,nlink) = a_inv(16,n) * rot_cos * tmp_sign
                          wgt(3,nlink) = - a_inv(16,n) * rot_sin * tmp_sign
!                          write(6,*) isrc_3d, idst_3d, wgt(2,nlink), wgt(3,nlink)

                        end do

                      end do

                      deallocate(a_fac, a_inv)
                      deallocate(ipiv)
                    end if

                  end if

                  exit

                end if
                
              end do

              exit

            end if

          end do

        end do
      end do

    end do

    !------

    write(6,'(1a,I8)') 'nlink =        ', nlink
    write(6,'(1a,I8)') 'num_max_link = ', num_max_link
    isrc_max = maxval(isrc(1:nlink))
    idst_max = maxval(idst(1:nlink))
    write(6,*) '   isrc_max = ',isrc_max,' / ', imut * jmut * km
    write(6,*) '   idst_max = ',idst_max,' / ', imaxg * jmaxg * km

    write(6,*) ' Writing table ...'
    open(mttbl,file=file_rmp_table,form='unformatted',action='write')
    write(mttbl) isrc_max
    write(mttbl)
    write(mttbl)
    write(mttbl) idst_max
    write(mttbl)
    write(mttbl)
    write(mttbl) num_wgts_all, nlink
    write(mttbl) isrc(1:nlink)
    write(mttbl) idst(1:nlink)
    write(mttbl) wgt (1:num_wgts_all,1:nlink)
    close(mttbl)

    deallocate(isrc,idst,wgt)

    deallocate(i_org)
    deallocate(j_org)
    deallocate(np_sign)

  end subroutine create_remap_table_vec_cubic
  !=====================================================================
end program remap_vector
