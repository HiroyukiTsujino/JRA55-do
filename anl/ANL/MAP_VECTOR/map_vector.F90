!-*-F90-*-
!map_vector.F90
!====================================================
program map_vector

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
#endif /* OGCM_BBL */

  use oc_mod_trnsfrm, only  : &
  &   set_abc, mp2lp, rot_mp2lp

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
  integer(4) :: j_begin_search

  real(8), allocatable :: u(:,:,:), v(:,:,:)
  real(8)          :: undef_in
  integer(4)       :: id_start, id_end
  integer(4)       :: itemu_start   ! start record of the 1st layer of X-vector 
  integer(4)       :: itemv_start   ! start record of the 1st layer of Y-vector 
  integer(4)       :: itemu_intv
  integer(4)       :: itemv_intv

  integer(4)       :: k_start, k_end
  integer(4) :: isrc_3d, isrc_2d, i_src, j_src, k_src

  real(4),allocatable :: d2_r4(:,:)
  real(4)    :: d0_r4

  ! original grid

  integer(4), parameter :: ibu = 3, ieu = imut - 2  ! cyclic
  integer(4), parameter :: jbu = 2, jeu = jmut - 3  ! tripolar

  integer(4), parameter :: ibt = 3, iet = imut - 2  ! cyclic
  integer(4), parameter :: jbt = 2, jet = jmut - 2  ! tripolar

  integer(4), save :: lrec_out
  real(8), allocatable :: area_received(:,:,:)
  real(8), allocatable :: rotation_cos(:,:)
  real(8), allocatable :: rotation_sin(:,:)
  real(8), allocatable :: u_received(:,:,:)
  real(8), allocatable :: v_received(:,:,:)
  real(8), parameter :: eps = 1.0d-5
  real(4), allocatable :: dat_out(:,:)
  real(8) :: undef_out
  integer(4) :: idst_3d, idst_2d, i_dst, j_dst, k_dst

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
  integer(4), parameter :: mtin_u   = 84
  integer(4), parameter :: mtin_v   = 85
  integer(4), parameter :: mtout_u  = 86
  integer(4), parameter :: mtout_v  = 87
  integer(4), parameter :: mtstdout = 88

  logical :: l_read_sep = .false.
  logical :: l_write_sep = .false.

  integer(4) :: mtin, mtout
  integer(4) :: ireci_u, ireci_v
  integer(4) :: irecw_u, irecw_v
  integer(4) :: irecw

  integer(4) :: i, j, k, m, n
#ifdef OGCM_BBL
  integer(4) :: kbtm
#endif /* OGCM_BBL */
  !
#ifdef OGCM_CYCLIC
  logical, parameter :: lcyclic = .true.
#else /* OGCM_CYCLIC */
  logical, parameter :: lcyclic = .false.
#endif /* OGCM_CYCLIC */

  namelist /nml_map_vec/       &
       &  idiv, jdiv,          &
       &  sph_grid,            &
       &  undef_in, undef_out, &
       &  id_start, id_end,    &
       &  itemu_start,         &
       &  itemv_start,         &
       &  itemu_intv,          &
       &  itemv_intv,          &
       &  l_read_sep,          &
       &  l_write_sep,         &
       &  k_start, k_end,      &
       &  l_read_table,        &
       &  l_check_table,       &
       &  file_rmp_table,      &
       &  file_recv_area,      &
       &  flinu, flinv,        &
       &  floutu, floutv,      &
       &  fltopo, flsclf, flstdout
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
  flstdout = 'map_vector.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml_map_vec)

  write(6,*) 'sph_grid :', trim(sph_grid)

  write(6,*) 'fltopo   :', trim(fltopo)
  write(6,*) 'flsclf   :', trim(flsclf)

  write(6,*) 'l_read_table   :',l_read_table
  write(6,*) 'file_rmp_table :',trim(file_rmp_table)
  write(6,*) 'file_recv_area :',trim(file_recv_area)

  if (.not. l_read_table) then
    write(6,*) 'idiv   :', idiv
    write(6,*) 'jdiv   :', jdiv
  end if

  write(6,*) 'flinu    :', trim(flinu)
  write(6,*) 'flinv    :', trim(flinv)
  write(6,*) 'itemu_start  :', itemu_start
  write(6,*) 'itemv_start  :', itemv_start
  write(6,*) 'undef_in :', undef_in
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

  !----------------------------------------------
  ! Set target lat-lon grid lattice

  call set_gaussgrid (sph_grid)

  write(mtstdout,*) 'imaxg = ', imaxg
  write(mtstdout,*) 'jmaxg = ', jmaxg

  !----------------------------------------------

  allocate(area_received(1:imut,1:jmut,k_start:k_end))
  allocate(rotation_cos(1:imut,1:jmut))
  allocate(rotation_sin(1:imut,1:jmut))
  allocate(u_received(1:imut,1:jmut,k_start:k_end))
  allocate(v_received(1:imut,1:jmut,k_start:k_end))
  allocate(dat_out(1:imut,1:jmut))

  lrec_out = imut*jmut*4

  !----------------------------------------------

  if (.not. l_read_table) then
    call create_map_table_vec_cubic ! output table
  end if

  write(6,*) ' Reading table ...'
  open(mttbl,file=file_rmp_table,form='unformatted',action='read')

  read(mttbl) isrc_max
  read(mttbl)
  read(mttbl)
  read(mttbl) idst_max
  read(mttbl)
  read(mttbl)

  write(6,*) '   isrc_max = ',isrc_max,' / ', imaxg * jmaxg
  write(6,*) '   idst_max = ',idst_max,' / ', imut * jmut

  read(mttbl) num_wgts_all, nlink

  write(6,'(1a,I8)') 'nlink = ', nlink

  allocate(isrc(1:nlink))
  allocate(idst(1:nlink))
  allocate(wgt(1:num_wgts_all,1:nlink))

  read(mttbl) isrc(1:nlink)
  read(mttbl) idst(1:nlink)
  read(mttbl) wgt (1:num_wgts_all,1:nlink)

  close(mttbl)

  !-------------------------------------------------

  write(6,*) imaxg, jmaxg

  allocate(u(1:imaxg, 1:jmaxg, k_start:k_end))
  allocate(v(1:imaxg,1:jmaxg,k_start:k_end))

  allocate(d2_r4(1:imaxg,1:jmaxg))

  !--------------------------------------------------

  open (mtin_u, file=flinu, form='unformatted', &
       &  access='direct',recl=4*imaxg*jmaxg, action='read')
  ireci_u = 0
  write(6,*) 'reading original data (U) from', trim(flinu)

  if (l_read_sep) then
    write(6,*) 'l_read_sep = ', l_read_sep
    open (mtin_v, file=flinv, form='unformatted', &
         &  access='direct', recl=4*imaxg*jmaxg, action='read')
    ireci_v = 0
    write(6,*) 'reading original data (V) from', trim(flinv)
  else
    write(6,*) 'reading original data (V) from', trim(flinu)
  end if

  !--------------------------------------------------

  open (mtout_u, file=floutu, form='unformatted', access='direct', recl=lrec_out)
  irecw_u = 0
  write(6,*) 'writing mapped data (U) to', trim(floutu)

  if (l_write_sep) then
    open (mtout_v, file=floutv, form='unformatted', access='direct', recl=lrec_out)
    irecw_v = 0
    write(6,*) 'writing mapped data (V) to', trim(floutv)
  else
    write(6,*) 'writing mapped data (V) to', trim(floutu)
  end if

  !--------------------------------------------------

  LOOP_DATA: do m = id_start, id_end

    write(6,*) 'Processing ',m,'-th record'

  !-----------
  ! read data

  mtin = mtin_u

  do k = k_start, k_end
    ireci_u = itemu_start + itemu_intv * (m - 1) - 1 + k
    write(6,*) 'reading record number = ', ireci_u, ' from unit ', mtin
    read (mtin, rec=ireci_u) d2_r4
    u(:,:,k) = real(d2_r4(:,:),8)
  end do

  if (l_read_sep) then
    mtin = mtin_v
  else
    mtin = mtin_u
  end if

  do k = k_start, k_end
    ireci_v = itemv_start + itemv_intv * (m - 1) - 1 + k
    write(6,*) 'reading record number = ', ireci_v, ' from unit ', mtin
    read (mtin, rec=ireci_v) d2_r4
    v(:,:,k) = real(d2_r4(:,:),8)
  end do

  !----------------------------------------------

  u_received(1:imut,1:jmut,k_start:k_end) = 0.0d0
  v_received(1:imut,1:jmut,k_start:k_end) = 0.0d0

  area_received(1:imut,1:jmut,k_start:k_end) = 0.0d0

  rotation_cos(1:imut,1:jmut) = 0.0d0
  rotation_sin(1:imut,1:jmut) = 0.0d0

  do n = 1, nlink

    ! source (lat-lon grid)

    isrc_3d = isrc(n)
    k_src = int((isrc_3d - 1) / (imaxg * jmaxg)) + 1
    isrc_2d = isrc_3d - (k_src - 1) * imaxg * jmaxg
    j_src = jmaxg - int((isrc_2d - 1) / imaxg)
    i_src = isrc_2d - (jmaxg - j_src) * imaxg

#ifdef OGCM_CMIP5
    idst_3d = idst(n)
    k_dst = int((idst_3d - 1) / ((ieu - ibu + 1) * (jeu - jbu + 1))) + 1
    idst_2d = idst_3d - (k_dst - 1) * (ieu - ibu + 1) * (jeu - jbu + 1)
    j_dst = int((idst_2d - 1) / (ieu - ibu + 1)) + jbu
    i_dst = idst_2d - (j_dst - jbu) * (ieu - ibu + 1) + ibu - 1
#else /* OGCM_CMIP5 */
    idst_3d = idst(n)
    k_dst = int((idst_3d - 1) / (imut * jmut)) + 1
    idst_2d = idst_3d - (k_dst - 1) * imut * jmut
    j_dst = int((idst_2d - 1) / imut) + 1
    i_dst = idst_2d - (j_dst - 1) * imut
#endif /* OGCM_CMIP5 */
    if ((i_dst < ibu)  .or. (ieu < i_dst)) then
      write(6,'(1a,4i6)') ' i_dst is out of core region : ', i_dst, ibu, ieu, j_dst
    end if
    if ((j_dst < jbu)  .or. (jeu < j_dst)) then
      write(6,'(1a,4i6)') ' j_dst is out of core region : ', j_dst, jbu, jeu, i_dst
    end if

    if (u(i_src,j_src,k_src) /= undef_in) then

#ifdef OGCM_CMIP5ORG
      if (j_dst == jeu) then
        if ((i_dst <= ((imut - 4)/4 + 2)) .and. (wgt(3,n) > 0.0d0)) then
          wgt(3,n) = -wgt(3,n)
        end if
        if ((((imut - 4)/2 + 3) <= i_dst) .and. (i_dst <= (3*(imut - 4)/4 + 2)) &
             & .and. (wgt(3,n) > 0.0d0)) then
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

      write(6,*) ' Valid datum is expected, but not found: ', i_src, j_src, k_src

    end if

  end do

  !-----------------------------------------------

#ifdef OGCM_CYCLIC
  do k = k_start, k_end
    do j = 1, jmut
      area_received(1,j,k) = area_received(ieu-1,j,k)
      area_received(2,j,k) = area_received(ieu  ,j,k)
      area_received(ieu+1,j,k) = area_received(ibu  ,j,k)
      area_received(ieu+2,j,k) = area_received(ibu+1,j,k)

      u_received(1,j,k) = u_received(ieu-1,j,k)
      u_received(2,j,k) = u_received(ieu  ,j,k)
      u_received(ieu+1,j,k) = u_received(ibu  ,j,k)
      u_received(ieu+2,j,k) = u_received(ibu+1,j,k)

      v_received(1,j,k) = v_received(ieu-1,j,k)
      v_received(2,j,k) = v_received(ieu  ,j,k)
      v_received(ieu+1,j,k) = v_received(ibu  ,j,k)
      v_received(ieu+2,j,k) = v_received(ibu+1,j,k)
    end do
  end do
#endif /* OGCM_CYCLIC */

#if defined OGCM_TRIPOLAR || defined OGCM_JOT
  do k = k_start, k_end
    do i = 1, imut
      area_received(i,jeu+1,k) = area_received(imut-i+1,jeu  ,k)
      area_received(i,jeu+2,k) = area_received(imut-i+1,jeu-1,k)
      area_received(i,jeu+3,k) = area_received(imut-i+1,jeu-2,k)

      u_received(i,jeu+1,k) = - u_received(imut-i+1,jeu  ,k)
      u_received(i,jeu+2,k) = - u_received(imut-i+1,jeu-1,k)
      u_received(i,jeu+3,k) = - u_received(imut-i+1,jeu-2,k)

      v_received(i,jeu+1,k) = - v_received(imut-i+1,jeu  ,k)
      v_received(i,jeu+2,k) = - v_received(imut-i+1,jeu-1,k)
      v_received(i,jeu+3,k) = - v_received(imut-i+1,jeu-2,k)
    end do
  end do
#endif /* OGCM_TRIPOLAR || OGCM_JOT */

  do k = k_start, k_end
    do j = 1, jmut
      do i = 1, imut
        if (area_received(i,j,k) == 0.0d0) then
          u_received(i,j,k) = undef_out
          v_received(i,j,k) = undef_out
        end if
      end do
    end do
  end do

  !----------------------------------------------
  ! output data

  mtout = mtout_u

  if (.not. l_write_sep) then
    irecw_u = irecw_v
  end if

  do k = k_start, k_end
    dat_out(1:imut,1:jmut) = real(u_received(1:imut,1:jmut,k),4)
    irecw_u = irecw_u + 1
    write(6,*) ' writing record number', irecw_u, ' for unit ', mtout
    write(mtout, rec=irecw_u) dat_out
  end do

  if (l_write_sep) then
!    write(6,*) 'l_write_sep = ', l_write_sep
    mtout = mtout_v
  else
    mtout = mtout_u
    irecw_v = irecw_u
  end if

  do k = k_start, k_end
    dat_out(1:imut,1:jmut) = real(v_received(1:imut,1:jmut,k),4)
    irecw_v = irecw_v + 1
    write(6,*) ' writing record number', irecw_v, ' for unit ', mtout
    write(mtout, rec=irecw_v) dat_out
  end do

  end do LOOP_DATA

  if (l_write_sep) then
    close ( mtout_v )
  end if
  close ( mtout_u )

  if (l_read_sep) then
    close ( mtin_v )
  end if
  close ( mtin_u )

  deallocate(d2_r4)

  !------------------------------------------------

  if (l_check_table) then

    open(mttmp,file=file_recv_area,form='unformatted',access='direct',action='write',recl=lrec_out)
    irecw = 0
    do k = k_start, k_end
      dat_out(1:imut,1:jmut) = real(area_received(1:imut,1:jmut,k),4)
      irecw = irecw + 1
      write(mttmp, rec=irecw) dat_out
    end do
    dat_out(1:imut,1:jmut) = real(rotation_cos(1:imut,1:jmut),4)
    irecw = irecw + 1
    write(mttmp, rec=irecw) dat_out
    dat_out(1:imut,1:jmut) = real(rotation_sin(1:imut,1:jmut),4)
    irecw = irecw + 1
    write(mttmp, rec=irecw) dat_out

    close(mttmp)

  end if

  close ( mtstdout )

contains

  !=====================================================================
  subroutine create_map_table_vec_cubic

    implicit none

    real(8)    :: mu_u, psi_u, lambdau, phiu
    real(8)    :: lon_trn, lat_trn
    real(8)    :: rot_cos, rot_sin

    real(8),allocatable :: long_search(:)
    real(8),allocatable :: latg_search(:)

    integer(4),allocatable :: i_org(:), j_org(:)

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
    real(8) :: weightx, weighty
    real(8) :: tmp

    !----------------------------------------------

    allocate(long_search(-1:imaxg+2))
    allocate(i_org(-1:imaxg+2))

    long_search(1:imaxg) = long(1:imaxg)
    do i = 1, imaxg
      i_org(i) = i
    end do

    long_search(-1) = long(imaxg - 1) - 360.d0
    i_org(-1) = imaxg - 1

    long_search(0) = long(imaxg) - 360.d0
    i_org(0) = imaxg

    long_search(imaxg+1) = long(1) + 360.d0
    i_org(imaxg+1) = 1

    long_search(imaxg+2) = long(2) + 360.d0
    i_org(imaxg+2) = 2

    allocate(latg_search(0:jmaxg+1))
    allocate(j_org(0:jmaxg+1))

    latg_search(1:jmaxg) = latg(1:jmaxg)
    do j = 1, jmaxg
      j_org(j) = j
    end do

    latg_search(0) = -90.0d0
    j_org(0) = 1

    latg_search(jmaxg+1) = 90.0d0
    j_org(jmaxg+1) = jmaxg

    !------

    k_levels = k_end - k_start + 1

    num_wgts_all = 3
    num_max_link = 16*imut*jmut*k_levels 

    allocate(isrc(num_max_link))
    allocate(idst(num_max_link))
    allocate(wgt(1:num_wgts_all,num_max_link))

    isrc(:) = 0
    idst(:) = 0
    wgt(:,:) = 0.d0

    nlink = 0

    do k = k_start, k_end

      do j = jbu, jeu

        write(6,*) j ,'/', jeu
        write(6,*) 'Ocean model latitude', alatu(j)

        do i = ibu, ieu

          if (aexl(i,j,k) /= 0.0d0) then ! U-point datum is valid

            mu_u  = alonu(i) * radian_r
            psi_u = alatu(j) * radian_r

            ! model (mu, psi) => lon-lat(labmda, phi)
            call mp2lp(lambdau, phiu, mu_u, psi_u)

            ! rotation: lon-lat(labmda, phi) => model (mu, psi)
            call rot_mp2lp(rot_cos, rot_sin, lambdau, phiu, mu_u, psi_u)

            ! 0 <= lon_trn < 360

            lon_trn = lambdau * radian
            if (lon_trn < 0.0d0) then
              lon_trn = lon_trn + 360.d0
            end if
            if (lon_trn >= 360.0d0) then
              lon_trn = lon_trn - 360.d0
            end if

            ! -90 <= lat_trn <= 90

            lat_trn = phiu * radian
            if (lat_trn > 90.d0) then
              write(6,*) 'erroneous transformation'
              write(6,*) lat_trn, alonu(i), alatu(j)
              stop
            end if

            ! create link, mapping table

#ifdef OGCM_CMIP5
            idst_3d = i - ibu + 1 &
                 &  + (j - jbu) * (ieu - ibu + 1) &
                 &  + (k - 1) * (ieu - ibu + 1) * (jeu - jbu + 1)
#else /* OGCM_CMIP5 */
            idst_3d = i + (j - 1) * imut + (k - 1) * imut * jmut
#endif /* OGCM_CMIP5 */

            do jj = 0, jmaxg

              if ((latg_search(jj) <= lat_trn) .and. (lat_trn < latg_search(jj+1))) then

                j_src = jj

                do ii = 1, imaxg

                  if ((long_search(ii) <= lon_trn) .and. (lon_trn < long_search(ii+1))) then

                    i_src = ii

                    if ((2 <= j_src) .and. (j_src <= (jmaxg - 2))) then ! cubic

                      ndim = 16

                      allocate(a_fac(ndim,ndim), a_inv(ndim,ndim))
                      allocate(ipiv(ndim))

                      n = 0

                      do jjj = j_src - 1, j_src + 2

                        y_tmp = latg_search(jjj) - lat_trn

                        do iii = i_src - 1, i_src + 2

                          x_tmp = long_search(iii) - lon_trn

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

                      if (abs(tmp-1.0d0) > 1.0d-4) write(6,*) 'ungood result ', tmp

                      n = 0

                      do jjj = j_src - 1, j_src + 2

                        do iii = i_src - 1, i_src + 2

                          n = n + 1

                          isrc_3d = i_org(iii) + (jmaxg - j_org(jjj)) * imaxg + (k - 1) * imaxg * jmaxg

                          lambda0 = long_search(i_org(iii)) * radian_r
                          phi0    = latg_search(j_org(jjj)) * radian_r

                          ! rotation: lon-lat(labmda0, phi0) => model (mu_u, psi_u)

                          call rot_mp2lp(rot_cos, rot_sin, lambda0, phi0, mu_u, psi_u)

                          nlink = nlink + 1
                          !write(6,*) ' New link (src, dst) ', isrc_div, idst_3d
                          !write(6,*) '                     ', i_div_t(iidv), j_div_t(jjdv), k
                          isrc(nlink) = isrc_3d
                          idst(nlink) = idst_3d
                          wgt(1,nlink) = a_inv(16,n)
                          wgt(2,nlink) = a_inv(16,n) * rot_cos
                          wgt(3,nlink) = a_inv(16,n) * rot_sin
!                          write(6,*) isrc_3d, idst_3d, wgt(2,nlink), wgt(3,nlink)

                        end do

                      end do

                      deallocate(a_fac, a_inv)
                      deallocate(ipiv)

                    else if ((j_src == 1) .or. (j_src == (jmaxg - 1))) then ! bilinear

                      do jjj = j_src, j_src + 1
                        do iii = i_src, i_src + 1

                          weightx = abs(lon_trn - long_search(iii)) / (long_search(i_src+1) - long_search(i_src))
                          weighty = abs(lat_trn - latg_search(jjj)) / (latg_search(j_src+1) - latg_search(j_src))

                          isrc_3d = i_org(iii) + (jmaxg - j_org(jjj)) * imaxg + (k - 1) * imaxg * jmaxg

                          ! rotation: lon-lat(labmda0, phi0) => model (mu_u, psi_u)
                          lambda0 = long_search(i_org(iii)) * radian_r
                          phi0    = latg_search(j_org(jjj)) * radian_r
                          call rot_mp2lp(rot_cos, rot_sin, lambda0, phi0, mu_u, psi_u)

                          nlink = nlink + 1
                          !write(6,*) ' New link (src, dst) ', isrc_div, idst_3d
                          !write(6,*) '                     ', i_div_t(iidv), j_div_t(jjdv), k
                          isrc(nlink) = isrc_3d
                          idst(nlink) = idst_3d
                          wgt(1,nlink) = (1.0d0 - weightx) * (1.0d0 - weighty)
                          wgt(2,nlink) = (1.0d0 - weightx) * (1.0d0 - weighty) * rot_cos
                          wgt(3,nlink) = (1.0d0 - weightx) * (1.0d0 - weighty) * rot_sin

!                          write(6,*) 'bilinear (X)', iii,long_search(i_src+1),long_search(i_src)
!                          write(6,*) weightx
!                          write(6,*) 'bilinear (Y)', jjj,latg_search(j_src+1),latg_search(j_src)
!                          write(6,*) weighty

                        end do
                      end do

                    else ! linear

                      do iii = i_src, i_src + 1

                        weightx = abs(lon_trn - long_search(iii)) / (long_search(i_src+1) - long_search(i_src))

                        isrc_3d = i_org(iii) + (jmaxg - j_org(j_src)) * imaxg + (k - 1) * imaxg * jmaxg

                        ! rotation: lon-lat(labmda0, phi0) => model (mu_u, psi_u)
                        lambda0 = long_search(i_org(iii)) * radian_r
                        phi0    = latg_search(j_org(j_src)) * radian_r
                        call rot_mp2lp(rot_cos, rot_sin, lambda0, phi0, mu_u, psi_u)
                        
                        nlink = nlink + 1
                        !write(6,*) ' New link (src, dst) ', isrc_div, idst_3d
                        !write(6,*) '                     ', i_div_t(iidv), j_div_t(jjdv), k
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(1,nlink) = (1.0d0 - weightx)
                        wgt(2,nlink) = (1.0d0 - weightx) * rot_cos
                        wgt(3,nlink) = (1.0d0 - weightx) * rot_sin

!                        write(6,*) 'linear', wgt(1,nlink), wgt(2,nlink), wgt(3,nlink)

                      end do

                    end if

                    exit

                  end if

                end do

                exit
                
              end if

            end do

          end if

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

    deallocate(long_search)
    deallocate(i_org)
    deallocate(latg_search)
    deallocate(j_org)

  end subroutine create_map_table_vec_cubic
  !=====================================================================
end program map_vector
