!-*-F90-*-
!remap_scalar.F90
!====================================================
program remap_scalar

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
  &   kbtm, ktbtm
#endif /* OGCM_BBL */

  use oc_mod_trnsfrm, only  : &
  &   set_abc, mp2lp

  use mod_gaussgrid

  !----------------------------------------------

  implicit none

  ! divided grid

  integer(4) :: idiv, jdiv    ! divide T-cells, this should be even
  integer(4) :: nxdiv, nydiv  ! model core region divivded into ...
  real(8),    allocatable :: alon_div(:), alat_div(:)
  integer(4), allocatable :: i_div_t(:), j_div_t(:)
  integer(4), allocatable :: i_div_u(:), j_div_u(:)

  real(8) :: area_tmp, basin_tmp, scalar_tmp
  integer(4) :: ivalid

  ! interpolated grid

  character(len=16) :: sph_grid
  integer(4), save :: lrec_out
  integer(4) :: j_begin_search
  real(8), allocatable :: area_received(:,:,:)
  real(8), allocatable :: scalar_received(:,:,:)
  real(8), parameter :: eps = 1.0d-5
  real(4), allocatable :: dat_out(:,:)
  real(8) :: undef_out
  integer(4) :: idst_3d, idst_2d, i_dst, j_dst, k_dst

  ! original grid

  integer(4), parameter :: ibu = 3, ieu = imut - 2  ! cyclic
  integer(4), parameter :: jbu = 2, jeu = jmut - 3  ! tripolar

  integer(4), parameter :: ibt = 3, iet = imut - 2  ! cyclic
  integer(4), parameter :: jbt = 2, jet = jmut - 2  ! tripolar

  real(8), allocatable :: scalar(:,:,:)
  real(8)          :: undef_in
  character(len=1) :: tuw
  integer(4)       :: item_start ! start record of this item
  integer(4)       :: k_start, k_end
  integer(4) :: isrc_3d, isrc_2d, i_src, j_src, k_src

  real(4)    :: d2_r4(imut,jmut)

  ! 入出力ファイル

  character(len=256)    :: flin     ! 入力ファイル
  character(len=256)    :: flout    ! 出力ファイル
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
  real(8),allocatable :: wgt(:)
  integer(4) :: num_wgts_all

  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mttbl    = 78
  integer(4), parameter :: mtchk    = 79
  integer(4), parameter :: mtin     = 84
  integer(4), parameter :: mtout    = 86
  integer(4), parameter :: mtstdout = 87
  !
  integer(4) :: irecw, ireci, irecc
  integer(4) :: i, j, k, m, n
  !
#ifdef OGCM_CYCLIC
  logical, parameter :: lcyclic = .true.
#else /* OGCM_CYCLIC */
  logical, parameter :: lcyclic = .false.
#endif /* OGCM_CYCLIC */

  namelist /nml_rmp_scl/       &
       &  idiv, jdiv,          &
       &  sph_grid,            &
       &  tuw,                 &
       &  undef_in, undef_out, &
       &  item_start,          &
       &  k_start, k_end,      &
       &  l_read_table,        &
       &  l_check_table,       &
       &  file_rmp_table,      &
       &  file_recv_area,      &
       &  flin, flout,         &
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
  flin   = 'hs_scalar.d'
  flout  = 'hs_scalar_sph.d'
  fltopo = 'topo.d'
  flsclf = 'scale_factor.d'
  flstdout = 'remap_t.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml_rmp_scl)

  write(6,*) 'sph_grid :', trim(sph_grid)

  write(6,*) 'fltopo   :', trim(fltopo)
  write(6,*) 'flsclf   :', trim(flsclf)

  write(6,*) 'l_read_table   :',l_read_table
  write(6,*) 'l_check_table  :',l_check_table
  write(6,*) 'file_rmp_table :',trim(file_rmp_table)
  write(6,*) 'file_recv_area :',trim(file_recv_area)

  if (.not. l_read_table) then
    write(6,*) 'idiv   :', idiv
    write(6,*) 'jdiv   :', jdiv
  end if

  write(6,*) 'flin      :', trim(flin)
  write(6,*) 'undef_in  :', undef_in
  write(6,*) 'item_start:', item_start
  write(6,*) 'k_start   :', k_start
  write(6,*) 'k_end     :', k_end
  write(6,*) 'tuw       :', tuw
  

  write(6,*) 'flout    :', trim(flout)
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

  !---------------------------------------------
  ! Set target lat-lon grid lattice

  call set_gaussgrid (sph_grid)

  write(mtstdout,*) 'imaxg = ', imaxg
  write(mtstdout,*) 'jmaxg = ', jmaxg

  allocate(area_received(1:imaxg,1:jmaxg,k_start:k_end))
  allocate(dat_out(1:imaxg,1:jmaxg))
  lrec_out = imaxg*jmaxg*4

  if (l_check_table) then
    open(mtchk,file=file_recv_area,form='unformatted',access='direct',action='write',recl=lrec_out)
    irecc = 0
  end if

  !----------------------------------------------

  if (.not. l_read_table) then
    call create_remap_table
  end if

  write(6,*) ' Reading table from...',trim(file_rmp_table)
  open(mttbl,file=file_rmp_table,form='unformatted',action='read')

  read(mttbl) isrc_max
  read(mttbl)
  read(mttbl)
  read(mttbl) idst_max
  read(mttbl)
  read(mttbl)

  write(6,*) '   isrc_max = ',isrc_max,' / ', imut * jmut, ' or ', imut * jmut * km
  write(6,*) '   idst_max = ',idst_max,' / ', imaxg * jmaxg, ' or ', imaxg * jmaxg * km

  read(mttbl) num_wgts_all, nlink

  write(6,'(1a,I8)') 'nlink = ', nlink

  allocate(isrc(1:nlink))
  allocate(idst(1:nlink))
  allocate(wgt(1:nlink))

  read(mttbl) isrc(1:nlink)
  read(mttbl) idst(1:nlink)
  read(mttbl) wgt (1:nlink)

  close(mttbl)

  !-------------------------------------------------
  ! Read Scalar data

  allocate(scalar(1:imut,1:jmut,k_start:k_end))

  open (mtin, file=flin, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)

  write(6,*) 'reading original data from', trim(flin)

  do k = k_start, k_end
    ireci = item_start - 1 + k
    read (mtin, rec=ireci) d2_r4
    if (tuw == 't' .or. tuw == 'w') then
      scalar(:,:,k) = atexl(:,:,k)*real(d2_r4(:,:),8)
    else
      scalar(:,:,k) = aexl(:,:,k)*real(d2_r4(:,:),8)
    end if
  end do

  write(6,*) 'reading data done'

  close ( mtin )

  !---------------------------------------------
  ! reset mask to treat interior only

#ifdef OGCM_BBL
  if (k_end == km) then
    if (tuw == 't' .or. tuw == 'w') then
      ! reset atexl
      atexl(1:imut,1:jmut,1:km) = 0.0d0
      do j = 1, jmut
        do i = 1, imut
          if (ktbtm(i,j) >= ksgm) then
            if (atexlbbl(i,j,1) == 1.0d0) then
              scalar(i,j,ktbtm(i,j)) = scalar(i,j,km)
              scalar(i,j,km) = 0.0d0
            end if
            do k = 1, ktbtm(i,j)
              atexl(i,j,k) = 1.0d0
            end do
          end if
        end do
      end do
    else
      ! reset aexl
      aexl(1:imut,1:jmut,1:km) = 0.0d0
      do j = 1, jmut
        do i = 1, imut
          if (kbtm(i,j) >= ksgm) then
            if (aexlbbl(i,j,1) == 1.0d0) then
              scalar(i,j,kbtm(i,j)) = scalar(i,j,km)
              scalar(i,j,km) = 0.0d0
            end if
            do k = 1, kbtm(i,j)
              aexl(i,j,k) = 1.0d0
            end do
          end if
        end do
      end do
    end if
  end if
#endif /* OGCM_BBL */

  !----------------------------------------------

  allocate(scalar_received(1:imaxg,1:jmaxg,k_start:k_end))
  scalar_received(1:imaxg,1:jmaxg,k_start:k_end) = 0.0d0
  area_received  (1:imaxg,1:jmaxg,k_start:k_end) = 0.0d0

  do n = 1, nlink

    idst_3d = idst(n)
    k_dst = int((idst_3d - 1) / (imaxg * jmaxg)) + 1
    idst_2d = idst_3d - (k_dst - 1) * imaxg * jmaxg
    j_dst = jmaxg - int((idst_2d - 1) / imaxg)
    i_dst = idst_2d - (jmaxg - j_dst) * imaxg

#ifdef OGCM_CMIP5
    isrc_3d = isrc(n)
    k_src = int((isrc_3d - 1) / ((iet - ibt + 1) * (jet - jbt + 1))) + 1
    isrc_2d = isrc_3d - (k_src - 1) * (iet - ibt + 1) * (jet - jbt + 1)
    j_src = int((isrc_2d - 1) / (iet - ibt + 1)) + jbt
    i_src = isrc_2d - (j_src - jbt) * (iet - ibt + 1) + ibt - 1
#else /* OGCM_CMIP5 */
    isrc_3d = isrc(n)
    k_src = int((isrc_3d - 1) / (imut * jmut)) + 1
    isrc_2d = isrc_3d - (k_src - 1) * imut * jmut
    j_src = int((isrc_2d - 1) / imut) + 1
    i_src = isrc_2d - (j_src - 1) * imut
#endif /* OGCM_CMIP5 */

!    write(6,*) idst(n), i_dst,j_dst,k_dst
!    write(6,*) isrc(n), i_src,j_src,k_src

    if ((i_src < ibt)  .or. ((iet + 1) < i_src)) then
      write(6,'(1a,4i6)') ' i_src is out of core region : ', i_src, ibt, iet, j_src
    end if
    if ((j_src < jbt)  .or. (jet < j_src)) then
      write(6,'(1a,4i6)') ' j_src is out of core region : ', j_src, jbt, jet, i_src
    end if

    if (scalar(i_src,j_src,k_src) /= undef_in) then
      scalar_received(i_dst,j_dst,k_dst) =        &
           & scalar_received(i_dst,j_dst,k_dst)   &
           & + wgt(n) * scalar(i_src,j_src,k_src)
      area_received(i_dst,j_dst,k_dst) =        &
           & area_received(i_dst,j_dst,k_dst) + wgt(n)
    end if

  end do

  !----------------------------------------------
  ! Average data for North Pole

  if (latg(jmaxg) == 90.0d0) then
    do k = k_start, k_end
      scalar_tmp = 0.0d0
      ivalid = 0
      do i = 1, imaxg
        if (scalar_received(i,jmaxg,k) /= 0.0d0) then
          scalar_tmp = scalar_tmp + scalar_received(i,jmaxg,k)
          ivalid = ivalid + 1
        end if
      end do
      do i = 1, imaxg
        scalar_received(i,jmaxg,k) = scalar_tmp / real(ivalid,8)
        area_received(i,jmaxg,k) = 1.0d0
      end do
    end do
  end if

  !-----------------------------------------------

  do k = k_start, k_end
    do j = 1, jmaxg
      do i = 1, imaxg
        if (area_received(i,j,k) == 0.0d0) then
          scalar_received(i,j,k) = undef_out
        end if
      end do
    end do
  end do

  !----------------------------------------------
  ! 出力ファイルオープン

  open (mtout, file=flout, form='unformatted', access='direct', recl=lrec_out)
  irecw = 0

  write(6,*) 'writing remapped data to', trim(flout)

  do k = k_start, k_end
    dat_out(1:imaxg,1:jmaxg) = real(scalar_received(1:imaxg,1:jmaxg,k),4)
    irecw = irecw + 1
    write(mtout, rec=irecw) dat_out
  end do

  close ( mtout )

  if (l_check_table) then
    do k = k_start, k_end
      dat_out(1:imaxg,1:jmaxg) = real(area_received(1:imaxg,1:jmaxg,k),4)
      irecc = irecc + 1
      write(mtchk, rec=irecc) dat_out
    end do
    close(mtchk)
  end if

  close ( mtstdout )

contains
  !=====================================================================
  subroutine create_remap_table

    implicit none

    integer(4) :: k
    real(8)    :: mu_u, psi_u, lambdau, phiu
    real(8)    :: lon_trn, lat_trn

    integer(4) :: iidv, jjdv
    integer(4) :: ii, jj
    integer(4) :: n

    integer(4) :: nlink, nlink_div, nlink_add
    integer(4) :: nxlink, nylink
    integer(4) :: isrc_div
    integer(4) :: id_3d, id_2d

    integer(4) :: k_layers
    integer(4) :: k_level
    integer(4) :: k_loop_end

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

!    do ii = 1, iidv
!      write(mtstdout,*) ii, alon_div(ii), i_div_t(ii), i_div_u(ii)
!    end do
!    write(mtstdout,*) 'iidv = ', iidv, nxdiv

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

!    do jj = 1, jjdv
!      write(mtstdout,*) jj, alat_div(jj), j_div_t(jj), j_div_u(jj)
!    end do
!    write(mtstdout,*) 'jjdv = ', jjdv, nydiv

    !---------------------------------------------------------------------

    k_layers = k_end - k_start + 1

!    allocate(isrc(nxdiv*nydiv*k_layers))
!    allocate(idst(nxdiv*nydiv*k_layers))
!    allocate(wgt(nxdiv*nydiv*k_layers))

    nxlink = (ieu - ibu + 1) * 8
    nylink = (jeu - jbu + 1) * 8

    write(6,*) ' Max link = ',nxlink*nylink*k_layers
    allocate(isrc(nxlink*nylink*k_layers))
    allocate(idst(nxlink*nylink*k_layers))
    allocate(wgt(nxlink*nylink*k_layers))

    isrc(:) = 0
    idst(:) = 0
    wgt(:) = 0.d0

    nlink = 0
    nlink_add = 0

    area_received(1:imaxg,1:jmaxg,k_start:k_end) = 0.0d0

#ifdef OGCM_BBL
    if (k_end == km) then
      k_loop_end = k_end - 1
    else
      k_loop_end = k_end
    end if
#else /* OGCM_BBL */
    k_loop_end = k_end
#endif /* OGCM_BBL */

    do k = k_start, k_loop_end

      nlink_div = nlink

      write(6,*) 'k = ',k, ' nlink_div = ', nlink_div

      do jjdv = 1, nydiv

!        write(6,*) jjdv ,'/', nydiv
        if (mod(jjdv,120) == 0) then
          write(6,*) jjdv ,'/', nydiv
          write(6,*) alat_div(jjdv)
        end if

        do iidv = 1, nxdiv

!          write(6,*) iidv ,'/', nxdiv

          mu_u = alon_div(iidv)*radian_r
          psi_u = alat_div(jjdv)*radian_r
          call mp2lp(lambdau, phiu, mu_u, psi_u)

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

            if (tuw == 't' .or. tuw == 'w') then

              k_level = k

#ifdef OGCM_CMIP5
              isrc_div = i_div_t(iidv) - ibt + 1 &
                   &  + (j_div_t(jjdv) - jbt) * (iet - ibt + 1) &
                   &  + (k_level - 1) * (iet - ibt + 1) * (jet - jbt + 1)
#else /* OGCM_CMIP5 */
              isrc_div = i_div_t(iidv) + (j_div_t(jjdv) - 1) * imut + (k_level - 1) * imut * jmut
#endif /* OGCM_CMIP5 */
            else
              k_level = k
              isrc_div = i_div_u(iidv) + (j_div_u(jjdv) - 1) * imut + (k_level - 1) * imut * jmut
            end if

            do j = 1, jmaxg

              if ((latg_boundary(j-1) < lat_trn) .and. (lat_trn <= latg_boundary(j))) then

                do i = 1, imaxg

                  if ((long_boundary(i-1) < lon_trn) .and. (lon_trn <= long_boundary(i))) then

                    idst_3d = i + (jmaxg - j) * imaxg + (k_level - 1) * imaxg * jmaxg
                    area_received(i,j,k_level) = area_received(i,j,k_level) + area_tmp

                    ! nlink_div : number of link at the start of the divided data grid loop
                    ! nlink     : number of link at present
                    ! nlink_add : new link ( it not already exist )

                    nlink_add = nlink + 1   !- new link (default)

                    if (nlink > nlink_div) then
!                      write(6,*) nlink, nlink_div, nxdiv*nydiv*k_layers
                      do n = nlink, nlink_div + 1, -1
                        if (idst(n) == idst_3d .and. isrc(n) == isrc_div) then
                          nlink_add = n
                          exit
                        end if
                      end do
                    end if

                    !-- Make link. --
                    if ( nlink_add > nlink ) then
                      nlink = nlink + 1
                      !write(6,*) ' New link (src, dst) ', isrc_div, idst_3d
                      !write(6,*) '                     ', i_div_t(iidv), j_div_t(jjdv), k
                      isrc(nlink) = isrc_div
                      idst(nlink) = idst_3d
                    end if

                    wgt(nlink_add) = wgt(nlink_add) + area_tmp

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

#ifdef OGCM_BBL

    if (k_end == km) then

      k = km

      nlink_div = nlink

      write(6,*) 'k = ',k, ' nlink_div = ', nlink_div

      do jjdv = 1, nydiv

        if (mod(jjdv,100) == 0) then
          write(6,*) jjdv ,'/', nydiv
          write(6,*) alat_div(jjdv)
        end if

        do iidv = 1, nxdiv

          mu_u = alon_div(iidv)*radian_r
          psi_u = alat_div(jjdv)*radian_r
          call mp2lp(lambdau, phiu, mu_u, psi_u)

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

          if (aexlbbl(i_div_u(iidv),j_div_u(jjdv),1) /= 0.0d0) then ! U-point datum is valid

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

            if (tuw == 't' .or. tuw == 'w') then
              k_level = ktbtm(i_div_t(iidv),j_div_t(jjdv))
#ifdef OGCM_CMIP5
              isrc_div = i_div_t(iidv) - ibt + 1 &
                   &  + (j_div_t(jjdv) - jbt) * (iet - ibt + 1) &
                   &  + (k_level - 1) * (iet - ibt + 1) * (jet - jbt + 1)
#else /* OGCM_CMIP5 */
              isrc_div = i_div_t(iidv) + (j_div_t(jjdv) - 1) * imut + (k_level - 1) * imut * jmut
#endif /* OGCM_CMIP5 */
            else
              k_level = kbtm(i_div_u(iidv),j_div_u(jjdv))
              isrc_div = i_div_u(iidv) + (j_div_u(jjdv) - 1) * imut + (k_level - 1) * imut * jmut
            end if

            do j = 1, jmaxg

              if ((latg_boundary(j-1) < lat_trn) .and. (lat_trn <= latg_boundary(j))) then

                do i = 1, imaxg

                  if ((long_boundary(i-1) < lon_trn) .and. (lon_trn <= long_boundary(i))) then

                    idst_3d = i + (jmaxg - j) * imaxg + (k_level - 1) * imaxg * jmaxg
                    area_received(i,j,k_level) = area_received(i,j,k_level) + area_tmp

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

                    !-- Make link. --
                    if ( nlink_add > nlink ) then
                      nlink = nlink + 1
                      !write(6,*) ' New link (src, dst) ', isrc_div, idst_3d
                      !write(6,*) '                     ', i_div_t(iidv), j_div_t(jjdv), k
                      isrc(nlink) = isrc_div
                      idst(nlink) = idst_3d
                    end if

                    wgt(nlink_add) = wgt(nlink_add) + area_tmp

                    exit

                  end if

                end do

                exit

              end if

            end do

          end if

        end do
      end do

    end if
#endif /* OGCM_BBL */

    write(6,*) 'nlink = ', nlink

    do n = 1, nlink
      idst_3d = idst(n)
      k_dst = int((idst_3d  - 1) / (imaxg * jmaxg)) + 1
      idst_2d = idst_3d - (k_dst - 1) * imaxg * jmaxg
      j_dst = jmaxg - int((idst_2d - 1) / imaxg)
      i_dst = idst_2d - (jmaxg - j_dst) * imaxg
!      write(6,*) idst(n), i_dst,j_dst,k_dst
      if (area_received(i_dst,j_dst,k_dst) == 0.0d0) then
        write(6,*) 'erroneous remapping', i_dst,j_dst,k_dst, atexlbbl(i_dst,j_dst,1)
        stop
      else
        wgt(n) = wgt(n) / area_received(i_dst,j_dst,k_dst)
      end if
    end do

    !------

    write(6,'(1a,I8)') 'nlink = ', nlink
    isrc_max = maxval(isrc(1:nlink))
    idst_max = maxval(idst(1:nlink))
    write(6,*) '   isrc_max = ',isrc_max,' / ', imut*jmut*km
    write(6,*) '   idst_max = ',idst_max,' / ', imaxg*jmaxg*km
    num_wgts_all = 1

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
    write(mttbl) wgt (1:nlink)
    close(mttbl)

    if (l_check_table) then
      do k = k_start, k_end
        dat_out(1:imaxg,1:jmaxg) = real(area_received(1:imaxg,1:jmaxg,k),4)
        irecc = irecc + 1
        write(mtchk, rec=irecc) dat_out
      end do
    end if

    deallocate(isrc,idst,wgt)
    deallocate(alon_div, alat_div)
    deallocate(i_div_t, j_div_t)
    deallocate(i_div_u, j_div_u)

  end subroutine create_remap_table
  !=====================================================================
end program remap_scalar
