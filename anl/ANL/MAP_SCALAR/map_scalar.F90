!-*-F90-*-
!map_scalar.F90
!====================================================
program map_scalar

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

  ! interpolated grid

  character(len=16) :: sph_grid
  integer(4), save :: lrec_out
  integer(4) :: j_begin_search
  real(8)          :: undef_in
  character(len=1) :: tuw
  integer(4)       :: id_start, id_end
  integer(4)       :: item_start
  integer(4)       :: item_end
  integer(4)       :: item_total
  integer(4)       :: item_layer
  integer(4)       :: k_start, k_end
  integer(4) :: isrc_3d, isrc_2d, i_src, j_src, k_src

  real(8), allocatable :: scalar(:,:,:)
  real(4),allocatable :: d2_r4(:,:)

  ! original grid

  integer(4), parameter :: ibu = 3, ieu = imut - 2  ! cyclic
  integer(4), parameter :: jbu = 2, jeu = jmut - 3  ! tripolar

  integer(4), parameter :: ibt = 3, iet = imut - 2  ! cyclic
  integer(4), parameter :: jbt = 2, jet = jmut - 2  ! tripolar

  real(8), allocatable :: area_received(:,:,:)
  real(8), allocatable :: scalar_received(:,:,:)
  real(8), parameter :: eps = 1.0d-5
  real(4), allocatable :: dat_out(:,:)
  real(8) :: undef_out
  integer(4) :: idst_3d, idst_2d, i_dst, j_dst, k_dst


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
  logical :: l_check_table = .false.
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
  integer(4), parameter :: mttmp    = 79
  integer(4), parameter :: mtin     = 84
  integer(4), parameter :: mtout    = 86
  integer(4), parameter :: mtstdout = 87

  integer(4) :: irecw, ireci
  integer(4) :: i, j, k, m, n, item
#ifdef OGCM_BBL
  integer(4) :: kbtm
#endif /* OGCM_BBL */

#ifdef OGCM_CYCLIC
  logical, parameter :: lcyclic = .true.
#else /* OGCM_CYCLIC */
  logical, parameter :: lcyclic = .false.
#endif /* OGCM_CYCLIC */

  namelist /nml_map_scl/       &
       &  idiv, jdiv,          &
       &  sph_grid,            &
       &  tuw,                 &
       &  undef_in, undef_out, &
       &  id_start, id_end,    &
       &  item_start,          &
       &  item_end,            &
       &  item_layer,          &
       &  item_total,          &
       &  k_start, k_end,      &
       &  l_read_table,        &
       &  l_check_table,       &
       &  file_rmp_table,      &
       &  file_recv_area,      &
       &  flin, flout,         &
       &  fltopo, flsclf, flstdout
#ifdef OGCM_VARIABLE
  namelist /inflg/ file_vgrid
#endif /* OGCM_VARIABLE */

  !==============================================

  write(*,*) 'slat0', slat0
  write(*,*) 'slon0', slon0

  !---------------------------------------------
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
  read(unit=5, nml_map_scl)

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

  write(6,*) 'flin      :', trim(flin)
  write(6,*) 'undef_in  :', undef_in
  write(6,*) 'item_start:', item_start
  write(6,*) 'k_start   :', k_start
  write(6,*) 'k_end     :', k_end
  write(6,*) 'tuw       :', tuw
  
  write(6,*) 'flout     :', trim(flout)
  write(6,*) 'undef_out :', undef_out

  write(6,*) 'flstdout  :', trim(flstdout)

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
  ! Set original lat-lon grid lattice

  call set_gaussgrid (sph_grid)

  write(mtstdout,*) 'imaxg = ', imaxg
  write(mtstdout,*) 'jmaxg = ', jmaxg

  !----------------------------------------------

  if (.not. l_read_table) then
    call create_map_table_scl_bilin ! bi-linear interpolation
  end if

  write(6,*) ' Reading table from ...',trim(file_rmp_table)
  open(mttbl,file=file_rmp_table,form='unformatted',action='read')

  read(mttbl) isrc_max
  read(mttbl)
  read(mttbl)
  read(mttbl) idst_max
  read(mttbl)
  read(mttbl)

  write(6,*) '   isrc_max = ',isrc_max,' / ', imaxg * jmaxg * km
  write(6,*) '   idst_max = ',idst_max,' / ', imut  * jmut  * km

  read(mttbl) num_wgts_all, nlink

  write(6,'(1a,I8)') 'nlink = ', nlink

  allocate(isrc(1:nlink))
  allocate(idst(1:nlink))
  allocate(wgt (1:nlink))

  read(mttbl) isrc(1:nlink)
  read(mttbl) idst(1:nlink)
  read(mttbl) wgt (1:nlink)

  close(mttbl)

  !-------------------------------------------------
  ! open input file

  allocate(scalar(1:imaxg,1:jmaxg,k_start:k_end))
  allocate(d2_r4(1:imaxg,1:jmaxg))

  open (mtin, file=flin, form='unformatted', access='direct', recl=4*imaxg*jmaxg)

  write(6,*) 'reading original data from', trim(flin)

  !-------------------------------------------------
  ! open output file

  allocate(dat_out(1:imut,1:jmut))
  lrec_out = imut*jmut*4

  open (mtout, file=flout, form='unformatted', access='direct', recl=lrec_out)
  irecw = 0

  write(6,*) 'writing remapped data to', trim(flout)

  allocate(area_received(1:imut,1:jmut,k_start:k_end))
  allocate(scalar_received(1:imut,1:jmut,k_start:k_end))

  !-------------------------------------------------

  LOOP_DATA: do m = id_start, id_end

  write(6,*) 'Processing ',m,'-th record'

  !-----------

  do item = item_start, item_end

    ! read data

    do k = k_start, k_end
      ireci = item_layer * item_total * (m - 1) + item_layer * (item - 1) + k
      write(6,*) 'reading record ', ireci
      read (mtin, rec=ireci) d2_r4
      scalar(:,:,k) = real(d2_r4(:,:),8)
    end do

    !----------------------------------------------

    scalar_received(1:imut,1:jmut,k_start:k_end) = 0.0d0
    area_received(1:imut,1:jmut,k_start:k_end) = 0.0d0

    do n = 1, nlink

      isrc_3d = isrc(n)
      k_src = int((isrc_3d - 1) / (imaxg * jmaxg)) + 1
      isrc_2d = isrc_3d - (k_src - 1) * imaxg * jmaxg
      j_src = jmaxg - int((isrc_2d - 1) / imaxg)
      i_src = isrc_2d - (jmaxg - j_src) * imaxg

#ifdef OGCM_CMIP5
      idst_3d = idst(n)
      k_dst = int((idst_3d - 1) / ((iet - ibt + 1) * (jet - jbt + 1))) + 1
      idst_2d = idst_3d - (k_dst - 1) * (iet - ibt + 1) * (jet - jbt + 1)
      j_dst = int((idst_2d - 1) / (iet - ibt + 1)) + jbt
      i_dst = idst_2d - (j_dst - jbt) * (iet - ibt + 1) + ibt - 1
#else /* OGCM_CMIP5 */
      idst_3d = idst(n)
      k_dst = int((idst_3d - 1) / (imut * jmut)) + 1
      idst_2d = idst_3d - (k_dst - 1) * imut * jmut
      j_dst = int((idst_2d - 1) / imut) + 1
      i_dst = idst_2d - (j_dst - 1) * imut
#endif /* OGCM_CMIP5 */

      if (wgt(n) > 0.0d0 .and. scalar(i_src,j_src,k_src) /= undef_in) then

        scalar_received(i_dst,j_dst,k_dst) =        &
             & scalar_received(i_dst,j_dst,k_dst)   &
             & + wgt(n) * scalar(i_src,j_src,k_src)
        area_received(i_dst,j_dst,k_dst) =        &
             & area_received(i_dst,j_dst,k_dst) + wgt(n)
      end if

    end do

#ifdef OGCM_CYCLIC
    do k = k_start, k_end
      do j = 1, jmut
        area_received(1,j,k)       = area_received(iet-1,j,k)
        area_received(2,j,k)       = area_received(iet  ,j,k)
        area_received(iet+1,j,k)   = area_received(ibt  ,j,k)
        area_received(iet+2,j,k)   = area_received(ibt+1,j,k)
        scalar_received(1,j,k)     = scalar_received(iet-1,j,k)
        scalar_received(2,j,k)     = scalar_received(iet  ,j,k)
        scalar_received(iet+1,j,k) = scalar_received(ibt  ,j,k)
        scalar_received(iet+2,j,k) = scalar_received(ibt+1,j,k)
      end do
    end do
#endif /* OGCM_CYCLIC */

#if defined OGCM_TRIPOLAR || defined OGCM_JOT
    do k = k_start, k_end
      do i = 2, imut
        area_received(i,jet+1,k)   = area_received(imut-i+2,jet-1,k)
        area_received(i,jet+2,k)   = area_received(imut-i+2,jet-2,k)
        scalar_received(i,jet+1,k) = scalar_received(imut-i+2,jet-1,k)
        scalar_received(i,jet+2,k) = scalar_received(imut-i+2,jet-2,k)
      end do
      do i = imut/2 + 2, imut
        area_received(i,jet,k)   = area_received(imut-i+2,jet,k)
        scalar_received(i,jet,k) = scalar_received(imut-i+2,jet,k)
      end do
    end do
#endif /* OGCM_TRIPOLAR || OGCM_JOT */

    do k = k_start, k_end
      do j = 1, jmut
        do i = 1, imut
          if (area_received(i,j,k) == 0.0d0) then
            scalar_received(i,j,k) = undef_out
          end if
        end do
      end do
    end do

    !----------------------------------------------
    
    do k = k_start, k_end
      dat_out(1:imut,1:jmut) = real(scalar_received(1:imut,1:jmut,k),4)
      irecw = irecw + 1
      write(6,*) 'writing record ', irecw
      write(mtout, rec=irecw) dat_out
    end do

  end do

  end do LOOP_DATA

  close ( mtin )

  deallocate(d2_r4)

  close ( mtout )

  !----------------------------------------------

  if (l_check_table) then

    open(mttmp,file=file_recv_area,form='unformatted',access='direct',action='write',recl=lrec_out)
    irecw = 0
    write(6,*) ' Output received area to ...', trim(file_recv_area)
    do k = k_start, k_end
      dat_out(1:imut,1:jmut) = real(area_received(1:imut,1:jmut,k),4)
      irecw = irecw + 1
      write(mttmp, rec=irecw) dat_out
    end do

    close(mttmp)

  end if

  deallocate(dat_out)

  close ( mtstdout )

contains

  !=====================================================================
  subroutine create_map_table_scl_bilin

    implicit none

    real(8)    :: mu_u, psi_u, lambdau, phiu
    real(8)    :: lon_trn, lat_trn
    real(8)    :: rot_cos, rot_sin

    real(8),allocatable :: long_search(:)
    real(8),allocatable :: latg_search(:)

    integer(4),allocatable :: i_org(:), j_org(:)

    integer(4) :: ii, jj
    integer(4) :: iii, jjj

    integer(4) :: k
    integer(4) :: n, ndim

    integer(4) :: nlink, num_max_link

    integer(4) :: k_levels

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

    num_max_link = 16*imut*jmut*k_levels 

    allocate(isrc(num_max_link))
    allocate(idst(num_max_link))
    allocate(wgt(num_max_link))

    isrc(:) = 0
    idst(:) = 0
    wgt (:) = 0.d0

    nlink = 0

    do k = k_start, k_end

      do j = jbt, jet

        write(6,*) j ,'/', jet
        write(6,*) 'Ocean model latitude', alatt(j)

        do i = ibt, iet

          if (atexl(i,j,k) /= 0.0d0) then ! T-point datum is valid

            mu_u  = alont(i) * radian_r
            psi_u = alatt(j) * radian_r

            ! model (mu, psi) => lon-lat(labmda, phi)
            call mp2lp(lambdau, phiu, mu_u, psi_u)

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
              write(6,*) lat_trn, alont(i), alatt(j)
              stop
            end if

            ! create link, mapping table

#ifdef OGCM_CMIP5
            idst_3d = i - ibt + 1 &
                 &  + (j - jbt) * (iet - ibt + 1) &
                 &  + (k - 1) * (iet - ibt + 1) * (jet - jbt + 1)
#else /* OGCM_CMIP5 */
            idst_3d = i + (j - 1) * imut + (k - 1) * imut * jmut
#endif /* OGCM_CMIP5 */

            do jj = 0, jmaxg

              if ((latg_search(jj) < lat_trn) .and. (lat_trn < latg_search(jj+1))) then

                j_src = jj

                do ii = 1, imaxg

                  if ((long_search(ii) <= lon_trn) .and. (lon_trn < long_search(ii+1))) then

                    i_src = ii

                    if ((1 <= j_src) .and. (j_src <= (jmaxg - 1))) then ! bilinear

                      do jjj = j_src, j_src + 1
                        do iii = i_src, i_src + 1

                          weightx = abs(lon_trn - long_search(iii)) / (long_search(i_src+1) - long_search(i_src))
                          weighty = abs(lat_trn - latg_search(jjj)) / (latg_search(j_src+1) - latg_search(j_src))

                          isrc_3d = i_org(iii) + (jmaxg - j_org(jjj)) * imaxg + (k - 1) * imaxg * jmaxg

                          nlink = nlink + 1
                          !write(6,*) ' New link (src, dst) ', isrc_div, idst_3d
                          !write(6,*) '                     ', i_div_t(iidv), j_div_t(jjdv), k
                          isrc(nlink) = isrc_3d
                          idst(nlink) = idst_3d
                          wgt (nlink) = (1.0d0 - weightx) * (1.0d0 - weighty)

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

                        nlink = nlink + 1
                        !write(6,*) ' New link (src, dst) ', isrc_div, idst_3d
                        !write(6,*) '                     ', i_div_t(iidv), j_div_t(jjdv), k
                        isrc(nlink) = isrc_3d
                        idst(nlink) = idst_3d
                        wgt(nlink) = (1.0d0 - weightx)

!                        write(6,*) 'linear', wgt(1,nlink), wgt(2,nlink), wgt(3,nlink)

                      end do
                      
                    end if

                    exit

                  end if

                end do

                exit
                
              else if (latg_search(0) >= lat_trn) then

                write(6,*) ' South Pole', i, j

                do ii = 1, imaxg
                  isrc_3d = ii + (jmaxg - 1) * imaxg + (k - 1) * imaxg * jmaxg
                  nlink = nlink + 1
                  isrc(nlink) = isrc_3d
                  idst(nlink) = idst_3d
                  wgt(nlink) = 1.0d0 / real(imaxg,8)
                end do

                exit

              else if (lat_trn >= latg_search(jmaxg+1)) then

                write(6,*) ' North Pole', i, j
                do ii = 1, imaxg
                  isrc_3d = ii + (k - 1) * imaxg * jmaxg
                  nlink = nlink + 1
                  isrc(nlink) = isrc_3d
                  idst(nlink) = idst_3d
                  wgt(nlink) = 1.0d0 / real(imaxg,8)
                end do

                exit

              else if (lat_trn == latg_search(jj)) then ! linear

                j_src = jj

                do ii = 1, imaxg

                  if ((long_search(ii) <= lon_trn) .and. (lon_trn < long_search(ii+1))) then

                    i_src = ii

                    do iii = i_src, i_src + 1

                      weightx = abs(lon_trn - long_search(iii)) / (long_search(i_src+1) - long_search(i_src))

                      isrc_3d = i_org(iii) + (jmaxg - j_org(j_src)) * imaxg + (k - 1) * imaxg * jmaxg

                      nlink = nlink + 1
                      isrc(nlink) = isrc_3d
                      idst(nlink) = idst_3d
                      wgt(nlink) = (1.0d0 - weightx)

                    end do
                      
                    exit

                  end if

                end do

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
    num_wgts_all = 1

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
    write(mttbl) wgt (1:nlink)
    close(mttbl)

    deallocate(isrc,idst,wgt)

    deallocate(long_search)
    deallocate(i_org)
    deallocate(latg_search)
    deallocate(j_org)

  end subroutine create_map_table_scl_bilin
  !=====================================================================
end program map_scalar
