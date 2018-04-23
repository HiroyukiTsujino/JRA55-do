! -*-F90-*-
!
! module for analysis
!
!        2010.07.28 -    M. Hirabara
!
!=====================================================================
!
module oc_structure

  use oc_mod_param

  use libmxe_grid, only: type_libmxe_grid
  use libmxe_topo, only: type_libmxe_topo

  implicit none

  public

  type(type_libmxe_grid),save :: grid
  type(type_libmxe_topo),save :: topo

  ! constants given in set_hgrids

  real(8), allocatable :: dxtdeg(:), dytdeg(:), dxudeg(:), dyudeg(:)

  real(8), allocatable :: alatt(:), alatu(:), alont(:), alonu(:)

  real(8), allocatable :: glatt(:,:), glatu(:,:), glont(:,:), glonu(:,:)

  real(8), allocatable :: alont_mod(:), alatt_mod(:)
  real(8), allocatable :: alonu_mod(:), alatu_mod(:)

!  real(8), allocatable :: dxt(:), dxu(:), dyt(:), dyu(:)
!  real(8), allocatable :: dxtr(:), dxur(:), dytr(:), dyur(:)

  real(8),save :: slat, slon

  ! constants given in read_topo -------------------------------
  !
  !  ho4   : 海底の深さ、単位cm、整数
  !  exnn  : UV点で何層目までが海なのかを示す。
  !  texnn : TS点で何層目までが海なのかを示す。
  !
  !  T点, U点からひとつ上の点までの距離
  real(8), allocatable :: dz_cm(:)
  real(8), allocatable :: dzz(:)
  real(8), allocatable :: dzzr(:) !  dzz の逆数
  real(8), allocatable :: dzr(:)  !  dz の逆数
  real(8), allocatable :: dp(:)   !  T点, U点の深さ
  !  T点, U点の上部境界の深さ
  real(8), allocatable :: dep(:)
  !
  integer(4), allocatable :: ho4(:,:), exnn(:,:), texnn(:,:)
  integer(4), allocatable :: ho4bbl(:,:), exnnbbl(:,:), texnnbbl(:,:)
  !
  real(8), allocatable :: dzu(:,:,:)   ! UV点の層厚
  !
  real(8), allocatable :: aexl(:,:,:)  ! UV点の海陸インデックス
  real(8), allocatable :: atexl(:,:,:) ! TS点の海陸インデックス
  !
  real(8), allocatable :: coefx(:,:,:)   ! X点(TS点の東)海陸インデックス
  real(8), allocatable :: coefy(:,:,:)   ! Y点(TS点の北)海陸インデックス
  real(8), allocatable :: aexlbbl(:,:,:)  ! UV点の海陸インデックス(BBL)
  real(8), allocatable :: atexlbbl(:,:,:) ! TS点の海陸インデックス(BBL)
  !
  integer(4),allocatable :: kbtm(:,:)  ! （内部の）海底の格子番号k (UV)
  integer(4),allocatable :: ktbtm(:,:) ! （内部の）海底の格子番号k (TS)
  !
  real(8),allocatable :: dsgm(:) ! sigma layer 内における各層の割合 (無次元量）
                        ! \sum_{i=1}^{ksgm} dsgm(i) = 1 であり，初期のdzから自動的に決定される
  real(8),save :: thcksgm    ! 初期のsigma layer 全体の厚さ 
  real(8),save :: thcksgmr   ! 1/thcksgm
  !
  ! constants given in read_scale ------------------------------
  !
  real(8), allocatable :: a_tl(:,:),  a_tr(:,:)  ! area (quarter of mesh)
  real(8), allocatable :: a_bl(:,:),  a_br(:,:)
  real(8), allocatable :: dx_tl(:,:), dx_tr(:,:) ! length(bottom)
  real(8), allocatable :: dx_bl(:,:), dx_br(:,:)
  real(8), allocatable :: dy_tl(:,:), dy_tr(:,:) ! length(left)
  real(8), allocatable :: dy_bl(:,:), dy_br(:,:)
  !
  ! constants given in set_area --------------------------------
  !
  real(8), allocatable :: areat(:,:,:) ! TS点の面積
  real(8), allocatable :: areau(:,:,:) ! UV点の面積
  real(8), allocatable :: nendidx(:)
  !
  ! constants given in set_volt --------------------------------
  !
  real(8), allocatable :: volt(:,:,:)
  !
  ! constants given in set_volu --------------------------------
  !
  real(8), allocatable :: volu(:,:,:)
  !
contains !============================================================

  subroutine structure_mxe__ini

    use libmxe_grid, only: &
         &    libmxe_grid__register
    use libmxe_topo, only: &
         &    libmxe_topo__register, &
         &    libmxe_topo__aexl,     &
         &    libmxe_topo__dz3d


    integer(4) :: i, j, k
    real(8)    :: hl1

    !------------------------------------------------------------------
    ! GRID

    write(6,*) ' Registering GRID '
    call libmxe_grid__register(grid,para)
    write(6,*) ' ..... done '

    SLAT0 = grid%lat_south_end_of_core
    SLON0 = grid%lon_west_end_of_core

    NPLAT = grid%north_pole_lat
    NPLON = grid%north_pole_lon
    SPLAT = grid%south_pole_lat
    SPLON = grid%south_pole_lon

    allocate( dxtdeg(imut) )
    allocate( dytdeg(jmut) )
    allocate( dxudeg(imut) )
    allocate( dyudeg(jmut) )
    allocate( alatt(jmut) )
    allocate( alatu(jmut) )
    allocate( alont(imut) )
    allocate( alonu(imut) )

    allocate(glatt(1:imut,1:jmut))
    allocate(glatu(1:imut,1:jmut))
    allocate(glont(1:imut,1:jmut))
    allocate(glonu(1:imut,1:jmut))

    dxtdeg(1:imut) = grid%dxtdeg(1:imut)
    dytdeg(1:jmut) = grid%dytdeg(1:jmut)

    if (para%lsub) then
      !slat = slat0 - dytdeg(1) - dytdeg(2)
      !slon = slon0 - dxtdeg(1) - dxtdeg(2)

      !  This is different from MRI.COM gridm.F90, but use this for the case of dytdeg(1) /= dytdeg(2)
      slat = slat0 - 0.5d0 * dytdeg(1) - dytdeg(2) - 0.5d0 * dytdeg(3)
      slon = slon0 - 0.5d0 * dxtdeg(1) - dxtdeg(2) - 0.5d0 * dxtdeg(3)
    else
      !slat = slat0 - dytdeg(1)

      !  This is different from MRI.COM gridm.F90, but use this for the case of dytdeg(1) /= dytdeg(2)
      slat = slat0 - 0.5d0 * dytdeg(1) - 0.5d0 * dytdeg(2)
      if (para%lcyclic) then
        !slon = slon0 - 2.0D0*dxtdeg(1)

        !  This is different from MRI.COM gridm.F90, but use this for the case of dxtdeg(1) /= dxtdeg(2)
        slon = slon0 - 0.5d0 * dxtdeg(1) - dxtdeg(2) - 0.5d0 * dxtdeg(3)
      else
        !slon = slon0 - dxtdeg(1)

        !  This is different from MRI.COM gridm.F90, but use this for the case of dytdeg(1) /= dytdeg(2)
        slon = slon0 - 0.5d0 * dxtdeg(1) - 0.5d0 * dxtdeg(2)
      end if
    end if

    do i = 1, imut - 1
      dxudeg(i) = 0.5d0*(dxtdeg(i)+dxtdeg(i+1))
    end do
    if (para%lcyclic) then
      dxudeg(imut) = dxudeg(4)
    else
      dxudeg(imut) = dxtdeg(imut)
    end if

    alont(1) = slon
    alonu(1) = slon + 0.5d0*dxtdeg(1)
    do i = 2, imut
      alont(i) = alont(i-1) + dxudeg(i-1)
      alonu(i) = alonu(i-1) + dxtdeg(i)
    end do

    do j = 1, jmut - 1
      dyudeg(j) = 0.5d0*(dytdeg(j)+dytdeg(j+1))
    end do
    dyudeg(jmut) = dytdeg(jmut)

    alatt(1) = slat
    alatu(1) = slat + 0.5d0*dytdeg(1)
    do j = 2, jmut
      alatt(j) = alatt(j-1) + dyudeg(j-1)
      alatu(j) = alatu(j-1) + dytdeg(j)
    end do

    write(6,*) slat, slat0, dytdeg(1)
    do j = 1, jmut
      write(6,*) j, alatt(j), alatu(j)
    end do

    if (para%ltripolar) then
      do j = 2, jmut-1
        if ( alatu(j+1) > nplat .and. alatu(j) < nplat) then
          alatt(j+1) = nplat
          dyudeg(j)   = alatt(j)  -alatt(j-1)
          dyudeg(j+1) = alatt(j+1)-alatt(j)
          exit
        endif
      end do
    end if

    !----------------------------------
    ! for easy monitoring

    allocate(alont_mod(1:imut), alatt_mod(1:jmut))
    allocate(alonu_mod(1:imut), alatu_mod(1:jmut))

    if (para%ltripolar) then
      alont_mod(:) = alont(:) + nplon
      alonu_mod(:) = alonu(:) + nplon
    else
      alont_mod(:) = alont(:)
      alonu_mod(:) = alonu(:)
    endif

    if (para%ltripolar) then
      do j = 1, jmut
        if ( alatu(j) >=  nplat ) then
          alatu_mod(j) = ( alatu(j) - nplat ) * ( 90.d0 - nplat ) / 90.d0 + nplat
        else
          alatu_mod(j) = alatu(j)
        endif
        if ( alatt(j) >=  nplat ) then
          alatt_mod(j) = ( alatt(j) - nplat ) * ( 90.d0 - nplat ) / 90.d0 + nplat
        else
          alatt_mod(j) = alatt(j)
        endif
      end do
      !- modify latitude if it >= 90.d0.
      do j = jmut, 1, -1
        if ( alatt_mod(j) >=  90.d0 ) then
          alatt_mod(j) = 90.d0 - 0.00001d0 * ( jmut - j + 1 )
        else
          exit
        endif
      enddo
      do j = jmut, 1, -1
        if ( alatu_mod(j) >=  90.d0 ) then
          alatu_mod(j) = 90.d0 - 0.00001d0 * ( jmut - j + 1 )
        else
          exit
        endif
      enddo
    else
      alatt_mod(:) = alatt(:)
      alatu_mod(:) = alatu(:)
    end if

    glatt(1:imut,1:jmut) = grid%glatt(1:imut,1:jmut)
    glatu(1:imut,1:jmut) = grid%glatu(1:imut,1:jmut)
    glont(1:imut,1:jmut) = grid%glont(1:imut,1:jmut)
    glonu(1:imut,1:jmut) = grid%glonu(1:imut,1:jmut)

!    allocate( dxt(imut) )
!    allocate( dxu(imut) )
!    allocate( dyt(jmut) )
!    allocate( dyu(jmut) )
!    allocate( dxtr(imut) )
!    allocate( dxur(imut) )
!    allocate( dytr(jmut) )
!    allocate( dyur(jmut) )

    allocate( dzz(km+1) )
    allocate( dzzr(km+1) )
    allocate( dzr(km) )
    allocate( dp(km) )
    allocate( dep(km+1) )
    allocate( dsgm(ksgm) )
    allocate( dz_cm(km) )

    !-----------------------------------------------------------------

    dz_cm(1:km) = grid%dz_cm(1:km)
    !
    thcksgm=0.d0                 !  sigma layer thickness
    do k = 1, ksgm
      thcksgm = thcksgm +dz(k)
    end do
    thcksgmr = 1.d0 / thcksgm
    !
    dzz(1) = 0.5d0 * dz(1)       !  鉛直方向に隣り合った格子の距離
    do k = 2, km
      dzz(k) = 0.5d0 * (dz(k-1) + dz(k))
    end do
    dzz(km+1) = 0.5d0 * dz(km)
    !
    do k = 1, km
      dzr (k) = 1.D0 / dz (k)    !  DZ の逆数
      dzzr(k) = 1.D0 / dzz(k)    !  DZZ の逆数
    end do
    dzzr(km+1) = 1.D0 / dzz(km+1)
    !
    dp(1) = dzz(1)               !  T点, U点の深さ
    do k = 2, km
      dp(k) = dp(k-1) + dzz(k)
    end do
    !
    dep(1)=0.d0                  !  upper boundary of T/U-box
    do k=2, km + 1
      dep(k)=dep(k-1)+dz(k-1)
    end do
    !
    do k = 1, ksgm
      dsgm(k) = dz(k) * thcksgmr
    end do

    !----------------------------------

    allocate( a_bl(imut, jmut) )
    allocate( a_br(imut, jmut) )
    allocate( a_tl(imut, jmut) )
    allocate( a_tr(imut, jmut) )

    allocate( dx_bl(imut, jmut) )
    allocate( dx_br(imut, jmut) )
    allocate( dx_tl(imut, jmut) )
    allocate( dx_tr(imut, jmut) )

    allocate( dy_bl(imut, jmut) )
    allocate( dy_br(imut, jmut) )
    allocate( dy_tl(imut, jmut) )
    allocate( dy_tr(imut, jmut) )

    a_bl(:,:) = grid%a_bl(:,:)
    a_br(:,:) = grid%a_br(:,:)
    a_tl(:,:) = grid%a_tl(:,:)
    a_tr(:,:) = grid%a_tr(:,:)
    dx_bl(:,:) = grid%dx_bl(:,:)
    dx_br(:,:) = grid%dx_br(:,:)
    dx_tl(:,:) = grid%dx_tl(:,:)
    dx_tr(:,:) = grid%dx_tr(:,:)
    dy_bl(:,:) = grid%dy_bl(:,:)
    dy_br(:,:) = grid%dy_br(:,:)
    dy_tl(:,:) = grid%dy_tl(:,:)
    dy_tr(:,:) = grid%dy_tr(:,:)

#ifdef OGCM_PLANE
    if (para%lspherical) then
      do j = 1, jmut-1
        do i = 1, imut-1
          dx_bl(i,j) = 0.5d0 * radius * dxtdeg(i) * radian_r * cos(base_lat*radian_r)
          dy_bl(i,j) = 0.5d0 * radius * dytdeg(j) * radian_r
          a_bl(i,j)  = dx_bl(i,j) * dy_bl(i,j)

          dx_br(i,j) = 0.5d0 * radius * dxtdeg(i+1) * radian_r * cos(base_lat*radian_r)
          dy_br(i,j) = 0.5d0 * radius * dytdeg(j) * radian_r
          a_br(i,j)  = dx_br(i,j) * dy_br(i,j)

          dx_tl(i,j) = 0.5d0 * radius * dxtdeg(i) * radian_r * cos(base_lat*radian_r)
          dy_tl(i,j) = 0.5d0 * radius * dytdeg(j+1) * radian_r
          a_tl(i,j)  = dx_tl(i,j) * dy_tl(i,j)
  
          dx_tr(i,j) = 0.5d0 * radius * dxtdeg(i+1) * radian_r * cos(base_lat*radian_r)
          dy_tr(i,j) = 0.5d0 * radius * dytdeg(j+1) * radian_r
          a_tr(i,j)  = dx_tr(i,j) * dy_tr(i,j)
        end do
        if (para%lcyclic) then
          dx_bl(imut,j) = dx_bl(4,j)
          dx_br(imut,j) = dx_br(4,j)
          dx_tl(imut,j) = dx_tl(4,j)
          dx_tr(imut,j) = dx_tr(4,j)
          dy_bl(imut,j) = dy_bl(4,j)
          dy_br(imut,j) = dy_br(4,j)
          dy_tl(imut,j) = dy_tl(4,j)
          dy_tr(imut,j) = dy_tr(4,j)
          a_bl(imut,j)  = a_bl(4,j)
          a_br(imut,j)  = a_br(4,j)
          a_tl(imut,j)  = a_tl(4,j)
          a_tr(imut,j)  = a_tr(4,j)
        end if
      end do
    end if
#endif /* OGCM_PLANE */

    !------------------------------------------------------------------
    ! TOPOGRAPHY

    write(6,*) ' Registering TOPOGRAPHY '

    call libmxe_topo__register(topo,para)
    call libmxe_topo__aexl(topo,para)
    call libmxe_topo__dz3d(topo,para)

    write(6,*) ' ..... done '

    allocate(ho4(imut, jmut))
    allocate(exnn(imut, jmut))
    allocate(texnn(imut, jmut))

    allocate(aexl(imut, jmut, km))
    allocate(atexl(imut, jmut, km))

    allocate(coefx(imut, jmut, km))
    allocate(coefy(imut, jmut, km))

    if (para%lbbl) then
      allocate(ho4bbl(imut, jmut))
      allocate(exnnbbl(imut, jmut))
      allocate(texnnbbl(imut, jmut))
      allocate(aexlbbl(imut, jmut, kbbl))
      allocate(atexlbbl(imut, jmut, kbbl))
    end if

    allocate(dzu(imut, jmut, km))

    allocate(kbtm(imut, jmut))
    allocate(ktbtm(imut, jmut))

    !-----------------------

    ho4(1:imut,1:jmut) = topo%ho4(1:imut,1:jmut)
    exnn(1:imut,1:jmut) = topo%exnn(1:imut,1:jmut)

    if (para%lbbl) then
      ho4bbl(1:imut,1:jmut) = topo%ho4bbl(1:imut,1:jmut)
      exnnbbl(1:imut,1:jmut) = topo%exnnbbl(1:imut,1:jmut)
    end if

    !-----------------------

    aexl(1:imut, 1:jmut, 1:km) = topo%aexl(1:imut, 1:jmut, 1:km)
    dzu(1:imut, 1:jmut, 1:km) = topo%dzu(1:imut, 1:jmut, 1:km)

    texnn(1:imut,1:jmut) = topo%texnn(1:imut,1:jmut)

    ! NOTE: atexl for interior BBL is zero (this is unity in MRI.COM)

    atexl(1:imut, 1:jmut, 1:km) = topo%atexl(1:imut, 1:jmut, 1:km)

    if (para%lbbl) then

      texnnbbl(1:imut, 1:jmut) = topo%texnnbbl(1:imut, 1:jmut)
      aexlbbl(1:imut, 1:jmut, 1:kbbl) = 0.d0

      do k = 1, kbbl
        do j = 1, jmut
          do i = 1, imut
            if (exnnbbl(i,j) >= k) aexlbbl(i, j, k) = 1.d0
          end do
        end do
      end do

      do j = 2, jmut
        do i = 2, imut
          atexlbbl(i,j, 1:kbbl) = max(aexlbbl(i-1,j  , 1:kbbl), aexlbbl(i,j  , 1:kbbl),  &
               &                      aexlbbl(i-1,j-1, 1:kbbl), aexlbbl(i,j-1, 1:kbbl)  )
        end do
      end do

      if (para%lcyclic) then
        atexlbbl(1, 2:jmut, 1:kbbl) = atexlbbl(imut-3, 2:jmut, 1:kbbl)
      end if

      do j = 1, jmut
        do i = 1, imut
          kbtm(i,j) = int(aexl(i,j,1),4)*(exnn(i,j) + exnnbbl(i,j)) + 1 - int(aexl(i,j,1),4)
        end do
      end do

      do j = 1, jmut
        do i = 1, imut
          ktbtm(i,j) = int(atexl(i,j,1),4)*(texnn(i,j) + texnnbbl(i,j)) + 1 - int(atexl(i,j,1),4)
        end do
      end do

    else

      do j = 1, jmut
        do i = 1, imut
          kbtm(i,j) = aexl(i,j,1) * exnn(i,j) + 1 - aexl(i,j,1)
        end do
      end do

      do j = 1, jmut
        do i = 1, imut
          ktbtm(i,j) = int(atexl(i,j,1),4)*texnn(i,j) + 1 - int(atexl(i,j,1),4)
        end do
      end do

    end if

    !     COEFX: TS格子の東側に海があれば 1, 陸なら 0

    coefx(1:imut, 1:jmut, 1:km) =0.d0
    do k = 1, km
      do j = 2, jmut
        do i = 1, imut
          coefx(i,j,k) = max(aexl(i,j,k),aexl(i,j-1,k))
        end do
      end do
    end do

    if (para%lfoldnp) then
      do i = 3, imut-2
        coefx(i, jmut  , 1:km) = coefx(imut-i, jmut-4, 1:km)
        coefx(i, jmut-1, 1:km) = coefx(imut-i, jmut-3, 1:km)
      end do
    end if

    if (para%lcyclic) then
      coefx(1:2        , 1:jmut, 1:km) = coefx(imut-3:imut-2, 1:jmut, 1:km)
      coefx(imut-1:imut, 1:jmut, 1:km) = coefx(3:4          , 1:jmut, 1:km)
    end if

    !     COEFY: TS格子の北側に海があれば 1, 陸なら 0

    coefy(1:imut, 1:jmut, 1:km) =0.d0
    do k = 1, km
      do j = 1, jmut
        do i = 2, imut
          coefy(i,j,k) = max(aexl(i,j,k),aexl(i-1,j,k))
        end do
      end do
    end do

    if (para%lfoldnp) then
      do i = 3, imut-2
        coefy(i, jmut  , 1:km) = coefy(imut-i+2, jmut-5, 1:km)
        coefy(i, jmut-1, 1:km) = coefy(imut-i+2, jmut-4, 1:km)
      end do
    end if

    if (para%lcyclic) then
      coefy(1:2        , 1:jmut, 1:km) = coefy(imut-3:imut-2, 1:jmut, 1:km)
      coefy(imut-1:imut, 1:jmut, 1:km) = coefy(3:4          , 1:jmut, 1:km)
    end if

    write(6,*) ' Exiting structure_mxe__ini '

  end subroutine structure_mxe__ini
  !=====================================================================
  subroutine set_area_t

    integer(4) :: i, j, k

    !-----------------------------

    allocate( areat(imut, jmut, km) )
    areat(1:imut, 1:jmut, 1:km) = 0.0d0

    do k = 1, km
      do j = 2, jmut
        do i = 2, imut
          areat(i, j, k) = atexl(i, j, k) *                                &
               & ( aexl(i-1,j  ,k) *a_br(i-1,j  ) +aexl(i,j  ,k) *a_bl(i,j  ) &
               &  +aexl(i-1,j-1,k) *a_tr(i-1,j-1) +aexl(i,j-1,k) *a_tl(i,j-1))
        end do
      end do
    end do

    if (para%lcyclic) then
      areat(1,1:jmut,1:km) = areat(imut-4,1:jmut,1:km)
    end if

    allocate( nendidx(jmut) )
    nendidx(1:jmut) = 1.d0
    if (para%lfoldnp) then
      nendidx(jmut-2) = 0.5d0
    end if

  end subroutine set_area_t
  !=====================================================================
  subroutine set_area_u
    !
    integer(4) :: i, j, k
    !
    !-----------------------------
    !
    allocate( areau(imut, jmut, km) )
    areau(1:imut, 1:jmut, 1:km) = 0.0d0

    do k = 1, km
      do j = 1, jmut
        do i = 1, imut
          areau(i, j, k) = aexl(i, j, k) *                                &
               & (a_bl(i,j) + a_br(i,j) + a_tl(i,j) + a_tr(i,j))
        end do
      end do
    end do
    
    allocate( nendidx(jmut) )
    nendidx(1:jmut) = 1.d0

  end subroutine set_area_u
  !=====================================================================
  subroutine set_volt
    !
    integer(4) :: i, j, k
    !
    !-----------------------------
    !
    allocate( volt(imut, jmut, km) )
    volt(1:imut, 1:jmut, 1:km) = 0.0d0
    !
    do k = 1, km
      do j = 2, jmut
        do i = 2, imut
          volt(i, j, k) = atexl(i, j, k) *                               &
               & ( dzu(i-1,j  ,k) *a_br(i-1,j  ) +dzu(i,j  ,k) *a_bl(i,j  ) &
               &  +dzu(i-1,j-1,k) *a_tr(i-1,j-1) +dzu(i,j-1,k) *a_tl(i,j-1))
        end do
      end do
    end do
      
    if (para%lcyclic) then
      volt(1,1:jmut,1:km) = volt(imut-4,1:jmut,1:km)
    end if
      
  end subroutine set_volt
  !=====================================================================
  subroutine set_volu
    !
    integer(4) :: i, j, k
    !
    !-----------------------------
    !
    allocate( volu(imut, jmut, km) )
    volu(1:imut, 1:jmut, 1:km) = 0.0d0
    !
    do k = 1, km
      do j = 1, jmut
        do i = 1, imut
          volu(i,j,k) = aexl(i,j,k) * dzu(i,j,k)         &
               & * (a_bl(i,j) + a_br(i,j) + a_tl(i,j) + a_tr(i,j))
        end do
      end do
    end do
    !
  end subroutine set_volu
  !=====================================================================
end module oc_structure
