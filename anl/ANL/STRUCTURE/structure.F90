! -*-F90-*-
!
! module for analysis
!
!        2010.07.28 -    M. Hirabara
!
!=====================================================================
!
module oc_structure
  !
  use oc_mod_param
  !
  implicit none
  !
  !
  public :: set_hgrids
  !
  ! constants given in set_hgrids
  !
  real(8), allocatable :: dxtdeg(:), dytdeg(:), dxudeg(:), dyudeg(:)
  !
  real(8), allocatable :: alatt(:), alatu(:), alont(:), alonu(:)
  !
  real(8), allocatable :: dxt(:),dxu(:), dyt(:), dyu(:)
  real(8), allocatable :: dxtr(:), dxur(:), dytr(:), dyur(:)
  !
  real(8) :: slat, slon
  !
  ! constants given in read_topo -------------------------------
  !
  !  ho4   : 海底の深さ、単位cm、整数
  !  exnn  : UV点で何層目までが海なのかを示す。
  !  texnn : TS点で何層目までが海なのかを示す。
  !
  !  T点, U点からひとつ上の点までの距離
  real(8), allocatable :: dzz(:)
  real(8), allocatable :: dzzr(:) !  dzz の逆数
  real(8), allocatable :: dzr(:)  !  dz の逆数
  real(8), allocatable :: dp(:)   !  T点, U点の深さ
  !  T点, U点の上部境界の深さ
  real(8), allocatable :: dep(:)
  !
  integer(4), allocatable :: ho4(:,:), exnn(:,:), texnn(:,:)
#ifdef OGCM_BBL
  integer(4), allocatable :: ho4bbl(:,:), exnnbbl(:,:), texnnbbl(:,:)
#endif /* OGCM_BBL */
  !
  real(8), allocatable :: dzu(:,:,:)   ! UV点の層厚
  !
  real(8), allocatable :: aexl(:,:,:)  ! UV点の海陸インデックス
  real(8), allocatable :: atexl(:,:,:) ! TS点の海陸インデックス
  !
  real(8), allocatable :: coefx(:,:,:)   ! X点(TS点の東)海陸インデックス
  real(8), allocatable :: coefy(:,:,:)   ! Y点(TS点の北)海陸インデックス
#ifdef OGCM_BBL
  real(8), allocatable :: aexlbbl(:,:,:)  ! UV点の海陸インデックス(BBL)
  real(8), allocatable :: atexlbbl(:,:,:) ! TS点の海陸インデックス(BBL)
#endif /* OGCM_BBL */
  !
  integer(4) :: kbtm(imut,jmut)  ! （内部の）海底の格子番号k (UV)
  integer(4) :: ktbtm(imut,jmut) ! （内部の）海底の格子番号k (TS)
  !
  real(8) :: dsgm(ksgm) ! sigma layer 内における各層の割合 (無次元量）
                        ! \sum_{i=1}^{ksgm} dsgm(i) = 1 であり，初期のdzから自動的に決定される
  real(8) :: thcksgm    ! 初期のsigma layer 全体の厚さ 
  real(8) :: thcksgmr   ! 1/thcksgm
  real(8) :: depsgm(ksgm+1) ! depsgm(k) = \sum_{i=1}^{k-1} dsgm(i)
  real(8) :: dpsgm(ksgm+1)
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
  real(8), allocatable :: nendidx(:)
  !
  ! constants given in set_volt --------------------------------
  !
  real(8), allocatable :: volt(:,:,:)
  !
  !
contains !============================================================
!
!
#ifdef OGCM_VARIABLE
subroutine set_hgrids(file_vgrid)
  character(len=256), intent(in) :: file_vgrid
  integer(4), parameter :: mt = 80
  integer(4) :: ios
#else /* OGCM_VARIABLE */
subroutine set_hgrids
#endif /* OGCM_VARIABLE */
  integer(4) :: i, j, k
  !
  !-------------------
  !
  allocate( dxtdeg(imut) )
  allocate( dytdeg(jmut) )
  allocate( dxudeg(imut) )
  allocate( dyudeg(jmut) )
  !
  allocate( alatt(jmut) )
  allocate( alatu(jmut) )
  allocate( alont(imut) )
  allocate( alonu(imut) )
  !
  allocate( dxt(imut) )
  allocate( dxu(imut) )
  allocate( dyt(jmut) )
  allocate( dyu(jmut) )
  allocate( dxtr(imut) )
  allocate( dxur(imut) )
  allocate( dytr(jmut) )
  allocate( dyur(jmut) )
  !
  !-------------------
  !
#ifdef OGCM_VARIABLE
  !   可変格子を使用する場合、Ｔ格子情報をファイルから読み込む
  !
  open(mt, file=file_vgrid, form="unformatted", access="sequential")
  read(mt,iostat=ios) dxtdeg, dytdeg
  if(ios /= 0) write(*, *) 'reading error in file:', trim(file_vgrid)
  close(mt)
#else /* OGCM_VARIABLE */
  !                    一定格子を使用する場合
  dxtdeg(1:imut) = dxtdgc
  dytdeg(1:jmut) = dytdgc
#endif /* OGCM_VARIABLE */
  !
#ifdef OGCM_SUB
    slat = slat0 - dytdeg(1) - dytdeg(2)
    slon = slon0 - dxtdeg(1) - dxtdeg(2)
#else /* OGCM_SUB */
    slat = slat0 - dytdeg(1)
#ifdef OGCM_CYCLIC
    slon = slon0 - 2.0D0*dxtdeg(1)
#else /* OGCM_CYCLIC */
    slon = slon0 - dxtdeg(1)
#endif /* OGCM_CYCLIC */
#endif /* OGCM_SUB */
  !
  do i = 1, imut - 1
    dxudeg(i) = 0.5d0*(dxtdeg(i)+dxtdeg(i+1))
  end do
#ifdef OGCM_CYCLIC
  dxudeg(imut) = dxudeg(4)
#else /* OGCM_CYCLIC */
  dxudeg(imut) = dxtdeg(imut)
#endif /* OGCM_CYCLIC */
  !
  alont(1) = slon
  alonu(1) = slon + 0.5d0*dxtdeg(1)
  do i = 2, imut
    alont(i) = alont(i-1) + dxudeg(i-1)
    alonu(i) = alonu(i-1) + dxtdeg(i)
  end do
  !----------
  do j = 1, jmut - 1
    dyudeg(j) = 0.5d0*(dytdeg(j)+dytdeg(j+1))
  end do
  dyudeg(jmut) = dytdeg(jmut)
  !
  alatt(1) = slat
  alatu(1) = slat + 0.5d0*dytdeg(1)
  do j = 2, jmut
    alatt(j) = alatt(j-1) + dyudeg(j-1)
    alatu(j) = alatu(j-1) + dytdeg(j)
  end do
#ifdef OGCM_TRIPOLAR
  do j = 2, jmut-1
    if ( alatu(j+1) > nplat .and. alatu(j) < nplat) then
      alatt(j+1) = nplat
      dyudeg(j)   = alatt(j)  -alatt(j-1)
      dyudeg(j+1) = alatt(j+1)-alatt(j)
      exit
    endif
  end do
#endif /* OGCM_TRIPOLAR */
end subroutine set_hgrids
!
!=====================================================================

subroutine read_topo(fltopo)
  character(len=256), intent(in) :: fltopo
  integer(4), parameter :: mttmp    = 80
  integer(4) :: ios
  integer(4) :: i, j, k
  !
  !-----------------------
  !
  allocate( dzz(km+1) )
  allocate( dzzr(km+1) )
  allocate( dzr(km) )
  allocate( dp(km) )
  allocate( dep(km) )
  !
  allocate( ho4(imut, jmut))
  allocate( exnn(imut, jmut))
  allocate( texnn(imut, jmut))
  !
  allocate( aexl(imut, jmut, km))
  allocate( atexl(imut, jmut, km))
  !
  allocate( coefx(imut, jmut, km))
  allocate( coefy(imut, jmut, km))
  !
#ifdef OGCM_BBL
  allocate( ho4bbl(imut, jmut))
  allocate( exnnbbl(imut, jmut))
  allocate( texnnbbl(imut, jmut))
  !
  allocate( aexlbbl(imut, jmut, kbbl))
  allocate( atexlbbl(imut, jmut, kbbl))
#endif /* OGCM_BBL */
  !
  allocate( dzu(imut, jmut, km))
  !
  !-----------------------
  !
  thcksgm=0.d0                 !  sigma layer thickness
  do k=1, ksgm
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
  do k=2, km
    dep(k)=dep(k-1)+dz(k-1)
  end do
  !
  do k = 1, ksgm
    dsgm(k) = dz(k) * thcksgmr
  end do
  !-----------------------
  !
  open(mttmp, file=fltopo, form="unformatted", access="sequential")
  read(mttmp,iostat=ios) ho4, exnn         !  地形の読み込み
  if(ios /= 0) write(*, *) 'reading error in file:', trim(fltopo)
#ifdef OGCM_BBL
  read(mttmp,iostat=ios) ho4bbl, exnnbbl   !  BBL情報の読み込み
  if(ios /= 0) write(*, *) 'reading error in file:', trim(fltopo)
#endif /* OGCM_BBL */
  close(mttmp)
  !
  aexl(1:imut, 1:jmut, 1:km) =1.d0
  do k=1, km
    dzu(1:imut, 1:jmut, k) = dz(k)  ! UV点の層厚
  end do
  !
  do k=1, km
    do j=1, jmut
      do i=1, imut
        if(exnn(i,j) < k) then
          aexl(i,j,k)=0.d0 !  UV点の海陸インデックス
          dzu(i,j,k)=0.d0
        end if
        if(exnn(i,j) == k) then
          dzu(i,j,k)=real(ho4(i,j), 8) -dep(k) ! 海底面直上
        end if
      end do
    end do
  end do
  !
  texnn(1:imut, 1) = 0
  do j=2, jmut
    do i=2, imut
      texnn(i,j) = max(exnn(i-1,j  ), exnn(i,j  ),  &
          &            exnn(i-1,j-1), exnn(i,j-1)  )
    end do
  end do
  texnn(1, 2:jmut) =texnn(imut-3, 2:jmut)
  !
#ifdef OGCM_BBL
  texnnbbl(1:imut, 1) = 0
  do j=2, jmut
    do i=2, imut
      texnnbbl(i,j) = max(exnnbbl(i-1,j  ), exnnbbl(i,j  ),  &
          &               exnnbbl(i-1,j-1), exnnbbl(i,j-1)  )
    end do
  end do
  texnnbbl(1, 2:jmut) =texnnbbl(imut-3, 2:jmut)
  !
  aexlbbl(1:imut, 1:jmut, 1:kbbl) = 0.d0
  do k=1, kbbl
    do j=1, jmut
      do i=1, imut
        if(exnnbbl(i,j) >= k) aexlbbl(i, j, k) = 1.d0
      end do
    end do
  end do
  !
  do j=2, jmut
    do i=2, imut
      atexlbbl(i,j, 1:kbbl) = max(aexlbbl(i-1,j  , 1:kbbl), aexlbbl(i,j  , 1:kbbl),  &
          &                       aexlbbl(i-1,j-1, 1:kbbl), aexlbbl(i,j-1, 1:kbbl)  )
    end do
  end do
  atexlbbl(1, 2:jmut, 1:kbbl) =atexlbbl(imut-3, 2:jmut, 1:kbbl)
  !
  !---------------------------------------
  !
  aexl(1:imut, 1:jmut, km) =0.d0
  dzu(1:imut, 1:jmut, km) =0.d0
  do j=1, jmut
    do i=1, imut
      if(exnn(i,j) > 0 .and. exnnbbl(i,j) > 0) then
        aexl(i,j,km)=1.d0 !  UV点の海陸インデックス
        dzu(i,j,km)=real(ho4bbl(i,j), 8)  ! BBLの厚さ
      end if
    end do
  end do
  !
  do j = 1, jmut
    do i = 1, imut
      kbtm(i,j) = int(aexl (i,j,1),4)*(exnn(i,j) +exnnbbl(i,j)) + 1 - int(aexl(i,j,1),4)
    end do
  end do
#else /* OGCM_BBL */
  do j = 1, jmut
    do i = 1, imut
      kbtm (i,j) = aexl (i,j,1) * exnn (i,j) + 1 - aexl (i,j,1)
    end do
  end do
#endif /* OGCM_BBL */
  !
  atexl(1:imut, 1:jmut, 1:km) =0.d0
  do k=1, km
    do j=2, jmut
      do i=2, imut
        if(aexl(i-1,j-1,k)+aexl(i,j-1,k)+aexl(i-1,j,k)+aexl(i,j,k) > 0.d0) then
          atexl(i,j,k) =1.d0     ! TS点の海陸インデックス
        end if
      end do
    end do
    atexl(1, 2:jmut, 1:km) =atexl(imut-3, 2:jmut, 1:km)
  end do
  !
#ifdef OGCM_BBL
  do j = 1, jmut
    do i = 1, imut
      ktbtm(i,j) = int(atexl (i,j,1),4)*(texnn(i,j) +texnnbbl(i,j)) + 1 - int(atexl(i,j,1),4)
    end do
  end do
#else /* OGCM_BBL */
  do j = 1, jmut
    do i = 1, imut
      ktbtm(i,j) = int(atexl (i,j,1),4)*texnn(i,j) + 1 - int(atexl(i,j,1),4)
    end do
  end do
#endif /* OGCM_BBL */
!!ktbtm(1:imut, 1) = 0
!!do j=2, jmut
!!  do i=2, imut
!!    ktbtm(i,j) = max(kbtm(i-1,j  ), kbtm(i,j  ), &
!!      &              kbtm(i-1,j-1), kbtm(i,j-1) )
!!  end do
!!end do
!!ktbtm(1, 2:jmut) =ktbtm(imut-3, 2:jmut)
  !
  !
  !     COEFX: TS格子の東側に海があれば 1, 陸なら 0
  !
  coefx(1:imut, 1:jmut, 1:km) =0.d0
  do k = 1, km
    do j = 2, jmut
      do i = 1, imut
        coefx(i,j,k) = max(aexl(i,j,k),aexl(i,j-1,k))
      end do
    end do
  end do
  !
#if defined OGCM_JOT || defined OGCM_TRIPOLAR
  do i = 3, imut-2
    coefx(i, jmut  , 1:km) = coefx(imut-i, jmut-4, 1:km)
    coefx(i, jmut-1, 1:km) = coefx(imut-i, jmut-3, 1:km)
  end do
#endif /* OGCM_JOT || OGCM_TRIPOLAR */
  !
#ifdef OGCM_CYCLIC
  coefx(1:2        , 1:jmut, 1:km) = coefx(imut-3:imut-2, 1:jmut, 1:km)
  coefx(imut-1:imut, 1:jmut, 1:km) = coefx(3:4          , 1:jmut, 1:km)
#endif /* OGCM_CYCLIC */
  !
  !     COEFY: TS格子の北側に海があれば 1, 陸なら 0
  !
  coefy(1:imut, 1:jmut, 1:km) =0.d0
  do k = 1, km
    do j = 1, jmut
      do i = 2, imut
        coefy(i,j,k) = max(aexl(i,j,k),aexl(i-1,j,k))
      end do
    end do
  end do
  !
#if defined OGCM_JOT || defined OGCM_TRIPOLAR
  do i = 3, imut-2
    coefy(i, jmut  , 1:km) = coefy(imut-i+2, jmut-5, 1:km)
    coefy(i, jmut-1, 1:km) = coefy(imut-i+2, jmut-4, 1:km)
  end do
#endif /* OGCM_JOT || OGCM_TRIPOLAR */
  !
#ifdef OGCM_CYCLIC
  coefy(1:2        , 1:jmut, 1:km) = coefy(imut-3:imut-2, 1:jmut, 1:km)
  coefy(imut-1:imut, 1:jmut, 1:km) = coefy(3:4          , 1:jmut, 1:km)
#endif /* OGCM_CYCLIC */
  !
end subroutine read_topo
!
!=====================================================================
!
#ifdef OGCM_SPHERICAL
subroutine calc_scale
  !
  integer(4) :: i, j
  real(8)    :: hl1
  !--------------------------------------------------
  !
  allocate( a_bl(imut, jmut) )
  allocate( a_br(imut, jmut) )
  allocate( a_tl(imut, jmut) )
  allocate( a_tr(imut, jmut) )
  !
  allocate( dx_bl(imut, jmut) )
  allocate( dx_br(imut, jmut) )
  allocate( dx_tl(imut, jmut) )
  allocate( dx_tr(imut, jmut) )
  !
  allocate( dy_bl(imut, jmut) )
  allocate( dy_br(imut, jmut) )
  allocate( dy_tl(imut, jmut) )
  allocate( dy_tr(imut, jmut) )
  !
  do j = 1, jmut-1
#ifdef OGCM_PLANE
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
#else /* OGCM_PLANE */
    do i = 1, imut-1
      hl1  = 0.5d0 * radius * dxtdeg(i) * radian_r * cos(alatt(j)*radian_r)
      dx_bl(i,j) = hl1
      dy_bl(i,j) = 0.5d0 * radius * dytdeg(j) * radian_r
      a_bl(i,j)  = hl1 * radius * sin(0.5d0*dytdeg(j)*radian_r) *            &
        &   (1.d0 - tan(alatt(j)*radian_r) * tan(0.25d0*dytdeg(j)*radian_r))
      !
      hl1  = 0.5d0 * radius * dxtdeg(i+1) * radian_r * cos(alatt(j)*radian_r)
      dx_br(i,j) = hl1
      dy_br(i,j) = 0.5d0 * radius * dytdeg(j) * radian_r
      a_br(i,j)  = hl1 * radius * sin(0.5d0*dytdeg(j)*radian_r) *            &
        &   (1.d0 - tan(alatt(j)*radian_r) * tan(0.25d0*dytdeg(j)*radian_r))
      !
      dx_tl(i,j) = 0.5d0 * radius * dxtdeg(i) * radian_r * cos(alatu(j  )*radian_r)
      hl1        = 0.5d0 * radius * dxtdeg(i) * radian_r * cos(alatt(j+1)*radian_r)
      dy_tl(i,j) = 0.5d0 * radius * dytdeg(j+1) * radian_r
      a_tl(i,j)  = hl1 * radius * sin(0.5d0*dytdeg(j+1)*radian_r) *            &
        &   (1.d0 + tan(alatt(j+1)*radian_r) * tan(0.25d0*dytdeg(j+1)*radian_r))
      !
      dx_tr(i,j) = 0.5d0 * radius * dxtdeg(i+1) * radian_r * cos(alatu(j  )*radian_r)
      hl1        = 0.5d0 * radius * dxtdeg(i+1) * radian_r * cos(alatt(j+1)*radian_r)
      dy_tr(i,j) = 0.5d0 * radius * dytdeg(j+1) * radian_r
      a_tr(i,j)  = hl1 * radius * sin(0.5d0*dytdeg(j+1)*radian_r) *            &
        &   (1.d0 + tan(alatt(j+1)*radian_r) * tan(0.25d0*dytdeg(j+1)*radian_r))
    end do
#endif /* OGCM_PLANE */
#ifdef OGCM_CYCLIC
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
#endif /* OGCM_CYCLIC */
  end do
end subroutine calc_scale
#endif /* OGCM_SPHERICAL */
!
!=====================================================================
!
subroutine read_scale(flsclf)
  character(len=256), intent(in) :: flsclf
  integer(4), parameter :: mttmp    = 80
  integer(4) :: ios
  !
  !-----------------------------
  !
  allocate( a_bl(imut, jmut) )
  allocate( a_br(imut, jmut) )
  allocate( a_tl(imut, jmut) )
  allocate( a_tr(imut, jmut) )
  !
  allocate( dx_bl(imut, jmut) )
  allocate( dx_br(imut, jmut) )
  allocate( dx_tl(imut, jmut) )
  allocate( dx_tr(imut, jmut) )
  !
  allocate( dy_bl(imut, jmut) )
  allocate( dy_br(imut, jmut) )
  allocate( dy_tl(imut, jmut) )
  allocate( dy_tr(imut, jmut) )
  !
  open(mttmp, file=flsclf, form="unformatted", access="sequential")
  read(mttmp, iostat=ios)   a_bl  !   面積
  read(mttmp, iostat=ios)   a_br
  read(mttmp, iostat=ios)   a_tl
  read(mttmp, iostat=ios)   a_tr
  read(mttmp, iostat=ios)   dx_bl !   東西長
  read(mttmp, iostat=ios)   dx_br
  read(mttmp, iostat=ios)   dx_tl
  read(mttmp, iostat=ios)   dx_tr
  read(mttmp, iostat=ios)   dy_bl !   南北長
  read(mttmp, iostat=ios)   dy_br
  read(mttmp, iostat=ios)   dy_tl
  read(mttmp, iostat=ios)   dy_tr
  if(ios /= 0) write(*, *) 'reading error in file:', trim(flsclf)
  close(mttmp)
  !
end subroutine read_scale
!
!=====================================================================
!
subroutine set_area
  !
  integer(4) :: i, j, k
  !
  !-----------------------------
  !
  allocate( areat(imut, jmut, km) )
  areat(1:imut, 1:jmut, 1:km) = 0.0d0
  !
  do k = 1, km
    do j = 2, jmut
      do i = 2, imut
        areat(i, j, k) = atexl(i, j, k) *                                &
          & ( aexl(i-1,j  ,k) *a_br(i-1,j  ) +aexl(i,j  ,k) *a_bl(i,j  ) &
          &  +aexl(i-1,j-1,k) *a_tr(i-1,j-1) +aexl(i,j-1,k) *a_tl(i,j-1))
      end do
    end do
  end do
#ifdef OGCM_CYCLIC
  areat(1,1:jmut,1:km) = areat(imut-4,1:jmut,1:km)
#endif /* OGCM_CYCLIC */
  !
  allocate( nendidx(jmut) )
  nendidx(1:jmut) = 1.d0
#if defined OGCM_JOT || defined OGCM_TRIPOLAR
  ! for global average
  nendidx(jmut-2) = 0.5d0
#endif /* OGCM_JOT || OGCM_TRIPOLAR */
  !
end subroutine set_area
!
!=====================================================================
!
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
#ifdef OGCM_CYCLIC
  volt(1,1:jmut,1:km) = volt(imut-4,1:jmut,1:km)
#endif /* OGCM_CYCLIC */
  !
end subroutine set_volt
!
!=====================================================================
end module oc_structure
