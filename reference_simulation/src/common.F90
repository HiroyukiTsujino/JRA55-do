! -*-F90-*-
!
!======================================================================
!
!     grid_common: モデル格子点情報の設定
!
!----------------------------------------------------------------------
module grid_common

  ! common block for model grid parameters

  use basin_param

  implicit none

  ! grid points

  real(8)  :: slon, slat
  real(8)  :: dxtdeg(imut), dytdeg(jmut)
  real(8)  :: dxudeg(imut), dyudeg(jmut)
  real(8)  :: dxt(imut), dyt(jmut)
  real(8)  :: dxu(imut), dyu(jmut)
  real(8)  :: alont(imut), alatt(jmut)
  real(8)  :: alonu(imut), alatu(jmut)

  integer(4) :: ho4(imut,jmut), exn(imut,jmut)
  integer(4) :: texn(imut,jmut)
#ifdef OGCM_BBL
  integer(4) :: ho4bbl(imut,jmut), exnbbl(imut,jmut)
  integer(4) :: texnbbl(imut,jmut)
#endif /* OGCM_BBL */
  real(8) :: hou(imut,jmut), hot(imut,jmut)

  integer(4) :: kbtm(imut,jmut)  ! （内部の）海底の格子番号k (UV)
  integer(4) :: ktbtm(imut,jmut) ! （内部の）海底の格子番号k (TS)

  real(8)  :: cs(jmut), cst(jmut)
  real(8)  :: ddyt(jmut)
  real(8)  :: tanfi(jmut),ttng(jmut),dxddy
  real(8)  :: sine(jmut)

  real(8)  :: ashft(imut,jmut), anhft(imut,jmut)
  real(8)  :: areauu(imut,jmut), areaur(imut,jmut)
  real(8)  :: aexl(imut,jmut,km), atexl(imut,jmut,km)
  real(8)  :: dzu(imut,jmut,km), dzu1c(imut,jmut), dzub(imut,jmut)
  real(8)  :: dep(km+1)
  real(8)  :: dz(km), dzz(km+1), dp(km), pd(km), pm(km+1)
  real(8)  :: dsgm(ksgm), thcksgm

  real(8)  :: volt(imut,jmut,km)

  real(8), parameter :: rmiss = -9.0, undef = -9.99e33

  !
  ! T(i,j+1)   T(i+1,j+1)
  !   +------+------+
  !   |      |      |
  !   |  tl  |  tr  |
  !   |      |      |
  !   +----U(i,j)---+
  !   |      |      |
  !   |  bl  |  br  |
  !   |      |      |
  !   +------+------+
  ! T(i,j)      T(i+1,j)
  !
  !
  ! 各領域に分割する配列

  real(8)   :: a_bl (imut,jmut), a_br (imut,jmut)  ! area(quarter of mesh)
  real(8)   :: a_tl (imut,jmut), a_tr (imut,jmut)
  real(8)   :: dx_bl(imut,jmut), dx_br(imut,jmut) ! length(bottom)
  real(8)   :: dx_tl(imut,jmut), dx_tr(imut,jmut)
  real(8)   :: dy_bl(imut,jmut), dy_br(imut,jmut) ! length(left)
  real(8)   :: dy_tl(imut,jmut), dy_tr(imut,jmut)

contains
  !------------------------------------------------------------------------
  ! setgrid

  subroutine setgrd(ftopo, fgrid, fscale)

    use basin_param

    implicit none

    character(80), intent(in) :: ftopo, fgrid, fscale
    integer(4), parameter     :: mttopo = 66, mtvgrid = 67, mtscale = 68

    integer(4) :: i, j, k, ios

    !----------------------------------------------------------------
    ! set parameters 

#ifdef OGCM_VARIABLE
    open (mtvgrid,file=fgrid,form='unformatted')
    read (mtvgrid) dxtdeg, dytdeg
    close(mtvgrid)
#else /* OGCM_VARIABLE */
    do i = 1, imut
      dxtdeg(i) = 1.0d0 / 4.0d0
    end do
    do j = 1, jmut
      dytdeg(j) = 1.0d0 / 6.0d0
    end do
#endif /* OGCM_VARIABLE */

#ifdef OGCM_CYCLIC
    slon = slon0 - dxtdeg(1) - dxtdeg(2)
#else /* OGCM_CYCLIC */
    slon = slon0 - dxtdeg(1)
#endif /* OGCM_CYCLIC */
    slat = slat0 - dytdeg(1)

    do i = 1, imut - 1
      dxudeg(i) = 0.5d0 * (dxtdeg(i) + dxtdeg(i+1))
    end do
    dxudeg(imut) = dxtdeg(imut)

    do j = 1, jmut - 1
      dyudeg(j) = 0.5d0 * (dytdeg(j) + dytdeg(j+1))
    end do
    dyudeg(jmut) = dytdeg(jmut)

    alatt(1) = slat
    alatu(1) = slat + 0.5d0 * dytdeg(1)
    alont(1) = slon
    alonu(1) = slon + 0.5d0 * dxtdeg(1)

    do i = 2, imut
      alont(i) = alont(i-1) + dxudeg(i-1)
      alonu(i) = alonu(i-1) + dxtdeg(i)
    end do
    do j = 2, jmut
      alatt(j) = alatt(j-1) + dyudeg(j-1)
      alatu(j) = alatu(j-1) + dytdeg(j)
    end do

    do j = 1, jmut
      write(21,'(I4,F10.3)') j, alatt(j)
    enddo
    do j = 1, jmut
      write(22,'(I4,F10.3)') j, alatu(j)
    enddo

    do i = 1, imut
      write(23,'(I4,F10.3)') i, alont(i)
    enddo
    do i = 1, imut
      write(24,'(I4,F10.3)') i, alonu(i)
    enddo

    write(25,'(5F10.3)') (alont(i),i=1,imut)
    write(25,'(5F10.3)') (alatt(j),j=1,jmut)

#ifdef OGCM_SPHERICAL

    do i = 1, imut
      dxt(i) = dxtdeg(i) * radius / radian
      dxu(i) = dxudeg(i) * radius / radian
    enddo

    do j = 1, jmut
      dyt(j) = dytdeg(j) * radius / radian
      dyu(j) = dyudeg(j) * radius / radian

      cs(j) = alatu(j) / radian
      sine(j) = sin(cs(j))
      cs(j) = cos(cs(j))
      ddyt(j) = 2.0d0 * radius * sin(0.5d0 * dytdeg(j) / radian)
      tanfi(j) = tan(0.25d0 * dytdeg(j) / radian)

      cst (j) = alatt(j) / radian
      ttng(j) = tan(cst(j))
      cst (j) = cos(cst(j))
      do i = 1, imut
        dxddy = dxt(i) * ddyt(j) * cst(j)
        ashft(i,j) = 0.25 * (1.0 + ttng(j) * tanfi(j)) * dxddy
        anhft(i,j) = 0.25 * (1.0 - ttng(j) * tanfi(j)) * dxddy
      end do
    end do

    !-----------------------------------------------------------

    a_bl(1:imut,1:jmut) = anhft(1:imut,1:jmut)

    do j = 1, jmut
      do i = 1, imut-1
        a_br(i,j) = anhft(i+1,j)
      end do
    end do

    do j = 1, jmut-1
      do i = 1, imut
        a_tl(i,j) = ashft(i,j+1)
      end do
    end do

    do j = 1, jmut-1
      do i = 1, imut-1
        a_tr(i,j) = ashft(i+1,j+1)
      end do
    end do

    a_br(imut, 1:jmut  ) = a_bl(imut, 1:jmut)
    a_tr(imut, 1:jmut-1) = a_tl(imut, 1:jmut-1)
    a_tl(1:imut, jmut)=a_bl(1:imut, jmut)
    a_tr(1:imut, jmut)=a_br(1:imut, jmut)

    do j = 1, jmut
      do i = 1, imut
        dx_bl(i,j) = 0.5 * dxt(i) * cst(j)
      end do
    end do

    do j = 1, jmut
      do i = 1, imut-1
        dx_br(i,j) = 0.5 * dxt(i+1) * cst(j)
      end do
    end do

    do j = 1, jmut
      do i = 1, imut
        dx_tl(i,j) = 0.5 * dxt(i) * cs(j)
      end do
    end do

    do j = 1, jmut
      do i = 1, imut-1
        dx_tr(i,j) = 0.5 * dxt(i+1) * cs(j)
      end do
    end do

    dx_br(imut,1:jmut) = dx_br(imut-1,1:jmut)
    dx_tr(imut,1:jmut-1) = dx_tr(imut-1,1:jmut-1)

    do j = 1, jmut
      do i = 1, imut
        dy_bl(i,j) = 0.5 * dyt(j)
        dy_br(i,j) = 0.5 * dyt(j)
      end do
    end do

    do j = 1, jmut-1
      do i = 1, imut
        dy_tl(i,j) = 0.5 * dyt(j+1)
        dy_tr(i,j) = 0.5 * dyt(j+1)
      end do
    end do

    dy_tl(1:imut,jmut) = dy_tl(1:imut,jmut-1)
    dy_tr(1:imut,jmut) = dy_tr(1:imut,jmut-1)

#else /* OGCM_SPHERICAL */

    open(mtscale,file=fscale,form='unformatted')

    ! 面積の受け渡し

    read(mtscale, iostat=ios) a_bl
    read(mtscale, iostat=ios) a_br
    read(mtscale, iostat=ios) a_tl
    read(mtscale, iostat=ios) a_tr

    ! 長さについては全球で計算した後、配分する

    read(mtscale, iostat=ios) dx_bl
    read(mtscale, iostat=ios) dx_br
    read(mtscale, iostat=ios) dx_tl
    read(mtscale, iostat=ios) dx_tr
    read(mtscale, iostat=ios) dy_bl
    read(mtscale, iostat=ios) dy_br
    read(mtscale, iostat=ios) dy_tl
    read(mtscale, iostat=ios) dy_tr

    close(mtscale)

    a_bl (1:imut,1:jmut) = a_bl (1:imut,1:jmut) * 1.0d-4
    a_br (1:imut,1:jmut) = a_br (1:imut,1:jmut) * 1.0d-4
    a_tl (1:imut,1:jmut) = a_tl (1:imut,1:jmut) * 1.0d-4
    a_tr (1:imut,1:jmut) = a_tr (1:imut,1:jmut) * 1.0d-4

    dx_bl(1:imut,1:jmut) = dx_bl(1:imut,1:jmut) * 1.0d-2
    dx_br(1:imut,1:jmut) = dx_br(1:imut,1:jmut) * 1.0d-2
    dx_tl(1:imut,1:jmut) = dx_tl(1:imut,1:jmut) * 1.0d-2
    dx_tr(1:imut,1:jmut) = dx_tr(1:imut,1:jmut) * 1.0d-2

    dy_bl(1:imut,1:jmut) = dy_bl(1:imut,1:jmut) * 1.0d-2
    dy_br(1:imut,1:jmut) = dy_br(1:imut,1:jmut) * 1.0d-2
    dy_tl(1:imut,1:jmut) = dy_tl(1:imut,1:jmut) * 1.0d-2
    dy_tr(1:imut,1:jmut) = dy_tr(1:imut,1:jmut) * 1.0d-2

#endif /* OGCM_SPHERICAL */

    areauu(1:imut,1:jmut) = a_bl(1:imut,1:jmut) + a_br(1:imut,1:jmut) &
         &                  + a_tl(1:imut,1:jmut) + a_tr(1:imut,1:jmut)
    areaur(1:imut,1:jmut) = 1.0d0 / areauu(1:imut,1:jmut)  !      逆数

    !-----------------------------------------------------------
    ! read topography
    !
    ! ho4 : bottom depth distribution
    ! exn : model grid number of bottom

    open (mttopo,file=ftopo,form='unformatted')
    read (mttopo) ho4, exn
#ifdef OGCM_BBL
    read (mttopo) ho4bbl, exnbbl
#endif /* OGCM_BBL */
    close(mttopo)

    texn(1:imut, 1) = 0
    do j = 2, jmut
      do i = 2, imut
        texn(i,j) = max(exn(i-1,j  ), exn(i,j  ),  &
             &          exn(i-1,j-1), exn(i,j-1)  )
      end do
    end do
#ifdef OGCM_CYCLIC
    texn(1,2:jmut) = texn(imut-3,2:jmut)
#endif /* OGCM_CYCLIC */

#ifdef OGCM_BBL
    texnbbl(1:imut,1) = 0
    do j = 2, jmut
      do i = 2, imut
        texnbbl(i,j) = max(exnbbl(i-1,j  ), exnbbl(i,j  ),  &
             &             exnbbl(i-1,j-1), exnbbl(i,j-1)  )
      end do
    end do
#ifdef OGCM_CYCLIC
    texnbbl(1,2:jmut) = texnbbl(imut-3,2:jmut)
#endif /* OGCM_CYCLIC */
#endif /* OGCM_BBL */



    ! U-points

    do k = 1, km
      do j = 1, jmut
        do i = 1, imut
          aexl (i,j,k) = 1.0d0
          if (k > exn(i,j)) then
            aexl(i,j,k) = 0.0d0
          end if
        end do
      end do
    end do

#ifdef OGCM_BBL
    do j = 1, jmut
      do i = 1, imut
        if (exn(i,j) > 0 .and. exnbbl(i,j) > 0) then
          aexl(i,j,km) = 1.d0
        end if
      end do
    end do
#endif /* OGCM_BBL */

#ifdef OGCM_BBL
    do j = 1, jmut
      do i = 1, imut
        kbtm(i,j) = aexl(i,j,1) * (exn(i,j) + exnbbl(i,j)) + 1 - aexl(i,j,1)
      end do
    end do
#else /* OGCM_BBL */
    do j = 1, jmut
      do i = 1, imut
        kbtm(i,j) = aexl(i,j,1) * exn(i,j) + 1 - aexl (i,j,1)
      end do
    end do
#endif /* OGCM_BBL */

    ! T-points

    atexl(1:imut, 1:jmut, 1:km) =0.d0
    do k = 1, km
      do j = 2, jmut
        do i = 2, imut
          atexl(i,j,k) = 1.0d0 - &
               &   (1.0d0 - aexl(i,j  ,k)) * (1.0d0 - aexl(i-1,j  ,k)) &
               & * (1.0d0 - aexl(i,j-1,k)) * (1.0d0 - aexl(i-1,j-1,k))
        end do
      end do
    end do
#ifdef OGCM_CYCLIC
    atexl(1, 2:jmut, 1:km) =atexl(imut-3, 2:jmut, 1:km)
#endif /* OGCM_CYCLIC */

    ktbtm(1:imut, 1) = 0
    do j = 1, jmut
      do i = 1, imut
        ktbtm(i,j) = atexl(i,j,1)*(texn(i,j)+texnbbl(i,j))+ 1 - atexl(i,j,1)
      end do
    end do
#ifdef OGCM_CYCLIC
    ktbtm(1, 2:jmut) = ktbtm(imut-3, 2:jmut)
#endif /* OGCM_CYCLIC */

    !------------------------------------------------------------------
    ! DEFINITION OF DISTANCE BETWEEN VERTICALLY ADJASCENT GRID LEVELS
    ! (DZZ), DEPTH OF GRID LEVELS (DP), DEPTH OF LAYER BOUNDARY (DEP),
    ! AND REVERSED VALUES OF DZ AND DZZ (DZR, DZZR)

    dz(1:km) = dz8(1:km) * 1.0e-2

    dzz(1) = 0.5d0 * dz(1)
    do k = 2, km
      dzz(k) = 0.5d0 * (dz(k-1) + dz(k))
    end do
    dzz(km+1) = 0.5d0 * dz(km)

    dp(1) = dzz(1)

    ! 1.0 bar = 10^5 [Pa]

    pd(1) = grav * dp(1) * ro * 1.0e-5

    dep(1) = 0.0d0
    dep(2) = dz(1)

    do k = 2, km
      dp(k) = dp(k-1) + dzz(k)
      dep(k+1) = dep(k) + dz(k)
      pd(k) = grav * dp(k) * ro * 1.0e-5
    end do

    pm(1:km+1) = grav * dep(1:km+1) * ro * 1.0e-5

    do k = 1, km
      write(20,'(F8.2,F8.2)') dep(k), dp(k)
    enddo
    write(20,'(F8.2)') dep(km+1)

    do j = 1, jmut
      do i = 1, imut
        dzub(i,j) = 0.0d0
        if (exn(i,j) /= 0) then
          dzub(i,j) = dble(ho4(i,j))*1.0d-2 - dep(exn(i,j))
        end if
      end do
    end do

    do k = 1, km
      do j = 1, jmut
        do i = 1, imut
          dzu(i,j,k) = aexl(i,j,k) * dz(k)
          if (exn(i,j) == k) dzu(i,j,k) = dzub(i,j)
          if (k == 1) dzu1c(i,j) = dzu(i,j,1)
        end do
      end do
    end do

    volt(1:imut, 1:jmut, 1:km) = 0.0d0

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

    thcksgm = 0.D0
    do k = 1, ksgm
      thcksgm = thcksgm + dz(k)
    enddo
    write(6,*) 'sigma-layer thickness :', thcksgm
    do k = 1, ksgm
      dsgm(k) = dz(k) / thcksgm
    enddo

    !------------------------------------------------
    ! depth

    hou(1:imut,1:jmut) = real(ho4(1:imut,1:jmut),8) * 1.0d-2

    hot(:,:) = 0.0d0
    do j = 2, jmut
      do i = 2, imut
        hot(i,j) = max(hou(i,j),hou(i-1,j),hou(i,j-1),hou(i-1,j-1))
      end do
    end do

#ifdef OGCM_BBL
    hou(1:imut,1:jmut) = hou(1:imut,1:jmut) + real(ho4bbl(1:imut,1:jmut),8) * 1.0d-2
    do j = 2, jmut
      do i = 2, imut
        hot(i,j) = hot(i,j) &
             & + max(dzu(i,j,km),dzu(i-1,j,km),dzu(i,j-1,km),dzu(i-1,j-1,km))
      end do
    end do
#endif /* OGCM_BBL */

#ifdef OGCM_CYCLIC
    hot(1,1:jmut) = hot(imut-4,1:jmut)
#endif /* OGCM_CYCLIC */


  end subroutine setgrd

end module grid_common
