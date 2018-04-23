! -*-F90-*-
!
!======================================================================
!
!     grid_common: モデル格子点情報の設定
!
!----------------------------------------------------------------------
module grid_common

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

  ! vertical

  real(8)  :: dep(km+1)
  real(8)  :: dz_mks(km), dzz(km+1), dp(km)

  ! topography

  integer(4) :: ho4(imut,jmut), exn(imut,jmut), texn(imut,jmut)
#ifdef OGCM_BBL
  integer(4) :: ho4bbl(imut,jmut), exnbbl(imut,jmut), texnbbl(imut,jmut)
#endif /* OGCM_BBL */

  real(8)  :: aexl(imut,jmut,km), atexl(imut,jmut,km)
#ifdef OGCM_BBL
  real(8)  :: aexltmp(imut,jmut,km)
#endif /* OGCM_BBL */
contains
  !------------------------------------------------------------------------
  ! setgrid

  subroutine setgrd(ftopo, fgrid)

    use basin_param

    implicit none

    character(256), intent(in) :: ftopo, fgrid
    integer(4), parameter      :: mttopo = 66, mtvgrid = 67

    integer(4) :: i, j, k, ktmp

    !----------------------------------------------------------------
    ! set parameters 

#ifdef OGCM_VARIABLE
    open (mtvgrid,file=fgrid,form='unformatted')
    read (mtvgrid) dxtdeg, dytdeg
    close(mtvgrid)
#else /* OGCM_VARIABLE */
    do i = 1, imut
      dxtdeg(i) = dxtdgc
    end do
    do j = 1, jmut
      dytdeg(j) = dxtdgc
    end do
#endif /* OGCM_VARIABLE */

#if defined OGCM_CYCLIC || defined OGCM_SUB
    slon = slon0 - dxtdeg(1) - dxtdeg(2)
#else /* OGCM_CYCLIC || OGCM_SUB */
    slon = slon0 - dxtdeg(1)
#endif /* OGCM_CYCLIC || OGCM_SUB */

#ifdef OGCM_SUB
    slat = slat0 - dytdeg(1) - dytdeg(2)
#else /* OGCM_SUB */
    slat = slat0 - dytdeg(1)
#endif /* OGCM_SUB */

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

!    do j = 1, jmut
!      write(21,'(I4,F10.3)') j, alatt(j)
!    enddo
!    do j = 1, jmut
!      write(22,'(I4,F10.3)') j, alatu(j)
!    enddo
!
!    do i = 1, imut
!      write(23,'(I4,F10.3)') i, alont(i)
!    enddo
!    do i = 1, imut
!      write(24,'(I4,F10.3)') i, alonu(i)
!    enddo

    !----------------------------------------------
    ! ho4 : bottom depth distribution
    ! exn : model grid number of bottom

    open (mttopo,file=ftopo,form='unformatted')
    read (mttopo) ho4, exn
#ifdef OGCM_BBL
    read (mttopo) ho4bbl, exnbbl
#endif /* OGCM_BBL */
    close(mttopo)

    !------

    do j = 1, jmut
      do i = 1, imut
        texn(i,j) = 0
#ifdef OGCM_BBL
        texnbbl(i,j) = 0
#endif /* OGCM_BBL */
      end do
    end do

    do j = 2, jmut
      do i = 2, imut  ! Ｔ点における海(1)陸(0)index
        texn(i,j) = &
             & max(exn(i,j),exn(i-1,j),exn(i,j-1),exn(i-1,j-1))
#ifdef OGCM_BBL        
        texnbbl(i,j) = &
             & max(exnbbl(i,j),exnbbl(i-1,j),exnbbl(i,j-1),exnbbl(i-1,j-1))
#endif /* OGCM_BBL */
      end do
#ifdef OGCM_CYCLIC
      texn(1,j) = texn(imut-3,j)
      texn(2,j) = texn(imut-2,j)
      texn(imut-1,j) = texn(3,j)
      texn(imut,j) = texn(4,j)
#ifdef OGCM_BBL        
      texnbbl(1,j) = texnbbl(imut-3,j)
      texnbbl(2,j) = texnbbl(imut-2,j)
      texnbbl(imut-1,j) = texnbbl(3,j)
      texnbbl(imut,j) = texnbbl(4,j)
#endif /* OGCM_BBL */
#endif /* OGCM_CYCLIC */
    end do

    K_LOOP: do k = 1, km
      do j = 1, jmut
        do i = 1, imut
          aexl(i,j,k) = 1.D0    ! Ｕ点の海陸インデックス
          atexl(i,j,k) = 0.D0   ! Ｔ点の海陸インデックス
          if (k > exn(i,j)) aexl(i,j,k) = 0.D0
#ifdef OGCM_BBL
          ! k = km および exn(i,j)+1の場所の両方にBBLは存在することにするが，
          ! dzuはexn(i,j)+1の場所では0である．
          if (k >= km+1-kbbl) then
            if (k-(km-kbbl) <= exnbbl(i,j)) aexl(i,j,k) = 1.D0
          end if
#endif /* OGCM_BBL */
        end do
      end do

#ifdef OGCM_SUB

      ! For nested grid SUB model : southern and western mergin

      atexl(1,1,k) = aexl(1,1,k)
      do i = 2, imut
        atexl(i,1,k) = 1.d0 - (1.d0-aexl(i,1,k))*(1.d0-aexl(i-1,1,k))
      end do
      do j = 2, jmut
        atexl(1,j,k) = 1.d0 - (1.d0-aexl(1,j-1,k))*(1.d0-aexl(1,j,k))
      end do

#endif /* OGCM_SUB */

      do j = 2, jmut
        do i = 2, imut
          atexl(i,j,k) = 1.D0 - (1.D0-aexl(i,j,k))*(1.D0-aexl(i-1,j,k)) &
               & *(1.D0-aexl(i,j-1,k))*(1.D0-aexl(i-1,j-1,k))
        end do
      end do

    end do K_LOOP

#ifdef OGCM_BBL

    ! temporary replace aexl

    do j = 1, jmut
      do i = 1, imut
        do k = exn(i,j) + 1, exn(i,j) + exnbbl(i,j)
          aexltmp(i,j,k) = aexl(i,j,k)
          aexl(i,j,k) = 1.0d0
        end do
      end do
    end do

    ! construct atexl

    do j = 2, jmut
      do i = 2, imut
        do ktmp = texn(i,j) + 1, texn(i,j) + texnbbl(i,j)
          !  for example, when kbbl = 1 and exnbbl(i,j) = 1, k = km
          k = km-kbbl+(ktmp-texn(i,j))
          atexl(i,j,ktmp) = 1.D0 - (1.D0-aexl(i,j,k))*(1.D0-aexl(i-1,j,k)) &
               & *(1.D0-aexl(i,j-1,k))*(1.D0-aexl(i-1,j-1,k))
        end do
      end do
    end do

    ! restore aexl

    do j = 1, jmut
      do i = 1, imut
        do k = exn(i,j) + 1, exn(i,j) + exnbbl(i,j)
          aexl(i,j,k) = aexltmp(i,j,k)
        end do
      end do
    end do

#endif /* OGCM_BBL */

#ifdef OGCM_CYCLIC
    do k = 1, km
      do j = 1, jmut
        atexl(1     ,j,k) = atexl(imut-3,j,k)
        atexl(2     ,j,k) = atexl(imut-2,j,k)
        atexl(imut-1,j,k) = atexl(3     ,j,k)
        atexl(imut  ,j,k) = atexl(4     ,j,k)
      end do
    end do
#endif /* OGCM_CYCLIC */

    !------------------------------------------------------------------
    ! DEFINITION OF DISTANCE BETWEEN VERTICALLY ADJASCENT GRID LEVELS
    ! (DZZ), DEPTH OF GRID LEVELS (DP), DEPTH OF LAYER BOUNDARY (DEP),
    ! AND REVERSED VALUES OF DZ AND DZZ (DZR, DZZR)

    dz_mks(1:km) = dz(1:km) * 1.0e-2

    dzz(1) = 0.5d0 * dz_mks(1)
    do k = 2, km
      dzz(k) = 0.5d0 * (dz_mks(k-1) + dz_mks(k))
    end do
    dzz(km+1) = 0.5d0 * dz_mks(km)

    dp(1) = dzz(1)

    dep(1) = 0.0d0
    dep(2) = dz_mks(1)

    do k = 2, km
      dp(k) = dp(k-1) + dzz(k)
      dep(k+1) = dep(k) + dz_mks(k)
    end do

  end subroutine setgrd

end module grid_common
