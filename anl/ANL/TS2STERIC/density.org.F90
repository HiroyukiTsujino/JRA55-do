! -*-F90-*-
!
!     MRI.COM(気象研究所統合海洋モデル) density.F90
!         Copyright 2002 Ocenographic Research Dept., MRI-JMA
!
!======================================================================
!
!  密度計算ルーチンを集約   2007.06 M. Hirabara
!
module oc_mod_density
  !
  use basin_param, only : km
  use grid_common, only : pd, pm
  !
  implicit none
  !
  ! 状態方程式の係数
  !
  integer(4), parameter :: numtrc=2
  !
  real(8), parameter :: dc_a(6) = (/&
       & -0.157406D0, &
       &  6.793952D-2, &
       & -9.095290D-3, &
       &  1.001685D-4, &
       & -1.120083D-6, &
       &  6.536332D-9 /)
  real(8), parameter :: dc_b(5) = (/&
       &  0.824493D0, &
       & -4.0899D-3, &
       &  7.6438D-5, &
       & -8.2467D-7, &
       &  5.3875D-9 /)
  real(8), parameter :: dc_c(3) = (/&
       & -5.72466D-3, &
       &  1.0227D-4, &
       & -1.6546D-6 /)
  real(8), parameter :: dc_d = 4.8314D-4
  real(8), parameter :: dc_e(5) = (/&
       & 19659.35D0, &
       &   144.5863D0, &
       &    -1.722523D0, &
       &     1.019238D-2, &
       &    -4.768276D-5 /)
  real(8), parameter :: dc_f(7) = (/&
       & 52.85624D0, &
       & -3.128126D-1, &
       &  6.456036D-3, &
       & -5.370396D-5, &
       &  3.884013D-1, &
       &  9.116446D-3, &
       & -4.628163D-4 /)
  real(8), parameter :: dc_g(8) = (/&
       &  3.185918D0, &
       &  2.189412D-2, &
       & -2.823685D-4, &
       &  1.715739D-6, &
       &  6.703377D-3, &
       & -1.839953D-4, &
       &  1.912264D-7, &
       &  1.477291D-4 /)
  real(8), parameter :: dc_h(6) = (/&
       &  2.111102D-4, &
       & -1.196438D-5, &
       &  1.364330D-7, &
       & -2.048755D-6, &
       &  6.375979D-8, &
       &  5.240967D-10 /)
  !
  integer(4), parameter:: INE = 5       ! 状態方程式の係数配列のサイズ
  integer(4), parameter:: INF = 7       ! 状態方程式の係数配列のサイズ
  !
!!!!!  real(8), save :: pd(km), pm(km)
  real(8), save :: ep(ine, km), fp(inf, km), em(ine, km+1), fm(inf, km+1)
  !
contains
!
!=====================================================================
!
!     CALCOE: 状態方程式の係数
!
!=====================================================================
subroutine calcoe
  !
  implicit none
  !
  integer(4) :: k
  !
  do k = 1, km
    ep(1,k) = dc_e(1) + pd(k)*(dc_g(1)+pd(k)*dc_h(1))
    ep(2,k) = dc_e(2) + pd(k)*(dc_g(2)+pd(k)*dc_h(2))
    ep(3,k) = dc_e(3) + pd(k)*(dc_g(3)+pd(k)*dc_h(3))
    ep(4,k) = dc_e(4) + pd(k)*dc_g(4)
    ep(5,k) = dc_e(5)
  end do
  do k = 1, km
    fp(1,k) = dc_f(1) + pd(k)*(dc_g(5)+pd(k)*dc_h(4))
    fp(2,k) = dc_f(2) + pd(k)*(dc_g(6)+pd(k)*dc_h(5))
    fp(3,k) = dc_f(3) + pd(k)*(dc_g(7)+pd(k)*dc_h(6))
    fp(4,k) = dc_f(4)
    fp(5,k) = dc_f(5) + pd(k)*dc_g(8)
    fp(6,k) = dc_f(6)
    fp(7,k) = dc_f(7)
  end do
  !
  do k = 1, km + 1
    em(1,k) = dc_e(1) + pm(k)*(dc_g(1)+pm(k)*dc_h(1))
    em(2,k) = dc_e(2) + pm(k)*(dc_g(2)+pm(k)*dc_h(2))
    em(3,k) = dc_e(3) + pm(k)*(dc_g(3)+pm(k)*dc_h(3))
    em(4,k) = dc_e(4) + pm(k)*dc_g(4)
    em(5,k) = dc_e(5)
  end do
  do k = 1, km + 1
    fm(1,k) = dc_f(1) + pm(k)*(dc_g(5)+pm(k)*dc_h(4))
    fm(2,k) = dc_f(2) + pm(k)*(dc_g(6)+pm(k)*dc_h(5))
    fm(3,k) = dc_f(3) + pm(k)*(dc_g(7)+pm(k)*dc_h(6))
    fm(4,k) = dc_f(4)
    fm(5,k) = dc_f(5) + pm(k)*dc_g(8)
    fm(6,k) = dc_f(6)
    fm(7,k) = dc_f(7)
  end do
end subroutine calcoe
!======================================================================!
!!                                                                    !!
!! Density of sea water (sigma=rho-1 in the cgs units) is calculated  !!
!!     based on potential temperature [degree],                       !!
!!     salinity [psu], and pressure [bar].                            !!
!!                                                                    !!
!======================================================================!
!
!    dens: 密度計算
!
!=====================================================================
subroutine dens(iml,jml,kml,t,s,dp,rho_local)
  !
  implicit none
  !
  integer(4), intent(IN) :: iml,jml,kml
  real(8),    intent(IN) :: t(iml,jml,kml), s(iml,jml,kml), dp(kml)
  real(8), intent(INOUT) :: rho_local(iml,jml,kml)
  !
  integer(4):: i,j,k
  real(8)   :: eml(ine,km+1), fml(inf,km+1)
  real(8)   :: ht1, ht2, ht3, ht4, ht5, hs1, hs05, hl1, hl2
  !
  do k = 1, kml
    eml(1,k) = dc_e(1) + dp(k)*(dc_g(1)+dp(k)*dc_h(1))
    eml(2,k) = dc_e(2) + dp(k)*(dc_g(2)+dp(k)*dc_h(2))
    eml(3,k) = dc_e(3) + dp(k)*(dc_g(3)+dp(k)*dc_h(3))
    eml(4,k) = dc_e(4) + dp(k)*dc_g(4)
    eml(5,k) = dc_e(5)
    fml(1,k) = dc_f(1) + dp(k)*(dc_g(5)+dp(k)*dc_h(4))
    fml(2,k) = dc_f(2) + dp(k)*(dc_g(6)+dp(k)*dc_h(5))
    fml(3,k) = dc_f(3) + dp(k)*(dc_g(7)+dp(k)*dc_h(6))
    fml(4,k) = dc_f(4)
    fml(5,k) = dc_f(5) + dp(k)*dc_g(8)
    fml(6,k) = dc_f(6)
    fml(7,k) = dc_f(7)
  end do
  !
  do k = 1, kml
    do j = 1, jml
      do i = 1, iml
        ht1 = t(i,j,k)
        hs1 = s(i,j,k)
        hs05 = dsqrt(hs1)
        ht2 = ht1*ht1
        ht3 = ht2*ht1
        ht4 = ht3*ht1
        ht5 = ht4*ht1
        hl1 = dc_a(1) + dc_a(2)*ht1 + dc_a(3)*ht2         &
          & + dc_a(4)*ht3 + dc_a(5)*ht4 + dc_a(6)*ht5     &
          & + hs1*(dc_b(1)+dc_b(2)*ht1+dc_b(3)*ht2        &
          &       +dc_b(4)*ht3+dc_b(5)*ht4                &
          &       +hs05*(dc_c(1)+dc_c(2)*ht1+dc_c(3)*ht2) &
          &       +hs1*dc_d)
        hl2 = eml(1,k) + eml(2,k)*ht1 + eml(3,k)*ht2 + eml(4,k)*ht3 + eml(5,k)*ht4 &
          & + hs1*(fml(1,k)+fml(2,k)*ht1+fml(3,k)*ht2+fml(4,k)*ht3                 &
          &        +hs05*(fml(5,k)+fml(6,k)*ht1+fml(7,k)*ht2) )
        rho_local(i,j,k) = (dp(k)+1.D-3*hl1*hl2)/(hl2-dp(k))
      end do
    end do
  end do
  !
end subroutine dens
#ifdef OGCM_BBL
!======================================================================!
!
!    dens: 密度計算 for BBL
!
!=====================================================================
subroutine dens_bbl(iml,jml,kml,t,s,dp,rho_local,rho_sigma0)
  !
  implicit none
  !
  integer(4), intent(IN) :: iml,jml,kml
  real(8),    intent(IN) :: t(iml,jml,kml), s(iml,jml,kml), dp(kml)
  real(8), intent(INOUT) :: rho_local(iml,jml,kml)
  real(8), intent(INOUT) :: rho_sigma0(iml,jml,kml)
  !
  integer(4):: i,j,k
  real(8)   :: eml(ine,km+1), fml(inf,km+1)
  real(8)   :: ht1, ht2, ht3, ht4, ht5, hs1, hs05, hl1, hl2
  !
  do k = 1, kml
    eml(1,k) = dc_e(1) + dp(k)*(dc_g(1)+dp(k)*dc_h(1))
    eml(2,k) = dc_e(2) + dp(k)*(dc_g(2)+dp(k)*dc_h(2))
    eml(3,k) = dc_e(3) + dp(k)*(dc_g(3)+dp(k)*dc_h(3))
    eml(4,k) = dc_e(4) + dp(k)*dc_g(4)
    eml(5,k) = dc_e(5)
    fml(1,k) = dc_f(1) + dp(k)*(dc_g(5)+dp(k)*dc_h(4))
    fml(2,k) = dc_f(2) + dp(k)*(dc_g(6)+dp(k)*dc_h(5))
    fml(3,k) = dc_f(3) + dp(k)*(dc_g(7)+dp(k)*dc_h(6))
    fml(4,k) = dc_f(4)
    fml(5,k) = dc_f(5) + dp(k)*dc_g(8)
    fml(6,k) = dc_f(6)
    fml(7,k) = dc_f(7)
  end do
  !
  do k = 1, kml
    do j = 1, jml
      do i = 1, iml
        ht1 = t(i,j,k)
        hs1 = s(i,j,k)
        hs05 = dsqrt(hs1)
        ht2 = ht1*ht1
        ht3 = ht2*ht1
        ht4 = ht3*ht1
        ht5 = ht4*ht1
        hl1 = dc_a(1) + dc_a(2)*ht1 + dc_a(3)*ht2         &
          & + dc_a(4)*ht3 + dc_a(5)*ht4 + dc_a(6)*ht5     &
          & + hs1*(dc_b(1)+dc_b(2)*ht1+dc_b(3)*ht2        &
          &       +dc_b(4)*ht3+dc_b(5)*ht4                &
          &       +hs05*(dc_c(1)+dc_c(2)*ht1+dc_c(3)*ht2) &
          &       +hs1*dc_d)
        hl2 = eml(1,k) + eml(2,k)*ht1 + eml(3,k)*ht2 + eml(4,k)*ht3 + eml(5,k)*ht4 &
          & + hs1*(fml(1,k)+fml(2,k)*ht1+fml(3,k)*ht2+fml(4,k)*ht3                 &
          &        +hs05*(fml(5,k)+fml(6,k)*ht1+fml(7,k)*ht2) )
        rho_local(i,j,k) = (dp(k)+1.D-3*hl1*hl2)/(hl2-dp(k))
        rho_sigma0(i,j,k) = hl1
      end do
    end do
  end do
  !
end subroutine dens_bbl
#endif /* OGCM_BBL */
!=====================================================================
!
!    sigma0_3d: 海面基準の密度を3次元配列に入れる
!
!=====================================================================
subroutine sigma0_3d(iml, jml, kml, trcmtx, sigma0)
  !
  implicit none
  !
  integer(4), intent(IN) :: iml,jml,kml
  real(8),    intent(IN) :: trcmtx(iml,jml,km,numtrc)
  real(8), intent(INOUT) :: sigma0(iml,jml,km)
  !
  integer(4):: i, j, k
  real(8)   :: ht1, ht2, ht3, ht4, ht5, hs1, hs05
  !
  do k=1, kml
    do j = 1, jml
      do i = 1, iml
        ht1 = trcmtx(i,j,k,1)
        hs1 = trcmtx(i,j,k,2)
        hs05 = sqrt(hs1)
        ht2 = ht1*ht1
        ht3 = ht2*ht1
        ht4 = ht3*ht1
        ht5 = ht4*ht1
        sigma0(i,j,k) = dc_a(1)     +dc_a(2)*ht1 +dc_a(3)*ht2                  &
          &            +dc_a(4)*ht3 +dc_a(5)*ht4 +dc_a(6)*ht5                  &
          & +hs1*( dc_b(1) +dc_b(2)*ht1 +dc_b(3)*ht2 +dc_b(4)*ht3 +dc_b(5)*ht4 &
          &       +hs05*(dc_c(1) +dc_c(2)*ht1 +dc_c(3)*ht2) +hs1*dc_d )
      end do
    end do
  end do
end subroutine sigma0_3d
!=====================================================================
!
!    sigma0_2d: 海面基準の密度を2次元配列に入れる
!
!=====================================================================
subroutine sigma0_2d(iml, jml, t, s, sigma0)
  !
  implicit none
  !
  integer(4), intent(IN) :: iml, jml
  real(8),    intent(IN) :: t(iml, jml), s(iml, jml)
  real(8), intent(INOUT) :: sigma0(iml, jml)
  !
  integer(4):: i, j
  real(8)   :: ht1, ht2, ht3, ht4, ht5, hs1, hs05
  !
  do j = 1, jml
    do i = 1, iml
      ht1 = t(i, j)
      hs1 = s(i, j)
      hs05 = sqrt(hs1)
      ht2 = ht1*ht1
      ht3 = ht2*ht1
      ht4 = ht3*ht1
      ht5 = ht4*ht1
      sigma0(i,j) = dc_a(1)     +dc_a(2)*ht1 +dc_a(3)*ht2                    &
        &          +dc_a(4)*ht3 +dc_a(5)*ht4 +dc_a(6)*ht5                    &
        & +hs1*( dc_b(1) +dc_b(2)*ht1 +dc_b(3)*ht2 +dc_b(4)*ht3 +dc_b(5)*ht4 &
        &       +hs05*(dc_c(1) +dc_c(2)*ht1 +dc_c(3)*ht2) +hs1*dc_d )
    end do
  end do
end subroutine sigma0_2d
!=====================================================================
!
!    sigmak_2d: k referred sigma
!
!=====================================================================
subroutine sigmak_2d(iml, jml, t, s, sigma0, k, sigmak)
  !
  implicit none
  !
  integer(4), intent(IN) :: iml, jml, k
  real(8),    intent(IN) :: t(iml, jml), s(iml, jml)
  real(8),    intent(IN) :: sigma0(iml, jml)
  real(8), intent(INOUT) :: sigmak(iml, jml)
  !
  integer(4):: i, j
  real(8)   :: ht1, ht2, ht3, ht4, hs1, hs05, hl2
  !
  do j = 1, jml
    do i = 1, iml
      ht1 = t(i, j)
      hs1 = s(i, j)
      hs05 = sqrt(hs1)
      ht2 = ht1*ht1
      ht3 = ht2*ht1
      ht4 = ht3*ht1
      hl2 = ep(1,k) + ep(2,k)*ht1 + ep(3,k)*ht2 + ep(4,k)*ht3   &
        & + ep(5,k)*ht4 + hs1*(fp(1,k)+fp(2,k)*ht1+fp(3,k)*ht2  &
        & +fp(4,k)*ht3  +hs05*(fp(5,k)+fp(6,k)*ht1+fp(7,k)*ht2))
      sigmak(i,j) = (pd(k)+1.D-3*sigma0(i,j)*hl2)/(hl2-pd(k))
    end do
  end do
end subroutine sigmak_2d
!=====================================================================
!
!    sigmakmh_2d: k-1/2 referred sigma
!
!=====================================================================
subroutine sigmakmh_2d(iml, jml, t, s, sigma0, k, sigmak)
  !
  implicit none
  !
  integer(4), intent(IN) :: iml, jml, k
  real(8),    intent(IN) :: t(iml, jml), s(iml, jml)
  real(8),    intent(IN) :: sigma0(iml, jml)
  real(8), intent(INOUT) :: sigmak(iml, jml)
  !
  integer(4):: i, j
  real(8)   :: ht1, ht2, ht3, ht4, hs1, hs05, hl2
  !
  do j = 1, jml
    do i = 1, iml
      ht1 = t(i,j)
      hs1 = s(i,j)
      hs05 = sqrt(hs1)
      ht2 = ht1*ht1
      ht3 = ht2*ht1
      ht4 = ht3*ht1
      hl2 = em(1,k) + em(2,k)*ht1 + em(3,k)*ht2 + em(4,k)*ht3   &
        & + em(5,k)*ht4 + hs1*(fm(1,k)+fm(2,k)*ht1+fm(3,k)*ht2  &
        & +fm(4,k)*ht3  +hs05*(fm(5,k)+fm(6,k)*ht1+fm(7,k)*ht2))
      sigmak(i,j) = (pm(k)+1.D-3*sigma0(i,j)*hl2)/(hl2-pm(k))
    end do
  end do
end subroutine sigmakmh_2d
!=====================================================================
!=====================================================================
end module oc_mod_density
