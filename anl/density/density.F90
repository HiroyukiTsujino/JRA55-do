! -*-F90-*-
module density
  implicit none
  private

  public :: dens


contains


subroutine dens( iml, jml, kml, potential_t, sal_psu, &
               & pressure_bar, rho_local_gpcm3 )
  !- copy of ANL/ts2sigma/density.F90 (Hirabara)
  implicit none

  integer(4),intent(in)  :: iml,jml,kml
  real(8),   intent(in)  :: potential_t(iml,jml,kml)
  real(8),   intent(in)  :: sal_psu(iml,jml,kml)
  real(8),   intent(in)  :: pressure_bar(kml)
  real(8),   intent(out) :: rho_local_gpcm3(iml,jml,kml)

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

  integer(4), parameter:: ine = 5       ! 状態方程式の係数配列のサイズ
  integer(4), parameter:: inf = 7       ! 状態方程式の係数配列のサイズ

  integer(4):: i,j,k
  real(8)   :: eml(ine,kml), fml(inf,kml)
  real(8)   :: dp, ht1, ht2, ht3, ht4, ht5, hs1, hs05, hl1, hl2

  do k = 1, kml
    dp = pressure_bar(k)
    eml(1,k) = dc_e(1) + dp*(dc_g(1)+dp*dc_h(1))
    eml(2,k) = dc_e(2) + dp*(dc_g(2)+dp*dc_h(2))
    eml(3,k) = dc_e(3) + dp*(dc_g(3)+dp*dc_h(3))
    eml(4,k) = dc_e(4) + dp*dc_g(4)
    eml(5,k) = dc_e(5)
    fml(1,k) = dc_f(1) + dp*(dc_g(5)+dp*dc_h(4))
    fml(2,k) = dc_f(2) + dp*(dc_g(6)+dp*dc_h(5))
    fml(3,k) = dc_f(3) + dp*(dc_g(7)+dp*dc_h(6))
    fml(4,k) = dc_f(4)
    fml(5,k) = dc_f(5) + dp*dc_g(8)
    fml(6,k) = dc_f(6)
    fml(7,k) = dc_f(7)
  end do

  do k = 1, kml
    dp = pressure_bar(k)
    do j = 1, jml
      do i = 1, iml
        ht1 = potential_t(i,j,k)
        hs1 = sal_psu(i,j,k)
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
        rho_local_gpcm3(i,j,k) = (dp+1.D-3*hl1*hl2)/(hl2-dp)
      end do
    end do
  end do

end subroutine dens


end module density
