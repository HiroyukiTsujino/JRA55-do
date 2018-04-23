! -*-F90-*-
!
!     MRI.COM(気象研究所統合海洋モデル)stmdlp.F90
!       Copyright 2001-2003 Oceanographic Research Dept.,MRI-JMA
!
!=================================================================
!                                                                 
!     trnsfrm: 一般直交座標<－>地理座標 変換ルーチン群
!                       （一次分数変換対応）
!                                                                 
!     
!                                                                 
!=================================================================
!
!  trnsfrm : 
!    function   length_on_sphere
!                      を含む
!
module oc_mod_trnsfrm
  real(8), parameter, private :: radius =   6375.D5        
  !
contains
!====================================================
!
! (λ1,φ1)－(λ2,φ2)間の大円距離:pi/2以内
!
!====================================================
  real(8) function length_on_sphere(lambda1, phi1, lambda2, phi2)
    implicit none
    !
    real(8), intent(in)  :: lambda1, phi1, lambda2, phi2
    !
    real(8)    :: xx, yy, zz
    real(8)    :: ww, theta
    !
    !
    xx = dcos(phi1)*dcos(lambda1)-dcos(phi2)*dcos(lambda2)
    yy = dcos(phi1)*dsin(lambda1)-dcos(phi2)*dsin(lambda2)
    zz = dsin(phi1)-dsin(phi2)
    ww = dsqrt(xx*xx + yy*yy + zz*zz)
    !
    theta=dasin(0.5d0*ww)
    length_on_sphere=2.d0*radius*theta
    !
  end function length_on_sphere
!=================================================================
!
!  ダミーのサブルーチン（子午面循環描画で必要）
!
!====================================================
  subroutine set_abc ( nplat0, nplon0, splat0, splon0 )
    !
    real(8),intent(in) :: nplat0, nplon0, splat0, splon0
    !
  end subroutine set_abc
!====================================================
  subroutine mp2lp(lambda, phi, mu, psi )
    !
    real(8), intent(out)  :: lambda, phi
    real(8), intent(in)   :: mu, psi
    !
    lambda = mu
    phi    = psi
    !
  end subroutine mp2lp
!====================================================
end module oc_mod_trnsfrm
