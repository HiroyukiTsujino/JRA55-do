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
!    subroutine set_abc
!    subroutine mp2lp
!    subroutine rot_mp2lp
!    subroutine lp2mp
!    function   length_on_sphere
!                      を含む
!
module libmxe_trnsfrm
!====================================================
!
! 一般直交座標<－>地理座標 変換ルーチン群
!                       （一次分数変換対応）
!
!====================================================
  use libmxe_para, only: pi, radius, radian, radian_r
  implicit none
  complex(8), private, save :: ac, bc, cc
  real(8), private, save :: nplat, nplon, splat, splon
  !
contains
!====================================================
!
!  一次分数変換の基準点a,b,cを決める
!
!====================================================
  subroutine set_abc ( nplat0, nplon0, splat0, splon0 )
    !
    real(8),intent(in) :: nplat0, nplon0, splat0, splon0
    !
    real(8)    :: lambdaa, phia
    real(8)    :: lambdab, phib
    real(8)    :: lambdac, phic
    real(8)    :: tmp1, tmp2, tmp3
    !
    nplat = nplat0
    nplon = nplon0
    splat = splat0
    splon = splon0
    !
    ! モデル北極移動先の経度、緯度、南極の経度、緯度
    !
    phia   = NPLAT *radian_r  ! 北極点緯度（移動先）
    lambdaa= NPLON *radian_r  ! 北極点経度
    phib   = SPLAT *radian_r  ! 南極点緯度
    lambdab= SPLON *radian_r  ! 南極点経度
    !
    tmp1=dcos(phia)*dcos(lambdaa)+dcos(phib)*dcos(lambdab)
    tmp2=dcos(phia)*dsin(lambdaa)+dcos(phib)*dsin(lambdab)
    tmp3=dsin(phia)+dsin(phib)
    !
    if(dabs(tmp1*tmp1+tmp2*tmp2+tmp3*tmp3) < 1.d-30) then  
      !  北極南極が球中心について対称のとき
      phic=phia-0.5d0*pi
      lambdac=lambdaa             !  北極の経度を基準にする
    else if (dabs(tmp1*tmp1+tmp2*tmp2) < 1.d-30 ) then
      phic=datan(tmp3/dsqrt(tmp1*tmp1+tmp2*tmp2))
      lambdac=0.d0
    else if (tmp1 < 1.d-15 ) then
      phic=datan(tmp3/dsqrt(tmp1*tmp1+tmp2*tmp2))
      lambdac=0.5d0*pi*sign(1.d0,tmp2)
    else
      phic=datan(tmp3/dsqrt(tmp1*tmp1+tmp2*tmp2))
      lambdac=datan(tmp2/tmp1)    !  ac,bcの「中点」cc
    end if
    !
    phia=0.5d0*pi-phia
    tmp1=dtan(0.5d0*phia)*dcos(lambdaa)
    tmp2=dtan(0.5d0*phia)*dsin(lambdaa)
    ac=dcmplx(tmp1,tmp2)
    !
    phib=0.5d0*pi-phib
    tmp1=dtan(0.5d0*phib)*dcos(lambdab)
    tmp2=dtan(0.5d0*phib)*dsin(lambdab)
    bc=dcmplx(tmp1,tmp2)
    !
    phic=0.5d0*pi-phic
    tmp1=dtan(0.5d0*phic)*dcos(lambdac)
    tmp2=dtan(0.5d0*phic)*dsin(lambdac)
    cc=dcmplx(tmp1,tmp2)
    !
  end subroutine set_abc
!====================================================
!
!  λ,φ(μ,Ψ)
!
!====================================================
  subroutine mp2lp(lambda, phi, mu0, psi0)
    !
    real(8), intent(out)    :: lambda, phi
    real(8), intent(in)     :: mu0, psi0
    !
    real(8)     :: mu, psi
    real(8)     :: tmp1,tmp2,tmp3
    complex(8) :: z,zeta
    !
    psi=psi0
    mu=mu0
    !
    REGION: if(psi0 <= NPLAT*radian_r) then
      lambda=mu+NPLON*radian_r
      phi=psi0
    else
      !
      if(mu > pi) then
        mu=mu-2.d0*pi
      else if(mu < -pi) then
        mu=mu+2.d0*pi
      end if
      !
      tmp1 = psi0
      psi = 0.5d0*pi - sign(1.D0,mu)*mu
      mu = sign(1.d0,mu)*(tmp1-NPLAT*radian_r-0.5d0*pi)
      !  
      psi=0.5d0*pi-psi
      !
      tmp1=dtan(0.5d0*psi)*dcos(mu)
      tmp2=dtan(0.5d0*psi)*dsin(mu)
      zeta=dcmplx(tmp1,tmp2)  !  mu,psi から zeta
      !
      z=-bc*zeta*(cc-ac)+ac*(cc-bc)
      z=z/(-zeta*(cc-ac)+cc-bc)
      !
      tmp1=real(z, 8)
      tmp2=imag(z)
      if(tmp1 > 0.d0) then        ! zの偏角
        lambda=datan(tmp2/tmp1)
      else if(tmp1 < 0.d0) then
        if(tmp2 >= 0.d0) then
          lambda=pi+datan(tmp2/tmp1)
        else
          lambda=-pi+datan(tmp2/tmp1)
        end if
      else
        if(tmp2 > 0.d0) then
          lambda=0.5d0*pi
        else
          lambda=-0.5d0*pi
        end if
      end if
      tmp3=dsqrt(tmp1*tmp1 + tmp2*tmp2)          ! |z|
      phi=2.d0*datan(tmp3)
      !
      phi=0.5d0*pi-phi
      !
    endif REGION
    if ( lambda > pi ) then
      lambda = lambda - 2.d0*pi
    end if
    if ( lambda < -pi ) then
      lambda = lambda + 2.d0*pi
    end if
    !
  end subroutine mp2lp
!====================================================
!
!  ベクトルの回転cosθ,sinθ(μ,Ψ)
!
!====================================================
  subroutine rot_mp2lp(rot_cos,rot_sin,lambda0, phi0, mu0, psi0)
    !
    real(8), intent(out)    :: rot_cos, rot_sin
    real(8), intent(in)     :: lambda0, phi0
    real(8), intent(in)     :: mu0, psi0
    !
    real(8)     :: mu, psi, phi
    real(8)     :: tmp1, tmp2
    complex(8) :: ctmp, z, zeta, dzdzeta
    !
    mu=mu0
    psi=psi0
    !
    REGIN: if(psi0 <= NPLAT*radian_r) then
      rot_cos=1.d0
      rot_sin=0.d0
    else
      if (mu > pi) then
        mu=mu-2.d0*pi 
      else if(mu < -pi) then
        mu=mu+2.d0*pi
      end if
      tmp1=psi0
      psi=0.5d0*pi - sign(1.D0,mu)*mu
      mu=sign(1.d0,mu)*(tmp1-NPLAT*radian_r-0.5d0*pi)
      !
      psi=0.5d0*pi-psi
      !
      tmp1=dtan(0.5d0*psi)*dcos(mu)
      tmp2=dtan(0.5d0*psi)*dsin(mu)
      zeta=dcmplx(tmp1,tmp2)  !  mu0, psi から zeta
      if (tmp1==0.d0 .and. tmp2==0.d0) then
        rot_cos=1.d0 ! 
        rot_sin=0.d0 ! どんな値使わないのでよい
        return
      end if
      !
      phi=0.5d0*pi-phi0
      tmp1=dtan(0.5d0*phi)*dcos(lambda0)
      tmp2=dtan(0.5d0*phi)*dsin(lambda0)
      z=dcmplx(tmp1,tmp2)  !  lambda0, phi から z
      !
      ctmp=(cc-ac)*zeta+(bc-cc)
      ctmp=ctmp*ctmp
      dzdzeta=(bc-ac)*(cc-ac)*(bc-cc)/ctmp
      !
      ctmp=z/zeta/dzdzeta
      !
      rot_cos=real(ctmp, 8)
      rot_sin=imag(ctmp)
      tmp1=dsqrt(rot_cos*rot_cos+rot_sin*rot_sin)
      rot_cos=rot_cos/tmp1
      rot_sin=rot_sin/tmp1
      !
      tmp1=rot_cos
      rot_cos= dsign(1.d0,mu)*rot_sin
      rot_sin=-dsign(1.d0,mu)*tmp1
      !
    end if REGIN
    !
  end subroutine rot_mp2lp
!====================================================
!
!  μ,Ψ(λ,φ)
!
!====================================================
  subroutine lp2mp(mu, psi, lambda, phi0)
    !
    real(8), intent(out)    :: mu, psi
    real(8), intent(in)     :: lambda, phi0
    !
    real(8)     :: phi
    real(8)     :: tmp1,tmp2,tmp3
    complex(8) :: z,zeta
    !
    REGION: if(phi0 <= NPLAT*radian_r) then
      mu=lambda-NPLON*radian_r
      psi=phi0    
    else 
      !
      phi=0.5d0*pi-phi0
      !
      tmp1=dtan(0.5d0*phi)*dcos(lambda)
      tmp2=dtan(0.5d0*phi)*dsin(lambda)
      z=dcmplx(tmp1,tmp2)  !  λ,φ から z
      !
      zeta=(z-ac)*(bc-cc)/(cc-ac)
      zeta=zeta/(bc-z)
      !
      tmp1=real(zeta, 8)
      tmp2=imag(zeta)
      if(tmp1 > 0.d0) then        ! zの偏角
        mu=datan(tmp2/tmp1)
      else if(tmp1 < 0.d0) then
        if(tmp2 >= 0.d0) then
          mu=pi+datan(tmp2/tmp1)
        else
          mu=-pi+datan(tmp2/tmp1)
        end if
      else
        if(tmp2 > 0.d0) then
          mu=0.5d0*pi
        else
          mu=-0.5d0*pi
        end if
      end if
      tmp3=dsqrt(tmp1*tmp1 + tmp2*tmp2)          ! |z|
      psi=2.d0*datan(tmp3)
      !
      psi=0.5d0*pi-psi
      !
      tmp1=psi    
      psi=NPLAT*radian_r+(0.5d0*pi-sign(1.d0,mu)*mu)
      mu=sign(1.d0,mu)*(tmp1-1.5d0*pi)-pi
    end if REGION
    !
    if(mu > pi) then
      mu=mu-2.d0*pi
    end if
    if(mu < -pi) then
      mu=mu+2.d0*pi
    end if
    !
  end subroutine lp2mp
!====================================================
!
! (λ1,φ1)－(λ2,φ2)間の大円距離:pi/2以内
!
!====================================================
  real(8) function length_on_sphere(lambda1, phi1, lambda2, phi2)
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
!====================================================
end module libmxe_trnsfrm
