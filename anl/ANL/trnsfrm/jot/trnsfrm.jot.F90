! -*-F90-*-
!
!     MRI.COM(‹CÛŒ¤‹†Š“‡ŠC—mƒ‚ƒfƒ‹)stmdlp.F90
!       Copyright 2001-2003 Oceanographic Research Dept.,MRI-JMA
!
!=================================================================
!                                                                 
!     trnsfrm: ˆê”Ê’¼ŒğÀ•W<|>’n—À•W •ÏŠ·ƒ‹[ƒ`ƒ“ŒQ
!                       iˆêŸ•ª”•ÏŠ·‘Î‰j
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
!                      ‚ğŠÜ‚Ş
!
module oc_mod_trnsfrm
!====================================================
!
! ˆê”Ê’¼ŒğÀ•W<|>’n—À•W •ÏŠ·ƒ‹[ƒ`ƒ“ŒQ
!                    iJoukowski•ÏŠ·‘Î‰j
!
!====================================================
  implicit none
  real(8), parameter, private :: pi=3.141592653589793d0  ! ‰~ü—¦
  real(8), parameter, private :: radian=180.d0/pi
  real(8), parameter, private :: radian_r=1.d0/radian
  real(8), parameter, private :: radius = 6375.d5    
  complex(8), private, save :: ac
  real(8), private, save :: rcjot, rsjot  
  real(8), private, save :: nplat, nplon, splat, splon
  !
contains
!====================================================
!
!  Joukowski•ÏŠ·‚Ìƒpƒ‰ƒ[ƒ^ac,rcjot,rsjot‚ğŒˆ‚ß‚é
!
!====================================================
  subroutine set_abc ( nplat0, nplon0, splat0, splon0 )
    !
    real(8),intent(in) :: nplat0, nplon0, splat0, splon0
    real(8) :: p_jot, q_jot
    !
    nplat = nplat0
    nplon = nplon0
    splat = splat0
    splon = splon0
    ! 
    p_jot=NPLAT*radian_r ! ‚Â‚Ô‚·ˆÜ“x‰~‚Ì–k‹É‚©‚ç‚ÌŠp“x
    p_jot=tan(0.5*p_jot)  ! •¡‘f•½–Êã‚Å‚ÌŠî€”¼Œa
    ac=cmplx(p_jot*p_jot, 0.d0)
    !
    q_jot=NPLON*radian_r ! “ì–k²‚Å‚Ì‰ñ“]
    rcjot=cos(q_jot)
    rsjot=sin(q_jot)
    !
  end subroutine set_abc
!====================================================
!
!  ƒÉ,ƒÓ(ƒÊ,ƒµ)
!
!====================================================
  subroutine mp2lp(lambda, phi, mu, psi0 )
    !
    real(8), intent(out)  :: lambda, phi
    real(8), intent(in)   :: mu, psi0
    !
    real(8)     :: psi
    real(8)     :: tmp1,tmp2
    complex(8) :: z,zeta
    !!!!!complex(16) :: z,zeta   ! old code
    !
    !
    psi=0.5d0*pi-psi0
    !
    zeta=cmplx(cos(mu),sin(mu))
    zeta=zeta*tan(0.5d0*psi)
    !
    z=cmplx(rcjot, rsjot)
    z=z*(zeta+ac/zeta)
    !
    tmp1=abs(z)              ! |z|
    phi=2.d0*atan(tmp1)
    !
    tmp1=real(z, 8)
    tmp2=imag(z)
    !                      z‚Ì•ÎŠp
    if(tmp1 > 0.d0) then
      lambda=atan(tmp2/tmp1)
    else if(tmp1 == 0.d0) then
      if(tmp2 < 0.d0) then
        lambda=-0.5d0*pi
      else
        lambda=0.5d0*pi
      end if
    else
      if(tmp2 < 0.d0) then
        lambda=atan(tmp2/tmp1)-pi
      else
        lambda=atan(tmp2/tmp1)+pi
      end if
    end if
    !
    phi=0.5d0*pi-phi
    !
  end subroutine mp2lp
!====================================================
!
!  ƒxƒNƒgƒ‹‚Ì‰ñ“]cosƒÆ,sinƒÆ(ƒÊ,ƒµ)
!
!====================================================
  subroutine rot_mp2lp(rot_cos,rot_sin,lambda0, phi0, mu0, psi0)
    !
    real(8), intent(out)  :: rot_cos, rot_sin
    real(8), intent(in)   :: lambda0, phi0
    real(8), intent(in)   :: mu0, psi0
    !
    real(8)     :: psi, phi
    real(8)     :: tmp1, tmp2
    complex(8)  :: ctmp0, ctmp, z, zeta
    !
    psi=0.5d0*pi-psi0
    !
    tmp1=tan(0.5d0*psi)*cos(mu0)
    tmp2=tan(0.5d0*psi)*sin(mu0)
    zeta=cmplx(tmp1,tmp2)  !  mu0, psi ‚©‚ç zeta
    !
    phi=0.5d0*pi-phi0
    tmp1=dtan(0.5d0*phi)*dcos(lambda0)
    tmp2=dtan(0.5d0*phi)*dsin(lambda0)
    z=cmplx(tmp1,tmp2)  !  lambda0, phi ‚©‚ç z
    !
    ctmp0=cmplx(rcjot, rsjot)
    ctmp=ctmp0*(1.d0 -ac/zeta/zeta) ! dz/dzeta(zeta0)
    !
    ctmp=z/zeta/ctmp
    !
    rot_cos=real(ctmp, 8)
    rot_sin=imag(ctmp)
    tmp1=sqrt(rot_cos*rot_cos+rot_sin*rot_sin)
    rot_cos=rot_cos/tmp1
    rot_sin=rot_sin/tmp1
    !
  end subroutine rot_mp2lp
!====================================================
!
!  ƒÊ,ƒµ(ƒÉ,ƒÓ)
!
!====================================================
  subroutine lp2mp(mu, psi, lambda, phi0)
    !
    real(8), intent(out)    :: mu, psi
    real(8), intent(in)     :: lambda, phi0
    !
    real(8)    :: phi
    real(8)    :: tmp1,tmp2
    complex(8) :: ctmp1, z, zeta0, zetap, zetam
    !
    phi=0.5d0*pi-phi0
    !
    tmp1=tan(0.5d0*phi)*cos(lambda)
    tmp2=tan(0.5d0*phi)*sin(lambda)
    z=cmplx(tmp1,tmp2)  !  lambda,phi ‚©‚ç z
    !
    zeta0=cmplx(rcjot, -rsjot)
    zeta0=0.5d0*z*zeta0
    !
    ctmp1=zeta0*zeta0-ac
    ctmp1=sqrt(ctmp1)
    zetap=zeta0+ctmp1    !  ƒÄ
    zetam=zeta0-ctmp1    !  ƒÄ
    !
    tmp1=max(abs(zetap),abs(zetam))  ! |ƒÄ|
    psi=2.d0*atan(tmp1)
    !
    if(abs(zetap) >= abs(zetam)) then
      tmp1=real(zetap)
      tmp2=aimag(zetap)
    else
      tmp1=real(zetam)
      tmp2=imag(zetam)
    end if
    !                     z‚Ì•ÎŠp
    if(tmp1 > 0.d0) then
      mu=atan(tmp2/tmp1)
    else if(tmp1 == 0.d0) then
      if(tmp2 < 0.d0) then
        mu=-0.5d0*pi
      else
        mu=0.5d0*pi
      end if
    else
      if(tmp2 < 0.d0) then
        mu=atan(tmp2/tmp1)-pi
      else
        mu=atan(tmp2/tmp1)+pi
      end if
    end if
    !
    psi=0.5d0*pi-psi
    !
  end subroutine lp2mp
!====================================================
!
! (ƒÉ1,ƒÓ1)|(ƒÉ2,ƒÓ2)ŠÔ‚Ì‘å‰~‹——£:pi/2ˆÈ“à
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
!=================================================================
end module oc_mod_trnsfrm
