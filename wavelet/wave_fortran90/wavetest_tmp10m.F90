!-*-F90-*-
!****************************************************************************
! WAVETEST: Example Fortran program for WAVELET, using NINO3 SST dataset
!
! COMPILE:   f77 chisqr.f cfftpack.f wavelet.f wavetest.f
!
! See "http://paos.colorado.edu/research/wavelets/"
!
!  Copyright (C) 1998, Christopher Torrence and Gilbert P. Compo
! This software may be used, copied, or redistributed as long as it is not
! sold and this copyright notice is reproduced on each copy made.  This
! routine is provided as is without any express or implied warranties
! whatsoever.
!
! Modified: November 1999 by Arjan van Dijk to include IMPLICIT NONE and
!           to convert all routines to DOUBLE precision.
!****************************************************************************

PROGRAM wavetest_tmp10m

  IMPLICIT none

  ! these parameters depend on the particular time series

  integer(4), parameter :: im_jra=640
  integer(4), parameter :: n=3*640 + 128
  integer(4), parameter :: subscale=16
  integer(4), parameter :: jtot=11*subscale ! 2**11 = 2048
  real(8),parameter :: dt=0.5625D0, s0=dt
  real(8),parameter :: dj=1.D0/subscale

  ! Note: for accurate reconstruction and wavelet-derived variance
  !     do not pad with zeroes, set s0=dt (for Paul set s0=dt/4), and use
  !     a large "jtot" (even though the extra scales will be within
  !     the cone of influence).
  !     For plotting purposes, it is only necessary to use
  !     s0=2dt (for Morlet) and "jtot" from Eqn(10) Torrence&Compo(1998).

  integer(4) :: mother,ibase2,npad
  real(8) :: sst(n),recon_sst(n),param,pi
  real(8) :: scale(jtot),period(jtot),coi(n)
  complex(8) :: wave(n,jtot)

  integer(4) :: i,j,isigtest,javg1,javg2
  real(8) :: lag1,siglvl,dof(jtot)
  real(8) :: fft_theor(jtot),signif(jtot),ymean,variance
  real(8) :: recon_mean,recon_vari
  real(8) :: Cdelta,psi0
  real(8) :: global_ws(jtot),global_signif(jtot)
  real(8) :: savg_dof(jtot),savg_signif(jtot),sstENSO(n)

  integer(4) :: m, irec
  character(256) :: file_in
  real(4) :: work4(im_jra)
  real(4) :: undef

  !-------------------------------------------------------------------

  pi = 4.D0 * atan(1.D0)
  ibase2 = nint(log(DBLE(n))/LOG(2.D0))+1
  !npad = INT(2.D0**ibase2)
  npad = n  ! this is for no padding with zeroes

  write(6,*) npad, n

  !*************************************************** Wavelet transform

  !** let the WAVELET subroutine choose the defaults for these:

  mother = 0
  param = 6.D0
  
  !** read in the NINO3 SST data
  
  file_in = 'tmp10m_ref_core_no_antarc_smooth.dat'
  irec=140
  undef = -9.99e33
  !open(UNIT=11,FILE='sst_nino3.dat',STATUS='old')
  open(UNIT=11,FILE=file_in,form='unformatted',access='direct',action='read',STATUS='old',recl=4*im_jra)
  READ(11,rec=irec) work4
  CLOSE(11)

  sst(:) = 0.0d0
  do m = 1, 3
    do i = 1, im_jra
      if (work4(i) /= undef) then
        sst(im_jra*(m-1)+i+64) = real(work4(i),8)
      !else
      !  sst(im_jra*(m-1)+i+64) = 0.0d0
      end if
    end do
  end do
  !do i = 1, im_jra
  !  write(6,*) sst(im_jra+i+64)
  !end do

  !write(6,'(1a,f6.2,1a,f6.2)') " sst(1) = ",sst(1),"  sst(n) = ",sst(n)
  !do m = 1, n
  !  write(6,*) sst(m)
  !end do

  !** get the wavelet transform

  CALL WAVELET(n,sst,dt,mother,param,s0,dj,jtot,npad, &
       &             wave,scale,period,coi)


  !*************************************************** Significance testing

  !** local significance test

  isigtest = 0
  lag1 = 0.72D0
  siglvl = 0.05D0
  CALL WAVE_SIGNIF (isigtest,n,sst,dt,mother,param,dj,jtot, &
       &       scale,period,lag1,siglvl,dof,fft_theor,signif, &
       &       ymean,variance,Cdelta,psi0)

  !** global wavelet spectrum & significance test

  isigtest = 1
  lag1 = 0.72D0
  siglvl = 0.05D0
  do j=1,jtot
    do i=1,n
      global_ws(j) = global_ws(j) + ABS(wave(i,j))**2
    end do
    global_ws(j) = global_ws(j)/n
    dof(j) = n - scale(j)
  end do

  CALL WAVE_SIGNIF (isigtest,n,sst,dt,mother,param,dj,jtot, &
       &       scale,period,lag1,siglvl,dof,fft_theor,global_signif, &
       &       ymean,variance,Cdelta,psi0)


  !** scale-average time series & significance test
  isigtest = 2
  lag1 = 0.72D0
  siglvl = 0.05D0
  ! scale average between 2 and 7.9 years
  savg_dof(1) = 2.0D0
  savg_dof(2) = 7.9D0
  ! find the "j"-values that correspond to savg_dof(1) & savg_dof(2)
  javg1 = 0
  javg2 = 0
  do j=1,jtot
    IF ((scale(j).GE.savg_dof(1)).AND.(javg1.EQ.0)) javg1 = j
    IF (scale(j).LE.savg_dof(2)) javg2 = j
  end do
  ! call wave_signif first, to get the value of "Cdelta"
  CALL WAVE_SIGNIF (isigtest,n,sst,dt,mother,param,dj,jtot, &
       &     scale,period,lag1,siglvl,savg_dof,fft_theor,savg_signif, &
       &     ymean,variance,Cdelta,psi0)
  ! construct the scale-averaged time series [Eqn(24)]
  do i=1,n
    sstENSO(i) = 0.D0
    do j=javg1,javg2
      sstENSO(i) = sstENSO(i) + (ABS(wave(i,j))**2)/scale(j)
    end do
    sstENSO(i) = dj*dt*sstENSO(i)/Cdelta
  end do


!************************************************************* print results

  write(6,*) ' n     =',n
  write(6,*) ' dt    =',dt
  write(6,*) ' mother=',mother
  write(6,*) ' param =',param
  write(6,*) ' s0    =',s0
  write(6,*) ' dj    =',dj
  write(6,*) ' jtot  =',jtot
  write(6,*) ' npad  =',npad

  write(6,'(1a)') "Let w = wave(n/2,j)"

  write(6,'(A4,7A10)') "j","Scale","Period","ABS(w)^2","phase(w)", &
       &  "5%signif","Global","GWS5%sig"
  write(6,'(I4,7F10.3)') (j,scale(j),period(j), &
       &   ABS(wave(n/2,j))**2, &
       &   ATAN2(DIMAG(wave(n/2,j)),DBLE(wave(n/2,j)))*180.D0/pi, &
       &   signif(j),global_ws(j),global_signif(j),j=1,jtot)
  write(6,'(1A,F10.3)') ' Scale-average degrees of freedom = ',savg_dof(1)
  write(6,'(1A,F10.3)') ' Scale-avg 5% significance level  = ',savg_signif(1)


  !************************************************************ Reconstruction

  !** construct the wavelet derived variance (Parseval's theorem)  [Eqn(14)]
  !   Cdelta & psi0 are returned from WAVE_SIGNIF
  recon_vari = 0.D0
  do i=1,n
    do j=1,jtot
      recon_vari = recon_vari + (ABS(wave(i,j))**2)/scale(j)
    end do
  end do

  recon_vari = dj*dt*recon_vari/(Cdelta*n)
  write(6,'(A,F14.5)')   ' Reconstructed variance=',recon_vari
  write(6,'(A,F14.5)')   ' Original variance   =',variance
  write(6,'(A,F14.5,A)') ' Ratio = ',recon_vari/variance, &
       &     ' (this is low due to padding with zeroes)'

  do m = 64+im_jra+1, 64+im_jra*2
    write(15,*) sst(m)
  end do

  !** reconstruct the time series [Eqn(11)]
  !   check mean and RMS difference of reconstructed time series
  recon_mean=0.D0
  recon_vari = 0.D0
  do i=1,n
    recon_sst(i)=0.D0
    do j=1,jtot
      recon_sst(i) = recon_sst(i)+(DBLE(wave(i,j)))/SQRT(scale(j))
    end do
    recon_sst(i) = dj*SQRT(dt)*recon_sst(i)/(Cdelta*psi0)
    recon_vari = recon_vari+(sst(i)-ymean-recon_sst(i))**2
    recon_mean = recon_mean + recon_sst(i)
  end do
  recon_mean = recon_mean/n
  recon_vari = SQRT(recon_vari/n)

  do m = 64+im_jra+1, 64+im_jra*2
    write(16,*) recon_sst(m) - recon_mean + ymean
  end do

  write(6,'(A,F14.6)') ' Reconstructed mean = ', recon_mean
  write(6,'(A,F14.6)') ' Original mean      = ', ymean
  write(6,'(A,F14.6)') ' Root-mean-square difference of time series = ', recon_vari

  !** reconstruct the time series [Eqn(11)]
  !   check mean and RMS difference of reconstructed time series
  recon_mean=0.D0
  recon_vari = 0.D0
  do i=1,n
    recon_sst(i)=0.D0
    do j=62,jtot
      recon_sst(i) = recon_sst(i)+(DBLE(wave(i,j)))/SQRT(scale(j))
    end do
    recon_sst(i) = dj*SQRT(dt)*recon_sst(i)/(Cdelta*psi0)
    recon_vari = recon_vari+(sst(i)-ymean-recon_sst(i))**2
    recon_mean = recon_mean + recon_sst(i)
  end do
  recon_mean = recon_mean/n
  recon_vari = SQRT(recon_vari/n)

  do m = 64+im_jra+1, 64+im_jra*2
    write(17,*) recon_sst(m) - recon_mean + ymean
  end do

  write(6,'(A,F14.6)') ' Reconstructed mean = ', recon_mean
  write(6,'(A,F14.6)') ' Original mean      = ', ymean
  write(6,'(A,F14.6)') ' Root-mean-square difference of time series = ', recon_vari

  !** reconstruct the time series [Eqn(11)]
  !   check mean and RMS difference of reconstructed time series
  recon_mean=0.D0
  recon_vari = 0.D0
  do i=1,n
    recon_sst(i)=0.D0
    do j=77,jtot
      recon_sst(i) = recon_sst(i)+(DBLE(wave(i,j)))/SQRT(scale(j))
    end do
    recon_sst(i) = dj*SQRT(dt)*recon_sst(i)/(Cdelta*psi0)
    recon_vari = recon_vari+(sst(i)-ymean-recon_sst(i))**2
    recon_mean = recon_mean + recon_sst(i)
  end do
  recon_mean = recon_mean/n
  recon_vari = SQRT(recon_vari/n)

  do m = 64+im_jra+1, 64+im_jra*2
    write(18,*) recon_sst(m) - recon_mean + ymean
  end do

  write(6,'(A,F14.6)') ' Reconstructed mean = ', recon_mean
  write(6,'(A,F14.6)') ' Original mean      = ', ymean
  write(6,'(A,F14.6)') ' Root-mean-square difference of time series = ', recon_vari

END PROGRAM wavetest_tmp10m
