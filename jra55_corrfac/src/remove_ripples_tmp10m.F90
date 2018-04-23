!-*-F90-*-
!****************************************************************************
! WAVETEST: Example Fortran program for WAVELET, using NINO3 SST dataset
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

PROGRAM remove_ripples_tmp10m


  use libmxe_para, only: libmxe_para__register, clen, rho &
                     & , pi, radius, radian, radian_r, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io
  use libmxe_grads, only: libmxe_grads__make, type_grads

  use file_open_close_manager

  implicit none

  integer(4) :: imut, jmut, km
  integer(4) :: lreclen_in
  integer(4) :: lreclen_out

  ! these parameters depend on the particular time series

  !integer(4), parameter :: n = 3 * imut + 128
  integer(4) :: n
  integer(4), parameter :: subscale = 16
  integer(4), parameter :: jtot = 11*subscale ! 2^11 = 2048
  real(8),parameter :: dt = 0.5625D0, s0 = dt
  real(8),parameter :: dj = 1.D0 / subscale

  ! Note: for accurate reconstruction and wavelet-derived variance
  !     do not pad with zeroes, set s0=dt (for Paul set s0=dt/4), and use
  !     a large "jtot" (even though the extra scales will be within
  !     the cone of influence).
  !     For plotting purposes, it is only necessary to use
  !     s0=2dt (for Morlet) and "jtot" from Eqn(10) Torrence&Compo(1998).

  integer(4) :: mother, ibase2, npad
  real(8) :: param
  real(8) :: scale(jtot),period(jtot)
  real(8), allocatable   :: coi(:)
  complex(8),allocatable :: wave(:,:)

  integer(4) :: i,j,isigtest,javg1,javg2
  real(8) :: lag1,siglvl,dof(jtot)
  real(8) :: fft_theor(jtot),signif(jtot),ymean,variance
  real(8) :: recon_mean,recon_vari
  real(8) :: Cdelta,psi0
  real(8) :: global_ws(jtot),global_signif(jtot)
  real(8) :: savg_dof(jtot),savg_signif(jtot)

  integer(4) :: nd, ndata
  integer(4) :: ii, jj, m, irec

  real(8),allocatable :: sst(:),recon_sst(:)
  real(8),allocatable :: sstENSO(:)

  real(8),allocatable :: aexl(:,:)
  real(4),allocatable :: work4_org(:,:), work4_new(:,:)

  real(4) :: undef_in, undef_out

  integer(4) :: mtin1 = 71, mtin2 = 72, mtin3 = 73, mtin4 = 74
  integer(4) :: mtot1 = 81, mtot2 = 82, mtot3 = 83, mtot4 = 84
  character(256) :: flnin
  character(256) :: flnot

  ! MXE structure variables

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo
  type(type_libmxe_io) :: io
  type(type_grads) :: grads

  !-----------------------------------------------------------------------

  namelist /nml_remove_ripples/ flnin, undef_in, ndata, flnot, undef_out

  open (11,file='namelist.remove_ripples_tmpcore')
  read (11,nml=nml_remove_ripples)
  close(11)

  !-----------------------------------------------------------------------
  ! set grid points

  !-- model settings --

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)

  imut = para%imut
  jmut = para%jmut

  lreclen_in = 4*imut*jmut
  lreclen_out = 4*imut*jmut

  allocate(work4_org(1:imut,1:jmut))
  allocate(work4_new(1:imut,1:jmut))
  allocate(aexl(1:imut,1:jmut))

  aexl(:,:) = 1.0d0

  !-------------------------------------------------------------------

  n = 3 * imut + 128 ! = 2048

  allocate(sst(1:n),recon_sst(1:n))
  allocate(sstENSO(1:n))
  allocate(coi(1:n))
  allocate(wave(1:n,1:jtot))

  ibase2 = nint(log(DBLE(n))/LOG(2.D0))+1
  !npad = INT(2.D0**ibase2)
  npad = n  ! this is for no padding with zeroes

  write(6,*) npad, n

  !** let the WAVELET subroutine choose the defaults for these:

  mother = 0
  param = 6.D0

  !*************************************************** Wavelet transform
  !** read in the NINO3 SST data
  
  call open_file_direct(mtin1,flnin,lreclen_in)
  write(6,'(1a,1a)') 'data read from ... ', trim(flnin)

  call open_file_direct(mtot1,flnot,lreclen_out)
  write(6,'(1a,1a)') 'data written to ... ', trim(flnot)

  LOOP_DATA: do nd = 1, ndata

  READ(mtin1,rec=nd) work4_org

  LOOP_JJ: do jj = 1, jmut
!  jj = 140

    sst(:) = 0.0d0
    do m = 1, 3
      do ii = 1, imut
        if (work4_org(ii,jj) /= undef_in) then
          sst(imut*(m-1)+ii+64) = real(work4_org(ii,jj),8)
        !else
        !  sst(imut*(m-1)+ii+64) = 0.0d0
        end if
      end do
    end do

    !do ii = 1, imut
    !  write(6,*) sst(imut+ii+64)
    !end do

    !write(6,'(1a,f6.2,1a,f6.2)') " sst(1) = ",sst(1),"  sst(n) = ",sst(n)
    !do m = 1, n
    !  write(6,*) sst(m)
    !end do

    !** get the wavelet transform

    write(6,*) 'Calling wavelet transform ', jj

    CALL WAVELET(n,sst,dt,mother,param,s0,dj,jtot,npad, &
         &             wave,scale,period,coi)

    write(6,*) ' done '

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
    !
    write(6,*) ' n     =',n
    write(6,*) ' dt    =',dt
    write(6,*) ' mother=',mother
    write(6,*) ' param =',param
    write(6,*) ' s0    =',s0
    write(6,*) ' dj    =',dj
    write(6,*) ' jtot  =',jtot
    write(6,*) ' npad  =',npad
    !
    !write(6,'(1a)') "Let w = wave(n/2,j)"
    !
    !write(6,'(A4,7A10)') "j","Scale","Period","ABS(w)^2","phase(w)", &
    !     &  "5%signif","Global","GWS5%sig"
    !write(6,'(I4,7F10.3)') (j,scale(j),period(j), &
    !     &   ABS(wave(n/2,j))**2, &
    !     &   ATAN2(DIMAG(wave(n/2,j)),DBLE(wave(n/2,j)))*180.D0/pi, &
    !     &   signif(j),global_ws(j),global_signif(j),j=1,jtot)
    !write(6,'(1A,F10.3)') ' Scale-average degrees of freedom = ',savg_dof(1)
    !write(6,'(1A,F10.3)') ' Scale-avg 5% significance level  = ',savg_signif(1)
    !
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
    if (variance > 0.0d0) then
      write(6,'(A,F14.5,A)') ' Ratio = ',recon_vari/variance, &
           &     ' (this is low due to padding with zeroes)'
    end if
    
    !** reconstruct the original time series [Eqn(11)]
    !   check mean and RMS difference of reconstructed time series

    !recon_mean=0.D0
    !recon_vari = 0.D0
    !do i=1,n
    !  recon_sst(i)=0.D0
    !  do j=1,jtot
    !    recon_sst(i) = recon_sst(i)+(DBLE(wave(i,j)))/SQRT(scale(j))
    !  end do
    !  recon_sst(i) = dj*SQRT(dt)*recon_sst(i)/(Cdelta*psi0)
    !  recon_vari = recon_vari+(sst(i)-ymean-recon_sst(i))**2
    !  recon_mean = recon_mean + recon_sst(i)
    !end do
    !recon_mean = recon_mean/n
    !recon_vari = SQRT(recon_vari/n)

    !do m = 64+imut+1, 64+imut*2
    !  write(16,*) recon_sst(m) - recon_mean + ymean
    !end do

    !write(6,'(A,F14.6)') ' Reconstructed mean = ', recon_mean
    !write(6,'(A,F14.6)') ' Original mean      = ', ymean
    !write(6,'(A,F14.6)') ' Root-mean-square difference of time series = ', recon_vari
    
    !** construct a new time series [Eqn(11)]
    !   check mean and RMS difference of reconstructed time series

    recon_mean=0.D0
    recon_vari = 0.D0
    do i=1,n
      recon_sst(i)=0.D0
      !do j=62,jtot
      do j=77,jtot
        recon_sst(i) = recon_sst(i)+(DBLE(wave(i,j)))/SQRT(scale(j))
      end do
      recon_sst(i) = dj*SQRT(dt)*recon_sst(i)/(Cdelta*psi0)
      recon_vari = recon_vari+(sst(i)-ymean-recon_sst(i))**2
      recon_mean = recon_mean + recon_sst(i)
    end do
    recon_mean = recon_mean/n
    recon_vari = SQRT(recon_vari/n)

    !do m = 64+imut+1, 64+imut*2
    !  write(17,*) recon_sst(m) - recon_mean + ymean
    !end do

    write(6,'(A,F14.6)') ' Reconstructed mean = ', recon_mean
    write(6,'(A,F14.6)') ' Original mean      = ', ymean
    write(6,'(A,F14.6)') ' Root-mean-square difference of time series = ', recon_vari

    do ii = 1, imut
      work4_new(ii,jj) = recon_sst(64+imut+ii) - recon_mean + ymean
    end do

  end do LOOP_JJ

  write(mtot1,rec=nd) work4_new

  end do LOOP_DATA

  call close_file(mtin1)
  call close_file(mtot1)

END PROGRAM remove_ripples_tmp10m
