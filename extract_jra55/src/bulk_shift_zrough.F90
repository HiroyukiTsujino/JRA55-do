! -*-F90-*-
!------------------------ bulk-ncar-shift.F90 ----------------------------
! Information:
!   Shift 2m temperature and specific humidity to those at 10m.
!
subroutine bulk_shift(tmptrg,sphtrg, &
     & sat,qar,wdv,tau,slp,sst,zrough,sphsrf,&
     & sens, evap, &
     & aexl,ice,num_data,altu,altt,altq,alt_target)

  implicit none

  integer(4), intent(in) :: num_data
  real(8), intent(out) :: tmptrg(num_data), sphtrg(num_data)
  real(8), intent(in)  :: sat(num_data) ! [degC]
  real(8), intent(in)  :: qar(num_data)
  real(8), intent(in)  :: wdv(num_data)
  real(8), intent(in)  :: slp(num_data) ! [hPa]
  real(8), intent(in)  :: tau(num_data)
  real(8), intent(in)  :: sst(num_data) ! [degC]
  real(8), intent(in)  :: zrough(num_data) ! roughness length
  ! sensible heat and evaporation : negative for the "gain" on the atmospheric side
  real(8), intent(in)  :: sens(num_data), evap(num_data)
  real(8), intent(in)  :: ice(num_data)
  real(8), intent(in)  :: aexl(num_data)
  real(8), intent(in)  :: altu, altt, altq
  real(8), intent(in)  :: alt_target
  real(8), intent(out) :: sphsrf(num_data)

  real(8) :: aexl_tmp(num_data)

  ! [cgs]

  real(8), parameter :: grav = 981.0d0

  ! ...

  real(8), parameter :: tab = 273.15d0

  ! [MKS]

  real(8), parameter :: grav_mks = grav * 1.0d-2
  real(8), parameter :: cpa_mks = 1004.67d0 ! specific heat of air (J/Kg/K)
  real(8), parameter :: gasr = 287.04d0

  real(8), parameter :: sphmin = 5.0d-5 ! minimum of specific humidity

  ! parameters to calculate saturation water vapor presure

  real(8), parameter :: ali1=0.7859d0, ali2=0.03477d0, ali3=0.00412d0, ali4=1.0d0
  real(8), parameter :: bli1=1.d-6, bli2=4.5d0, bli3=0.0006d0
  real(8), parameter :: cli1=2.839d6, cli2=3.6d0, cli3=35.0d0
  real(8), parameter :: ewa = 0.62197d0    !Molecular Weight Ratio Water/DryAir

  ! work variables

  real(8) :: es, qs
  real(8) :: ewp1, ewp2
  !
  real(8) :: dqr, dtemp
  !
  real(8) :: qatmos, satmos, slpres, tssurf
  real(8) :: rhoair
  !
  ! defined at 10 [m]
  !
  real(8) :: cdn10, cen10, ctn10
  real(8) :: cdn10_rt
  real(8) :: wv10
  !
  ! defined at velocity level
  !
  real(8) :: qatmosu, satmosu
  real(8) :: cdn, cen, ctn
  real(8) :: cd_rt
  real(8) :: wv
  !
  real(8), parameter :: karman = 0.4    ! vor Karman constant
  real(8) :: stab
  !
  real(8) :: ustar, tstar, qstar, bstar
  real(8) :: zetau, zetat, zetaq, zetatrg
  real(8) :: psi_mu, psi_hu
  real(8) :: psi_mt, psi_ht
  real(8) :: psi_mq, psi_hq
  real(8) :: psi_mtrg, psi_htrg
  !
  real(8) :: x, xx, x2, tv

  integer(4) :: m
  !
  ! 入力（単位に注意！  用意するデータの単位を変更するか、
  !                     このサブルーチンで変更すること）
  !     TBL     :  Model SST [degC]
  !     SAT,SATA:  Observed Air Temperature [degC]
  !     DWT,DWTA:  Observed Specific Humidity [g/g]
  !     WDV,WDVA:  Observed Wind Speed [cm/s]
  !     SLP,SLPA:  Observed sea level pressure [hPa]
  ! 出力
  !     QSN     :  Sensible Heat Flux [g/s3] （W/m2での1000倍の値）
  !     QLA     :  Latent Heat Flux   [g/s3] （W/m2での1000倍の値）
  !
  !       ES : 飽和水蒸気圧 (hPa) (!! MKS !!)
  !       QS : 海面における飽和比湿 (g/kg)
  !       WV : 海上風速 (cm/s)
  !       CDN,CEN: バルク係数（無次元）
  !       DQR,DTEMP: 大気海洋間の比湿差、温度差
  !
  !----------------------------------------------------------------------

  aexl_tmp(1:num_data) = aexl(1:num_data)

!  aexl_tmp(1:num_data) = 0.0d0

!$omp parallel
!$omp do private(slpres,qatmos,satmos, &
!$omp & tssurf, dtemp, es, qs, dqr, wv, tv, wv10, &
!$omp & cdn10, cdn10_rt, cen10, stab, ctn10, cdn, ctn, cen, &
!$omp & cd_rt, ustar, tstar, qstar, bstar, zetau, x2, x, xx, &
!$omp & psi_mu, psi_hu, zetat, psi_mt, psi_ht, &
!$omp & zetatrg, psi_mtrg, psi_htrg, &
!$omp & zetaq, psi_mq, psi_hq, satmosu, qatmosu, rhoair)
  do m = 1, num_data

    slpres = slp(m)
    qatmos = qar(m)
    satmos = sat(m) + tab
    tssurf = sst(m)

    ! calculcate surface saturated specific humidity

    if (ice(m) == 0.0d0) then
      es = 9.8d-1 * 6.1078d0 * 1.0d1**(7.5d0 * tssurf / (2.373d2 + tssurf))
      qs = ewa * es / (slpres - 3.78d-1 * es)
    else
      ewp1 = 10.D0**((ali1 + ali2 * tssurf) / (1.0d0 + ali3 * tssurf))  ! [hPa]
      ewp2 = ewp1 * (1.0d0 + bli1 * slpres * (bli2 + bli3 * tssurf**2)) ! [hPa]
      qs = ewa * ewp2 / (slpres - (1.0d0 - ewa) * ewp2)
    end if

    sphsrf(m) = qs

    if (aexl_tmp(m) == 1.0d0) then ! ocean

      dtemp = tssurf + tab - satmos
      dqr = qs - qatmos

      tmptrg(m) = tssurf - dtemp * log(alt_target / zrough(m)) / log(altt / zrough(m))
      sphtrg(m) = qs - dqr * log(alt_target / zrough(m)) / log(altq / zrough(m))

    else ! land

      tv = satmos * (1.0d0 + 0.6078d0 * qatmos)              ! virtual temperature
      rhoair = (slpres * 1.0d2) / gasr / tv

      ustar = sqrt(tau(m)/rhoair)                          ! L-Y eqn. 7a
      tstar = sens(m) / rhoair / cpa_mks / ustar           ! L-Y eqn. 7b
      qstar = evap(m) / rhoair / ustar                     ! L-Y eqn. 7c

      bstar = grav_mks * &
           & ( tstar / tv + qstar / (qatmos + 1.0d0 / 0.6078d0))
      !
      ! velocity level
      !
      zetau = karman * bstar * altu / (ustar * ustar)      ! L-Y eqn. 8a
      zetau = sign(min(abs(zetau),10.d0),zetau)            ! undocumented NCAR
      x2 = sqrt(abs(1.0d0 - 16.0d0 * zetau))               ! L-Y eqn. 8b
      x2 = max(x2,1.0d0)                                   ! undocumented NCAR
      x = sqrt(x2)
      if (zetau > 0.0d0) then
        psi_mu = - 5.0d0 * zetau                           ! L-Y eqn. 8c
        psi_hu = - 5.0d0 * zetau                           ! L-Y eqn. 8c
      else
        psi_mu = log((1.0d0 + 2.0d0 * x + x2) &
             & * (1.0d0 + x2) / 8.0d0) &
             & - 2.0d0 * (atan(x) - atan(1.0d0))           ! L-Y eqn. 8d
        psi_hu = 2.0d0 * log((1.0d0 + x2) / 2.0d0)         ! L-Y eqn. 8e
      end if
      !
      ! temperature level
      !
      zetat = karman * bstar * altt / (ustar * ustar)      ! L-Y eqn. 8a
      zetat = sign(min(abs(zetat),10.d0),zetat)            ! undocumented NCAR
      x2 = sqrt(abs(1.0d0 - 16.0d0 * zetat))               ! L-Y eqn. 8b
      x2 = max(x2,1.0d0)                                   ! undocumented NCAR
      x = sqrt(x2)
      if (zetat > 0.0d0) then
        psi_mt = - 5.0d0 * zetat                           ! L-Y eqn. 8c
        psi_ht = - 5.0d0 * zetat                           ! L-Y eqn. 8c
      else
        psi_mt = log((1.0d0 + 2.0d0 * x + x2) &
             & * (1.0d0 + x2) / 8.0d0) &
             & - 2.0d0 * (atan(x) - atan(1.0d0))           ! L-Y eqn. 8d
        psi_ht = 2.0d0 * log((1.0d0 + x2) / 2.0d0)         ! L-Y eqn. 8e
      end if
      !
      ! water vapor level
      !
      zetaq = karman * bstar * altq / (ustar * ustar)      ! L-Y eqn. 8a
      zetaq = sign(min(abs(zetaq),10.d0),zetaq)            ! undocumented NCAR
      x2 = sqrt(abs(1.0d0 - 16.0d0 * zetaq))               ! L-Y eqn. 8b
      x2 = max(x2,1.0d0)                                   ! undocumented NCAR
      x = sqrt(x2)
      if (zetaq > 0.0d0) then
        psi_mq = - 5.0d0 * zetaq                           ! L-Y eqn. 8c
        psi_hq = - 5.0d0 * zetaq                           ! L-Y eqn. 8c
      else
        psi_mq = log((1.0d0 + 2.0d0 * x + x2) &
             & * (1.0d0 + x2) / 8.0d0) &
             & - 2.0d0 * (atan(x) - atan(1.0d0))           ! L-Y eqn. 8d
        psi_hq = 2.0d0 * log((1.0d0 + x2) / 2.0d0)         ! L-Y eqn. 8e
      end if
      !
      ! target level
      !
      zetatrg = karman * bstar * alt_target / (ustar * ustar)   ! L-Y eqn. 8a
      zetatrg = sign(min(abs(zetatrg),10.d0),zetatrg)           ! undocumented NCAR
      x2 = sqrt(abs(1.0d0 - 16.0d0 * zetatrg))                  ! L-Y eqn. 8b
      x2 = max(x2,1.0d0)                                        ! undocumented NCAR
      x = sqrt(x2)
      if (zetatrg > 0.0d0) then
        psi_mtrg = - 5.0d0 * zetatrg                            ! L-Y eqn. 8c
        psi_htrg = - 5.0d0 * zetatrg                            ! L-Y eqn. 8c
      else
        psi_mtrg = log((1.0d0 + 2.0d0 * x + x2) &
             & * (1.0d0 + x2) / 8.0d0) &
             & - 2.0d0 * (atan(x) - atan(1.0d0))                ! L-Y eqn. 8d
        psi_htrg = 2.0d0 * log((1.0d0 + x2) / 2.0d0)            ! L-Y eqn. 8e
      end if

      ! neglect stability

      tmptrg(m) = satmos - tab - tstar * (log(altt / alt_target)) / karman ! L-Y eqn. 9b
      sphtrg(m) = qatmos - qstar * (log(altq / alt_target)) / karman ! L-Y eqn. 9c
 
      !tmptrg(m) = satmos - tab - tstar * &
      !     & (log(altt / alt_target) + psi_htrg - psi_ht) / karman ! L-Y eqn. 9b
      !sphtrg(m) = qatmos - qstar * &
      !     & (log(altq / alt_target) + psi_htrg - psi_hq) / karman ! L-Y eqn. 9c

    end if

    sphtrg(m) = max(sphtrg(m),sphmin)

!    if (sphtrg(m) < 0.0d0) then
!    if (sphtrg(m) < sphmin) then
!
!      !write(6,*)  ' !!!!! WARNING !!!!! '
!      !write(6,'(1a,i8,1a,i8,1a,e10.3,1a,f8.3,f5.1)')  ' humidity at ', m, '/', num_data, &
!      !     & ' is negative ', real(sphtrg(m),4), ' temp = ', real(tmptrg(m),4), real(aexl(m),4)
!      write(6,'(1a,i8,1a,i8,1a,e8.3,1a,f8.3,f5.1)')  ' humidity at ', m, '/', num_data, &
!           & ' is replaced with ', real(sphmin,4), ' temp = ', real(tmptrg(m),4), real(aexl(m),4)
!      sphtrg(m) = sphmin
!
!      write(15,*)
!
!      if (aexl(m) == 1.0d0) then
!        write(15,*) 'Ocean', m, ice(m)
!        write(15,*) 'qar ', real(sphtrg(m),4), real(qatmos,4), real(qs,4)
!        write(15,*) 'sat ', real(tmptrg(m),4), real(satmos-tab,4),real(tssurf,4)
!      else
!        write(15,*) 'Land', m, ice(m)
!        write(15,*) 'qar ', real(sphtrg(m),4), real(qatmos,4), real(qs,4)
!        write(15,*) 'sat ', real(tmptrg(m),4), real(satmos-tab,4),real(tssurf,4)
!        write(15,*) 'qstar ', real(qstar,4)
!        write(15,*) 'tstar ', real(tstar,4)
!        write(15,*) 'bstar ', real(bstar,4)
!        write(15,*) 'ustar ', real(ustar,4)
!        write(15,*) 'psi(10), psi(2) ', psi_htrg, psi_hq
!        write(15,*) 'factor ', (log(altq / alt_target) + psi_htrg - psi_hq) / karman
!        write(15,*) 'zetaq ', real(zetaq,4)
!        write(15,*) 'zetatrg ', real(zetatrg,4)
!      end if
!      write(15,*) 'sens, evap ', real(sens(m),4), real(evap(m),4)
!
!    end if

  end do

!$omp end parallel

end subroutine bulk_shift
