! -*-F90-*-
!------------------------ bulk-n.F90 --------------------------------
!   Subroutine bulk
!
!     Calculate Latent/Sensible heat flux based on 
!          the bulk formula by Large and Yeager (2004).
!
subroutine bulk(wsx,wsy,qla,qsn,us,vs,sat,qar,wdv,slp,sst,&
     & imx,jmx,atexl.altu,altt,altq)
  !
  implicit none
  !
  integer(4), intent(in) :: imx, jmx
  real(8), intent(out) :: wsx(imx,jmx), wsy(imx,jmx)
  real(8), intent(out) :: qla(imx,jmx), qsn(imx,jmx)
  real(8), intent(in) :: us (imx,jmx), vs (imx,jmx)
  real(8), intent(in) :: sat(imx,jmx), qar(imx,jmx)
  real(8), intent(in) :: wdv(imx,jmx), slp(imx,jmx)
  real(8), intent(in) :: sst(imx,jmx)      
  real(8), intent(in)  :: atexl(imx,jmx)
  real(8), intent(in)  :: altu, altt, altq 
  !
  !   cpa : 大気比熱 (erg/g/K)=(cm^2/s^2/K) (J/Kg/K)での値の10000倍
  !
  real(8), parameter :: cpa = 1004.67d0
  !
  !   大気密度 (g/cm^3)  (Kg/m^3)での値の 0.001 倍
  !
  real(8), parameter :: rhoa = 1.205d0
  !
  real(8), parameter :: grav = 9.81d0
  real(8), parameter :: tab = 273.15d0
  real(8), parameter :: wvmin = 0.3d0 ! 海上風速の下限(m/s)
  !
  !     変数
  !
  !   sll: 水の気化潜熱 (J/Kg)
  !
  real(8) :: sll, es, qs
  !
  real(8) :: dqr, dtemp
  !
  real(8) :: qatmos, satmos, slpres, tssurf
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
  real(8) :: zrough                     ! roughness length
  real(8) :: stab
  !
  real(8) :: ustar, tstar, qstar, bstar
  real(8) :: zetau, zetat, zetaq
  real(8) :: psi_mu, psi_hu
  real(8) :: psi_mt, psi_ht
  real(8) :: psi_mq, psi_hq
  !
  real(8) :: x, xx, x2, tv
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
#ifdef OGCM_CALHEIGHT
  ! this is for GSM
!  real(8)            :: altu, altt, altq 
  real(8), parameter :: gasr = 287.04
  real(8), parameter :: bgsm = 0.995
  real(8)            :: bgsmr
  real(8)            :: psurf, pfull, height
  real(8)            :: hl1
#else /* OGCM_CALHEIGHT */
  ! this is for NCEP1 or NCEP2 and OMIP? and ECMWF?
!  real(8), parameter :: altu = 10.0d0   ! wind speed defined
!  real(8), parameter :: altt = 10.0d0    ! surface temperature defined
!  real(8), parameter :: altq = 10.0d0    ! specific humidity defined
#endif /* OGCM_CALHEIGHT */
  !
#ifdef OGCM_BULKITER
  integer(4) :: n
  integer(4), parameter :: n_itts = 2
#endif /* OGCM_BULKITER */
  !
  !  作業用変数
  !
  integer(4) :: i, j
  !
#ifdef OGCM_TAUBULK
  real(8) :: cdt(imx,jmx)
#endif /* OGCM_TAUBULK */
  !
  !----------------------------------------------------------------------
  !
  do j = 1, jmu
    do i = 1, imu
      !
      slpres = slp(i,j)
      qatmos = qa (i,j)
      satmos = sat(i,j) + tab
      !
#ifdef OGCM_CALHEIGHT
      !
      ! !!!!! do not change the line order below !!!!!
      !
      ! psurf : 海面気圧     [Pa]
      ! pfull : 一層目の気圧  [Pa]
      !
      pfull = slpres * 1.0d2
      bgsmr = 1.0d0 - bgsm
      hl1 = (bgsmr * (1.0d0 + log(pfull)) + bgsm * log(bgsm)) / bgsmr
      psurf = exp(hl1)
      !
      height = (log(psurf) - log(pfull)) &
           &  * gasr * (1.0d0 + 0.6078d0 * qatmos) * satmos / grav_mks
      !
      altu = height
      altt = height
      altq = height
      !
      ! satmos : potential temperature referred to sea level
      !
      satmos = satmos * ((psurf / pfull)**(2.0d0/7.0d0))
      slpres = psurf * 1.0d-2
      !
#endif /* OGCM_CALHEIGHT */
      !
      tssurf = sst(i,j)
      dtemp = tssurf + tab - satmos
      !
      !     海面における比湿、飽和比湿の計算
      !
      sll = 4.186d3 * (5.949d2 - 5.1d-1 * tssurf)
      es = 9.8d-1 * 6.1078d0 * 1.0d1**(7.5d0 * tssurf / (2.373d2 + tssurf))
      qs = 6.22d-1 * es / (slpres - 3.78d-1 * es)
      dqr = qs - qatmos
      !
      wv = wdvi(i,j) * 1.0d-2
      wv = max(wv, wvmin)                  ! 0.3 [m/s] floor on wind
      !
      if (atexl(i,j,1) > 0.0d0) then
        !
        tv = satmos * (1.0d0 + 0.6078d0 * qatmos)
        wv10 = wv                                              ! first guess 10m wind
        !
        cdn10 = (2.7d0 / wv10 + 0.142d0 + 0.0764d0 * wv10) &
             &      / 1.0d3                                    ! L-Y eqn. 6a
        cdn10_rt = sqrt(cdn10)
        cen10 = 34.6d0 * cdn10_rt / 1.0d3                      ! L-Y eqn. 6b
        stab = 0.5d0 + sign(0.5d0,-dtemp)
        ctn10 = (18.0d0 * stab + 32.7d0 * (1.0d0 - stab)) &
             &      * cdn10_rt / 1.0d3                         ! L-Y eqn. 6c

        cdn = cdn10                                            ! first guess for exchange coeff's at z
        ctn = ctn10
        cen = cen10
        !
#ifdef OGCM_BULKITER
        !
        do n = 1, n_itts                                       ! Monin-Obukhov iteration
          !
          cd_rt = sqrt(cdn)
          ustar = cd_rt * wv                                   ! L-Y eqn. 7a
          tstar = (ctn / cd_rt) * (-dtemp)                     ! L-Y eqn. 7b
          qstar = (cen / cd_rt) * (-dqr)                       ! L-Y eqn. 7c
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
            psi_mt = - 5.0d0 * zetat                            ! L-Y eqn. 8c
            psi_ht = - 5.0d0 * zetat                            ! L-Y eqn. 8c
          else
            psi_mt = log((1.0d0 + 2.0d0 * x + x2) &
                 & * (1.0d0 + x2) / 8.0d0) &
                 & - 2.0d0 * (atan(x) - atan(1.0d0))            ! L-Y eqn. 8d
            psi_ht = 2.0d0 * log((1.0d0 + x2) / 2.0d0)          ! L-Y eqn. 8e
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
            psi_mq = - 5.0d0 * zetaq                            ! L-Y eqn. 8c
            psi_hq = - 5.0d0 * zetaq                            ! L-Y eqn. 8c
          else
            psi_mq = log((1.0d0 + 2.0d0 * x + x2) &
                 & * (1.0d0 + x2) / 8.0d0) &
                 & - 2.0d0 * (atan(x) - atan(1.0d0))           ! L-Y eqn. 8d
            psi_hq = 2.0d0 * log((1.0d0 + x2) / 2.0d0)          ! L-Y eqn. 8e
          end if
          !
          ! re-evaluation
          !
          wv10 = wv / (1.0d0 + &
               & cdn10_rt * (log(altu / 10.0d0) - psi_mu) &
               & / karman)                                     ! L-Y eqn. 9a
          wv10 = max(wv10, wvmin)             ! 0.3 [m/s] floor on wind
          satmosu = satmos - tstar * &
               & (log(altt / altu) + psi_hu - psi_ht) / karman ! L-Y eqn. 9b
          qatmosu = qatmos - qstar * &
               & (log(altq / altu) + psi_hu - psi_hq) / karman ! L-Y eqn. 9c
          !
          tv = satmosu * (1.0d0 + 0.6078d0 * qatmosu)

          cdn10 = (2.7d0 / wv10 + 0.142d0 + 0.0764d0 * wv10) & !
               & / 1.0d3                                       ! L-Y eqn. 6a again
          cdn10_rt = sqrt(cdn10)                               !
          cen10 = 34.6d0 * cdn10_rt / 1.0d3                    ! L-Y eqn. 6b again
          stab = 0.5d0 + sign(0.5d0,zetau)
          ctn10 = (18.0d0 * stab + 32.7d0 * (1.0d0 - stab)) &
               & * cdn10_rt / 1.0d3                            ! L-Y eqn. 6c again
          zrough = 10.d0 * exp(-karman / cdn10_rt)             ! diagnostic

          xx = (log(altu / 10.0d0) - psi_mu) / karman
          cdn = cdn10 / (1.0d0 + cdn10_rt * xx)**2             ! L-Y 10a
          xx = (log(altu / 10.0d0) - psi_hu) / karman
          ctn = ctn10 / (1.0d0 + ctn10 * xx / cdn10_rt)**2     !       b
          cen = cen10 / (1.0d0 + cen10 * xx / cdn10_rt)**2     !       c

          dtemp = tssurf + tab - satmosu
          dqr = qs - qatmosu

        enddo
        !
#endif /* OGCM_BULKITER */
        !
      else
        cdn = 0.0d0
        ctn = 0.0d0
        cen = 0.0d0
      end if
      !
#ifdef OGCM_WFLUX
      !
      !     蒸発
      !
      evp(i,j) = atexl(i,j,1) * rhoa * wv * cen * dqr / ro0mks * 1.0d2
      !
#endif /* OGCM_WFLUX */
      !
      !     潜熱
      !
      if (dqr < 0.0d0) dqr = 0.0d0
      !
      qla(i,j) = - atexl(i,j,1) * rhoa * sll * cen * dqr * wv * 1.0d3
      !
      !     顕熱
      !
      qsn(i,j) = - atexl(i,j,1) * rhoa * cpa * ctn * dtemp * wv * 1.0d3
      !
#ifdef OGCM_TAUBULK
      cdt(i,j) =   atexl(i,j,1) * rhoa * cdn * wv ! MKS
#endif /* OGCM_TAUBULK */
      !
    end do
  end do
  !
#ifdef OGCM_TAUBULK
  !
  ! U10, V10 => TAU_X, TAU_Y
  !
  do j = 1, jmu - 1
    do i = 1, imu - 1
      cdn = cdt(i,j)

      ! 風速が観測された高度で求める
      ! strsx, strsy には cgs で10m風速が入っていると仮定(2004-11-15)

      wsx(i,j) = cdn * wsx(i,j) * 1.d-2 * 10.d0
      wsy(i,j) = cdn * wsy(i,j) * 1.d-2 * 10.d0 ! [MKS] -> [cgs]

    end do
  end do

#endif /* OGCM_TAUBULK */
  !
end subroutine bulk
