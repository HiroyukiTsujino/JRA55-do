! -*-F90-*-
!------------------------ bulk-ncar-shift.F90 ----------------------------
!   Subroutine bulk
!
!     Calculate Latent/Sensible heat flux based on 
!          the bulk formula by Large and Yeager (2004).
!
!   About CPP directives
!     - LYCOEF    : Compute properties of moist air based on Large and Yeager (2004;2009)
!     - CALHEIGHT : Estimate the height of the bottom level (where surface variables are defined)
!                    of JMA global spectral models (This option will not be usually used.)
!
!   Input:
!     us   : zonal wind speed (m s-1)
!     vs   : meridional wind speed (m s-1)
!     sat  : Surface air temperature (degree Celsius)
!     qar  : Specific humidity of surface air (kg kg-1)
!     wdv  : Scalar wind speed (m s-1)
!     slp  : Sea level pressure (hPa)
!     sst  : Sea surface temperature (degree Celsius)
!     imx,jmx : Size of the arrays (integer)
!     atexl: land-sea mask (0:land, 1:sea)
!     altu : Observation height of wind (m)
!     altt : Observation height of air temperature (m)
!     altq : Observation height of specific humidity (m)
!
!   Output:
!     tmptrg : Height shifted air temperature (degree Celsius)
!     sphtrg : Height shifted specific humidity (kg kg-1)
!     tu   : Air temperature at the height where wind is observed (degree Celsius)
!     qu   : Specific humidity at the height where wind is observed (kg kg-1)
!     dtu  : Difference of temperature between air temperature and sea surface temperature (degree Celsius)
!     dqu  : Difference of specific humidity between air specific humidity
!          :  and saturation specific humidity at the sea surface  (kg kg-1)
!
subroutine bulk_shift(tmptrg,sphtrg, &
     & us,vs,sat,qar,wdv,slp,sst,ice,&
     & imx,jmx,atexl,altu,altt,altq,alt_target,sphmin)

  implicit none

  integer(4), intent(in) :: imx, jmx
  real(8), intent(out) :: tmptrg(imx,jmx), sphtrg(imx,jmx)
  real(8), intent(in)  :: us (imx,jmx), vs (imx,jmx)
  real(8), intent(in)  :: sat(imx,jmx), qar(imx,jmx)
  real(8), intent(in)  :: wdv(imx,jmx), slp(imx,jmx)
  real(8), intent(in)  :: sst(imx,jmx), ice(imx,jmx)
  real(8), intent(in)  :: atexl(imx,jmx)
  real(8), intent(in)  :: altu, altt, altq 
  real(8), intent(in)  :: alt_target
  real(8), intent(in)  :: sphmin

  !   cpa  : 大気比熱 (erg/g/K)=(cm^2/s^2/K) (J/Kg/K)での値の10000倍
  !   rhoa : 大気密度 (g/cm^3)  (Kg/m^3)での値の 0.001 倍

  ! [cgs]

  real(8), parameter :: RO   = 1.036d0       !  水の密度
  real(8), parameter :: grav = 981.0d0

  ! ...

  !   cpa  : 大気比熱 (erg/g/K)=(cm^2/s^2/K) (J/Kg/K)での値の10000倍
  !   rhoa : 大気密度 (g/cm^3)  (Kg/m^3)での値の 0.001 倍

  real(8), parameter :: rhoa = 1.22d0  ! L-Y
  real(8), parameter :: tab = 273.15d0
#ifdef OGCM_LYCOEF
  real(8), parameter :: sll_cnst = 2.5d6 ! L-Y
  !real(8), parameter :: cpa = 1000.5d0 ! L-Y
#else /* OGCM_LYCOEF */
  !real(8), parameter :: cpa = 1004.67d0 ! MRI.COM 
#endif /* OGCM_LYCOEF */
  real(8) :: cpa

  real(8), parameter :: mwwater = 18.016d0  ! molecular weight of water vapor
  real(8), parameter :: mwair = 28.966d0    ! modecular weight of air

  ! [MKS]

  real(8), parameter :: rhoa_mks = rhoa     ! 大気密度 (kg/m^3)
  real(8), parameter :: ro0mks = ro * 1.d3
  real(8), parameter :: grav_mks = grav * 1.0d-2
!  real(8), parameter :: cpa_mks = cpa ! 大気比熱 (J/Kg/K)
  real(8), parameter :: gasr = 287.04d0

  real(8), parameter :: wvmin = 0.3d0 ! 海上風速の下限(m/s)
  real(8), save :: eps_air   ! = 0.62197
  real(8), save :: tvq_air   ! = 0.6078

  !J 海氷上の飽和蒸気圧を求めるために必要なパラメータ

  ! Gill (1982) Appendix 4
  real(8), parameter :: agill1 = 0.7859d0, agill2 = 0.03477d0, agill3 = 0.00412d0 ! saturation vapor pressure
  real(8), parameter :: bgill0 = 0.98d0 ! 2 % reduction for saturation vapor pressure over sea water
  real(8), parameter :: bgill1 = 1.d-6, bgill2 = 4.5d0, bgill3 = 0.0006d0 ! correction of vapor pressure
  real(8), parameter :: cgill1 = 2.5008d6, cgill2 = -2.3d3   ! for latent heat of vaporization
  real(8), parameter :: dgill1 = 1004.6d0, dgill2 = 0.8735d0 ! for specific heat (Gill 1982, P.43)
  real(8), parameter :: igill1 = 0.00422d0

!  real(8), parameter :: ali1=0.7859d0, ali2=0.03477d0, ali3=0.00412d0, ali4=1.0d0
!  real(8), parameter :: bli1=1.d-6, bli2=4.5d0, bli3=0.0006d0
!  real(8), parameter :: cli1=2.839d6, cli2=3.6d0, cli3=35.0d0
!  real(8), parameter :: ewa = 0.62197d0    !Molecular Weight Ratio Water/DryAir
!  real(8), parameter :: temp_freeze = -2.0d0

  !     変数

  !   sll: 水の気化潜熱 (J/Kg)

  real(8) :: sll, es, qs
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
  real(8) :: wv10n
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
  real(8) :: zetau, zetat, zetaq, zetatrg
  real(8) :: psi_mu, psi_hu
  real(8) :: psi_mt, psi_ht
  real(8) :: psi_mq, psi_hq
  real(8) :: psi_mtrg, psi_htrg
  !
  real(8) :: x, xx, x2, tv
  !
  ! 入力（単位に注意！  用意するデータの単位を変更するか、
  !                     このサブルーチンで変更すること）
  !     TBL     :  Model SST [degC]
  !     SAT,SATA:  Observed Air Temperature [degC]
  !     DWT,DWTA:  Observed Specific Humidity [kg/kg]
  !     WDV,WDVA:  Observed Wind Speed [m/s]
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
  real(8), parameter :: bgsm = 0.995
  real(8)            :: bgsmr
  real(8)            :: psurf, pfull, height
#else /* OGCM_CALHEIGHT */
  ! this is for NCEP1 or NCEP2 and OMIP? and ECMWF?
!  real(8), parameter :: altu = 10.0d0   ! wind speed defined
!  real(8), parameter :: altt = 10.0d0    ! surface temperature defined
!  real(8), parameter :: altq = 10.0d0    ! specific humidity defined
!  real(8), parameter :: alt2 =  2.0d0 ! 2m
#endif /* OGCM_CALHEIGHT */
  !
#ifdef OGCM_BULKITER
  integer(4) :: n
  integer(4), parameter :: n_itts = 5
  real(8), parameter :: inc_ratio = 1.0d-4
  real(8) :: test, cdn_prv
#endif /* OGCM_BULKITER */
  !
  !  作業用変数
  !
  integer(4) :: i, j
  real(8)            :: hl1, hl2
  !
#ifdef OGCM_TAUBULK
  real(8) :: cdt(imx,jmx)
#endif /* OGCM_TAUBULK */
  !
  !----------------------------------------------------------------------

  eps_air = mwwater / mwair
  tvq_air = 1.0d0/eps_air - 1.0d0

  !
!$omp parallel
!$omp do private(j,i,n,slpres,qatmos,satmos,&
!$omp & tssurf, dtemp, hl1, hl2, sll, es, qs, dqr, wv, tv, wv10n, &
!$omp & cdn10, cdn10_rt, cen10, stab, ctn10, cdn, ctn, cen, &
!$omp & cd_rt, ustar, tstar, qstar, bstar, zetau, x2, x, xx, &
!$omp & psi_mu, psi_hu, zetat, psi_mt, psi_ht, &
!$omp & zetatrg, psi_mtrg, psi_htrg, &
!$omp & zetaq, psi_mq, psi_hq, satmosu, qatmosu, zrough, rhoair, cdn_prv, test)
  do j = 1, jmx
    do i = 1, imx

      !if (atexl(i,j) == 1.0d0) then

        slpres = slp(i,j)
        qatmos = qar(i,j)
        satmos = sat(i,j) + tab
        !
#ifdef OGCM_CALHEIGHT
        !
        ! !!!!! do not change the line order below !!!!!
        !
        ! psurf : Sea level pressure [Pa]
        ! pfull : Pressure at the first level [Pa]
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
        sll = cgill1 + cgill2 * tssurf
#ifdef OGCM_LYCOEF
        rhoair = (slpres * 1.0d2)/gasr/satmos/(1.0d0+tvq_air*qatmos)
        if (ice(i,j) == 0.0d0) then
          ! ice free
          if (atexl(i,j) == 1.0d0) then
            qs = 0.98d0 * 6.40380d5 * exp(-5107.4d0 / (tssurf + tab)) / rhoair
          else
            qs = 6.40380d5 * exp(-5107.4d0 / (tssurf + tab)) / rhoair
          end if
        else
          ! ice
          qs = 11637800.0d0 * exp(-5897.8d0 / (tssurf + tab)) / rhoair
        end if
#else /* OGCM_LYCOEF */
        !!!!!es = 9.8d-1 * 6.1078d0 * 1.0d1**(7.5d0 * tssurf / (2.373d2 + tssurf))
        !!!!!qs = 6.22d-1 * es / (slpres - 3.78d-1 * es)
        if (ice(i,j) == 0.0d0) then
          ! ice free
          hl2 = (agill1 + agill2 * tssurf) / (1.0d0 + agill3 * tssurf)
          hl1 = 10.D0 ** hl2
          if (atexl(i,j) == 1.0d0) then
            es = hl1 * bgill0 * (1.0d0 + bgill1 * slpres * (bgill2 + bgill3 * tssurf**2)) ! [hPa]
          else
            es = hl1 * (1.0d0 + bgill1 * slpres * (bgill2 + bgill3 * tssurf**2)) ! [hPa]
          end if
        else
          ! ice
          hl2 = (agill1 + agill2 * tssurf) / (1.0d0 + agill3 * tssurf) + igill1 * tssurf
          hl1 = 10.D0 ** hl2
          es = hl1 * (1.0d0 + bgill1 * slpres * (bgill2 + bgill3 * tssurf**2)) ! [hPa]
        end if
        qs = eps_air * es / (slpres - (1.0d0 - eps_air) * es)
#endif /* OGCM_LYCOEF */
!        if (tssurf > temp_freeze) then
!          sll = 4.186d3 * (5.949d2 - 5.1d-1 * tssurf)
!          es = 9.8d-1 * 6.1078d0 * 1.0d1**(7.5d0 * tssurf / (2.373d2 + tssurf))
!          qs = ewa * es / (slpres - 3.78d-1 * es)
!        else
!          ewp1 = 10.D0**((ali1 + ali2 * tssurf) / (1.0d0 + ali3 * tssurf))  ! [hPa]
!          ewp2 = ewp1 * (1.0d0 + bli1 * slpres * (bli2 + bli3 * tssurf**2)) ! [hPa]
!          qs = ewa * ewp2 / (slpres - (1.0d0 - ewa) * ewp2)
!        end if

        dqr = qs - qatmos
        !
        wv = wdv(i,j)
        !!!!!wv = (wdv(i,j) + min(max(dtemp,0.0d0),2.0d0)) * 1.0d-2
        wv = max(wv, wvmin)                  ! 0.3 [m/s] floor on wind
        !
        tv = satmos * (1.0d0 + tvq_air * qatmos)
        !tv = satmos * (1.0d0 + 0.6078d0 * qatmos)
        qatmosu = qatmos

        wv10n = wv                                              ! first guess 10m wind
        !
        if (ice(i,j) == 0.0d0) then
          !cdn10 = (2.7d0 / wv10n + 0.142d0 + 0.0764d0 * wv10n) &
          !     &      / 1.0d3                                    ! L-Y eqn. 6a
          hl1 = (2.7d0 / wv10n + 0.142d0 + 0.0764d0 * wv10n - 3.14807d-10 * (wv10n**6)) &
               &      / 1.0d3                                    ! LY2009 eqn. 11a
          cdn10 = (0.5d0 - sign(0.5d0,wv10n-33.0d0)) * hl1 &
               & + (0.5d0 + sign(0.5d0,wv10n-33.0d0)) * 2.34d-3   ! LY2009 eqn. 11b
          cdn10_rt = sqrt(cdn10)
          cen10 = 34.6d0 * cdn10_rt / 1.0d3                      ! L-Y eqn. 6b
          stab = 0.5d0 + sign(0.5d0,-dtemp)
          ctn10 = (18.0d0 * stab + 32.7d0 * (1.0d0 - stab)) &
               &      * cdn10_rt / 1.0d3                         ! L-Y eqn. 6c
        else
          cdn10 = 1.63d-3 ! L-Y eqn. 21
          cen10 = 1.63d-3 ! L-Y eqn. 21
          ctn10 = 1.63d-3 ! L-Y eqn. 21
          cdn10_rt = sqrt(cdn10)
        end if

        cdn = cdn10                                            ! first guess for exchange coeff's at z
        ctn = ctn10
        cen = cen10

        cdn_prv = cdn

#ifdef OGCM_BULKITER
        !
        LOOP_ADJUST: do n = 1, n_itts                                       ! Monin-Obukhov iteration
          !
          cd_rt = sqrt(cdn)
          ustar = cd_rt * wv                                   ! L-Y eqn. 7a
          tstar = (ctn / cd_rt) * (-dtemp)                     ! L-Y eqn. 7b
          qstar = (cen / cd_rt) * (-dqr)                       ! L-Y eqn. 7c
          !bstar = grav_mks * &
          !     & ( tstar / tv + qstar / (qatmos + 1.0d0 / 0.6078d0))
          bstar = grav_mks * &
               & ( tstar / tv + qstar / (qatmosu + 1.0d0 / tvq_air))
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
          ! target level
          !
          zetatrg = karman * bstar * alt_target / (ustar * ustar)      ! L-Y eqn. 8a
          zetatrg = sign(min(abs(zetatrg),10.d0),zetatrg)            ! undocumented NCAR
          x2 = sqrt(abs(1.0d0 - 16.0d0 * zetatrg))               ! L-Y eqn. 8b
          x2 = max(x2,1.0d0)                                   ! undocumented NCAR
          x = sqrt(x2)
          if (zetatrg > 0.0d0) then
            psi_mtrg = - 5.0d0 * zetatrg                            ! L-Y eqn. 8c
            psi_htrg = - 5.0d0 * zetatrg                            ! L-Y eqn. 8c
          else
            psi_mtrg = log((1.0d0 + 2.0d0 * x + x2) &
                 & * (1.0d0 + x2) / 8.0d0) &
                 & - 2.0d0 * (atan(x) - atan(1.0d0))           ! L-Y eqn. 8d
            psi_htrg = 2.0d0 * log((1.0d0 + x2) / 2.0d0)          ! L-Y eqn. 8e
          end if
          !
          ! re-evaluation
          !
          wv10n = wv / (1.0d0 + &
               & cdn10_rt * (log(altu / 10.0d0) - psi_mu) &
               & / karman)                                     ! L-Y eqn. 9a
          wv10n = max(wv10n, wvmin)             ! 0.3 [m/s] floor on wind
          satmosu = satmos - tstar * &
               & (log(altt / altu) + psi_hu - psi_ht) / karman ! L-Y eqn. 9b
          qatmosu = qatmos - qstar * &
               & (log(altq / altu) + psi_hu - psi_hq) / karman ! L-Y eqn. 9c
          !
          tv = satmosu * (1.0d0 + tvq_air * qatmosu)
        
          if (ice(i,j) == 0.0d0) then
             !!!!!cdn10 = (2.7d0 / wv10n + 0.142d0 + 0.0764d0 * wv10n) & !
             !!!!!     & / 1.0d3                                       ! L-Y eqn. 6a again
            hl1 = (2.7d0 / wv10n + 0.142d0 + 0.0764d0 * wv10n - 3.14807d-10 * (wv10n**6)) &
                 &      / 1.0d3                                    ! LY2009 eqn. 11a
            cdn10 = (0.5d0 - sign(0.5d0,wv10n-33.0d0)) * hl1 &
               & + (0.5d0 + sign(0.5d0,wv10n-33.0d0)) * 2.34d-3  ! LY2009 eqn. 11b
            cdn10_rt = sqrt(cdn10)                               !
            cen10 = 34.6d0 * cdn10_rt / 1.0d3                    ! L-Y eqn. 6b again
            stab = 0.5d0 + sign(0.5d0,zetau)
            ctn10 = (18.0d0 * stab + 32.7d0 * (1.0d0 - stab)) &
                 & * cdn10_rt / 1.0d3                            ! L-Y eqn. 6c again
          else
            cdn10 = 1.63d-3 ! L-Y eqn. 21
            cen10 = 1.63d-3 ! L-Y eqn. 21
            ctn10 = 1.63d-3 ! L-Y eqn. 21
            cdn10_rt = sqrt(cdn10)
          end if

          zrough = 10.d0 * exp(- karman / cdn10_rt)            ! diagnostic
        
          xx = (log(altu / 10.0d0) - psi_mu) / karman
          cdn = cdn10 / (1.0d0 + cdn10_rt * xx)**2             ! L-Y 10a
          xx = (log(altu / 10.0d0) - psi_hu) / karman
          !!!!!ctn = ctn10 / (1.0d0 + ctn10 * xx / cdn10_rt)**2     !       b
          !!!!!cen = cen10 / (1.0d0 + cen10 * xx / cdn10_rt)**2     !       c
          ctn = ctn10 / (1.0d0 + ctn10 * xx / cdn10_rt) * sqrt(cdn / cdn10)  !       b
          cen = cen10 / (1.0d0 + cen10 * xx / cdn10_rt) * sqrt(cdn / cdn10)  !       c
        
          dtemp = tssurf + tab - satmosu
          dqr = qs - qatmosu

!          rhoair = (slpres * 1.0d2)/gasr/satmosu/(1.0d0+0.6078d0*qatmosu)
          rhoair = (slpres * 1.0d2)/gasr/tv

          test = abs(cdn - cdn_prv) / (cdn + 1.0d-8)

          if (test < inc_ratio) exit LOOP_ADJUST

          cdn_prv = cdn

        end do LOOP_ADJUST

#endif /* OGCM_BULKITER */

        tmptrg(i,j) = satmos - tab - tstar * &
               & (log(altt / alt_target) + psi_htrg - psi_ht) / karman ! L-Y eqn. 9b
        sphtrg(i,j) = qatmos - qstar * &
               & (log(altq / alt_target) + psi_htrg - psi_hq) / karman ! L-Y eqn. 9c
        sphtrg(i,j) = max(sphtrg(i,j),sphmin)

        !if ((j == 180) .and. (i == 320)) then
        !  write(6,*) qs, sphtrg(i,j), qar(i,j)
        !end if


      !else
      !
      !  tmptrg(i,j) = 0.0d0
      !  sphtrg(i,j) = 0.0d0
      !
      !end if

    end do
  end do
!$omp end parallel

end subroutine bulk_shift
