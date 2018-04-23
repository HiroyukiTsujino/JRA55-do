! -*-F90-*-
!--------------- shift_wind_neutral_to_real.F90 -----------------------
!   Subroutine bulk
!
!     Equivalent neutral wind is adjusted to real wind
!      by taking into account of stability.
!     The bulk formula by Large and Yeager (2004,2009) is used.
!
subroutine wind_neutral_to_real( &
     & wdvtrg,tmptrg,sphtrg, &
     & wdv,sat,qar,slp,sst,ice,&
     & imx,jmx,atexl,altu,altt,altq,alt_target,sphmin)

  implicit none

  integer(4), intent(in) :: imx, jmx
  real(8), intent(out) :: wdvtrg(imx,jmx)
  real(8), intent(out) :: tmptrg(imx,jmx), sphtrg(imx,jmx)
  real(8), intent(in)  :: wdv(imx,jmx)
  real(8), intent(in)  :: sat(imx,jmx), qar(imx,jmx)
  real(8), intent(in)  :: slp(imx,jmx)
  real(8), intent(in)  :: sst(imx,jmx)
  real(8), intent(in)  :: ice(imx,jmx)
  real(8), intent(in)  :: atexl(imx,jmx)
  real(8), intent(in)  :: altu, altt, altq 
  real(8), intent(in)  :: alt_target
  real(8), intent(in)  :: sphmin

  ! [cgs]

  real(8), parameter :: RO   = 1.036d0      ! Water density
  real(8), parameter :: grav = 981.0d0

  ! ...

  ! Properties of moist air

  real(8), parameter :: cpa_mks = 1004.67d0 ! specific humidity of air [J/Kg/K]
  real(8), parameter :: rhoa_mks = 1.22d0   ! Air density [kg/m^3] This is based on Large and Yeager (2004)
  real(8), parameter :: gasr = 287.04d0     ! the gas constant for dry air [J/kg/K]
  real(8), parameter :: mwwater = 18.016d0  ! molecular weight of water vapor
  real(8), parameter :: mwair = 28.966d0    ! modecular weight of air

  real(8), parameter :: tab = 273.15d0

  ! [MKS]

  real(8), parameter :: ro0mks = ro * 1.d3
  real(8), parameter :: grav_mks = grav * 1.0d-2

  real(8), parameter :: wvmin = 0.3d0 ! floor on wind
  real(8), save :: eps_air   ! = 0.62197
  real(8), save :: tvq_air   ! = 0.6078

  ! Gill (1982) Appendix 4
  real(8), parameter :: agill1 = 0.7859d0, agill2 = 0.03477d0, agill3 = 0.00412d0 ! saturation vapor pressure
  real(8), parameter :: bgill0 = 0.98d0 ! 2 % reduction for saturation vapor pressure over sea water
  real(8), parameter :: bgill1 = 1.d-6, bgill2 = 4.5d0, bgill3 = 0.0006d0 ! correction of vapor pressure
  real(8), parameter :: cgill1 = 2.5008d6, cgill2 = -2.3d3   ! for latent heat of vaporization
  real(8), parameter :: dgill1 = 1004.6d0, dgill2 = 0.8735d0 ! for specific heat (Gill 1982, P.43)
  real(8), parameter :: igill1 = 0.00422d0

  !   sll: latent heat of vaporization [J/Kg]

  real(8) :: sll, es, qs
  real(8) :: ewp1, ewp2
  !
  real(8) :: dqr, dtemp
  !
  real(8) :: qatmos, satmos, slpres, tssurf
  !
  ! defined at 10 [m]
  !
  real(8) :: cdn10, cen10, ctn10
  real(8) :: cdn10_rt
  real(8) :: wv10n ! neutral wind
  !
  ! defined at velocity level
  !
  real(8) :: qatmosu, satmosu
  real(8) :: cdn, cen, ctn
  real(8) :: cd_rt
  real(8) :: wv ! real wind
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
  !
  ! 入力（単位に注意！  用意するデータの単位を変更するか、
  !                     このサブルーチンで変更すること）
  !     SST : Sea Surface Temperature     [K]
  !     SAT : Observed Air Temperature    [K]
  !     QAR : Observed Specific Humidity  [kg/kg]
  !     WDV : Observed Wind Speed         [m/s]
  !     SLP : Observed sea level pressure [Pa]
  !
  !       ES : 飽和水蒸気圧 (hPa) (!! MKS !!)
  !       QS : 海面における飽和比湿 (kg/kg)
  !       WV : 海上風速 (m/s)
  !       CDN,CEN: バルク係数（無次元）
  !       DQR,DTEMP: 大気海洋間の比湿差、温度差
  !
  integer(4) :: n
  integer(4), parameter :: n_itts = 5
  real(8), parameter :: inc_ratio = 1.0d-4
  real(8) :: test, cdn_prv

  !  作業用変数
  !
  integer(4) :: i, j
  real(8) :: hl1, hl2
  !
  !----------------------------------------------------------------------
  !
  eps_air = mwwater / mwair
  tvq_air = 1.0d0/eps_air - 1.0d0

  !write(6,*) 'eps_air                 = ', eps_air
  !write(6,*) 'tvq_air                 = ', tvq_air

  do j = 1, jmx
!$omp parallel
!$omp do private(i,n,slpres,qatmos,satmos,&
!$omp & tssurf, dtemp, sll, hl1, hl2, es, qs, dqr, wv, tv, wv10n, &
!$omp & cdn10, cdn10_rt, cen10, stab, ctn10, cdn, ctn, cen, &
!$omp & cd_rt, ustar, tstar, qstar, bstar, zetau, x2, x, xx, &
!$omp & psi_mu, psi_hu, zetat, psi_mt, psi_ht, &
!$omp & zetatrg, psi_mtrg, psi_htrg, &
!$omp & zetaq, psi_mq, psi_hq, satmosu, qatmosu, test, cdn_prv)
    do i = 1, imx

!      if (atexl(i,j) == 1.0d0) then ! needs stability adjustment
        !
        slpres = slp(i,j) * 1.0d-2 ! [hPa]
        qatmos = qar(i,j)
        satmos = sat(i,j) - tab
        !
        tssurf = sst(i,j) - tab
        dtemp = tssurf - satmos
        !
        ! saturation specific humidity at the sea surface
        sll = cgill1 + cgill2 * tssurf

        if (ice(i,j) == 0.0d0) then
          hl2 = (agill1 + agill2 * tssurf) / (1.0d0 + agill3 * tssurf)
          hl1 = 10.D0 ** hl2  ! [hPa]
          if (atexl(i,j) == 1.0d0) then
            es = hl1 * bgill0 * (1.0d0 + bgill1 * slpres * (bgill2 + bgill3 * tssurf**2)) ! [hPa]
          else
            es = hl1 * (1.0d0 + bgill1 * slpres * (bgill2 + bgill3 * tssurf**2)) ! [hPa]
          end if
        else
          hl2 = (agill1 + agill2 * tssurf) / (1.0d0 + agill3 * tssurf) + igill1 * tssurf
          hl1 = 10.D0 ** hl2  ! [hPa]
          es = hl1 * (1.0d0 + bgill1 * slpres * (bgill2 + bgill3 * tssurf**2)) ! [hPa]
        end if

        qs = eps_air * es / (slpres - (1.0d0 - eps_air) * es)

        dqr = qs - qatmos

        wv10n = wdv(i,j)
        wv10n = max(wv10n, wvmin)                  ! 0.3 [m/s] floor on wind

        tv = (satmos + tab) * (1.0d0 + tvq_air * qatmos)
        qatmosu = qatmos
        !
        if ((atexl(i,j) == 1.0d0) .and. (ice(i,j) > 0.0d0)) then
          cdn10 = 1.63d-3 ! L-Y eqn. 21
          cen10 = 1.63d-3 ! L-Y eqn. 21
          ctn10 = 1.63d-3 ! L-Y eqn. 21
          cdn10_rt = sqrt(cdn10)
        else
          hl1 = (2.7d0 / wv10n + 0.142d0 + 0.0764d0 * wv10n - 3.14807d-10 * (wv10n**6)) &
               &      / 1.0d3                                    ! LY2009 eqn. 11a
          cdn10 = (0.5d0 - sign(0.5d0,wv10n-33.0d0)) * hl1 &
              & + (0.5d0 + sign(0.5d0,wv10n-33.0d0)) * 2.34d-3   ! LY2009 eqn. 11b

          cdn10_rt = sqrt(cdn10)
          cen10 = 34.6d0 * cdn10_rt / 1.0d3                      ! L-Y eqn. 6b
          stab = 0.5d0 + sign(0.5d0,-dtemp)
          ctn10 = (18.0d0 * stab + 32.7d0 * (1.0d0 - stab)) &
               &      * cdn10_rt / 1.0d3                         ! L-Y eqn. 6c
        end if

        ustar = cdn10_rt * wv10n                               ! L-Y eqn. 7a

        cdn = cdn10                                            ! first guess for exchange coeff's at z
        ctn = ctn10
        cen = cen10

        cdn_prv = cdn

        LOOP_ADJUST: do n = 1, n_itts                                       ! Monin-Obukhov iteration
          !
          cd_rt = sqrt(cdn)
          tstar = (ctn / cd_rt) * (-dtemp)                     ! L-Y eqn. 7b
          qstar = (cen / cd_rt) * (-dqr)                       ! L-Y eqn. 7c
          !bstar = grav_mks * &
          !     & ( tstar / tv + qstar / (qatmos + 1.0d0 / tvq_air))
          bstar = grav_mks * &
               & ( tstar / tv + qstar / (qatmosu + 1.0d0 / tvq_air)) ! 2015.11.19 : Would this be consistent? 
                                                                     !              Using the same level.
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
                 & - 2.0d0 * (atan(x) - atan(1.0d0))            ! L-Y eqn. 8d
            psi_hq = 2.0d0 * log((1.0d0 + x2) / 2.0d0)          ! L-Y eqn. 8e
          end if
          !
          ! target level
          !
          zetatrg = karman * bstar * alt_target / (ustar * ustar)  ! L-Y eqn. 8a
          zetatrg = sign(min(abs(zetatrg),10.d0),zetatrg)          ! undocumented NCAR
          x2 = sqrt(abs(1.0d0 - 16.0d0 * zetatrg))                 ! L-Y eqn. 8b
          x2 = max(x2,1.0d0)                                       ! undocumented NCAR
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
          !wv = wv10n * (1.0d0 + cdn10_rt * (log(altu / 10.0d0) - psi_mu) / karman) ! L-Y eqn. 9a
          !wv = max(wv, wvmin)             ! 0.3 [m/s] floor on wind

          satmosu = satmos - tstar * &
               & (log(altt / altu) + psi_hu - psi_ht) / karman ! L-Y eqn. 9b
          qatmosu = qatmos - qstar * &
               & (log(altq / altu) + psi_hu - psi_hq) / karman ! L-Y eqn. 9c

          tv = (satmosu + tab) * (1.0d0 + tvq_air * qatmosu)
        
          stab = 0.5d0 + sign(0.5d0,zetau)

          if ((atexl(i,j) == 1.0d0) .and. (ice(i,j) > 0.0d0)) then
            ctn10 = 1.63d-3 ! L-Y eqn. 21
          else
            ctn10 = (18.0d0 * stab + 32.7d0 * (1.0d0 - stab)) &
                 & * cdn10_rt / 1.0d3                            ! L-Y eqn. 6c again
          end if
        
          xx = (log(altu / 10.0d0) - psi_mu) / karman
          cdn = cdn10 / (1.0d0 + cdn10_rt * xx)**2                           ! L-Y 10a
          xx = (log(altu / 10.0d0) - psi_hu) / karman
          ctn = ctn10 / (1.0d0 + ctn10 * xx / cdn10_rt) * sqrt(cdn / cdn10)  !       b
          cen = cen10 / (1.0d0 + cen10 * xx / cdn10_rt) * sqrt(cdn / cdn10)  !       c
        
          dtemp = tssurf - satmosu
          dqr = qs - qatmosu

          test = abs(cdn - cdn_prv) / (cdn + 1.0d-8)

          if (test < inc_ratio) exit LOOP_ADJUST

          cdn_prv = cdn

        end do LOOP_ADJUST

        wdvtrg(i,j) = ustar / sqrt(cdn)                                ! L-Y eqn. 7a
        tmptrg(i,j) = satmos + tab - tstar * &
               & (log(altt / alt_target) + psi_htrg - psi_ht) / karman ! L-Y eqn. 9b
        sphtrg(i,j) = qatmos - qstar * &
               & (log(altq / alt_target) + psi_htrg - psi_hq) / karman ! L-Y eqn. 9c
        sphtrg(i,j) = max(sphtrg(i,j),sphmin)

!      end if

    end do
!$omp end parallel
  end do

end subroutine wind_neutral_to_real
