!-*-F90-*-
!localmean.F90
!====================================================
!
! Calculate Local Mean for TS-grid Value
!
!====================================================
program local_mean
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ksgm, dz,            &
  &   pi, radian, radian_r, &
  &   slat0, slon0,        &
  &   nplat, nplon,        &
  &   splat, splon,        &
  &   ro,    cp
  !
  use oc_mod_trnsfrm, only  : &
  &   set_abc, mp2lp,         &
  &   length_on_sphere
  !
  use oc_structure, only : &
  &   read_topo,           &
  &   dep, dsgm,           & ! UVTSボックス境界（上面）の深さ
  &   dzu,                 & ! UVボックスの厚さ
  &   thcksgm, thcksgmr,   &
  &   ho4, exnn ,          & ! 水深、層数
  &   aexl, atexl,         & ! 海陸インデックス
#ifdef OGCM_BBL
  &   ho4bbl, exnnbbl,     & ! BBL層厚、層数
  &   aexlbbl, atexlbbl,   & ! BBLインデックス
  &   kbtm,                &
#endif /* OGCM_BBL */
  &   set_hgrids,          &
  &   slat, slon,          & ! 
  &   alatt, alatu,        &
  &   alont, alonu,        &
  &   read_scale,          &
  &   a_tl  , a_tr  ,      & ! 格子面積
  &   a_bl  , a_br  ,      &
  &   dx_tl , dx_tr ,      & ! 東西長
  &   dx_bl , dx_br ,      &
  &   dy_tl , dy_tr ,      & ! 南北長
  &   dy_bl , dy_br
  !
  !----------------------------------------------
  !
  implicit none
  !
#if defined OGCM_TRIPOLAR || defined OGCM_JOT
  integer(4), parameter :: ibu = 3, ieu = imut - 2  ! cyclic
  integer(4), parameter :: jbu = 2, jeu = jmut - 3  ! tripolar
  integer(4), parameter :: ibt = 3, iet = imut - 2  ! cyclic
  integer(4), parameter :: jbt = 2, jet = jmut - 2  ! tripolar
#else /* OGCM_TRIPOLAR || OGCM_JOT */
  integer(4), parameter :: ibu = 2, ieu = imut - 1
  integer(4), parameter :: jbu = 2, jeu = jmut - 1
  integer(4), parameter :: ibt = 2, iet = imut
  integer(4), parameter :: jbt = 2, jet = jmut
#endif /* OGCM_TRIPOLAR || OGCM_JOT */
  !
  ! 海洋モデル地形
  !
  real(8)    :: ssh(imut, jmut)
  real(4)    :: ssh4(imut, jmut)
  real(8), allocatable :: dt8(:,:,:)
  real(4), allocatable :: dt4(:,:,:)
  real(4) :: undef4
  real(8) :: undef8
  real(8), allocatable :: mask(:,:)
  real(8), allocatable :: mask_val(:,:,:)

  real(8) :: hld

  !real(4), parameter :: UNDEF = -9.99e33

  ! 入出力ファイル

  character(len=256)    :: flin     ! 入力ファイル
  character(len=256)    :: flin_ssh ! 入力ファイル
  character(len=256)    :: fltopo   ! 海底地形ファイル
  character(len=256)    :: flsclf   ! スケールファクター
  character(len=256)    :: flout    ! 診断量の出力ファイル
  character(len=256)    :: flout2
  character(len=256)    :: flmsk

  integer(4) :: d2_d3, kstt, kend
  real(8)    :: lat_b, lat_e
  real(8)    :: lon_b, lon_e
  integer(4), save :: knum
  character(len=1) :: tuw
  logical :: lmskout

  namelist /nmllocalm/   fltopo, flsclf, flin, flin_ssh, &
    &                    d2_d3, kstt, kend, flout,       &
    &                    lat_b, lat_e, lon_b, lon_e,     &
    &                    undef4, tuw, lmskout

#ifdef OGCM_VARIABLE
  character(len=256)    :: file_vgrid ! 可変格子定義ファイル
  namelist /inflg/ file_vgrid
#endif /* OGCM_VARIABLE */
  !
  integer(4) :: ios       !  入出力エラーチェック用
  integer(4), parameter :: mttmp    = 80
  integer(4), parameter :: mtin     = 81
  integer(4), parameter :: mtin_ssh = 82
  integer(4), parameter :: mtout    = 83
  integer(4), parameter :: mtout2   = 84
  integer(4), parameter :: mtmsk    = 85
  !
  integer(4), save :: lrec_out
  !
  integer(4) :: irecw
  integer(4) :: i, j, k, m, jj
  integer(4) :: nkai
  real(8)    :: wrk0
  real(8), allocatable :: wrk(:)
  !
  !==============================================

  fltopo = 'topo.d'
  flsclf = 'scale_factor.d'
  flin     = 'hs_t.d'
  flin_ssh = 'hs_ssh.d'
  d2_d3 = 3
  kstt  = 1
  kend  = km
  flout = '../logs/hoge.txt'

  ! 標準入力から読み込み

  read(unit=5, nmllocalm)
  write(*,*) 'fltopo   :', trim(fltopo)
  write(*,*) 'flsclf   :', trim(flsclf)
  write(*,*) 'flin     :', trim(flin)
  write(*,*) 'flin_ssh :', trim(flin_ssh)
  write(*,*) 'd2_d3    :', d2_d3
  write(*,*) 'k start  :', kstt
  write(*,*) 'k end    :', kend
  write(*,*) 'flout    :', trim(flout)
  write(*,*) 'lat start:', lat_b
  write(*,*) 'lat end  :', lat_e
  write(*,*) 'lon start:', lon_b
  write(*,*) 'lon end  :', lon_e
  write(*,*) 'undef    :', undef4
  write(*,*) 'tuw      :', tuw
  write(*,*) 'lmskout  :', lmskout

  flmsk='mask_region.dat'

  allocate(wrk(1:km))
  allocate(mask(1:imut, 1:jmut))

  if(d2_d3 == 2) then
    knum = kend
    allocate(dt4(1:imut, 1:jmut, 1:knum))
    allocate(dt8(1:imut, 1:jmut, 1:knum))
    allocate(mask_val(1:imut, 1:jmut, 1:knum))
  else
!    write(6,*) ' Program not completed yet '
!    stop
    knum = kend - kstt + 1
    allocate(dt4(1:imut, 1:jmut, 1:km))
    allocate(dt8(1:imut, 1:jmut, 1:km))
    allocate(mask_val(1:imut, 1:jmut, 1:km))
  end if
  !
  undef8 = real(undef4,8)
  !
#ifdef OGCM_VARIABLE
  read(unit=5, inflg) ! file_vgrid
#endif /* OGCM_VARIABLE */
  !
  !----------------------------------------------
  !
  call read_topo(fltopo)
  !
  call set_abc ( nplat, nplon, splat, splon )
  !
  call read_scale(flsclf)
  !
  !
#ifdef OGCM_VARIABLE
  call set_hgrids(file_vgrid)
#else /* OGCM_VARIABLE */
  call set_hgrids
#endif /* OGCM_VARIABLE */
  !
  !==============================================
  !
  ! 入出力ファイルオープン
  !
  !
  if(d2_d3 == 3) then
    open (mtin, file=flin, form='unformatted', &
      &  access='direct', recl=4*imut*jmut*km)
    open (mtin_ssh, file=flin_ssh, form='unformatted', &
      &  access='direct', recl=4*imut*jmut)
  else
    open (mtin, file=flin, form='unformatted', &
      &  access='direct', recl=4*imut*jmut*knum)
  end if
  !
  !-------------------------
  !
  read (mtin, rec=1) dt4
  close(mtin)
  if(d2_d3 == 2) then
    do k = 1, knum
      do j = 1, jmut
        do i = 1, imut
          if (atexl(i,j,k) == 1.0d0) then
            dt8(i,j,k) = real(dt4(i,j,k),8)
          else
            dt8(i,j,k) = undef8
          end if
        end do
      end do
    end do
    do k = 1, knum
!      dt8(:,:,k) = atexl(:,:,k)*real(dt4(:,:,k),8)
!      dt8(:,:,k) = real(dt4(:,:,k),8)
      dt8(1:2,         1:jmut, k)=dt8(imut-3:imut-2, 1:jmut, k)
      dt8(imut-1:imut, 1:jmut, k)=dt8(3:4,           1:jmut, k)
      do j = 1, jmut
        do i = 1, imut
          if (dt8(i,j,k) /= undef8) then
            mask_val(i,j,k) = 1.0d0
          else
            mask_val(i,j,k) = 0.0d0
          end if
        end do
      end do
    end do
  else
    do k = 1, km
      do j = 1, jmut
        do i = 1, imut
          if (atexl(i,j,k) == 1.0d0) then
            dt8(i,j,k) = real(dt4(i,j,k),8)
          else
            dt8(i,j,k) = undef8
          end if
        end do
      end do
    end do
!    dt8(:,:,1:km) = atexl(:,:,1:km)*real(dt4(:,:,1:km),8)
!    dt8(:,:,1:km) = real(dt4(:,:,1:km),8)
    dt8(1:2,         1:jmut, 1:km)=dt8(imut-3:imut-2, 1:jmut, 1:km)
    dt8(imut-1:imut, 1:jmut, 1:km)=dt8(3:4,           1:jmut, 1:km)
    do k = 1, km
      do j = 1, jmut
        do i = 1, imut
          if (dt8(i,j,k) /= undef8) then
            mask_val(i,j,k) = 1.0d0
          else
            mask_val(i,j,k) = 0.0d0
          end if
        end do
      end do
    end do
  end if

  if(d2_d3 == 3) then

    !  SSH

    read (mtin_ssh, rec=1) ssh4
    ssh(:,:) = atexl(:,:,1)*real(ssh4(:,:),8)
!    ssh(:,:) = real(ssh4(:,:),8)
    ssh(1:2,         1:jmut)=ssh(imut-3:imut-2, 1:jmut)
    ssh(imut-1:imut, 1:jmut)=ssh(3:4,           1:jmut)
    close(mtin_ssh)

    do j = 1, jmut - 1
      do i = 1, imut - 1
        hld = (ssh(i,j) * a_bl(i,j) + ssh(i+1,j) * a_br(i,j) &
             &   + ssh(i,j+1) * a_tl(i,j) + ssh(i+1,j+1) * a_tr(i,j)) &
             &  / (a_bl(i,j) + a_br(i,j) + a_tl(i,j) + a_tr(i,j))
        do k = 1, ksgm
          dzu(i,j,k) = aexl(i,j,k) * (dzu(i,j,k) + dsgm(k) * hld)
        end do
      end do
    end do

  end if

  !---------------------------------------------------
  !
  ! Calculate Local Average
  !
  !---------------------------------------------------

  call lave(wrk0, wrk, lat_b, lat_e, lon_b, lon_e)

  open (mtout, file=flout, form='unformatted', access='direct', recl=4)

  if(d2_d3 == 3) then
    write(*, *) ' Local Average 3D ', wrk0
    do k=1, km
      write(*, "(a, i3, f)") 'level k= ', k, wrk(k)
    end do

    do k=1, km
      write(mtout, rec=k) real(wrk(k), 4)
    end do
    write(mtout, rec=km+1) real(wrk0, 4)

    write(flout2, *) trim(flout), '.txt'
    open (mtout2, file=flout2)
    write(mtout2, *)  wrk0
    close(mtout2)
  else
    write(*, *) ' Local Average 2D', trim(flin)
    do k=1, knum
      write(*, "(a, i3, f)") 'level k= ', k, wrk(k)
    end do

    do k=1, knum
      write(mtout, rec=k) real(wrk(k), 4)
    end do

    do k=1, knum
      write(flout2, "(a, a, i2.2)") trim(flout), '.txt', k
      open (mtout2, file=flout2)
      write(mtout2, *) wrk(k)
      close(mtout2)
    end do
  end if

  close ( mtin )
  close ( mtout )

  if (lmskout) then
    open (mtmsk, file=flmsk, form='unformatted', &
         & access='direct', recl=4*imut*jmut)
    write(mtmsk,rec=1) real(mask(1:imut,1:jmut),4)
    close(mtmsk)
  end if

contains
!====================================================
!
!  地理座標で指定した矩形領域の平均値を計算
!
!====================================================
subroutine lave(aveval0, aveval, latstt, latend, lonstt, lonend)
  !
  real(8), intent(inout) :: aveval0
  real(8), intent(inout) :: aveval(km)
  real(8), intent(in) :: latstt
  real(8), intent(in) :: latend
  real(8), intent(in) :: lonstt
  real(8), intent(in) :: lonend
  !
  !    (lonstt, latend) ------- (lonend, latend)
  !       |                                |
  !       |                                |
  !       |                                |
  !    (lonstt, latstt) ------- (lonend, latstt)
  !
  !
  real(8)    :: lambdas, phis
  real(8)    :: lambdae, phie
  real(8)    :: lambda, phi
  real(8)    :: mu, psi
  !
  real(8)    :: hl0, hl1
  !
  integer(4) :: i, j, k
#ifdef OGCM_BBL
  integer(4), parameter :: kmax = km-1
#else /* OGCM_BBL */
  integer(4), parameter :: kmax = km
#endif /* OGCM_BBL */
  real(8)    :: av
  real(8)    :: avol(km)
  !
  !--------------------------------------------

  mask(1:imut,1:jmut) = 0.0d0

  !---

  lambdas = lonstt * radian_r
  phis    = latstt * radian_r
  lambdae = lonend * radian_r
  phie    = latend * radian_r

!  if ((tuw == 'T') .or. (tuw == 't') .or. (tuw == 'W') .or. (tuw == 'w')) then
!    do j = 3, jmut - 2
!      do i = 3, imut - 2
!        mu  = alont(i)*radian_r
!        psi = alatt(j)*radian_r
!        call mp2lp(lambda, phi, mu, psi)
!        if (lambda < 0.0d0) then
!          lambda = lambda + 2.0d0 * pi
!        end if
!        if (lambda > 2.0d0 * pi) then
!          lambda = lambda - 2.0d0 * pi
!        end if
!        if (phi < -0.5d0 * pi) then
!          phi = - pi - phi
!        end if
!        if (phi > 0.5d0 * pi) then
!          phi = pi - phi
!        end if
!
!        if ((lambdas <= lambda) .and. (lambda <= lambdae)) then
!          if ((phis <= phi) .and. (phi <= phie)) then
!            mask(i,j) = 1.0d0
!          end if
!        end if
!      end do
!    end do
!  end if

!  if ((tuw == 'U') .or. (tuw == 'u')) then
    do j = jbu, jeu
      do i = ibu, ieu

        mu  = alonu(i)*radian_r
        psi = alatu(j)*radian_r

        call mp2lp(lambda, phi, mu, psi)

        if (lambda < 0.0d0) then
          lambda = lambda + 2.0d0 * pi
        end if
        if (lambda > 2.0d0 * pi) then
          lambda = lambda - 2.0d0 * pi
        end if
        if (phi < -0.5d0 * pi) then
          phi = - pi - phi
        end if
        if (phi > 0.5d0 * pi) then
          phi = pi - phi
        end if
        if ((lambdas <= lambda) .and. (lambda <= lambdae)) then
          if ((phis <= phi) .and. (phi <= phie)) then
            mask(i,j) = 1.0d0
          end if
        end if

      end do
    end do
!  end if

  !---------------------

  aveval0 = 0.d0
  av  = 0.d0
  avol(1:km) = 0.d0
  aveval(1:km) = 0.d0
  hl0 = 0.0d0
  hl1 = 0.0d0

  if (d2_d3 == 3) then   !  3D ----------
    do j = jbu, jeu
      do i = ibu, ieu
        do k = 1, kmax
          if ((tuw == 'T') .or. (tuw == 't') &
               & .or. (tuw == 'W') .or. (tuw == 'w')) then
            hl0 = (a_bl(i,j) * mask_val(i,j,k) + a_br(i,j) * mask_val(i+1,j,k) &
                 & + a_tl(i,j) * mask_val(i,j+1,k) + a_tr(i,j) * mask_val(i+1,j+1,k))
            hl1 = (a_bl(i,j) * mask_val(i  ,j  ,k) * dt8(i  ,j  ,k) &
               & + a_br(i,j) * mask_val(i+1,j  ,k) * dt8(i+1,j  ,k) &
               & + a_tl(i,j) * mask_val(i  ,j+1,k) * dt8(i  ,j+1,k) &
               & + a_tr(i,j) * mask_val(i+1,j+1,k) * dt8(i+1,j+1,k))
          else if ((tuw == 'U') .or. (tuw == 'u')) then
            hl0 = (a_bl(i,j) + a_br(i,j) + a_tl(i,j) + a_tr(i,j)) * mask_val(i,j,k)
            hl1 = (a_bl(i,j) + a_br(i,j) + a_tl(i,j) + a_tr(i,j)) * mask_val(i,j,k) * dt8(i,j,k)
          end if
          hl0 = hl0 * mask(i,j) * dzu(i,j,k)
          hl1 = hl1 * mask(i,j) * dzu(i,j,k)
          avol(k)   = avol(k)   + hl0
          aveval(k) = aveval(k) + hl1
          if (k <= kend) then
            av       = av        + hl0
            aveval0  = aveval0   + hl1
          end if
        end do
#ifdef OGCM_BBL
        if (aexlbbl(i,j,1) == 1.0d0) then
          if ((tuw == 'T') .or. (tuw == 't') &
               & .or. (tuw == 'W') .or. (tuw == 'w')) then
            hl0 = (a_bl(i,j) * mask_val(i,j,km) + a_br(i,j) * mask_val(i+1,j,km) &
                 & + a_tl(i,j) * mask_val(i,j+1,km) + a_tr(i,j) * mask_val(i+1,j+1,km))
            hl1 = (a_bl(i,j) * mask_val(i  ,j  ,km) * dt8(i  ,j  ,km) &
                 & + a_br(i,j) * mask_val(i+1,j  ,km) * dt8(i+1,j  ,km) &
                 & + a_tl(i,j) * mask_val(i  ,j+1,km) * dt8(i  ,j+1,km) &
                 & + a_tr(i,j) * mask_val(i+1,j+1,km) * dt8(i+1,j+1,km))
          else if ((tuw == 'U') .or. (tuw == 'u')) then
            hl0 = (a_bl(i,j) + a_br(i,j) + a_tl(i,j) + a_tr(i,j)) * mask_val(i,j,km)
            hl1 = (a_bl(i,j) + a_br(i,j) + a_tl(i,j) + a_tr(i,j)) * mask_val(i,j,km) * dt8(i,j,km)
          end if
          hl0 = hl0 * mask(i,j) * dzu(i,j,km)
          hl1 = hl1 * mask(i,j) * dzu(i,j,km)
          avol  (kbtm(i,j)) = avol  (kbtm(i,j)) + hl0
          aveval(kbtm(i,j)) = aveval(kbtm(i,j)) + hl1
          if (kbtm(i,j) <= kend) then
            av       = av        + hl0
            aveval0  = aveval0   + hl1
          end if
        end if
#endif /* OGCM_BBL */
      end do
    end do

    do k = 1, km
      write(*,*) k, 'numerator  :', aveval(k)
      write(*,*) k, 'denominator:', avol(k)
      if(avol(k) > 0.d0) then
        aveval(k) = aveval(k) / avol(k)
      else
        aveval(k) = 0.d0
      end if
    end do

    if(av > 0.d0) then
      aveval0 = aveval0 / av
    else
      aveval0 = 0.d0
    end if

  else                  !  2D ----------

    do j = 1, jmut
      do i = 1, imut
        do k = 1, knum
          if ((tuw == 'T') .or. (tuw == 't') &
               & .or. (tuw == 'W') .or. (tuw == 'w')) then
            hl0 = (a_bl(i,j) * mask_val(i,j,k) + a_br(i,j) * mask_val(i+1,j,k) &
                 & + a_tl(i,j) * mask_val(i,j+1,k) + a_tr(i,j) * mask_val(i+1,j+1,k))
            hl1 = (a_bl(i,j) * mask_val(i  ,j  ,k) * dt8(i  ,j  ,k) &
               & + a_br(i,j) * mask_val(i+1,j  ,k) * dt8(i+1,j  ,k) &
               & + a_tl(i,j) * mask_val(i  ,j+1,k) * dt8(i  ,j+1,k) &
               & + a_tr(i,j) * mask_val(i+1,j+1,k) * dt8(i+1,j+1,k))
          else if ((tuw == 'U') .or. (tuw == 'u')) then
            hl0 = (a_bl(i,j) + a_br(i,j) + a_tl(i,j) + a_tr(i,j)) * mask_val(i,j,k)
            hl1 = (a_bl(i,j) + a_br(i,j) + a_tl(i,j) + a_tr(i,j)) * mask_val(i,j,k) * dt8(i,j,k)
          end if
          hl0 = hl0 * mask(i,j)
          hl1 = hl1 * mask(i,j)
          av        = av        + hl0
          aveval(k) = aveval(k) + hl1
        end do
      end do
    end do

    do k = 1, knum
      write(*,*) k, 'numerator  :', aveval(k)
      write(*,*) k, 'denominator:', av
      if (av > 0.d0) then
        aveval(k) = aveval(k) / av
      else
        aveval(k) = 0.d0
      end if
    end do

  end if

end subroutine lave
!====================================================
end program local_mean
