!globalmean.F90
!====================================================
!
! Global Average of Variables at TS-Grid
!
!====================================================
program globalmean
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ksgm, dz
  !
  use oc_structure, only :     &
  &   dep,                    &
  &   thcksgm,                &
  &   thcksgmr,               &
  &   ho4, exnn ,             & ! 水深、層数
  &   aexl, atexl,            & ! 海陸インデックス
  &   dzu,                    & ! UVボックスの厚さ
  &   a_tl  , a_tr  ,         & ! 格子面積
  &   a_bl  , a_br  ,         &
  &   dx_tl , dx_tr ,         & ! 東西長
  &   dx_bl , dx_br ,         &
  &   dy_tl , dy_tr ,         & ! 南北長
  &   dy_bl , dy_br ,         &
  &   read_topo,              &
  &   read_scale
  !
#ifdef OGCM_BBL
  use oc_structure, only :     &
  &   ho4bbl, exnnbbl,        & ! BBL層厚、層数
  &   aexlbbl, atexlbbl         ! BBLインデックス
#endif /* OGCM_BBL */
  !
  !----------------------------------------------
  !
  implicit none
  !
  real(4), parameter :: UNDEF = -9.99e33
  !
  ! 海洋モデル地形
  !
  real(8), allocatable :: t(:,:,:)
  !
  real(4)    :: d3_r4(imut,jmut,km)
  real(4)    :: d2_r4(imut,jmut)
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flin   ! 入力ファイル
  character(len=256)    :: fltopo ! 海底地形ファイル
  character(len=256)    :: flsclf ! スケールファクター・ファイル
  character(len=256)    :: flout  ! 出力ファイル
  integer(4)            :: knum
  !
  namelist /nml_glbm2d/ flin, fltopo, flsclf, flout, knum
  !
  character(len=256)    :: flout2
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mttmp  = 79
  integer(4), parameter :: mtin   = 80
  integer(4), parameter :: mtout  = 84
  integer(4), parameter :: mtout2 = 85
  !
#ifdef OGCM_BBL
  integer(4) :: kbtm
  integer(4), parameter :: kmax = km-1
#else /* OGCM_BBL */
  integer(4), parameter :: kmax = km
#endif /* OGCM_BBL */
  !
  real(8) :: area, tarea
  !
  real(8), allocatable :: sumlt(:)
  real(8), allocatable :: meanlt(:)
  !
  real(4), allocatable :: meanlt4(:)
  !
  real(8)    :: nendidx(jmut)
  integer(4) :: i, j, k
  !
  !==============================================
  !
  !----------------------------------------------
  ! 入力パラメタ既定値
  !----------------------------------------------
  flin    = 'hs_ice.d'
  fltopo  = 'topo.d'
  flsclf  = 'scale_factor.d'
  flout   = 'ice.d'
  flout2  = 'ice.d.txt'
  knum    = 1
  !
  !----------------------------------------------
  ! 標準入力から読み込み
  !----------------------------------------------
  read(unit=5, nml_glbm2d)
  print *,'flin     :', trim(flin)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  print *,'flout    :', trim(flout)
  !
  print *,'knum     :', knum
  !
  allocate(t(1:imut, 1:jmut, 1:knum))
  allocate(sumlt(1:knum))
  allocate(meanlt(1:knum))
  allocate(meanlt4(1:knum))
  !
  !----------------------------------------------
  !  地形の読み込み
  !----------------------------------------------
  call read_topo(fltopo)
  !
  !----------------------------------------------
  !  スケールファクタの読み込み
  !----------------------------------------------
  call read_scale(flsclf)
  !
  !==============================================
  !
  !----------------------------------------------
  ! 入出力ファイルオープン
  !----------------------------------------------
  open (mtin, file=flin, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)
  write(*, *) 'file in :', trim(flin)
  !
  !----------------------------------------------
  ! 読み込み、倍精度実数への変換
  !----------------------------------------------
  do k = 1, knum
    read (mtin, rec=k) d2_r4
    t(:,:,k) = atexl(:,:,1)*real(d2_r4(:,:),8)
  end do
  !
  close (mtin)
  !
  !----------------------------------------------
  ! 体積重みをつけて全球で積算
  !----------------------------------------------
  nendidx(1:jmut) = 1.d0
#if defined OGCM_JOT || defined OGCM_TRIPOLAR
  nendidx(jmut-2) = 0.5d0
  nendidx(jmut-1:jmut) = 0.d0
#endif /* OGCM_JOT || OGCM_TRIPOLAR */
  tarea = 0.d0
  sumlt(1:knum) = 0.d0
  !
  do j = 2, jmut-2
    do i = 3, imut-2
      area = nendidx(j) * atexl(i,j,1) * &
        &      ( aexl(i-1, j  , 1)*a_br(i-1, j  ) +aexl(i, j  , 1)*a_bl(i, j  ) &
        &       +aexl(i-1, j-1, 1)*a_tr(i-1, j-1) +aexl(i, j-1, 1)*a_tl(i, j-1))
      tarea = tarea + area
      do k = 1, knum
        sumlt(k) = sumlt(k) + t(i, j, k) * area
      end do
    end do
  end do
  !
  !----------------------------------------------
  ! 単精度に変換して書き込み
  !----------------------------------------------
  meanlt(1:knum)  = sumlt(1:knum) / tarea
  meanlt4(1:knum) = real(meanlt(1:knum), 4)
  write(*,*) 'Total Area :', tarea*1.d-4, '[m^2]'
  !
  open (mtout, file=flout, form='unformatted',&
    &  access='direct', recl=4)
  !
  !
  do k = 1, knum
    write(*,*) 'Total      :', sumlt(k)*1.d-4
    write(*,"(a, i4, a, f)") 'k=', k, ' Global Average:', meanlt(k)
    write(mtout, rec=k) meanlt4(k)
    !
    write(flout2, "(a, a, i2.2)") trim(flout), '.txt', k
    print *,'flout2   :', trim(flout2)
    open (mtout2, file=flout2)
    write(mtout2, *) meanlt(k)
    close(mtout2)
  end do
  !
  !----------------------------------------------
  !
  close (mtout)
  !
  deallocate(t)
  deallocate(sumlt)
  deallocate(meanlt)
  deallocate(meanlt4)
  !
!====================================================
end program globalmean
