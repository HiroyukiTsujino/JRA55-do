!zos.F90
!====================================================
!
! Global Average of Sea Surface Height (0D)
! Sea Surface Height above the Average (2D) (CMIP5; zos)
!
!====================================================
program zos
  !
  use oc_mod_param, only : &
  &   imut, jmut, km,      &
  &   ro
  !
  use oc_structure, only : &
  &   read_topo,           &
  &   ho4, exnn ,          & ! 水深、層数
  &   aexl, atexl,         & ! 海陸インデックス
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
  real(4), parameter :: UNDEF = -9.99e33
  !
  real(8), parameter :: ro0 = ro * 1.0d3  ! 海水の参照密度
  real(8), parameter :: rice = 900.d0     ! 海氷の標準密度
  real(8), parameter :: rrr = ro0 / rice
  real(8), parameter :: rdsw = 0.33d0  ! 雪の密度/水の密度
  real(8), parameter :: m2cm = 1.0d2
  !
  !
  real(8)    :: ssh(imut, jmut)
  real(8)    :: hi(imut, jmut)
  real(8)    :: hsnw(imut, jmut)
  real(8)    :: hl1
  !
  real(4)    :: d2_r4(imut,jmut)
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flin_ssh  ! 海面高度ファイル
  character(len=256)    :: flin_ice  ! 海氷ファイル
  character(len=256)    :: fltopo ! 海底地形ファイル
  character(len=256)    :: flsclf ! スケールファクター・ファイル
  character(len=256)    :: flout  ! 出力ファイル
  character(len=256)    :: flout_ssh ! ZOS出力ファイル
  !
  character(len=256)    :: flout2
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: mttmp  = 79
  integer(4), parameter :: mtin_ice = 81
  integer(4), parameter :: mtin_ssh = 82
  integer(4), parameter :: mtout    = 83
  integer(4), parameter :: mtout2   = 84
  integer(4), parameter :: mtout_ssh = 85
  !
  real(8) :: area, tarea
  real(8) :: sumh
  real(8) :: meanh
  !
  real(8) :: ticesnw
  real(8) :: micesnw
  !
  real(8) :: nendidx(jmut)
  !
  real(4) :: meanh4
  !
  integer(4) :: i, j, k
  !
  !==============================================
  !
  namelist /nml_zos/ flin_ssh, flin_ice, fltopo, flsclf, flout, flout_ssh
  !
  !----------------------------------------------
  ! 入力パラメタ既定値
  !----------------------------------------------
  flin_ssh= 'hs_ssh.d'
  flin_ice= 'hs_ice.d'
  fltopo  = 'topo.d'
  flsclf  = 'scale_factor.d'
  flout   = 'zosog.d'
  flout2  = 'zosog.d.txt'
  flout_ssh= 'zos.d'
  !
  !----------------------------------------------
  ! 標準入力から読み込み
  !----------------------------------------------
  read(unit=5, nml=nml_zos)

  print *,'flin_ssh :', trim(flin_ssh)
  print *,'flin_ice :', trim(flin_ice)
  print *,'fltopo   :', trim(fltopo)
  print *,'flsclf   :', trim(flsclf)
  print *,'flout    :', trim(flout)
  print *,'flout_ssh :', trim(flout_ssh)
  !
  write(flout2, *) trim(flout), '.txt'
  print *,'flout2   :', trim(flout2)
  !
  !----------------------------------------------
  !  地形の読み込み
  !----------------------------------------------
  !
  call read_topo(fltopo)
  !
  !----------------------------------------------
  !  スケールファクタの読み込み
  !----------------------------------------------
  !
  call read_scale(flsclf)
  !
  !==============================================
  !
  !----------------------------------------------
  ! 入出力ファイルオープン
  !----------------------------------------------
  !
  open (mtin_ssh, file=flin_ssh, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)
  write(*, *) 'ssh in  :', trim(flin_ssh)
  !
  open (mtin_ice, file=flin_ice, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)
  write(*, *) 'ice in  :', trim(flin_ice)
  !
  !----------------------------------------------
  ! 読み込み、倍精度実数への変換
  !----------------------------------------------
  !
  !  SSH
  read (mtin_ssh, rec=1) d2_r4
  ssh(:,:) = atexl(:,:,1)*real(d2_r4(:,:),8)
  !
  !  ICE
  read (mtin_ice, rec=1) d2_r4
  hi(:,:) = atexl(:,:,1)*real(d2_r4(:,:),8)
  !
  !  SNOW
  read (mtin_ice, rec=2) d2_r4
  hsnw(:,:) = atexl(:,:,1)*real(d2_r4(:,:),8)
  !
  close (mtin_ice)
  close (mtin_ssh)
  !
  !----------------------------------------------
  ! 面積重みをつけて全球で積算
  !----------------------------------------------
  nendidx(1:jmut) = 1.d0
#if defined OGCM_JOT || defined OGCM_TRIPOLAR
  nendidx(jmut-2) = 0.5d0
  nendidx(jmut-1:jmut) = 0.d0
#endif /* OGCM_JOT || OGCM_TRIPOLAR */
  tarea = 0.d0
  sumh = 0.d0
  !
  do j = 2, jmut-2
    do i = 3, imut-2
      area =  atexl(i, j, 1) * nendidx(j) *   &
        &     ( aexl(i-1, j  , 1)*a_br(i-1, j  ) +aexl(i, j  , 1)*a_bl(i, j  )  &
        &      +aexl(i-1, j-1, 1)*a_tr(i-1, j-1) +aexl(i, j-1, 1)*a_tl(i, j-1))
      tarea = tarea + area
      sumh  = sumh  + ssh(i, j) * area
    end do
  end do
  !
  do j = 2, jmut-2
    do i = 3, imut-2
      hl1 = atexl(i, j, 1) * nendidx(j) *           &
        &     ( aexl(i-1, j  , 1)*a_br(i-1, j  ) +aexl(i, j  , 1)*a_bl(i, j  )  &
        &      +aexl(i-1, j-1, 1)*a_tr(i-1, j-1) +aexl(i, j-1, 1)*a_tl(i, j-1)) &
        &   * (hi(i,j)/rrr + hsnw(i,j)*rdsw) * m2cm
      ticesnw = ticesnw + hl1
    end do
  end do
  !
  !----------------------------------------------
  ! 単精度に変換して書き込み
  !----------------------------------------------
  !
  open (mtout, file=flout, form='unformatted',&
    &  access='direct', recl=4)
  !
  open (mtout2, file=flout2)
  !
  open (mtout_ssh, file=flout_ssh, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)
  !
  ! Sea Water
  meanh = sumh / tarea
  write(*,*) 'Total Area   :', tarea * 1.d-4, '[m^2]'
  write(*,*) 'Total Volume :', sumh * 1.d-6, '[m^3]'
  write(*,*) trim(flout), ', ZOS offset (A) :', meanh * 1.d-2, '[m]'
  !
  ! Sea Ice and Snow
  micesnw = ticesnw / tarea
  write(*,*) 'Total Volume (Ice and Snow) :', ticesnw * 1.d-6, '[m^3]'
  write(*,*) 'Thickness of Ice and Snow as Water (B) :', micesnw * 1.d-2, '[m]'
  !
  !  ZOS
  d2_r4(:,:) = real(ssh(:,:) - meanh, 4) *1.e-2 ! [m]
  where(atexl(1:imut, 1:jmut, 1) == 0.d0)
    d2_r4(1:imut, 1:jmut) = UNDEF
  end where
  write (mtout_ssh, rec=1) d2_r4
  !
  meanh4 = real(meanh+micesnw, 4) * 1.e-2  !  [m]
  write(*,*) 'Global Average Sea Surface Height (A) + (B) :', meanh4, '[m]'
  !
  write(mtout, rec=1) meanh4
  write(mtout2, *)    meanh4
  !----------------------------------------------
  !
  close (mtout)
  close (mtout2)
  close (mtout_ssh)
  !
!====================================================
end program zos
