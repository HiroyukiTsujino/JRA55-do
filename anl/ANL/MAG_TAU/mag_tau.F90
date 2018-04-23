!mag_tau.F90
!====================================================
!
! calculate |(taux, tauy)|
!
!====================================================
program mag_tau
  !
  use oc_mod_param, only : &
  &   imut, jmut
  !
  use oc_structure, only :    &
  &   read_topo,              & !-----------------------
  &   aexl, atexl
  !
  !----------------------------------------------
  !
  implicit none
  !
  real(8) :: taux(1:imut, 1:jmut)
  real(8) :: tauy(1:imut, 1:jmut)
  real(4) :: taux4(1:imut, 1:jmut)
  real(4) :: tauy4(1:imut, 1:jmut)
  !
  real(8) :: mag(1:imut, 1:jmut)
  real(4) :: mag4(1:imut, 1:jmut)
  !
  real(4), save :: UNDEF = -9.99e33
  !
  ! 入出力ファイル
  !
  character(len=256)    :: fltopo   ! 海底地形ファイル
  character(len=256)    :: flin     ! 入力ファイル
  character(len=256)    :: flout    ! 出力ファイル
  !
  namelist /nml_mag_tau/ fltopo, flin, flout, UNDEF
  !
  integer(4), parameter :: mtin     = 81
  integer(4), parameter :: mtout    = 83
  integer(4) :: ios       !  入出力エラーチェック用
  !
  integer(4) :: i, j
  !
  !==============================================
  !
  ! 入力パラメタ既定値
  !
  fltopo = 'topo.d'
  flin   = 'hs_tau_sea.d'
  flout  = 'mag_tau.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml_mag_tau)
  write(*,*) 'fltopo   :', trim(fltopo)
  write(*,*) 'flin     :', trim(flin)
  write(*,*) 'flout    :', trim(flout)
  !
  !----------------------------------------------
  !
  !  地形の読み込み、海陸インデックス、層厚設定
  !
  call read_topo(fltopo)
  !
  !==============================================
  !
  ! 入力
  !
  open (mtin, file=flin, form='unformatted', &
    &     access='direct', recl=4*imut*jmut)
  !
  read (mtin, rec=1, iostat=ios) taux4
  if(ios /= 0) write(*, *) 'reading error in file:', mtin
  taux(:,:) = aexl(:,:,1)*real(taux4(:,:),8)
  taux(1:2, 1:jmut)=taux(imut-3:imut-2, 1:jmut)
  !
  read (mtin, rec=2, iostat=ios) tauy4
  if(ios /= 0) write(*, *) 'reading error in file:', mtin
  tauy(:,:) = aexl(:,:,1)*real(tauy4(:,:),8)
  tauy(1:2, 1:jmut)=tauy(imut-3:imut-2, 1:jmut)
  !
  close (mtin)
  !
  !---------------------------------------------------
  !
  do j = 1, jmut
    do i = 1, imut
      mag(i, j) = aexl(i, j, 1) * sqrt(taux(i, j)*taux(i, j) +tauy(i, j)*tauy(i, j))
    end do
  end do
  !
  !---------------------------------------------------
  !
  ! 出力
  !
  mag4(:,:) = real(mag(:,:),4)
  where(aexl(:,:,1) == 0.0d0)
    mag4(:,:) = UNDEF
  end where
  !
  open (mtout, file=flout, form='unformatted', &
    &     access='direct', recl=4*imut*jmut)
  !
  write (mtout, rec=1, iostat=ios) mag4
  if(ios /= 0) write(*, *) 'writing error in file:', mtout
  !
  close ( mtout )
!====================================================
end program mag_tau
