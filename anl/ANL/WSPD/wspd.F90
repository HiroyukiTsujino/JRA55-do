!wspd.F90
!====================================================
!
! calculate |(u, v)|
!
!====================================================
program wspd
  !
  use oc_mod_param, only : &
  &   imut, jmut
  !
  use oc_structure, only :    &
  &   read_topo,              & !-----------------------
  &   aexl, atexl,            &
#ifdef OGCM_SPHERICAL
  &   set_hgrids,             &
  &   calc_scale,             &
#else /* OGCM_SPHERICAL */
  &   read_scale,             &
#endif /* OGCM_SPHERICAL */
  &   a_bl, a_br, a_tl, a_tr
  !
  !----------------------------------------------
  !
  implicit none
  !
  real(8) :: u(imut, jmut)
  real(8) :: v(imut, jmut)
  real(4) :: u4(imut, jmut)
  real(4) :: v4(imut, jmut)
  real(8) :: flgu(imut, jmut)
  real(8) :: flgt(imut, jmut)
  !
  real(8) :: spd(imut, jmut)
  real(4) :: spd4(imut, jmut)
  !
  real(4), save :: undef = -9.99e33
  !
  ! 入出力ファイル
  !
  character(len=256)    :: fltopo   ! 海底地形ファイル
  character(len=256)    :: flin
  character(len=256)    :: flout    ! 出力ファイル
  character(len=256)    :: file_vgrid
  character(len=256)    :: file_scale
  !
  namelist /nml_wspd/ fltopo, file_vgrid, file_scale, undef, flin, flout
  !
  integer(4), parameter :: mttmp    = 80
  integer(4), parameter :: mtin     = 81
  integer(4), parameter :: mtout    = 84
  integer(4) :: ios       !  入出力エラーチェック用
  !
  integer(4) :: i, j
  !==============================================
  !
  ! 入力パラメタ既定値
  !
  fltopo = 'topo.d'
  flin  = 'hs_uv.d'
  flout  = 'spd.d'
  !
  ! 標準入力から読み込み
  !
  read(unit=5, nml_wspd)
  write(*,*) 'fltopo    :', trim(fltopo)
#ifdef OGCM_SPHERICAL
#ifdef OGCM_VARIABLE
  write(*,*) 'file_vgrid:', trim(file_vgrid)
#endif /* OGCM_VARIABLE */
#else /* OGCM_SPHERICAL */
  write(*,*) 'file_scale:', trim(file_scale)
#endif /* OGCM_SPHERICAL */
  write(*,*) 'UNDEF     :', undef
  write(*,*) 'flin      :', trim(flin)
  write(*,*) 'flout     :', trim(flout)
  !
  !----------------------------------------------
  !
  !  地形の読み込み、海陸インデックス、層厚設定
  !
  call read_topo(fltopo)
#ifdef OGCM_SPHERICAL
#ifdef OGCM_VARIABLE
  call set_hgrids(file_vgrid)
#else /* OGCM_VARIABLE */
  call set_hgrids
#endif /* OGCM_VARIABLE */
  call calc_scale
#else /* OGCM_SPHERICAL */
  call read_scale(file_scale)
#endif /* OGCM_SPHERICAL */
  !
  !==============================================
  !
  ! 入力
  !
  open (mtin, file=flin, form='unformatted', access='direct', recl=4*imut*jmut)
  read (mtin, rec=1, iostat=ios) u4
  if(ios /= 0) write(*, *) 'reading error u in file:', mtin
  read (mtin, rec=2, iostat=ios) v4
  if(ios /= 0) write(*, *) 'reading error v in file:', mtin
  close (mtin)
  flgu(:,:) = 1.d0
  where(u4(:,:) == undef)
    u4(:,:) = 0.0
    v4(:,:) = 0.0
    flgu(:,:) = 0.d0
  end where
  u(:,:) = aexl(:,:,1)*real(u4(:,:),8)
  v(:,:) = aexl(:,:,1)*real(v4(:,:),8)
  do j = 2, jmut
    do i = 2, imut
      flgt(i,j) = max(flgu(i-1,j  ), flgu(i,j  ), &
        &             flgu(i-1,j-1), flgu(i,j-1) )
    end do
  end do
#ifdef OGCM_CYCLIC
  u(1:2, 1:jmut)=u(imut-3:imut-2, 1:jmut)
  v(1:2, 1:jmut)=v(imut-3:imut-2, 1:jmut)
  flgu(1:2, 1:jmut)=flgu(imut-3:imut-2, 1:jmut)
  flgt(1:2, 1:jmut)=flgt(imut-3:imut-2, 1:jmut)
#endif /* OGCM_CYCLIC */
  !
  !---------------------------------------------------
  !
  ! calculate wind speed at t-grid from u,v at u-grid
  !
  call cal_spd
  !
  !---------------------------------------------------
  !
  ! 出力
  !
  spd4(1:imut, 1:jmut) = real(spd(1:imut, 1:jmut),4)
  where(atexl(1:imut, 1:jmut, 1) == 0.0d0)
    spd4(1:imut, 1:jmut) = undef
  end where
  where(flgt(1:imut, 1:jmut) == 0.d0)
    spd4(1:imut, 1:jmut) = undef
  end where
write(*,*) 'maxval: ', maxval(spd4)
  !
  open (mtout, file=flout, form='unformatted', &
    &     access='direct', recl=4*imut*jmut)
  !
  write (mtout, rec=1, iostat=ios) spd4(:,:)
  if(ios /= 0) write(*, *) 'writing error in file:', mtout
  !
  close ( mtout )
  !
contains
!====================================================
!
!  calculate relative vorticity
!
!====================================================
subroutine cal_spd
  !
  integer(4) :: i, j
  real(8) :: spdu(imut, jmut)
  !
  spdu(1:imut, 1:jmut) =0.0d0
  spd(1:imut, 1:jmut) =0.0d0
  !
  do j = 1, jmut
    do i = 1, imut
      spdu(i, j) = aexl(i,j,1)*sqrt(u(i,j)*u(i,j) +v(i,j)*v(i,j))
    end do
  end do
  !
  do j = 2, jmut
    do i = 2, imut
      spd(i, j) = atexl(i,j,1) * flgt(i,j) *                               &
        &    ( aexl(i-1,j  ,1)*flgu(i-1,j  )*a_br(i-1,j  )*spdu(i-1,j  )   &
        &     +aexl(i  ,j  ,1)*flgu(i  ,j  )*a_bl(i  ,j  )*spdu(i  ,j  )   &
        &     +aexl(i-1,j-1,1)*flgu(i-1,j-1)*a_tr(i-1,j-1)*spdu(i-1,j-1)   &
        &     +aexl(i  ,j-1,1)*flgu(i  ,j-1)*a_tl(i  ,j-1)*spdu(i  ,j-1) ) &
        &  / ( 1.0d0 -atexl(i,j,1)                                         &
        &     +aexl(i-1,j  ,1)*flgu(i-1,j  )*a_br(i-1,j  )                 &
        &     +aexl(i  ,j  ,1)*flgu(i  ,j  )*a_bl(i  ,j  )                 &
        &     +aexl(i-1,j-1,1)*flgu(i-1,j-1)*a_tr(i-1,j-1)                 &
        &     +aexl(i  ,j-1,1)*flgu(i  ,j-1)*a_tl(i  ,j-1) )
    end do
  end do
#ifdef OGCM_CYCLIC
  spd(1, 1:jmut)=spd(imut-3, 1:jmut)
#endif /* OGCM_CYCLIC */
  !
end subroutine cal_spd
!====================================================
end program wspd
