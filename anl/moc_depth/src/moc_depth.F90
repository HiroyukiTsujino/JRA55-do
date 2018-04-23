!-*-F90-*-
!moc_depth.F90
!====================================================
!
! Make Meridional Overturning Streamfunction
!
!             original version written by M.Hirabara
!  modified for variable grid  @040720 by S.Yukimoto
!  make using the model's modules @060927 by S.Yukimoto
!  modified for readability  @20070416 by M.Hirabara
!====================================================
program meridional_streamfunction

  use libmxe_para, only :    &
       & pi, radian, radian_r

  use oc_mod_param, only :    &
       & imut, jmut, km,      &
       & ksgm, dz,            &
       & slat0, slon0,        &
       & nplat, nplon,        &
       & splat, splon,        &
       & ro,    cp,           &
       & para,                &
       & param_mxe__ini

  use oc_structure, only  :   &
       & dep,                 & ! UVTSボックス境界（上面）の深さ
       & ho4, exnn,           & ! 水深、層数
       & aexl, atexl,         & ! 海陸インデックス
       & ho4bbl, exnnbbl,     & ! BBL層厚、層数
       & aexlbbl, atexlbbl,   & ! BBLインデックス
       & dzu,                 & ! UVボックスの厚さ
       & thcksgm, thcksgmr,   & ! sigma 層の厚さ、逆数
       & a_tl  , a_tr  ,      & ! 格子面積
       & a_bl  , a_br  ,      &
       & dx_tl , dx_tr ,      & ! 東西長
       & dx_bl , dx_br ,      &
       & dy_tl , dy_tr ,      & ! 南北長
       & dy_bl , dy_br ,      &
       & dxtdeg, dytdeg,      & ! T点を中心とするモデル格子間隔(度)
       & dxudeg, dyudeg,      & ! U点を中心とするモデル格子間隔(度)
       & slat, slon,          &
       & alatt, alatu,        & ! モデル座標上緯度(psi)経度(mu)
       & alont, alonu,        &
       & glonu, glatu,        &
       & grid, topo,          &
       & structure_mxe__ini
       
  use libmxe_topo, only :    &
       & libmxe_topo__updatedz

  implicit none

  !----------------------------------------------------
  ! 地理座標パラメタ
  ! 
  real(8)    :: slatg, elatg, dlatg
  integer(4) :: jmgeo
  !
  ! 海洋モデル地形
  !
  logical :: l_read_basin = .true.
  integer(4) :: basin_index = 0
  character(len=256) :: flibas ! basinインデックスファイル
  integer(4), allocatable :: ibas(:,:)      ! basin区分, UV-Grid
  !
  !             0:LAND, 1:ATL, 2:PAC, 3:IND, 4:MED, 9:SO
  !
  integer(4), allocatable :: mask_glb(:,:) ! 全球海洋      1 or -1
  integer(4), allocatable :: mask_atl(:,:) ! 大西洋+地中海 1 or -1
  integer(4), allocatable :: mask_pac(:,:) ! 太平洋        1 or -1
  integer(4), allocatable :: mask_ind(:,:)
  real(8), allocatable :: ibm(:,:)      ! 積算で使用する海盆マスク(1 or 0)

  logical :: l_exclude
  character(len=256) :: file_exclude
  real(8), allocatable :: exclude(:,:)

  logical :: l_specify
  character(len=256) :: file_specify
  real(8), allocatable :: specify(:,:)

  logical :: l_check_used
  character(len=256) :: file_check_used
  real(8), allocatable :: check_used(:,:)

  real(8), allocatable :: u(:,:,:)
  real(8), allocatable :: v(:,:,:)
  real(8), allocatable :: ssh(:,:)

  real(8), allocatable :: sv(:,:)  ! 緯度ごと層ごとの北向き流量     [cgs]

  real(8) :: phi_std_degree ! この緯度に沿って地球を一周
  real(8) :: lambdau, phiu

  ! 子午面循環流線関数 [Sv]
  !     鉛直流量(WLWL)と同じところで定義される
  !     最下層T-boxの下面でk=1, 海面でk=km+1
  !     海面淡水フラックスがあるとき、海面で0にならない。
  !
  real(4), allocatable :: strmf_g(:,:)  ! Global
  real(4), allocatable :: strmf_a(:,:)  ! Atlantic
  real(4), allocatable :: strmf_p(:,:)  ! Pacific
  real(4), allocatable :: strmf_i(:,:)  ! Pacific
  !
  real(4), parameter :: cgs2Sv  = 1.0e-12      ! cm^3/s => Sv   ( 10^6 m3/s )
  real(4), parameter :: cgs2kgs = ro * 1.0e-3  ! cm^3/s => kg/s
  real(4), save      :: conv_unit = cgs2Sv     ! default
  !
  character(len=8), parameter :: str_Sv   = ' [Sv]'
  character(len=8), parameter :: str_kgs  = ' [kg/s]'
  character(len=8), save      :: str_unit = str_Sv
  !
  real(4), allocatable :: d3_r4(:,:,:)
  real(4), allocatable :: d2_r4(:,:)
  !
  ! 入出力ファイル
  !
  character(len=256)    :: flin_u ! 入力ファイル
  character(len=256)    :: flin_v ! 入力ファイル
  character(len=256)    :: flin_ssh ! 入力ファイル
  character(len=256)    :: flout  ! 出力ファイル
  !
  integer(4) :: ios          !  入出力エラーチェック用
  integer(4), parameter :: lun = 10
  integer(4), parameter :: mtin_u   = 80
  integer(4), parameter :: mtin_v   = 81
  integer(4), parameter :: mtin_ssh = 84
  integer(4), parameter :: mtout    = 86
  !
  integer(4), save :: lrec_out
  !
  integer(4) :: irecw
  integer(4) :: i, j, k, m, jj
  integer(4) :: nkai
  integer(4) :: jg
  character(80) :: fmt_ibas
  !
  !==============================================

  namelist /nml_moc_dep/ flin_u, flin_v, flin_ssh, flout, &
       & l_read_basin, basin_index, flibas, &
       & l_exclude, file_exclude, &
       & l_specify, file_specify, &
       & l_check_used, file_check_used, &
       & slatg, elatg, dlatg
  !---------------------------------------------
  ! 入力パラメタ規定値

  flin_u   = 'hs_u.d'
  flin_v   = 'hs_v.d'
  flin_ssh = 'hs_ssh.d'
  flout    = 'moc.d'
  flibas   = 'basin_map.txt'

  ! 標準入力から読み込み

  read(unit=5, nml_moc_dep)

  write(6,*) 'flin_u   :', trim(flin_u)
  write(6,*) 'flin_v   :', trim(flin_v)
  write(6,*) 'flin_ssh :', trim(flin_ssh)
  write(6,*) 'flout    :', trim(flout)
  write(6,*) 'flibas   :', trim(flibas)

  !----------------------------------------------
  ! 海洋モデル格子情報等の準備

  call param_mxe__ini
  call structure_mxe__ini

  jmgeo = int((elatg - slatg + 1.0d-6)) / dlatg

  write(*,"(a)")                'XDEF      1  LINEAR    0.0000   1.0'
  write(*,"(a,i5,a,f8.4,f8.4)") 'YDEF   ', jmgeo, '  LINEAR  ', slatg + 0.5d0*dlatg, dlatg
  write(*,"(a,i5,a)")           'ZDEF   ', km+1,  '  LEVELS'
  write(*,"(5f8.2)") (dep(k) * 1.d-2, k = km+1,1,-1)     !  cm => m

  lrec_out = jmgeo*(km+1)*4

  !---------------------------------------------

  allocate(ibas(imut,jmut))
  allocate(mask_glb(imut, jmut))
  allocate(mask_atl(imut, jmut))
  allocate(mask_pac(imut, jmut))
  allocate(mask_ind(imut, jmut))
  allocate(ibm(imut, jmut))

  allocate(u(imut, jmut, km))
  allocate(v(imut, jmut, km))
  allocate(ssh(imut, jmut))
  allocate(sv(jmgeo, km))
  allocate(strmf_g(jmgeo, km+1))
  allocate(strmf_a(jmgeo, km+1))
  allocate(strmf_p(jmgeo, km+1))
  allocate(strmf_i(jmgeo, km+1))

  allocate(d3_r4(imut,jmut,km))
  allocate(d2_r4(imut,jmut))

  !----------------------------------------------
  ! make basin index

  if (l_read_basin) then
    ! read from file
    write(fmt_ibas,"(1a,i4,1a)") "(i6,",imut,"i1)"
    write(6,*) ' Basin index is read from ', trim(flibas)
    open(lun, file=flibas, form="formatted")  
    do j = jmut, 1, -1
      read(lun, fmt=fmt_ibas,iostat=ios) jj,(ibas(i,j), i=1, imut)
      if (ios /= 0) write(6,*) 'reading error in file:', trim(flibas)
      if (jj /= j) then
        write(6,*) ' data error in ',trim(flibas)
        stop 999
      end if
    end do
    close(lun)
  else
    write(6,*) ' Basin index is set to ', basin_index
    ibas(:,:) = basin_index * int(aexl(:,:,1) + 1.0d-4)
  end if

  mask_glb(1:imut, 1:jmut) = -1
  mask_atl(1:imut, 1:jmut) = -1
  mask_pac(1:imut, 1:jmut) = -1
  mask_ind(1:imut, 1:jmut) = -1

  do j = 1, jmut
    do i = 1, imut
      m = ibas(i, j)
      if (m > 0) then
        mask_glb(i, j) = 1           ! Ocean: 1, Land: -1
!        if(m == 1 .or. m == 4) mask_atl(i, j) = 1 ! Atlantic or Mediterranean
        if(m == 1)             mask_atl(i, j) = 1 ! Atlantic
        if(m == 2)             mask_pac(i, j) = 1 ! Pacific
        if(m == 3)             mask_ind(i, j) = 1 ! Indian
      end if
    end do
  end do
  !
  !  Pacific side half of the Arctic Sea is excluded from ATL
  !
  do j = 1, jmut
    do i = 1, imut
      lambdau = glonu(i,j) * radian_r
      phiu    = glatu(i,j) * radian_r
      if(phiu > 60.d0*radian_r .and. cos(lambdau) < 0.d0) mask_atl(i, j) = -1
    end do
  end do
  !----------------------------------------------

  if (l_exclude) then
    open (lun, file=file_exclude, form='unformatted', access='direct', recl=4*imut*jmut)
    read (lun, rec=1) d2_r4
    close(lun)
    allocate(exclude(1:imut,1:jmut))
    exclude(1:imut,1:jmut) = max(real(d2_r4(1:imut,1:jmut),8),0.0d0)
  end if

  if (l_specify) then
    open (lun, file=file_specify, form='unformatted', access='direct', recl=4*imut*jmut)
    read (lun, rec=1) d2_r4
    close(lun)
    allocate(specify(1:imut,1:jmut))
    specify(1:imut,1:jmut) = max(real(d2_r4(1:imut,1:jmut),8),0.0d0)
  end if

  if (l_check_used) then
    allocate(check_used(1:imut,1:jmut))
    check_used(1:imut,1:jmut) = 0.0d0
  end if
  !==============================================
  !
  ! 入出力ファイルオープン
  !
  open (mtin_u, file=flin_u, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_v, file=flin_v, form='unformatted', &
    &  access='direct', recl=4*imut*jmut*km)
  !
  open (mtin_ssh, file=flin_ssh, form='unformatted', &
    &  access='direct', recl=4*imut*jmut)
  !
  open (mtout, file=flout, form='unformatted',&
    &  access='direct', recl=lrec_out )
  irecw=1
  !
  !-------------------------
  !
  ! U
  !
  read (mtin_u, rec=1) d3_r4
  u(:,:,:) = aexl(:,:,:)*real(d3_r4(:,:,:),8)
  !
  if (para%lcyclic) then
    u(1:2,         1:jmut, 1:km)=u(imut-3:imut-2, 1:jmut, 1:km)
    u(imut-1:imut, 1:jmut, 1:km)=u(3:4,           1:jmut, 1:km)
  end if
  !
  ! V
  !
  read (mtin_v, rec=1) d3_r4
  v(:,:,:) = aexl(:,:,:)*real(d3_r4(:,:,:),8)
  !
  if (para%lcyclic) then
    v(1:2,         1:jmut, 1:km)=v(imut-3:imut-2, 1:jmut, 1:km)
    v(imut-1:imut, 1:jmut, 1:km)=v(3:4,           1:jmut, 1:km)
  end if

  !-------------------------
  !
  !  SSH
  !
  read (mtin_ssh, rec=1) d2_r4
  ssh(:,:) = atexl(:,:,1)*real(d2_r4(:,:),8)
  !
  if (para%lcyclic) then
    ssh(1:2,         1:jmut)=ssh(imut-3:imut-2, 1:jmut)
    ssh(imut-1:imut, 1:jmut)=ssh(3:4,           1:jmut)
  end if

  close ( mtin_u )
  close ( mtin_v )
  close ( mtin_ssh)

  write(6,*) ' READING Data done '

  call libmxe_topo__updatedz(ssh,topo,para,grid)
  dzu(:,:,:) = topo%dzu(:,:,:)

  if (l_exclude) then
    do k = 1, km
      dzu(:,:,k) = dzu(:,:,k) * (1.0d0 - exclude(:,:))
!      u(:,:,k) = u(:,:,k) * (1.0d0 - exclude(:,:))
!      v(:,:,k) = v(:,:,k) * (1.0d0 - exclude(:,:))
    end do
  end if

  if (l_specify) then
    do k = 1, km
      dzu(:,:,k) = dzu(:,:,k) * specify(:,:)
!      u(:,:,k) = u(:,:,k) * specify(:,:)
!      v(:,:,k) = v(:,:,k) * specify(:,:)
    end do
  end if

  !--------------------
  !  Global
  !--------------------
  write(6,*) '       Global '
  !
  do j = 1, jmut
    do i= 1, imut
      ibm(i, j) = 0.5d0 + 0.5d0*real(mask_glb(i, j), 8)
    end do
  end do
  !
  sv(1:jmgeo, 1:km)=0.d0
  !
  do jg=1, jmgeo
    !
    phi_std_degree = slatg + dlatg * (real(jg,8) - 0.5d0)
    call around_the_world(jg, phi_std_degree, l_check_used)
    !
  end do
  !
  strmf_g(1:jmgeo, 1)=0.0
  do k=2, km+1
    do j=2, jmgeo
      strmf_g(j,k)=strmf_g(j,k-1)-real(sv(j,km+2-k), 4) * conv_unit
    end do
  end do
  !
  !--------------------
  !  Atlantic
  !--------------------
  write(6,*) '       Atlantic '
  !
  do j = 1, jmut
    do i= 1, imut
      ibm(i, j) = 0.5d0 + 0.5d0*real(mask_atl(i, j), 8)
    end do
  end do
  !
  sv(1:jmgeo, 1:km)=0.d0
  !
  do jg=1, jmgeo
    !
    phi_std_degree = slatg + dlatg * (real(jg,8) - 0.5d0)
    call around_the_world(jg, phi_std_degree, .false.)
    !
  end do
  !
  strmf_a(1:jmgeo, 1)=0.0
  do k=2, km+1
    do j=2, jmgeo
      strmf_a(j,k)=strmf_a(j,k-1)-real(sv(j,km+2-k), 4) * conv_unit
    end do
  end do
  !
  !--------------------
  !  Pacific
  !--------------------
  write(6,*) '       Pacific '
  !
  do j = 1, jmut
    do i= 1, imut
      ibm(i, j) = 0.5d0 + 0.5d0*real(mask_pac(i, j), 8)
    end do
  end do
  !
  sv(1:jmgeo, 1:km)=0.d0
  !
  do jg=1, jmgeo
    !
    phi_std_degree = slatg + dlatg * (real(jg,8) - 0.5d0)
    call around_the_world(jg, phi_std_degree, .false.)
    !
  end do
  !
  strmf_p(1:jmgeo, 1)=0.0
  do k=2, km+1
    do j=2, jmgeo
      strmf_p(j,k)=strmf_p(j,k-1)-real(sv(j,km+2-k), 4) * conv_unit
    end do
  end do
  !
  !--------------------
  !  Indian
  !--------------------
  write(6,*) '       Indian '
  !
  do j = 1, jmut
    do i= 1, imut
      ibm(i, j) = 0.5d0 + 0.5d0*real(mask_ind(i, j), 8)
    end do
  end do
  !
  sv(1:jmgeo, 1:km)=0.d0
  !
  do jg=1, jmgeo
    !
    phi_std_degree = slatg + dlatg * (real(jg,8) - 0.5d0)
    call around_the_world(jg, phi_std_degree, .false.)
    !
  end do
  !
  strmf_i(1:jmgeo, 1)=0.0
  do k = 2, km+1
    do j = 2, jmgeo
      strmf_i(j,k)=strmf_i(j,k-1)-real(sv(j,km+2-k), 4) * conv_unit
    end do
  end do
  !
  !--------------------------------------------
  write ( mtout, rec=irecw )  strmf_g(1:jmgeo, 1:km+1)
  irecw=irecw+1
  !
  write ( mtout, rec=irecw )  strmf_a(1:jmgeo, 1:km+1)
  irecw=irecw+1
  !
  write ( mtout, rec=irecw )  strmf_p(1:jmgeo, 1:km+1)
  irecw=irecw+1
  !
  write ( mtout, rec=irecw )  strmf_i(1:jmgeo, 1:km+1)  &
    &                        +strmf_p(1:jmgeo, 1:km+1)
  irecw=irecw+1
  !
  close ( mtout )
  !
  if (l_exclude) then
    deallocate(exclude)
  end if

  if (l_specify) then
    deallocate(specify)
  end if

  if (l_check_used) then
    open (lun, file=file_check_used, form='unformatted', access='direct', recl=4*imut*jmut)
    d2_r4(1:imut,1:jmut) = real(check_used(1:imut,1:jmut),4)
    write(lun, rec=1) d2_r4
    d2_r4(1:imut,1:jmut) = real(aexl(1:imut,1:jmut,1),4)
    write(lun, rec=2) d2_r4
    d2_r4(1:imut,1:jmut) = real(glonu(1:imut,1:jmut),4)
    write(lun, rec=3) d2_r4
    d2_r4(1:imut,1:jmut) = real(glatu(1:imut,1:jmut),4)
    write(lun, rec=4) d2_r4
    close(lun)
    deallocate(check_used(1:imut,1:jmut))
  end if

  deallocate(ibas)
  deallocate(mask_glb)
  deallocate(mask_atl)
  deallocate(mask_pac)
  deallocate(mask_ind)
  deallocate(ibm)

  deallocate(u)
  deallocate(v)
  deallocate(ssh)
  deallocate(sv)
  deallocate(strmf_g)
  deallocate(strmf_a)
  deallocate(strmf_p)
  deallocate(strmf_i)

  deallocate(d3_r4)
  deallocate(d2_r4)

contains
!====================================================
!
!  指定緯度で世界一周
!
!====================================================
subroutine around_the_world(jg, phi_std_degree, l_check)
  !
  integer(4), intent(IN) :: jg
  real(8),    intent(IN) :: phi_std_degree
  logical,    intent(in) :: l_check
  !
  integer(4) :: kmax
  !
  integer(4) :: i00, j00 ! start point
  integer(4) :: ic, jc   ! next point
  integer(4) :: jend, jmax
  !
  real(8)    :: phi_std
  real(8)    :: mu0,  mu2
  real(8)    :: psi0, psi1, psi2
  !
  real(8)    :: lambda00,  lambda20,  lambda01,  lambda02
  real(8)    :: phi00, phi20, phi01, phi02
  !
  real(8)    :: sqphi00, sqphi20, sqphi01, sqphi02
  real(8)    :: sqphimin
  real(8)    :: ustar, vstar
  real(8)    :: use_prev, use_next
  real(8)    :: hl1
  !
  real(8),allocatable :: u_done(:,:)
  !
  integer(4) :: j, k
  !------------------------------------------------------

  allocate(u_done(1:imut,1:jmut))

  if (para%lbbl) then
    kmax = km - 1
  else
    kmax = km
  end if

  phi_std = phi_std_degree * radian_r

  u_done(1:imut, 1:jmut) = 0.d0

  ! search starting longitude

  if (para%lspherical) then
    hl1 = maxval(dytdeg)
    sqphimin = (hl1*radian_r)**2
  else
    hl1 = (elatg - nplat)
    sqphimin = (hl1*radian_r)**2
  end if

  if (para%lcyclic) then
    i00 = 3
    u_done(1:2,    1:jmut)     = 1.d0
    u_done(imut-1:imut,1:jmut) = 1.d0
  else
    i00 = 2
    u_done(1,   1:jmut) = 1.d0
    u_done(imut,1:jmut) = 1.d0
  end if

  u_done(1:imut, 1)    = 1.d0
  u_done(1:imut, jmut) = 1.d0

  if (para%ltripolar) then
    u_done(1:imut, jmut-2:jmut-1) = 1.d0
  end if

  j00 = 0
  if (para%ltripolar) then
    jmax = jmut - 3
  else
    jmax = jmut - 1
  end if

  do j = 2, jmax
    phi00 = glatu(i00,j) * radian_r
    sqphi00 = (phi00-phi_std)*(phi00-phi_std)
    if (sqphi00 < sqphimin) then
      sqphimin = sqphi00
      j00 = j
    end if
  end do

  if (j00 == 0) then
    write(6,*) ' Latitude', phi_std_degree ,' is out of model domain, skipping... '
    return
  end if

  !------------------------------------------------

  u_done(i00,j00) = 1.d0

  use_next = use_this_grid(i00,j00)

  if (l_check) then
    check_used(i00,j00) = check_used(i00,j00) + 0.5d0 * aexl(i00,j00,1)*ibm(i00,j00) * use_next
  end if

  do k = 1, kmax
    vstar = dx_tl(i00,j00)*dzu(i00,j00,k)*v(i00,j00,k)*ibm(i00,j00) * use_next
    sv(jg,k) = sv(jg,k) + vstar
  end do

  if (para%lbbl) then
    k = max(exnn(i00,j00), exnn(i00-1,j00))
    vstar = dx_tl(i00,j00)*dzu(i00,j00,km)*v(i00,j00,km)*ibm(i00,j00) * use_next
    sv(jg,k) = sv(jg,k) + vstar
  end if

  ic = i00+1  !  移動先
  jc = j00

  u_done(ic, jc) = 1.d0

  use_prev = use_next
  use_next = use_this_grid(ic,jc)

  ! ustar, vstar の定義
  !
  !                i,j
  !       i-1,j uv--V*--uv i,j
  !              |      |
  !       i-1,j U*  TS  U* i,j
  !              |      |
  !     i-1,j-1 uv--V*--uv i,j-1
  !                i,j-1
  !

  if (l_check) then
    check_used(i00,j00) = check_used(i00,j00) + 0.5d0 * aexl(i00,j00,1)*ibm(i00,j00) * use_prev
    check_used(ic,jc)   = check_used(ic,jc)   + 0.5d0 * aexl(ic ,jc ,1)*ibm(ic ,jc) * use_next
  end if

  do k = 1, kmax
    vstar = dx_tr(i00,j00)*dzu(i00,j00,k)*v(i00,j00,k)*ibm(i00,j00) * use_prev &
      &   + dx_tl(ic ,jc )*dzu(ic ,jc ,k)*v(ic ,jc ,k)*ibm(ic ,jc ) * use_next
    sv(jg,k) = sv(jg,k) + vstar
  end do

  if (para%lbbl) then
    k = max(exnn(i00,j00), exnn(ic,jc))
    vstar = dx_tr(i00,j00)*dzu(i00,j00,km)*v(i00,j00,km)*ibm(i00,j00) * use_prev &
       &  + dx_tl(ic ,jc )*dzu(ic ,jc ,km)*v(ic ,jc ,km)*ibm(ic ,jc ) * use_next
    sv(jg,k) = sv(jg,k) + vstar
  end if

  jend = j00  !  terminater

  LOOP_AROUND_THE_WORLD : do while (ic < imut)

!    if (l_check) then
!      if (jg == jmgeo) write(11,*) ic, jc, glatu(ic,jc)
!    end if

    i00 = ic
    j00 = jc

    phi00 = glatu(i00  ,j00  ) * radian_r
    phi20 = glatu(i00+1,j00  ) * radian_r
    phi02 = glatu(i00  ,j00+1) * radian_r
    phi01 = glatu(i00  ,j00-1) * radian_r

    if ((para%ltripolar) .and. (phi_std_degree > nplat)) then
      sqphi20 = (phi20-phi_std)*(phi20-phi_std) +u_done(i00+1,j00  )
      sqphi02 = (phi02-phi_std)*(phi02-phi_std) +u_done(i00,  j00+1)
      sqphi01 = (phi01-phi_std)*(phi01-phi_std) +u_done(i00  ,j00-1)
      if (min(sqphi20, sqphi02, sqphi01) > 1.0d0) then
        exit LOOP_AROUND_THE_WORLD
      end if
    else
      sqphi20 = (phi20-phi_std)*(phi20-phi_std)
      sqphi02 = (phi02-phi_std)*(phi02-phi_std)
      sqphi01 = (phi01-phi_std)*(phi01-phi_std)
      if (min(sqphi20, sqphi02, sqphi01) > sqphimin) then
        exit LOOP_AROUND_THE_WORLD
      end if
    end if

    if (min(sqphi02, sqphi01) >= sqphi20) then ! 等号が必要

      ! proceed westward

      ic = i00 + 1
      jc = j00

      use_prev = use_next
      use_next = use_this_grid(ic,jc)

      ! the following operation is ad hoc, needs sophistication

      if (para%ltripolar) then
        hl1 = use_prev + use_next
        if ((hl1 == 1.0d0) .and. (jc == jmut-3)) then
          if (use_prev == 1.0d0) then ! proceed northward
            if (l_check) then
              check_used(i00,j00) = check_used(i00,j00) + 0.5d0 * aexl(i00,j00,1)*ibm(i00,j00) * use_prev
            end if
            do k = 1, kmax
              ustar = dy_tr(i00,j00)*dzu(i00,j00,k)*u(i00,j00,k)*ibm(i00,j00) * use_prev
              sv(jg,k) = sv(jg,k) - ustar
            end do
            if (para%lbbl) then
              k = max(exnn(i00, j00), exnn(i00, j00+1))
              ustar = dy_tr(i00, j00)*dzu(i00, j00, km)*u(i00, j00, km)*ibm(i00, j00) * use_prev
              sv(jg,k) = sv(jg,k) - ustar
            end if
          else ! proceed southward
            if (l_check) then
              check_used(ic,jc) = check_used(ic,jc) + 0.5d0 * aexl(ic,jc,1)*ibm(ic,jc) * use_next
            end if
            do k = 1, kmax
              ustar = dy_tr(ic,jc)*dzu(ic,jc,k)*u(ic,jc,k)*ibm(ic,jc) * use_next
              sv(jg,k) = sv(jg,k) + ustar
            end do
            if (para%lbbl) then
              k = max(exnn(ic, jc), exnn(ic, jc+1))
              ustar = dy_tr(ic, jc)*dzu(ic, jc, km)*u(ic, jc, km)*ibm(ic, jc) * use_next
              sv(jg,k) = sv(jg,k) + ustar
            end if
          end if
          u_done(ic, jc)=1.d0
          cycle LOOP_AROUND_THE_WORLD
        end if
      end if

      ! the above operation is ad hoc, needs sophistication

      if(u_done(ic,jc) == 1.0d0) then
        if (l_check) then
          check_used(i00,j00) = check_used(i00,j00) + 0.5d0 * aexl(i00,j00,1)*ibm(i00,j00) * use_prev
        end if
        do k = 1, kmax
          vstar = dx_tr(i00,j00)*dzu(i00,j00,k)*v(i00,j00,k)*ibm(i00,j00) * use_prev
          sv(jg,k) = sv(jg,k) + vstar
        end do
        if (para%lbbl) then
          k = max(exnn(i00, j00), exnn(ic, jc))
          vstar = dx_tr(i00, j00)*dzu(i00, j00, km)*v(i00, j00, km)*ibm(i00, j00) * use_prev
          sv(jg,k) = sv(jg,k) +vstar
        end if
        exit LOOP_AROUND_THE_WORLD
      end if

      if (l_check) then
        check_used(i00,j00) = check_used(i00,j00) + 0.5d0 * aexl(i00,j00,1)*ibm(i00,j00) * use_prev
        check_used(ic ,jc ) = check_used(ic ,jc ) + 0.5d0 * aexl(ic ,jc ,1)*ibm(ic ,jc ) * use_next
      end if

      do k = 1, kmax
        vstar = dx_tr(i00,j00)*dzu(i00,j00,k)*v(i00,j00,k)*ibm(i00,j00) * use_prev &
          &   + dx_tl(ic ,jc )*dzu(ic ,jc ,k)*v(ic ,jc ,k)*ibm(ic ,jc ) * use_next
        sv(jg,k) = sv(jg,k) + vstar
      end do

      if (para%lbbl) then
        k = max(exnn(i00, j00), exnn(ic, jc))
        vstar = dx_tr(i00, j00)*dzu(i00, j00, km)*v(i00, j00, km)*ibm(i00, j00) * use_prev &
          &   + dx_tl(ic , jc )*dzu(ic , jc , km)*v(ic , jc , km)*ibm(ic , jc ) * use_next
        sv(jg,k)=sv(jg,k) + vstar
      end if

    else if (min(sqphi20, sqphi01) >= sqphi02) then

      ! proceed northward

      ic = i00
      jc = j00 + 1

      use_prev = use_next
      use_next = use_this_grid(ic,jc)

      if(u_done(ic,jc) == 1.0d0) then
        if (l_check) then
          check_used(i00,j00) = check_used(i00,j00) + 0.5d0 * aexl(i00,j00,1)*ibm(i00,j00) * use_prev
        end if
        do k = 1, kmax
          ustar = dy_tr(i00, j00)*dzu(i00, j00, k)*u(i00, j00, k)*ibm(i00,j00) * use_prev
          sv(jg,k) = sv(jg,k) - ustar
        end do
        if (para%lbbl) then
          k = max(exnn(i00, j00), exnn(ic, jc))
          ustar = dy_tr(i00, j00)*dzu(i00, j00, km)*u(i00, j00, km)*ibm(i00,j00) * use_prev
          sv(jg,k) = sv(jg,k) - ustar
        end if
        exit LOOP_AROUND_THE_WORLD
      end if

      if (l_check) then
        check_used(i00,j00) = check_used(i00,j00) + 0.5d0 * aexl(i00,j00,1)*ibm(i00,j00) * use_prev
        check_used(ic,jc  ) = check_used(ic ,jc ) + 0.5d0 * aexl(ic ,jc ,1)*ibm(ic ,jc ) * use_next
      end if

      do k = 1, kmax
        ustar = dy_tr(i00, j00)*dzu(i00, j00, k)*u(i00, j00, k)*ibm(i00,j00) * use_prev &
          &   + dy_br(ic , jc )*dzu(ic , jc , k)*u(ic , jc , k)*ibm(ic ,jc ) * use_next
        sv(jg,k) = sv(jg,k) -ustar
      end do
      if (para%lbbl) then
        k=max(exnn(i00, j00), exnn(ic, jc))
        ustar = dy_tr(i00, j00)*dzu(i00, j00, km)*u(i00, j00, km)*ibm(i00,j00) * use_prev &
          &   + dy_br(ic , jc )*dzu(ic , jc , km)*u(ic , jc , km)*ibm(ic ,jc ) * use_next
        sv(jg,k) = sv(jg,k) -ustar
      end if

    else

      ! proceed southward

      ic = i00
      jc = j00 - 1

      use_prev = use_next
      use_next = use_this_grid(ic,jc)

      if (u_done(ic,jc) == 1.0d0) then
        if (l_check) then
          check_used(i00,j00) = check_used(i00,j00) + 0.5d0 * aexl(i00,j00,1)*ibm(i00,j00) * use_prev
        end if
        do k = 1, kmax
          ustar = dy_br(i00, j00)*dzu(i00, j00, k)*u(i00, j00, k)*ibm(i00,j00) * use_prev
          sv(jg,k) = sv(jg,k) + ustar
        end do
        if (para%lbbl) then
          k=max(exnn(i00, j00), exnn(ic, jc))
          ustar = dy_br(i00, j00)*dzu(i00, j00, km)*u(i00, j00, km)*ibm(i00,j00) * use_prev
          sv(jg,k) = sv(jg,k) + ustar
        end if
        exit LOOP_AROUND_THE_WORLD
      end if

      if (l_check) then
        check_used(i00,j00) = check_used(i00,j00) + 0.5d0 * aexl(i00,j00,1)*ibm(i00,j00) * use_prev
        check_used(ic ,jc ) = check_used(ic ,jc ) + 0.5d0 * aexl(ic ,jc ,1)*ibm(ic ,jc ) * use_next
      end if

      do k = 1, kmax
        ustar = dy_br(i00, j00)*dzu(i00, j00, k)*u(i00, j00, k)*ibm(i00,j00) * use_prev &
          &    +dy_tr(ic , jc )*dzu(ic , jc , k)*u(ic , jc , k)*ibm(ic ,jc ) * use_next
        sv(jg,k) = sv(jg,k) + ustar
      end do
      if (para%lbbl) then
        k=max(exnn(i00, j00), exnn(ic, jc))
        ustar = dy_br(i00, j00)*dzu(i00, j00, km)*u(i00, j00, km)*ibm(i00,j00) * use_prev &
             & +dy_tr(ic , jc )*dzu(ic , jc , km)*u(ic , jc , km)*ibm(ic ,jc ) * use_next
        sv(jg,k) = sv(jg,k) + ustar
      end if
    end if

    u_done(ic, jc)=1.d0

    if (para%lcyclic) then
      if (ic == imut-1 .and. jc == jend) then
        exit LOOP_AROUND_THE_WORLD
      end if
    end if
    if ((para%ltripolar) .and. (phi_std_degree > nplat)) then
      if (glatu(ic,jc) < nplat) then ! do not search geographical lat-lon region
        exit LOOP_AROUND_THE_WORLD
      end if
    end if

  end do LOOP_AROUND_THE_WORLD

  deallocate(u_done)

end subroutine around_the_world
!====================================================
real(8) function use_this_grid(ip,jp)

  integer(4),intent(in) :: ip, jp

  if ((para%ltripolar) .and. (phi_std_degree > nplat)) then
    if (abs(phi_std_degree - glatu(ip,jp)) > 0.5d0 * dlatg) then
      use_this_grid = 0.0d0
    else
      use_this_grid = 1.0d0
    end if
  else
    use_this_grid = 1.0d0
  end if

end function use_this_grid
!====================================================
end program meridional_streamfunction
