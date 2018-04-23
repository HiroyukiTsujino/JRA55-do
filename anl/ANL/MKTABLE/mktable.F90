!mktable.F90
!====================================================
!
! calculate mapping table
!
!====================================================
program mktable
  !
  use oc_mod_param, only : &
  &   imut, jmut,          &
  &   radian_r
  !
  use oc_structure, only :    &
  &   set_hgrids,             &
  &   alatt, alont,           &
  &   alatu, alonu
  !
  !----------------------------------------------
  !
  implicit none
  !
  real(4), allocatable :: glati4(:,:)
  real(4), allocatable :: gloni4(:,:)
  real(8), allocatable :: glati(:,:)
  real(8), allocatable :: gloni(:,:)

  real(4), allocatable :: uin4(:,:)
  real(4), allocatable :: vin4(:,:)
  real(4), allocatable :: din4(:,:)
  real(4), allocatable :: uout4(:,:)
  real(4), allocatable :: vout4(:,:)
  real(4), allocatable :: dout4(:,:)
  real(8), allocatable :: uin(:,:)
  real(8), allocatable :: vin(:,:)
  real(8), allocatable :: din(:,:)
  real(8), allocatable :: uout(:,:)
  real(8), allocatable :: vout(:,:)
  real(8), allocatable :: dout(:,:)

  type :: type_table
    logical    :: ldef
    integer(4) :: i(4) !  3   4
    integer(4) :: j(4) !    o
    real(8)    :: w(4) !  1   2
  end type
  type(type_table), save :: t_tbl(imut, jmut)

  type :: type_utable
    logical    :: ldef
    integer(4) :: i(4)
    integer(4) :: j(4)
    real(8)    :: w(4)
    real(8)    :: c(4) !  cosine
    real(8)    :: s(4) !  sine
  end type
  type(type_utable), save :: u_tbl(imut, jmut)
  !
  real(4), parameter :: UNDEFI = -1.E+383
  real(4), parameter :: UNDEFO = -9.99e33
  !
  ! 入出力ファイル
  !
  logical, save         :: l_make_table = .false.
  logical, save         :: l_read_latlon= .true.
  character(len=256)    :: fgrid_info
#ifdef OGCM_VARIABLE
  character(len=256)    :: file_vgrid
#endif /* OGCM_VARIABLE */
  integer(4)            :: nxin
  integer(4)            :: nyin
  integer(4)            :: nlat
  integer(4)            :: nlon
  real(4)    :: slon, slat, dlon, dlat
  character(len=256)    :: fltable
#ifdef OGCM_VARIABLE
  namelist /nml_mktable/ l_make_table, l_read_latlon, fgrid_info, nlat, nlon, &
    &                                  slat, slon, dlat, dlon,                &
    &                    file_vgrid, nxin, nyin, fltable
#else /* OGCM_VARIABLE */
  namelist /nml_mktable/ l_make_table, l_read_latlon, fgrid_info, nlat, nlon, &
    &                                  slat, slon, dlat, dlon,                &
    &                    nxin, nyin, fltable
#endif /* OGCM_VARIABLE */
  !
  logical, save         :: l_extrapolate= .false.
  character(len=256)    :: fcorein
  character(len=32)     :: fdatein
  integer(4), save      :: nvar = 1
  integer(4), save      :: nvaru = 1
  integer(4), save      :: nvarv = 1
  character(len= 1)     :: txyu
  logical, save         :: l_rot_in = .false.
  character(len=256)    :: fcoreout
  namelist /nml_iodata/  fcorein, fdatein, txyu, nvaru, nvarv, nvar, &
    &                    fcoreout, l_rot_in, l_extrapolate
  !
  character(len=256)    :: ftable_u
  character(len=256)    :: ftable_t
  character(len=256)    :: filein
  character(len=256)    :: fileout
  !
  integer(4), parameter :: mtin     = 81
  integer(4), parameter :: mtout    = 83
  integer(4) :: ios       !  入出力エラーチェック用
  !
  integer(4) :: i, j, ii, jj
  real(8) ::  r_b, r_t, r_l, r_r
  real(8) ::  c_bl, c_br, c_tl, c_tr
  real(8) ::  s_bl, s_br, s_tl, s_tr
  real(8) ::  u1, u2, u3, u4
  real(8) ::  v1, v2, v3, v4

  !==============================================
  !
  call read_namelist
  !
  !----------------------------------------------
  !
#ifdef OGCM_VARIABLE
  call set_hgrids(file_vgrid)
#else /* OGCM_VARIABLE */
  call set_hgrids
#endif /* OGCM_VARIABLE */
  !
  !----------------------------------------------
  !
  if(l_make_table) then
    u_tbl(1,1)%ldef = .true.
    u_tbl(1,1)%i = 1
    u_tbl(1,1)%j = 1
    u_tbl(1,1)%w = 0.0d0
    u_tbl(1,1)%c = 1.0d0
    u_tbl(1,1)%s = 0.0d0
    u_tbl(:,:) = u_tbl(1,1)
    !
    t_tbl(1,1)%ldef = .true.
    t_tbl(1,1)%i = 1
    t_tbl(1,1)%j = 1
    t_tbl(1,1)%w = 0.0d0
    t_tbl(:,:) = t_tbl(1,1)
    !
    call interpolate
    !
    open(unit=mtout, file=ftable_u, form='unformatted')
    write(mtout) u_tbl(:,:)
    close(mtout)
    write(*,*) 'table for u-grids: ', trim(ftable_u), ' has been written.'
    !
    open(mtout, file=ftable_t, form='unformatted')
    write(mtout) t_tbl
    close(mtout)
    write(*,*) 'table for t-grids: ', trim(ftable_t), ' has been written.'
  else
    open(unit=mtin, file=ftable_u, form='unformatted')
    read(mtin) u_tbl
    close(mtin)
    write(*,*) 'table for u-grids: ', trim(ftable_u), ' has been read.'
    !
    open(unit=mtin, file=ftable_t, form='unformatted')
    read(mtin) t_tbl
    close(mtin)
    write(*,*) 'table for t-grids: ', trim(ftable_t), ' has been read.'
  end if
  !
  !----------------------------------------------
  if(txyu == 'u' .or. txyu == 'U') then
    !
    allocate(uin4(nxin,nyin))
    allocate(vin4(nxin,nyin))
    allocate(uin(nxin,nyin))
    allocate(vin(nxin,nyin))
    open(mtin, file=filein, form='unformatted', access='direct', recl=4*nxin*nyin)
    read(mtin, rec=nvaru, iostat=ios) uin4
    read(mtin, rec=nvarv, iostat=ios) vin4
    if(ios /= 0) write(*, *) 'reading error in file: ', trim(filein)
    close (mtin)
    where(uin4 == UNDEFI)
      uin4(:,:) = 0.0
    end where
    where(vin4 == UNDEFI)
      vin4(:,:) = 0.0
    end where
    uin(:,:) = real(uin4(:,:), 8)
    vin(:,:) = real(vin4(:,:), 8)
    !
    allocate(uout(imut,jmut))
    allocate(vout(imut,jmut))
    if(l_rot_in) then
      do j = 1, jmut
        do i = 1, imut
          if(u_tbl(i,j)%ldef) then
            u1 = u_tbl(i,j)%w(1)*uin(u_tbl(i,j)%i(1), u_tbl(i,j)%j(1))
            u2 = u_tbl(i,j)%w(2)*uin(u_tbl(i,j)%i(2), u_tbl(i,j)%j(2))
            u3 = u_tbl(i,j)%w(3)*uin(u_tbl(i,j)%i(3), u_tbl(i,j)%j(3))
            u4 = u_tbl(i,j)%w(4)*uin(u_tbl(i,j)%i(4), u_tbl(i,j)%j(4))
            v1 = u_tbl(i,j)%w(1)*vin(u_tbl(i,j)%i(1), u_tbl(i,j)%j(1))
            v2 = u_tbl(i,j)%w(2)*vin(u_tbl(i,j)%i(2), u_tbl(i,j)%j(2))
            v3 = u_tbl(i,j)%w(3)*vin(u_tbl(i,j)%i(3), u_tbl(i,j)%j(3))
            v4 = u_tbl(i,j)%w(4)*vin(u_tbl(i,j)%i(4), u_tbl(i,j)%j(4))
            uout(i,j) = u_tbl(i,j)%c(1)*u1 -u_tbl(i,j)%s(1)*v1  &
              &        +u_tbl(i,j)%c(2)*u2 -u_tbl(i,j)%s(2)*v2  &
              &        +u_tbl(i,j)%c(3)*u3 -u_tbl(i,j)%s(3)*v3  &
              &        +u_tbl(i,j)%c(4)*u4 -u_tbl(i,j)%s(4)*v4
            vout(i,j) = u_tbl(i,j)%s(1)*u1 +u_tbl(i,j)%c(1)*v1  &
              &        +u_tbl(i,j)%s(2)*u2 +u_tbl(i,j)%c(2)*v2  &
              &        +u_tbl(i,j)%s(3)*u3 +u_tbl(i,j)%c(3)*v3  &
              &        +u_tbl(i,j)%s(4)*u4 +u_tbl(i,j)%c(4)*v4
          else if(l_extrapolate) then
            uout(i,j) = 0.d0
            vout(i,j) = 0.d0
          else
            uout(i,j) = UNDEFO
            vout(i,j) = UNDEFO
          end if
        end do
      end do
    else
      do j = 1, jmut
        do i = 1, imut
          if(u_tbl(i,j)%ldef) then
            uout(i,j) = u_tbl(i,j)%w(1)*uin(u_tbl(i,j)%i(1), u_tbl(i,j)%j(1))  &
              &        +u_tbl(i,j)%w(2)*uin(u_tbl(i,j)%i(2), u_tbl(i,j)%j(2))  &
              &        +u_tbl(i,j)%w(3)*uin(u_tbl(i,j)%i(3), u_tbl(i,j)%j(3))  &
              &        +u_tbl(i,j)%w(4)*uin(u_tbl(i,j)%i(4), u_tbl(i,j)%j(4))
            vout(i,j) = u_tbl(i,j)%w(1)*vin(u_tbl(i,j)%i(1), u_tbl(i,j)%j(1))  &
              &        +u_tbl(i,j)%w(2)*vin(u_tbl(i,j)%i(2), u_tbl(i,j)%j(2))  &
              &        +u_tbl(i,j)%w(3)*vin(u_tbl(i,j)%i(3), u_tbl(i,j)%j(3))  &
              &        +u_tbl(i,j)%w(4)*vin(u_tbl(i,j)%i(4), u_tbl(i,j)%j(4))
          else if(l_extrapolate) then
            uout(i,j) = 0.d0
            vout(i,j) = 0.d0
          else
            uout(i,j) = UNDEFO
            vout(i,j) = UNDEFO
          end if
        end do
      end do
    end if
    allocate(uout4(imut,jmut))
    allocate(vout4(imut,jmut))
    uout4(:,:) = real(uout(:,:), 4)
    vout4(:,:) = real(vout(:,:), 4)
    deallocate(uout)
    deallocate(vout)
    !
    open(mtout, file=fileout, form='unformatted', access='direct', recl=4*imut*jmut)
    write(mtout, rec=1, iostat=ios) uout4
    write(mtout, rec=2, iostat=ios) vout4
    if(ios /= 0) write(*, *) 'writing error in file: ', trim(fileout)
    close (mtout)
    write(*,*) 'file : ', trim(fileout), ' has been written.'
    deallocate(uout4)
    deallocate(vout4)
  else
    allocate(din4(nxin,nyin))
    allocate(din(nxin,nyin))
    open(mtin, file=filein, form='unformatted', access='direct', recl=4*nxin*nyin)
    read(mtin, rec=nvar, iostat=ios) din4
    if(ios /= 0) write(*, *) 'reading error in file: ', trim(filein)
    close (mtin)
    where(din4 == UNDEFI)
      din4(:,:) = 0.0
    end where
    din(:,:) = real(din4(:,:), 8)
    deallocate(din4)
    !
    allocate(dout(imut,jmut))
    do j = 1, jmut
      do i = 1, imut
        if(t_tbl(i,j)%ldef .or. l_extrapolate) then
          dout(i,j) = t_tbl(i,j)%w(1)*din(t_tbl(i,j)%i(1), t_tbl(i,j)%j(1))  &
            &        +t_tbl(i,j)%w(2)*din(t_tbl(i,j)%i(2), t_tbl(i,j)%j(2))  &
            &        +t_tbl(i,j)%w(3)*din(t_tbl(i,j)%i(3), t_tbl(i,j)%j(3))  &
            &        +t_tbl(i,j)%w(4)*din(t_tbl(i,j)%i(4), t_tbl(i,j)%j(4))
        else
          dout(i,j) = UNDEFO
        end if
      end do
    end do
    allocate(dout4(imut,jmut))
    dout4(:,:) = real(dout(:,:), 4)
    deallocate(dout)
    !
    open(mtout, file=fileout, form='unformatted', access='direct', recl=4*imut*jmut)
    write(mtout, rec=1, iostat=ios) dout4
    if(ios /= 0) write(*, *) 'writing error in file: ', trim(fileout)
    close (mtout)
    write(*,*) 'file : ', trim(fileout), ' has been written.'
    deallocate(dout4)
  end if
  !
contains
!====================================================
!
subroutine read_namelist
!
!====================================================
  !
  fgrid_info = 'grid_info.grd'
#ifdef OGCM_VARIABLE
  file_vgrid = 'vgrid.d'
#endif /* OGCM_VARIABLE */
  nxin = 0
  nyin = 0
  nlat = 3
  nlon = 4
  slat = -90.0
  slon = 0.0
  dlat = 1.25
  dlon = 1.25
  fltable= 'MSM2seto.table'
  !
  fcorein  = 'MSM'
  fdatein  = 'yyyymmddhh'
  !
  fcoreout    = 'MSMO'
  !
  write(*,*) '=== reading namelist: nml_mktable'
  read(unit=5, nml_mktable)
  write(*,*) 'l_make_table : ', l_make_table
  write(*,*) 'l_read_latlon: ', l_read_latlon
  if(l_read_latlon) then
    write(*,*) 'fgrid_info   : ', trim(fgrid_info)
    write(*,*) 'nlat     : ', nlat
    write(*,*) 'nlon     : ', nlon
  else
    write(*,*) 'slat     : ', slat
    write(*,*) 'slon     : ', slon
    write(*,*) 'dlat     : ', dlat
    write(*,*) 'dlon     : ', dlon
  end if
#ifdef OGCM_VARIABLE
  write(*,*) 'file_vgrid   : ', trim(file_vgrid)
#endif /* OGCM_VARIABLE */
  write(*,*) 'nxin     : ', nxin
  write(*,*) 'nyin     : ', nyin
  write(*,*) 'fltable  : ', trim(fltable)
  !
  write(ftable_u, '(a,a)') trim(fltable), '_u.table'
  write(ftable_t, '(a,a)') trim(fltable), '_t.table'
  !
  write(*,*) 'ftable_u : ', trim(ftable_u)
  write(*,*) 'ftable_t : ', trim(ftable_t)
  !
  !
  write(*,*) '=== reading namelist: nml_iodata'
  read(unit=5, nml_iodata)
  write(*,*) 'fcorein  : ', trim(fcorein)
  write(*,*) 'fdatein  : ', trim(fdatein)
  write(*,*) 'nvaru    : ', nvaru
  write(*,*) 'nvarv    : ', nvarv
  write(*,*) 'nvar     : ', nvar
  write(*,*) 'txyu     : ', trim(txyu)
  write(*,*) 'l_rot_in : ', l_rot_in
  write(*,*) 'fcoreout : ', trim(fcoreout)
  !
  write(filein,  '(a,a,a)') trim(fcorein),  '.', trim(fdatein)
  write(fileout, '(a,a,a)') trim(fcoreout), '.', trim(fdatein)
  write(*,*) 'filein   : ', trim(filein)
  write(*,*) 'fileout  : ', trim(fileout)
  !
end subroutine read_namelist
!====================================================
!
subroutine interpolate
!
!====================================================
  !
  real(8)  :: cf   !  cos(phi)
  real(8)  :: rmin
  real(8)  :: hl1, hl2, hl3
  real(8)  :: ltl, ltr
  real(8)  :: lbl, lbr
  real(8)  :: r1, r2, r3
  integer(4)  :: i0, j0
  !
  !---------------------------------------
  !
  allocate(glati4(nxin, nyin))
  allocate(gloni4(nxin, nyin))
  allocate(glati(nxin, nyin))
  allocate(gloni(nxin, nyin))
  glati4(:,:) = 0.0
  gloni4(:,:) = 0.0
  if(l_read_latlon) then
    open(mtin, file=fgrid_info, form='unformatted', access='direct', recl=4*nxin*nyin)
    read(mtin, rec=nlat, iostat=ios) glati4
    read(mtin, rec=nlon, iostat=ios) gloni4
    if(ios /= 0) write(*, *) 'reading error in file: ', trim(fgrid_info)
    close (mtin)
    where(glati4 == UNDEFI)
      glati4(:,:) = 0.0
      gloni4(:,:) = 0.0
    end where
  else
    do j = 1, nyin
      glati4(:,j) = slat +dlat*real(j-1, 4)
    end do
    do i = 1, nxin
      gloni4(i,:) = slon +dlon*real(i-1, 4)
    end do
  end if
  glati(:,:) = real(glati4(:,:), 8)
  gloni(:,:) = real(gloni4(:,:), 8)
  deallocate(glati4)
  deallocate(gloni4)
  !
  ! U-grid
  !
  write(*,*) '==== making table for U-grid ===='
  do jj = 1, jmut
    write(*,'(a, i5, a, i5)') 'j=', jj, '/', jmut
    do ii = 1, imut
      cf = cos(alatu(jj)*radian_r)
      rmin = 1.d10
      do j = 2, nyin-1
        do i = 2, nxin-1
          hl1 = cf*(gloni(i,j) -alonu(ii))
          hl2 = glati(i,j) -alatu(jj)
          hl3 = hl1*hl1 +hl2*hl2
          if(hl3 < rmin) then
            i0 = i
            j0 = j
            rmin = hl3
          end if
        end do
      end do
      !
      hl1 = cf*(gloni(i0-1,j0-1) -alonu(ii))
      hl2 = glati(i0-1,j0-1) -alatu(jj)
      lbl = hl1*hl1 +hl2*hl2
      
      hl1 = cf*(gloni(i0+1,j0-1) -alonu(ii))
      hl2 = glati(i0+1,j0-1) -alatu(jj)
      lbr = hl1*hl1 +hl2*hl2

      hl1 = cf*(gloni(i0-1,j0+1) -alonu(ii))
      hl2 = glati(i0-1,j0+1) -alatu(jj)
      ltl = hl1*hl1 +hl2*hl2
      
      hl1 = cf*(gloni(i0+1,j0+1) -alonu(ii))
      hl2 = glati(i0+1,j0+1) -alatu(jj)
      ltr = hl1*hl1 +hl2*hl2

      if(lbl == min(lbl, lbr, ltl, ltr)) then
        u_tbl(ii,jj)%i(1) = i0-1
        u_tbl(ii,jj)%j(1) = j0-1
        u_tbl(ii,jj)%i(2) = i0
        u_tbl(ii,jj)%j(2) = j0-1
        u_tbl(ii,jj)%i(3) = i0-1
        u_tbl(ii,jj)%j(3) = j0
        u_tbl(ii,jj)%i(4) = i0
        u_tbl(ii,jj)%j(4) = j0
        if(i0 == 2 .or. j0 == 2) then
          u_tbl(ii,jj)%ldef = .false.
          u_tbl(ii,jj)%w(1) =  0.d0
          u_tbl(ii,jj)%w(2) =  0.d0
          u_tbl(ii,jj)%w(3) =  0.d0
          u_tbl(ii,jj)%w(4) =  1.d0
          hl1 = cf*(gloni(i0+1,j0) -gloni(i0-1,j0))
          hl2 = glati(i0+1,j0) -glati(i0-1,j0)
          hl3 = sqrt(hl1*hl1 +hl2*hl2)
          u_tbl(ii,jj)%c(1) =  0.d0
          u_tbl(ii,jj)%s(1) =  0.d0
          u_tbl(ii,jj)%c(2) =  0.d0
          u_tbl(ii,jj)%s(2) =  0.d0
          u_tbl(ii,jj)%c(3) =  0.d0
          u_tbl(ii,jj)%s(3) =  0.d0
          u_tbl(ii,jj)%c(4) =  hl1 / hl3
          u_tbl(ii,jj)%s(4) =  hl2 / hl3
          cycle
        end if
        !
        hl1 = cf*(gloni(i0-1,j0) -gloni(i0,j0))
        hl2 = (glati(i0-1,j0) -glati(i0,j0))
        r_r = (hl1*cf*(alonu(ii)-gloni(i0,j0))   &
          &   +hl2*(alatu(jj)-glati(i0,j0)) )    &
          & / (hl1*hl1 + hl2*hl2)
        r_l = 1.d0 -r_r
        !
        hl1 = cf*(gloni(i0,j0-1) -gloni(i0,j0))
        hl2 = (glati(i0,j0-1) -glati(i0,j0))
        r_t = (hl1*cf*(alonu(ii)-gloni(i0,j0))   &
          &   +hl2*(alatu(jj)-glati(i0,j0)) )    &
          & / (hl1*hl1 + hl2*hl2)
        r_b = 1.d0 -r_t
        !
        hl1 = cf*(gloni(i0,j0-1) -gloni(i0-2,j0-1))
        hl2 = glati(i0,j0-1) -glati(i0-2,j0-1)
        hl3 = sqrt(hl1*hl1 +hl2*hl2)
        c_bl = hl1 / hl3
        s_bl = hl2 / hl3
        hl1 = cf*(gloni(i0+1,j0-1) -gloni(i0-1,j0-1))
        hl2 = glati(i0+1,j0-1) -glati(i0-1,j0-1)
        hl3 = sqrt(hl1*hl1 +hl2*hl2)
        c_br = hl1 / hl3
        s_br = hl2 / hl3
        hl1 = cf*(gloni(i0,j0) -gloni(i0-2,j0))
        hl2 = glati(i0,j0) -glati(i0-2,j0)
        hl3 = sqrt(hl1*hl1 +hl2*hl2)
        c_tl = hl1 / hl3
        s_tl = hl2 / hl3
        hl1 = cf*(gloni(i0+1,j0) -gloni(i0-1,j0))
        hl2 = glati(i0+1,j0) -glati(i0-1,j0)
        hl3 = sqrt(hl1*hl1 +hl2*hl2)
        c_tr = hl1 / hl3
        s_tr = hl2 / hl3
      else if(lbr == min(lbl, lbr, ltl, ltr)) then
        u_tbl(ii,jj)%i(1) = i0
        u_tbl(ii,jj)%j(1) = j0-1
        u_tbl(ii,jj)%i(2) = i0+1
        u_tbl(ii,jj)%j(2) = j0-1
        u_tbl(ii,jj)%i(3) = i0
        u_tbl(ii,jj)%j(3) = j0
        u_tbl(ii,jj)%i(4) = i0+1
        u_tbl(ii,jj)%j(4) = j0
        if(i0 == nxin-1 .or. j0 == 2) then
          u_tbl(ii,jj)%ldef = .false.
          u_tbl(ii,jj)%w(1) =  0.d0
          u_tbl(ii,jj)%w(2) =  0.d0
          u_tbl(ii,jj)%w(3) =  1.d0
          u_tbl(ii,jj)%w(4) =  0.d0
          hl1 = cf*(gloni(i0+1,j0) -gloni(i0-1,j0))
          hl2 = glati(i0+1,j0) -glati(i0-1,j0)
          hl3 = sqrt(hl1*hl1 +hl2*hl2)
          u_tbl(ii,jj)%c(1) =  0.d0
          u_tbl(ii,jj)%s(1) =  0.d0
          u_tbl(ii,jj)%c(2) =  0.d0
          u_tbl(ii,jj)%s(2) =  0.d0
          u_tbl(ii,jj)%c(3) =  hl1 / hl3
          u_tbl(ii,jj)%s(3) =  hl2 / hl3
          u_tbl(ii,jj)%c(4) =  0.d0
          u_tbl(ii,jj)%s(4) =  0.d0
          cycle
        end if
        !
        hl1 = cf*(gloni(i0+1,j0) -gloni(i0,j0))
        hl2 = (glati(i0+1,j0) -glati(i0,j0))
        r_l = (hl1*cf*(alonu(ii)-gloni(i0,j0))   &
          &   +hl2*(alatu(jj)-glati(i0,j0)) )     &
          & / (hl1*hl1 + hl2*hl2)
        r_r = 1.d0 -r_l
        !
        hl1 = cf*(gloni(i0,j0-1) -gloni(i0,j0))
        hl2 = (glati(i0,j0-1) -glati(i0,j0))
        r_t = (hl1*cf*(alonu(ii)-gloni(i0,j0))   &
          &   +hl2*(alatu(jj)-glati(i0,j0)) )     &
          & / (hl1*hl1 + hl2*hl2)
        r_b = 1.d0 -r_t
        !
        hl1 = cf*(gloni(i0+1,j0-1) -gloni(i0-1,j0-1))
        hl2 = glati(i0+1,j0-1) -glati(i0-1,j0-1)
        hl3 = sqrt(hl1*hl1 +hl2*hl2)
        c_bl = hl1 / hl3
        s_bl = hl2 / hl3
        hl1 = cf*(gloni(i0+2,j0-1) -gloni(i0,j0-1))
        hl2 = glati(i0+2,j0-1) -glati(i0,j0-1)
        hl3 = sqrt(hl1*hl1 +hl2*hl2)
        c_br = hl1 / hl3
        s_br = hl2 / hl3
        hl1 = cf*(gloni(i0+1,j0) -gloni(i0-1,j0))
        hl2 = glati(i0+1,j0) -glati(i0-1,j0)
        hl3 = sqrt(hl1*hl1 +hl2*hl2)
        c_tl = hl1 / hl3
        s_tl = hl2 / hl3
        hl1 = cf*(gloni(i0+2,j0) -gloni(i0,j0))
        hl2 = glati(i0+2,j0) -glati(i0,j0)
        hl3 = sqrt(hl1*hl1 +hl2*hl2)
        c_tr = hl1 / hl3
        s_tr = hl2 / hl3
      else if(ltl == min(lbl, lbr, ltl, ltr)) then
        u_tbl(ii,jj)%i(1) = i0-1
        u_tbl(ii,jj)%j(1) = j0
        u_tbl(ii,jj)%i(2) = i0
        u_tbl(ii,jj)%j(2) = j0
        u_tbl(ii,jj)%i(3) = i0-1
        u_tbl(ii,jj)%j(3) = j0+1
        u_tbl(ii,jj)%i(4) = i0
        u_tbl(ii,jj)%j(4) = j0+1
        if(i0 == 2 .or. j0 == nyin-1) then
          u_tbl(ii,jj)%ldef = .false.
          u_tbl(ii,jj)%w(1) =  0.d0
          u_tbl(ii,jj)%w(2) =  1.d0
          u_tbl(ii,jj)%w(3) =  0.d0
          u_tbl(ii,jj)%w(4) =  0.d0
          hl1 = cf*(gloni(i0+1,j0) -gloni(i0-1,j0))
          hl2 = glati(i0+1,j0) -glati(i0-1,j0)
          hl3 = sqrt(hl1*hl1 +hl2*hl2)
          u_tbl(ii,jj)%c(1) =  0.d0
          u_tbl(ii,jj)%s(1) =  0.d0
          u_tbl(ii,jj)%c(2) =  hl1 / hl3
          u_tbl(ii,jj)%s(2) =  hl2 / hl3
          u_tbl(ii,jj)%c(3) =  0.d0
          u_tbl(ii,jj)%s(3) =  0.d0
          u_tbl(ii,jj)%c(4) =  0.d0
          u_tbl(ii,jj)%s(4) =  0.d0
          cycle
        end if
        !
        hl1 = cf*(gloni(i0-1,j0) -gloni(i0,j0))
        hl2 = (glati(i0-1,j0) -glati(i0,j0))
        r_r = (hl1*cf*(alonu(ii)-gloni(i0,j0))   &
          &   +hl2*(alatu(jj)-glati(i0,j0)) )     &
          & / (hl1*hl1 + hl2*hl2)
        r_l = 1.d0 -r_r
        !
        hl1 = cf*(gloni(i0,j0+1) -gloni(i0,j0))
        hl2 = (glati(i0,j0+1) -glati(i0,j0))
        r_b = (hl1*cf*(alonu(ii)-gloni(i0,j0))   &
          &   +hl2*(alatu(jj)-glati(i0,j0)) )     &
          & / (hl1*hl1 + hl2*hl2)
        r_t = 1.d0 -r_b
        !
        hl1 = cf*(gloni(i0,j0) -gloni(i0-2,j0))
        hl2 = glati(i0,j0) -glati(i0-2,j0)
        hl3 = sqrt(hl1*hl1 +hl2*hl2)
        c_bl = hl1 / hl3
        s_bl = hl2 / hl3
        hl1 = cf*(gloni(i0+1,j0) -gloni(i0-1,j0))
        hl2 = glati(i0+1,j0) -glati(i0-1,j0)
        hl3 = sqrt(hl1*hl1 +hl2*hl2)
        c_br = hl1 / hl3
        s_br = hl2 / hl3
        hl1 = cf*(gloni(i0,j0+1) -gloni(i0-2,j0+1))
        hl2 = glati(i0,j0+1) -glati(i0-2,j0+1)
        hl3 = sqrt(hl1*hl1 +hl2*hl2)
        c_tl = hl1 / hl3
        s_tl = hl2 / hl3
        hl1 = cf*(gloni(i0+1,j0+1) -gloni(i0-1,j0+1))
        hl2 = glati(i0+1,j0+1) -glati(i0-1,j0+1)
        hl3 = sqrt(hl1*hl1 +hl2*hl2)
        c_tr = hl1 / hl3
        s_tr = hl2 / hl3
      else if(ltr == min(lbl, lbr, ltl, ltr)) then
        u_tbl(ii,jj)%i(1) = i0
        u_tbl(ii,jj)%j(1) = j0
        u_tbl(ii,jj)%i(2) = i0+1
        u_tbl(ii,jj)%j(2) = j0
        u_tbl(ii,jj)%i(3) = i0
        u_tbl(ii,jj)%j(3) = j0+1
        u_tbl(ii,jj)%i(4) = i0+1
        u_tbl(ii,jj)%j(4) = j0+1
        if(i0 == nxin-1 .or. j0 == nyin-1) then
          u_tbl(ii,jj)%ldef = .false.
          u_tbl(ii,jj)%w(1) =  1.d0
          u_tbl(ii,jj)%w(2) =  0.d0
          u_tbl(ii,jj)%w(3) =  0.d0
          u_tbl(ii,jj)%w(4) =  0.d0
          hl1 = cf*(gloni(i0+1,j0) -gloni(i0-1,j0))
          hl2 = glati(i0+1,j0) -glati(i0-1,j0)
          hl3 = sqrt(hl1*hl1 +hl2*hl2)
          u_tbl(ii,jj)%c(1) =  hl1 / hl3
          u_tbl(ii,jj)%s(1) =  hl2 / hl3
          u_tbl(ii,jj)%c(2) =  0.d0
          u_tbl(ii,jj)%s(2) =  0.d0
          u_tbl(ii,jj)%c(3) =  0.d0
          u_tbl(ii,jj)%s(3) =  0.d0
          u_tbl(ii,jj)%c(4) =  0.d0
          u_tbl(ii,jj)%s(4) =  0.d0
          cycle
        end if
        !
        hl1 = cf*(gloni(i0+1,j0) -gloni(i0,j0))
        hl2 = (glati(i0+1,j0) -glati(i0,j0))
        r_l = (hl1*cf*(alonu(ii)-gloni(i0,j0))   &
          &   +hl2*(alatu(jj)-glati(i0,j0)) )     &
          & / (hl1*hl1 + hl2*hl2)
        r_r = 1.d0 -r_l
        !
        hl1 = cf*(gloni(i0,j0+1) -gloni(i0,j0))
        hl2 = (glati(i0,j0+1) -glati(i0,j0))
        r_b = (hl1*cf*(alonu(ii)-gloni(i0,j0))   &
          &   +hl2*(alatu(jj)-glati(i0,j0)) )     &
          & / (hl1*hl1 + hl2*hl2)
        r_t = 1.d0 -r_b
        !
        hl1 = cf*(gloni(i0+1,j0) -gloni(i0-1,j0))
        hl2 = glati(i0+1,j0) -glati(i0-1,j0)
        hl3 = sqrt(hl1*hl1 +hl2*hl2)
        c_bl = hl1 / hl3
        s_bl = hl2 / hl3
        hl1 = cf*(gloni(i0+2,j0) -gloni(i0,j0))
        hl2 = glati(i0+2,j0) -glati(i0,j0)
        hl3 = sqrt(hl1*hl1 +hl2*hl2)
        c_br = hl1 / hl3
        s_br = hl2 / hl3
        hl1 = cf*(gloni(i0+1,j0+1) -gloni(i0-1,j0+1))
        hl2 = glati(i0+1,j0+1) -glati(i0-1,j0+1)
        hl3 = sqrt(hl1*hl1 +hl2*hl2)
        c_tl = hl1 / hl3
        s_tl = hl2 / hl3
        hl1 = cf*(gloni(i0+2,j0+1) -gloni(i0,j0+1))
        hl2 = glati(i0+2,j0+1) -glati(i0,j0+1)
        hl3 = sqrt(hl1*hl1 +hl2*hl2)
        c_tr = hl1 / hl3
        s_tr = hl2 / hl3
      else
        write(*,*) '????? in interpolate'
        stop
      end if
      !
      if(u_tbl(ii,jj)%ldef) then
        u_tbl(ii,jj)%w(1) = r_r*r_t
        u_tbl(ii,jj)%w(2) = r_l*r_t
        u_tbl(ii,jj)%w(3) = r_r*r_b
        u_tbl(ii,jj)%w(4) = r_l*r_b
        !
        u_tbl(ii,jj)%c(1) = c_bl
        u_tbl(ii,jj)%s(1) = s_bl
        u_tbl(ii,jj)%c(2) = c_br
        u_tbl(ii,jj)%s(2) = s_br
        u_tbl(ii,jj)%c(3) = c_tl
        u_tbl(ii,jj)%s(3) = s_tl
        u_tbl(ii,jj)%c(4) = c_tr
        u_tbl(ii,jj)%s(4) = s_tr
      end if
!!!!!!write(*,*) ii, jj, r_r*r_t +r_l*r_t +r_r*r_b +r_l*r_b
!!!!!!write(*,*) ii, jj, c_bl*c_bl +s_bl*s_bl
!!!!!!write(*,*) ii, jj, c_br*c_br +s_br*s_br
!!!!!!write(*,*) ii, jj, c_tl*c_tl +s_bl*s_tl
!!!!!!write(*,*) ii, jj, c_tr*c_tr +s_br*s_tr
    end do
  end do
  !
  ! T-grid
  !
  write(*,*) '=== making table for T-grid'
  do jj = 1, jmut
    write(*,'(a, i5, a, i5)') 'j=', jj, '/', jmut
    do ii = 1, imut
      cf = cos(alatt(jj)*radian_r)
      rmin = 1.d10
      do j = 2, nyin-1
        do i = 2, nxin-1
          hl1 = cf*(gloni(i,j) -alont(ii))
          hl2 = glati(i,j) -alatt(jj)
          hl3 = hl1*hl1 +hl2*hl2
          if(hl3 < rmin) then
            i0 = i
            j0 = j
            rmin = hl3
          end if
        end do
      end do
      !
      hl1 = cf*(gloni(i0-1,j0-1) -alont(ii))
      hl2 = glati(i0-1,j0-1) -alatt(jj)
      lbl = hl1*hl1 +hl2*hl2

      hl1 = cf*(gloni(i0+1,j0-1) -alont(ii))
      hl2 = glati(i0+1,j0-1) -alatt(jj)
      lbr = hl1*hl1 +hl2*hl2

      hl1 = cf*(gloni(i0-1,j0+1) -alont(ii))
      hl2 = glati(i0-1,j0+1) -alatt(jj)
      ltl = hl1*hl1 +hl2*hl2

      hl1 = cf*(gloni(i0+1,j0+1) -alont(ii))
      hl2 = glati(i0+1,j0+1) -alatt(jj)
      ltr = hl1*hl1 +hl2*hl2

      if(lbl == min(lbl, lbr, ltl, ltr)) then
        t_tbl(ii,jj)%i(1) = i0-1
        t_tbl(ii,jj)%j(1) = j0-1
        t_tbl(ii,jj)%i(2) = i0
        t_tbl(ii,jj)%j(2) = j0-1
        t_tbl(ii,jj)%i(3) = i0-1
        t_tbl(ii,jj)%j(3) = j0
        t_tbl(ii,jj)%i(4) = i0
        t_tbl(ii,jj)%j(4) = j0
        if(i0 == 2 .or. j0 == 2) then
          t_tbl(ii,jj)%ldef = .false.
          t_tbl(ii,jj)%w(1) = 0.d0
          t_tbl(ii,jj)%w(2) = 0.d0
          t_tbl(ii,jj)%w(3) = 0.d0
          t_tbl(ii,jj)%w(4) = 1.d0
          cycle
        end if
        !
        hl1 = cf*(gloni(i0-1,j0) -gloni(i0,j0))
        hl2 = (glati(i0-1,j0) -glati(i0,j0))
        r_r = (hl1*cf*(alont(ii)-gloni(i0,j0))   &
          &   +hl2*(alatt(jj)-glati(i0,j0)) )    &
          & / (hl1*hl1 + hl2*hl2)
        r_l = 1.d0 -r_r
        !
        hl1 = cf*(gloni(i0,j0-1) -gloni(i0,j0))
        hl2 = (glati(i0,j0-1) -glati(i0,j0))
        r_t = (hl1*cf*(alonu(ii)-gloni(i0,j0))   &
          &   +hl2*(alatu(jj)-glati(i0,j0)) )    &
          & / (hl1*hl1 + hl2*hl2)
        r_b = 1.d0 -r_t
      else if(lbr == min(lbl, lbr, ltl, ltr)) then
        t_tbl(ii,jj)%i(1) = i0
        t_tbl(ii,jj)%j(1) = j0-1
        t_tbl(ii,jj)%i(2) = i0+1
        t_tbl(ii,jj)%j(2) = j0-1
        t_tbl(ii,jj)%i(3) = i0
        t_tbl(ii,jj)%j(3) = j0
        t_tbl(ii,jj)%i(4) = i0+1
        t_tbl(ii,jj)%j(4) = j0
        if(i0 == nxin-1 .or. j0 == 2) then
          t_tbl(ii,jj)%ldef = .false.
          t_tbl(ii,jj)%w(1) = 0.d0
          t_tbl(ii,jj)%w(2) = 0.d0
          t_tbl(ii,jj)%w(3) = 1.d0
          t_tbl(ii,jj)%w(4) = 0.d0
          cycle
        end if
        !
        hl1 = cf*(gloni(i0+1,j0) -gloni(i0,j0))
        hl2 = (glati(i0+1,j0) -glati(i0,j0))
        r_l = (hl1*cf*(alont(ii)-gloni(i0,j0))   &
          &   +hl2*(alatt(jj)-glati(i0,j0)) )    &
          & / (hl1*hl1 + hl2*hl2)
        r_r = 1.d0 -r_l
        !
        hl1 = cf*(gloni(i0,j0-1) -gloni(i0,j0))
        hl2 = (glati(i0,j0-1) -glati(i0,j0))
        r_t = (hl1*cf*(alonu(ii)-gloni(i0,j0))   &
          &   +hl2*(alatu(jj)-glati(i0,j0)) )    &
          & / (hl1*hl1 + hl2*hl2)
        r_b = 1.d0 -r_t
      else if(ltl == min(lbl, lbr, ltl, ltr)) then
        t_tbl(ii,jj)%i(1) = i0-1
        t_tbl(ii,jj)%j(1) = j0
        t_tbl(ii,jj)%i(2) = i0
        t_tbl(ii,jj)%j(2) = j0
        t_tbl(ii,jj)%i(3) = i0-1
        t_tbl(ii,jj)%j(3) = j0+1
        t_tbl(ii,jj)%i(4) = i0
        t_tbl(ii,jj)%j(4) = j0+1
        if(i0 == 2 .or. j0 == nyin-1) then
          t_tbl(ii,jj)%ldef = .false.
          t_tbl(ii,jj)%w(1) = 0.d0
          t_tbl(ii,jj)%w(2) = 1.d0
          t_tbl(ii,jj)%w(3) = 0.d0
          t_tbl(ii,jj)%w(4) = 0.d0
          cycle
        end if
        !
        hl1 = cf*(gloni(i0-1,j0) -gloni(i0,j0))
        hl2 = (glati(i0-1,j0) -glati(i0,j0))
        r_r = (hl1*cf*(alont(ii)-gloni(i0,j0))   &
          &   +hl2*(alatt(jj)-glati(i0,j0)) )    &
          & / (hl1*hl1 + hl2*hl2)
        r_l = 1.d0 -r_r
        !
        hl1 = cf*(gloni(i0,j0+1) -gloni(i0,j0))
        hl2 = (glati(i0,j0+1) -glati(i0,j0))
        r_b = (hl1*cf*(alonu(ii)-gloni(i0,j0))   &
          &   +hl2*(alatu(jj)-glati(i0,j0)) )    &
          & / (hl1*hl1 + hl2*hl2)
        r_t = 1.d0 -r_b
      else if(ltr == min(lbl, lbr, ltl, ltr)) then
        t_tbl(ii,jj)%i(1) = i0
        t_tbl(ii,jj)%j(1) = j0
        t_tbl(ii,jj)%i(2) = i0+1
        t_tbl(ii,jj)%j(2) = j0
        t_tbl(ii,jj)%i(3) = i0
        t_tbl(ii,jj)%j(3) = j0+1
        t_tbl(ii,jj)%i(4) = i0+1
        t_tbl(ii,jj)%j(4) = j0+1
        if(i0 == nxin-1 .or. j0 == nyin-1) then
          t_tbl(ii,jj)%ldef = .false.
          t_tbl(ii,jj)%w(1) = 1.d0
          t_tbl(ii,jj)%w(2) = 0.d0
          t_tbl(ii,jj)%w(3) = 0.d0
          t_tbl(ii,jj)%w(4) = 0.d0
          cycle
        end if
        !
        hl1 = cf*(gloni(i0+1,j0) -gloni(i0,j0))
        hl2 = (glati(i0+1,j0) -glati(i0,j0))
        r_l = (hl1*cf*(alont(ii)-gloni(i0,j0))   &
          &   +hl2*(alatt(jj)-glati(i0,j0)) )    &
          & / (hl1*hl1 + hl2*hl2)
        r_r = 1.d0 -r_l
        !
        hl1 = cf*(gloni(i0,j0+1) -gloni(i0,j0))
        hl2 = (glati(i0,j0+1) -glati(i0,j0))
        r_b = (hl1*cf*(alonu(ii)-gloni(i0,j0))   &
          &   +hl2*(alatu(jj)-glati(i0,j0)) )    &
          & / (hl1*hl1 + hl2*hl2)
        r_t = 1.d0 -r_b
      else
        write(*,*) '????? in interpolate'
        stop
      end if
      t_tbl(ii,jj)%w(1) = r_r*r_t
      t_tbl(ii,jj)%w(2) = r_l*r_t
      t_tbl(ii,jj)%w(3) = r_r*r_b
      t_tbl(ii,jj)%w(4) = r_l*r_b
!!!!!!write(*,*) ii, jj, r_r*r_t +r_l*r_t +r_r*r_b +r_l*r_b
    end do
  end do
  !
  deallocate(glati)
  deallocate(gloni)
  !
end subroutine interpolate
!====================================================
end program mktable
