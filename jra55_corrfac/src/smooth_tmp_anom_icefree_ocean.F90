! -*-F90-*-
!
!======================================================================
! Information:
!     smooth temperature correction factor
!----------------------------------------------------------------------
program smooth_correction_factor

  use libmxe_para, only: libmxe_para__register, clen, rho &
                     & , pi, radius, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_stmrgn, only: libmxe_stmrgn__var2_n  &
                     & , libmxe_stmrgn__var2_x
  use file_open_close_manager

  implicit none

  character(128) :: namelist_inout
  integer(4) :: lreclen_inout

  real(4),allocatable :: work4(:,:)

  real(8),allocatable :: crrc_org(:,:)
  real(8),allocatable :: crrc_fin(:,:)
  real(8),allocatable :: crrc_nh(:,:)
  real(8),allocatable :: crrc_sh(:,:)
  real(8),allocatable :: crrc_high(:,:)

  real(8),allocatable :: mask_wrk(:,:)
  real(8),allocatable :: mask_jpn(:,:)
  real(8),allocatable :: crrc_tmp(:,:)

  real(8),allocatable :: alonf(:), alatf(:)
  real(8),allocatable :: aexlf(:,:)

  integer :: i,j,ii,jj,ij,n,nd,imut,jmut, is, id, istat
  integer :: ibu, ieu, jbu, jeu, juend

  integer :: iuvts
  integer :: ireverse

  type(type_libmxe_para) :: para, para_i1, para_i2
  type(type_libmxe_grid) :: grid, grid_i1, grid_i2
  type(type_libmxe_topo) :: topo, topo_i1, topo_i2

  !-----

  integer(4) :: mtin1, mtin2, mtin3, mtin4
  integer(4) :: mtot1, mtot2

  !-----
  ! SST (1)

  character(128) :: namelist_ice1, namelist_ice2
  character(128) :: flice_1, flice_2 
  real(4) :: undef_ice1, undef_ice2
  real(4) :: undef8_ice1, undef8_ice2

  real(8),allocatable :: ice1f(:,:)
  integer(4) :: imt1, jmt1, kmt1
  real(8),allocatable :: alond1(:), alatd1(:)
  real(8),allocatable :: aexl_ice1(:,:)
  real(8),allocatable :: ice1(:,:)
  real(4),allocatable :: work1(:,:)
  integer(4) :: lreclen_ice1

  ! SST (2)

  real(8),allocatable :: ice2f(:,:)
  integer(4) :: imt2, jmt2, kmt2
  real(8),allocatable :: alond2(:), alatd2(:)
  real(8),allocatable :: aexl_ice2(:,:)
  real(8),allocatable :: ice2(:,:)
  real(4),allocatable :: work2(:,:)
  integer(4) :: lreclen_ice2

  !-----------------------------------------

  real(4),allocatable :: data_in1(:,:)
  real(4),allocatable :: data_in2(:,:)

  character(128) :: flnin_1, flnin_2
  real(4) :: undef_in1, undef_in2
  real(4) :: undef8_in1, undef8_in2

  character(128) :: flnot

  integer(4) :: irec1, irec2, irec3
  integer(4) :: iter, iter_ocean, iter_all, iter_total

  real(8) :: fill_value, hl1, hl2
  real(8) :: low_trans_n, high_trans_n
  real(8) :: low_trans_s, high_trans_s
  logical :: l_transition_n, l_transition_s

  character(128) :: file_fill
  logical :: l_out_fill_value

  character(128) :: file_jpn_mask

  !-----------------------------------------------------------------------

  namelist /nml_smooth_tmp_icefree/ &
       & flnin_1, undef_in1,  &
       & flnin_2, undef_in2,  &
       & namelist_inout,  &
       & flice_1, undef_ice1,  &
       & flice_2, undef_ice2,  &
       & namelist_ice1, namelist_ice2, &
       & flnot, &
       & iter_ocean, iter_all, &
       & low_trans_n, high_trans_n, &
       & low_trans_s, high_trans_s, &
       & fill_value, &
       & l_out_fill_value, &
       & file_fill, &
       & file_jpn_mask

  !-----------------------------------------------------------------------

  low_trans_n = 0.0d0
  high_trans_n = 0.0d0
  low_trans_s = 0.0d0
  high_trans_s = 0.0d0

  l_out_fill_value = .false.

  open (11,file='namelist.smooth_tmp_icefree')
  read (11,nml=nml_smooth_tmp_icefree)
  close(11)

  if (low_trans_n >= high_trans_n) then
    l_transition_n = .false.
  else
    l_transition_n = .true.
  end if

  if (high_trans_s >= low_trans_s) then
    l_transition_s = .false.
  else
    l_transition_s = .true.
  end if


  !-----------------------------------------------------------------------
  ! set grid points

  !-- model settings --

  call libmxe_para__register(para,file_namelist=namelist_inout)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)

  imut = para%imut
  jmut = para%jmut
  lreclen_inout = 4 * imut * jmut

  allocate(alonf(1:imut), alatf(1:jmut))
  alonf(1:imut) = grid%lonu(1:imut)
  alatf(1:jmut) = grid%latu(1:jmut)

  ibu = 1
  ieu = imut
  jbu = 1
  jeu = jmut

  allocate (work4(1:imut,1:jmut))
  allocate (crrc_org(1:imut,1:jmut))
  allocate (crrc_fin(1:imut,1:jmut))
  allocate (crrc_nh(1:imut,1:jmut))
  allocate (crrc_sh(1:imut,1:jmut))
  allocate (crrc_high(1:imut,1:jmut))

  allocate (aexlf(0:imut+1,0:jmut+1))

  aexlf(:,:) = 0.0d0
  aexlf(1:imut,1:jmut) = topo%aexl(1:imut,1:jmut,1)
  aexlf(0,1:jmut) = aexlf(imut,1:jmut)
  aexlf(imut+1,1:jmut) = aexlf(1,1:jmut)

  allocate (ice1f(1:imut,1:jmut))
  allocate (ice2f(1:imut,1:jmut))

  allocate (data_in1(1:imut,1:jmut))
  allocate (data_in2(1:imut,1:jmut))

  allocate (mask_jpn(1:imut,1:jmut))

  allocate (mask_wrk(0:imut+1,0:jmut+1))
  allocate (crrc_tmp(0:imut+1,0:jmut+1))

  !---------------------------------------------------------------

  call libmxe_para__register(para_i1,file_namelist=namelist_ice1)
  call libmxe_grid__register(grid_i1,para_i1)
  call libmxe_topo__register(topo_i1,para_i1)
  call libmxe_topo__aexl(topo_i1,para_i1)

  imt1 = para_i1%imut
  jmt1 = para_i1%jmut
  kmt1 = para_i1%km
  lreclen_ice1 = 4*imt1*jmt1

  allocate(alond1(1:imt1), alatd1(1:jmt1))
  alond1(1:imt1) = grid_i1%lonu(1:imt1)
  alatd1(1:jmt1) = grid_i1%latu(1:jmt1)
  allocate(aexl_ice1(1:imt1,1:jmt1))
  aexl_ice1(1:imt1,1:jmt1) = topo_i1%aexl(1:imt1,1:jmt1,1)
  allocate(ice1(1:imt1,1:jmt1))
  undef8_ice1 = real(undef_ice1,8)
  allocate(work1(1:imt1,1:jmt1))

  !---------------------------------------------------------------

  call libmxe_para__register(para_i2,file_namelist=namelist_ice2)
  call libmxe_grid__register(grid_i2,para_i2)
  call libmxe_topo__register(topo_i2,para_i2)
  call libmxe_topo__aexl(topo_i2,para_i2)

  imt2 = para_i2%imut
  jmt2 = para_i2%jmut
  kmt2 = para_i2%km
  lreclen_ice2 = 4*imt2*jmt2

  allocate(alond2(1:imt2), alatd2(1:jmt2))
  alond2(1:imt2) = grid_i2%lonu(1:imt2)
  alatd2(1:jmt2) = grid_i2%latu(1:jmt2)
  allocate(aexl_ice2(1:imt2,1:jmt2))
  aexl_ice2(1:imt2,1:jmt2) = topo_i2%aexl(1:imt2,1:jmt2,1)
  allocate(ice2(1:imt2,1:jmt2))
  allocate(work2(1:imt2,1:jmt2))

  !---------------------------------------------------------------

  call open_file_direct(mtin1,flice_1,lreclen_ice1)
  read(mtin1,rec=1) work1
  call close_file(mtin1)

  ice1(1:imt1,1:jmt1) = real(work1(1:imt1,1:jmt1),8)

  do j = 1, jmt1
    do i = 1, imt1
      if (ice1(i,j) == undef8_ice1) then
        ice1(i,j) = 0.0d0
      end if
      if ((ice1(i,j) < 0.0d0) .or. (ice1(i,j) > 1.0d0)) then
        write(6,*) ' WARNING: ice data is erroneous (1) ', i, j, ice1(i,j), aexl_ice1(i,j)
        ice1(i,j) = max(ice1(i,j),0.0d0)
        ice1(i,j) = min(ice1(i,j),1.0d0)
      end if
    end do
  end do

  call hintpl(ice1f,imut,jmut,alonf,alatf,ice1,imt1,jmt1,alond1,alatd1)

  !-----

  call open_file_direct(mtin2,flice_2,lreclen_ice2)
  read(mtin2,rec=1) work2
  call close_file(mtin2)

  do j = 1, jmt2
    do i = 1, imt2
      if (ice2(i,j) == undef8_ice2) then
        ice2(i,j) = 0.0d0
      end if
      !ice2(i,j) = ice2(i,j) * aexl_ice2(i,j)
      if ((ice2(i,j) < 0.0d0) .or. (ice2(i,j) > 1.0d0)) then
        write(6,*) ' WARNING: ice data is erroneous (2)', i, j, ice2(i,j), aexl_ice2(i,j)
        ice2(i,j) = max(ice2(i,j),0.0d0)
        ice2(i,j) = min(ice2(i,j),1.0d0)
      end if
    end do
  end do

  call hintpl(ice2f,imut,jmut,alonf,alatf,ice2,imt2,jmt2,alond2,alatd2)

  !---------------------------------------------------------------

  call open_file_direct(mtin1, file_jpn_mask, lreclen_inout)
  write(6,'(1a,1a)') 'jpn mask read from ... ', trim(file_jpn_mask)
  read(mtin1,rec=1) data_in1
  call close_file(mtin1)

  mask_jpn(:,:) = real(data_in1(:,:),8)

  ! determine the region where filter is applied

  mask_wrk(:,:) = 0.0d0

  do j = 1, jmut
    do i = 1, imut
      if (aexlf(i,j) > 0.0d0) then
        if (ice1f(i,j) == 0.0d0 .and. ice2f(i,j) == 0.0d0) then
          mask_wrk(i,j) = 1.0d0
        end if
        if (mask_jpn(i,j) == 1.0d0) then
          mask_wrk(i,j) = 0.0d0
        end if
      end if
    end do
  end do

  mask_wrk(0,1:jmut) = mask_wrk(imut,1:jmut)
  mask_wrk(imut+1,1:jmut) = mask_wrk(1,1:jmut)

  !---------------------------------------------------------------
  ! read data (1)

  call open_file_direct(mtin1, flnin_1, lreclen_inout)
  write(6,'(1a,1a)') 'data (1) (raw) read from ... ', trim(flnin_1)
  read(mtin1,rec=1) data_in1
  call close_file(mtin1)

  ! read data (2)

  call open_file_direct(mtin2, flnin_2, lreclen_inout)
  write(6,'(1a,1a)') 'data (2) (ref) read from ... ', trim(flnin_2)
  read(mtin2,rec=1) data_in2
  call close_file(mtin2)

  crrc_org(:,:) = 0.0d0
  do j = 1, jmut
    do i = 1, imut
      if ((data_in1(i,j) /= undef_in1) .and. (data_in2(i,j) /= undef_in2)) then
        crrc_org(i,j) = real(data_in2(i,j) - data_in1(i,j),8)
      end if
    end do
  end do

  ! iteration

  crrc_fin(1:imut,1:jmut) = crrc_org(1:imut,1:jmut)
  do j = 1, jmut
    do i = 1, imut
      if (aexlf(i,j) > 0.0d0) then
        if (mask_jpn(i,j) == 1.0d0) then
          crrc_fin(i,j) = 0.0d0
        end if
      end if
    end do
  end do

  iter_total = iter_ocean + iter_all

  write(6,*) ' Total iteration count = ', iter_total

  LOOP_ITER: do iter = 1, iter_total

    do j = jbu, jeu
      do i = ibu, ieu
        crrc_tmp(i,j) = crrc_fin(i,j)
      end do
    end do

    crrc_tmp(0,1:jmut) = crrc_tmp(imut,1:jmut)
    crrc_tmp(imut+1,1:jmut) = crrc_tmp(1,1:jmut)

    do j = jbu, jeu
      do i = ibu, ieu
        if (iter <= iter_ocean) then
          if (mask_wrk(i,j) > 0.0d0) then
            hl1 =   12.0d0 * mask_wrk(i  ,j  ) &
                 & + 2.0d0 * mask_wrk(i+1,j  ) &
                 & + 2.0d0 * mask_wrk(i  ,j+1) &
                 & + 2.0d0 * mask_wrk(i-1,j  ) &
                 & + 2.0d0 * mask_wrk(i  ,j-1) &
                 & + 1.0d0 * mask_wrk(i+1,j+1) &
                 & + 1.0d0 * mask_wrk(i-1,j+1) &
                 & + 1.0d0 * mask_wrk(i+1,j-1) &
                 & + 1.0d0 * mask_wrk(i-1,j-1) 
            hl2 =   12.0d0 * mask_wrk(i  ,j  ) * crrc_tmp(i  ,j  ) &
                 & + 2.0d0 * mask_wrk(i+1,j  ) * crrc_tmp(i+1,j  ) &
                 & + 2.0d0 * mask_wrk(i  ,j+1) * crrc_tmp(i  ,j+1) &
                 & + 2.0d0 * mask_wrk(i-1,j  ) * crrc_tmp(i-1,j  ) &
                 & + 2.0d0 * mask_wrk(i  ,j-1) * crrc_tmp(i  ,j-1) &
                 & + 1.0d0 * mask_wrk(i+1,j+1) * crrc_tmp(i+1,j+1) &
                 & + 1.0d0 * mask_wrk(i-1,j+1) * crrc_tmp(i-1,j+1) &
                 & + 1.0d0 * mask_wrk(i+1,j-1) * crrc_tmp(i+1,j-1) &
                 & + 1.0d0 * mask_wrk(i-1,j-1) * crrc_tmp(i-1,j-1)
            if (hl1 > 0.0d0) then
              crrc_fin(i,j) = hl2 / hl1
            end if
          end if
        else
          if (aexlf(i,j) > 0.0d0) then
            hl1 =   12.0d0 * aexlf(i  ,j  ) &
                 & + 2.0d0 * aexlf(i+1,j  ) &
                 & + 2.0d0 * aexlf(i  ,j+1) &
                 & + 2.0d0 * aexlf(i-1,j  ) &
                 & + 2.0d0 * aexlf(i  ,j-1) &
                 & + 1.0d0 * aexlf(i+1,j+1) &
                 & + 1.0d0 * aexlf(i-1,j+1) &
                 & + 1.0d0 * aexlf(i+1,j-1) &
                 & + 1.0d0 * aexlf(i-1,j-1) 
            hl2 =   12.0d0 * aexlf(i  ,j  ) * crrc_tmp(i  ,j  ) &
                 & + 2.0d0 * aexlf(i+1,j  ) * crrc_tmp(i+1,j  ) &
                 & + 2.0d0 * aexlf(i  ,j+1) * crrc_tmp(i  ,j+1) &
                 & + 2.0d0 * aexlf(i-1,j  ) * crrc_tmp(i-1,j  ) &
                 & + 2.0d0 * aexlf(i  ,j-1) * crrc_tmp(i  ,j-1) &
                 & + 1.0d0 * aexlf(i+1,j+1) * crrc_tmp(i+1,j+1) &
                 & + 1.0d0 * aexlf(i-1,j+1) * crrc_tmp(i-1,j+1) &
                 & + 1.0d0 * aexlf(i+1,j-1) * crrc_tmp(i+1,j-1) &
                 & + 1.0d0 * aexlf(i-1,j-1) * crrc_tmp(i-1,j-1)
            if (hl1 > 0.0d0) then
              crrc_fin(i,j) = hl2 / hl1
            end if
          end if
        end if
      end do
    end do
    
  end do LOOP_ITER
  
  if (l_transition_n) then
    do j = 1, jmut
      if (grid%latu(j) <= low_trans_n) then
        do i = 1, imut
          crrc_nh(i,j) = fill_value
        end do
      end if
      if ((low_trans_n < grid%latu(j)) .and. (grid%latu(j) < high_trans_n)) then
        hl1 = (grid%latu(j) - low_trans_n)/(high_trans_n - low_trans_n)
        do i = 1, imut
          crrc_nh(i,j) = hl1 * crrc_fin(i,j) + (1.0d0 - hl1) * fill_value
        end do
      end if
      if (grid%latu(j) >= high_trans_n) then
        do i = 1, imut
          crrc_nh(i,j) = crrc_fin(i,j)
        end do
      end if
    end do
  else
    crrc_nh(:,:) = crrc_fin(:,:)
  end if


  if (l_transition_s) then
    do j = 1, jmut
      if (grid%latu(j) >= low_trans_s) then
        do i = 1, imut
          crrc_sh(i,j) = fill_value
        end do
      end if
      if ((high_trans_s < grid%latu(j)) .and. (grid%latu(j) < low_trans_s)) then
        hl1 = (grid%latu(j) - high_trans_s)/(low_trans_s - high_trans_s)
        do i = 1, imut
          crrc_sh(i,j) = (1.0d0 - hl1) * crrc_fin(i,j) + hl1 * fill_value
        end do
      end if
      if (grid%latu(j) <= high_trans_s) then
        do i = 1, imut
          crrc_sh(i,j) = crrc_fin(i,j)
        end do
      end if
    end do
  else
    crrc_sh(:,:) = crrc_fin(:,:)
  end if

  do j = 1, jmut
    if (grid%latu(j) >= 0.0d0) then
      do i = 1, imut
        crrc_high(i,j) = crrc_nh(i,j)
      end do
    else
      do i = 1, imut
        crrc_high(i,j) = crrc_sh(i,j)
      end do
    end if
  end do



  do j = 1, jmut
    do i = 1, imut
      if (aexlf(i,j) == 0.0d0) then
        crrc_high(i,j) = fill_value
        crrc_fin(i,j) = fill_value
      end if
    end do
  end do
      

  call open_file_direct(mtot1, flnot, lreclen_inout)
  write(6,'(1a,1a)') 'data written to ... ', trim(flnot)
  work4(1:imut,1:jmut) = real(crrc_high(1:imut,1:jmut),4)
  write(mtot1,rec=1) work4
  work4(1:imut,1:jmut) = real(crrc_fin(1:imut,1:jmut),4)
  write(mtot1,rec=2) work4
  work4(1:imut,1:jmut) = real(crrc_org(1:imut,1:jmut),4)
  write(mtot1,rec=3) work4
  call close_file(mtot1)

  if (l_out_fill_value) then
    crrc_high(:,:) = fill_value
    call open_file_direct(mtot2, file_fill, lreclen_inout)
    write(6,'(1a,1a)') 'fill value data written to ... ', trim(file_fill)
    work4(1:imut,1:jmut) = real(crrc_high(1:imut,1:jmut),4)
    write(mtot2,rec=1) work4
    work4(1:imut,1:jmut) = real(crrc_fin(1:imut,1:jmut),4)
    write(mtot2,rec=2) work4
    work4(1:imut,1:jmut) = real(crrc_org(1:imut,1:jmut),4)
    write(mtot2,rec=3) work4
    call close_file(mtot2)
  end if

end program smooth_correction_factor
