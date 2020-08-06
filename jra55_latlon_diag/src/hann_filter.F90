! -*-F90-*-
!
!------------------------ hann_filter.F90 --------------------------
!
!  Information:
!      Apply hann filter to JRA55-do product.
!
!-------------------------------------------------------------------
program hann_filter

  use libmxe_para, only: libmxe_para__register, clen, rho, omega &
                     & , pi, radius, radian, radian_r, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io
  use libmxe_grads, only: libmxe_grads__make, type_grads

  use file_open_close_manager

  implicit none

  integer(4) :: imf = 1440, jmf = 720
  real(4),allocatable :: dat4_org(:,:)

  ! original data

  real(8),allocatable :: datu_org(:,:)
  real(8),allocatable :: mask_org(:,:)

  real(8),allocatable :: latu_org(:,:)
  real(8),allocatable :: lonu_org(:,:)

  ! smoothed data

  real(8),allocatable :: datu_hps(:,:)
  real(8),allocatable :: datu_smt(:,:)

  integer(4), parameter :: mtin1 = 61
  integer(4), parameter :: mtot1 = 70
  integer(4) :: mtnam

  character(len=256) :: fl_in
  character(len=256) :: fl_ot
  character(len=256) :: file_namelist_hann

  integer(4) :: i, j, n, ii, jj

  real(4) :: undef_out, undef_in
  real(8) :: hl1, hl2, hl3, hl4

  ! MXE structure variables

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo
  type(type_libmxe_io) :: io

  integer(4) :: i_search, j_search
  real(8) :: weight_sum, weight_mas
  real(8) :: deg_dist, rad_dist, h_deg_width

  !-----------------------------------------------------------------------

  namelist /nml_hann/ fl_in, fl_ot, &
       & i_search, j_search, h_deg_width, &
       & undef_in, undef_out

  !-----------------------------------------------------------------------

  file_namelist_hann='namelist.hann'
  call open_file_plain(mtnam,file_namelist_hann)
  read(mtnam,nml=nml_hann)
  close(mtnam)

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)

  imf = para%imut
  jmf = para%jmut

  allocate(dat4_org(1:imf,1:jmf))
  allocate(datu_org(-i_search+1:imf+i_search,1:jmf))
  allocate(mask_org(-i_search+1:imf+i_search,1:jmf))

  allocate(latu_org(-i_search+1:imf+i_search,1:jmf))
  allocate(lonu_org(-i_search+1:imf+i_search,1:jmf))

  dat4_org(:,:) = 0.0e0
  datu_org(:,:) = 0.0d0
  mask_org(:,:) = 0.0d0

  allocate(datu_hps(-i_search+1:imf+i_search,1:jmf))
  allocate(datu_smt(-i_search+1:imf+i_search,1:jmf))

  datu_hps(:,:) = 0.0d0
  datu_smt(:,:) = 0.0d0

  !--------------------------------------------------------------------

  write(6,*) 'DATA read from ',trim(fl_in)
  open(mtin1,file=trim(fl_in),form='unformatted',access='direct',recl=4*imf*jmf)
  read(mtin1,rec=1) dat4_org(1:imf,1:jmf)
  datu_org(1:imf,1:jmf) = real(dat4_org(1:imf,1:jmf),8)
  datu_org(-i_search+1:0,1:jmf) = datu_org(imf-i_search+1:imf,1:jmf)
  datu_org(imf+1:imf+i_search,1:jmf) = datu_org(1:i_search,1:jmf)
  close(mtin1)

  where(dat4_org(1:imf,1:jmf)/=undef_in) mask_org(1:imf,1:jmf) = 1.d0
  mask_org(-i_search+1:0,     1:jmf) = mask_org(imf-i_search+1:imf,1:jmf)
  mask_org(imf+1:imf+i_search,1:jmf) = mask_org(1:i_search,      1:jmf)

  latu_org(1:imf,1:jmf) = grid%glatu(1:imf,1:jmf)
  lonu_org(1:imf,1:jmf) = grid%glonu(1:imf,1:jmf)

  latu_org(-i_search+1:0,1:jmf) = latu_org(imf-i_search+1:imf,1:jmf)
  latu_org(imf+1:imf+i_search,1:jmf) = latu_org(1:i_search,1:jmf)

  lonu_org(-i_search+1:0,1:jmf) = lonu_org(imf-i_search+1:imf,1:jmf)
  lonu_org(imf+1:imf+i_search,1:jmf) = lonu_org(1:i_search,1:jmf)

  do j = 1, jmf
    do i = -i_search+1, 0
      if (lonu_org(i,j) > 0.d0) then
        lonu_org(i,j) = lonu_org(i,j) - 360.d0
      end if
    end do
    do i = imf+1, imf+i_search
      if (lonu_org(i,j) <= 0.d0) then
        lonu_org(i,j) = lonu_org(i,j) + 360.d0
      end if
    end do
  end do

  !------

  do j = 1, jmf
    do i = 1, imf

      if (mask_org(i,j) > 0.0d0) then

        weight_sum = 0.d0
        weight_mas = 0.d0

        do jj = j-j_search, j+j_search
          if (jj >= 1 .and. jj <= jmf) then
            do ii = i-i_search, i+i_search
              deg_dist = &
                   & sqrt((lonu_org(ii,jj)-lonu_org(i,j))**2  &
                   &    + (latu_org(ii,jj)-latu_org(i,j))**2)
              deg_dist = min(deg_dist,h_deg_width)
              rad_dist = 0.5d0 * pi * deg_dist / h_deg_width
              hl1 = mask_org(ii,jj) * cos(rad_dist)**2
              weight_sum = weight_sum + hl1
              weight_mas = weight_mas + datu_org(ii,jj) * hl1
              if ((i == 1) .and. (j == 160)) then
                write(6,*) ii, jj, deg_dist, hl1
                !write(6,*) lonu_org(ii,jj)
              end if
            end do
          end if
        end do

        if (weight_sum > 0.0d0) then
          datu_smt(i,j) = weight_mas / weight_sum
          datu_hps(i,j) = datu_org(i,j) - datu_smt(i,j)
        end if

      end if

    end do
  end do

  where(mask_org(1:imf,1:jmf)==0.d0) datu_smt(1:imf,1:jmf) = real(undef_out,4)
  where(mask_org(1:imf,1:jmf)==0.d0) datu_hps(1:imf,1:jmf) = real(undef_out,4)

  write(6,*) 'DATA written to ',trim(fl_ot)
  open(mtot1,file=trim(fl_ot),form='unformatted',access='direct',recl=4*imf*jmf)
  write(mtot1,rec=1) real(datu_hps(1:imf,1:jmf),4)
  write(mtot1,rec=2) real(datu_smt(1:imf,1:jmf),4)
  close(mtot1)

  !--------------------------------------------------------------------

end program hann_filter
