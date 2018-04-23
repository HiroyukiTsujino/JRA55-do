! -*-F90-*-
!
!======================================================================
! Information:
!     smooth temperature correction factor
!----------------------------------------------------------------------
program merge_radiation_core_srb

  use libmxe_para, only: libmxe_para__register, clen, rho &
                     & , pi, radius, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io
  use libmxe_grads, only: libmxe_grads__make, type_grads
  use libmxe_stmrgn, only: libmxe_stmrgn__var2_n  &
                     & , libmxe_stmrgn__var2_x

  implicit none

  integer(4) :: ndata

  real(4),allocatable :: work4(:,:)
  real(4) :: undef_in1, undef_in2, undef_out

  real(8),allocatable :: rad_core(:,:)
  real(8),allocatable :: rad_srb (:,:)
  real(8),allocatable :: rad_out(:,:)
  real(8),allocatable :: mask_wrk(:,:)

  integer :: i,j,ii,jj,ij,n,nd,imut,jmut, is, id, istat
  integer :: ibu, ieu, jbu, jeu, juend


  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo
  type(type_libmxe_io) :: io
  type(type_grads) :: grads

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtot1 = 81, mtot2 = 82
  character(128) :: flnin1, flnin2, flnot

  integer(4) :: irec1, irec2, irec3
  integer(4) :: iter, itermax

  logical :: l_apply_mask
  real(8) :: hl1
  real(8) :: low_trans, high_trans

  !-----------------------------------------------------------------------

  namelist /nml_merge_rad/ flnin1, flnin2, undef_in1, undef_in2, &
       & flnot, undef_out, &
       & l_apply_mask, low_trans, high_trans
  open (11,file='namelist.merge_core_srb')
  read (11,nml=nml_merge_rad)
  close(11)

  !-----------------------------------------------------------------------
  ! set grid points

  !-- model settings --

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)

  imut = para%imut
  jmut = para%jmut

  ibu = 1
  ieu = imut
  jbu = 1
  jeu = jmut

  allocate (work4(1:imut,1:jmut))
  allocate (rad_core(1:imut,1:jmut))
  allocate (rad_srb (1:imut,1:jmut))
  allocate (rad_out (1:imut,1:jmut))
  allocate (mask_wrk(1:imut,1:jmut))

  if (l_apply_mask) then
    mask_wrk(:,:) = topo%aexl(:,:,1)
  else
    mask_wrk(:,:) = 1.0d0
  end if

  !---------------------------------------------------------------
  ! read correction factor

  open(mtin1, file=flnin1, access='direct', recl=4*imut*jmut)
  write(6,'(1a,1a)') 'CORE data read from ... ', trim(flnin1)

  open(mtin2, file=flnin2, access='direct', recl=4*imut*jmut)
  write(6,'(1a,1a)') 'SRB data read from ...  ', trim(flnin2)
  !-------

  read(mtin1,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
  rad_core(1:imut,1:jmut) = real(work4(1:imut,1:jmut),8)

  read(mtin2,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
  rad_srb (1:imut,1:jmut) = real(work4(1:imut,1:jmut),8) * 1.0d3

  close(mtin2)
  close(mtin1)

  !-------

  do j = 1, jmut
    do i = 1, imut
      if (mask_wrk(i,j) == 0.0d0) then
        rad_out(i,j) = undef_out
      end if
    end do
  end do
      
  do j = 1, jmut
!    if (abs(grid%latu(j)) > high_trans) then
    if (grid%latu(j) > high_trans) then
      do i = 1, imut
        if (mask_wrk(i,j) == 1.0d0) then
          rad_out(i,j) = rad_srb(i,j)
        end if
      end do
    else
!      if ((low_trans < abs(grid%latu(j))) .and. (abs(grid%latu(j)) < high_trans)) then
      if ((low_trans < grid%latu(j)) .and. (grid%latu(j) < high_trans)) then
        hl1 = (abs(grid%latu(j)) - low_trans)/(high_trans - low_trans)
        do i = 1, imut
          if (mask_wrk(i,j) == 1.0d0) then
            rad_out(i,j) = (1.0d0 - hl1) * rad_core(i,j) + hl1 * rad_srb(i,j)
          end if
        end do
      else
        do i = 1, imut
          if (mask_wrk(i,j) == 1.0d0) then
            rad_out(i,j) = rad_core(i,j)
          end if
        end do
      end if
    end if
  end do

  !-----

  open(mtot1, file=flnot, access='direct', recl=4*imut*jmut)
  write(6,'(1a,1a)') 'data written to ... ', trim(flnot)

  work4(1:imut,1:jmut) = real(rad_out(1:imut,1:jmut),4)
  write(mtot1,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)

  close(mtot1)

end program merge_radiation_core_srb
