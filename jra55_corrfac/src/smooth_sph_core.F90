! -*-F90-*-
!
!======================================================================
! Information:
!     smooth specific humidity correction factor
!----------------------------------------------------------------------
program smooth_correction_factor

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
  real(4) :: undef_in

  real(8),allocatable :: crrc_org(:,:)
  real(8),allocatable :: crrc_fin(:,:)

  real(8),allocatable :: mask_wrk(:,:)
  real(8),allocatable :: crrc_wrk(:,:)
  real(8),allocatable :: crrc_tmp(:,:)

  integer :: i,j,ii,jj,ij,n,nd,imut,jmut, is, id, istat
  integer :: ibu, ieu, jbu, jeu, juend

  integer :: iuvts
  integer :: ireverse

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo
  type(type_libmxe_io) :: io
  type(type_grads) :: grads

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtot1 = 81, mtot2 = 82
  character(128) :: flnin, flnot

  integer(4) :: irec1, irec2, irec3
  integer(4) :: iter, itermax

  logical :: l_apply_mask
  real(8) :: fill_value, hl1
  real(8) :: low_trans, high_trans

  !-----------------------------------------------------------------------

  namelist /nml_smooth_sphcore/ flnin, undef_in, flnot, ndata, itermax, &
       & l_apply_mask, fill_value
  open (11,file='namelist.smooth_sphcore')
  read (11,nml=nml_smooth_sphcore)
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
  allocate (crrc_org(1:imut,1:jmut))
  allocate (crrc_fin(1:imut,1:jmut))

  allocate (mask_wrk(0:imut+1,0:jmut+1))
  allocate (crrc_wrk(0:imut+1,0:jmut+1))
  allocate (crrc_tmp(0:imut+1,0:jmut+1))

  mask_wrk(:,:) = 0.0d0

  if (l_apply_mask) then
    mask_wrk(1:imut,1:jmut) = topo%aexl(1:imut,1:jmut,1)
    mask_wrk(0,1:jmut) = mask_wrk(imut,1:jmut)
    mask_wrk(imut+1,1:jmut) = mask_wrk(1,1:jmut)
  else
    mask_wrk(0:imut+1,1:jmut) = 1.0d0
  end if

  !---------------------------------------------------------------
  ! read correction factor

  open(mtin1, file=flnin, access='direct', recl=4*imut*jmut)
  write(*,'(1a,1a)') 'data read from ... ', trim(flnin)

  open(mtot1, file=flnot, access='direct', recl=4*imut*jmut)
  write(*,'(1a,1a)') 'data written to ... ', trim(flnot)

  do nd = 1, ndata

    write(6,*) ' Processing ', nd , ' th data'

    read(mtin1,rec=nd) ((work4(i,j),i=1,imut),j=1,jmut)

    crrc_org(1:imut,1:jmut) = real(work4(1:imut,1:jmut),8)

    crrc_fin(1:imut,1:jmut) = crrc_org(1:imut,1:jmut)

    LOOP_ITER: do iter = 1, itermax

      crrc_tmp(:,:) = fill_value

      do j = jbu, jeu
        do i = ibu, ieu
          if (crrc_fin(i,j) > real(undef_in,8)) then
            crrc_tmp(i,j) = crrc_fin(i,j)
          end if
        end do
      end do

      ! cyclic 

      crrc_tmp(0,1:jmut) = crrc_tmp(imut,1:jmut)
      crrc_tmp(imut+1,1:jmut) = crrc_tmp(1,1:jmut)

      do j = jbu, jeu
        do i = ibu, ieu
          hl1 = 4.0d0 * mask_wrk(i,j) &
               & + mask_wrk(i+1,j) + mask_wrk(i,j+1) &
               & + mask_wrk(i-1,j) + mask_wrk(i,j-1) 
          crrc_wrk(i,j) = mask_wrk(i,j) * ( &
               &   4.0d0 * mask_wrk(i,j) * crrc_tmp(i,j) &
               & + mask_wrk(i+1,j) * crrc_tmp(i+1,j)  &
               & + mask_wrk(i,j+1) * crrc_tmp(i,j+1) &
               & + mask_wrk(i-1,j) * crrc_tmp(i-1,j) &
               & + mask_wrk(i,j-1) * crrc_tmp(i,j-1)) &
               & / hl1
        end do
      end do

      crrc_fin(1:imut,1:jmut) = crrc_wrk(1:imut,1:jmut)

    end do LOOP_ITER

    do j = 1, jmut
      do i = 1, imut
        if (mask_wrk(i,j) == 0.0d0) then
          crrc_fin(i,j) = fill_value
        end if
      end do
    end do
      
    work4(1:imut,1:jmut) = real(crrc_fin(1:imut,1:jmut),4)
    write(mtot1,rec=nd) ((work4(i,j),i=1,imut),j=1,jmut)

  end do

  close(mtin1)
  close(mtot1)

end program smooth_correction_factor
