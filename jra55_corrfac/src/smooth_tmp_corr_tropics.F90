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

  real(8),allocatable :: tropics_mask(:,:)

  real(8),allocatable :: mask_wrk(:,:)
  real(8),allocatable :: crrc_wrk(:,:)
  real(8),allocatable :: crrc_tmp(:,:)

  real(8),allocatable :: alonf(:), alatf(:)
  real(8),allocatable :: aexlf(:,:)

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
  character(128) :: flnin, flnot, file_mask

  integer(4) :: irec1, irec2, irec3
  integer(4) :: iter, iter_total

  real(8) :: fill_value, hl1, hl2, weight
  real(8) :: low_trans, high_trans

  !-----------------------------------------------------------------------

  namelist /nml_smooth_tmp_tropics/ &
       & flnin, undef_in, flnot, ndata, &
       & file_mask, &
       & fill_value, &
       & iter_total

  open (11,file='namelist.smooth_tmp_tropics')
  read (11,nml=nml_smooth_tmp_tropics)
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

  allocate(alonf(1:imut), alatf(1:jmut))
  alonf(1:imut) = grid%lonu(1:imut)
  alatf(1:jmut) = grid%latu(1:jmut)

  allocate (aexlf(1:imut,1:jmut))
  aexlf(:,:) = topo%aexl(:,:,1)

  allocate (work4(1:imut,1:jmut))
  allocate (crrc_org(1:imut,1:jmut))
  allocate (crrc_fin(1:imut,1:jmut))

  allocate (tropics_mask(1:imut,1:jmut))

  ! read correction factor

  open(mtin1, file=file_mask, access='direct', recl=4*imut*jmut)
  write(*,'(1a,1a)') 'mask data read from ... ', trim(file_mask)
  read(mtin1,rec=1) ((work4(i,j),i=1,imut),j=1,jmut)
  tropics_mask(1:imut,1:jmut) = real(work4(1:imut,1:jmut),8)
  close(mtin1)

  allocate (mask_wrk(0:imut+1,0:jmut+1))
  allocate (crrc_wrk(0:imut+1,0:jmut+1))
  allocate (crrc_tmp(0:imut+1,0:jmut+1))

  mask_wrk(:,:) = 0.0d0
  mask_wrk(1:imut,1:jmut) = tropics_mask(1:imut,1:jmut)
  mask_wrk(0,1:jmut) = mask_wrk(imut,1:jmut)
  mask_wrk(imut+1,1:jmut) = mask_wrk(1,1:jmut)

  !---------------------------------------------------------------
  ! read correction factor

  open(mtin1, file=flnin, access='direct', recl=4*imut*jmut)
  write(*,'(1a,1a)') 'data read from ... ', trim(flnin)

  open(mtot1, file=flnot, access='direct', recl=4*imut*jmut)
  write(*,'(1a,1a)') 'data written to ... ', trim(flnot)

  write(6,*) ' Total iteration count = ', iter_total

  do nd = 1, ndata

    write(6,*) ' Processing ', nd , ' th data'

    read(mtin1,rec=nd) ((work4(i,j),i=1,imut),j=1,jmut)

    crrc_org(1:imut,1:jmut) = real(work4(1:imut,1:jmut),8)

    crrc_fin(1:imut,1:jmut) = crrc_org(1:imut,1:jmut)

    LOOP_ITER: do iter = 1, iter_total

      crrc_tmp(:,:) = fill_value

      do j = jbu, jeu
        do i = ibu, ieu
          if (crrc_fin(i,j) > real(undef_in,8)) then
            crrc_tmp(i,j) = crrc_fin(i,j)
          end if
        end do
      end do

      ! cyclic 

      crrc_tmp(     0,1:jmut) = crrc_tmp(imut,1:jmut)
      crrc_tmp(imut+1,1:jmut) = crrc_tmp(   1,1:jmut)

      do j = jbu, jeu
        do i = ibu, ieu
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
        end do
      end do

      do j = jbu, jeu
        if (abs(alatf(j)) <= 10.d0) then
          weight = 0.0d0
        else if (abs(alatf(j)) < 15.d0) then
          weight = (abs(alatf(j)) - 10.0d0) / 5.0d0
        else
          weight = 1.0d0
        end if
        !write(6,*) alatf(j), weight
        do i = ibu, ieu
          if (mask_wrk(i,j) > 0.0d0) then
            crrc_fin(i,j) = (1.0d0 - weight) * crrc_fin(i,j) + weight * crrc_org(i,j)
          end if
        end do
      end do

    end do LOOP_ITER

    work4(1:imut,1:jmut) = real(crrc_fin(1:imut,1:jmut),4)
    write(mtot1,rec=nd) ((work4(i,j),i=1,imut),j=1,jmut)

  end do

  close(mtin1)
  close(mtot1)

end program smooth_correction_factor
