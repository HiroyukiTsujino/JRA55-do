! -*-F90-*-
!
!======================================================================
! Information:
!     Reduce downward short wave climatology of CERES
!----------------------------------------------------------------------
program reduce_downward_shortwave

  use libmxe_para, only: libmxe_para__register, clen, rho &
                     & , pi, radius, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo

  implicit none

  integer(4) :: ndata

  real(4),allocatable :: work4(:,:)
  real(4) :: undef_in

  real(8),allocatable :: rad_org(:,:)
  real(8),allocatable :: rad_fin(:,:)

  integer :: i,j,ii,jj,ij,n,nd,imut,jmut, is, id, istat
  integer :: ibu, ieu, jbu, jeu, juend

  integer :: iuvts
  integer :: ireverse

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo

  !-----

  integer(4), parameter :: mtin1 = 71, mtin2 = 72, mtot1 = 81, mtot2 = 82
  character(128) :: flnin, flnot

  integer(4) :: irec1, irec2, irec3
  integer(4) :: iter, itermax

  logical :: l_apply_mask
  real(8) :: hl1, hl2

  real(8) :: low_trans_n, high_trans_n
  real(8) :: low_sigm_n, high_sigm_n
  real(8) :: reduct_fac_n

  real(8) :: low_trans_s, high_trans_s
  real(8) :: low_sigm_s, high_sigm_s
  real(8) :: reduct_fac_s

  !-----------------------------------------------------------------------

  namelist /nml_reduce_swdn/ flnin, undef_in, flnot, ndata, &
       & low_trans_n, low_sigm_n, high_trans_n, high_sigm_n, reduct_fac_n, &
       & low_trans_s, low_sigm_s, high_trans_s, high_sigm_s, reduct_fac_s
  open (11,file='namelist.reduce_ceres_swdn')
  read (11,nml=nml_reduce_swdn)
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
  allocate (rad_org(1:imut,1:jmut))
  allocate (rad_fin(1:imut,1:jmut))

  !---------------------------------------------------------------
  ! read correction factor

  open(mtin1, file=flnin, access='direct', recl=4*imut*jmut)
  write(*,'(1a,1a)') 'data read from ... ', trim(flnin)

  open(mtot1, file=flnot, access='direct', recl=4*imut*jmut)
  write(*,'(1a,1a)') 'data written to ... ', trim(flnot)

  do nd = 1, ndata

    write(6,*) ' Processing ', nd , ' th data'

    read(mtin1,rec=nd) ((work4(i,j),i=1,imut),j=1,jmut)

    rad_org(1:imut,1:jmut) = real(work4(1:imut,1:jmut),8)
      
    do j = 1, jmut
      
      if (grid%latu(j) >= 0.0d0) then
        if (abs(grid%latu(j)) > high_trans_n) then
          hl1 = 1.0d0 - reduct_fac_n * exp(-((abs(grid%latu(j))-high_trans_n)/high_sigm_n)**2)
        end if
        if (abs(grid%latu(j)) < low_trans_n) then
          hl1 = (1.0d0 - reduct_fac_n * exp(-((abs(grid%latu(j))-low_trans_n)/low_sigm_n)**2))
          !hl2 = abs(grid%latu(j)) / low_trans
          !hl1 = 1.0d0 - reduct_fac * hl2 * exp(-hl2) / exp(-1.0d0)
        end if
        if ((low_trans_n <= abs(grid%latu(j))) .and. (abs(grid%latu(j)) <= high_trans_n)) then
          hl1 = 1.0d0 - reduct_fac_n
        end if
      else
        if (abs(grid%latu(j)) > high_trans_s) then
          hl1 = 1.0d0 - reduct_fac_s * exp(-((abs(grid%latu(j))-high_trans_s)/high_sigm_s)**2)
        end if
        if (abs(grid%latu(j)) < low_trans_s) then
          hl1 = (1.0d0 - reduct_fac_s * exp(-((abs(grid%latu(j))-low_trans_s)/low_sigm_s)**2))
          !hl2 = abs(grid%latu(j)) / low_trans
          !hl1 = 1.0d0 - reduct_fac * hl2 * exp(-hl2) / exp(-1.0d0)
        end if
        if ((low_trans_s <= abs(grid%latu(j))) .and. (abs(grid%latu(j)) <= high_trans_s)) then
          hl1 = 1.0d0 - reduct_fac_s
        end if
      end if

      write(6,*) grid%latu(j), hl1

      do i = 1, imut
        rad_fin(i,j) = rad_org(i,j) * hl1
      end do

    end do

    work4(1:imut,1:jmut) = real(rad_fin(1:imut,1:jmut),4)
    write(mtot1,rec=nd) ((work4(i,j),i=1,imut),j=1,jmut)

  end do

  close(mtin1)
  close(mtot1)

end program reduce_downward_shortwave
