! -*-F90-*-
!
!======================================================================
! Information:
!     Reduce downward radiation climatology of CERES
!----------------------------------------------------------------------
program filter_downward_radiation

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

  real(8) :: filter_lat_s, filter_lat_n

  !-----------------------------------------------------------------------

  namelist /nml_filter_clim/ flnin, undef_in, flnot, ndata, &
       & filter_lat_s, filter_lat_n
  open (11,file='namelist.filter_clim')
  read (11,nml=nml_filter_clim)
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
      
    rad_fin(1:imut,1:jmut) = rad_org(1:imut,1:jmut)

    do j = 2, jmut - 1
      
      if ((filter_lat_s <= grid%latu(j)) &
           & .and. (grid%latu(j) <= filter_lat_n)) then
        do i = 1, imut
          rad_fin(i,j) = 0.25d0 * (rad_org(i,j-1)&
               & + 2.0d0 * rad_org(i,j) + rad_org(i,j+1))
        end do
      end if
    end do

    work4(1:imut,1:jmut) = real(rad_fin(1:imut,1:jmut),4)
    write(mtot1,rec=nd) ((work4(i,j),i=1,imut),j=1,jmut)

  end do

  close(mtin1)
  close(mtot1)

end program filter_downward_radiation
