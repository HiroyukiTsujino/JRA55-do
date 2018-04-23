! -*-F90-*-
!
!------------------------- h_intpol.F90 ----------------------------
!
!  Information:
!      Horizontal interpolation within the same coordinate system.  
!
!-------------------------------------------------------------------
program horizontal_interpolation

  use libmxe_para, only: libmxe_para__register &
                     & , clen, pi, radian, radian_r, radius &
                     & , type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io

  implicit none

  ! original data

  integer(4) :: imf, jmf, kmf
  real(8),allocatable :: alonf(:), alatf(:)
  real(4),allocatable :: dat4_org(:,:)
  real(8),allocatable :: dat_org(:,:,:)

  ! interpolated data

  integer(4) :: imt, jmt, kmt
  real(8),allocatable :: alond(:), alatd(:)
  real(4),allocatable :: dat4_new(:,:)
  real(8),allocatable :: dat_new(:,:,:)

  integer(4), parameter :: mtin1 = 61
  integer(4), parameter :: mtot1 = 70

  character(len=clen) :: flin1
  character(len=clen) :: flot1

  integer(4) :: irec1, irec2

  integer(4) :: i, j, k, n

  type(type_libmxe_para) :: newp, orgp
  type(type_libmxe_grid) :: newg, orgg
  type(type_libmxe_topo) :: newt, orgt
  type(type_libmxe_io)   :: newio, orgio

  integer(4) :: num_data

  !-----------------------------------------------------------------------

  namelist /nml_hintpol/ flin1, flot1, num_data

  !-----------------------------------------------------------------------

  num_data = 1 ! initialize

  read(5,nml=nml_hintpol)

  call libmxe_para__register(orgp, file_namelist='NAMELIST.MXE.ORG')
  imf = orgp%imut
  jmf = orgp%jmut
  kmf = orgp%km
  write(6,*) imf, jmf, kmf
  call libmxe_grid__register(orgg,orgp)
  !call libmxe_topo__register(orgt,orgp)
  !call libmxe_topo__aexl(orgt,orgp)
  !call libmxe_io__register(orgio,orgp)

  allocate(alonf(1:imf),alatf(1:jmf))
  allocate(dat4_org(1:imf,1:jmf))
  allocate(dat_org(1:imf,1:jmf,1:kmf))

  alonf(1:imf) = orgg%lonu(1:imf)
  alatf(1:jmf) = orgg%latu(1:jmf)

  !-----------------------------------------------------------------------

  call libmxe_para__register(newp, file_namelist='NAMELIST.MXE.NEW')
  imt = newp%imut
  jmt = newp%jmut
  kmt = newp%km
  write(6,*) imt, jmt, kmt
  call libmxe_grid__register(newg,newp)
  !call libmxe_topo__register(newt,newp)
  !call libmxe_topo__aexl(newt,newp)
  !call libmxe_io__register(newio,newp)

  allocate(alond(1:imt),alatd(1:jmt))
  allocate(dat4_new(1:imt,1:jmt))
  allocate(dat_new(1:imt,1:jmt,1:kmt))

  alond(1:imt) = newg%lonu(1:imt)
  alatd(1:jmt) = newg%latu(1:jmt)

  !--------------------------------------------------------------------

  write(6,*) 'DATA read from ',trim(flin1)
  open(mtin1,file=flin1,form='unformatted',access='direct',recl=4*imf*jmf)
  irec1 = 0

  write(6,*) 'DATA written to ',trim(flot1)
  open(mtot1,file=flot1,form='unformatted',access='direct',recl=4*imt*jmt)
  irec2 = 0

  do n = 1, num_data

    do k = 1, kmf
      irec1 = irec1 + 1
      read(mtin1,rec=irec1) dat4_org(1:imf,1:jmf)
      dat_org(1:imf,1:jmf,k) = real(dat4_org(1:imf,1:jmf),8)
    end do

    do k = 1, kmf
      call hintpl(dat_new(1,1,k),imt,jmt,alond,alatd,&
           &      dat_org(1,1,k),imf,jmf,alonf,alatf)
    end do

    ! open output file

    do k = 1, kmt
      dat4_new(1:imt,1:jmt) = real(dat_new(1:imt,1:jmt,k),4)
      irec2 = irec2 + 1
      write(mtot1,rec=irec2) dat4_new
    end do

  end do

  close(mtin1)
  close(mtot1)

  deallocate(dat_new, dat4_new)
  deallocate(alond, alatd)
  deallocate(dat_org, dat4_org)
  deallocate(alonf, alatf)

end program horizontal_interpolation
