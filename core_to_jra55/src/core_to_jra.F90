! -*-F90-*-
!
!----------------------- core_to_jra.F90 --------------------------
!
!  Information:
!      Horizontal interpolation between the same coordinate system.  
!
!-------------------------------------------------------------------
program horizontal_interpolation

  use file_open_close_manager

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

  character(len=256) :: flin1
  character(len=256) :: flot1

  integer(4) :: irec1, irec2

  integer(4) :: i, j, k, n

  type(type_libmxe_para) :: newp, orgp
  type(type_libmxe_grid) :: newg, orgg
  type(type_libmxe_topo) :: newt, orgt
  type(type_libmxe_io)   :: newio, orgio

  integer(4) :: num_data

  integer(4) :: iss, jss, issp, jssp
  real(8) :: ssi, ssj, ssic, ssjc
  integer(4) :: ii, jj
  real(4) :: undef_out
  real(8) :: hl1, hl2, hl3, hl4

  integer(4) :: nf_out, lreclen

  !-----------------------------------------------------------------------

  namelist /nml_hintpol/ flin1, flot1, undef_out, num_data

  !-----------------------------------------------------------------------

  num_data = 1 ! initialize

  open(10,file='namelist_core2jra55')
  read(10,nml=nml_hintpol)
  close(10)

  !-----------------------------------------------------------------------

  call libmxe_para__register(orgp, file_namelist='NAMELIST.MXE.ORG')
  imf = orgp%imut
  jmf = orgp%jmut
  kmf = orgp%km
  write(6,*) imf, jmf, kmf
  call libmxe_grid__register(orgg,orgp)
  call libmxe_topo__register(orgt,orgp)
  call libmxe_topo__aexl(orgt,orgp)
  !call libmxe_io__register(orgio,orgp)

  allocate(alonf(0:imf+1),alatf(1:jmf))
  allocate(dat4_org(1:imf,1:jmf))
  allocate(dat_org(0:imf+1,1:jmf,1:kmf))

  alonf(1:imf) = orgg%lonu(1:imf)
  alatf(1:jmf) = orgg%latu(1:jmf)

  alonf(0) = orgg%lonu(imf) - 360.d0
  alonf(imf+1) = orgg%lonu(1) + 360.d0

  !-----------------------------------------------------------------------

  call libmxe_para__register(newp, file_namelist='NAMELIST.MXE.NEW')
  imt = newp%imut
  jmt = newp%jmut
  kmt = newp%km
  write(6,*) imt, jmt, kmt
  call libmxe_grid__register(newg,newp)
  call libmxe_topo__register(newt,newp)
  call libmxe_topo__aexl(newt,newp)
  !call libmxe_io__register(newio,newp)

  allocate(alond(1:imt),alatd(1:jmt))
  allocate(dat4_new(1:imt,1:jmt))
  allocate(dat_new(1:imt,1:jmt,1:kmt))

  alond(1:imt) = newg%lonu(1:imt)
  alatd(1:jmt) = newg%latu(1:jmt)

  lreclen = 4*imt*jmt

  !--------------------------------------------------------------------

  write(6,*) '  DATA read from ',trim(flin1)
  open(mtin1,file=flin1,form='unformatted',access='direct',recl=4*imf*jmf)
  irec1 = 0

  write(6,*) '  DATA written to ',trim(flot1)
  open(mtot1,file=flot1,form='unformatted',access='direct',recl=lreclen)
  irec2 = 0

  do n = 1, num_data

    do k = 1, kmf

      irec1 = irec1 + 1
      read(mtin1,rec=irec1) dat4_org(1:imf,1:jmf)
      dat_org(1:imf,1:jmf,k) = real(dat4_org(1:imf,1:jmf),8)
      dat_org(0,1:jmf,k) = dat_org(imf,1:jmf,k)
      dat_org(imf+1,1:jmf,k) = dat_org(1,1:jmf,k)

      do j = 1, jmt

        jss = jmf
        do jj = 1, jmf
          if ( alatd(j) <= alatf(jj) ) then
            jss = jj - 1
            exit
          end if
        end do

        jssp = jss + 1
        if ( jss < 1 ) jss = 1
        if ( jssp > jmf ) jssp = jmf
        if ( jss == jssp ) then
          ssj = 1.0d0
        else
          ssj = (alatd(j) - alatf(jss)) / (alatf(jssp) - alatf(jss))
        end if
        ssjc = 1.0d0 - ssj

        do i = 1, imt
          iss = imf
          do ii = 1, imf
            if ( alond(i) <= alonf(ii) ) then
              iss = ii - 1
              exit
            end if
          end do
          issp = iss + 1
          ssi = (alond(i) - alonf(iss)) / (alonf(issp) - alonf(iss))
          ssic = 1.0d0 - ssi

          dat_new(i,j,k) = ssic * ssjc * dat_org(iss ,jss ,k) &
               &         + ssi  * ssjc * dat_org(issp,jss ,k) &
               &         + ssic * ssj  * dat_org(iss ,jssp,k) &
               &         + ssi  * ssj  * dat_org(issp,jssp,k)

        end do
      end do
    end do

    do k = 1, kmt
      dat4_new(1:imt,1:jmt) = real(dat_new(1:imt,1:jmt,k),4)
      irec2 = irec2 + 1
      write(mtot1,rec=irec2) dat4_new
    end do

  end do

  close(mtin1)
  close(mtot1)

  !--------------------------------------------------------------------

  deallocate(dat_new, dat4_new)
  deallocate(alond, alatd)
  deallocate(dat_org, dat4_org)
  deallocate(alonf, alatf)

end program horizontal_interpolation
