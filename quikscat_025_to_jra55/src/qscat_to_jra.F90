! -*-F90-*-
!
!----------------------- qscat_to_jra.F90 --------------------------
!
!  Information:
!      Horizontal interpolation within the same coordinate system.  
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
  real(8),allocatable :: mask_org(:,:,:)

  ! interpolated data

  integer(4) :: imt, jmt, kmt
  real(8),allocatable :: alond(:), alatd(:)
  real(4),allocatable :: dat4_new(:,:)
  real(8),allocatable :: dat_new(:,:,:)
  real(8),allocatable :: mask_new(:,:,:)
  real(4),allocatable :: mask_valid(:,:)
  integer(4),allocatable :: imask_valid(:,:)

  integer(4), parameter :: mtin1 = 61
  integer(4), parameter :: mtot1 = 70

  character(len=clen) :: flin1
  character(len=clen) :: flot1
  character(len=clen) :: file_topo, file_mask

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

  logical :: l_mask_out
  integer(4) :: nf_out, lreclen

  !-----------------------------------------------------------------------

  namelist /nml_hintpol/ flin1, flot1, &
       & undef_out, num_data, &
       & file_topo, file_mask, l_mask_out

  !-----------------------------------------------------------------------

  num_data = 1 ! initialize

  read(5,nml=nml_hintpol)

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
  allocate(mask_org(0:imf+1,1:jmf,1:kmf))

  alonf(1:imf) = orgg%lonu(1:imf)
  alatf(1:jmf) = orgg%latu(1:jmf)

  alonf(0) = orgg%lonu(imf) - 360.d0
  alonf(imf+1) = orgg%lonu(1) + 360.d0

  mask_org(1:imf,1:jmf,1:kmf) = orgt%aexl(1:imf,1:jmf,1:kmf)
  mask_org(0,1:jmf,1:kmf) = mask_org(imf,1:jmf,1:kmf)
  mask_org(imf+1,1:jmf,1:kmf) = mask_org(1,1:jmf,1:kmf)

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
  allocate(mask_new(1:imt,1:jmt,1:kmt))
  allocate(mask_valid(1:imt,1:jmt))
  allocate(imask_valid(1:imt,1:jmt))

  alond(1:imt) = newg%lonu(1:imt)
  alatd(1:jmt) = newg%latu(1:jmt)

  mask_new(1:imt,1:jmt,1:kmt) = newt%aexl(1:imt,1:jmt,1:kmt)

  lreclen = 4*imt*jmt

  !--------------------------------------------------------------------

  write(6,*) 'DATA read from ',trim(flin1)
  open(mtin1,file=flin1,form='unformatted',access='direct',recl=4*imf*jmf)
  irec1 = 0

  write(6,*) 'DATA written to ',trim(flot1)
  open(mtot1,file=flot1,form='unformatted',access='direct',recl=lreclen)
  irec2 = 0

  mask_valid(:,:) = 0.0
  imask_valid(:,:) = 0

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

          hl1 = mask_org(iss ,jss ,k)
          hl2 = mask_org(issp,jss ,k)
          hl3 = mask_org(iss ,jssp,k)
          hl4 = mask_org(issp,jssp,k)

          if ((mask_new(i,j,k) > 0.0d0) .and. (hl1*hl2*hl3*hl4 > 0.0d0)) then
            dat_new(i,j,k) = ssic * ssjc * dat_org(iss ,jss ,k) &
                 &         + ssi  * ssjc * dat_org(issp,jss ,k) &
                 &         + ssic * ssj  * dat_org(iss ,jssp,k) &
                 &         + ssi  * ssj  * dat_org(issp,jssp,k)
          else
            dat_new(i,j,k) = real(undef_out,8)
          end if

        end do
      end do
    end do

    do k = 1, kmt
      dat4_new(1:imt,1:jmt) = real(dat_new(1:imt,1:jmt,k),4)
      irec2 = irec2 + 1
      write(mtot1,rec=irec2) dat4_new
    end do

    if (n == 1) then
      do j = 1, jmt
        do i = 1, imt
          if (dat_new(i,j,1) /= real(undef_out,8)) then
            mask_valid(i,j) = 1.0
          end if
        end do
      end do
    end if

  end do

  close(mtin1)
  close(mtot1)

  if (l_mask_out) then
    write(6,*) ' mask file = ',trim(file_mask)
    call open_file_direct(nf_out,file_mask,lreclen)
    write(nf_out,rec=1) mask_valid
    call close_file(nf_out)

    imask_valid(1:imt,1:jmt) = nint(mask_valid(1:imt,1:jmt))
    write(6,*) ' topo file = ',trim(file_topo)
    call open_file_sequential(nf_out,file_topo)
    write(nf_out) imask_valid, imask_valid
    call close_file(nf_out)
  end if

  !--------------------------------------------------------------------

  deallocate(dat_new, dat4_new, mask_new, mask_valid, imask_valid)
  deallocate(alond, alatd)
  deallocate(dat_org, dat4_org, mask_org)
  deallocate(alonf, alatf)

end program horizontal_interpolation
