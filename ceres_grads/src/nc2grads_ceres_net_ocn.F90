! -*-F90-*-
!==================================================================
program read_netcdf_output_grads
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of
  !     the CERES radiation
  !----------------------------------------------------------------

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

  include '/usr/local/netcdf/include/netcdf.inc'

  integer(4), parameter :: mtin1 = 81, mtin2 = 82
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  real(4),allocatable :: dat4(:,:)
  real(4),allocatable :: nswrf(:,:)
  real(4),allocatable :: nlwrf(:,:)
  real(4),allocatable :: ice(:,:)

  integer(4) :: i, j, k, l, m, n
  integer(4) :: ii, jj, ij, nd, imut, jmut, is, id, istat
  integer(4) :: ibu, ieu, jbu, jeu, juend

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo
  type(type_libmxe_io) :: io
  type(type_grads) :: grads

  integer(4) :: idmon(12)

  character(128) :: flnin1_base, flnin2_base
  character(128) :: flnot1_base, flnot2_base

  integer(4) :: nmocount
  integer(4) :: ireco1, ireco2
  integer(4) :: nyear
  integer(4) :: mst, med

  real(4) :: undef_in1 = -999.e0
  real(4) :: undef_in2 = -9.99e33
  real(4) :: undef_out = -9.99e33

  ! for netCDF

  integer(4), parameter :: nvars = 2, nfiles = 1
  integer(4) :: var(nvars), sta(nvars)
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  character(128) :: flnin1, flnin2
  character(128) :: flnot1, flnot2

  !-----------------------------------------------------------------------

  namelist /nml_ceres_net_ocn/ &
       & flnin1_base, undef_in1, &
       & flnin2_base, undef_in2, &
       & flnot1_base, flnot2_base, undef_out

  open (11,file='namelist.ceres_net_ocn')
  read (11,nml=nml_ceres_net_ocn)
  close(11)

  !--------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

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

  allocate (dat4 (1:imut,1:jmut))
  allocate (nswrf(1:imut,1:jmut))
  allocate (nlwrf(1:imut,1:jmut))
  allocate (ice  (1:imut,1:jmut))

  !---------------------------------------------------------------------
  ! open netcdf file

  write(flnin(1),'(1a)') trim(flnin1_base)

  do n = 1, nfiles
    sta(n) = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
    if (sta(n) /= 0) then
      write(6,*) 'nf_open error for file number ',n
      stop
    end if
  end do

  start(1:3) = (/ 1, 1, 1 /)
  range(1:3) = (/ imut, jmut, 1 /)

  nmocount = 0

  do nyear = 2000, 2014

    mst = 1
    med = 12

    if (nyear == 2000) mst = 3
    if (nyear == 2014) med = 9

    do m = mst, med

      write(flnin2,'(1a,i4.4,i2.2)') trim(flnin2_base),nyear,m
      write(6,*) ' Open ICE file ', trim(flnin2)
      open(mtin2, file=flnin2, form='unformatted', &
           & access='direct', recl=4*imut*jmut)
      read(mtin2,rec=1) ice
      close(mtin2)

      sta(1) = nf_inq_varid(ifiles(1),'sfc_net_sw_all_mon',var(1))
      sta(2) = nf_inq_varid(ifiles(1),'sfc_net_lw_all_mon',var(2))

      do n = 1, nvars
        if (sta(n) /= 0) then
          write(6,*) 'nf_inq_varid error for variable ',n, var(n)
          stop
        end if
      end do

      nmocount = nmocount + 1

      start(3) = nmocount

      !-------------------------------------------------------------------------

      sta(1) = nf_get_vara_real(ifiles(1),var(1),start,range,dat4)

      do j = 1, jmut
        do i = 1, imut
          if (topo%aexl(i,j,1) /= 0.0d0) then
            nswrf(i,j) = dat4(i,j) * topo%aexl(i,j,1) * (1.0 - ice(i,j))
          else
            nswrf(i,j) = undef_out
          end if
        end do
      end do

      ! open output file

      write(flnot1,'(1a,i4.4,i2.2)') trim(flnot1_base),nyear,m
      open(mtot1,file=flnot1,form='unformatted',access='direct',recl=4*imut*jmut)
      write(6,*) 'Net short wave written to ',trim(flnot1)
      ireco1 = 0
      ireco1 = ireco1 + 1
      write(mtot1,rec=ireco1) nswrf
      close(mtot1)

      !-------------------------------------------------------------------------

      sta(2) = nf_get_vara_real(ifiles(1),var(2),start,range,dat4)

      do j = 1, jmut
        do i = 1, imut
          if (topo%aexl(i,j,1) /= 0.0d0) then
            nlwrf(i,j) = dat4(i,j) * topo%aexl(i,j,1) * (1.0 - ice(i,j))
          else
            nlwrf(i,j) = undef_out
          end if
        end do
      end do

      ! open output file

      write(flnot2,'(1a,i4.4,i2.2)') trim(flnot2_base),nyear,m
      open(mtot2,file=flnot2,form='unformatted',access='direct',recl=4*imut*jmut)
      write(6,*) 'Net long wave written to ',trim(flnot2)
      ireco2 = 0
      ireco2 = ireco2 + 1
      write(mtot2,rec=ireco2) nlwrf
      close(mtot2)

    end do
  end do

  do n = 1, nfiles
    write(6,*) ifiles(n)
    sta(n) = nf_close(ifiles(n))
    if (sta(n) /= 0) then
      write(6,*) 'nf_close error for file number ',n, ifiles(n), sta(n)
    end if
  end do

end program read_netcdf_output_grads
