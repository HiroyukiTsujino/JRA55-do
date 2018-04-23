!-*-F90-*-
program core_to_cama

  use libmxe_para, only: libmxe_para__register &
                     & , clen, pi, radian, radian_r, radius &
                     & , type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io
  use libmxe_calendar

  use file_open_close_manager

  implicit none

  !------------------------------------------------------------

  type(type_libmxe_para) :: camap
  type(type_libmxe_grid) :: camag

  character(256) :: file_namelist_cama
  character(256) :: file_mask_cama
  character(256) :: file_cama_in
  character(256) :: file_river_out
  character(256) :: riv_cama_base
  character(256) :: riv_out_base

  integer(4) :: mtinf, mtot
  integer(4) :: ireclen

  integer(4) :: nxf, nyf
  real(8),allocatable :: roff(:,:), rofm(:,:), rofa(:,:), rofb(:,:)
  real(4),allocatable :: workf(:,:)
  real(4) :: undef_in_cama
  real(8),allocatable :: alonuf(:), alatuf(:)
  real(8),allocatable :: alontf(:), alattf(:)
  real(8),allocatable :: mask_cama(:,:)

  !------------------------------------------------------------

  type(type_libmxe_para) :: corep
  type(type_libmxe_grid) :: coreg

  character(256) :: file_namelist_core
  character(256) :: riv_core_base
  character(256) :: file_core_in
  character(256) :: file_mask_core

  integer(4) :: mtinc

  integer(4) :: nxc, nyc
  real(8),allocatable :: rofc(:,:)
  real(4),allocatable :: workc(:,:)
  real(4) :: undef_in_core
  real(8),allocatable :: alonuc(:), alatuc(:)
  real(8),allocatable :: alontc(:), alattc(:)
  real(8),allocatable :: mask_core(:,:)

  !------------------------------------------------------------

  integer(4) :: nt, nyr, nm, nd, irec, i, j
  integer(4) :: ii, jj, iii, jjj
  
  integer(4) :: irec1, irec2, irec3

  integer(4),parameter :: nmonyr = 12
  integer(4) :: ndmon(nmonyr) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
  integer(4) :: nday_mon

  !------------------------------------------------------------

  type(type_calendar) :: start_date
  type(type_calendar) :: end_date

  type(type_calendar) :: today
  type(type_calendar) :: current_date
  type(type_calendar) :: next_date

  type(type_calendar) :: current_month_start
  type(type_calendar) :: prev_month_start
  type(type_calendar) :: latest_month_start

  integer(4) :: calc_start(6)
  integer(4) :: calc_end(6)

  integer(4) :: current_month_sec
  integer(4) :: this_month_sec
  integer(4) :: prev_month_sec
  integer(4) :: latest_month_sec
  integer(4) :: from_previous_data
  real(8) :: afm, bfm
  integer(4) :: yra, yrb
  integer(4) :: monb, mona, mont

  integer(4) :: lun

  real(8) :: hl1, hl2

  real(8),parameter :: dens_pw = 1.0d3 ! density of pure water

  !--------------------------------------------------------------------------

  namelist /nml_core_to_cama/ &
       & file_namelist_cama, file_namelist_core, &
       & file_mask_cama, file_mask_core, &
       & riv_core_base, riv_out_base, &
       & undef_in_cama, undef_in_core

  !--------------------------------------------------------------------------

  open(10,file='namelist.core_to_cama')
  read(10,nml=nml_core_to_cama)
  close(10)

  !--------------------------------------------------------------------------

  call libmxe_para__register(camap,file_namelist=file_namelist_cama)
  call libmxe_grid__register(camag,camap)

  nxf = camap%imut
  nyf = camap%jmut
  allocate(alonuf(1:nxf), alatuf(1:nyf))
  allocate(alontf(1:nxf), alattf(1:nyf))
  alonuf(1:nxf) = camag%lonu(1:nxf)
  alatuf(1:nyf) = camag%latu(1:nyf)
  alontf(1:nxf) = camag%lont(1:nxf)
  alattf(1:nyf) = camag%latt(1:nyf)

  allocate(workf(1:nxf,1:nyf))
  allocate(mask_cama(1:nxf,1:nyf))
  allocate(roff(1:nxf,1:nyf))
  allocate(rofm(1:nxf,1:nyf))
  allocate(rofa(1:nxf,1:nyf),rofb(1:nxf,1:nyf))

  !------

  call libmxe_para__register(corep,file_namelist=file_namelist_core)
  call libmxe_grid__register(coreg,corep)

  nxc = corep%imut
  nyc = corep%jmut

  allocate(alonuc(1:nxc), alatuc(1:nyc))
  allocate(alontc(1:nxc), alattc(1:nyc))

  alonuc(1:nxc) = coreg%lonu(1:nxc)
  alatuc(1:nyc) = coreg%latu(1:nyc)
  alontc(1:nxc) = coreg%lont(1:nxc)
  alattc(1:nyc) = coreg%latt(1:nyc)

  allocate(workc(1:nxc,1:nyc))
  allocate(mask_core(1:nxc,1:nyc))
  allocate(rofc(1:nxc,1:nyc))

  !--------------------------------------------------------------------------
  !
  !ireclen = 4*nxf*nyf
  !call open_file_direct(lun,file_mask_cama,ireclen)
  !write(6,*) ' Read MASK (CaMa) from ', trim(file_mask_cama)
  !read(lun,rec=1) workf
  !call close_file(lun)
  !mask_cama(1:nxf,1:nyf) = real(workf(1:nxf,1:nyf),8)
  !
  !--------------------------------------------------------------------------
  !
  !ireclen = 4*nxc*nyc
  !call open_file_direct(lun,file_mask_core,ireclen)
  !write(6,*) ' Read MASK (Core) from ', trim(file_mask_core)
  !read(lun,rec=1) workc
  !call close_file(lun)
  !mask_core(1:nxc,1:nyc) = real(workc(1:nxc,1:nyc),8)
  !
  !-------------------------------------------------------------------------

  ireclen = 4*nxc*nyc

  write(file_core_in,'(1a)') trim(riv_core_base)
  call open_file_direct(mtinc,file_core_in,ireclen)
  write(6,*) ' reading from ', trim(file_core_in)
  read(mtinc,rec=1) workc(1:nxc,1:nyc)
  call close_file(mtinc)

  rofc(1:nxc,1:nyc) = real(workc(1:nxc,1:nyc),8) / dens_pw ! [kg/m^2/s] -> [m/s]

  do j = 1, nyc
    do i = 1, nxc
      do jj = 1, 4
        do ii = 1, 4
          iii = 4*(i-1) + ii
          jjj = 4*(j-1) + jj
          rofb(iii,jjj) = rofc(i,j) * camag%areau(iii,jjj) * 1.0d-4 ! area [cm2] -> [m2]
        end do
      end do
    end do
  end do

  !------

  ireclen = 4*nxf*nyf

  write(file_river_out,'(1a)') trim(riv_out_base)
  call open_file_direct(mtot,file_river_out,ireclen)
  write(6,*) ' writing to ', trim(file_river_out)

  irec = 0

  write(mtot,rec=irec) real(rofb(1:nxf,1:nyf),4)

  call close_file(mtot)

  deallocate(alonuf, alatuf)
  deallocate(alontf, alattf)
  deallocate(workf)
  deallocate(roff)
  deallocate(rofm)
  deallocate(rofa,rofb)

  deallocate(alonuc, alatuc)
  deallocate(alontc, alattc)
  deallocate(workc)
  deallocate(rofc)

  write(6,*) ' Operation ended '
  
end program core_to_cama
