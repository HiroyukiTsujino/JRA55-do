!-*-F90-*-
!grd2nc_g.F90
!====================================================
!
!  Create netCDF file from GrADs file
!
!====================================================
program grads_to_netCDF_general

  use oc_mod_param, only :   &
  &  imut, jmut, km

  use oc_structure, only : &
  &  set_hgrids,           &
  &  read_scale,           &
  &  read_topo,            &
  &  set_volt,             &
  &  set_volu,             &
  &  set_area_t,           &
  &  set_area_u,           &
  &  alatt, alont, alonu, alatu, &
  &  dp, dep,              &
  &  atexl, volt,          &
  &  aexl, volu,           &
  &  areau, areat,         &
  &  exnn, exnnbbl,        &
  &  texnn, texnnbbl

  use netCDF_write, only : &
  &  netCDF_write__create_file, &
  &  netCDF_write__var_4d

  implicit none

  real(4),allocatable :: dat4(:,:,:)

  real(4),   allocatable :: mask(:,:,:)
  real(4),   allocatable :: volm(:,:,:)
  integer(4),allocatable :: ibas_u(:,:) ! basin区分, UV-Grid
  integer(4),allocatable :: ibas_t(:,:) ! basin区分, UV-Grid

  character(len=256)     :: fpath
  character(len=256)     :: flin
  character(len=256)     :: flout
  character(len=256)     :: file_topo
  character(len=256)     :: file_scale
  character(len=256)     :: file_vgrid
  character(len=256)     :: file_basin ! basinインデックスファイル
  character(len=80)      :: fmt_ibas

  logical :: l_one_data = .false.
  logical :: l_mask_out = .false.
  logical :: l_append_ext = .false.
  character(len=16)    :: cext

  integer(4) :: num_vars

  integer(4), parameter :: mtin    = 82
  integer(4), parameter :: mtbas   = 83
  integer(4), parameter :: mtout   = 85
  integer(4) :: ios

  integer(4) :: i, j, k, m, n, jj

  !--- netCDF ---

  integer(4) :: NLVLS, NLATS, NLONS
  real(4), allocatable :: var_out(:,:,:,:)
  real(8), allocatable :: dlon(:), dlat(:), depth(:)
#ifdef OGCM_TRIPOLAR
  real(8), parameter :: lon_off = 80.d0
  real(8), parameter :: lat_off = 0.d0
#else /* OGCM_TRIPOLAR */
  real(8), parameter :: lon_off = 0.d0
  real(8), parameter :: lat_off = 0.d0
#endif /* OGCM_TRIPOLAR */

  real(8) :: lon_first, delta_lon
  real(8) :: lat_first, delta_lat
  logical :: l_lon_model, l_lat_model, l_lev_model
  character(len=1) :: tuw

  integer(4), parameter :: nlvls_max = 700
  real(8) :: zlev(nlvls_max)

  integer(4) :: nrecs, nrecs_per_year, ireci, ireco, nd_year
  real(8), allocatable :: time_axis(:), time_axis_b(:), time_axis_e(:)
  logical :: l_initialize = .true.
  character(len=64) :: LON_UNITS, LAT_UNITS, LVL_UNITS
  character(len=64) :: REC_UNITS ='months since XXXX-1-1 00:00:00' 

  character(len=128) :: file_in, file_out

  integer(4) :: num_var_out, num_var_all
  integer(4) :: nth_place_tmp
  integer(4),allocatable :: nth_place(:)

  character(len=16)  :: vname_tmp, vunit_tmp
  character(len=128) :: vlongname_tmp
  character(len=16), allocatable :: vname(:), vunit(:)
  character(len=128),allocatable :: vlongname(:)

  real(4) :: rconv_tmp, rmin_tmp, rmax_tmp, undef_in_tmp, undef_out_tmp
  real(4),allocatable :: rconv(:), rmin(:), rmax(:), undef_in(:), undef_out(:)


  integer(4) :: ibyear=2000
  integer(4) :: ieyear=2000
  integer(4) :: i_ref_year=2000
  integer(4) :: intv_indx ! unit of interval of time axis ... 1: year, 2: month, 3: day, 4: hour, 5: minute
  logical    :: l_leap

  integer(4) :: nyear

  character(len=12),parameter :: cintv(5) = (/ &
       & "years       ", &
       & "months      ", &
       & "days        ", &
       & "hours       ", &
       & "minutes     "  /)

  character(len=256) :: file_ctl
  character(len=16)  :: time_start
  character(len=16)  :: time_intv

  !------------------------------------------

  namelist /nml_grd2nc_g/ file_in, file_out, tuw,             &
       & l_append_ext,  cext,  l_one_data, l_mask_out,        &
       & num_vars,  num_var_out,                              &
       & lon_first, delta_lon, nlons, l_lon_model, LON_UNITS, & ! X
       & lat_first, delta_lat, nlats, l_lat_model, LAT_UNITS, & ! Y
       & zlev,                 nlvls, l_lev_model, LVL_UNITS, & ! Z
       & i_ref_year, ibyear, ieyear, intv_indx, nrecs_per_year, l_leap,   & ! T
       & file_topo, file_vgrid, file_scale, file_basin,       &
       & file_ctl, time_start, time_intv

  namelist /nml_vars/ nth_place_tmp,               &
       & vname_tmp, vunit_tmp, vlongname_tmp,      &
       & rconv_tmp, rmin_tmp, rmax_tmp, undef_in_tmp, undef_out_tmp

  !------------------------------------------

  file_in    = 'hs_s.grd'
  file_out   = 'hs_s.nc'
  file_topo  = 'topo.d'
  file_scale = 'scale.d'
  file_vgrid = 'vgrid.d'

  write(6,*) 'reading namelist ......'

  read(unit=5, nml=nml_grd2nc_g)

  write(6,*) 'file_in    :', trim(file_in)
  write(6,*) 'file_out   :', trim(file_out)
  write(6,*) 'file_topo  :', trim(file_topo)
  write(6,*) 'file_scale :', trim(file_scale)
  write(6,*) 'file_vgrid :', trim(file_vgrid)
  write(6,*) 'file_basin :', trim(file_basin)

  !----------------------------------------------

  if (l_mask_out) then
    num_var_all = num_var_out + 2
  else
    num_var_all = num_var_out
  end if

  write(6,*) num_var_all

  allocate(nth_place(1:num_var_out))
  allocate(rconv(1:num_var_out), rmin(1:num_var_out),rmax(1:num_var_out),undef_in(1:num_var_out))

  allocate(vname(1:num_var_all), vunit(1:num_var_all))
  allocate(vlongname(1:num_var_all))
  allocate(undef_out(1:num_var_all))

  rewind(5)

  do n = 1, num_var_out
    read(5,nml=nml_vars)
    nth_place(n) = nth_place_tmp
    vname(n)     = trim(vname_tmp)
    vunit(n)     = trim(vunit_tmp)
    vlongname(n) = trim(vlongname_tmp)
    rconv(n)     = rconv_tmp
    rmin(n)      = rmin_tmp
    rmax(n)      = rmax_tmp
    undef_in(n)  = undef_in_tmp
    undef_out(n) = undef_out_tmp
  end do

  if (l_mask_out) then
    vname(num_var_out + 1)     = 'mask'
    vunit(num_var_out + 1)     = 'none'
    vlongname(num_var_out + 1) = 'basin mask (0:land, 1:atl, 2:ind, 3:pac, 4:med, 9:sou)'
    undef_out(num_var_out + 1) = -9.99e33
    if (l_lev_model) then
      vname(num_var_out + 2)     = 'volume'
      vunit(num_var_out + 2)     = 'm^3'
      vlongname(num_var_out + 2) = 'cell volume'
      undef_out(num_var_out + 2) = -9.99e33
    else
      vname(num_var_out + 2)     = 'area'
      vunit(num_var_out + 2)     = 'm^2'
      vlongname(num_var_out + 2) = 'cell area'
      undef_out(num_var_out + 2) = -9.99e33
    end if
  end if


  !----------------------------------------------

  write(REC_UNITS,'(1a,1a,i4.4,1a)') &
       & trim(cintv(intv_indx)),' since ',i_ref_year,'-1-1 00:00:00' 
  write(6,*) 'REC_UNITS : ', trim(REC_UNITS)

  ! assumes one year per one file

  if (l_one_data) then

    nrecs = 1

    allocate(time_axis  (1:nrecs))
    allocate(time_axis_b(1:nrecs))
    allocate(time_axis_e(1:nrecs))

    if (intv_indx == 1) then
      nd_year = 1
      ireco = 1
      time_axis  (ireco) = (real(ieyear-i_ref_year+1,8) * 0.5d0) * nd_year
      time_axis_b(ireco) = (real(i_ref_year-i_ref_year,8)) * nd_year
      time_axis_e(ireco) = (real(ieyear-i_ref_year+1,8)) * nd_year
    end if

    if (intv_indx == 3) then
      nd_year = 365
      ireco = 1
      time_axis  (ireco) = (real(ieyear-i_ref_year+1,8) * 0.5d0) * nd_year
      time_axis_b(ireco) = (real(i_ref_year-i_ref_year,8)) * nd_year
      time_axis_e(ireco) = (real(ieyear-i_ref_year+1,8)) * nd_year
    end if

    ibyear = ieyear

  else

    nrecs = (ieyear - ibyear + 1) * nrecs_per_year

    allocate(time_axis  (1:nrecs))
    allocate(time_axis_b(1:nrecs))
    allocate(time_axis_e(1:nrecs))

    if (intv_indx == 1) then
      nd_year = 1
      ireco = 0
      do nyear = ibyear, ieyear
        do n = 1, nrecs_per_year
          ireco = ireco + 1
          time_axis  (ireco) = (real(nyear-ibyear,8) + (real(n,8) - 0.5d0) / real(nrecs_per_year,8)) * nd_year
          time_axis_b(ireco) = (real(nyear-ibyear,8) + (real(n,8) - 1.0d0) / real(nrecs_per_year,8)) * nd_year
          time_axis_e(ireco) = (real(nyear-ibyear,8) + (real(n,8) - 0.0d0) / real(nrecs_per_year,8)) * nd_year
        end do
      end do
    end if

    if (intv_indx == 3) then
      nd_year = 365
      ireco = 0
      do nyear = ibyear, ieyear
        do n = 1, nrecs_per_year
          ireco = ireco + 1
          time_axis  (ireco) = (real(nyear-ibyear,8) + (real(n,8) - 0.5d0) / real(nrecs_per_year,8)) * nd_year
          time_axis_b(ireco) = (real(nyear-ibyear,8) + (real(n,8) - 1.0d0) / real(nrecs_per_year,8)) * nd_year
          time_axis_e(ireco) = (real(nyear-ibyear,8) + (real(n,8) - 0.0d0) / real(nrecs_per_year,8)) * nd_year
        end do
      end do
    end if

  end if

  !----------------------------------------------
  !
  write(6,*) ' model information '

  ! read topography of the model

  write(6,*) ' topography '

  call read_topo(file_topo)

  ! モデル水平格子情報定義

  write(6,*) ' grid points '

  call set_hgrids(file_vgrid)

  !  スケールファクタの読み込み

  write(6,*) ' scale factor '

  call read_scale(file_scale)

  !------------------------------------------

  write(6,*) ' masking information '

  if (l_mask_out) then

    !  basinインデックス読み込み

    allocate(ibas_u(1:imut,1:jmut))
    allocate(mask(1:imut,1:jmut,1:nlvls))
    allocate(volm(1:imut,1:jmut,1:nlvls))

    write(fmt_ibas,"(a,i4,a)") "(i6,",imut,"i1)"
    open(mtbas, file=file_basin, form="formatted")  
    do j = jmut, 1, -1
      read(mtbas, fmt=fmt_ibas,iostat=ios) jj,(ibas_u(i,j), i=1, imut)
      if(ios /= 0) write(*, *) 'reading error in file:', trim(file_basin)
      if ( jj /= j ) then
        print *,' error in ',trim(file_basin)
        stop 999
      end if
    end do
    close(mtbas)

    if (tuw == 't' .or. tuw == 'w') then

      allocate(ibas_t(1:imut,1:jmut))

      ibas_t(1:imut,1:jmut) = 0

      mask(1:imut,1:jmut,1:nlvls) = 0.0d0

      do j = 2, jmut
        do i = 2, imut
          ibas_t(i,j) = max(ibas_u(i-1,j-1),ibas_u(i,j-1),ibas_u(i-1,j),ibas_u(i,j))
        end do
      end do
#ifdef OGCM_CYCLIC
      ibas_t(1,1:jmut) = ibas_t(imut-4,1:jmut)
#endif /* OGCM_CYCLIC */

      do k = 1, nlvls
        do j = 1, jmut
          do i = 1, imut
            mask(i,j,k) = real(ibas_t(i,j),4) * atexl(i,j,k)
          end do
        end do
      end do

      if (l_lev_model) then
        
        call set_volt

        volm(1:imut,1:jmut,1:nlvls) = 0.0

        do j = 1, jmut
          do i = 1, imut
            do k = 1, texnn(i,j)
              volm(i,j,k) = real(volt(i,j,k),4) * 1.0e-6
            end do
          end do
        end do

#ifdef OGCM_BBL
        do j = 1, jmut
          do i = 1, imut
            if (texnnbbl(i,j) > 0) then
              volm(i,j,km) = real(volt(i,j,k),4) * 1.0e-6
            end if
          end do
        end do
#endif /* OGCM_BBL */

      else

        call set_area_t

        allocate(volm(1:imut,1:jmut,1:nlvls))
        volm(1:imut,1:jmut,1:nlvls) = 0.0

        do k = 1, nlvls
          do j = 1, jmut
            do i = 1, imut
              volm(i,j,k) = real(areat(i,j,k),4) * 1.0e-4
            end do
          end do
        end do

      end if

    else

      mask(1:imut,1:jmut,1:nlvls) = 0.0d0

      do k = 1, nlvls
        do j = 1, jmut
          do i = 1, imut
            mask(i,j,k) = real(ibas_u(i,j),4) * aexl(i,j,k)
          end do
        end do
      end do

      if (l_lev_model) then

        volm(1:imut,1:jmut,1:nlvls) = 0.0

        call set_volu
        
        do j = 1, jmut
          do i = 1, imut
            do k = 1, exnn(i,j)
              volm(i,j,k) = real(volu(i,j,k),4) * 1.0e-6
            end do
          end do
        end do

#ifdef OGCM_BBL
        do j = 1, jmut
          do i = 1, imut
            if (exnnbbl(i,j) > 0) then
              volm(i,j,km) = real(volu(i,j,k),4) * 1.0e-6
            end if
          end do
        end do
#endif /* OGCM_BBL */

      else

        call set_area_u

        volm(1:imut,1:jmut,1:nlvls) = 0.0

        do k = 1, nlvls
          do j = 1, jmut
            do i = 1, imut
              volm(i,j,k) = real(areau(i,j,k),4) * 1.0e-4
            end do
          end do
        end do

      end if
      
    end if

  end if

  !------------------------------------------

  allocate(dat4   (1:nlons,1:nlats,1:nlvls))
  allocate(var_out(1:nlons,1:nlats,1:nlvls,1:num_var_all))
  allocate(dlon(1:nlons), dlat(1:nlats), depth(1:nlvls))

  if (l_lon_model) then
    if (tuw=='t') then
      dlon(1:nlons) = alont(1:nlons) + lon_off
    end if
    if (tuw=='u') then
      dlon(1:nlons) = alonu(1:nlons) + lon_off
    end if
  else
    do i = 1, nlons
      dlon(i) = lon_first + delta_lon * real((i-1),8)
    end do
  end if

  if (l_lat_model) then
    if (tuw=='t') then
      dlat(1:nlats) = alatt(1:nlats) + lat_off
    end if
    if (tuw=='u') then
      dlat(1:nlats) = alatu(1:nlats) + lat_off
    end if
  else
    do j = 1, nlats
      dlat(j) = lat_first + delta_lat * real((j-1),8)
    end do
  end if

  if (l_lev_model) then
    if (tuw=='u' .or. tuw=='t') then
      depth(1:nlvls) = dp(1:nlvls)
    end if
    if (tuw=='w') then
      depth(1:nlvls) = dep(2:km+1)
    end if
  else
    depth(1:nlvls) = zlev(1:nlvls)
  end if

  !------------------------------------------------------

  !if (irec == 1) then
  !  l_initialize = .true.
  !else
  !  l_initialize = .false.
  !end if

  call netCDF_write__create_file (              &
         &   nlons, nlats, nlvls,               &
         &   dlat, dlon, depth,                 &
         &   file_out,                          &
         &   num_var_all,  vname, vunit, vlongname,   &
         &   undef_out,                         &
         &   LAT_UNITS, LON_UNITS, LVL_UNITS,   &
         &   REC_UNITS,                         &
         &   l_leap )

  !------------------------------------------------------

  ireco = 0

  loop_year: do nyear = ibyear, ieyear

    if (l_one_data) then
      write(flin,'(1a)') trim(file_in)
    else
      write(6,*) nyear
      if (l_append_ext) then
        write(flin,'(1a,i4.4,1a)') trim(file_in),nyear,trim(cext)
      else
        write(flin,'(1a,i4.4)') trim(file_in),nyear
      end if
    end if

    open(mtin, file=flin, form='unformatted', access='direct', recl=nlons*nlats*nlvls*4)
    write(6,*) 'reading from ...', trim(flin)

    do m = 1, nrecs_per_year

      ireco = ireco + 1

      do n = 1, num_var_out

        ireci = num_vars * (m - 1) + nth_place(n)

        write(6,*) ireci, nlons, nlats, nlvls, ireco

        read(mtin,rec=ireci) dat4

        do k = 1, nlvls
          do j = 1, nlats
            do i = 1, nlons
              if (dat4(i,j,k) /= undef_in(n)) then
                dat4(i,j,k) = rconv(n) * dat4(i,j,k)
              end if
            end do
          end do
        end do
        
        do k = 1, nlvls
          do j = 1, nlats
            do i = 1, nlons
              if ( dat4(i,j,k) /= undef_in(n) ) then
                var_out(i,j,k,n) = min(max(dat4(i,j,k),rmin(n)),rmax(n))
              else
                var_out(i,j,k,n) = undef_out(n)
              end if
            end do
          end do
        end do
      end do
      
      if (l_mask_out) then

        do k = 1, nlvls
          do j = 1, nlats
            do i = 1, nlons
              var_out(i,j,k,num_var_out+1) = mask(i,j,k)
              var_out(i,j,k,num_var_out+2) = volm(i,j,k)
            end do
          end do
        end do

      end if

      call netCDF_write__var_4d(                  &
           &   var_out, ireco,                    &
           &   nrecs, time_axis, time_axis_b, time_axis_e,  &
           &   file_out,                          &
           &   num_var_all, vname,                &
           &   nlons, nlats, nlvls )

    end do

    close(mtin)

  end do loop_year

  call write_ctl_for_xdfopen(            &
       & file_ctl, file_out, l_leap,     &
       & ireco, time_start, time_intv    &
       & )

contains
  !--------------------------------------------------
  subroutine write_ctl_for_xdfopen(       &
       & file_ctl, file_out, l_leap,      &
       & num_data, time_start, time_intv  &
       & )

    implicit none

    character(len=*), intent(in) :: file_ctl
    character(len=*), intent(in) :: file_out
    character(len=*), intent(in) :: time_start
    character(len=*), intent(in) :: time_intv
    logical, intent(in)    :: l_leap
    integer(4), intent(in) :: num_data
    integer(4), parameter  :: mtctl = 91

    !--------------------------------------------------

    open(mtctl,file=file_ctl)

    write(mtctl,'(1a,1a)') 'DSET ^',trim(file_out)
    write(mtctl,'(1a)') 'DTYPE netcdf'
    if (.not. l_leap) then
      write(mtctl,'(1a)') 'OPTIONS 365_day_calendar'
    end if
    write(mtctl,'(1a,i6,1a,1a,1a,1a)') 'TDEF time ',num_data,' LINEAR ', &
         & trim(time_start),' ',trim(time_intv)

    close(mtctl)

  end subroutine write_ctl_for_xdfopen

end program grads_to_netCDF_general
