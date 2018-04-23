! -*-F90-*-
program make_table_reduced2regular

  use libmxe_para, only: pi, radian, radian_r, radius

  implicit none

  integer(4) :: imut, jmut
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: total_grid_1d

  integer(4) :: ibgn, iend, i0
  integer(4) :: ibgn_p, iend_p
  integer(4) :: ibgn_m, iend_m

  integer(4) :: nlink, num_link_to_add

  integer(4),allocatable :: isrc(:)
  integer(4),allocatable :: idst(:)
  real(8),allocatable    :: wgt(:)

  real(4),allocatable :: work4(:)
  real(8),allocatable :: data_in(:)
  real(8),allocatable :: data_cl(:)
  real(8),allocatable :: data_mask(:)

  real(8),allocatable :: mask_org(:), mask_new(:)
  real(8),allocatable :: lon_org(:), lon_new(:)
  real(8) :: dlon, dlon_rg, dlon_rg_m, dlon_rg_p
  real(8),allocatable :: lat_org(:)

  real(8),allocatable :: mask_org_p(:), mask_org_m(:)
  real(8),allocatable :: lon_org_p(:), lon_org_m(:)
  real(8),allocatable :: lat_p, lat_m

  character(256) :: file_mask
  character(256) :: file_table

  character(len=258) :: file_ydef
  logical :: l_simple_linear

  integer(4),parameter :: lun=10

  integer(4) :: i, j, ii, jj, n, ios
  integer(4) :: im, imm, jm, ip, ipp, jp
  real(8) :: weight, weight_total, data_tmp
  real(8) :: regular_lat, regular_lon
  real(8) :: dist_tmp
  real(8),parameter :: eps_dist = 1.0d-8

  integer(4) :: iii, m, m_left, m_right, i_left, i_right

  integer(4) :: nbyr, nbmn, neyr, nemn, month, nyear
  integer(4) :: nst, ned
       
  real(4) :: undef4_in, undef4_out
  real(8) :: undef8_in, undef8_out

  real(8) :: distance_rad  ! external function

  !---------------------------------------------

  namelist /nml_make_table/  &
       & file_mask,                   &
       & file_table,                  &
       & l_simple_linear,             &
       & imut, jmut, dlon, grid_name, &
       & file_ydef

  !---------------------------------------------

  l_simple_linear = .false.
  open(lun,file='namelist.make_table')
  read(lun,nml=nml_make_table)
  close(lun)

  !---------------------------------------------

  allocate(num_xgrid(1:jmut))
  allocate(lat_org(1:jmut))
  allocate(mask_new(1:imut))
  allocate(lon_new(1:imut))

  do i = 1, imut
    lon_new(i) = dlon * real(i-1,8)
  end do

  open(lun,file=file_ydef)
  do j = 1, jmut
    read(lun,*,iostat=ios) lat_org(j)
    if (ios /= 0) then
      write(6,*) ' Error : inconsistent number of data ', jmut
      stop
    else
      !write(6,*) lat_org(j)
    end if
  end do
  close(lun)

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  allocate(work4(1:total_grid_1d))

  allocate(data_mask(1:total_grid_1d))

  allocate(isrc(1:total_grid_1d*4))
  allocate(idst(1:total_grid_1d*4))
  allocate(wgt (1:total_grid_1d*4))

  !----------------------------------------------

  open(lun,file=file_mask,form='unformatted',access='direct', &
       & convert='little_endian',recl=4*total_grid_1d)
  read(lun,rec=1) work4
  data_mask(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  close(lun)
  do n = 1, total_grid_1d
    data_mask(n) = 1.0d0 - data_mask(n) ! 0 for land, 1 for water
  end do

  !-------------------------------------------------

  nlink = 0

  i0 = 0

  do j = 1, jmut

    regular_lat = lat_org(j)

    jp = min(j+1,jmut)
    jm = max(j-1,1)

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    if (j < jmut) then
      ibgn_p = i0 + num_xgrid(j) + 1
      iend_p = i0 + num_xgrid(j) + num_xgrid(j+1)
    else
      ibgn_p = ibgn
      iend_p = iend
    end if

    if (j > 1) then
      ibgn_m = i0 - num_xgrid(j-1) + 1
      iend_m = i0
    else
      ibgn_m = ibgn
      iend_m = iend
    end if

    if (num_xgrid(j) == imut) then

      do i = 1, imut
        nlink = nlink + 1
        isrc(nlink) = ibgn + i - 1
        idst(nlink) = imut * (jmut - j) + i
        wgt (nlink) = 1.0d0
      end do

    else

      if (l_simple_linear) then

        allocate(lon_org (1:num_xgrid(j)+1))
        allocate(mask_org(1:num_xgrid(j)+1))

        mask_org(1:num_xgrid(j)) = data_mask(ibgn:iend)
        mask_org(num_xgrid(j)+1) = data_mask(ibgn)

        dlon_rg = 360.0 / real(num_xgrid(j),8)
        do ii = 1, num_xgrid(j) + 1
          lon_org(ii) = dlon_rg * real(ii-1,8)
        end do

        do i = 1, imut
          regular_lon = lon_new(i)
          do ii = 1, num_xgrid(j)
            if (abs(lon_org(ii) - regular_lon) < 1.0d-4) then
              nlink = nlink + 1
              isrc(nlink) = ibgn + ii - 1
              idst(nlink) = imut * (jmut - j) + i
              wgt (nlink) = 1.0d0
              exit
            else if ( (lon_org(ii) < regular_lon) .and. (regular_lon < lon_org(ii+1)) ) then
              weight = (regular_lon - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
                
              nlink = nlink + 1
              isrc(nlink) = ibgn + ii - 1
              idst(nlink) = imut * (jmut - j) + i
              wgt (nlink) = 1.0d0 - weight
                
              nlink = nlink + 1
              if (ii == num_xgrid(j)) then
                isrc(nlink) = ibgn
                idst(nlink) = imut * (jmut - j) + i
                wgt (nlink) = weight
              else
                isrc(nlink) = ibgn + ii
                idst(nlink) = imut * (jmut - j) + i
                wgt (nlink) = weight
              end if

              exit
            end if
          end do
        end do

        deallocate(mask_org)
        deallocate(lon_org)

      else

        allocate(lon_org   (1:num_xgrid(j )+1))
        allocate(lon_org_p (1:num_xgrid(jp)+1))
        allocate(lon_org_m (1:num_xgrid(jm)+1))
        allocate(mask_org  (1:num_xgrid(j )+1))
        allocate(mask_org_p(1:num_xgrid(jp)+1))
        allocate(mask_org_m(1:num_xgrid(jm)+1))

        mask_org(1:num_xgrid(j)) = data_mask(ibgn:iend)
        mask_org(num_xgrid(j)+1) = data_mask(ibgn)

        mask_org_p(1:num_xgrid(jp)) = data_mask(ibgn_p:iend_p)
        mask_org_p(num_xgrid(jp)+1) = data_mask(ibgn_p)

        mask_org_m(1:num_xgrid(jm)) = data_mask(ibgn_m:iend_m)
        mask_org_m(num_xgrid(jm)+1) = data_mask(ibgn_m)

        dlon_rg = 360.0 / real(num_xgrid(j),8)
        do ii = 1, num_xgrid(j) + 1
          lon_org(ii) = dlon_rg * real(ii-1,8)
        end do

        dlon_rg_m = 360.0 / real(num_xgrid(jm),8)
        do ii = 1, num_xgrid(jm) + 1
          lon_org_m(ii) = dlon_rg_m * real(ii-1,8)
        end do

        dlon_rg_p = 360.0 / real(num_xgrid(jp),8)
        do ii = 1, num_xgrid(jp) + 1
          lon_org_p(ii) = dlon_rg_p * real(ii-1,8)
        end do

        do i = 1, imut
          regular_lon = lon_new(i)
          do ii = 1, num_xgrid(j)
            if (abs(lon_org(ii) - regular_lon) < 1.0d-4) then
              nlink = nlink + 1
              isrc(nlink) = ibgn + ii - 1
              idst(nlink) = imut * (jmut - j) + i
              wgt (nlink) = 1.0d0
              exit
            else if ( (lon_org(ii) < regular_lon) .and. (regular_lon < lon_org(ii+1)) ) then
              if (((mask_org(ii) + mask_org(ii+1)) == 2.0d0) &
                   & .or. ((mask_org(ii) + mask_org(ii+1)) == 0.0d0)) then
                
                weight = (regular_lon - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
                
                nlink = nlink + 1
                isrc(nlink) = ibgn + ii - 1
                idst(nlink) = imut * (jmut - j) + i
                wgt (nlink) = 1.0d0 - weight
                
                nlink = nlink + 1
                if (ii == num_xgrid(j)) then
                  isrc(nlink) = ibgn
                  idst(nlink) = imut * (jmut - j) + i
                  wgt (nlink) = weight
                else
                  isrc(nlink) = ibgn + ii
                  idst(nlink) = imut * (jmut - j) + i
                  wgt (nlink) = weight
                end if

              else ! land and water

                weight = (regular_lon - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
                mask_new(i) = (1.0d0 - weight) * mask_org(ii) + weight * mask_org(ii+1)
                if (mask_new(i) < 0.5d0) then ! closer to land (0.5 ==> water)
                  
                  nlink = nlink + 1
                  isrc(nlink) = ibgn + ii - 1
                  idst(nlink) = imut * (jmut - j) + i
                  wgt (nlink) = 1.0d0 - weight

                  nlink = nlink + 1
                  if (ii == num_xgrid(j)) then
                    isrc(nlink) = ibgn
                    idst(nlink) = imut * (jmut - j) + i
                    wgt (nlink) = weight
                  else
                    isrc(nlink) = ibgn + ii
                    idst(nlink) = imut * (jmut - j) + i
                    wgt (nlink) = weight
                  end if
                  
                else ! closer to water (0.5 ==> water)
                  
                  do ip = 1, num_xgrid(jp)
                    if ( (lon_org_p(ip) <= regular_lon) .and. &
                         & (regular_lon < lon_org_p(ip+1)) ) then
                      ipp = ip
                      exit
                    end if
                  end do
                  do im = 1, num_xgrid(jm)
                    if ( (lon_org_m(im) <= regular_lon) .and. &
                         & (regular_lon < lon_org_m(im+1)) ) then
                      imm = im
                      exit
                    end if
                  end do
                  
                  num_link_to_add = 0
                  weight_total = 0.0d0
                  
                  if (mask_org_m(imm) == 1.d0) then
                    dist_tmp = &
                         & distance_rad(lon_org_m(imm),lat_org(jm),regular_lon,regular_lat)
                    if (dist_tmp > radian_r * 1.5d0 * dlon_rg_m) then
                      write(6,*) ' erroneous distance (1): ', dist_tmp,radian_r * 1.5d0 * dlon_rg_m
                      write(6,*) lon_org_m(imm),lat_org(jm),regular_lon,regular_lat
                      stop
                    end if

                    nlink = nlink + 1
                    num_link_to_add = num_link_to_add + 1
                    weight = 1.0d0 / (dist_tmp + eps_dist)
                    weight_total = weight_total + weight

                    isrc(nlink) = ibgn_m + imm - 1
                    idst(nlink) = imut * (jmut - j) + i
                    wgt (nlink) = weight

                  end if

                  if (mask_org_m(imm+1) == 1.d0) then
                    dist_tmp = &
                         & distance_rad(lon_org_m(imm+1),lat_org(jm),regular_lon,regular_lat)
                    if (dist_tmp > radian_r * 1.5d0 * dlon_rg_m) then
                      write(6,*) ' erroneous distance (2): ', dist_tmp,radian_r * 1.5d0 * dlon_rg_m
                      write(6,*) lon_org_m(imm+1),lat_org(jm),regular_lon,regular_lat
                      stop
                    end if
                    nlink = nlink + 1
                    num_link_to_add = num_link_to_add + 1
                    weight = 1.0d0 / (dist_tmp + eps_dist)
                    weight_total = weight_total + weight

                    if (imm == num_xgrid(jm)) then
                      isrc(nlink) = ibgn_m
                      idst(nlink) = imut * (jmut - j) + i
                      wgt (nlink) = weight
                    else
                      isrc(nlink) = ibgn_m + imm
                      idst(nlink) = imut * (jmut - j) + i
                      wgt (nlink) = weight
                    end if

                  end if

                  if (mask_org(ii) == 1.d0) then
                    dist_tmp = distance_rad(lon_org(ii),lat_org(j),regular_lon,regular_lat)
                    if (dist_tmp > radian_r * 1.5d0 * dlon_rg) then
                      write(6,*) ' erroneous distance (3): ', dist_tmp,radian_r * 1.5d0 * dlon_rg
                      write(6,*) lon_org(ii),lat_org(j),regular_lon,regular_lat
                      stop
                    end if
                    nlink = nlink + 1
                    num_link_to_add = num_link_to_add + 1
                    weight = 1.0d0 / (dist_tmp + eps_dist)
                    weight_total = weight_total + weight

                    isrc(nlink) = ibgn + ii - 1
                    idst(nlink) = imut * (jmut - j) + i
                    wgt (nlink) = weight

                  end if

                  if (mask_org(ii+1) == 1.d0) then
                    dist_tmp = distance_rad(lon_org(ii+1),lat_org(j),regular_lon,regular_lat)
                    if (dist_tmp > radian_r * 1.5d0 * dlon_rg) then
                      write(6,*) ' erroneous distance (4): ', dist_tmp,radian_r * 1.5d0 * dlon_rg
                      write(6,*) lon_org(ii+1),lat_org(j),regular_lon,regular_lat
                      stop
                    end if
                    nlink = nlink + 1
                    num_link_to_add = num_link_to_add + 1
                    weight = 1.0d0 / (dist_tmp + eps_dist)
                    weight_total = weight_total + weight
                    
                    if (ii == num_xgrid(j)) then
                      isrc(nlink) = ibgn
                      idst(nlink) = imut * (jmut - j) + i
                      wgt (nlink) = weight
                    else
                      isrc(nlink) = ibgn + ii
                      idst(nlink) = imut * (jmut - j) + i
                      wgt (nlink) = weight
                    end if

                  end if

                  if (mask_org_p(ipp) == 1.d0) then
                    dist_tmp = &
                         & distance_rad(lon_org_p(ipp),lat_org(jp),regular_lon,regular_lat)
                    if (dist_tmp > radian_r * 1.5d0 * dlon_rg_p) then
                      write(6,*) ' erroneous distance (5): ', dist_tmp,radian_r * 1.5d0 * dlon_rg_p
                      write(6,*) lon_org_p(ipp),lat_org(jp),regular_lon,regular_lat
                      stop
                    end if
                    nlink = nlink + 1
                    num_link_to_add = num_link_to_add + 1
                    weight = 1.0d0 / (dist_tmp + eps_dist)
                    weight_total = weight_total + weight

                    isrc(nlink) = ibgn_p + ipp - 1
                    idst(nlink) = imut * (jmut - j) + i
                    wgt (nlink) = weight

                  end if
                  
                  if (mask_org_p(ipp+1) == 1.d0) then
                    dist_tmp = &
                         & distance_rad(lon_org_p(ipp+1),lat_org(jp),regular_lon,regular_lat)
                    if (dist_tmp > radian_r * 1.5d0 * dlon_rg_p) then
                      write(6,*) ' erroneous distance (6): ', dist_tmp, radian_r * 1.5d0 * dlon_rg_p
                      write(6,*) lon_org_p(ipp+1),lat_org(jp),regular_lon,regular_lat
                      stop
                    end if

                    nlink = nlink + 1
                    num_link_to_add = num_link_to_add + 1
                    weight = 1.0d0 / (dist_tmp + eps_dist)
                    weight_total = weight_total + weight
                    
                    if (ipp == num_xgrid(j)) then
                      isrc(nlink) = ibgn_p
                      idst(nlink) = imut * (jmut - j) + i
                      wgt (nlink) = weight
                    else
                      isrc(nlink) = ibgn_p + ipp
                      idst(nlink) = imut * (jmut - j) + i
                      wgt (nlink) = weight
                    end if
                    
                  end if

                  if (weight_total > 0.0d0) then
                    do n = nlink - num_link_to_add + 1, nlink
                      wgt(n) = wgt(n) / weight_total
                    end do
                  else
                    write(6,*) ' error ! all grids are land '
                    write(6,*) ii, num_xgrid(j), mask_org(ii), mask_org(ii+1), weight_total
                    write(6,*) lon_org(ii+1),lat_org(j),regular_lon,regular_lat
                    stop
                  end if

                end if
              end if
              exit
            end if
          end do
        end do

        deallocate(mask_org,mask_org_p,mask_org_m)
        deallocate(lon_org,lon_org_p,lon_org_m)

      end if
      
    end if

    i0 = iend

  end do


  write(6,*) ' mapping table written to ',trim(file_table)
  open (lun,file=file_table,form='unformatted')
  write(lun) nlink
  write(lun) isrc(1:nlink)
  write(lun) idst(1:nlink)
  write(lun) wgt(1:nlink)
  close(lun)

end program make_table_reduced2regular
