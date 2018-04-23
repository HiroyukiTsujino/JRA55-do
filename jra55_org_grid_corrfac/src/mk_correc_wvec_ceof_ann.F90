!-*-F90-*-
program calc_wind_ceof_annual

  use libmxe_para, only: pi, radian, radian_r, radius
  use file_open_close_manager

  implicit none

  integer(4) :: imut, jmut, km
  integer(4) :: lreclen_in
  integer(4) :: lreclen_out

  integer(4) :: ibu, ieu, jbu, jeu

  integer(4) :: i1, i2, j1, j2
  integer(4) :: ibyr, ieyr
  integer(4) :: ibmn, iemn
  integer(4) :: iy1, iy2
  integer(4) :: mst, med, idn
  integer(4) :: yst(12), yed(12)

  complex(8),allocatable :: model_wind(:,:), obs_wind(:,:)

  real(8),allocatable :: eigen_mu(:), eigen_ou(:)
  real(8),allocatable :: eigen_mv(:), eigen_ov(:)
  real(8),allocatable :: corr_mag(:)
  real(8),allocatable :: corr_rot(:)
  real(8),allocatable :: corr_rot_org(:)
  real(8),allocatable :: angle_model(:)
  real(8),allocatable :: angle_obs(:)
  real(8),allocatable :: ene_ratio(:)
  real(8),allocatable :: var_ratio(:)
 
  real(4),allocatable :: worku(:), workv(:), work4(:)
  real(8),allocatable :: model_u10m(:), model_v10m(:)
  real(8),allocatable :: data_mask(:)
  integer(4),allocatable :: nvalid(:)

  ! for interpolation

  real(8),allocatable :: data_correc_latlon(:,:)
  real(8),allocatable :: data_correc_org_latlon(:,:)
  real(8),allocatable :: data_ene_ratio_latlon(:,:)
  real(8),allocatable :: data_var_ratio_latlon(:,:)
  real(8),allocatable :: data_org1(:), data_new1(:)
  real(8),allocatable :: data_org2(:), data_new2(:)
  real(8),allocatable :: data_org3(:), data_new3(:)
  real(8),allocatable :: data_org4(:), data_new4(:)
  real(8),allocatable :: mask_org(:)
  real(8),allocatable :: lon_org(:), lon_new(:)
  real(8) :: dlon, dlon_rg
  real(8) :: weight
  integer(4) :: ii

  integer(4) :: irecu_obs
  integer(4) :: irecv_obs

  real(4) :: undef_model, undef_obs, undef_out

  integer(4) :: m, n, i, j, k, iy, im

  integer(4),parameter :: nmonyr = 12
  integer(4) :: ndy(nmonyr) = (/31,29,31,30,31,30,31,31,30,31,30,31/)
  
  real(8) :: total_area, sumh, ave_tarea
  real(8) :: model_vecmag

  integer(4) :: mtin1 = 71, mtin2 = 72, mtin3 = 73, mtin4 = 74
  integer(4) :: mtot1 = 81, mtot2 = 82, mtot3 = 83, mtot4 = 84
  character(256) :: flnin_base1, flnin_base2, flnin_base3, flnin_base4
  character(256) :: flnot_base1, flnot_base2, flnot_base3, flnot_base4
  character(256) :: flnin1, flnin2, flnin3, flnin4
  character(256) :: flnot1, flnot2, flnot3, flnot4
  character(256) :: file_mask

  integer(4) :: total_grid_1d
  integer(4),allocatable :: num_xgrid(:)
  character(256) :: grid_name
  integer(4) :: ibgn, iend, i0

  integer(4),allocatable :: n_valid(:)

  ! for lapack

  complex(8) :: a(2,2), b(2,2), c(2,2) ! covariance matrix
  complex(8) :: check_orth

  integer(4) :: itype, ndim, lda, ldb, lwork, info
  character(1) :: jobz, uplo
  real(8) :: w(2)
  complex(8),allocatable :: work(:)
  real(8),allocatable :: rwork(:)
  logical :: l_check
  real(8) :: ratio_explain
  real(8) :: tanh_half

  !----------------------------------------------

  namelist /nml_wind_ceof_ann/ &
       & flnin_base1, &
       & flnin_base2, &
       & flnin_base3, &
       & irecu_obs, &
       & flnin_base4, &
       & irecv_obs, &
       & undef_model, undef_obs, &
       & file_mask, &
       & flnot1, flnot2, flnot3, &
       & undef_out, &
       & ibyr, ieyr, ibmn, iemn, &
       & imut, jmut, dlon, grid_name, &
       & l_check, &
       & ratio_explain, tanh_half

  !-----------------------------------------------------------------------

  l_check = .false.
  ratio_explain = 0.95d0
  tanh_half = 1.0d2
  open (11,file='namelist.windcorrec_ceof_ann')
  read (11,nml=nml_wind_ceof_ann)
  close(11)

  !-----------------------------------------------------------------------
  ! set grid points

  allocate(num_xgrid(1:jmut))
  allocate(data_correc_latlon(1:imut,1:jmut))
  allocate(data_correc_org_latlon(1:imut,1:jmut))
  allocate(data_ene_ratio_latlon(1:imut,1:jmut))
  allocate(data_var_ratio_latlon(1:imut,1:jmut))
  allocate(data_new1(1:imut))
  allocate(data_new2(1:imut))
  allocate(data_new3(1:imut))
  allocate(data_new4(1:imut))
  allocate(lon_new(1:imut))

  do i = 1, imut
    lon_new(i) = dlon * real(i-1,8)
  end do

  call set_reduced_grid(grid_name,num_xgrid,jmut,total_grid_1d)

  lreclen_in = 4 * total_grid_1d
  lreclen_out = 4 * total_grid_1d

  allocate(work4(1:total_grid_1d))
  allocate(model_u10m(1:total_grid_1d))
  allocate(model_v10m(1:total_grid_1d))
  allocate(data_mask(1:total_grid_1d))
  allocate(worku(1:total_grid_1d), workv(1:total_grid_1d))
  allocate(nvalid(1:total_grid_1d))

  ! read land-sea mask

  call open_file_direct(mtin1,file_mask,lreclen_in,convert_mode='little_endian',action='read')
  read(mtin1,rec=1) work4
  data_mask(1:total_grid_1d) = real(work4(1:total_grid_1d),8)
  call close_file(mtin1)

  do n = 1, total_grid_1d
    data_mask(n) = 1.d0 - data_mask(n) ! 0 for land, 1 for water
  end do

  deallocate(work4)

  !--------------------------------------------------------
  ! set data size

  yst(:) = 0
  yed(:) = 0

  do m = 1, ibmn - 1
    yst(m) = ibyr + 1
  end do
  do m = ibmn, 12
    yst(m) = ibyr
  end do
  do m = 1, iemn
    yed(m) = ieyr
  end do
  do m = iemn+1, 12
    yed(m) = ieyr - 1
  end do

  allocate(n_valid(total_grid_1d))

  allocate(eigen_mu(total_grid_1d), eigen_mv(total_grid_1d))
  allocate(eigen_ou(total_grid_1d), eigen_ov(total_grid_1d))
  allocate(corr_mag(total_grid_1d), corr_rot(total_grid_1d))
  allocate(corr_rot_org(total_grid_1d))
  allocate(angle_model(total_grid_1d), angle_obs(total_grid_1d))
  allocate(ene_ratio(total_grid_1d))
  allocate(var_ratio(total_grid_1d))

  write(6,*) ' Data allocation O.K. '

  !--------------------------------------------------------

  write(6,*) ' Correction factor of speed     = ',trim(flnot1)
  call open_file_direct(mtot1,flnot1,lreclen_out,convert_mode='little_endian',action='write')

  write(6,*) ' Correction factor of direction = ',trim(flnot2)
  call open_file_direct(mtot2,flnot2,lreclen_out,convert_mode='little_endian',action='write')

  write(6,*) ' Correction factor of direction (latlon) = ',trim(flnot3)
  call open_file_direct(mtot3,flnot3,4*imut*jmut,action='write')

  idn = 0
  do n = ibyr, ieyr
    mst = 1
    med = 12
    if (n == ibyr) mst = ibmn
    if (n == ieyr) med = iemn
    do m = mst, med
      idn = idn + 1
    end do
  end do

  write(6,*) ' idn = ', idn

  allocate(model_wind(total_grid_1d,idn), obs_wind(total_grid_1d,idn))

  n = 0
  nvalid(1:total_grid_1d) = 0

  do iy = ibyr, ieyr

    mst = 1
    med = 12

    if (iy == ibyr) mst = ibmn
    if (iy == ieyr) med = iemn

    do im = mst, med

      write(6,*) 'year = ', iy, ' month = ', im, ' idn = ', idn

      n = n + 1

      model_u10m(1:total_grid_1d) = 0.d0
      model_v10m(1:total_grid_1d) = 0.d0

      ! reanalysis

      write(flnin1,'(1a,i4.4,i2.2)') trim(flnin_base1),iy,im
      write(6,*) 'opening ',trim(flnin1)
      call open_file_direct(mtin1,flnin1,lreclen_in, &
           & convert_mode='little_endian',action='read')
      read(mtin1,rec=1) worku
      call close_file(mtin1)

      write(flnin2,'(1a,i4.4,i2.2)') trim(flnin_base2),iy,im
      write(6,*) 'opening ',trim(flnin2)
      call open_file_direct(mtin2,flnin2,lreclen_in, &
           & convert_mode='little_endian',action='read')
      read(mtin2,rec=1) workv
      call close_file(mtin2)

      do i = 1, total_grid_1d
        if ((worku(i) /= undef_model) .and. (workv(i) /= undef_model)) then
          model_wind(i,n) = cmplx(real(worku(i),8),real(workv(i),8))
          model_u10m(i) = model_u10m(i) + real(worku(i),8)
          model_v10m(i) = model_v10m(i) + real(workv(i),8)
          nvalid(i) = nvalid(i) + 1
        else
          model_wind(i,n) = cmplx(0.0d0,0.0d0)
        end if
      end do

      ! observation

      write(flnin3,'(1a,i4.4,i2.2)') trim(flnin_base3),iy,im
      write(6,*) 'opening ',trim(flnin3)
      call open_file_direct(mtin3,flnin3,lreclen_in, &
           & convert_mode='little_endian',action='read')
      read(mtin3,rec=irecu_obs) worku
      call close_file(mtin3)

      write(flnin4,'(1a,i4.4,i2.2)') trim(flnin_base4),iy,im
      write(6,*) 'opening ',trim(flnin4)
      call open_file_direct(mtin4,flnin4,lreclen_in, &
           & convert_mode='little_endian',action='read')
      read(mtin4,rec=irecv_obs) workv
      call close_file(mtin4)

      do i = 1, total_grid_1d
        if ((worku(i) /= undef_obs) .and. (workv(i) /= undef_obs)) then
          obs_wind(i,n) = cmplx(real(worku(i),8),real(workv(i),8))
        else
          obs_wind(i,n) = cmplx(0.0d0,0.0d0)
        end if
      end do
    end do
  end do

  do i = 1, total_grid_1d
    if (nvalid(i) == idn) then
      model_u10m(i) = model_u10m(i) / real(nvalid(i),8)
      model_v10m(i) = model_v10m(i) / real(nvalid(i),8)
    else
      model_u10m(i) = 0.0d0
      model_v10m(i) = 0.0d0
    end if
  end do

  !-----------------------------------------------------

  write(6,*) ' calculation of co-variance matrix '

  a(1:2,1:2) = cmplx(0.d0, 0.d0)
  b(1:2,1:2) = cmplx(0.d0, 0.d0)
  b(1,1) = cmplx(1.d0, 0.d0)
  b(2,2) = cmplx(1.d0, 0.d0)

  itype = 1
  ndim = 2
  lda = 2
  ldb = 2
  lwork = 2*ndim - 1
  jobz='V'
  uplo='U'
  allocate(work(1:lwork))
  allocate(rwork(1:3*ndim-2))
    
  n_valid(:) = 0

  do i = 1, total_grid_1d
    do n = 1, idn
      if ((abs(model_wind(i,n)) > 0.0d0) .and. (abs(obs_wind(i,n)) > 0.0d0)) then
        n_valid(i) = n_valid(i) + 1
        a(1,1) = a(1,1) + conjg(model_wind(i,n)) * model_wind(i,n)
        a(1,2) = a(1,2) + conjg(obs_wind(i,n)) * model_wind(i,n)
        a(2,1) = a(2,1) + conjg(model_wind(i,n)) * obs_wind(i,n)
        a(2,2) = a(2,2) + conjg(obs_wind(i,n)) * obs_wind(i,n)
      end if
    end do
    
    if (n_valid(i) > 0) then
      a(1,1) = a(1,1) / real(n_valid(i),8)
      a(2,1) = a(2,1) / real(n_valid(i),8)
      a(1,2) = a(1,2) / real(n_valid(i),8)
      a(2,2) = a(2,2) / real(n_valid(i),8)
      
      if (l_check) then
        write(6,*) ' a(1,1) = ', a(1,1)
        write(6,*) ' a(2,1) = ', a(2,1)
        write(6,*) ' a(1,2) = ', a(1,2)
        write(6,*) ' a(2,2) = ', a(2,2)
      end if
      
      c(1:2,1:2) = a(1:2,1:2)
      
      call zhegv(itype,jobz,uplo,ndim,a,lda,b,ldb,w,work,lwork,rwork,info)
      
      if (l_check) then
        write(6,*) info
        write(6,*) ' MODE 1'
        write(6,*) ' w(1) = ', w(1)
        write(6,*) ' a(1,1) = ', a(1,1)
        write(6,*) ' a(2,1) = ', a(2,1)
        write(6,*) ' MODE 2'
        write(6,*) ' w(2) = ', w(2)
        write(6,*) ' a(1,2) = ', a(1,2)
        write(6,*) ' a(2,2) = ', a(2,2)
        
        write(6,*) ' orthogonality check '
        check_orth = conjg(a(1,1)) * a(1,2) + conjg(a(2,1)) * a(2,2)
        write(6,*) check_orth
        
        !---
        
        write(6,*) ' MODE 1'
        check_orth = c(1,1) * a(1,1) + c(1,2) * a(2,1)
        write(6,*) check_orth / w(1)
        
        check_orth = c(2,1) * a(1,1) + c(2,2) * a(2,1)
        write(6,*) check_orth / w(1)
        
        !---
        
        write(6,*) ' MODE 2'
        check_orth = c(1,1) * a(1,2) + c(1,2) * a(2,2)
        write(6,*) check_orth / w(2)
        
        check_orth = c(2,1) * a(1,2) + c(2,2) * a(2,2)
        write(6,*) check_orth / w(2)
        
      end if
      
      eigen_mu(i) = real (a(1,2))
      eigen_mv(i) = aimag(a(1,2))
      eigen_ou(i) = real (a(2,2))
      eigen_ov(i) = aimag(a(2,2))
      
      corr_mag(i) = abs(a(2,2)) / abs(a(1,2))
      
      if (eigen_ou(i) /= 0.0d0) then
        angle_obs(i) = 180.d0 / pi * atan(eigen_ov(i)/eigen_ou(i)) ! [-90, 90]
        if ((eigen_ou(i) < 0.0d0) .and. (eigen_ov(i) > 0.0d0)) then
          angle_obs(i) = angle_obs(i) + 180.0d0
        end if
        if ((eigen_ou(i) < 0.0d0) .and. (eigen_ov(i) < 0.0d0)) then
          angle_obs(i) = angle_obs(i) - 180.0d0
        end if
      else
        if (eigen_ov(i) > 0.0d0) then
          angle_obs(i) = 90.0d0
        else
          angle_obs(i) = -90.0d0
        end if
      end if
      
      if (eigen_mu(i) /= 0.0d0) then
        angle_model(i) = 180.d0 / pi * atan(eigen_mv(i)/eigen_mu(i))
        if ((eigen_mu(i) < 0.0d0) .and. (eigen_mv(i) > 0.0d0)) then
          angle_model(i) = angle_model(i) + 180.0d0
        end if
        if ((eigen_mu(i) < 0.0d0) .and. (eigen_mv(i) < 0.0d0)) then
          angle_model(i) = angle_model(i) - 180.0d0
        end if
      else
        if (eigen_mv(i) > 0.0d0) then
          angle_model(i) = 90.0d0
        else
          angle_model(i) = -90.0d0
        end if
      end if
          
      corr_rot(i) = angle_obs(i) - angle_model(i)
      if (corr_rot(i) > 180.0d0) then
        corr_rot(i) = corr_rot(i) - 360.0d0
      end if
      if (corr_rot(i) < -180.0d0) then
        corr_rot(i) = 360.0d0 + corr_rot(i)
      end if
          
      ene_ratio(i) = abs(w(2)) / (abs(w(1)) + abs(w(2)))
      var_ratio(i) = w(2) / (w(1) + w(2))

      corr_rot_org(i) = corr_rot(i)
      ! smooth transition
      corr_rot(i) = corr_rot(i) * 0.5d0 * (1.0d0 + tanh(tanh_half*(ene_ratio(i)-ratio_explain))) 

      if (data_mask(i) == 0.0d0) then
        corr_mag(i) = 1.0d0
        corr_rot(i) = 0.0d0
      end if

      if (l_check) then
        write(6,'(1a,2f8.2,2f10.3)') ' location mag ang', i, corr_mag(i), corr_rot(i)
      end if

    else
      corr_mag(i) = undef_out
      corr_rot(i) = undef_out
      corr_rot_org(i) = undef_out
      ene_ratio(i) = undef_out
      var_ratio(i) = undef_out
    end if

  end do

  deallocate(work)
  deallocate(rwork)

  write(mtot1,rec=1) real(corr_mag(1:total_grid_1d),4)
  write(mtot2,rec=1) real(corr_rot(1:total_grid_1d),4)
  write(mtot2,rec=2) real(corr_rot_org(1:total_grid_1d),4)
  write(mtot2,rec=3) real(ene_ratio(1:total_grid_1d),4)
  write(mtot2,rec=4) real(var_ratio(1:total_grid_1d),4)

  deallocate(model_wind, obs_wind)

  !-----------------------------------------------------------
  ! reduced grid to lat-lon grid for check

  i0 = 0
  
  do j = 1, jmut

    ibgn = i0 + 1
    iend = i0 + num_xgrid(j)

    if (num_xgrid(j) == imut) then
      
      data_correc_latlon    (1:imut,jmut-j+1) = corr_rot    (ibgn:iend)
      data_correc_org_latlon(1:imut,jmut-j+1) = corr_rot_org(ibgn:iend)
      data_ene_ratio_latlon  (1:imut,jmut-j+1) = ene_ratio   (ibgn:iend)
      data_var_ratio_latlon  (1:imut,jmut-j+1) = var_ratio   (ibgn:iend)
      
    else
      
      allocate(data_org1(1:num_xgrid(j)+1))
      allocate(data_org2(1:num_xgrid(j)+1))
      allocate(data_org3(1:num_xgrid(j)+1))
      allocate(data_org4(1:num_xgrid(j)+1))
      allocate(lon_org (1:num_xgrid(j)+1))
      
      data_org1(1:num_xgrid(j)) = corr_rot(ibgn:iend)
      data_org1(num_xgrid(j)+1) = corr_rot(ibgn)

      data_org2(1:num_xgrid(j)) = corr_rot_org(ibgn:iend)
      data_org2(num_xgrid(j)+1) = corr_rot_org(ibgn)

      data_org3(1:num_xgrid(j)) = ene_ratio(ibgn:iend)
      data_org3(num_xgrid(j)+1) = ene_ratio(ibgn)
      
      data_org4(1:num_xgrid(j)) = var_ratio(ibgn:iend)
      data_org4(num_xgrid(j)+1) = var_ratio(ibgn)
      
      dlon_rg = 360.0 / real(num_xgrid(j),8)
      
      do ii = 1, num_xgrid(j) + 1
        lon_org(ii) = dlon_rg * real(ii-1,8)
      end do
      
      do i = 1, imut
        do ii = 1, num_xgrid(j)
          if (abs(lon_org(ii) - lon_new(i)) < 1.0d-4) then
            data_new1(i) = data_org1(ii)
            data_new2(i) = data_org2(ii)
            data_new3(i) = data_org3(ii)
            data_new4(i) = data_org4(ii)
            exit
          else if ( (lon_org(ii) < lon_new(i)) .and. (lon_new(i) <= lon_org(ii+1)) ) then
            weight = (lon_new(i) - lon_org(ii)) / (lon_org(ii+1) - lon_org(ii))
            data_new1(i) = (1.0d0 - weight) * data_org1(ii) + weight * data_org1(ii+1)
            data_new2(i) = (1.0d0 - weight) * data_org2(ii) + weight * data_org2(ii+1)
            data_new3(i) = (1.0d0 - weight) * data_org3(ii) + weight * data_org3(ii+1)
            data_new4(i) = (1.0d0 - weight) * data_org4(ii) + weight * data_org4(ii+1)
            exit
          end if
        end do
      end do
      data_correc_latlon(1:imut,jmut-j+1) = data_new1(1:imut)
      data_correc_org_latlon(1:imut,jmut-j+1) = data_new2(1:imut)
      data_ene_ratio_latlon(1:imut,jmut-j+1) = data_new3(1:imut)
      data_var_ratio_latlon(1:imut,jmut-j+1) = data_new4(1:imut)

      deallocate(lon_org)
      deallocate(data_org4)
      deallocate(data_org3)
      deallocate(data_org2)
      deallocate(data_org1)
    end if
    i0 = iend
  end do
  
  write(mtot3,rec=1) real(data_correc_latlon,4)
  write(mtot3,rec=2) real(data_correc_org_latlon,4)
  write(mtot3,rec=3) real(data_ene_ratio_latlon,4)
  write(mtot3,rec=4) real(data_var_ratio_latlon,4)

  call close_file(mtot3)
  call close_file(mtot2)
  call close_file(mtot1)

end program calc_wind_ceof_annual
