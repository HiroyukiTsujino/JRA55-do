! -*-F90-*-
!==================================================================
program read_netcdf_output_grads
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of
  !     the CERES radiation
  !----------------------------------------------------------------

  implicit none

  include '/usr/local/netcdf/include/netcdf.inc'

  integer(4), parameter :: imut = 360, jmut = 180
  integer(4), parameter :: mtot1 = 91, mtot2 = 92
  integer(4), parameter :: mtin1 = 81, mtin2 = 82

  real(4) :: dat4(imut,jmut)
  real(8) :: dat8(imut,jmut)

  real(4) :: swdn(imut,jmut)
  real(4) :: lwdn(imut,jmut)
  real(4) :: swup(imut,jmut)
  real(4) :: lwup(imut,jmut)
  real(4) :: work4(imut,jmut)
  real(4) :: reduction_factor(imut,jmut)
  real(4) :: reduction_factor_tmp(0:imut+1,jmut)
  real(4) :: mask_sea(imut,jmut)

  real(4) :: hl1, hl2

  integer(4) :: idmon(12)

  character(128) :: flot1, flot2

  integer(4) :: i, j, k, l, m, n

  integer(4) :: nmocount
  integer(4) :: ireco1, ireco2
  integer(4) :: nyear
  integer(4) :: mst, med


  character(128) :: flin_ice
  real(4) :: ice(imut,jmut)
  real(4),parameter :: undef_ice = -9.99e33

  real(4),parameter :: undef_srb_sw = -999.e0
  real(4),parameter :: undef_srb_lw = -999.e0
  real(4),parameter :: undef_mxe = -9.99e33

  ! for netCDF

  integer(4), parameter :: nvars = 4, nfiles = 1
  integer(4) :: var(nvars), sta(nvars)
  integer(4) :: start(3), range(3)
  character(128) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  !--------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  !---------------------------------------------------------------------
  ! open netcdf file

  write(flnin(1),'(1a)') &
       & '/work116/htsujino/CERES/org_data/CERES_EBAF-Surface_Ed2.8_Subset_200003-201502.nc'

  do n = 1, nfiles
    sta(n) = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
    if (sta(n) /= 0) then
      write(6,*) 'nf_open error for file number ',n
      stop
    end if
  end do

  start(1:3) = (/ 1, 1, 1 /)
  range(1:3) = (/ 360, 180, 1 /)

  nmocount = 0

  do nyear = 2000, 2015

    mst = 1
    med = 12

    if (nyear == 2000) mst = 3
    if (nyear == 2015) med = 2

    do m = mst, med


      write(flin_ice,'(1a,i4.4,i2.2)') &
           & '/work116/htsujino/COBESST/monthly/grads/ice-glb.',nyear,m
      open(mtin1,file=flin_ice,form='unformatted',access='direct',action='read',recl=4*imut*jmut)
      write(6,*) 'Sea ice distribution read from ',trim(flin_ice)
      read(mtin1,rec=1) ice
      close(mtin1)

      sta(1) = nf_inq_varid(ifiles(1),'sfc_sw_down_all_mon',var(1))
      sta(2) = nf_inq_varid(ifiles(1),'sfc_sw_up_all_mon',  var(2))
      sta(3) = nf_inq_varid(ifiles(1),'sfc_lw_down_all_mon',var(3))
      sta(4) = nf_inq_varid(ifiles(1),'sfc_lw_up_all_mon',  var(4))

      do n = 1, nvars
        if (sta(n) /= 0) then
          write(6,*) 'nf_inq_varid error for variable ',n, var(n)
          stop
        end if
      end do

      nmocount = nmocount + 1

      start(3) = nmocount

      !---------------------------------------------------------------------------

      sta(1) = nf_get_vara_real(ifiles(1),var(1),start,range,dat4)

      do j = 1, jmut
        do i = 1, imut
          if (dat4(i,j) == undef_srb_sw) then
            swdn(i,j) = undef_mxe
          else
            swdn(i,j) = dat4(i,j)
          end if
        end do
      end do

      ! reduction by up to 10 % over seaice

      reduction_factor(:,:) = 1.0e0
      mask_sea(:,:) = 0.0e0

      do j = 1, jmut
        do i = 1, imut
          if (ice(i,j) /= undef_ice) then
            if (ice(i,j) > 0.0e0) then
              reduction_factor(i,j) = 1.0e0 - min(ice(i,j),1.0e0) * 0.1e0
            end if
            mask_sea(i,j) = 1.0e0
          end if
        end do
      end do

      ! filter on reduction factor

      reduction_factor_tmp(1:imut,1:jmut) = reduction_factor(1:imut,1:jmut)
      reduction_factor_tmp(     0,1:jmut) = reduction_factor_tmp(imut,1:jmut)
      reduction_factor_tmp(imut+1,1:jmut) = reduction_factor_tmp(   1,1:jmut)

      do j = 2, jmut - 1
        do i = 1, imut
          if (mask_sea(i,j) == 0.0) then
            hl1 = 4.0 * mask_sea(i,j) &
                 & + mask_sea(i+1,j) + mask_sea(i,j+1) &
                 & + mask_sea(i-1,j) + mask_sea(i,j-1) 
            hl2 = 4.0 * mask_sea(i,j) * reduction_factor_tmp(i,j) &
                 & + mask_sea(i+1,j) * reduction_factor_tmp(i+1,j)  &
                 & + mask_sea(i,j+1) * reduction_factor_tmp(i,j+1) &
                 & + mask_sea(i-1,j) * reduction_factor_tmp(i-1,j) &
                 & + mask_sea(i,j-1) * reduction_factor_tmp(i,j-1)
            if (hl1 > 0.0) then
              reduction_factor(i,j) = hl2 / hl1
            end if
          end if
        end do
      end do

      do j = 1, jmut
        do i = 1, imut
          if (swdn(i,j) /= undef_mxe) then
            swdn(i,j) = swdn(i,j) * reduction_factor(i,j)
          end if
        end do
      end do

      ! open output file

      write(flot1,'(1a,i4.4,i2.2)') &
           & '/work116/htsujino/CERES/grads/swdn_reduced_over_seaice.',nyear,m
      open(mtot1,file=flot1,form='unformatted',access='direct',recl=4*imut*jmut)
      write(6,*) 'Downward short wave written to ',trim(flot1)
      ireco1 = 0
      ireco1 = ireco1 + 1
      write(mtot1,rec=ireco1) swdn
      close(mtot1)

      !------------------------------------------------------------------------

!      sta(3) = nf_get_vara_real(ifiles(1),var(3),start,range,dat4)
!
!      do j = 1, jmut
!        do i = 1, imut
!          if (dat4(i,j) == undef_srb_lw) then
!            lwdn(i,j) = undef_mxe
!          else
!            lwdn(i,j) = dat4(i,j)
!          end if
!        end do
!      end do
!
!      ! open output file
!
!      write(flot1,'(1a,i4.4,i2.2)') &
!           & '/work116/htsujino/CERES/grads/lwdn_reduced_over_seaice.',nyear,m
!      open(mtot1,file=flot1,form='unformatted',access='direct',recl=4*imut*jmut)
!      write(6,*) 'Downward long wave written to ',trim(flot1)
!      ireco1 = 0
!      ireco1 = ireco1 + 1
!      write(mtot1,rec=ireco1) lwdn
!      close(mtot1)

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
