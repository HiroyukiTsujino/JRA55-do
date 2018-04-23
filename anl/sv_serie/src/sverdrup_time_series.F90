! -*-F90-*-
program sverdrup_time_series

  use libmxe_para, only: libmxe_para__register &
                     & , clen, pi, radian, radian_r, radius, rho, omega &
                     & , type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io
  use libmxe_calendar

  use file_open_close_manager
  
  implicit none

  !-------
  ! model

  integer(4) :: imt, jmt, kmt
  integer(4) :: lreclenm
  real(8),allocatable :: alonu(:), alatu(:)
  real(8),allocatable :: alont(:), alatt(:)
  real(8),allocatable :: aexl(:,:), atexl(:,:)

  real(8),allocatable :: sv(:,:)
  real(8),allocatable :: wsx(:,:), wsy(:,:), curltau(:,:)
  real(8),allocatable :: C_Rossby(:)
  real(8),allocatable :: beta(:)
  real(4),allocatable :: r2d(:,:)

  !-------
  ! data

  integer(4) :: imf, jmf, kmf
  integer(4) :: lreclend
  real(8),allocatable :: alonf(:), alatf(:)
  real(4),allocatable :: wsxf(:,:), wsyf(:,:)
  real(4) :: undef_data
  INTEGER(4) :: iwxfile = 11, iwyfile = 12

  !------

  real(8) :: hl1, hl2, hl3

  logical    :: l_intpl
  integer(4) :: intpmode = 2 
  integer(4) :: ntime, nntime, nntotal

  real(8)    :: lonst, loned, latst, lated
  integer(4) :: ist=117, ied=280, jst=102, jed=143
  integer(4) :: nyst, nyed
  logical, save :: lend_clm = .false. ! 

  ! if (lend_clm) then
  !  when there is not enough data, use climatology instead.
  ! else
  !  when there is not enough data, use y1949 instead
  ! end if

  integer(4) :: i, j, ii, jj
  integer(4) :: nyear, nnyear
  integer(4) :: nmonth, nnmonth
  real(8) :: distance
  integer(4),allocatable, dimension(:,:) :: iold, east_coast
  real(8),allocatable, dimension(:,:) :: residual
  logical :: is_end = .false.

  integer(4), parameter :: nmyear=12
  integer(4) :: nday(nmyear) =(/31, 28, 31, 30, 31, 30, &
       &                        31, 31, 30, 31, 30, 31/)
  real(8) :: dt(nmyear) 

  integer(4) :: osvfile = 81
  character(256) :: file_sv

  !----------------------------------------------------------------------

  type(type_libmxe_para) :: modelp, datap
  type(type_libmxe_grid) :: modelg, datag
  type(type_libmxe_topo) :: modelt
  character(256) :: file_namelist_data, file_namelist_model

  character(256) :: data_base_dir
  character(256) :: data_base_us, data_base_vs
  character(256) :: clim_base_dir
  character(256) :: clim_base_us, clim_base_vs
  character(256) :: file_us, file_vs
  integer(4),parameter :: lun = 10
  logical :: l_succeedu, l_succeedv
  integer(4) :: istat, ios
  logical :: l_units_mks

  !-----------------------------------------------------------

  namelist /nml_model/ file_namelist_model, &
       &  lonst, loned, latst, lated

  namelist /nml_data/ file_namelist_data, &
       &  data_base_dir, &
       &  data_base_us, data_base_vs,   &
       &  l_units_mks,   &
       &  clim_base_dir, &
       &  clim_base_us, clim_base_vs,   &
       &  undef_data

  namelist /nml_sv/ nyst, nyed, lend_clm, file_sv, l_intpl, intpmode

  !-----------------------------------------------------------

  open(lun,file='namelist.sverdrup',iostat=istat,status='old')

  if ( istat /= 0 ) then
    write(6,*) ' Cannot find file : namelist.sverdrup'
    stop
  end if

  ios = 0

  rewind(lun)
  read(lun, nml=nml_model, iostat=istat)
  ios = ios + istat
  write(6,*) ios

  l_units_mks = .false.
  rewind(lun)
  read(lun, nml=nml_data, iostat=istat)
  ios = ios + istat
  write(6,*) ios

  rewind(lun)
  read(lun, nml=nml_sv, iostat=istat) 
  ios = ios + istat
  write(6,*) ios

  close(lun)

  if ( ios /= 0 ) then
    write(6,*) ' Read error of namelist from namelist.sverdrup'
    stop
  end if

  !--------------------------------------------------------------
  ! set dimension

  dt(:) = dble(nday(:)) * 86400.d0

  !--------------------------------------------------------------

  call libmxe_para__register(modelp,file_namelist=file_namelist_model)
  call libmxe_grid__register(modelg,modelp)
  call libmxe_topo__register(modelt,modelp)
  call libmxe_topo__aexl(modelt,modelp)

  imt = modelp%imut
  jmt = modelp%jmut
  kmt = modelp%km
  lreclenm = 4 * imt * jmt

  allocate(alonu(1:imt), alatu(1:jmt))
  allocate(alont(1:imt), alatt(1:jmt))
  alonu(1:imt) = modelg%lonu(1:imt)
  alatu(1:jmt) = modelg%latu(1:jmt)
  alont(1:imt) = modelg%lont(1:imt)
  alatt(1:jmt) = modelg%latt(1:jmt)
  allocate(aexl(1:imt,1:jmt))
  aexl(1:imt,1:jmt) = modelt%aexl(1:imt,1:jmt,1)
  allocate(atexl(1:imt,1:jmt))
  atexl(1:imt,1:jmt) = modelt%atexl(1:imt,1:jmt,1)

  allocate(sv (imt,jmt))
  allocate(wsx(imt,jmt),wsy(imt,jmt),curltau(imt,jmt))
  allocate(C_Rossby(jmt))
  allocate(r2d(imt,jmt))
  allocate(iold(imt,jmt),residual(imt,jmt))
  allocate(east_coast(imt,jmt))
  allocate(beta(1:jmt))

  do j = 1, jmt
    beta(j) = 2.D0 * omega * cos ( radian_r * alatt(j) ) / radius
  end do

  do i = 1, imt
    if (lonst <= alont(i)) then
      ist = i
      exit
    end if
  end do

  do i = imt, 1, -1
    if (loned >= alont(i)) then
      ied = i
      exit
    end if
  end do

  do j = 1, jmt
    if (latst <= alatt(j)) then
      jst = j
      exit
    end if
  end do

  do j = jmt, 1, -1
    if (lated >= alatt(j)) then
      jed = j
      exit
    end if
  end do

  write(6,*) ' X range ', alont(ist), alont(ied)
  write(6,*) ' Y range ', alatt(jst), alatt(jed)

  !--------------------------------------------------------------

  call libmxe_para__register(datap,file_namelist=file_namelist_data)
  call libmxe_grid__register(datag,datap)
  imf = datap%imut
  jmf = datap%jmut
  kmf = datap%km
  lreclend = 4 * imf * jmf

  allocate(alonf(1:imf), alatf(1:jmf))
  alonf(1:imf) = datag%lonu(1:imf)
  alatf(1:jmf) = datag%latu(1:jmf)

  allocate(wsxf(1:imf,1:jmf))
  allocate(wsyf(1:imf,1:jmf))
  
  if (.not. l_intpl) then
    if ((imt /= imf) .or. (jmt /= jmf)) then
      write(6,*) ' if l_intpl is .false., size of model and data must be the same '
      stop
    end if
  end if

  !---------------------------------------------------------------
  ! set the speed of the 1st baroclinic Rossby wave at each latitude.

  call set_rossby_wave_speed

  call set_east_coast

  !----------------------------------------------------------------

  call open_file_direct(osvfile, file_sv, lreclenm)

  nntotal = (nyed - nyst + 1) * 12

  write(6,*) ' Number of time sequence ', nntotal

  nnyear  = nyed
  nnmonth = 12

  do nntime = nntotal, 1, -1

    write(*,*) 'nntime=', nntime, ' year ', nnyear, ' month ', nnmonth

    sv(:,:) = 0.d0
    residual(:,:) = 0.d0
    do j = jst, jed
      do i = ist, ied
        iold(i,j) = i
      end do
    end do

!    backward integration 

    nyear = nnyear
    nmonth = nnmonth

    ntimeloop: do ntime = nntime, -600, -1
!    ntimeloop: do ntime = nntime, 1, -1

      write(file_us,'(1a,1a,1a,1a,i4.4,i2.2)') trim(data_base_dir),'/',trim(data_base_us), &
           & '.',nyear,nmonth
      write(file_vs,'(1a,1a,1a,1a,i4.4,i2.2)') trim(data_base_dir),'/',trim(data_base_vs), &
           & '.',nyear,nmonth

      call open_file_direct(iwxfile,file_us,lreclend,l_succeed=l_succeedu)
      call open_file_direct(iwyfile,file_vs,lreclend,l_succeed=l_succeedv)
      if (l_succeedu .and. l_succeedv) then
        read(iwxfile,rec=1) wsxf
        read(iwyfile,rec=1) wsyf
        where(wsxf(1:imf,1:jmf) == undef_data)
          wsxf(1:imf,1:jmf) = 0.0
        end where
        where(wsyf(1:imf,1:jmf) == undef_data)
          wsyf(1:imf,1:jmf) = 0.0
        end where
        call close_file(iwxfile)
        call close_file(iwyfile)
      else
!        stop
        if (lend_clm) then
          ! when there is not enough data, use climatology instead.
          write(file_us,'(1a,1a,1a,1a,i2.2)') trim(clim_base_dir),'/',trim(clim_base_us),'.m',nmonth
          write(file_vs,'(1a,1a,1a,1a,i2.2)') trim(clim_base_dir),'/',trim(clim_base_vs),'.m',nmonth
          call open_file_direct(iwxfile,file_us,lreclend,l_succeed=l_succeedu)
          call open_file_direct(iwyfile,file_vs,lreclend,l_succeed=l_succeedv)
          read(iwxfile,rec=1) wsxf
          read(iwyfile,rec=1) wsyf
          call close_file(iwxfile)
          call close_file(iwyfile)
        else
          ! when there is not enough data, use 1st year instead
          write(file_us,'(1a,1a,1a,1a,i4.4,i2.2)') trim(clim_base_dir),'/',trim(clim_base_us), &
               & '.',nyear,nmonth
          write(file_vs,'(1a,1a,1a,1a,i4.4,i2.2)') trim(clim_base_dir),'/',trim(clim_base_vs), &
               & '.',nyear,nmonth
          call open_file_direct(iwxfile,file_us,lreclend,l_succeed=l_succeedu)
          call open_file_direct(iwyfile,file_vs,lreclend,l_succeed=l_succeedv)
          read(iwxfile,rec=1) wsxf
          read(iwyfile,rec=1) wsyf
          call close_file(iwxfile)
          call close_file(iwyfile)
        end if
      end if
!
      if (l_intpl) then
        ! interpolate wind stress at model grids.
        call hintpl(wsx,imt,jmt,alonu,alatu,wsxf,imf,jmf,alonf,alatf,intpmode)
        call hintpl(wsy,imt,jmt,alonu,alatu,wsyf,imf,jmf,alonf,alatf,intpmode)
      else
        wsx(:,:) = real(wsxf(:,:),8)
        wsy(:,:) = real(wsyf(:,:),8)
      end if
!
      if (l_units_mks) then
        wsx(:,:) = wsx(:,:) * 1.d1
        wsy(:,:) = wsy(:,:) * 1.d1
      end if

!     hcurl on T-points
!      
      do j = 2, jmt - 1
        do i = 2, imt - 1
          curltau(i,j) = 0.5d0 * &
               & (aexl(i,j)*aexl(i-1,j)*aexl(i,j-1)*aexl(i-1,j-1)) &
               & * ((wsy(i  ,j  )+wsy(i  ,j-1))*(modelg%dy_br(i  ,j)+modelg%dy_tr(i  ,j-1)) &
               &   -(wsy(i-1,j  )+wsy(i-1,j-1))*(modelg%dy_br(i-1,j)+modelg%dy_tr(i-1,j-1)) &
               &   -(wsx(i  ,j  )+wsx(i-1,j  ))*(modelg%dx_tl(i  ,j)+modelg%dx_tr(i-1,j  )) &
               &   +(wsx(i  ,j-1)+wsx(i-1,j-1))*(modelg%dx_tl(i,j-1)+modelg%dx_tr(i-1,j-1))) &
               & / (2.d0 *(modelg%a_bl(i,j)+modelg%a_tl(i,j-1)))
        end do
      end do

      do j = jst, jed
        do i = ist, ied

          hl1 = 0.d0
          hl2 = 0.d0
          distance = C_rossby(j) * dt(nmonth) + residual(i,j)
          ii = iold(i,j)

          if ( residual(i,j) <= modelg%dx_bl(ii,j) ) then
            sv(i,j) = sv(i,j) + residual(i,j)*curltau(ii,j) / beta(j)
          else
            sv(i,j) = sv(i,j) + modelg%dx_bl(ii,j)*curltau(ii,j) / beta(j) &
             &      + (residual(i,j)-modelg%dx_bl(ii,j))*curltau(ii+1,j) / beta(j)
          end if

          iiloop: do ii = iold(i,j), east_coast(i,j)
            
!# left part
!   |                     |                     |
!   |      dx_bl(i,j)     |     dx_br(i,j)      | 
!   +---------------------+---------------------+
! T(i,j)                                     T(i+1,j)
!
!------------>distance
!   <--hl3---> 
!   ^                     ^
!  hl1                   hl2
            hl1 = hl2
            hl2 = hl2 + modelg%dx_bl(ii,j)           
            if (distance .gt. hl2) then
              sv(i,j) = sv(i,j) - modelg%dx_bl(ii,j)*curltau(ii,j) / beta(j)
            else
              hl3 = distance - hl1
              sv(i,j) = sv(i,j) - hl3*curltau(ii,j) / beta(j)
              iold(i,j) = ii
              residual(i,j) = hl3
              exit iiloop
            end if

!# right part
!   |                     |                     |
!   |      dx_bl(i,j)     |     dx_br(i,j)      | 
!   +---------------------+---------------------+
! T(i,j)                                     T(i+1,j)
!
!-------------------------------->distance
!   <-------------hl3------------> 
!                         ^                     ^
!                        hl1                   hl2
            hl1 = hl2
            hl2 = hl2 + modelg%dx_br(ii,j) 
            if (distance .gt. hl2) then
              sv(i,j) = sv(i,j) - modelg%dx_br(ii,j)*curltau(ii+1,j) / beta(j)
            else
              hl3 = distance - hl1 + modelg%dx_bl(ii,j)
              sv(i,j) = sv(i,j) &
                   &  - (distance-hl1)*curltau(ii+1,j) / beta(j)
              iold(i,j) = ii
              residual(i,j) = hl3
              exit iiloop
            end if

            iold(i,j) = east_coast(i,j)
          end do iiloop

        end do
      end do

! check integration limit
      is_end = .true.
      do j = jst, jed
        do i = ist, jed
          if (iold(i,j) < east_coast(i,j)) then
            !write(6,*) alont(i),alatt(j),iold(i,j),east_coast(i,j)
            is_end = .false.
          end if
        end do
      end do
      if ( is_end ) then
        write(*,*) nntime, 'Rossbywaves reach eastcoast : ntime = ', ntime
        exit ntimeloop
      end if
!      write(*,*) 'ntime=',ntime,iold(ist,jed),atexl(iold(ist,jed),jed)

      nmonth = nmonth - 1
      if ( nmonth == 0 ) then
        nmonth = 12
        nyear = nyear - 1
      end if

    end do ntimeloop

    r2d(:,:) = sv(:,:) * 1.0e-12
    write(osvfile,REC=nntime) r2d

    !---------------------

    nnmonth = nnmonth - 1
    if ( nnmonth == 0 ) then
      nnmonth = 12
      nnyear = nnyear - 1
    end if

  end do

  call close_file(osvfile)
  
contains
!=====================================================================
!-------------------------------------------------------------------

        

!-------------------------------------------------------------------
  subroutine set_rossby_wave_speed

    REAL(8) :: C_Rossby_1deg(1:181) ! 90S - 90N
    REAL(8) :: lat_1deg(1:181)

!-------------------------------------------------------------------

    do j = 1, 181
      lat_1deg(j) = -91.d0 + real(j-1,8)
    end do

    C_rossby_1deg(:) = 0.d0

! from Fig.7 of Qiu (2003) and Fig.4a of Killworth and Blundell (2003)

    C_rossby_1deg(101) = 33.5d0   !09N
    C_rossby_1deg(102) = 30.0d0   !10N
    C_rossby_1deg(103) = 27.0d0   !11N
    C_rossby_1deg(104) = 24.2d0   !12N
    C_rossby_1deg(105) = 21.6d0   !13N
    C_rossby_1deg(106) = 19.2d0   !14N
    C_rossby_1deg(107) = 17.2d0   !15N
    C_rossby_1deg(108) = 15.4d0   !16N
    C_rossby_1deg(109) = 13.8d0   !17N
    C_rossby_1deg(110) = 12.4d0   !18N
    C_rossby_1deg(111) = 11.2d0   !19N
    C_rossby_1deg(112) = 10.2d0   !20N
    C_rossby_1deg(113) = 9.30d0   !21N
    C_rossby_1deg(114) = 8.50d0   !22N
    C_rossby_1deg(115) = 7.80d0   !23N
    C_rossby_1deg(116) = 7.20d0   !24N
    C_rossby_1deg(117) = 6.70D0   !25N
    C_rossby_1deg(118) = 6.30d0   !26N     
    C_rossby_1deg(119) = 5.95d0   !27N          
    C_rossby_1deg(120) = 5.60d0   !28N          
    C_rossby_1deg(121) = 5.25d0   !29N          
    C_rossby_1deg(122) = 4.90d0   !30N
    C_rossby_1deg(123) = 4.55d0   !31N
    C_rossby_1deg(124) = 4.15d0   !32N
    C_rossby_1deg(125) = 3.80d0   !33N
    C_rossby_1deg(126) = 3.45d0   !34N
    C_rossby_1deg(127) = 3.15d0   !35N
    C_rossby_1deg(128) = 2.85d0   !36N
    C_rossby_1deg(129) = 2.55d0   !37N
    C_rossby_1deg(130) = 2.30d0   !38N
    C_rossby_1deg(131) = 2.05d0   !39N
    C_rossby_1deg(132) = 1.85d0   !40N
    C_rossby_1deg(133) = 1.70d0   !41N
    C_rossby_1deg(134) = 1.55d0   !42N
    C_rossby_1deg(135) = 1.40d0   !43N
    C_rossby_1deg(136) = 1.30d0   !44N
    C_rossby_1deg(137) = 1.20d0   !45N
    C_rossby_1deg(138) = 1.10d0   !46N
    C_rossby_1deg(139) = 1.05d0   !47N
    C_rossby_1deg(140) = 1.00d0   !48N
    C_rossby_1deg(141) = 0.95d0   !49N
    C_rossby_1deg(142) = 0.90d0   !50N
    C_rossby_1deg(143) = 0.85d0   !51N

    !-------------------------------------------

    C_rossby(:) = 0.D0

    do j = 1, jmt

      if ((10.d0 <= alatt(j)) .and. (alatt(j) <= 51.d0)) then

        do jj = 1, 181
          if ((lat_1deg(jj) <= alatt(j)) .and. (alatt(j) < lat_1deg(jj+1))) then
            C_rossby(j) = C_rossby_1deg(jj) &
                 & + (C_rossby_1deg(jj+1) -  C_rossby_1deg(jj)) &
                 & * (alatt(j) - lat_1deg(jj)) / (lat_1deg(jj+1) - lat_1deg(jj))
          end if
        end do

      end if

      write(6,*) 'Rossby wave:', j, alatt(j), C_rossby(j)

    end do


  end subroutine set_rossby_wave_speed
  !-----------------------------------------------------------------------------
  subroutine set_east_coast

    do j = jst, jed
      do i = ist, ied
!
! edit here 
! 
        do ii = (ist + ied)/2 + 1, ied
          if ( atexl(ii,j) == 0.d0 ) then
            east_coast(i,j) = ii
            exit
          end if
        end do
        if ( atexl(i,j) == 0.d0 ) then
          east_coast(i,j) = -1
        end if 
      end do
    end do 
  end subroutine set_east_coast

  !------------------------------------------------------------------------

end program sverdrup_time_series
