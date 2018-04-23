! -*-F90-*-
!==============================================================
program compare_downward_shortwave_with_buoy
!==============================================================

  use libmxe_para, only: libmxe_para__register, clen, rho &
                     & , pi, radius, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo

  implicit none

  integer(4) :: imax
  integer(4) :: jmax

  integer(4) :: i, j, k, nyear, month, klev, mday
  integer(4) :: nbyr, neyr, nbmn, nemn
  integer(4) :: iw, ie
  integer(4) :: js, jn
  integer(4) :: mst, med

  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
  integer(4),parameter :: nday_year = 365

  integer(4),parameter :: mtin1 = 31, mtin2 = 32, mtin3 = 33
  integer(4),parameter :: mtot1 = 51, mtot2 = 52, mtot3 = 53

  integer(4) :: lrec, ios, ireco

  real(4) :: undef_ceres
  real(4) :: undef_buoy
  real(4) :: tmp4
  real(8) :: rad_buoy
  real(8) :: rad_local
  real(8) :: rad_bias
  real(8) :: rad_ceres_mean
  integer(4) :: num_valid

  real(4),allocatable :: dat2(:,:)
  real(8),allocatable :: rad_ceres(:,:)

  real(4) :: lat_buoy4, lon_buoy4
  real(8) :: lat_buoy, lon_buoy, wi, wj

  character(len=256) :: flnin1, flnin2, flnin3
  character(len=256) :: flnin1_base, flnin2_base
  character(len=256) :: flnot1, flnot2, flnot3
  character(len=256) :: elem_ceres
  character(len=256) :: elem_buoy
  character(len=256) :: location

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo

!-----------------------------------------------------------------

  namelist /nml_rad_comp/ &
       &  flnin1_base, &
       &  flnin2_base, &
       &  nbyr, nbmn,  &
       &  neyr, nemn,  &
       &  undef_ceres, undef_buoy, &
       &  lon_buoy4, lat_buoy4, &
       &  flnot1,        &
       &  flnot2,        &
       &  flnot3,        &
       &  elem_ceres, &
       &  elem_buoy,  &
       &  location

!-----------------------------------------------------------------

  open (10,file='namelist.rad_comp')
  read (10,nml_rad_comp) 
  close(10)

  lon_buoy = real(lon_buoy4,8)
  lat_buoy = real(lat_buoy4,8)

!-----------------------------------------------------------------
  ! set grid points

  !-- model settings --

  call libmxe_para__register(para)
  call libmxe_grid__register(grid,para)
  call libmxe_topo__register(topo,para)
  call libmxe_topo__aexl(topo,para)

  imax = para%imut
  jmax = para%jmut

  allocate (dat2(1:imax,1:jmax))
  allocate (rad_ceres(1:imax,1:jmax))

  lrec=imax*jmax*4

  if (lon_buoy < grid%lonu(1)) then
    lon_buoy = lon_buoy + 360.d0
  end if

  !-----------------------------------------------------------------------

  write(6,*) ' Time series of radiation at ', trim(location), ' is written to ',trim(flnot1)
  open (mtot1,file=flnot1,access='direct',form='unformatted',recl=4)
  ireco = 0

  rad_bias = 0.0d0
  rad_ceres_mean = 0.0d0
  num_valid = 0

  do nyear = nbyr, neyr

    mst = 1
    med = 12
    if (nyear == nbyr) mst = nbmn
    if (nyear == neyr) med = nemn

    do month = mst, med

      write(flnin1,'(4a,i4.4,i2.2)') trim(flnin1_base),'/',trim(elem_ceres),'.',nyear,month
      write(6,*) ' opening  ', trim(flnin1)
      open(mtin1, file=flnin1, form='unformatted', access='direct', status='old', recl=lrec)
      read(mtin1,rec=1) dat2
      close(mtin1)

      do j = 1, jmax
        do i = 1, imax
          if (dat2(i,j) /= undef_ceres) then
            rad_ceres(i,j) = real(dat2(i,j),8)
          else
            rad_ceres(i,j) = 0.0d0
          end if
        end do
      end do
      
      write(flnin2,'(6a,i4.4,i2.2)') trim(flnin2_base),'/',&
           & trim(elem_buoy),'_',trim(location),'.',nyear,month
      write(6,*) ' opening file : ', trim(flnin2)
      open(mtin2, file=flnin2, form='unformatted', access='direct', status='old', recl=4, iostat=ios)
      write(6,*) 'iostat = ', ios
      if (ios == 0) then
        read(mtin2,rec=1) tmp4
        rad_buoy = real(tmp4,8)
        close(mtin2)
      else
        write(6,*) ' no file for this year and month'
        rad_buoy = real(undef_buoy,8)
      end if

      do j = 2, jmax
        if ((grid%latu(j-1) <= lat_buoy) .and. (lat_buoy < grid%latu(j))) then
          jn = j
          js = j - 1
          wj = (lat_buoy - grid%latu(js)) / (grid%latu(jn) - grid%latu(js))
          do i = 2, imax
            if ((grid%lonu(i-1) <= lon_buoy) .and. (lon_buoy < grid%lonu(i))) then
              ie = i
              iw = i - 1
              wi = (lon_buoy - grid%lonu(iw)) / (grid%lonu(ie) - grid%lonu(iw))
              rad_local = (1.0d0 - wi) * (1.0d0 - wj) * rad_ceres(iw,js) &
                   &    +          wi  * (1.0d0 - wj) * rad_ceres(ie,js) &
                   &    + (1.0d0 - wi) *          wj  * rad_ceres(iw,jn) &
                   &    +          wi  *          wj  * rad_ceres(ie,jn)
              exit
            end if
            if (i == imax .and. ((grid%lonu(1)+360.d0) > lon_buoy)) then
              ie = 1
              iw = imax
              wi = (lon_buoy - grid%lonu(iw)) / (grid%lonu(ie) + 360.d0 - grid%lonu(iw))
              rad_local = (1.0d0 - wi) * (1.0d0 - wj) * rad_ceres(iw,js) &
                   &    +          wi  * (1.0d0 - wj) * rad_ceres(ie,js) &
                   &    + (1.0d0 - wi) *          wj  * rad_ceres(iw,jn) &
                   &    +          wi  *          wj  * rad_ceres(ie,jn)
              exit
            end if
          end do
          exit
        end if
      end do

      write(6,*) rad_local, rad_buoy
      if (rad_buoy /= real(undef_buoy,8)) then
        rad_bias = rad_bias + (rad_local - rad_buoy)
        rad_ceres_mean = rad_ceres_mean + rad_local
        num_valid = num_valid + 1
      end if

      ireco = ireco + 1
      write(mtot1,rec=ireco) real(rad_local,4)
      ireco = ireco + 1
      write(mtot1,rec=ireco) real(rad_buoy,4)

    end do

  end do

  close(mtot1)

  if (num_valid > 12) then
    rad_bias = rad_bias / real(num_valid,8)
    rad_ceres_mean = rad_ceres_mean / real(num_valid,8)
    write(6,*) ' Bias = ', rad_bias, num_valid
    write(6,*) ' CERES bias at ', trim(location), ' is written to ',trim(flnot2)
    open (mtot2,file=flnot2,form='formatted')
    if (rad_bias > 0.0d0) then
      write(mtot2,'(1a)') '\'set string 2 c \''
    else
      write(mtot2,'(1a)') '\'set string 4 c \''
    end if
    write(mtot2,'(1a,f10.3,f10.3,f8.1,1a)') &
         & '\'run drawstring.gs', lon_buoy4 , lat_buoy4, real(rad_bias,4),'\''
    close(mtot2)

    write(6,*) ' BIAS / CERES at ', trim(location), ' is written to ',trim(flnot3)
    open (mtot3,file=flnot3,form='formatted')
    if (rad_bias > 0.0d0) then
      write(mtot3,'(1a)') '\'set string 1 c \''
    else
      write(mtot3,'(1a)') '\'set string 15 c \''
    end if
    write(mtot3,'(1a,f10.3,f10.3,f8.1,1a)') &
         & '\'run drawstring.gs', lon_buoy4 , lat_buoy4, real(-rad_bias/rad_ceres_mean*1.d2,4),'\''
    close(mtot3)
  end if

end program compare_downward_shortwave_with_buoy
