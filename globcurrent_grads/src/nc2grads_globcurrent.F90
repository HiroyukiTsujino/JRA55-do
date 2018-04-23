! -*-F90-*-
!==================================================================
program read_netcdf_output_grads
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of NOAA-AOML drifter data
  !----------------------------------------------------------------

  implicit none

  include 'netcdf.inc'

  integer(4), parameter :: imut = 1440, jmut = 720
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  real(8) :: dat8(jmut,imut)
  real(4) :: dat4(imut,jmut)

  real(8) :: svel(imut,jmut,2)
  integer(4) :: numd(imut,jmut,2)
  real(4) :: work4(imut,jmut)

  integer(4) :: idmon(12)

  character(256) :: flot1, flot2

  integer(4) :: i, j, k, l, m, n, jw

  integer(4) :: nmocount
  integer(4) :: ireco1, ireco2
  integer(4) :: nyear
  integer(4) :: mst, med

  real(4) :: undef_in = 0.0, undef_out = 0.0
  real(4),parameter :: undef_globcurrent = 9.96921e36
  real(4),parameter :: undef_mxe = -9.99e33

  ! for netCDF

  integer(4), parameter :: nvars = 12, nfiles = 1
  integer(4) :: var(nvars), sta(nvars)
  integer(4) :: start(3), range(3)
  character(256) :: flnin(nfiles)
  integer(4) :: ifiles(nfiles)

  character(256) :: flnin_globcurrent, flnot_base, flnin_dir

  logical :: isnan
  logical :: leap

  integer(4) :: iyst, iyed, iy, im, id, nded

  !--------------------------------------------------------------------

  namelist /nml_globcurrent/ &
       & flnin_dir, flnin_globcurrent, undef_in, &
       & flnot_base, undef_out, &
       & iyst, iyed, mst, med

  mst = 1
  med = 1
  iyst = 1
  iyed = 1

  open (11,file='namelist.globcurrent')
  read (11,nml=nml_globcurrent)
  close(11)

  !--------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  !---------------------------------------------------------------------

  start(1:3) = (/ 1, 1, 1 /)
  range(1:3) = (/ imut, jmut, 1 /)

  LOOP_YEAR: do iy = iyst, iyed
    if (mod(iy,4) == 0) then
      if (mod(iy,100) == 0) then
        if (mod(iy,400) == 0) then
          leap=.true.
        else
          leap=.false.
        end if
      else
        leap=.true.
      end if
    else
      leap=.false.
    end if
  LOOP_MONTH: do im = mst, med
    if (im /= 2) then
      nded = idmon(im)
    else
      if (leap) then
        nded = idmon(im) + 1
      else
        nded = idmon(im)
      end if
    end if
    write(flot1,'(1a,i4.4,i2.2)') trim(flnot_base),iy,im
    open(mtot1,file=flot1,form='unformatted',access='direct',convert='big_endian',recl=4*imut*jmut)
    write(6,*) 'Surface current written to ',trim(flot1)
    ireco1 = 0
    svel(:,:,:) = 0.0d0
    numd(:,:,:) = 0

  LOOP_DAY: do id = 1, nded

    ! open netcdf file
    write(flnin(1),'(1a,1a,i4.4,1a,i4.4,i2.2,i2.2,1a,1a)') &
         & trim(flnin_dir),'/',iy,'/',iy,im,id,'-',trim(flnin_globcurrent)

    do n = 1, nfiles
      write(6,*) 'Openning ',trim(flnin(n))
      sta(n) = nf_open(flnin(n),nf_nowrite,ifiles(n)) 
      write(6,*) 'ifiles = ', ifiles(1)
      if (sta(n) /= 0) then
        write(6,*) 'nf_open error for file number ',n, sta(n)
        stop
      end if
    end do

    sta(1) = nf_inq_varid(ifiles(1),'eastward_eulerian_current_velocity' ,var(1))
    sta(2) = nf_inq_varid(ifiles(1),'northward_eulerian_current_velocity',var(2))

    do n = 1, 2
      if (sta(n) /= 0) then
        write(6,*) 'nf_inq_varid error for variable ',n, var(n)
        stop
      else
        write(6,*) ' var sta = ', var(n), sta(n)
      end if
    end do

    do n = 1, 2

      sta(n) = nf_get_vara_real(ifiles(1),var(n),start,range,dat4)

      if (sta(n) /= 0) then
        write(6,*) 'nf_get_vara_real error for variable ',n, var(n), sta(n), ifiles(1)
        stop
      end if

      do j = 1, jmut
        do i = 1, imut/2 ! eastern longitude
          if (dat4(i+imut/2,j) /= undef_in) then
            numd(i,j,n) = numd(i,j,n) + 1
            svel(i,j,n) = svel(i,j,n) + real(dat4(i+imut/2,j),4)
          end if
        end do
        do i = 1, imut/2 ! western longitude
          if (dat4(i,j) /= undef_in) then
            numd(i+imut/2,j,n) = numd(i+imut/2,j,n) + 1
            svel(i+imut/2,j,n) = svel(i+imut/2,j,n) + real(dat4(i,j),4)
          end if
        end do
      end do
    end do

    do n = 1, nfiles
      !write(6,*) ifiles(n)
      sta(n) = nf_close(ifiles(n))
      if (sta(n) /= 0) then
        write(6,*) 'nf_close error for file number ',n, ifiles(n), sta(n)
      end if
    end do

  end do LOOP_DAY

  do j = 1, jmut
    do i = 1, imut
      if ((numd(i,j,1) > 0) .and. (numd(i,j,2) > 0)) then
        if (numd(i,j,1) == numd(i,j,2)) then
          svel(i,j,1) = svel(i,j,1) / real(numd(i,j,1),8)
          svel(i,j,2) = svel(i,j,2) / real(numd(i,j,2),8)
        else
          write(6,*) 'Warning : U and V are not consistent'
        end if
      else
        svel(i,j,1) = real(undef_out,8)
        svel(i,j,2) = real(undef_out,8)
      end if
    end do
  end do

  write(mtot1,rec=1) real(svel(1:imut,1:jmut,1),4)
  write(mtot1,rec=2) real(svel(1:imut,1:jmut,2),4)
  write(mtot1,rec=3) real(numd(1:imut,1:jmut,1),4)
  write(mtot1,rec=4) real(numd(1:imut,1:jmut,2),4)
  close(mtot1)

  end do LOOP_MONTH
  end do LOOP_YEAR

contains

  logical function isnan(a) 
    real(8) ::  a 
    if (a.ne.a) then 
      isnan = .true. 
    else 
      isnan = .false. 
    end if
    return 
  end function isnan

end program read_netcdf_output_grads
