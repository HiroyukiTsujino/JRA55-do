! -*-F90-*-
!------------------------------------------------------------------------
program make_monthly_flux_data

  implicit none

  integer(4), parameter :: imf = 157792 ! reduced TL319 grid
  integer(4), parameter :: jmf = 1

  integer(4), parameter :: mtin1 = 80
  integer(4), parameter :: mtot1 = 90

  real(4) :: data_snap(imf,jmf) ! snap shot read from file
  real(8) :: data_day(imf,jmf)  ! daily mean
  real(8) :: data_mon(imf,jmf)  ! monthly mean

  integer(4) :: idmon(12)

  character(len=256) :: item_name
  character(len=256) :: fin_snap
  character(len=256) :: fout_month
  character(len=256) :: in_base_dir
  character(len=256) :: out_base_dir

  character(len=256) :: flout

  integer(4) :: lreclen

  integer(4) :: ireci1
  integer(4) :: ireco1

  integer(4) :: iyst, iyed
  integer(4) :: imst, imed
  integer(4) :: mst, med
  integer(4) :: iyear, ileap

  integer(4) :: i, j, m, n, nh, ihour

  integer(4), parameter :: lun = 10
  integer(4) :: int_hour
  integer(4) :: num_data_per_day

  !+***!S++1++++*++++2++++*++++3++++*++++4++++*++++5++++*++++6++++*++++7++

  namelist /nml_org2mon_flux/ iyst, iyed, imst, imed, item_name, &
       & in_base_dir, out_base_dir, int_hour

  !-----------------------------------------------------------------------

  open(lun,file='namelist_org2monthly_flux')
  read(lun,nml=nml_org2mon_flux)
  close(lun)

  lreclen = imf * jmf * 4
  num_data_per_day = 24 / int_hour

  !-----------------------------------------------------------------------

  LOOP_YEAR: do iyear = iyst, iyed

  idmon(1) = 31
  idmon(2) = 28
  idmon(3) = 31
  idmon(4) = 30
  idmon(5) = 31
  idmon(6) = 30
  idmon(7) = 31
  idmon(8) = 31
  idmon(9) = 30
  idmon(10) = 31
  idmon(11) = 30
  idmon(12) = 31

  ileap = 0
  if (mod(iyear,4) == 0) ileap = 1
  if (mod(iyear,100) == 0) ileap = 0
  if (mod(iyear,400) == 0) ileap = 1
  idmon(2) = idmon(2) + ileap

  !-----------------------------------------------------------------------

  mst = 1
  med = 12

  if (iyear == iyst) then
    mst = imst
  end if

  if (iyear == iyed) then
    med = imed
  end if

  do m = mst, med

    write(6,*) ' month = ',m, ' days = ',idmon(m)

    data_mon(1:imf,1:jmf) = 0.0d0

    do n = 1, idmon(m)

      data_day(1:imf,1:jmf) = 0.0d0

      do nh = 1, num_data_per_day
        ihour = int_hour * (nh - 1)
        write(fin_snap,'(1a,1a,i4.4,i2.2,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') &
             & trim(in_base_dir),'/',iyear,m,'/',trim(item_name),'.',iyear,m,n,ihour
        write(6,*) ' reading from .....', trim(fin_snap)
        open(mtin1,file=fin_snap,form='unformatted',access='direct', &
             & convert='little_endian',recl=lreclen)
        read(mtin1,rec=1) data_snap
        close(mtin1)
        data_day(1:imf,1:jmf) = data_day(1:imf,1:jmf) + real(data_snap(1:imf,1:jmf),8)
      end do

      data_day(1:imf,1:jmf) = data_day(1:imf,1:jmf) / real(num_data_per_day,8)

      data_mon(1:imf,1:jmf) = data_mon(1:imf,1:jmf) + data_day(1:imf,1:jmf)

    end do

    data_mon(1:imf,1:jmf) = data_mon(1:imf,1:jmf) / real(idmon(m),8)

    data_snap(1:imf,1:jmf) = real(data_mon(1:imf,1:jmf),4)

    write(fout_month,'(1a,1a,1a,1a,i4.4,i2.2)') &
         & trim(out_base_dir),'/',trim(item_name),'.',iyear,m
    write(6,*) ' Writing to .....', trim(fout_month)
    open(mtot1,file=fout_month,form='unformatted',access='direct', &
         & convert='little_endian',recl=lreclen)
    write(mtot1,rec=1) data_snap
    close(mtot1)

  end do

  end do LOOP_YEAR

end program make_monthly_flux_data
