! -*-F90-*-
!------------------------------------------------------------------------
program make_daily_data

  implicit none

  integer(4), parameter :: imf = 640
  integer(4), parameter :: jmf = 320

  integer(4), parameter :: mtin1 = 80
  integer(4), parameter :: mtot1 = 91, mtot2 = 92, mtot3 = 93

  real(4) :: data_snap(imf,jmf) ! snap shot read from file
  real(8) :: data_day(imf,jmf)  ! daily mean

  integer(4) :: idmon(12)

  character(len=256) :: item_name
  character(len=256) :: fin_snap
  character(len=256) :: fout_year
  character(len=256) :: fout_year_first
  character(len=256) :: fout_year_last
  character(len=256) :: in_base_dir
  character(len=256) :: out_base_dir

  character(len=256) :: flout

  integer(4) :: lreclen

  integer(4) :: ireci1
  integer(4) :: ireco1

  integer(4) :: iyst, iyed
  integer(4) :: iyear, ileap
  integer(4) :: num_day

  integer(4) :: i, j, m, n, nh, ihour

  integer(4), parameter :: lun = 10
  integer(4), parameter :: num_data_per_day = 8 ! 3-hourly

  !+***!S++1++++*++++2++++*++++3++++*++++4++++*++++5++++*++++6++++*++++7++

  namelist /nml_org2day/ iyst, iyed, item_name, &
       & in_base_dir, out_base_dir

  !-----------------------------------------------------------------------

  open(lun,file='namelist_org2daily')
  read(lun,nml=nml_org2day)
  close(lun)

  lreclen = imf*jmf*4

  !-----------------------------------------------------------------------

  LOOP_YEAR: do iyear = iyst, iyed

  write(fout_year,'(1a,1a,1a,1a,i4.4)') trim(out_base_dir),'/',trim(item_name),'.',iyear
  write(6,*) ' Writing to .....', trim(fout_year)
  open(mtot1,file=fout_year,form='unformatted',access='direct',recl=lreclen)

  write(fout_year_first,'(1a,1a,1a,1a,i4.4,1a)') trim(out_base_dir),'/',trim(item_name),'.',iyear,'0101'
  write(6,*) ' Writing to .....', trim(fout_year_first)
  open(mtot2,file=fout_year_first,form='unformatted',access='direct',recl=lreclen)


  write(fout_year_last,'(1a,1a,1a,1a,i4.4,1a)') trim(out_base_dir),'/',trim(item_name),'.',iyear,'1231'
  write(6,*) ' Writing to .....', trim(fout_year_last)
  open(mtot3,file=fout_year_last,form='unformatted',access='direct',recl=lreclen)

  num_day = 0

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

  do m = 1, 12
!  do m = 1, 1

    write(6,*) ' month = ',m, ' days = ',idmon(m)

    do n = 1, idmon(m)

      num_day = num_day + 1
      data_day(1:imf,1:jmf) = 0.0d0

      do nh = 1, num_data_per_day
        ihour = 3 * (nh - 1)
        write(fin_snap,'(1a,1a,i4.4,i2.2,1a,1a,1a,i4.4,i2.2,i2.2,i2.2)') &
             & trim(in_base_dir),'/',iyear,m,'/',trim(item_name),'.',iyear,m,n,ihour
        write(6,*) ' reading from .....', trim(fin_snap)
        open(mtin1,file=fin_snap,form='unformatted',access='direct',recl=lreclen)
        read(mtin1,rec=1) data_snap
        close(mtin1)
        data_day(1:imf,1:jmf) = data_day(1:imf,1:jmf) + real(data_snap(1:imf,1:jmf),8)
      end do

      data_day(1:imf,1:jmf) = data_day(1:imf,1:jmf) / real(num_data_per_day,8)

      data_snap(1:imf,1:jmf) = real(data_day(1:imf,1:jmf),4)

      write(mtot1,rec=num_day) data_snap

      if (num_day == 1) then
        write(mtot2,rec=1) data_snap
      end if

      if (m == 12 .and. n == idmon(m)) then
        write(mtot3,rec=1) data_snap
      end if

    end do

  end do

  close(mtot3)
  close(mtot2)
  close(mtot1)

  end do LOOP_YEAR

end program make_daily_data
