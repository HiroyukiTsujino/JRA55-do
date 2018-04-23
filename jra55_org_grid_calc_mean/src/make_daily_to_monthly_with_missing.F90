! -*-F90-*-
!------------------------------------------------------------------------
program make_monthly_data_from_daily_with_missing

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
  integer(4) :: iyear, ileap
  integer(4) :: imst, imed
  integer(4) :: mst, med

  integer(4) :: i, j, m, n

  integer(4), parameter :: lun = 10

  real(4) :: undef_in, undef_out
  real(8) :: undef8_in, undef8_out
  real(8) :: min_ratio

  integer(4) :: n_valid(imf,jmf)

  !+***!S++1++++*++++2++++*++++3++++*++++4++++*++++5++++*++++6++++*++++7++

  namelist /nml_day2mon_miss/ iyst, imst, iyed, imed, &
       & item_name, &
       & in_base_dir, out_base_dir, &
       & undef_in, undef_out, &
       & min_ratio

  !-----------------------------------------------------------------------

  open(lun,file='namelist_daily2monthly_miss')
  read(lun,nml=nml_day2mon_miss)
  close(lun)

  undef8_in  = real(undef_in,8)
  undef8_out = real(undef_out,8)

  lreclen = imf*jmf*4

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

    write(6,*) ' month = ',m, ' days = ',idmon(m), ' limit = ', real(idmon(m),8) * min_ratio

    data_mon(1:imf,1:jmf) = 0.0d0
    n_valid(1:imf,1:jmf) = 0

    do n = 1, idmon(m)

      write(fin_snap,'(1a,1a,i4.4,i2.2,1a,1a,1a,i4.4,i2.2,i2.2)') &
           & trim(in_base_dir),'/',iyear,m,'/',trim(item_name),'.',iyear,m,n
      write(6,*) ' reading from .....', trim(fin_snap)
      open(mtin1,file=fin_snap,form='unformatted',access='direct',action='read', &
           & convert='little_endian',recl=lreclen)
      read(mtin1,rec=1) data_snap
      close(mtin1)

      do j = 1, jmf
        do i = 1, imf
          if (data_snap(i,j) /= undef_in) then
            data_mon(i,j) = data_mon(i,j) + real(data_snap(i,j),8)
            n_valid(i,j) = n_valid(i,j) + 1
          end if
        end do
      end do
    end do

    do j = 1, jmf
      do i = 1, imf
        if (real(n_valid(i,j),8) >= min_ratio * real(idmon(m),8)) then
          data_mon(i,j) = data_mon(i,j) / real(n_valid(i,j),8)
        else
          data_mon(i,j) = undef8_out
        end if
      end do
    end do

    data_snap(1:imf,1:jmf) = real(data_mon(1:imf,1:jmf),4)

    write(fout_month,'(1a,1a,1a,1a,i4.4,i2.2)') &
         & trim(out_base_dir),'/',trim(item_name),'.',iyear,m
    write(6,*) ' Writing to .....', trim(fout_month)
    open(mtot1,file=fout_month,form='unformatted',access='direct',action='write', &
         & convert='little_endian',recl=lreclen)
    write(mtot1,rec=1) data_snap
    close(mtot1)

  end do

  end do LOOP_YEAR

end program make_monthly_data_from_daily_with_missing
