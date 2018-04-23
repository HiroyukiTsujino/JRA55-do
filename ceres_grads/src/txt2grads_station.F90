! -*-F90-*-
!==================================================================
program read_text_output_grads
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of the STATION and CERES data
  !----------------------------------------------------------------

  implicit none

  integer(4), parameter :: imut = 1, jmut = 1, km = 1
  integer(4), parameter :: mtin1 = 81, mtin2 = 82
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  character(128) :: file_in, file_out, file_clim

  integer(4) :: i, j, k, l, m, n
  real(4) :: undef_in, undef_out

  real(4) :: sta, ceres, diff

  integer(4) :: ireco1, ireco2
  integer(4) :: ny, mn
  integer(4) :: ny_tmp, mn_tmp, count
  integer(4) :: nbyr, neyr, nbmn, nemn
  integer(4) :: mst, med

  real(8) :: sta_clim(12), ceres_clim(12)
  integer(4) :: num_sta(12), num_ceres(12)

  !--------------------------------------------------------------------

  namelist /nml_station_mon/ &
       & file_in, undef_in, &
       & file_out, undef_out, &
       & file_clim, &
       & nbyr, nbmn, &
       & neyr, nemn

  !--------------------------------------------------------------------

  open(10,file='namelist.station')
  read(10,nml=nml_station_mon)
  close(10)

  !---------------------------------------------------------------------
  ! open netcdf file

  open(mtin1,file=file_in,form='formatted')
  write(6,*) ' read from ', trim(file_in)
  do n = 1, 7
    read(mtin1,*)
  end do

  open(mtot1,file=file_out,form='unformatted',access='direct',recl=4)
  write(6,*) 'Downward radiation written to ',trim(file_out)
  ireco1 = 0

  sta_clim(:) = 0.0d0
  ceres_clim(:) = 0.0d0
  num_sta(:) = 0
  num_ceres(:) = 0

  do ny = nbyr, neyr

    if (ny == nbyr) then
      mst = nbmn
    else
      mst = 1
    end if

    if (ny == neyr) then
      med = nemn
    else
      med = 12
    end if

    do mn = mst, med

      read(mtin1,*) ny_tmp, mn_tmp, sta, count, ceres
      write(6,*) ny_tmp, mn_tmp, sta, count, ceres

      if (sta /= undef_in .and. ceres /= undef_in) then
        sta_clim(mn) = sta_clim(mn) + real(sta,8)
        num_sta(mn) = num_sta(mn) + 1
        ceres_clim(mn) = ceres_clim(mn) + real(ceres,8)
        num_ceres(mn) = num_ceres(mn) + 1
      end if

      ireco1 = ireco1 + 1
      write(mtot1,rec=ireco1) real(sta,4)
      ireco1 = ireco1 + 1
      write(mtot1,rec=ireco1) real(ceres,4)
      diff = ceres - sta
      ireco1 = ireco1 + 1
      write(mtot1,rec=ireco1) real(diff,4)

    end do
  end do
  close(mtot1)
  close(mtin1)

  open(mtot2,file=file_clim,form='unformatted',access='direct',recl=4)
  write(6,*) 'Monthly climatology written to ',trim(file_clim)
  ireco2 = 0

  do mn = 1, 12
    if (num_sta(mn) > 1) then
      sta_clim(mn) = sta_clim(mn) / real(num_sta(mn),8)
      ceres_clim(mn) = ceres_clim(mn) / real(num_ceres(mn),8)
    end if
    write(6,*) mn, sta_clim(mn), ceres_clim(mn)
    ireco2 = ireco2 + 1
    write(mtot2,rec=ireco2) real(sta_clim(mn),4)
    ireco2 = ireco2 + 1
    write(mtot2,rec=ireco2) real(ceres_clim(mn),4)
  end do
  
  close(mtot2)

end program read_text_output_grads
