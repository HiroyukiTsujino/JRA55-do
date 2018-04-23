! -*-F90-*-
!-------------------------------------------------------------------
program takeaway_29Feb_cobesst_daily

  implicit none

  ! HadISST

  integer(4), parameter :: imt = 360, jmt = 180
  integer(4), parameter :: nyear_first = 1870

  real(4), parameter :: rundef_sst = 9.999e+20
  real(8), parameter :: dundef_sst = 9.999d+20

  real(4) :: work_sst(imt,jmt), work_ice(imt,jmt)

  real(8) :: sst(imt,jmt)
  real(8) :: ice(imt,jmt)

  !-----------------------------------------------

  integer(4), parameter :: mtin1 = 61, mtin2 = 62
  integer(4), parameter :: mtot1 = 71, mtot2 = 72

  character(256) :: dir_in, dir_out
  character(256) :: flin1, flin2, flin3, flin4
  character(256) :: flot1, flot2

  integer(4) :: irec1, irec2, irec3, irec4
  integer(4) :: irec_sst, irec_ice, irec_ssto, irec_iceo

  integer(4) :: i, j, k, l, m, n

  integer(4) :: idmon(12)

  integer(4) :: nday, month, nyear, ibyr, ieyr

  !-----------------------------------------------------------------------

  namelist /nml_remove_leap/ dir_in, dir_out, ibyr, ieyr

  !-----------------------------------------------------------------------

  open (10,file='namelist.remove_leap')
  read (10,nml=nml_remove_leap)
  close(10)

  !-----------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /) ! leap year

  !-----------------------------------------------------------------------

  LOOP_YEAR : do nyear = ibyr, ieyr

    if (mod(nyear,4) /= 0) then
      write(6,*) ' Operation cancelled for year ', nyear
      cycle
    end if

    write(flin1,'(1a,1a,i4.4)') trim(dir_in),'/sst-glb.',nyear
    write(6,*) 'reading temparature from ...', trim(flin1)
    open(mtin1,file=flin1,form='unformatted',access='direct',recl=4*imt*jmt)
    irec_sst = 0

    write(flin2,'(1a,1a,i4.4)') trim(dir_in),'/ice-glb.',nyear
    write(6,*) 'reading ice fractional area from ...', trim(flin2)
    open(mtin2,file=flin2,form='unformatted',access='direct',recl=4*imt*jmt)
    irec_ice = 0

    write(flot1,'(1a,1a,i4.4)') trim(dir_out),'/sst-glb.',nyear
    open(mtot1,file=flot1,form='unformatted',access='direct',recl=4*imt*jmt)
    irec_ssto = 0

    write(flot2,'(1a,1a,i4.4)') trim(dir_out),'/ice-glb.',nyear
    open(mtot2,file=flot2,form='unformatted',access='direct',recl=4*imt*jmt)
    irec_iceo = 0

    do month = 1, 12

      do nday = 1, idmon(month)

        irec_sst = irec_sst + 1
        read(mtin1,rec=irec_sst) work_sst(1:imt,1:jmt)

        irec_ice = irec_ice + 1
        read(mtin2,rec=irec_ice) work_ice(1:imt,1:jmt)

        !------

        if (month == 2 .and. nday == 29) cycle

        irec_ssto = irec_ssto + 1
        write(mtot1,rec=irec_ssto) work_sst
        
        irec_iceo = irec_iceo + 1
        write(mtot2,rec=irec_iceo) work_ice

      end do

    end do

    write(6,*) 'read  ', irec_sst, irec_ice
    write(6,*) 'write ', irec_ssto, irec_iceo

    close(mtot2)
    close(mtot1)
    close(mtin2)
    close(mtin1)
    
  end do LOOP_YEAR


end program takeaway_29Feb_cobesst_daily
