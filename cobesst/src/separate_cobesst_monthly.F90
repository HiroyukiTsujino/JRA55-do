! -*-F90-*-
!-------------------------------------------------------------------
program separate_cobesst_monthly

  implicit none

  ! HadISST

  integer(4), parameter :: imt = 360, jmt = 180
  integer(4), parameter :: nyear_first = 1870

  real(4), parameter :: rundef_sst = 9.999e+20
  real(8), parameter :: dundef_sst = -9.99d+33

  real(8) :: alond(imt), alatd(jmt)

  real(4) :: work_sst(imt,jmt)

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

  integer(4) :: month, nyear, ibyr, ieyr, iemn

  !-----------------------------------------------------------------------

  namelist /nml_sepmondata/ dir_in, dir_out, ibyr, ieyr, iemn

  !-----------------------------------------------------------------------

  iemn = 12
  open(10,file='namelist.sepmondata')
  read(10,nml=nml_sepmondata)
  close(10)

  !-----------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  !-----------------------------------------------------------------------

  LOOP_YEAR : do nyear = ibyr, ieyr

     write(flin1,'(1a,1a,i4.4)') trim(dir_in),'/sst-glb.',nyear
     write(6,*) 'reading temparature from ... ', trim(flin1)
     open(mtin1,file=trim(flin1),form='unformatted',access='direct',recl=4*imt*jmt)
     irec_sst = 0

     write(flin2,'(1a,1a,i4.4)') trim(dir_in),'/ice-glb.',nyear
     write(6,*) 'reading ice fractional area from ... ', trim(flin2)
     open(mtin2,file=trim(flin2),form='unformatted',access='direct',recl=4*imt*jmt)
     irec_ice = 0

     do month = 1, iemn

        ! open output file

        write(flot1,'(1a,1a,i4.4,i2.2)') trim(dir_out),'/sst-glb.',nyear, month
        open(mtot1,file=trim(flot1),form='unformatted',access='direct',recl=4*imt*jmt)
        irec_ssto = 0
        
        write(flot2,'(1a,1a,i4.4,i2.2)') trim(dir_out),'/ice-glb.',nyear, month
        open(mtot2,file=trim(flot2),form='unformatted',access='direct',recl=4*imt*jmt)
        irec_iceo = 0

        irec_sst = irec_sst + 1
        read(mtin1,rec=irec_sst) work_sst(1:imt,1:jmt)
        do j = 1, jmt
           do i = 1, imt
              if (work_sst(i,j) == rundef_sst) then
                 sst(i,j) = dundef_sst
              else
                 sst(i,j) = dble(work_sst(i,j))
              end if
           end do
        end do

        irec_ice = irec_ice + 1
        read(mtin2,rec=irec_ice) work_sst(1:imt,1:jmt)
        do j = 1, jmt
           do i = 1, imt
              if (work_sst(i,j) == rundef_sst) then
                 if (sst(i,j) == dundef_sst) then
                    ice(i,j) = dundef_sst
                 else
                    ice(i,j) = 0.0d0
                 end if
              else
                 ice(i,j) = min(dble(work_sst(i,j)),1.0d0)
              end if
           end do
        end do

        !------

        irec_ssto = irec_ssto + 1
        work_sst(1:imt,1:jmt) = real(sst(1:imt,1:jmt),4)
        write(mtot1,rec=irec_ssto) work_sst
        
        irec_iceo = irec_iceo + 1
        work_sst(1:imt,1:jmt) = real(ice(1:imt,1:jmt),4)
        write(mtot2,rec=irec_iceo) work_sst

        close(mtot2)
        close(mtot1)

     end do

  end do LOOP_YEAR

  close(mtin2)
  close(mtin1)

end program separate_cobesst_monthly
