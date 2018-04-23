! -*-F90-*-
!
!------------------------- diag_bulk.F90 ----------------------------
!
!  Information:
!     diangnose bulk heat flux of NOCS
!
!-------------------------------------------------------------------
program diagnosis_heat_flux

  implicit none

  real(8), parameter :: ro = 1.036d3
  real(8), parameter :: ems = 1.00d0
  real(8), parameter :: stfblz = 5.67d-5
  real(8), parameter :: tab = 273.15d0
  real(8), parameter :: sst_freeze = -1.8d0
  real(8), parameter :: cnst1ly = 0.069d0
  real(8), parameter :: cnst2ly = 0.011d0
  real(8), parameter :: PI =       3.141592653589793D0  ! 円周率
  real(8), parameter :: RADIAN =   180.D0/PI
  real(8), parameter :: radian_r = 1.d0/radian

  real(8), parameter :: q0 = 0.98d0
  real(8), parameter :: q1 = 640380.d0
  real(8), parameter :: q2 = -5107.4d0
  real(8), parameter :: rgas = 287.04d0
  real(8), parameter :: rhoa = 1.22d0

  !-------------------------------------------

  integer(4), parameter :: imt = 360, jmt = 180

  real(4), parameter :: rundef_sst = -9.99e33
  real(8), parameter :: dundef_sst = -9.99d33

  real(8) :: alond(imt), alatd(jmt)

  real(4) :: work_nocs(imt,jmt)

  real(8) :: atexl(imt,jmt)
  real(8) :: sst(imt,jmt)

  real(8) :: us(imt,jmt), vs(imt,jmt)
  real(8) :: sat(imt,jmt), qar(imt,jmt)
  real(8) :: slp(imt,jmt), wdv(imt,jmt)

  ! following items are diagnosed in this program

  real(8) :: wsx(imt,jmt), wsy(imt,jmt)
  real(8) :: qla(imt,jmt), qsn(imt,jmt)
  real(8) :: evp(imt,jmt)

  real(8) :: tmp2m(imt,jmt), sph2m(imt,jmt)
  real(8) :: rlh2m(imt,jmt)

  !-----------------------------------------------

  integer(4), parameter :: mtin1 = 61, mtin2 = 62, mtin3 = 63, mtin4 = 64, mtin5 = 65
  integer(4), parameter :: mtot1 = 70, mtot2 = 72, mtot3 = 73, mtot4 = 74, mtot5 = 75, mtot6 = 76

  character(256) :: flin1, flin2, flin3, flin4, flin5
  character(256) :: flot1, flot2, flot3, flot4, flot5, flot6

  integer(4) :: irec1, irec2, irec3, irec4

  integer(4) :: i, j, k, l, m, n, nyear

  integer(4),parameter  :: ibyr=1973, ieyr=2006

  real(8) :: qsat

  !-----------------------------------------------------------------------
  ! set NOCS grid points

  do i = 1, imt
    alond(i) = 0.5d0 + 1.0d0 * (i-1)
  end do

  do j = 1, jmt
    alatd(j) = - 89.5d0 + 1.0d0 * (j-1)
  end do

  !---------------------------------------------------------------------

  LOOP_YEAR : do nyear = ibyr, ieyr

    do m = 1, 12

      write(flin1,'(1A,I4.4,I2.2)') &
           & '/worke/htsujino/NOCS/grads/tmp10m.',nyear, m
      open(mtin1,file=flin1,form='unformatted',access='direct',recl=4*imt*jmt)
      write(6,*) 'Air temperature read from ',trim(flin1)

      write(flin2,'(1A,I4.4,I2.2)') &
           & '/worke/htsujino/NOCS/grads/qair.',nyear, m
      open(mtin2,file=flin2,form='unformatted',access='direct',recl=4*imt*jmt)
      write(6,*) 'Specific humidity read from ',trim(flin2)

      write(flin3,'(1A,I4.4,I2.2)') &
           & '/worke/htsujino/NOCS/grads/wind10m.',nyear, m
      open(mtin3,file=flin3,form='unformatted',access='direct',recl=4*imt*jmt)
      write(6,*) 'Wind speed read from ',trim(flin3)

      write(flin4,'(1A,I4.4,I2.2)') &
           & '/worke/htsujino/NOCS/grads/slp.',nyear, m
      open(mtin4,file=flin4,form='unformatted',access='direct',recl=4*imt*jmt)
      write(6,*) 'SLP read from ',trim(flin4)

      write(flin5,'(1A,I4.4,I2.2)') &
           & '/worke/htsujino/NOCS/grads/sst.',nyear, m
      open(mtin5,file=flin5,form='unformatted',access='direct',recl=4*imt*jmt)
      write(6,*) 'SST read from ',trim(flin5)

      ! open output file
      
      write(flot1,'(1A,I4.4,I2.2)') &
           & '/worke/htsujino/NOCS/grads_diag/tmp2m.',nyear, m
      open(mtot1,file=flot1,form='unformatted',access='direct',recl=4*imt*jmt)

      write(flot2,'(1A,I4.4,I2.2)') &
           & '/worke/htsujino/NOCS/grads_diag/sph2m.',nyear, m
      open(mtot2,file=flot2,form='unformatted',access='direct',recl=4*imt*jmt)
      
      write(flot3,'(1A,I4.4,I2.2)') &
           & '/worke/htsujino/NOCS/grads_diag/latent.',nyear, m
      open(mtot3,file=flot3,form='unformatted',access='direct',recl=4*imt*jmt)
      
      write(flot4,'(1A,I4.4,I2.2)') &
           & '/worke/htsujino/NOCS/grads_diag/sensible.',nyear, m
      open(mtot4,file=flot4,form='unformatted',access='direct',recl=4*imt*jmt)
      
      write(flot5,'(1A,I4.4,I2.2)') &
           & '/worke/htsujino/NOCS/grads_diag/evap.',nyear, m
      open(mtot5,file=flot5,form='unformatted',access='direct',recl=4*imt*jmt)
      
      write(flot6,'(1A,I4.4,I2.2)') &
           & '/worke/htsujino/NOCS/grads_diag/rlh2m.',nyear, m
      open(mtot6,file=flot6,form='unformatted',access='direct',recl=4*imt*jmt)
      
      ! read atmospheric data

      !read(mtin3,rec=1) work_nocs(1:imt,1:jmt)
      !us10m(1:imt,1:jmt) = work_nocs(1:imt,1:jmt) * 100.0
      !
      !read(mtin3,rec=1) work_nocs(1:imt,1:jmt)
      !vs10m(1:imt,1:jmt) = work_nocs(1:imt,1:jmt) * 100.0

      read(mtin1,rec=1) work_nocs(1:imt,1:jmt)
      sat(1:imt,1:jmt) = real(work_nocs(1:imt,1:jmt),8)

      read(mtin2,rec=1) work_nocs(1:imt,1:jmt)
      qar(1:imt,1:jmt) = real(rundef_sst,8)
      where(work_nocs(1:imt,1:jmt) /= rundef_sst)
        qar(1:imt,1:jmt) = real(work_nocs(1:imt,1:jmt),8) * 1.0d-3
      end where

      read(mtin3,rec=1) work_nocs(1:imt,1:jmt)
      wdv(1:imt,1:jmt) = real(rundef_sst,8)
      where(work_nocs(1:imt,1:jmt) /= rundef_sst)
        wdv(1:imt,1:jmt) = real(work_nocs(1:imt,1:jmt),8) * 1.0d2
      end where

      read(mtin4,rec=1) work_nocs(1:imt,1:jmt)
      slp(1:imt,1:jmt) = real(work_nocs(1:imt,1:jmt),8)

      read(mtin5,rec=1) work_nocs(1:imt,1:jmt)
      sst(1:imt,1:jmt) = real(work_nocs(1:imt,1:jmt),8)

      do j = 1, jmt
        do i = 1, imt
          if ((sst(i,j) == real(rundef_sst,8)) .or. &
              (sat(i,j) == real(rundef_sst,8)) .or. &
              (qar(i,j) == real(rundef_sst,8)) .or. &
              (wdv(i,j) == real(rundef_sst,8)) .or. &
              (slp(i,j) == real(rundef_sst,8))) then
            atexl(i,j) = 0.0d0
          else
            atexl(i,j) = 1.0d0
          end if
        end do
      end do

      tmp2m(:,:) = 0.0d0
      sph2m(:,:) = 0.0d0

      call bulk(wsx,wsy,qla,qsn,evp,us,vs,sat,qar,wdv,slp,sst,&
           & tmp2m, sph2m, &
           & imt,jmt,atexl)

      do j = 1, jmt
        do i = 1, imt
          if (atexl(i,j) == 0.0d0) then
            tmp2m(i,j) = real(rundef_sst,8)
            sph2m(i,j) = real(rundef_sst,8)
            qla  (i,j) = real(rundef_sst,8)
            qsn  (i,j) = real(rundef_sst,8)
            evp  (i,j) = real(rundef_sst,8)
          end if
        end do
      end do

      do j = 1, jmt
        do i = 1, imt
          if ((tmp2m(i,j) /= real(rundef_sst,8)) .and. (sph2m(i,j) /= real(rundef_sst,8))) then
            qsat = q0 * q1 * exp(q2 / (tmp2m(i,j) + tab)) / rhoa 
            rlh2m(i,j) = sph2m(i,j) * (1.0d0 - qsat) / (qsat * (1.0d0 - sph2m(i,j)))
          else
            rlh2m(i,j) = real(rundef_sst,8)
          end if
        end do
      end do

      ! output data

      !work_nocs(1:imt,1:jmt) = real(wsx(1:imt,1:jmt),4)
      !write(mtot1,rec=1) work_nocs

      !irec_out = irec_out + 1
      !work_nocs(1:imt,1:jmt) = real(wsy(1:imt,1:jmt),4)
      !write(mtot1,rec=1) work_nocs

      work_nocs(1:imt,1:jmt) = real(tmp2m(1:imt,1:jmt),4)
      write(mtot1,rec=1) work_nocs

      work_nocs(1:imt,1:jmt) = real(sph2m(1:imt,1:jmt),4)
      write(mtot2,rec=1) work_nocs

      work_nocs(1:imt,1:jmt) = real(qla(1:imt,1:jmt),4)
      write(mtot3,rec=1) work_nocs

      work_nocs(1:imt,1:jmt) = real(qsn(1:imt,1:jmt),4)
      write(mtot4,rec=1) work_nocs

      work_nocs(1:imt,1:jmt) = real(evp(1:imt,1:jmt),4)
      write(mtot5,rec=1) work_nocs

      work_nocs(1:imt,1:jmt) = real(rlh2m(1:imt,1:jmt),4)
      write(mtot6,rec=1) work_nocs

    end do

    close(mtin5)
    close(mtin4)
    close(mtin3)
    close(mtin2)
    close(mtin1)

    close(mtot6)
    close(mtot5)
    close(mtot4)
    close(mtot3)
    close(mtot2)
    close(mtot1)

  end do LOOP_YEAR

end program diagnosis_heat_flux
