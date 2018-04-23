! -*-F90-*-
!
!------------------------------------------------------------------------
program make_monthly_data

  implicit none

  integer(4), parameter :: imf = 192
  integer(4), parameter :: jmf = 94

  integer(4), parameter :: mtin1 = 80, mtin2 = 81, mtin3 = 82, mtin4 = 83
  integer(4), parameter :: mtin5 = 84, mtin6 = 85, mtin7 = 86, mtin8 = 87
  integer(4), parameter :: mtin9 = 88

  integer(4), parameter :: mtot1 = 90, mtot2 = 91, mtot3 = 92, mtot4 = 93
  integer(4), parameter :: mtot5 = 94, mtot6 = 95, mtot7 = 96, mtot8 = 97
  integer(4), parameter :: mtot9 = 98

  ! snap shot

  real(4) :: U10(imf,jmf), V10(imf,jmf)
  real(4) :: QSH(imf,jmf), QLO(imf,jmf)
  real(4) :: SAT(imf,jmf), QAR(imf,jmf)
  real(4) :: SLP(imf,jmf), WDV(imf,jmf)
  real(4) :: PCP(imf,jmf)

  ! daily

  real(8) :: U10D(imf,jmf), V10D(imf,jmf)
  real(8) :: SATD(imf,jmf), QARD(imf,jmf)
  real(8) :: SLPD(imf,jmf), WDVD(imf,jmf)

  ! monthly

  real(8) :: U10M(imf,jmf), V10M(imf,jmf)
  real(8) :: QSHM(imf,jmf), QLOM(imf,jmf)
  real(8) :: SATM(imf,jmf), QARM(imf,jmf)
  real(8) :: SLPM(imf,jmf), WDVM(imf,jmf)
  !real(8) :: PCPM(imf,jmf)

  integer(4) :: idmon(12)

  character(len=256) :: fin_u10m, fin_v10m, fin_wind10m
  character(len=256) :: fin_tmp10m, fin_sph10m, fin_slprs
  character(len=256) :: fin_dlwrf, fin_dswrf
  character(len=256) :: fin_prcp

  character(len=256) :: fout_u10m, fout_v10m, fout_wind10m
  character(len=256) :: fout_tmp10m, fout_sph10m, fout_slprs
  character(len=256) :: fout_dlwrf, fout_dswrf
  character(len=256) :: fout_prcp

  character(len=256) :: flout

  integer(4) :: lreclen

  integer(4) :: ireci1, ireci2, ireci3, ireci4
  integer(4) :: ireci5, ireci6, ireci7, ireci8
  integer(4) :: ireci9

  integer(4) :: ileap

  integer(4) :: i, j, m, n, nd, iyear, nyear

  integer(4), parameter :: lun = 10

  !+***!S++1++++*++++2++++*++++3++++*++++4++++*++++5++++*++++6++++*++++7++

  namelist /nml_org2mon/  fin_u10m, fin_v10m, fin_wind10m, &
                      &   fin_tmp10m, fin_sph10m, fin_slprs, &
                      &   fin_dlwrf, fin_dswrf, fin_prcp, &
                      &   fout_u10m, fout_v10m, fout_wind10m, &
                      &   fout_tmp10m, fout_sph10m, fout_slprs, &
                      &   fout_dlwrf, fout_dswrf, fout_prcp

  !-----------------------------------------------------------------------

  open(lun,file='namelist_org2monthly')
  read(lun,nml=nml_org2mon)
  close(lun)

  lreclen = imf*jmf*4

  !-----------------------------------------------------------------------

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

  !--------------------------------------------------------------------

  open(mtin1,file=fin_u10m   ,form='unformatted',access='direct',recl=lreclen)
  open(mtin2,file=fin_v10m   ,form='unformatted',access='direct',recl=lreclen)
  open(mtin3,file=fin_wind10m,form='unformatted',access='direct',recl=lreclen)
  open(mtin4,file=fin_tmp10m ,form='unformatted',access='direct',recl=lreclen)
  open(mtin5,file=fin_sph10m ,form='unformatted',access='direct',recl=lreclen)
  open(mtin6,file=fin_slprs  ,form='unformatted',access='direct',recl=lreclen)
  open(mtin7,file=fin_dswrf  ,form='unformatted',access='direct',recl=lreclen)
  open(mtin8,file=fin_dlwrf  ,form='unformatted',access='direct',recl=lreclen)
  open(mtin9,file=fin_prcp   ,form='unformatted',access='direct',recl=lreclen)

  ireci1 = 0
  ireci2 = 0
  ireci3 = 0
  ireci4 = 0
  ireci5 = 0
  ireci6 = 0
  ireci7 = 0
  ireci8 = 0
  ireci9 = 0

  !-----------------------------------------------------------------------

  do m = 1, 12

    write(6,*) ' month = ',m, ' days = ',idmon(m)

    write(flout,'(1a,i2.2)') trim(fout_u10m),m
    open(mtot1,file=flout,form='unformatted',access='direct',recl=lreclen)

    write(flout,'(1a,i2.2)') trim(fout_v10m),m
    open(mtot2,file=flout,form='unformatted',access='direct',recl=lreclen)

    write(flout,'(1a,i2.2)') trim(fout_wind10m),m
    open(mtot3,file=flout,form='unformatted',access='direct',recl=lreclen)

    write(flout,'(1a,i2.2)') trim(fout_tmp10m),m
    open(mtot4,file=flout,form='unformatted',access='direct',recl=lreclen)

    write(flout,'(1a,i2.2)') trim(fout_sph10m),m
    open(mtot5,file=flout,form='unformatted',access='direct',recl=lreclen)

    write(flout,'(1a,i2.2)') trim(fout_slprs),m
    open(mtot6,file=flout,form='unformatted',access='direct',recl=lreclen)

    write(flout,'(1a,i2.2)') trim(fout_dswrf),m
    open(mtot7,file=flout,form='unformatted',access='direct',recl=lreclen)

    write(flout,'(1a,i2.2)') trim(fout_dlwrf),m
    open(mtot8,file=flout,form='unformatted',access='direct',recl=lreclen)

    write(flout,'(1a,i2.2)') trim(fout_prcp),m
    open(mtot9,file=flout,form='unformatted',access='direct',recl=lreclen)

    !-----------------------------------------------------------------------

    u10m(1:imf,1:jmf) = 0.0d0
    v10m(1:imf,1:jmf) = 0.0d0

    qshm(1:imf,1:jmf) = 0.0d0
    qlom(1:imf,1:jmf) = 0.0d0

    satm(1:imf,1:jmf) = 0.0d0
    qarm(1:imf,1:jmf) = 0.0d0
    wdvm(1:imf,1:jmf) = 0.0d0
    slpm(1:imf,1:jmf) = 0.0d0
    !pcpm(1:imf,1:jmf) = 0.0d0

    !----------------------------------------------------------------------

    ireci9 = ireci9 + 1
    read(mtin9,rec=ireci9) pcp
    !pcpm(1:imf,1:jmf) = real(pcp(1:imf,1:jmf),8)

    write(mtot9,rec=1) pcp
    close(mtot9)

    do n = 1, idmon(m)

      ireci7 = ireci7 + 1
      read(mtin7,rec=ireci7) qsh
      ireci8 = ireci8 + 1
      read(mtin8,rec=ireci8) qlo

      qshm(1:imf,1:jmf) = qshm(1:imf,1:jmf) + real(qsh(1:imf,1:jmf),8)
      qlom(1:imf,1:jmf) = qlom(1:imf,1:jmf) + real(qlo(1:imf,1:jmf),8)

      u10d(1:imf,1:jmf) = 0.0d0
      v10d(1:imf,1:jmf) = 0.0d0
      satd(1:imf,1:jmf) = 0.0d0
      qard(1:imf,1:jmf) = 0.0d0
      wdvd(1:imf,1:jmf) = 0.0d0
      slpd(1:imf,1:jmf) = 0.0d0

      do nd = 1, 4

        ireci1 = ireci1 + 1
        read(mtin1,rec=ireci1) u10
        ireci2 = ireci2 + 1
        read(mtin2,rec=ireci2) v10
        ireci3 = ireci3 + 1
        read(mtin3,rec=ireci3) wdv
        ireci4 = ireci4 + 1
        read(mtin4,rec=ireci4) sat
        ireci5 = ireci5 + 1
        read(mtin5,rec=ireci5) qar
        ireci6 = ireci6 + 1
        read(mtin6,rec=ireci6) slp

        do j = 1, jmf
          do i = 1, imf
            u10d(i,j) = u10d(i,j) + real(u10(i,j),8)
            v10d(i,j) = v10d(i,j) + real(v10(i,j),8)
            wdvd(i,j) = wdvd(i,j) + real(wdv(i,j),8)
            satd(i,j) = satd(i,j) + real(sat(i,j),8)
            qard(i,j) = qard(i,j) + real(qar(i,j),8)
            slpd(i,j) = slpd(i,j) + real(slp(i,j),8)
          end do
        end do

      end do

      u10d(1:imf,1:jmf) = u10d(1:imf,1:jmf) / 4.0d0
      v10d(1:imf,1:jmf) = v10d(1:imf,1:jmf) / 4.0d0
      wdvd(1:imf,1:jmf) = wdvd(1:imf,1:jmf) / 4.0d0
      satd(1:imf,1:jmf) = satd(1:imf,1:jmf) / 4.0d0
      qard(1:imf,1:jmf) = qard(1:imf,1:jmf) / 4.0d0
      slpd(1:imf,1:jmf) = slpd(1:imf,1:jmf) / 4.0d0

      u10m(1:imf,1:jmf) = u10m(1:imf,1:jmf) + u10d(1:imf,1:jmf)
      v10m(1:imf,1:jmf) = v10m(1:imf,1:jmf) + v10d(1:imf,1:jmf)
      wdvm(1:imf,1:jmf) = wdvm(1:imf,1:jmf) + wdvd(1:imf,1:jmf)
      satm(1:imf,1:jmf) = satm(1:imf,1:jmf) + satd(1:imf,1:jmf)
      qarm(1:imf,1:jmf) = qarm(1:imf,1:jmf) + qard(1:imf,1:jmf)
      slpm(1:imf,1:jmf) = slpm(1:imf,1:jmf) + slpd(1:imf,1:jmf)

    end do

    u10m(1:imf,1:jmf) = u10m(1:imf,1:jmf) / real(idmon(m),8)
    v10m(1:imf,1:jmf) = v10m(1:imf,1:jmf) / real(idmon(m),8)
    wdvm(1:imf,1:jmf) = wdvm(1:imf,1:jmf) / real(idmon(m),8)

    satm(1:imf,1:jmf) = satm(1:imf,1:jmf) / real(idmon(m),8)
    qarm(1:imf,1:jmf) = qarm(1:imf,1:jmf) / real(idmon(m),8)
    slpm(1:imf,1:jmf) = slpm(1:imf,1:jmf) / real(idmon(m),8)

    qshm(1:imf,1:jmf) = qshm(1:imf,1:jmf) / real(idmon(m),8)
    qlom(1:imf,1:jmf) = qlom(1:imf,1:jmf) / real(idmon(m),8)

    !-----

    u10(1:imf,1:jmf) = real(u10m(1:imf,1:jmf),4)
    v10(1:imf,1:jmf) = real(v10m(1:imf,1:jmf),4)
    wdv(1:imf,1:jmf) = real(wdvm(1:imf,1:jmf),4)

    sat(1:imf,1:jmf) = real(satm(1:imf,1:jmf),4)
    qar(1:imf,1:jmf) = real(qarm(1:imf,1:jmf),4)
    slp(1:imf,1:jmf) = real(slpm(1:imf,1:jmf),4)

    qsh(1:imf,1:jmf) = real(qshm(1:imf,1:jmf),4)
    qlo(1:imf,1:jmf) = real(qlom(1:imf,1:jmf),4)

    ! output forcing

    write(mtot1,rec=1) u10
    write(mtot2,rec=1) v10
    write(mtot3,rec=1) wdv
    write(mtot4,rec=1) sat
    write(mtot5,rec=1) qar
    write(mtot6,rec=1) slp
    write(mtot7,rec=1) qsh
    write(mtot8,rec=1) qlo

    close(mtot8)
    close(mtot7)
    close(mtot6)
    close(mtot5)
    close(mtot4)
    close(mtot3)
    close(mtot2)
    close(mtot1)

  enddo

  close(mtin9)
  close(mtin8)
  close(mtin7)
  close(mtin6)
  close(mtin5)
  close(mtin4)
  close(mtin3)
  close(mtin2)
  close(mtin1)

  !--------------------------------------------------------------------

end program make_monthly_data
