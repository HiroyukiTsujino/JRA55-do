! -*-F90-*-
!
!------------------------------------------------------------------------
program make_monthly_data_tmp_cutoff

  use libmxe_para, only: libmxe_para__register &
                     & , clen, pi, radian, radian_r, radius &
                     & , type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register  &
                     & , type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, libmxe_topo__aexl &
                     & , type_libmxe_topo
  use libmxe_io, only: libmxe_io__register, type_libmxe_io

  implicit none

  ! original data

  integer(4) :: imf, jmf, kmf
  real(8),allocatable :: alonf(:), alatf(:)

  integer(4), parameter :: mtin1 = 80, mtin2 = 81, mtin3 = 82, mtin4 = 83
  integer(4), parameter :: mtin5 = 84, mtin6 = 85, mtin7 = 86, mtin8 = 87
  integer(4), parameter :: mtin9 = 88

  integer(4), parameter :: mtot1 = 90, mtot2 = 91, mtot3 = 92, mtot4 = 93
  integer(4), parameter :: mtot5 = 94, mtot6 = 95, mtot7 = 96, mtot8 = 97
  integer(4), parameter :: mtot9 = 98

  real(8), parameter :: avg_c0 = 61.846d0
  real(8), parameter :: avg_c1 =  1.107d0
  real(8), parameter :: amp_c0 = -21.841d0
  real(8), parameter :: amp_c1 = -0.447d0
  real(8), parameter :: phs_c0 = 0.298d0
  real(8) :: rsec
  real(8) :: cosfactor
  real(8) :: tmin, tmin60s, lat60s

  integer(4),parameter :: nsec_year = 365 * 86400
  integer(4) :: nsec

  ! snap shot

  real(4),allocatable :: SAT(:,:)
  real(8),allocatable :: SAT8(:,:)

  ! daily

  real(8),allocatable :: SATD(:,:)

  ! monthly

  real(8),allocatable :: SATM(:,:)

  integer(4) :: idmon(12)

  character(len=256) :: fin_tmp10m

  character(len=256) :: fout_tmp10m

  character(len=256) :: flout

  integer(4) :: lreclen

  integer(4) :: ireci1, ireci2, ireci3, ireci4
  integer(4) :: ireci5, ireci6, ireci7, ireci8
  integer(4) :: ireci9

  integer(4) :: ileap

  integer(4) :: i, j, m, n, nd, iyear, nyear

  integer(4), parameter :: lun = 10

  type(type_libmxe_para) :: orgp
  type(type_libmxe_grid) :: orgg
  type(type_libmxe_topo) :: orgt

  !+***!S++1++++*++++2++++*++++3++++*++++4++++*++++5++++*++++6++++*++++7++

  namelist /nml_org2mon_tmpcutoff/ fin_tmp10m, fout_tmp10m

  !-----------------------------------------------------------------------

  open(lun,file='namelist_org2monthly_tmpcutoff')
  read(lun,nml=nml_org2mon_tmpcutoff)
  close(lun)

  !-----------------------------------------------------------------------

  call libmxe_para__register(orgp, file_namelist='NAMELIST.MXE.CORE')

  imf = orgp%imut
  jmf = orgp%jmut
  kmf = orgp%km

  write(6,*) imf, jmf, kmf
  lreclen = 4 * imf * jmf

  call libmxe_grid__register(orgg,orgp)
  call libmxe_topo__register(orgt,orgp)

  allocate(alonf(0:imf+1),alatf(1:jmf))

  alonf(1:imf) = orgg%lonu(1:imf)
  alatf(1:jmf) = orgg%latu(1:jmf)

  alonf(0) = orgg%lonu(imf) - 360.d0
  alonf(imf+1) = orgg%lonu(1) + 360.d0

  allocate(SAT(1:imf,1:jmf))
  allocate(SAT8(1:imf,1:jmf))
  allocate(SATD(1:imf,1:jmf))
  allocate(SATM(1:imf,1:jmf))

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

  open(mtin4,file=fin_tmp10m ,form='unformatted',access='direct',recl=lreclen)

  ireci4 = 0

  !-----------------------------------------------------------------------

  nsec = - 3 * 3600

  do m = 1, 12

    write(6,*) ' month = ',m, ' days = ',idmon(m)

    write(flout,'(1a,i2.2)') trim(fout_tmp10m),m
    open(mtot4,file=flout,form='unformatted',access='direct',recl=lreclen)

    !-----------------------------------------------------------------------

    satm(1:imf,1:jmf) = 0.0d0

    !----------------------------------------------------------------------

    do n = 1, idmon(m)

      satd(1:imf,1:jmf) = 0.0d0

      do nd = 1, 4

        ireci4 = ireci4 + 1
        read(mtin4,rec=ireci4) sat

        nsec = nsec + 3600 * 6

        rsec = real(nsec,8) / real(nsec_year,8)
        cosfactor = cos(2.d0*pi*rsec - phs_c0)

        do j = 1, jmf
          do i = 1, imf
            sat8(i,j) = real(sat(i,j),8) - 273.15d0
          end do
        end do

        if ((0.0d0 < rsec) .and. (rsec < 1.0d0)) then
          write(6,*) ' rsec      = ', rsec
          write(6,*) ' cosfactor = ', cosfactor
        else
          write(6,*) ' Error in rsec, rsec = ', rsec
          stop
        end if

        lat60s = -60.0d0
        tmin60s = (avg_c0 + avg_c1 * lat60s) + (amp_c0 + amp_c1 * lat60s) * cosfactor

        do j = 1, jmf
          if (alatf(j) < -50.d0) then
            tmin = (avg_c0 + avg_c1 * alatf(j)) + (amp_c0 + amp_c1 * alatf(j)) * cosfactor
            tmin = min(tmin,tmin60s) ! ad hoc modification by MRI to suppress extremes
            do i = 1, imf
              sat8(i,j) = max(sat8(i,j),tmin)
            end do
          end if
        end do

        do j = 1, jmf
          do i = 1, imf
            satd(i,j) = satd(i,j) + sat8(i,j)
          end do
        end do

      end do

      satd(1:imf,1:jmf) = satd(1:imf,1:jmf) / 4.0d0
      satm(1:imf,1:jmf) = satm(1:imf,1:jmf) + satd(1:imf,1:jmf)

    end do

    satm(1:imf,1:jmf) = satm(1:imf,1:jmf) / real(idmon(m),8)

    sat(1:imf,1:jmf) = real(satm(1:imf,1:jmf),4) + 273.15

    ! output forcing

    write(mtot4,rec=1) sat

    close(mtot4)

  end do

  close(mtin4)

  !--------------------------------------------------------------------

end program make_monthly_data_tmp_cutoff
