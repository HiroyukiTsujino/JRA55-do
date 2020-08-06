!-*-F90-*-
program main

  implicit none

  integer(4), parameter :: nx = 1440, ny = 720
  real(4) :: roff(nx,ny), solid(nx,ny)
  real(4) :: mask(nx,ny), undef
  real(8) :: clim, annual, monthly, daily
  real(8) :: clim_cmf, annual_cmf, monthly_cmf, daily_cmf
  real(8) :: clim_grn, annual_grn, monthly_grn, daily_grn
  real(8) :: clim_caa, annual_caa, monthly_caa, daily_caa
  real(8) :: clim_ant, annual_ant, monthly_ant, daily_ant

  real(8) :: clim_sld, annual_sld, monthly_sld, daily_sld
  real(8) :: clim_sld_cmf, annual_sld_cmf, monthly_sld_cmf, daily_sld_cmf
  real(8) :: clim_sld_grn, annual_sld_grn, monthly_sld_grn, daily_sld_grn
  real(8) :: clim_sld_caa, annual_sld_caa, monthly_sld_caa, daily_sld_caa
  real(8) :: clim_sld_ant, annual_sld_ant, monthly_sld_ant, daily_sld_ant

  character(256) :: cfriv, cdate
  character(256) :: file_river_in
  character(256) :: file_solid_in
  character(256) :: file_riv_ann
  character(256) :: file_riv_mon
  character(256) :: file_daily, file_clim

  character(256) :: file_nextxy
  logical :: l_sep_grn_caa
  character(256) :: file_sep_grn_caa
  real(4) :: riv_indx(nx,ny)

  integer(4) :: mo(12,2), yr(2)

  integer(4) :: nt, nyr, nm, nd, irec, irecs, i, j, tday, ii, jj
  
  integer(4) :: irec1, irec2, irec3

  character(128) :: fl_liquid_base, fl_solid_base
  character(128) :: dir_out

  logical :: l_apply_area
  character(128) :: file_area
  real(4) :: area(nx,ny)

  logical :: l_solid_clim

  integer(4) :: nextx(nx,ny), nexty(nx,ny)
  integer(4),parameter :: index_cmf = -9
  integer(4),parameter :: index_grn = -888
  integer(4),parameter :: index_ant = -777

  integer(4) :: ibyr, ieyr

  data yr / 365, 366 /
  data mo / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, &
          & 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
  data undef / 1e20 /

  !--------------------------------------------------------------------

  namelist /nml_separated_runoff/ &
       & fl_liquid_base, fl_solid_base, &
       & l_solid_clim, &
       & l_sep_grn_caa, file_sep_grn_caa, &
       & dir_out, ibyr, ieyr, &
       & l_apply_area, file_area, &
       & file_nextxy

  !--------------------------------------------------------------------
 
  l_sep_grn_caa=.false.
  open(10, file='namelist.separated_runoff_noyrev')
  read(10, nml=nml_separated_runoff)
  close(10)

  !--------------------------------------------------------------------

  open(10,file=file_nextxy,form='unformatted',status='old',access='direct',recl=4*nx*ny)
  read(10,rec=1) nextx
  read(10,rec=2) nexty
  close(10)

  if (l_apply_area) then
    open(10,file=file_area,form='unformatted',status='old',access='direct',recl=4*nx*ny)
    read(10,rec=1) area
    close(10)
  else
    area(:,:) = 1.e0
  end if

  if (l_sep_grn_caa) then
    open(10,file=file_sep_grn_caa,form='unformatted',status='old',access='direct',recl=4*nx*ny)
    read(10,rec=1) riv_indx
    close(10)
  else
    riv_indx(:,:) = 1.e0
  end if

  !--------------------------------------------------------------------

  write(file_clim,'(1a,1a)') trim(dir_out),'/clim.txt'
  open(50,file=file_clim)

  clim     = 0.d0
  clim_cmf = 0.d0
  clim_grn = 0.d0
  clim_caa = 0.d0
  clim_ant = 0.d0
  clim_sld     = 0.d0
  clim_sld_cmf = 0.d0
  clim_sld_grn = 0.d0
  clim_sld_caa = 0.d0
  clim_sld_ant = 0.d0

  tday = 0

  do nyr = ibyr, ieyr

    write(*,*) nyr

    write(file_daily,'(1a,1a,i4.4)') trim(dir_out),'/daily.',nyr
    open(20,file=file_daily,form='unformatted',access='direct',recl=4)
    irec1 = 0

    annual = 0.d0
    annual_cmf = 0.d0
    annual_grn = 0.d0
    annual_caa = 0.d0
    annual_ant = 0.d0
    annual_sld = 0.d0
    annual_sld_cmf = 0.d0
    annual_sld_grn = 0.d0
    annual_sld_caa = 0.d0
    annual_sld_ant = 0.d0

    write(file_river_in,'(1a,1a,i4.4)') trim(fl_liquid_base),'.',nyr
    open(10,file=file_river_in,form='unformatted',status='old',access='direct',recl=4*nx*ny)
    irec = 0

    write(file_solid_in,'(1a,1a,i4.4)') trim(fl_solid_base),'.',nyr
    open(11,file=file_solid_in,form='unformatted',status='old',access='direct',recl=4*nx*ny)
    irecs = 0

    write(file_riv_ann,'(1a,1a,i4.4)') trim(dir_out),'/runoff.',nyr
    open(40,file=file_riv_ann,form='unformatted',access='direct',recl=4)
    irec3 = 0

    if ( lleap(nyr) ) then
      nt = 2
    else
      nt = 1
    end if
    do nm = 1, 12

      monthly = 0.d0
      monthly_cmf = 0.d0
      monthly_grn = 0.d0
      monthly_caa = 0.d0
      monthly_ant = 0.d0

      monthly_sld = 0.d0
      monthly_sld_cmf = 0.d0
      monthly_sld_grn = 0.d0
      monthly_sld_caa = 0.d0
      monthly_sld_ant = 0.d0

      write(file_riv_mon,'(1a,1a,i4.4,i2.2)') trim(dir_out),'/runoff.',nyr,nm
      open(30,file=file_riv_mon,form='unformatted',access='direct',recl=4)
      irec2 = 0

      do nd = 1, mo(nm,nt)

        daily = 0.d0
        daily_cmf = 0.d0
        daily_grn = 0.d0
        daily_caa = 0.d0
        daily_ant = 0.d0

        daily_sld = 0.d0
        daily_sld_cmf = 0.d0
        daily_sld_grn = 0.d0
        daily_sld_caa = 0.d0
        daily_sld_ant = 0.d0

        irec = irec + 1
        read(10,rec=irec) roff

        if (l_solid_clim) then
          irecs = 1
        else
          irecs = irecs + 1
        end if
        read(11,rec=irecs) solid

        do j = 1, ny
          do i = 1, nx
            if ( abs(roff(i,j)) > 1.0d30 ) then
              write(6,*) ' infinite at ', i,j, roff(i,j)
              roff(i,j) = 0.
            end if
            if ( abs(roff(i,j)) > 0.0d0 .and. abs(roff(i,j)) < 1.0d-30 ) then
              write(6,*) ' tiny at ', i,j, roff(i,j)
              roff(i,j) = 0.
            end if
            daily = daily + real(roff(i,j)*area(i,j),8)
            daily_sld = daily_sld + real(solid(i,j)*area(i,j),8)
            if ((nextx(i,j) == index_cmf) .and. (nexty(i,j) == index_cmf)) then
              daily_cmf = daily_cmf + real(roff(i,j)*area(i,j),8)
              daily_sld_cmf = daily_sld_cmf + real(solid(i,j)*area(i,j),8)
            end if
            if ((nextx(i,j) == index_grn) .and. (nexty(i,j) == index_grn)) then
              if (l_sep_grn_caa) then
                if (int(riv_indx(i,j)+1.e-4) == 8) then
                  daily_grn = daily_grn + real(roff(i,j)*area(i,j),8)
                  daily_sld_grn = daily_sld_grn + real(solid(i,j)*area(i,j),8)
                end if
                if (int(riv_indx(i,j)+1.e-4) == 6) then
                  daily_caa = daily_caa + real(roff(i,j)*area(i,j),8)
                  daily_sld_caa = daily_sld_caa + real(solid(i,j)*area(i,j),8)
                end if
              else
                daily_grn = daily_grn + real(roff(i,j)*area(i,j),8)
                daily_sld_grn = daily_sld_grn + real(solid(i,j)*area(i,j),8)
              end if
            end if
            if ((nextx(i,j) == index_ant) .and. (nexty(i,j) == index_ant)) then
              daily_ant = daily_ant + real(roff(i,j)*area(i,j),8)
              daily_sld_ant = daily_sld_ant + real(solid(i,j)*area(i,j),8)
            end if
          end do
        end do
        write(cdate,'(I4.4,I2.2,I2.2)') nyr,nm,nd
        irec1 = irec1 + 1
        write(20,rec=irec1) real(daily,4)
        irec1 = irec1 + 1
        write(20,rec=irec1) real(daily_cmf,4)
        irec1 = irec1 + 1
        write(20,rec=irec1) real(daily_grn,4)
        irec1 = irec1 + 1
        write(20,rec=irec1) real(daily_caa,4)
        irec1 = irec1 + 1
        write(20,rec=irec1) real(daily_ant,4)
        irec1 = irec1 + 1
        write(20,rec=irec1) real(daily_sld,4)
        irec1 = irec1 + 1
        write(20,rec=irec1) real(daily_sld_cmf,4)
        irec1 = irec1 + 1
        write(20,rec=irec1) real(daily_sld_grn,4)
        irec1 = irec1 + 1
        write(20,rec=irec1) real(daily_sld_caa,4)
        irec1 = irec1 + 1
        write(20,rec=irec1) real(daily_sld_ant,4)

        monthly = monthly + daily
        monthly_cmf = monthly_cmf + daily_cmf
        monthly_grn = monthly_grn + daily_grn
        monthly_caa = monthly_caa + daily_caa
        monthly_ant = monthly_ant + daily_ant

        monthly_sld = monthly_sld + daily_sld
        monthly_sld_cmf = monthly_sld_cmf + daily_sld_cmf
        monthly_sld_grn = monthly_sld_grn + daily_sld_grn
        monthly_sld_caa = monthly_sld_caa + daily_sld_caa
        monthly_sld_ant = monthly_sld_ant + daily_sld_ant
      end do

      annual = annual + monthly
      annual_cmf = annual_cmf + monthly_cmf
      annual_grn = annual_grn + monthly_grn
      annual_caa = annual_caa + monthly_caa
      annual_ant = annual_ant + monthly_ant

      annual_sld = annual_sld + monthly_sld
      annual_sld_cmf = annual_sld_cmf + monthly_sld_cmf
      annual_sld_grn = annual_sld_grn + monthly_sld_grn
      annual_sld_caa = annual_sld_caa + monthly_sld_caa
      annual_sld_ant = annual_sld_ant + monthly_sld_ant

      monthly = monthly / real(mo(nm,nt),8)
      monthly_cmf = monthly_cmf / real(mo(nm,nt),8)
      monthly_grn = monthly_grn / real(mo(nm,nt),8)
      monthly_caa = monthly_caa / real(mo(nm,nt),8)
      monthly_ant = monthly_ant / real(mo(nm,nt),8)

      monthly_sld = monthly_sld / real(mo(nm,nt),8)
      monthly_sld_cmf = monthly_sld_cmf / real(mo(nm,nt),8)
      monthly_sld_grn = monthly_sld_grn / real(mo(nm,nt),8)
      monthly_sld_caa = monthly_sld_caa / real(mo(nm,nt),8)
      monthly_sld_ant = monthly_sld_ant / real(mo(nm,nt),8)

      write(cdate,'(I4.4,I2.2)') nyr,nm
      irec2 = irec2 + 1
      write(30,rec=irec2) real(monthly,4)
      irec2 = irec2 + 1
      write(30,rec=irec2) real(monthly_cmf,4)
      irec2 = irec2 + 1
      write(30,rec=irec2) real(monthly_grn,4)
      irec2 = irec2 + 1
      write(30,rec=irec2) real(monthly_caa,4)
      irec2 = irec2 + 1
      write(30,rec=irec2) real(monthly_ant,4)
      irec2 = irec2 + 1
      write(30,rec=irec2) real(monthly_sld,4)
      irec2 = irec2 + 1
      write(30,rec=irec2) real(monthly_sld_cmf,4)
      irec2 = irec2 + 1
      write(30,rec=irec2) real(monthly_sld_grn,4)
      irec2 = irec2 + 1
      write(30,rec=irec2) real(monthly_sld_caa,4)
      irec2 = irec2 + 1
      write(30,rec=irec2) real(monthly_sld_ant,4)
      close(30)
    end do
    close(20)

    clim = clim + annual
    clim_cmf = clim_cmf + annual_cmf
    clim_grn = clim_grn + annual_grn
    clim_caa = clim_caa + annual_caa
    clim_ant = clim_ant + annual_ant

    clim_sld = clim_sld + annual_sld
    clim_sld_cmf = clim_sld_cmf + annual_sld_cmf
    clim_sld_grn = clim_sld_grn + annual_sld_grn
    clim_sld_caa = clim_sld_caa + annual_sld_caa
    clim_sld_ant = clim_sld_ant + annual_sld_ant

    annual = annual / real(yr(nt),8)
    annual_cmf = annual_cmf / real(yr(nt),8)
    annual_grn = annual_grn / real(yr(nt),8)
    annual_caa = annual_caa / real(yr(nt),8)
    annual_ant = annual_ant / real(yr(nt),8)
    annual_sld = annual_sld / real(yr(nt),8)
    annual_sld_cmf = annual_sld_cmf / real(yr(nt),8)
    annual_sld_grn = annual_sld_grn / real(yr(nt),8)
    annual_sld_caa = annual_sld_caa / real(yr(nt),8)
    annual_sld_ant = annual_sld_ant / real(yr(nt),8)

    write(6,*) 'total  (liquid) ', annual
    write(6,*) 'CaMa   (liquid) ', annual_cmf
    write(6,*) 'Green  (liquid) ', annual_grn
    write(6,*) 'CAA    (liquid) ', annual_caa
    write(6,*) 'Antarc (liquid) ', annual_ant
    write(6,*) 'total  (solid)  ', annual_sld
    write(6,*) 'CaMa   (solid)  ', annual_sld_cmf
    write(6,*) 'Green  (solid)  ', annual_sld_grn
    write(6,*) 'CAA    (solid)  ', annual_sld_caa
    write(6,*) 'Antarc (solid)  ', annual_sld_ant
    irec3 = irec3 + 1
    write(40,rec=irec3) real(annual,4)
    irec3 = irec3 + 1
    write(40,rec=irec3) real(annual_cmf,4)
    irec3 = irec3 + 1
    write(40,rec=irec3) real(annual_grn,4)
    irec3 = irec3 + 1
    write(40,rec=irec3) real(annual_caa,4)
    irec3 = irec3 + 1
    write(40,rec=irec3) real(annual_ant,4)
    irec3 = irec3 + 1
    write(40,rec=irec3) real(annual_sld,4)
    irec3 = irec3 + 1
    write(40,rec=irec3) real(annual_sld_cmf,4)
    irec3 = irec3 + 1
    write(40,rec=irec3) real(annual_sld_grn,4)
    irec3 = irec3 + 1
    write(40,rec=irec3) real(annual_sld_caa,4)
    irec3 = irec3 + 1
    write(40,rec=irec3) real(annual_sld_ant,4)
    close(40)
    tday = tday + irec
    close(11)
    close(10)
  end do

  clim = clim / real(tday,8)
  clim_cmf = clim_cmf / real(tday,8)
  clim_grn = clim_grn / real(tday,8)
  clim_caa = clim_caa / real(tday,8)
  clim_ant = clim_ant / real(tday,8)

  clim_sld = clim_sld / real(tday,8)
  clim_sld_cmf = clim_sld_cmf / real(tday,8)
  clim_sld_grn = clim_sld_grn / real(tday,8)
  clim_sld_caa = clim_sld_caa / real(tday,8)
  clim_sld_ant = clim_sld_ant / real(tday,8)
  write(50,*) 'total  (liquid) ', clim
  write(50,*) 'CaMa   (liquid) ', clim_cmf
  write(50,*) 'Green  (liquid) ', clim_grn
  write(50,*) 'CAA    (liquid) ', clim_caa
  write(50,*) 'Antarc (liquid) ', clim_ant

  write(50,*) 'total  (solid)  ', clim_sld
  write(50,*) 'CaMa   (solid)  ', clim_sld_cmf
  write(50,*) 'Green  (solid)  ', clim_sld_grn
  write(50,*) 'CAA    (solid)  ', clim_sld_caa
  write(50,*) 'Antarc (solid)  ', clim_sld_ant

contains

  logical function lleap( year )
    implicit none
    integer(4) :: year
    logical :: lflg
    if ( mod(year,4) == 0 ) then
      if ( mod(year,100) == 0 ) then
        if ( mod(year,400) == 0 ) then
          lflg = .true.
        else
          lflg = .false.
        end if
      else
        lflg = .true.
      end if
    else
      lflg = .false.
    end if
    lleap = lflg
  end function lleap

end program main
