! -*-F90-*-
!=========================================================================
program mkgrads_hist_tsclim
  !-----------------------------------------------------------------------
  !     Make GrADS data from OGCM output
  !-----------------------------------------------------------------------

  use basin_param
  use grid_common

  implicit none

  real(4) :: tm(imut,jmut,km), sm(imut,jmut,km)
  real(4) :: dat2(imut,jmut)

  integer(4) :: month
  integer(4) :: i, j, k, n, nt, irec, nst, ned, inum
  integer(4), parameter :: mtin = 77, mtout = 88, mtopo = 78
  integer(4), parameter :: mtot1 = 51, mtot2 = 52

  integer(4) :: irec1, irec2

  character(256) :: ftopo, fgrid, fscale
  character(256) :: file_in, file_org, file_out
  character(256) :: file_out_t, file_out_s

  real(4), parameter :: undef4 = -9.99e33
  real(4), parameter :: bad = -9.0e33

  !---------------------------------------------------------------------

  namelist /ioinfcl/ ftopo, fgrid, fscale, file_in, file_org
  open (11,file='ioinfcl.dat')
  read (11,nml=ioinfcl)
  close(11)

  !---------------------------------------------------------------------

  call setgrd(ftopo, fgrid, fscale)

  !---------------------------------------------------------------------

  open (mtin,file=file_in,form='unformatted')
  write(*,*) 'ocean reading from ... ',trim(file_in)

  do month = 1, 12

    write(file_out_t,'(1A,1A,I2.2)') trim(file_org),'_t.1000',month
    write(file_out_s,'(1A,1A,I2.2)') trim(file_org),'_s.1000',month

    open(mtot1,file=file_out_t,form='unformatted',access='direct', &
         &     recl=4*imut*jmut)
    open(mtot2,file=file_out_s,form='unformatted',access='direct', &
         &     recl=4*imut*jmut)

    write(*,*) 'Temperature data written to ..... ',trim(file_out_t)
    write(*,*) 'Salinity data written to ........ ',trim(file_out_s)

    irec1 = 0
    irec2 = 0

    read(mtin) tm, sm

    !--------------
    ! Temeperature

    do k = 1, km

      do j = 1, jmut
        do i = 1, imut
          if (atexl(i,j,k) /= 0.0d0) then
            if (tm(i,j,k) < bad) then
              if (tm(i,j,km) > bad) then
                dat2(i,j) = tm(i,j,km)
              else
                write(6,*) ' data is missing ', i, j, k
              end if
            else
              dat2(i,j) = tm(i,j,k)
            end if
          else
            dat2(i,j) = undef4
          end if
        end do
      end do

      if (k == km) then
        do j = 1, jmut
          do i = 1, imut
            if (tm(i,j,km) > bad) then
              dat2(i,j) = tm(i,j,k)
            end if
          end do
        end do
      end if

      irec1 = irec1 + 1
      write(mtot1,rec=irec1) dat2

    end do

    !----------
    ! Salinity    

    do k = 1, km

      do j = 1, jmut
        do i = 1, imut
          if (atexl(i,j,k) /= 0.0d0) then
            if (sm(i,j,k) < bad) then
              if (sm(i,j,km) > bad) then
                dat2(i,j) = sm(i,j,km)
              else
                write(6,*) ' data is missing ', i, j, k
              end if
            else
              dat2(i,j) = sm(i,j,k)
            end if
          else
            dat2(i,j) = undef4
          end if
        end do
      end do
      
      if (k == km) then
        do j = 1, jmut
          do i = 1, imut
            if (sm(i,j,km) > bad) then
              dat2(i,j) = sm(i,j,k)
            end if
          end do
        end do
      end if
      
      irec2 = irec2 + 1
      write(mtot2,rec=irec2) dat2

    end do

  end do

  close ( mtot2 )
  close ( mtot1 )

  close ( mtin )

end program mkgrads_hist_tsclim
