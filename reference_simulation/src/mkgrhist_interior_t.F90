! -*-F90-*-
!=========================================================================
program mkgrads_hist_interior_t
  !-----------------------------------------------------------------------
  !     Make interior data from OGCM output
  !-----------------------------------------------------------------------

  use basin_param
  use grid_common

  implicit none

  real(4) :: tm(imut,jmut,km)
  real(4) :: dat2(imut,jmut)

  integer(4) :: month
  integer(4) :: i, j, k, n, nt, irec, nst, ned, inum, ktmp
  integer(4), parameter :: mtin = 77, mtout = 88, mtopo = 78
  integer(4), parameter :: mtot1 = 51, mtot2 = 52

  integer(4) :: ibyr, ieyr, iy

  integer(4) :: irec1, irec2

  character(256) :: ftopo, fgrid, fscale
  character(256) :: file_core_in, file_in, file_core_out, file_out
  character(256) :: file_out_depth

  logical :: l_out_depth

  real(4), parameter :: undef4 = -9.99e33
  real(4), parameter :: bad = -9.0e33

  !---------------------------------------------------------------------

  namelist /ioinfit/ ftopo, fgrid, fscale, &
       & file_core_in, file_core_out, &
       & file_out_depth, l_out_depth, &
       & ibyr, ieyr

  open (11,file='ioinfit.dat')
  read (11,nml=ioinfit)
  close(11)

  !---------------------------------------------------------------------

  call setgrd(ftopo, fgrid, fscale)

  if (l_out_depth) then
    open (mtot2,file=file_out_depth,form='unformatted',access='direct', &
         &      action='write', recl=4*imut*jmut)
    write(*,*) 'depth (T) written to ... ',trim(file_out_depth)
    write(mtot2,rec=1) real(hot(1:imut,1:jmut),4)
    close(mtot2)
  end if

  !---------------------------------------------------------------------

  do iy = ibyr, ieyr

    do month = 1, 12

      write(file_in,'(1a,i4.4,i2.2)') trim(file_core_in),iy,month
      open (mtin,file=file_in,form='unformatted',access='direct', &
           &      action='read', recl=4*imut*jmut*km)
      write(*,*) 'ocean reading from ... ',trim(file_in)

      write(file_out,'(1a,i4.4,i2.2)') trim(file_core_out),iy,month
      open(mtot1,file=file_out,form='unformatted',access='direct',  &
           &     action='write', recl=4*imut*jmut*km)

      write(*,*) 'data written to ..... ',trim(file_out)

      read(mtin,rec=1) tm

      close ( mtin )

      do j = 1, jmut
        do i = 1, imut
          ktmp = ktbtm(i,j)
          if (ktmp > 1) then
            if (tm(i,j,ktmp) < bad) then
              if (tm(i,j,km) > bad) then
                tm(i,j,ktmp) = tm(i,j,km)
                if (ktmp /= km) then
                  tm(i,j,km) = undef4
                end if
              else
                write(6,*) ' data is missing ', i, j, ktmp
              end if
            else
              if (tm(i,j,km) > bad) then
                tm(i,j,km) = undef4
              end if
            end if
          end if
        end do
      end do

      write(mtot1,rec=1) tm

      close ( mtot1 )

    end do

  end do


end program mkgrads_hist_interior_t
