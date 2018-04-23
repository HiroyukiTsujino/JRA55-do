! -*-F90-*-
!mk_baro_america.F90
!====================================================
!
!   Make GrADS data from OGCM restart file
!
!====================================================
program make_baro_america

  use basin_param
  use grid_common

  implicit none

  integer(4), parameter :: ibu = 3, ieu = imut - 2  ! cyclic
  integer(4), parameter :: jbu = 2, jeu = jmut - 3  ! tripolar

  integer(4), parameter :: ibt = 3, iet = imut - 2  ! cyclic
  integer(4), parameter :: jbt = 2, jet = jmut - 2  ! tripolar

  real(4) :: btsf(imut,jmut)
  real(4) :: acc

  integer(4), parameter :: mtin1 = 71, mtin2 = 72
  integer(4), parameter :: mtot1 = 81
  character(256) :: flnin1, flnin2
  character(256) :: flnin1_base, flnin2_base
  character(256) :: flnot1
  character(256) :: flnot1_base
  character(256) :: ftopo, fgrid, fscale

  integer(4) :: nday, month, nyear, nd, nbyr, neyr
  integer(4), parameter :: ndata = 12
  integer(4) :: ndmon(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)

  integer(4) :: irec1, irec2, irec3, ireco
  integer(4) :: i, j, k, m, n
  integer(4) :: ibyr, ieyr
  
  real(4), parameter :: bad = -1.0e30

  !-----------------------------------------------------------------------

  namelist /ioinfbra/ flnin1_base, flnin2_base, &
       & flnot1_base, ftopo, fgrid, fscale, ibyr, ieyr
  open (11,file='ioinfbra.dat')
  read (11,nml=ioinfbra)
  close(11)

  !-----------------------------------------------------------------------
  ! set grid points

  call setgrd(ftopo, fgrid, fscale)

  do nyear = ibyr, ieyr

    write(flnin1,'(1a,i4.4)') trim(flnin1_base),nyear
    open(mtin1, file=flnin1, access='direct', recl=4*imut*jmut)

    write(6,*) 'Grads data read from... ', trim(flnin1)

    write(flnin2,'(1a,i4.4)') trim(flnin2_base),nyear
    open(mtin2, file=flnin2, access='direct', recl=4)

    write(6,*) 'Grads data read from... ', trim(flnin2)

    read(mtin1,rec=1) ((btsf(i,j),i=1,imut),j=1,jmut)

    read(mtin2,rec=1) acc

    close(mtin2)
    close(mtin1)

    do j = 1, jmut
      do i = 1, imut
        if (btsf(i,j) > bad) then
          btsf(i,j) = btsf(i,j) + acc
        end if
      end do
    end do

    write(flnot1,'(1a,i4.4)') trim(flnot1_base),nyear
    write(*,*) ' GrADs data written to... ', trim(flnot1)

    open(mtot1,file=flnot1,form='unformatted',access='direct',recl=4*imut*jmut)

    write(mtot1,rec=1) btsf(1:imut,1:jmut)

    close(mtot1)

  end do

end program make_baro_america
