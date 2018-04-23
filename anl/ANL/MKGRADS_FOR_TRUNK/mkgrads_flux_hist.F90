! -*-F90-*-
!=========================================================================
program mkgrads_hist_flux
  !-----------------------------------------------------------------------
  !     Make GrADS data from OGCM output
  !-----------------------------------------------------------------------

  use basin_param
  use grid_common

  implicit none

  real(4) :: dat2(imut,jmut)
  real(4) :: dat3(imut,jmut,6)

  integer(4) :: iy, is
  integer(4) :: i, j, m, n, mt, nt, irec, nst, ned, inum
  integer(4) :: NKAI,MONTH,IDAY,IHOUR,IMIN
  integer(4), parameter :: mtin=77, mtout=88

  character(len=256) :: file_in, file_out, cnum

  !---------------------------------------------------------------------

  call getarg (1, file_in )
  call getarg (2, file_out)
  call getarg (3, cnum )

  read(cnum, '(I10)') inum
  write(*,*) 'reading ', inum ,' data'

  open(mtin, file=file_in,form='unformatted')
  write(*,*) 'reading from ... ', trim(file_in)

  open(mtout, file=file_out,form='unformatted',access='direct',recl=4*imut*jmut)
  write(*,*) 'output to ... ', trim(file_out)
  irec = 0

  !------------------------------------------------------------------------

  read (mtin) NKAI,MONTH,IDAY,IHOUR,IMIN
  write(*,*) NKAI,MONTH,IDAY,IHOUR,IMIN

!!!!!  read (mtin) dat3

  do n = 1, 9

    write(6,*) ' Processing item ', n
    read (mtin) dat2
    irec = irec + 1
    write(mtout,rec=irec) dat2

  end do

  close ( mtout )
  close ( mtin )

end program mkgrads_hist_flux
