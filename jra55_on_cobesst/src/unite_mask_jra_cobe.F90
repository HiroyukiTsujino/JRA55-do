! -*-F90-*-
!==============================================================
program unite_mask_jra_and_cobe
!==============================================================

  implicit none

  integer(4),parameter :: imax = 360
  integer(4),parameter :: jmax = 180

  integer(4) :: i, j, k

  integer(4),parameter :: mtin1 = 31, mtin2 = 32
  integer(4),parameter :: mtot1 = 51, mtot2 = 52

  integer(4),parameter :: lrec=imax*jmax*4

  real(4) :: dat2(imax,jmax)

  real(8) :: mask_cobe(imax,jmax)
  real(8) :: mask_jra (imax,jmax)

  real(8)    :: umask(imax,jmax)
  integer(4) :: imask(imax,jmax)

  character(255) :: flnin1
  character(255) :: flnin2
  character(255) :: flnot1, flnot2

  real(8), parameter :: PI = 3.141592653589793D0

!-----------------------------------------------------------------

  namelist /nml_unitemask/ &
       &  flnin1, flnin2, flnot1, flnot2

!-----------------------------------------------------------------

  open (10,file='namelist.unite_jra_cobe')
  read (10,nml_unitemask) 
  close(10)

!-----------------------------------------------------------------

  mask_cobe(:,:) = 0.0d0
  open(mtin1, file=flnin1, form='unformatted', access='direct', recl=lrec)
  write(6,*) ' file : ', trim(flnin1), ' opened '
  read(mtin1,rec=1) dat2
  do j = 1, jmax
    do i = 1, imax
      mask_cobe(i,j) = real(dat2(i,j),8)
    end do
  end do
  close(mtin1)

  mask_jra(:,:) = 0.0d0
  open(mtin2, file=flnin2, form='unformatted', access='direct', recl=lrec)
  write(6,*) ' file : ', trim(flnin2), ' opened '
  read(mtin2,rec=2) dat2
  do j = 1, jmax
    do i = 1, imax
      mask_jra(i,j) = real(dat2(i,j),8)
    end do
  end do
  close(mtin2)

 !---------------------------------------------------------------

  do j = 1, jmax
    do i = 1, imax
      umask(i,j) = mask_cobe(i,j) * mask_jra(i,j)
    end do
  end do

  imask(:,:) = nint(umask(:,:))

  write(6,*) ' United mask (topo.d) = ',trim(flnot1)
  open (mtot1,file=flnot1,form='unformatted')
  write(mtot1) imask, imask
  close(mtot1)

  write(6,*) ' United mask (mask.gd) = ',trim(flnot2)
  open(mtot2, file=flnot2, form='unformatted', access='direct', recl=lrec)
  write(mtot2,rec=1) real(umask(1:imax,1:jmax),4)
  close(mtot2)

end program unite_mask_jra_and_cobe
