! -*-F90-*-
!==============================================================
program extract_one_record

  implicit none

  integer(4) :: imax
  integer(4) :: jmax
  integer(4) :: kmax

  integer(4) :: i, j, k

  integer(4),parameter :: nf_in = 31
  integer(4),parameter :: nf_out = 41

  integer(4) :: reclen, irec

  real(4) :: undef_in, undef_out

  real(4),allocatable :: dat4(:,:,:)

  character(255) :: infile, infile_base
  character(255) :: outfile, outfile_base

!-----------------------------------------------------------------

  namelist /nml_extract_rec/ &
       &  imax, jmax, kmax,  &
       &  infile, undef_in, outfile, undef_out, &
       &  irec

!-----------------------------------------------------------------

  open(10,file='namelist.extract')
  read(10,nml_extract_rec) 
  close(10)

  reclen = 4 * imax * jmax * kmax

  allocate(dat4(imax,jmax,kmax))

!-----------------------------------------------------------------

  open(nf_in,file=infile,form='unformatted',access='direct',recl=reclen)
  write(6,*) ' file   : ', trim(infile), ' opened '
  write(6,*) ' record : ', irec
  read(nf_in,rec=irec) dat4
  close(nf_in)

  do k = 1, kmax
    do j = 1, jmax
      do i = 1, imax
        if (dat4(i,j,k) == undef_in) then
          dat4(i,j,k) = undef_out
        end if
      end do
    end do
  end do

  write(6,*) ' outfile = ',trim(outfile)
  open (nf_out,file=outfile,access='direct',form='unformatted',recl=reclen)
  write(nf_out,rec=1) dat4
  close(nf_out)

end program extract_one_record
