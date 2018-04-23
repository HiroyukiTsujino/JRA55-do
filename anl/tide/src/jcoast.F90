! -*-F90-*-
!- Read the dataset "Chouseki Kansoku CD-ROM".
module jcoast
  implicit none


  integer,public,parameter :: njcoast = 24 * 365
  public :: jcoast__read
  public :: jcoast__mean_25h


contains


subroutine jcoast__read( filei, h )
  implicit none

  character(*),intent(in) :: filei    !- data file
  real(4),intent(out) :: h(njcoast)   !- surface height (data)

  integer,parameter :: lun=10

  open(lun,file=filei,recl=njcoast*4,access='direct',form='unformatted' &
        & , action='read' )
    read(lun,rec=1) h
  close(lun)

end subroutine jcoast__read


subroutine jcoast__mean_25h( h, ha )
  implicit none

  real(4),intent(in) :: h(njcoast)
  real(4),intent(out) :: ha(njcoast)

  integer,parameter :: nwidth = 25
  integer,parameter :: nwidth_half = nwidth / 2

  integer :: n, n0, n1

  ha(:) = 0.d0
  if ( njcoast < nwidth ) return

  n0 = 1 + nwidth_half
  n1 = njcoast - nwidth_half
  do n = n0, n1
    ha(n) = sum( h( n - nwidth_half : n + nwidth_half ) )
  enddo

  ha(n0:n1) = ha(n0:n1) / real( nwidth )

end subroutine jcoast__mean_25h


end module jcoast
