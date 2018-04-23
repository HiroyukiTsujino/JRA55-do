!-*-F90-*-
program main
  implicit none
  integer(4), parameter :: nx = 1440, ny = 720
  character(256) :: cfriv
  integer(4) :: nextx(nx,ny), nexty(nx,ny)
  integer(4) :: ii, jj, i, j

  !----------------------------------------------------------

  cfriv='/worke/htsujino/RUNOFF_YOSHIMURA/nextxy.bin'
  !cfriv='/worke/htsujino/RUNOFF_YOSHIMURA/nextxy_wo_lake.bin'

  open(10,file=cfriv,form='unformatted',status='old',access='direct',recl=4*nx*ny)
  read(10,rec=1) nextx
  read(10,rec=2) nexty
  close(10)

  do j = 1, ny
    do i = 1, nx
      jj = nexty(i,j)
      ii = nextx(i,j)
!      if ( ii > 0 .and. jj > 0 ) then
!        if ( nextx(ii,jj) == -9 ) write(*,*) i,j, ii,jj
!      end if
      if ( ii == -9 .and. jj /= -9 ) write(*,*) i,j, ii,jj
      if ( ii /= -9 .and. jj == -9 ) write(*,*) i,j, ii,jj
    end do
  end do

end program main
  
