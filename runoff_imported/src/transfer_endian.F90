!-*-F90-*-
program transfer_endian
  implicit none
  integer(4),parameter :: nx  =  1440,  ny =   720
  integer(4),parameter :: nnx = 21600, nny = 10800
  integer(4) :: nextx(nx,ny), nexty(nx,ny)
  integer(4) :: outx (nx,ny), outy (nx,ny)
  integer(4) :: i, j
  integer(1) :: flwdir(nnx,nny)

!---------------- nextxy -----------
  open(11,file='data_etc/nextxy_little_endian.bin',form='unformatted',access='direct',&
       & recl = 4 * nx * ny)
  read(11,rec=1) nextx
  read(11,rec=2) nexty
  close(11)

  do j = 1, ny
    do i = 1, nx
      call endian_convert(nextx(i,j))
      call endian_convert(nexty(i,j))
    end do
  end do

!  open(21,file='data_etc/nextxy_big_endian.bin',form='unformatted',access='direct',&
!       & recl = 4 * nx * ny)
!  write(21,rec=1) nextx
!  write(21,rec=2) nexty
!  close(21)

  do j = 1, ny
    do i = 1, nx
      if ( nextx(i,j) > 0 ) then
        if ( nextx(i,j) <= nx/2 ) then
          nextx(i,j) = nextx(i,j) + nx/2
        else
          nextx(i,j) = nextx(i,j) - nx/2
        end if
      end if
      if ( nexty(i,j) > 0 ) then
        nexty(i,j) = ny + 1 - nexty(i,j)
      end if
    end do
  end do  

  open(21,file='data_etc/nextxy_big_endian_noyrev_lon0strt.bin',form='unformatted',access='direct',&
       & recl = 4)
  do j = 1, ny
    do i = 1, nx
      if ( i <= nx/2 ) then
        write(21,rec=nx*(j-1)+i) nextx(i+nx/2,ny+1-j)
      else
        write(21,rec=nx*(j-1)+i) nextx(i-nx/2,ny+1-j)
      end if
    end do
  end do

  do j = 1, ny
    do i = 1, nx
      if ( i <= nx/2 ) then
        write(21,rec=nx*ny+nx*(j-1)+i) nexty(i+nx/2,ny+1-j)
      else
        write(21,rec=nx*ny+nx*(j-1)+i) nexty(i-nx/2,ny+1-j)
      end if
    end do
  end do
  close(21)

!  open(21,file='nextxy_big_endian_noyrev_lon0strt.bin',form='unformatted',access='direct',&
!       & recl = 4 * nx * ny)
!  read(21,rec=1) nextx
!  read(21,rec=2) nexty
!  close(21)
!
!  write(*,*) 'hoge', nextx(311, 635)
!  stop

!---------------- out_xy -----------

  open(11,file='data_etc/out_xy_little_endian.bin',form='unformatted',access='direct',&
       & recl = 4 * nx * ny)
  read(11,rec=1) outx
  read(11,rec=2) outy
  close(11)
  
  do j = 1, ny
    do i = 1, nx
      call endian_convert(outx(i,j))
      call endian_convert(outy(i,j))
    end do
  end do

  do j = 1, ny
    do i = 1, nx
      if ( outx(i,j) /= 0 ) then
        if ( outx(i,j) <= nnx/2 ) then
          outx(i,j) = outx(i,j) + nnx/2
        else
          outx(i,j) = outx(i,j) - nnx/2
        end if
      end if
      if ( outy(i,j) /= 0 ) then
        outy(i,j) = nny+1-outy(i,j)
      end if
    end do
  end do

  open(21,file='data_etc/out_xy_big_endian_noyrev_lon0strt.bin',form='unformatted',access='direct',&
       & recl = 4)
  do j = 1, ny
    do i = 1, nx
      if ( i <= nx/2 ) then
        write(21,rec=nx*(j-1)+i) outx(i+nx/2,ny+1-j)
      else
        write(21,rec=nx*(j-1)+i) outx(i-nx/2,ny+1-j)
      end if
    end do
  end do

  do j = 1, ny
    do i = 1, nx
      if ( i <= nx/2 ) then
        write(21,rec=nx*ny+nx*(j-1)+i) outy(i+nx/2,ny+1-j)
      else
        write(21,rec=nx*ny+nx*(j-1)+i) outy(i-nx/2,ny+1-j)
      end if
    end do
  end do
  close(21)

!--------------------- flwdir ---
  open(11,file='data_etc/flwdir_little_endian.bin',form='unformatted',access='direct',&
       & recl = nnx * nny)
  read(11,rec=1) flwdir
  close(11)

  open(21,file='data_etc/flwdir_big_endian_noyrev_lon0strt.bin',form='unformatted',access='direct',&
       & recl = 1)
  do j = 1, nny
    do i = 1, nnx
      if ( i <= nnx/2 ) then
        write(21,rec=nnx*(j-1)+i) flwdir(i+nnx/2,nny+1-j)
      else
        write(21,rec=nnx*(j-1)+i) flwdir(i-nnx/2,nny+1-j)
      end if
    end do
  end do
  close(21)
!-----------------------------------------------------------------------  
  
end program transfer_endian

subroutine endian_convert(c)
  implicit none
  integer(1),intent(inout) :: c(1:4)

  integer(1) :: c_tmp(1:4)
  integer(4) :: i
  do i = 1, 4
    c_tmp(5-i) = c(i)
  end do
  do i = 1, 4
    c(i) = c_tmp(i)
  end do
end subroutine endian_convert
