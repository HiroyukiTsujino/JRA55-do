!-*-F90-*-
program divide_mask

  !------------------------------------------------------------------

  integer(4), parameter :: imut = 1440
  integer(4), parameter :: jmut = 720
  integer(4), parameter :: mtin1 = 81

  real(4) :: mask(imut,jmut)
  character(1),dimension(imut,jmut) :: indx
  character(16) :: flout
  character(50) :: rform

  character(256) :: file_mask

  integer(4),dimension(9) :: idiv=(/0,180,360,540,720,900,1080,1260,1440/)
  integer(4),dimension(9) :: jdiv=(/0,90,180,270,360,450,540,630,720/)

  integer(4) :: i, j, k, mi, mj, length

  !-----------------------------------------------------------------------

  file_mask = '/worke/htsujino/RUNOFF_YOSHIMURA/mask.gd'
  open(mtin1,file=file_mask,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'MASK read from ',trim(file_mask)
  read(mtin1,rec=1) mask(1:imut,1:jmut)
  close(mtin1)

  !-----------------------------------------------------------------------

  do j = 1, jmut
    do i = 1, imut

      if (mask(i,j)==0.0) then
        indx(i,j) = '0'
      else
        indx(i,j) = '1'
      end if

    end do
  end do

  n = 0
  do mj = 1, 8
    do mi = 1, 8
      n = n + 1
      write(flout,'(1A,I2.2,A4)') 'mask/indx',n,'.dat'
      length = idiv(mi+1) - idiv(mi)
      write(rform,'(A4,I3,A3)') '(1X,',length,'A1)'
      write(6,*) 'format: ',rform
      open (21,file=flout)
      do j = jdiv(mj+1),jdiv(mj)+1,-1
        write(21,rform) (indx(i,j),i=idiv(mi)+1,idiv(mi+1))
      end do
      close(21)
    end do
  end do

end program divide_mask
