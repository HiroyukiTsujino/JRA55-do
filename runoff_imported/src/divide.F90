!-*-F90-*-
program divide_mask

  !------------------------------------------------------------------

  integer(4), parameter :: imut = 1440
  integer(4), parameter :: jmut = 720
  integer(4), parameter :: mtin1 = 81

  integer(4) :: imaskx(imut,jmut)
  integer(4) :: imasky(imut,jmut)
  real(4) :: mask(imut,jmut)
  character(1),dimension(imut,jmut) :: indx
  character(16) :: flout
  character(50) :: rform

  character(256) :: file_nextxy
  integer(4),parameter :: next_Grn = -888
  integer(4),parameter :: next_Ant = -777
  integer(4),parameter :: next_mouth = -9

  integer(4),dimension(9) :: idiv=(/0,180,360,540,720,900,1080,1260,1440/)
  integer(4),dimension(9) :: jdiv=(/0,90,180,270,360,450,540,630,720/)

  integer(4) :: i, j, k, mi, mj, length

  !-----------------------------------------------------------------------

  file_nextxy = '/denkei-shared/og1/htsujino/SURF_FLUX/forcing/data_etc/nextxy_wo_lake_big_endian_noyrev_lon0strt_rm_GA.bin'
  open(mtin1,file=file_nextxy,form='unformatted',access='direct',recl=4*imut*jmut)
  write(6,*) 'MASK read from ',trim(file_nextxy)
  read(mtin1,rec=1) imaskx(1:imut,1:jmut)
  read(mtin1,rec=2) imasky(1:imut,1:jmut)
  close(mtin1)

  !-----------------------------------------------------------------------

  do j = 1, jmut
    do i = 1, imut

      if (imaskx(i,j)==-888 .and. imasky(i,j)==-888) then
        indx(i,j) = '8'
      else if (imaskx(i,j)==-777 .and. imasky(i,j)==-777) then
        indx(i,j) = '7'
      else if (imaskx(i,j)==-9 .and. imasky(i,j)==-9) then
        indx(i,j) = '1'
      else
        indx(i,j) = '0'
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
