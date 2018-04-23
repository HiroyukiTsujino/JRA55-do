!-*-F90-*-
program unite_mask

  integer(4), parameter :: imut = 360
  integer(4), parameter :: jmut = 180
  integer(4), parameter :: mtin1 = 81
  !
  real(4),dimension(imut,jmut) :: mask

  character(1),dimension(imut,jmut) :: indx
  character(256) :: flnin
  character(50) :: rform

  character(256) :: file_mask

  integer(4),dimension(3) :: idiv=(/0,180,360/)
  integer(4),dimension(3) :: jdiv=(/0,90,180/)
  !
  integer(4) :: i,j,k,mi,mj
  integer(4), parameter :: mtgrd=20
  !
  !--------------------------------------------------------------------------

  n = 0
  do mj = 1, 2
    do mi = 1, 2
      n = n + 1
      k = idiv(mi+1) - idiv(mi)
!      write(flnin,'(1A,I2.2,A4)') 'mask_green/indx',n,'.dat'
      write(flnin,'(1A,I2.2,A4)') 'mask_antarctic/indx',n,'.dat'
!      write(flnin,'(1A,I2.2,A4)') 'mask/indx',n,'.dat'
      write(6,*) 'reading from... ',trim(flnin)
      write(rform,'(A4,I3,A3)') '(1X,',k,'A1)'
      write(6,*) 'format: ',rform
      open (21,file=flnin)
      do j = jdiv(mj+1), jdiv(mj)+1, -1
        read(21,rform) (indx(i,j),i=idiv(mi)+1,idiv(mi+1))
      enddo
      close(21)
    enddo
  enddo

  mask(:,:) = 0
  do j = 1, jmut
    do i = 1, imut
      if (indx(i,j)=='5') then
        mask(i,j) = 1.0
      end if
    end do
  end do
  !
  ! Output Grads Data File
  !
!  file_mask = '/work116/htsujino/CORE/data/runoff_mask_green.gd'
  file_mask = '/work116/htsujino/CORE/data/runoff_mask_antarctic.gd'
!  file_mask = '/work116/htsujino/CORE/data/runoff_mask_poles.gd'
  open(24,file=file_mask,form='unformatted', &
       & access='direct',recl=4*imut*jmut)
  write(24,rec=1) mask
  close(24)
  !
end program unite_mask
