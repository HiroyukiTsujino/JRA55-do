! -*-F90-*-
!==================================================================!
!                                                                  !
!                    Program mk_mask_divide                        !
!                                                                  !
!------------------------------------------------------------------!
program main

  use libmxe_para, only: libmxe_para__register, type_libmxe_para
  use libmxe_grid, only: libmxe_grid__register, type_libmxe_grid
  use libmxe_topo, only: libmxe_topo__register, type_libmxe_topo, &
       & libmxe_topo__aexl

  use libmxe_io, only: libmxe_io__register, type_libmxe_io
  use libmxe_grads, only: libmxe_grads__make, type_grads

  implicit none

  !-- arguments --

  integer(4),parameter :: lun = 91
  integer(4) :: imut, jmut

  type(type_libmxe_para) :: para
  type(type_libmxe_grid) :: grid
  type(type_libmxe_topo) :: topo

  character(1),allocatable :: indx(:,:)
  character(64) :: flout
  character(50) :: rform

  integer(4),allocatable :: idiv(:)
  integer(4),allocatable :: jdiv(:)
  integer(4) :: nidiv, njdiv
  integer(4) :: num_width = 150

  integer(4) :: i,j,k,mi,mj,n,length
  integer(4) :: i0,i1,j0,j1

  real(8) :: slat, slon, elat, elon

  character(len=1),save :: cgrid  !- "T": T-grid / "U": U-grid
  character(len=16),save :: cdir   !- diretory

  real(4),allocatable :: wind_mask(:,:)
  character(len=256) :: file_wind_mask

  !------ preprocessing: read settings and data ------

  namelist /nml_mkmask/ slat, slon, elat, elon, cgrid, cdir, &
       & file_wind_mask

  !---------------------------------------------------

  slat = -90.0d0
  elat =  90.0d0
  slon =   0.0d0
  elon = 360.0d0

  read(5,nml=nml_mkmask,iostat=i)
  if ( i /= 0 ) then
    write(*,*) ' Read error : namelist nml_mkmask'
    stop
  endif

  !---- settings ----
  call libmxe_para__register(para, file_namelist='NAMELIST.MXE.JRA55ocean')
  call libmxe_grid__register(grid, para)
  call libmxe_topo__register(topo, para)
  call libmxe_topo__aexl(topo, para)
  imut = para%imut
  jmut = para%jmut

  !---------------------------------------------------------

  allocate(wind_mask(1:imut,1:jmut))
  open(lun,file=file_wind_mask,form='unformatted',access='direct',recl=4*imut*jmut)
  read(lun,rec=1) wind_mask
  close(lun)

  !---------------------------------------------------------

  allocate(indx(1:imut,1:jmut))

  if (cgrid == 'U') then

    ! U-points

    do j = 1, jmut
      do i = 1, imut
        if (topo%aexl(i,j,1)==0.0d0) then
          indx(i,j) = '@' ! land
        else
          indx(i,j) = '0' ! sea
        end if
      end do
    end do

    do j = 1, jmut
      do i = 1, imut
        if ((slat <= grid%glatu(i,j)) .and. (grid%glatu(i,j) <= elat) &
             & .and. (slon <= grid%glonu(i,j)) .and. (grid%glonu(i,j) <= elon)) then
          if ((topo%aexl(i,j,1)==1.0d0) .and. (wind_mask(i,j) == 0.0)) then
            indx(i,j) = '5'
          end if
        end if
      end do
    end do

  end if

  if (cgrid == 'T') then

    ! T-points

    do j = 1, jmut
      do i = 1, imut
        if (topo%atexl(i,j,1)==0.0d0) then
          indx(i,j) = '@'
        else
          indx(i,j) = '0'
        end if
      end do
    end do

    do j = 1, jmut
      do i = 1, imut
        if ((slat <= grid%glatt(i,j)) .and. (grid%glatt(i,j) <= elat) &
             & .and. (slon <= grid%glont(i,j)) .and. (grid%glont(i,j) <= elon)) then
          if (topo%atexl(i,j,1)==1.0d0) then
            indx(i,j) = '5'
          end if
        end if
      end do
    end do

  end if

  !-------------------------------------------------------------

  nidiv = imut / num_width  + 2
  njdiv = jmut / num_width  + 2

  allocate(idiv(1:nidiv))
  allocate(jdiv(1:njdiv))

  idiv(1) = 0
  jdiv(1) = 0

  do mi = 2, nidiv - 1
    idiv(mi) = idiv(mi-1) + num_width
  end do
  idiv(nidiv) = imut

  do mj = 2, njdiv - 1
    jdiv(mj) = jdiv(mj-1) + num_width
  end do
  jdiv(njdiv) = jmut

  write(6,*) 'nidiv = ', nidiv
  write(6,*) 'njdiv = ', njdiv
  write(6,*) 'i=',(idiv(mi),mi=1,nidiv)
  write(6,*) 'j=',(jdiv(mj),mj=1,njdiv)

  n = 0
  do mj = 1, njdiv - 1
    do mi = 1, nidiv - 1
      n = n + 1
      write(flout,'(1a,1a,i2.2,a4)') trim(cdir),'/indx',n,'.dat'
      length = idiv(mi+1) - idiv(mi)
      write(rform,'(A4,I3,A3)') '(1X,',length,'A1)'
      write(6,*) 'format: ',rform
      open (lun,file=flout)
      do j = jdiv(mj+1),jdiv(mj)+1,-1
        write(lun,rform) (indx(i,j),i=idiv(mi)+1,idiv(mi+1))
      end do
      close(lun)
    end do
  end do

end program main
