! -*-F90-*-
!==================================================================
program read_littleendian_output_bigendian
  !----------------------------------------------------------------
  ! Information:
  !   Create direct access file (GrADS) of
  !     the NOCS surface data
  !----------------------------------------------------------------

  implicit none

  integer(4), parameter :: imut = 360, jmut = 180
  integer(4), parameter :: mtin1 = 81, mtin2 = 82
  integer(4), parameter :: mtot1 = 91, mtot2 = 92

  real(4) :: dat4(imut,jmut)
  real(8) :: dat8(imut,jmut)

  real(4) :: swdn(imut,jmut)
  real(4) :: lwdn(imut,jmut)
  real(4) :: work4(imut,jmut)

  integer(4) :: idmon(12)

  character(128) :: flot1, flot2
  character(128) :: flin1, flin2

  integer(4) :: i, j, k, l, m, n

  integer(4) :: nmocount
  integer(4) :: ireco1, ireco2
  integer(4) :: nyear
  integer(4) :: mst, med

  real(4),parameter :: undef_srb_sw = -999.e0
  real(4),parameter :: undef_srb_lw = -999.e0
  real(4),parameter :: undef_mxe = -9.99e33


  !--------------------------------------------------------------------
  ! general settings

  idmon(1:12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  !---------------------------------------------------------------------
  ! main year
  !

  do nyear = 1983, 2007

    mst = 1
    med = 12
    if (nyear == 1983) mst = 7
    if (nyear == 2007) med = 6

    do m = mst, med

      !----------------------------------------------------------------------
      ! open netcdf file

      write(flin1,'(1a,i4.4,i2.2,1a)') &
           & '/worke/htsujino/SRB/rel3.0_qc/orgdata/srb_rel3.0_qcsw_monthly_',nyear,m,'.ieee'
      write(flin2,'(1a,i4.4,i2.2,1a)') &
           & '/worke/htsujino/SRB/rel3.0_qc/orgdata/srb_rel3.0_qclw_monthly_',nyear,m,'.ieee'

      !----------------------------------------------------------------------

      open(mtin1,file=flin1,form='unformatted',access='direct',&
           & convert='little_endian',recl=4*imut*jmut)
      read(mtin1,rec=4) dat4
      close(mtin1)

      do j = 1, jmut
        do i = 1, imut
          if (dat4(i,j) == undef_srb_sw) then
            swdn(i,j) = undef_mxe
          else
            swdn(i,j) = dat4(i,j)
          end if
        end do
      end do

      ! open output file

      write(flot1,'(1a,i4.4,i2.2)') &
           & '/worke/htsujino/SRB/rel3.0_qc/grads/swdn.',nyear,m
      open(mtot1,file=flot1,form='unformatted',access='direct',recl=4*imut*jmut)
      write(6,*) 'Downward short wave written to ',trim(flot1)
      write(mtot1,rec=1) swdn
      close(mtot1)

      !------------------------------------------------------------------------

      open(mtin2,file=flin2,form='unformatted',access='direct',&
           & convert='little_endian',recl=4*imut*jmut)
      read(mtin2,rec=1) dat4
      close(mtin2)

      do j = 1, jmut
        do i = 1, imut
          if (dat4(i,j) == undef_srb_lw) then
            lwdn(i,j) = undef_mxe
          else
            lwdn(i,j) = dat4(i,j)
          end if
        end do
      end do

      ! open output file

      write(flot2,'(1a,i4.4,i2.2)') &
           & '/worke/htsujino/SRB/rel3.0_qc/grads/lwdn.',nyear,m
      open(mtot2,file=flot2,form='unformatted',access='direct',recl=4*imut*jmut)
      write(6,*) 'Downward long wave written to ',trim(flot2)
      ireco2 = 0
      ireco2 = ireco2 + 1
      write(mtot2,rec=ireco2) lwdn
      close(mtot2)

    end do
  end do

end program read_littleendian_output_bigendian
