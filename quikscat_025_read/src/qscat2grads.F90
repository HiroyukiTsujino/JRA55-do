!-*-F90-*-
program read_qscat3_and_output_grads
!====================================================================
!     Filename: qscat2grads.F90
!
!     Usage: 
!
!       To run this program, use the following command:
!     
!       your_computer% read_qscat3 <filename>
!
!       where "<filename>" is the name of the QuikSCAT Level 3
!       input file
!
!     Description:
!
!       This file contains 3 subroutines in order to read the 
!       QuikSCAT Level 3 data in Hierarchical Data Format (HDF).  
!       The subroutines are as follows.
!
!       1. read_attrib_byname():  a subroutine to read the name 
!                                 and value(s) of a global attribute
!                                 referenced by its name.
!       
!       2. extract_sds():  a subroutine to read the contents of an
!                          SDS from an HDF file
!
!     NOTES:
!     1. Please refer all questions concerning this program and
!        QuikSCAT data obtained from the JPL PO.DAAC to
!        qscat@podaac.jpl.nasa.gov.
!
!     2. The HDF library must be installed before this program will 
!        work properly.  The HDF library and further information 
!        about HDF may be obtained from the National Center for 
!        Supercomputing Applications (NCSA) at http://hdf.ncsa.uiuc.edu.
!
!     3. The L3 data are read in their entirety.  Examples of reading
!        the QuikSCAT data by slabs can be found in read_qscat1b.f
!        read_qscat2a.f.
!
!  5/5/2000 R.S. Dunbar, K.L. Perry
!
!  Modifications:
!
!  5/15/2000 Corrected handling of 32-bit unsigned integers
!             in extract_sds subroutine. K.L. Perry
!
!  Copyright 2000, California Institute of Technology
!====================================================================

  use file_open_close_manager

  implicit none

!     Set Parameters

  integer(4),parameter :: XGRID=1440,YGRID=720

  integer(4),parameter ::  DFACC_RDONLY=1

!     Define Variables

  character(128) :: l3_file
  character(8)   :: product

  integer(4) :: sd_id,retn,sfstart,sfend
  integer(4) :: irow,iwvc,iamb
  integer(4) :: ntype, nval
  integer(4) :: lreclen

  real(4) :: asc_avg_wind_speed(XGRID,YGRID)
  real(4) :: des_avg_wind_speed(XGRID,YGRID)
  real(4) :: asc_avg_wind_vel_u(XGRID,YGRID)
  real(4) :: des_avg_wind_vel_u(XGRID,YGRID)
  real(4) :: asc_avg_wind_vel_v(XGRID,YGRID)
  real(4) :: des_avg_wind_vel_v(XGRID,YGRID)
  real(4) :: asc_avg_wind_speed_sq(XGRID,YGRID)
  real(4) :: des_avg_wind_speed_sq(XGRID,YGRID)
  real(4) :: asc_wvc_count(XGRID,YGRID)
  real(4) :: des_wvc_count(XGRID,YGRID)
  real(4) :: asc_time_frac(XGRID,YGRID)
  real(4) :: des_time_frac(XGRID,YGRID)
  real(4) :: asc_rain_prob(XGRID,YGRID)
  real(4) :: des_rain_prob(XGRID,YGRID)
  real(4) :: asc_rain_flag(XGRID,YGRID)
  real(4) :: des_rain_flag(XGRID,YGRID)

  integer(4) :: lun_out1
  character(128) :: file_all='wind_all.gd'

!-------------------------------------------------------------------
!     Read the input filename.
  call GETARG(1,l3_file)
  if (l3_file .eq. ' ') then
    print *,'Usage: read_qscat3 <Level 3 file>'
    stop
  endif
  write(*,*)
  write(*,*) 'FILENAME: ',l3_file

!     Open the HDF input file and initiate the SD interface
  sd_id = sfstart(l3_file,DFACC_RDONLY)

!     Make sure that the file is a QuikSCAT Level 3 file
  call read_attrib_byname(sd_id,'ShortName',ntype,nval,product)
  if (product.ne.'QSCATL3') then
    print *,'The input file is not a QuikSCAT Level 3 file'
    print *,'*** Aborting program ***'
    stop
  endif
  
!     Read each SDS in its entirety.  For an example of reading
!     the QuikSCAT SDS data in slabs, please refer to read_qscat2a.f.

  irow=1
  call extract_sds(sd_id,'asc_avg_wind_speed',irow,YGRID, &
       &     asc_avg_wind_speed)
  call extract_sds(sd_id,'des_avg_wind_speed',irow,YGRID, &
       &     des_avg_wind_speed)
  call extract_sds(sd_id,'asc_avg_wind_vel_u',irow,YGRID, &
       &     asc_avg_wind_vel_u)
  call extract_sds(sd_id,'des_avg_wind_vel_u',irow,YGRID, &
       &     des_avg_wind_vel_u)
  call extract_sds(sd_id,'asc_avg_wind_vel_v',irow,YGRID, &
       &     asc_avg_wind_vel_v)
  call extract_sds(sd_id,'des_avg_wind_vel_v',irow,YGRID, &
       &     des_avg_wind_vel_v)
  call extract_sds(sd_id,'asc_avg_wind_speed_sq',irow,YGRID, &
       &     asc_avg_wind_speed_sq)
  call extract_sds(sd_id,'des_avg_wind_speed_sq',irow,YGRID, &
       &     des_avg_wind_speed_sq)
  call extract_sds(sd_id,'asc_wvc_count',irow,YGRID, &
       &     asc_wvc_count)
  call extract_sds(sd_id,'des_wvc_count',irow,YGRID, &
       &     des_wvc_count)
  call extract_sds(sd_id,'asc_time_frac',irow,YGRID, &
       &     asc_time_frac)
  call extract_sds(sd_id,'des_time_frac',irow,YGRID, &
       &     des_time_frac)
  call extract_sds(sd_id,'asc_rain_prob',irow,YGRID, &
       &     asc_rain_prob)
  call extract_sds(sd_id,'des_rain_prob',irow,YGRID, &
       &     des_rain_prob)
  call extract_sds(sd_id,'asc_rain_flag',irow,YGRID, &
       &     asc_rain_flag)
  call extract_sds(sd_id,'des_rain_flag',irow,YGRID, &
       &     des_rain_flag)

  retn=sfend(sd_id)

  !------------------------------------------------------------------

  lreclen = 4 * XGRID * YGRID
  call open_file_direct(lun_out1,file_all,lreclen)
  write(lun_out1,rec=1) asc_avg_wind_speed
  write(lun_out1,rec=2) des_avg_wind_speed
  write(lun_out1,rec=3) asc_avg_wind_vel_u
  write(lun_out1,rec=4) des_avg_wind_vel_u
  write(lun_out1,rec=5) asc_avg_wind_vel_v
  write(lun_out1,rec=6) des_avg_wind_vel_v
  write(lun_out1,rec=7) asc_rain_flag
  write(lun_out1,rec=8) des_rain_flag
  call close_file(lun_out1)

end program read_qscat3_and_output_grads

!====================================================================
!    READ_ATTRIB_BYNAME:  a subroutine to read the name and
!                         value(s) of a global attribute
!                         referenced by its name.
!    
!    5/14/1998 R.S. Dunbar
!====================================================================

subroutine read_attrib_byname(sd_id,in_attr_name, &
     &     num_type,n_values,fvalues)
      
  integer(4),parameter :: MAX_NC_NAME=256

  integer(4) :: sd_id,num_type,n_values
  integer(4) :: attr_index,count,retn,n,oldn
  integer(4) :: sffattr,sfgainfo,sfrattr
  character(*) :: in_attr_name
  character(*) :: fvalues(*)
  character(MAX_NC_NAME) :: attr_name
  character(512) :: attr_data
  character(MAX_NC_NAME) :: values(20)
  character :: cr
 
  !------------------------------------------------------------------
!     Find the attribute assigned to in_attr_name
  attr_index = sffattr(sd_id,in_attr_name)

!     Get information about the  file attribute
  retn = sfgainfo(sd_id,attr_index,attr_name,num_type,count)

!     Read the attribute data
  retn = sfrattr(sd_id,attr_index,attr_data)

  cr = char(10)
  ival = 0
  oldn = 1
5 continue

!     QuikSCAT attributes have atleast three lines: 
!     metadata type, array size and metadata contents
!     Use "blank spaces" to identify the end of a line

  n = index(attr_data(oldn:(count-1)),cr)

!     Read all of the metadata lines
  if (n .eq. 0) then
    ival=ival+1
    values(ival) = attr_data(oldn:(count-1))
    goto 99
  else
    ival=ival+1
    values(ival) = attr_data(oldn:(oldn+n-2))
  endif
  oldn=n+oldn
  goto 5

99 continue
  n_values = ival - 2
  do i=1,n_values
    fvalues(i) = values(i+2)
  enddo
  return
end subroutine read_attrib_byname

!====================================================================
!    EXTRACT_SDS:  a subroutine to read the contents of an
!                  SDS from an HDF file
!    
!    5/12/1998 R.S. Dunbar
!
!    Revisions:
!    7/1999   Code adapted to read input in bytes as well as ints 
!             and floats.  Comments were also added.  K.L. Perry
!
!    3/2000   Corrected code for 8-bit unsigned integers.  "buffer"
!             was used instead of "buffer2".  K.L. Perry
!
!    5/2000   Changed MAX_BUF_SIZE from 1000000 to 10000000.
!             Corrected code for 32-bit unsigned integers.  Created
!             "buffer3" array of int*4 to correctly read in uint32.
!             K.L. Perry
!
!====================================================================
subroutine extract_sds(sd_id,in_var,irec,slab_size,out_var)

  integer(4),parameter ::  MAX_BUF_SIZE=10000000
  integer(4) :: sd_id,sds_index,sds_id,retn
  integer(4) :: rank,dim_sizes(3),data_type,nattrs,num_type
  integer(4) ::  edge(3),stride(3),start(3),irec,slab_size
  real(8) :: cal,cal_err,off,off_err
  integer(4) :: iprod,i,itmp
  
  character(*) :: in_var
  character(256) :: name
  integer(4) :: sfn2index,sfselect,sfginfo,sfrdata,sfgcal,sfendacc
  
  integer(2) :: buffer(MAX_BUF_SIZE)
  byte :: buffer2(MAX_BUF_SIZE)
  integer(4) :: buffer3(MAX_BUF_SIZE)
  real(4) :: out_var(MAX_BUF_SIZE)
  
  !------------------------------------------------------------------
!     Search for the index of "in_var"
  sds_index = sfn2index(sd_id, in_var)

!     Select data set corresponding to the returned index
  sds_id = sfselect(sd_id,sds_index)
  retn = sfginfo(sds_id,name,rank,dim_sizes,data_type,nattrs)

  do i=1,rank
    edge(i)=dim_sizes(i)
    start(i)=0
    stride(i)=1
  enddo
  edge(rank)=slab_size
  start(rank)=irec-1

  iprod=1
  do i=1,rank
    iprod=iprod*edge(i)
  enddo

!     Get the calibration and offset values of input
  retn = sfgcal(sds_id,cal,cal_err,off,off_err,num_type)

!     Read Arrays which are not float32 or int8 or uint8 or uint32
  if ((data_type.ne.5).and.(data_type.ne.20).and. &
    &     (data_type.ne.21).and.(data_type.ne.25)) then

!     Read the data set into the "buffer" array
    retn=sfrdata(sds_id,start,stride,edge,buffer)

!     Calibrate the output
    do i=1,iprod
!     Correct for 16-bit unsigned integers
      if ((data_type.eq.23).and.(buffer(i).lt.0)) then
        out_var(i)=buffer(i)+65536.0

!     No correction needed for signed or positive unsigned integers
      else
        out_var(i)=buffer(i)
      endif

      out_var(i)=out_var(i)*cal
    enddo

!     Read uint32 arrays.
  else if (data_type.eq.25) then

!     Read the data set into the "buffer3" uint32 array
    retn=sfrdata(sds_id,start,stride,edge,buffer3)

!     Calibrate the output
    do i=1,iprod
!     Correct for 32-bit unsigned integers
      if ((data_type.eq.25).and.(buffer3(i).lt.0)) then
        out_var(i)=buffer3(i)+4294967296.0
      else
        out_var(i)=buffer3(i)
      endif
      out_var(i)=out_var(i)*cal
    enddo

!     Read int8 and uint8 arrays. 
  else if ((data_type.eq.20).or.(data_type.eq.21)) then

!     Read the data set into the "buffer2" byte-array
    retn=sfrdata(sds_id,start,stride,edge,buffer2)

!     Calibrate the output
    do i=1,iprod

!     Correct for 8-bit unsigned integers
      itmp=buffer2(i)
      if ((data_type.eq.21).and.(buffer2(i).lt.0)) then
        itmp=itmp+256
        out_var(i)=itmp
               
!     No correction needed for signed or positive unsigned integers
      else
        out_var(i)=itmp
      endif

      out_var(i)=out_var(i)*cal
    enddo

  else
!     Read float32 arrays directly into the "out_var" array
    retn=sfrdata(sds_id,start,stride,edge,out_var)

!     Calibrate the output
    do i=1,iprod
      out_var(i)=out_var(i)*cal
    enddo
  endif

!     Terminate access to the array data set.
  retn = sfendacc(sds_id)

end subroutine extract_sds
