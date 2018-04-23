!-*-F90-*-
!============================================================================
module netCDF_write

  implicit none
  include 'netcdf.inc'
  private

  !     We are writing 4D data, a lon-lat-lvl grid, with 2
  !     timesteps of data.

  integer(4),parameter :: NDIMS=4
  character(len=64),parameter ::  &
       &               LVL_NAME='level',     &
       &               LAT_NAME='latitude',  &
       &               LON_NAME='longitude', &
       &               REC_NAME='time'

  character(len=64),parameter :: BND_NAME='time_bnds'
  integer(4),parameter :: NBNDS = 2
  integer(4),parameter :: NBNDDIM = 2

  !     We recommend that each variable carry a "units" attribute.
  character(len=64),parameter :: UNITS='units'
  character(len=64),parameter :: LongName='long_name'
  character(len=64),parameter :: CALENDAR='calendar'

  public :: netCDF_write__create_file, &
       &    netCDF_write__var_4d

contains
  !----------------------------------------------------------------------------
  subroutine netCDF_write__create_file (                &
       & NLONS, NLATS, NLVLS,                           &
       & lats, lons, lvls,                              &
       & FILE_NAME,                                     &
       & nvars, VAR_NAME, VAR_UNITS, VAR_LongName,      &
       & undef_out,                                     &
       & LAT_UNITS, LON_UNITS, LVL_UNITS, REC_UNITS,    &
       & l_leap )

    !     This is the name of the data file we will create.

    integer(4),intent(in) :: NLVLS, NLATS, NLONS
    real(8),   intent(in) :: lats(NLATS), lons(NLONS), lvls(NLVLS)
    integer(4),intent(in) :: nvars
    character(len=*),intent(in) :: FILE_NAME, &
         & LAT_UNITS, LON_UNITS, LVL_UNITS, REC_UNITS
    character(len=*)  VAR_NAME(nvars), VAR_UNITS(nvars), VAR_LongName(nvars)
    real(4),   intent(in) :: undef_out(nvars)
    logical,   intent(in) :: l_leap

    integer(4) :: ncid

    integer(4) :: lvl_dimid, lon_dimid, lat_dimid, rec_dimid, bnd_dimid

    !     The start and count arrays will tell the netCDF library where to
    !     write our data.

    integer(4) :: lon_varid, lat_varid, lvl_varid, rec_varid, bnd_varid
    integer(4) :: dimids(NDIMS)

    integer(4) :: dimids_bnd(NBNDDIM)

    !     We will create two netCDF variables, one each for temperature and
    !     pressure fields.

    integer(4) :: var_varid(nvars)
    
    !     Loop indices.
    integer(4) :: lvl, lat, lon, rec, i, n

    !     Error handling.
    integer(4) :: retval

    integer(4) :: ltmp

    !-------------------------------------------------------------------------
    !     Create the file. 

    retval = nf_create(FILE_NAME, nf_clobber, ncid)
    if (retval .ne. nf_noerr) call handle_err(retval)
    
    !----------------------------------------------------------------------
    !     Define the dimensions. The record dimension is defined to have
    !     unlimited length - it can grow as needed. In this example it is
    !     the time dimension.

    retval = nf_def_dim(ncid, LVL_NAME, NLVLS, lvl_dimid)
    if (retval .ne. nf_noerr) call handle_err(retval)

    retval = nf_def_dim(ncid, LAT_NAME, NLATS, lat_dimid)
    if (retval .ne. nf_noerr) call handle_err(retval)

    retval = nf_def_dim(ncid, LON_NAME, NLONS, lon_dimid)
    if (retval .ne. nf_noerr) call handle_err(retval)
    
    retval = nf_def_dim(ncid, REC_NAME, NF_UNLIMITED, rec_dimid)
    if (retval .ne. nf_noerr) call handle_err(retval)
    
    retval = nf_def_dim(ncid, BND_NAME, NBNDS, bnd_dimid)
    if (retval .ne. nf_noerr) call handle_err(retval)

    dimids(1) = lon_dimid
    dimids(2) = lat_dimid
    dimids(3) = lvl_dimid
    dimids(4) = rec_dimid

    write(6,*) ' definition of dimensions O.K. '

    !----------------------------------------------------------------------
    !     Define the coordinate variables. We will only define coordinate
    !     variables.

    retval = nf_def_var(ncid, LAT_NAME, NF_DOUBLE, 1, lat_dimid, lat_varid)
    if (retval .ne. nf_noerr) call handle_err(retval)

    retval = nf_def_var(ncid, LON_NAME, NF_DOUBLE, 1, lon_dimid, lon_varid)
    if (retval .ne. nf_noerr) call handle_err(retval)

    retval = nf_def_var(ncid, LVL_NAME, NF_DOUBLE, 1, lvl_dimid, lvl_varid)
    if (retval .ne. nf_noerr) call handle_err(retval)

    retval = nf_def_var(ncid, REC_NAME, NF_DOUBLE, 1, rec_dimid, rec_varid)
    if (retval .ne. nf_noerr) call handle_err(retval)
    
    !----------------------------------------------------------------------
    !     Assign units attributes to coordinate variables.

    retval = nf_put_att_text(ncid, lat_varid, UNITS, len(LAT_UNITS), LAT_UNITS)
    if (retval .ne. nf_noerr) call handle_err(retval)
    retval = nf_put_att_text(ncid, lat_varid, "axis", 1, "Y")
    if (retval .ne. nf_noerr) call handle_err(retval)

    retval = nf_put_att_text(ncid, lon_varid, UNITS, len(LON_UNITS), LON_UNITS)
    if (retval .ne. nf_noerr) call handle_err(retval)
    retval = nf_put_att_text(ncid, lon_varid, "axis", 1, "X")
    if (retval .ne. nf_noerr) call handle_err(retval)

    retval = nf_put_att_text(ncid, lvl_varid, UNITS, len(LVL_UNITS), LVL_UNITS)
    if (retval .ne. nf_noerr) call handle_err(retval)
    retval = nf_put_att_text(ncid, lvl_varid, "axis", 1, "Z")
    if (retval .ne. nf_noerr) call handle_err(retval)
    
    retval = nf_put_att_text(ncid, rec_varid, UNITS, len(REC_UNITS), REC_UNITS)
    if (retval .ne. nf_noerr) call handle_err(retval)
    retval = nf_put_att_text(ncid, rec_varid, "axis", 1, "T")
    if (retval .ne. nf_noerr) call handle_err(retval)

    retval = nf_put_att_text(ncid, rec_varid, "bounds", 9, "time_bnds")
    if (retval .ne. nf_noerr) call handle_err(retval)

    if (.not. l_leap) then
      retval = nf_put_att_text(ncid, rec_varid, "calendar", 6, "noleap")
      if (retval .ne. nf_noerr) call handle_err(retval)
    end if

    write(6,*) ' attributions O.K. '

    !  retval = nf_put_att_text(ncid, rec_varid, "axis", 1, "T")
    !  if (retval .ne. nf_noerr) call handle_err(retval)
    !  retval = nf_put_att_text(ncid, rec_varid, "time_origin", 10, "1-JAN-1985")
    !  if (retval .ne. nf_noerr) call handle_err(retval)

    !----------------------------------------------------------------------
    !     Define time bounds

    dimids_bnd(1) = bnd_dimid
    dimids_bnd(2) = rec_dimid

    write(6,*) dimids_bnd(1),dimids_bnd(2)
    retval = nf_def_var(ncid, BND_NAME, NF_DOUBLE, NBNDDIM, dimids_bnd, bnd_varid)
    if (retval .ne. nf_noerr) call handle_err(retval)
    
    write(6,*) ' definition of bounds O.K. '
    
    !----------------------------------------------------------------------
    !     Define the netCDF variable

    do n = 1, nvars

      retval = nf_def_var(ncid, VAR_NAME(n), NF_REAL, NDIMS, dimids, var_varid)
      if (retval .ne. nf_noerr) call handle_err(retval)
    
      !     Assign units attributes to the netCDF variables.
      !     The dimids array is used to pass the dimids of the dimensions of
      !     the netCDF variables. Both of the netCDF variables we are creating
      !     share the same four dimensions. In Fortran, the unlimited
      !     dimension must come last on the list of dimids.

      retval = nf_put_att_text(ncid, var_varid, UNITS, len(VAR_UNITS(n)), VAR_UNITS(n))
      if (retval .ne. nf_noerr) call handle_err(retval)

      retval = nf_put_att_text(ncid, var_varid, LongName, len(VAR_LongName(n)), VAR_LongName(n))
      if (retval .ne. nf_noerr) call handle_err(retval)
      
      retval = nf_put_att_real(ncid, var_varid, "missing_value", nf_float, 1, undef_out(n))
      if (retval .ne. nf_noerr) call handle_err(retval)

    end do
    
    !     End define mode.
    retval = nf_enddef(ncid)
    if (retval .ne. nf_noerr) call handle_err(retval)
    
    !----------------------------------------------------------------------
    !     Write the coordinate variable data. This will put the latitudes
    !     and longitudes of our data grid into the netCDF file.

    retval = nf_put_var_double(ncid, lat_varid, lats)
    if (retval .ne. nf_noerr) call handle_err(retval)

    retval = nf_put_var_double(ncid, lon_varid, lons)
    if (retval .ne. nf_noerr) call handle_err(retval)

    retval = nf_put_var_double(ncid, lvl_varid, lvls)
    if (retval .ne. nf_noerr) call handle_err(retval)

    !----------------------------------------------------------------------
    !     Close the file. This causes netCDF to flush all buffers and make
    !     sure your data are really written to disk.

    retval = nf_close(ncid)
    if (retval .ne. nf_noerr) call handle_err(retval)
  
    ltmp=len_trim(FILE_NAME)

    write(*,"(A,A,A,I4,A,I4,A)") '*** SUCCESS creating ', FILE_NAME(1:ltmp)
    !----------------------------------------------------------------------

  end subroutine netCDF_write__create_file
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  subroutine netCDF_write__var_4d  (                    &
       & var_out,  irec,                                &
       & nrecs, recs, recsb, recse,                     &
       & FILE_NAME,                                     &
       & nvars, VAR_NAME,                               &
       & NLONS, NLATS, NLVLS        )
    
    integer(4),intent(in) :: NLVLS, NLATS, NLONS
    integer(4),intent(in) :: nvars
    real(4),   intent(in) :: var_out(NLONS, NLATS, NLVLS, nvars)
    integer(4),intent(in) :: nrecs, irec
    real(8),   intent(in) :: recs(nrecs), recsb(nrecs), recse(nrecs)
    character(len=*),intent(in) :: FILE_NAME
    character(len=*),intent(in) :: VAR_NAME(nvars)

    integer(4) :: ncid

    real(8), allocatable :: rec_axis(:)
    real(8), allocatable :: bnd_axis(:,:)

    !     The start and count arrays will tell the netCDF library where to
    !     write our data.

    integer(4) :: start(NDIMS), count(NDIMS)

    integer(4) :: rec_varid
    integer(4) :: bnd_varid
    integer(4) :: var_varid(nvars)

    !     Loop indices.
    integer(4) :: i, n

    !     Error handling.
    integer(4) :: retval

    integer(4) :: ltmp

    !---------------------------------------------------------

    retval = nf_open(FILE_NAME, nf_write, ncid)
    if (retval .ne. nf_noerr) call handle_err(retval)

    retval = nf_inq_varid(ncid, REC_NAME, rec_varid)
    if (retval .ne. nf_noerr) call handle_err(retval)

    retval = nf_inq_varid(ncid, BND_NAME, bnd_varid)
    if (retval .ne. nf_noerr) call handle_err(retval)

    !----------------------------------------------------------------------
    !     These settings tell netcdf to write one timestep of data. (The
    !     setting of start(4) inside the loop below tells netCDF which
    !     timestep to write.)

    count(1) = NLONS
    count(2) = NLATS
    count(3) = NLVLS
    count(4) = 1

    start(1) = 1
    start(2) = 1
    start(3) = 1
  
    !----------------------------------------------------------------------
    !     Write the pretend data. This will write our surface pressure and
    !     surface temperature data. The arrays only hold one timestep worth
    !     of data. We will just rewrite the same data for each timestep. In
    !     a real application, the data would change between timesteps.
  
    do n = 1, nvars

      retval = nf_inq_varid(ncid, VAR_NAME(n), var_varid(n))
      if (retval .ne. nf_noerr) call handle_err(retval)

      start(4) = irec
      retval = nf_put_vara_real(ncid, var_varid(n), start, count, var_out(1,1,1,n))
      if (retval .ne. nf_noerr) call handle_err(retval)
      write(6,*) 'writing data O.K.'
  
    end do

    !----------------------------------------------------------------------

    allocate(rec_axis(1:irec))
    rec_axis(1:irec) = recs(1:irec)
    retval = nf_put_var_double(ncid, rec_varid, rec_axis)
    if (retval .ne. nf_noerr) call handle_err(retval)
    write(6,*) 'writing time O.K.'

    !----------------------------------------------------------------------

    allocate(bnd_axis(1:irec,1:2))
    bnd_axis(1:irec,1) = recsb(1:irec)
    bnd_axis(1:irec,2) = recse(1:irec)
    retval = nf_put_var_double(ncid, bnd_varid, bnd_axis)
    if (retval .ne. nf_noerr) call handle_err(retval)

    !     Close the file. This causes netCDF to flush all buffers and make
    !     sure your data are really written to disk.

    retval = nf_close(ncid)
    if (retval .ne. nf_noerr) call handle_err(retval)
  
    ltmp=len_trim(FILE_NAME)

    write(*,"(A,A,A,I4,A,I4,A)") '*** SUCCESS writing ', &
         & FILE_NAME(1:ltmp), ' : rec=[', irec, ']/[',nrecs,'] ***'

  end subroutine netCDF_write__var_4d
  !----------------------------------------------------------------------------
  subroutine handle_err(errcode)

    implicit none
    integer errcode
  
    print *, 'Error: ', nf_strerror(errcode)
    stop 2

  end subroutine handle_err
  !----------------------------------------------------------------------------
end module netCDF_write
