!-*-F90-*-
!============================================================================
module netCDF_general

  implicit none
  include 'netcdf.inc'
  private

  type :: netCDF_global_attributes
    character(len=256) :: title=''
    character(len=256) :: institution=''
    character(len=256) :: conventions=''
    character(len=256) :: source=''
    character(len=256) :: history=''
    character(len=1024) :: comment=''
    character(len=256) :: version=''
    character(len=256) :: fill_value=''
  end type netCDF_global_attributes

  public :: netCDF_global_attributes         &
       &  , netCDF_write__global_attributes  &
       &  , handle_err 

contains
  !-----------------------------------------------------------------------------
  subroutine netCDF_write__global_attributes (   &
       & FILE_NAME,  GLB_ATTRIB                  &
       & )
    
    character(len=*),intent(in) :: FILE_NAME
    type(netCDF_global_attributes) :: GLB_ATTRIB

    integer(4) :: ncid

    !     Loop indices.
    integer(4) :: len_tmp

    !     Error handling.
    integer(4) :: retval

    !---------------------------------------------------------

    retval = nf_open(FILE_NAME, nf_write, ncid)
    if (retval .ne. nf_noerr) call handle_err(retval)

    retval = nf_redef(ncid)
    if (retval .ne. nf_noerr) call handle_err(retval)

    len_tmp = len_trim(glb_attrib%title)
    if (len_tmp > 0) then
      retval = nf_put_att_text(ncid, nf_global, "title", len_tmp, glb_attrib%title)
      if (retval .ne. nf_noerr) call handle_err(retval)
    end if
  
    len_tmp = len_trim(glb_attrib%institution)
    if (len_tmp > 0) then
      retval = nf_put_att_text(ncid, nf_global, "institution", len_tmp, glb_attrib%institution)
      if (retval .ne. nf_noerr) call handle_err(retval)
    end if

    len_tmp = len_trim(glb_attrib%conventions)
    if (len_tmp > 0) then
      retval = nf_put_att_text(ncid, nf_global, "conventions", len_tmp, glb_attrib%conventions)
      if (retval .ne. nf_noerr) call handle_err(retval)
    end if

    len_tmp = len_trim(glb_attrib%source)
    write(6,*) len_tmp
    if (len_tmp > 0) then
      retval = nf_put_att_text(ncid, nf_global, "source", len_tmp, glb_attrib%source)
      if (retval .ne. nf_noerr) call handle_err(retval)
    end if
  
    len_tmp = len_trim(glb_attrib%history)
    if (len_tmp > 0) then
      retval = nf_put_att_text(ncid, nf_global, "history", len_tmp, glb_attrib%history)
      if (retval .ne. nf_noerr) call handle_err(retval)
    end if

    len_tmp = len_trim(glb_attrib%comment)
    if (len_tmp > 0) then
      retval = nf_put_att_text(ncid, nf_global, "comment", len_tmp, glb_attrib%comment)
      if (retval .ne. nf_noerr) call handle_err(retval)
    end if

    len_tmp = len_trim(glb_attrib%version)
    if (len_tmp > 0) then
      retval = nf_put_att_text(ncid, nf_global, "version", len_tmp, glb_attrib%version)
      if (retval .ne. nf_noerr) call handle_err(retval)
    end if

    len_tmp = len_trim(glb_attrib%fill_value)
    if (len_tmp > 0) then
      retval = nf_put_att_text(ncid, nf_global, "fill_value", len_tmp, glb_attrib%fill_value)
      if (retval .ne. nf_noerr) call handle_err(retval)
    end if

    retval = nf_enddef(ncid)
    if (retval .ne. nf_noerr) call handle_err(retval)

    retval = nf_close(ncid)
    if (retval .ne. nf_noerr) call handle_err(retval)

    len_tmp=len_trim(FILE_NAME)

    write(6,"(A,A,A)") '*** SUCCESS writing global attributes ', FILE_NAME(1:len_tmp), ' ***'

  end subroutine netCDF_write__global_attributes
  !----------------------------------------------------------------------------
  subroutine handle_err(errcode)

    implicit none
    integer errcode
  
    print *, 'Error: ', nf_strerror(errcode)
    stop 2

  end subroutine handle_err
  !----------------------------------------------------------------------------
end module netCDF_general
