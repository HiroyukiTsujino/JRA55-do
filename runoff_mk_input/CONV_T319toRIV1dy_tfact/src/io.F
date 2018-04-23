      SUBROUTINE readf( 
     I                 iunit, 
     D                 nx, ny, nz, nxy,
     O                 rout, dout, leof,
     O                 chead )
      USE kindmodule
      IMPLICIT NONE
!
      INTEGER :: iunit,  n
      INTEGER :: nx, ny, nz, nxy

      REAL(single), DIMENSION(nx, ny, nz), INTENT(OUT) :: rout
      REAL(double), DIMENSION(nx, ny, nz), INTENT(OUT) :: dout
      LOGICAL, INTENT(OUT) :: leof
      CHARACTER(len=16), DIMENSION(64) :: chead
! local variables
C      CHARACTER(len=16), DIMENSION(64) :: chead
      INTEGER, DIMENSION(6) ::  idate
C      DATA chead / 64*'                ' /

      CHARACTER(len=5) :: cfmt

      INTEGER :: ij, j, k, istat

      leof = .true.

      READ(IUNIT, IOSTAT=istat) chead
      IF (istat == 0) THEN
         leof = .false.
         cfmt  = chead(38)

C------- for miroc 27
         READ(chead(27),'(I4.4,2I2.2,I3.3,2I2.2)') idate
c------- for coco  50
c     READ(chead(50),'(I6.6,5I2.2)') idate
c--------

         WRITE(*,*) 'READ FILE :', chead(3),
     &        IDATE(1),'yr',IDATE(2),'mon',IDATE(3),'day',
     &        IDATE(4),'h',IDATE(5),'m',IDATE(6),'s'
c     WRITE(*,*) chead
         IF( (cfmt == 'REAL4' ) .or. (cfmt == 'UR4')) THEN
            READ(iunit) rout
            dout(:,:,:) = rout(:,:,:)
         ELSEIF ( (cfmt == 'REAL8') .or. (cfmt == 'UR8') ) THEN
            READ(iunit) dout
            rout(:,:,:) = dout(:,:,:)
         ELSEIF ( (cfmt == 'URC')) THEN
            call sread(iunit,rout,nxy,nz)
            dout(:,:,:) = rout(:,:,:)
         ENDIF



      ENDIF

      RETURN
      END


      SUBROUTINE SREAD(IFN,DATA,ij,kk)
      INTEGER i,ifn,ij,k,kk,ijmax
      parameter(ijmax=1280*912)
      REAL*4       DATA(ij,kk)
      real*8       data0(ijmax)
      real*8       rmin
      real*8        fac1,   fac2
      integer*4    nd
      integer*4    ne
      integer*4    idata(ijmax/2)
      DO K=1,KK 
          read ( ifn ) rmin
c          write(0,*) rmin
          read ( ifn ) nd
c          write(0,*) 'o', nd
          read ( ifn ) ne
c          write(0,*) 'ok',ij, ne
          read ( ifn ) (idata(i),i=1,ij/2)
          call sunpack
     i         ( rmin  , nd    , ne    , idata , ij  ,
     i          -999.d0, 65534 ,
     o           data0                                  )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         FAC1 = 10.D0**(-ND)
         FAC2 =  2.D0**NE
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          do i=1,ij
c             data(i,k)=data0(i)
             data(i,k)=data0(i) + fac1 * fac2 * 0.5D0
          enddo
      ENDDO
      return
      end
C
      SUBROUTINE SUNPACK        !! simple 2-byte unpacking
     I         ( RMIN  , ND    , NE    , NX     , IJNUM ,
     I           VMISS , IMISS ,
     O           GDATA                                   )
*
      IMPLICIT NONE
*
*   [PARAM]
      INTEGER    IJNUM
*
*   [INPUT]
      REAL*8     RMIN                          !! min. vaule
      INTEGER*4  ND                            !! decimal scale factor
      INTEGER*4  NE                            !! binary scale factor
      INTEGER*4  NX    ( IJNUM/2 )             !! packed data
      REAL*8     VMISS                         !! missing value
      INTEGER*4  IMISS                         !! missing value
*
*   [OUTPUT]
      REAL*8     GDATA ( IJNUM )               !! unpacked data
*
*   [INTERNAL WORK]
      INTEGER*4  I4MAX                         !! max. of signed long int
      INTEGER*4  I2MAX                         !! max. of signed long int
      INTEGER    ILOW, IHIGH
      REAL*8     FAC1, FAC2
      INTEGER    I
*
      DATA      I4MAX / 2147483647 /
      DATA      I2MAX / 32767 /
*
      IF ( NE .EQ. IMISS ) THEN              !! binary scale factor is too small
         DO I = 2, IJNUM, 2
            IF ( NX(I/2) .LT. 0 ) THEN       !! complementary of signed long int
               IHIGH  = INT( (NX(I/2)+I4MAX+1)/ 65536 ) + I2MAX+1 !! unsigned short int
               ILOW   = MOD( (NX(I/2)+I4MAX+1), 65536 )           !! unsigned short int
            ELSE
               IHIGH = INT( NX(I/2)/ 65536 )
               ILOW  = MOD( NX(I/2), 65536 )
            END IF
            IF ( IHIGH .NE. IMISS ) THEN
               GDATA( I-1 ) = RMIN
            ELSE
               GDATA( I-1 ) = VMISS
            END IF
            IF ( ILOW .NE. IMISS ) THEN
               GDATA( I   ) = RMIN
            ELSE
               GDATA( I   ) = VMISS
            END IF
         END DO
      ELSE
         FAC1 = 10.D0**(-ND)
         FAC2 =  2.D0**NE
         DO I = 2, IJNUM, 2
            IF ( NX(I/2) .LT. 0 ) THEN       !! complementary of signed long int
               IHIGH  = INT( (NX(I/2)+I4MAX+1)/ 65536 ) + I2MAX+1 !! unsigned short int
               ILOW   = MOD( (NX(I/2)+I4MAX+1), 65536 )           !! unsigned short int
            ELSE
               IHIGH = INT( NX(I/2)/ 65536 )
               ILOW  = MOD( NX(I/2), 65536 )
            END IF
            IF ( IHIGH .NE. IMISS ) THEN
               GDATA( I-1 ) = ( IHIGH*FAC2 + RMIN ) * FAC1
            ELSE
               GDATA( I-1 ) = VMISS
            END IF
            IF ( ILOW .NE. IMISS ) THEN
               GDATA( I   ) = ( ILOW *FAC2 + RMIN ) * FAC1
            ELSE
               GDATA( I   ) = VMISS
            END IF
         END DO
      END IF
*
      RETURN
      END


