      PROGRAM CONV2RIV_3HRto1DY
C
C --- for coco4.3
C
      USE dimmodule
      IMPLICIT NONE
      INTEGER, DIMENSION(jd) :: numx
      INTEGER :: numsum
C
      REAL(8), DIMENSION(jd)   :: alat, qsinla, qgw
      REAL(8), DIMENSION(jd+1) :: alatm
      REAL(8), ALLOCATABLE,SAVE :: lon(:), lonw(:), lone(:)
      REAL(8), ALLOCATABLE,SAVE :: lat(:), lats(:), latn(:)
      REAL(8), ALLOCATABLE,SAVE :: adx(:), ady(:)
      REAL(4), ALLOCATABLE,SAVE :: adat(:), mdat(:), idat(:)
C -- river grid
      REAL(4), DIMENSION(nx*ny) :: rdat, radat, tfact
      REAL(8), DIMENSION(nx)   :: rlon
      REAL(8), DIMENSION(nx+1) :: rlonm
      REAL(8), DIMENSION(ny)   :: rlat
      REAL(8), DIMENSION(ny+1) :: rlatm
      REAL(8), DIMENSION(ny)   :: sdy
      REAL(8) :: sdx, EA, dyy, dxx, ds
C
      INTEGER, PARAMETER :: iwork=3000000
      REAL(8), ALLOCATABLE, SAVE :: dss(:)
      INTEGER, ALLOCATABLE, SAVE :: ijriv(:), numdt(:)
C
      INTEGER :: i, j, k, ij, n, ii, jj, nn, lt, l, istat,m
      LOGICAL :: fact=.false.
C
      CHARACTER(len=256) :: ifile, ofile, mfile, factor
      NAMELIST /nmlst/ ifile, ofile, mfile, fact, factor
C
      READ(*, nmlst, IOSTAT=istat)
C
      open(11,file=ifile,form='unformatted', status='old')
      open(21,file=mfile,form='unformatted', status='old')
      open(12,file=ofile,form='unformatted',access='direct',
     &          recl=4*nx*ny,convert='little_endian')
C
      if(fact) then
         open(22,file=factor,form='unformatted',access='direct',
     &          recl=4*nx*ny,convert='little_endian')
      endif
C
C     set reduced TL319 grid
C
      call gauss(qsinla,qgw,jd)
      alat(:)=qsinla(:)              ! sin(lat)
C      call mklatm(alatm,qgw,jd)
      alatm(1)=1.0D0
      do j=1,jd/2
      alatm(j+1)=alatm(j)-2.0D0*qgw(j) ! sin(latm)
      enddo
      alatm( jd/2+1 ) = 0.D0
      do j=1,jd/2
      alatm(jd-j+2)=-alatm(j) ! sin(latm)
      enddo
C
      numx(1)      = 48
      numx(2)      = 64
      numx(3:4)    = 80
      numx(5)      = 96
      numx(6:7)    = 112
      numx(8:9)    = 128
      numx(10:11)  = 144
      numx(12:13)  = 160
      numx(14:17)  = 192
      numx(18:21)  = 224
      numx(22:23)  = 240
      numx(24:25)  = 256
      numx(26:30)  = 288
      numx(31:35)  = 320
      numx(36:37)  = 336
      numx(38:45)  = 384
      numx(46:48)  = 400
      numx(49:54)  = 432
      numx(55:57)  = 448
      numx(58:63)  = 480
      numx(64:70)  = 512
      numx(71:81)  = 560
      numx(82:86)  = 576
      numx(87:160) = 640
C
      do j = 1, jd/2
        numx(jd-j+1) = numx(j)
      end do
      numsum=0
      do j=1,jd
         numsum=numsum+numx(j)
      enddo
C
      allocate(lon(numsum))
      allocate(lonw(numsum))
      allocate(lone(numsum))
      allocate(lat(numsum))
      allocate(lats(numsum))
      allocate(latn(numsum))
      allocate(adx(numsum))
      allocate(ady(numsum))
C
      allocate(adat(numsum))
      allocate(mdat(numsum))
      allocate(idat(numsum))
C
      k=0
 
      do j=1,jd
         do i=1,numx(j)
         k=k+1
         lat(k)=alat(j)
         latn(k)=alatm(j)
         lats(k)=alatm(j+1)

         ady(k)=latn(k)-lats(k)
         adx(k)=2.0D0*pi/real(numx(j))

         lon(k)=adx(k)*real(i-1)
         lonw(k)=lon(k)-0.5D0*adx(k)
         lone(k)=lon(k)+0.5D0*adx(k)
         enddo
      enddo

      do k=1,numsum
         if(lon(k).gt.pi) lon(k)=lon(k)-2.0D0*pi
         if(lonw(k).gt.pi) lonw(k)=lonw(k)-2.0D0*pi
         if(lone(k).gt.pi) lone(k)=lone(k)-2.0D0*pi
      enddo

C
C     set river grids (North to South)
C
      do i=1,nx
        rlon(i) = -180.D0 + rdx*0.5D0 + rdx * real(i-1)
      enddo
       rlonm(1) = -180.D0
      do i=1,nx
        rlonm(i+1)=rlonm(i) + rdx
      enddo
        rlon(:)=rlon(:)*qi
        rlonm(:)=rlonm(:)*qi
        sdx=rdx*qi
C
      do j=1,ny
        rlat(j)=90.0D0 - rdy*0.5D0 - rdy*real(j-1)
      enddo
       rlatm(1)=90.0D0
      do j=1,ny
        rlatm(j+1)=rlatm(j) - rdy
      enddo
C
      rlat(:)=sin(rlat(:)*qi)
      rlatm(:)=sin(rlatm(:)*qi)
C
      do j=1,ny
         sdy(j)=rlatm(j)-rlatm(j+1)
      enddo
C
      ALLOCATE(ijriv(iwork))
      ALLOCATE(numdt(iwork))
      ALLOCATE(  DSS(iwork))

CC
CC      write(6,*) adx
CC
      nn=0
      do k=1,numsum

         do j=1,ny

            dyy=sdy(j)-min(max(rlatm(j  )-latn(k),0.0D0),sdy(j))
     &                -min(max(lats(k)-rlatm(j+1),0.0D0),sdy(j))

            if(dyy.gt.0.0D0) then
            do i=1,nx
               if(lone(k).lt.lonw(k)) then
                 dxx= min(max(lone(k)-rlonm(i),0.0D0),sdx)
     &               +min(max(rlonm(i+1)-lonw(k),0.0D0),sdx)
               else
                 dxx=sdx-min(max(rlonm(i+1)-lone(k),0.0D0),sdx)
     &                  -min(max(lonw(k)-rlonm(i  ),0.0D0),sdx)
               endif

               ds=dyy*dxx

               ij=i+nx*(j-1)

               if(ds.gt.0.0D0) then
                  nn=nn+1
                  IJRIV(nn)    =IJ
                  NUMDT(nn)    =K
                  DSS(nn)      =ds
               endif
            enddo

            endif
         enddo

      enddo

            if(nn.gt.iwork) then
                write(6,*) 'iwork is too small, must be larger than', nn
                stop
            endif

C
C      EA=0.0D0
C      do n=1,nn
C         EA=EA+dss(n)
C      enddo
C      write(6,*) 'EA=', ea, 4.0D0*pi

      radat(:)=0.0D0
      do n=1,nn
         radat(ijriv(n))=radat(ijriv(n))+dss(n)
      enddo
C
      read(21) mdat(1:numsum)
c
      lt=0
      DO l=1,366

      adat(1:numsum)=0.0
      DO m=1,16                  ! sum of two vars ( rofsd rofbd ) & 1day (3hourly*8)
      read(11,end=555) idat(1:numsum)
      adat(1:numsum)=adat(1:numsum)+idat(1:numsum)/8.0
      ENDDO

      rdat(:)=0.0D0
      do n=1,nn
         ij=ijriv(n)
         k=numdt(n)
         rdat(ij)=rdat(ij)+dss(n)*adat(k)*mdat(k)
      enddo
      rdat(:)=rdat(:)/radat(:)
      lt=lt+1
      read(22,rec=lt) (tfact(ij),ij=1,nx*ny)
      do ij=1,nx*ny
         rdat(ij) = max( rdat(ij)* tfact(ij), 0.D0)
      enddo
      write(12,rec=lt) (rdat(ij),ij=1,nx*ny) 
      ENDDO
 555  continue
cc      write(6,*) 'lt=',lt,'(',lt,'days)'

      STOP
      END

         
      
