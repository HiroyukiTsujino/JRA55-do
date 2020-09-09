C--     ifort -assume byterecl 
      PROGRAM CALDENS
C
      IMPLICIT NONE
      INTEGER, PARAMETER :: nx=1440, ny=720
      INTEGER, PARAMETER :: nriv=46, nbasin=16
      INTEGER, DIMENSION(nriv) :: im, jm, ic,jc
C
      INTEGER, PARAMETER :: nt=720
C
      REAL(4), DIMENSION(nx, ny) ::  rivm, rmi, rmi2, rivg, rivs
      REAL(4), DIMENSION(nx, ny) ::  runoff, mrunoff
      REAL(4), DIMENSION(nbasin) ::  rbasin
      REAL(4) :: lat(ny), lon(nx), mlat, mlon, dy
      REAL(4) :: rmiss = -9999.E0
      Character*4 :: cn
      Character*2 :: rn, bn
      INTEGER :: i, j, l, ii, jj, iriv, lt, lm, ld, lr, lb,ll,ln, it
      INTEGER :: len, olen, ilen, isyear, ieyear
C
      integer(4) :: ileap, iy1, iy2, iy3
      integer(4),dimension(12) :: imon
      data imon  /31, 29, 31, 30 ,31, 30, 31, 31, 30, 31, 30, 31/

      character(64) :: cdir, odir, idir
C      cdir='/mwork/tsuzuki/CaMa_out/'
      cdir='/work113/htsujino/CaMa-Flood/OUTPUT'
      odir='/riv_v0.7_mri'
      idir='/JRA55_tfact2_rivmap160805_mri'
      len=len_trim(cdir)
      olen=len_trim(odir)
      ilen=len_trim(idir)
      isyear=2017
      ieyear=2017
C
C --- set Region Selection Index
c  0:land,  1:southern ocean, 2:atlantic, 3:pacific, 4:arctic, 5:indian,
c  6: mediterranean, 7:black sea, 8: hudson_bay, 9:baltic sea 10: red sea
C  11:Atlantic western boundary, 12:atlantic eastern boundary,
C  13:pacific western boundary , 14:pacific eastern boundary,
C  15:indain western boundary,   16:indian eastern boundary,
C 1. Amazon (South America, Atlantic)
      iriv=1; im(iriv)=523; jm(iriv)=360; ic(iriv)=129; jc(iriv)=90 ! 51.75W 0.75S
C      iriv=1; im(iriv)=523; jm(iriv)=360; ic(iriv)=125; jc(iriv)=88 ! 55.25W 2.75S <- Tapajos river ?
C      iriv=1; im(iriv)=523; jm(iriv)=360; ic(iriv)=128; jc(iriv)=88 ! 52.25W 2.25S <- Xingu river ?
C 2. Congo
      iriv=2; im(iriv)=770; jm(iriv)=336; ic(iriv)=193; jc(iriv)=85 ! 12.75W 5.75S
C 3. Orinoco (South America, Atlantic)
      iriv=3; im(iriv)=478; jm(iriv)=395; ic(iriv)=119; jc(iriv)=100 ! 61.75W 9.25N
C 4. Changjiang (Pacific)
      iriv=4; im(iriv)=1207; jm(iriv)=486; ic(iriv)=301; jc(iriv)=123 ! 120.75E 32.25N
C 5. Brahmaputra (Indian)
      iriv=5; im(iriv)=1085; jm(iriv)=451; ic(iriv)=270; jc(iriv)=115 ! 89.75E 24.5N
C 6. Mississippi (North America, Atlantic)
CC -old version      iriv=6; im(iriv)=355; jm(iriv)=478; ic(iriv)=90; jc(iriv)=120 
      iriv=6; im(iriv)=363; jm(iriv)=476; ic(iriv)=90; jc(iriv)=120  ! 90.25W 29.75N
C 7. Yenisey
      iriv=7; im(iriv)=1052; jm(iriv)=648; ic(iriv)=264; jc(iriv)=161 ! 83.25E 70.75N
C 8. Parana and Uruguay  (South America, Atlantic)
      iriv=8; im(iriv)=488; jm(iriv)=224; ic(iriv)=123; jc(iriv)=56 ! 57.75W 34.75S <-Parana
      iriv=8; im(iriv)=488; jm(iriv)=224; ic(iriv)=122; jc(iriv)=57 ! 58.25W 33.75S <-Urguay
C 9. Lena
      iriv=9; im(iriv)=1236; jm(iriv)=649; ic(iriv)=307; jc(iriv)=163 ! 126.75E 72.5N
C 10. Mekong
      iriv=10; im(iriv)=1146; jm(iriv)=399; ic(iriv)=287; jc(iriv)=100 ! 106.25E 9.75N
C 11. Tocantins (South America, Atlantic) 
      iriv=11; im(iriv)=526; jm(iriv)=355; ic(iriv)=131; jc(iriv)=88  ! 49.25W 2.25S
C 12. Ob
      iriv=12; im(iriv)=1003; jm(iriv)=626; ic(iriv)=250; jc(iriv)=157 ! 69.25E 66.75N
C 13. Ganges (Indian)
      iriv=13; im(iriv)=1073; jm(iriv)=449; ic(iriv)=269; jc(iriv)=115 ! 88.25E 24.25N
C 14. Irrawaddy (Indian)
      iriv=14; im(iriv)=1099; jm(iriv)=424; ic(iriv)=276; jc(iriv)=108 ! 95.75E 17.75N
C 15. St Lawrence  (North America, Atlantic)
      iriv=15; im(iriv)=441; jm(iriv)=552; ic(iriv)=113; jc(iriv)=139 ! 67.75W 48.75N
C 16. Amur  (Pacific)
      iriv=16; im(iriv)=1284; jm(iriv)=573; ic(iriv)=321; jc(iriv)=142 ! 140.25W 51.75N
C 17. Machenzie
      iriv=17; im(iriv)=172; jm(iriv)=636; ic(iriv)=46; jc(iriv)=159 ! 134.25W 68.75N
C 18. Xijiang
C -old version    iriv=18; im(iriv)=1174; jm(iriv)=449; ic(iriv)=294; jc(iriv)=113 
      iriv=18; im(iriv)=1173; jm(iriv)=449; ic(iriv)=294; jc(iriv)=113 ! 113.25E 22.25N
C 19. Columbia (Pacific)
      iriv=19; im(iriv)=225; jm(iriv)=545; ic(iriv)=57; jc(iriv)=137 ! 123.75W 46.25N
C 20. Magdalena (South America Atlantic)
CC -ola version     iriv=20; im(iriv)=422; jm(iriv)=405; ic(iriv)=106; jc(iriv)=101
      iriv=20; im(iriv)=421; jm(iriv)=405; ic(iriv)=106; jc(iriv)=101 ! 74.75W 10.75N
C 21. Yukon
      iriv=21; im(iriv)=61; jm(iriv)=612; ic(iriv)=16; jc(iriv)=153 ! 164.25W 62.75N
C 22. Atrato river (South America Atlantic ***Panama
      iriv=22; im(iriv)=413; jm(iriv)=394; ic(iriv)=103; jc(iriv)=97 ! 77.25W 6.25N <- Into atlantic 
C 23. Danube
      iriv=23; im(iriv)=839; jm(iriv)=541; ic(iriv)=209; jc(iriv)=136 ! 28.75E 45.25N
C 24. Niger
      iriv=24; im(iriv)=745; jm(iriv)=378; ic(iriv)=187; jc(iriv)=95 ! 6.25E 4.75N
C 25. Ogooue river (Gabon Atlantic)
      iriv=25; im(iriv)=756; jm(iriv)=356; ic(iriv)=190; jc(iriv)=90 ! 9.75E 0.75S
C 26. Essequibo river (Atlantic south america)
      iriv=26; im(iriv)=487; jm(iriv)=389; ic(iriv)=122; jc(iriv)=97 ! 58.75W 6.25N
C 27. Fraser river (canada pacific)
      iriv=27; im(iriv)=228; jm(iriv)=557; ic(iriv)=58; jc(iriv)=139 ! 122.75W 48.75N
C 28. Pechora river (Arctic)
      iriv=28; im(iriv)=937; jm(iriv)=633; ic(iriv)=234; jc(iriv)=158 ! 53.25E 67.75N
C 29. Nelson (Hadoson bay)
      iriv=29; im(iriv)=352; jm(iriv)=589; ic(iriv)=88; jc(iriv)=148 ! 92.75W 57.25N
C 30. Khatanga river (Arctic)
      iriv=30; im(iriv)=1147; jm(iriv)=654; ic(iriv)=287; jc(iriv)=164 ! 106.75E 73.25N 
C 31. Sepik river (New Guinea )
      iriv=31; im(iriv)=1298; jm(iriv)=345; ic(iriv)=324; jc(iriv)=87 ! 143.25E 3.75S
C 32. Kolyma river (Arctic sea  )
      iriv=32; im(iriv)=1367; jm(iriv)=638; ic(iriv)=341; jc(iriv)=159 ! 16075E. 68.75N
C 33. Zambeze (Africa Indian Ocean)
C -- old version      iriv=26; im(iriv)=866; jm(iriv)=286; ic(iriv)=216; jc(iriv)=72
      iriv=33; im(iriv)=866; jm(iriv)=285; ic(iriv)=216; jc(iriv)=72 ! 35.75E 18.25S
C 34. Severnaya Dvina river (Arctic sea  )
      iriv=34; im(iriv)=881; jm(iriv)=620; ic(iriv)=221; jc(iriv)=155 ! 40.25E 64.75N
C 35. Indus
      iriv=35; im(iriv)=990; jm(iriv)=456; ic(iriv)=249; jc(iriv)=115 ! 68.25E 24.75N
C 36. Sanaga river (Africa, Atlantic  )
      iriv=36; im(iriv)=759; jm(iriv)=375; ic(iriv)=191; jc(iriv)=94 ! 10.25E 3.75N
C 37. Godavari river (India  )
      iriv=37; im(iriv)=1050; jm(iriv)=427; ic(iriv)=262; jc(iriv)=107 ! 81.75E 16.75N
C 38. Sao Francisco river (Atlantic)
      iriv=38; im(iriv)=575; jm(iriv)=318; ic(iriv)=144; jc(iriv)=80 ! 36.75W 10.25S
C 39. Sacramento river + San joaquin river (USA Pacific)
      iriv=39; im(iriv)=230; jm(iriv)=512; ic(iriv)=58; jc(iriv)=128 ! 122.25W 37.75N <- Sacramento 
      iriv=39; im(iriv)=230; jm(iriv)=512; ic(iriv)=59; jc(iriv)=128 ! 121.26W 37.68N <- San Joaquin

C ------------large drainage basin rivers
C 40. Nile
C  old version    iriv=28; im(iriv)=848; jm(iriv)=486; ic(iriv)=209; jc(iriv)=121
      iriv=40; im(iriv)=842; jm(iriv)=486; ic(iriv)=209; jc(iriv)=121 ! 
      iriv=40; im(iriv)=842; jm(iriv)=486; ic(iriv)=212; jc(iriv)=122 ! 31.25E 31.25N   
C 41. Murray
      iriv=41; im(iriv)=1276; jm(iriv)=218; ic(iriv)=320; jc(iriv)=55 ! 139.25E 35.25S
C 42. Senegal
      iriv=42; im(iriv)=655; jm(iriv)=427; ic(iriv)=164; jc(iriv)=107 ! 16.25W 16.25N *
C 43. Colorado
      iriv=43; im(iriv)=263; jm(iriv)=487; ic(iriv)=66; jc(iriv)=122 ! 114.75W 31.75N
C 44. Orange 
      iriv=44; im(iriv)=786; jm(iriv)=246; ic(iriv)=197; jc(iriv)=62 ! 16.75E 28.25S *
C 45. Huanghe river (yellow river, China)
      iriv=45; im(iriv)=1197; jm(iriv)=512; ic(iriv)=297; jc(iriv)=127 ! 116.75E 36.75N
C 46. Rio Grande river (Mexico )
      iriv=46; im(iriv)=332; jm(iriv)=464; ic(iriv)=83; jc(iriv)=116 ! 97.25E 25.75N

      OPEN (12, FILE = 'rmi.grd',
     &          form='unformatted', access='direct',
     &          recl=4*nx*ny,convert='big_endian')
      read(12,rec=1) ((rmi(i,j),i=1,nx),j=1,ny)
      close(12)
      OPEN (12, FILE = 'rmi_greenland.grd',
     &          form='unformatted', access='direct',
     &          recl=4*nx*ny,convert='big_endian')
      read(12,rec=1) ((rmi2(i,j),i=1,nx),j=1,ny)
      close(12)
C
      rivm(:,:)=0.0
      rivg(:,:)=0.0
      rivs(:,:)=0.0
      do j=1,ny
      do i=1,nx
         if(rmi(i,j).gt.1.0) rivm(i,j)=1.0
         if(rmi(i,j).eq.1.0) rivs(i,j)=1.0
         if(rmi2(i,j).gt.0.0) rivg(i,j)=1.0
      enddo
      enddo
C---  Set latlon
      do i=1,nx
         lon(i)= - 179.875 + 0.25 * real(i-1)
      enddo
      do j=1,ny
         lat(j)= - 89.875 +0.25 *real(j-1)
      enddo
C---- factor file
      open(22,file=''//cdir(1:len)//''//odir(1:olen)//'/runoff_ice.grd',
     & form='unformatted',access='direct',status='new',recl=4*nx*ny,
     & convert='big_endian')
C---- start time loop
C      it=0
      do lt =isyear, ieyear
CC      do lt=1958, 1979
CC      do lt=1994,2015
         write(cn,'(i4.4)') lt
C --- Input file
       open(11,
     & file=''//cdir(1:len)//''//idir(1:ilen)//'/outflw'//cn//'.bin',
     & form='unformatted',access='direct',status='old',recl=4*nx*ny,
     & convert='little_endian')
C --- Output file
       open(21,
     & file=''//cdir(1:len)//''//odir(1:olen)//'/runoff'//cn//'.grd',
     & form='unformatted',access='direct',status='new',recl=4*nx*ny,
     & convert='big_endian')
       open(51,
     & file=''//cdir(1:len)//''//odir(1:olen)//'/runoff'//cn//'mon.grd',
     & form='unformatted',access='direct',status='new',recl=4*nx*ny,
     & convert='big_endian')



         iy1 = mod(lt,400) 
         iy2 = mod(lt,100) 
         iy3 = mod(lt,4)
         if( iy1 == 0) then
            ileap = 1
         elseif( iy2 == 0) then
            ileap = 0
         elseif( iy3 == 0) then
            ileap = 1
         else
            ileap = 0
         endif
         write(6,*) lt,ileap
         if(ileap ==1 ) then
            imon(2)=29
         else
            imon(2)=28
         endif
C
         lr=0
         do lm=1,3
C            it=it+1
            write(6,*) 'month=', lm
            mrunoff(:,:)=0.0D0
            do ld=1,imon(lm)

              lr=lr+1
              write(6,*) lr
              read(11,rec=lr, err=555) ((runoff(i,j),i=1,nx),j=ny,1,-1) ! yrev

              do j=1,ny
              do i=1,nx
                 runoff(i,j)=max(runoff(i,j)*rivm(i,j),0.0D0)
                 if(rivg(i,j).eq.1.0) then
                    runoff(i,j)=runoff(i,j) * 0.01319/0.0020947 ! for liquid discharge from greenland
                 endif
              enddo
              enddo
              
              write(21,rec=lr) ((runoff(i,j),i=1,nx),j=1,ny)   ! m3/sec

              mrunoff(:,:)=mrunoff(:,:)+runoff(:,:)

            enddo

            mrunoff(:,:)=mrunoff(:,:)/real(imon(lm))

            write(51,rec=lm) ((mrunoff(i,j),i=1,nx),j=1,ny)   ! m3/sec
C
         enddo
      enddo
      goto 55
 555  continue
      write(6,*) 'to month (',lm-1 , ')', lt
 55   continue


      runoff(:,:)=0.0
      do j=1,ny
         do i=1,nx
             runoff(i,j)=rivs(i,j)/2259.0 * 0.073 * 1000000 ! m3/sec for antarctica 
             runoff(i,j)=runoff(i,j)
     &                 + rivg(i,j)/751.0 * 0.01576 * 1000000 ! m3/sec for greenland 
         enddo
      enddo


            write(22,rec=1) ((runoff(i,j),i=1,nx),j=1,ny)   ! m3/sec

      stop
      end


