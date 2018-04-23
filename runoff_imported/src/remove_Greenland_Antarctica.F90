!-*-F90-*-
program remote_Greenland_Antarctica

  integer(4),parameter :: nx = 1440, ny = 720
  character(len=64) :: cfile_org = 'data_org/runoff????.grd'
  character(len=64) :: cfile_new = 'data_new/runoff.????' 
  character(len=64) :: cfile_Gld = 'data_new/runoff_Greenland.????' 
  character(len=64) :: cfile_ice='data_org/runoff_ice.grd'

  character(len=64) :: cfile_new_total='data_new/total_runoff.????'
  character(len=64) :: cfile_GLD_total='data_new/total_runoff_Greenland.????'
  character(len=64) :: cfile_Antarctica_total='data_new/total_Antarctica.cnst'

  integer(4),parameter :: iunit1 = 11, iunit2 = 12
  integer(4),parameter :: ounit1 = 21, ounit2 = 22, ounit3 = 23, ounit4 = 24
  real(4) :: runoff_org(nx,ny), runoff_new(nx,ny), runoff_ice(nx,ny)
  real(4) :: runoff_Gld(nx,ny) 
  integer(4) :: nyr, irec

  real(8) :: total_runoff, total_runoff_Greenland
  real(8) :: total_Greenland_ice, total_Greenland_liquid, total_Antactic_ice

  real(4) :: rtotal
  integer(4) :: ileap

  character(len=64) :: cfile_nextxy_org = &
       & 'data_etc/nextxy_wo_lake_big_endian_noyrev_lon0strt.bin'
!       & 'data_etc/nextxy_big_endian_noyrev_lon0strt.bin'
  character(len=64) :: cfile_nextxy_new = &
       & 'data_etc/nextxy_wo_lake_big_endian_noyrev_lon0strt_rm_GA.bin'
!       & 'data_etc/nextxy_big_endian_noyrev_lon0strt_rm_GA.bin'
  integer(4) :: nextx_org(nx,ny), nexty_org(nx,ny)
  integer(4) :: nextx_new(nx,ny), nexty_new(nx,ny)

  character(len=64) :: cfile_CF = &
       & 'data_Antarctica/CF_025x025.dat'
  real(4) :: runoff_CF(nx,ny)
  character(len=64) :: cfile_BMF = &
       & 'data_Antarctica/BMF_025x025.dat'
  real(4) :: runoff_BMF(nx,ny)
!  character(len=64) :: cfile_Ant_total = &
!       & 'data_Antarctica/Antarctica_total_025x025.dat'       
  real(4) :: runoff_Ant_Total(nx,ny)

  integer(4), parameter :: next_Ant = -777
  integer(4), parameter :: next_Grn = -888

  integer(4) :: i, j

  !-----------------------------------------------------------------------------

  open(iunit2,file=cfile_ice,form='unformatted',access='direct',recl = 4*nx*ny)
  read(iunit2,rec=1) runoff_ice
  close(iunit2)
  
  open(iunit2,file=cfile_CF,form='unformatted',access='direct',recl = 4*nx*ny)
  read(iunit2,rec=1) runoff_CF
  close(iunit2)

  open(iunit2,file=cfile_BMF,form='unformatted',access='direct',recl = 4*nx*ny)
  read(iunit2,rec=1) runoff_BMF
  close(iunit2)

  runoff_Ant_Total(:,:) = runoff_CF(:,:)  + runoff_BMF(:,:)

  total_Antarctic_ice = 0.d0
  do j = 1, ny
    do i = 1, nx
      total_Antarctic_ice = total_Antarctic_ice + runoff_Ant_total(i,j)
    end do
  end do
  total_Antarctic_ice = total_Antarctic_ice * 1.d-6
  write(*,'(A,F12.5,A)') 'total_Antarctic_ice', total_Antarctic_ice, ' Sv'

!  open(iunit2,file=cfile_Ant_total,form='unformatted',access='direct',recl = 4*nx*ny)
!  write(iunit2,rec=1) runoff_Ant_total
!  close(iunit2)

  open(iunit2,file=cfile_nextxy_org,form='unformatted',access='direct',recl=4*nx*ny)
  read(iunit2,rec=1) nextx_org
  read(iunit2,rec=2) nexty_org
  close(iunit2)

  nextx_new(:,:) = nextx_org(:,:)
  nexty_new(:,:) = nexty_org(:,:)
  do j = 1,ny/2
    do i = 1, nx
      if ( (runoff_ice(i,j) > 0 .or. j <= 121) .and. nextx_org(i,j) == -9 ) then
        nextx_new(i,j) = -999
        nexty_new(i,j) = -999
      end if
      if ( runoff_Ant_total(i,j) > 0.d0) then
        nextx_new(i,j) = next_Ant
        nexty_new(i,j) = next_Ant
      end if
    end do
  end do
  do j = ny/2+1, ny
    do i = 1, nx
      if ( (i <= nx/2 .and. runoff_ice(i+nx/2,j) > 0.d0) .or. &
        &  (i >  nx/2 .and. runoff_ice(i-nx/2,j) > 0.d0) ) then
        nextx_new(i,j) = next_Grn
        nexty_new(i,j) = next_Grn
      end if
    end do
  end do

  open(iunit2,file=cfile_nextxy_new,form='unformatted',access='direct',recl=4*nx*ny)
  write(iunit2,rec=1) nextx_new
  write(iunit2,rec=2) nexty_new
  close(iunit2)

  open(iunit2,file=cfile_Antarctica_total,form='unformatted',access='direct',recl=4)
  write(iunit2,rec=1) real(total_Antarctic_ice,4)
  close(iunit2)

  do nyr = 1958, 2015

    write(cfile_org(16:19),'(I4.4)') nyr
    write(cfile_new(17:20),'(I4.4)') nyr
    write(cfile_Gld(27:30),'(I4.4)') nyr
    
    write(cfile_new_total(23:26),'(I4.4)') nyr
    write(cfile_GLD_total(33:36),'(I4.4)') nyr
    write(*,*) trim(cfile_org)

    if ( mod(nyr,4) == 0 ) then
      ileap = 1
    else
      ileap = 0
    end if

    open(iunit1,file=cfile_org,form='unformatted',access='direct',recl = 4*nx*ny)

    open(ounit1,file=cfile_new,form='unformatted',access='direct',recl = 4)
    open(ounit2,file=cfile_Gld,form='unformatted',access='direct',recl = 4)

    open(ounit3,file=cfile_new_total,form='unformatted',access='direct',recl = 4)
    open(ounit4,file=cfile_Gld_total,form='unformatted',access='direct',recl = 4)

    do irec = 1, 365 + ileap
      read(iunit1,rec=irec) runoff_org
      call remove_Greenland
      call write_newrunoff
      call write_Greenland
    end do

    close(iunit1)
    close(ounit1)
    close(ounit2)
    close(ounit3)
    close(ounit4)

  end do
!  write(*,*) total_Greenland_ice / 365.d0 * 1.d-6
!  write(*,*) total_Greenland_liquid /365.d0 * 1.d-6
!  write(*,*) total_Greenland_ice * 1.d-6
!  write(*,*) total_Greenland_liquid * 1.d-6


contains
subroutine remove_Greenland
  integer(4) :: i, j  

  total_Greenland_ice    = 0.d0
  total_Greenland_liquid = 0.d0
  do j = ny/2+1, ny
    do i = 1, nx
      if ( runoff_ice(i,j) > 0 ) then  
        total_Greenland_liquid = total_Greenland_liquid + runoff_org(i,j)
        total_Greenland_ice    = total_Greenland_ice    + runoff_ice(i,j)
        runoff_Gld(i,j)  = runoff_org(i,j) + runoff_ice(i,j)
        runoff_org(i,j)  = 0.d0
      end if
    end do
  end do
end subroutine remove_Greenland

subroutine write_newrunoff
  integer(4) :: i, j  

  total_runoff = 0.d0
  do j = 1, ny
    do i = 1, nx
      if ( i <= nx/2 ) then
        write(ounit1,rec=nx*ny*(irec-1)+nx*(j-1)+i) runoff_org(i+nx/2,j)
        total_runoff = total_runoff + runoff_org(i+nx/2,j)
      else
        write(ounit1,rec=nx*ny*(irec-1)+nx*(j-1)+i) runoff_org(i-nx/2,j)
        total_runoff = total_runoff + runoff_org(i-nx/2,j)
      end if
    end do
  end do

  total_runoff = 0.d0
  do j = 1, ny
    do i = 1, nx
      total_runoff = total_runoff + runoff_org(i,j)
    end do
  end do
  rtotal = total_runoff*1.d-6
  write(ounit3,rec=irec) rtotal


end subroutine write_newrunoff


subroutine write_Greenland
  integer(4) :: i, j  

  do j = 1, ny
    do i = 1, nx
      if ( i <= nx/2 ) then
        write(ounit2,rec=nx*ny*(irec-1)+nx*(j-1)+i) runoff_Gld(i+nx/2,j)
      else
        write(ounit2,rec=nx*ny*(irec-1)+nx*(j-1)+i) runoff_Gld(i-nx/2,j)
      end if
    end do
  end do

  total_runoff_Greenland = 0.d0
  do j = 1, ny
    do i = 1, nx
      total_runoff_Greenland = total_runoff_Greenland + runoff_Gld(i,j)
    end do
  end do
  rtotal = total_runoff_Greenland*1.d-6
  write(ounit4,rec=irec) rtotal

end subroutine write_Greenland

subroutine change_index
  integer(4) :: im(46), jm(46)
  integer(4) :: iriv
! 1. Amazon (South America, Atlantic)
      iriv=1; im(iriv)=523; jm(iriv)=360
! 2. Congo
      iriv=2; im(iriv)=770; jm(iriv)=336
! 3. Orinoco (South America, Atlantic)
      iriv=3; im(iriv)=478; jm(iriv)=395
! 4. Changjiang (Pacific)
      iriv=4; im(iriv)=1207; jm(iriv)=486
! 5. Brahmaputra (Indian)
      iriv=5; im(iriv)=1085; jm(iriv)=451
! 6. Mississippi (North America, Atlantic)
      iriv=6; im(iriv)=363; jm(iriv)=476
! 7. Yenisey
      iriv=7; im(iriv)=1052; jm(iriv)=648
! 8. Parana and Uruguay  (South America, Atlantic)
      iriv=8; im(iriv)=488; jm(iriv)=224
! 9. Lena
      iriv=9; im(iriv)=1236; jm(iriv)=649
! 10. Mekong
      iriv=10; im(iriv)=1146; jm(iriv)=399
! 11. Tocantins (South America, Atlantic) 
      iriv=11; im(iriv)=526; jm(iriv)=355
! 12. Ob
      iriv=12; im(iriv)=1003; jm(iriv)=626
! 13. Ganges (Indian)
      iriv=13; im(iriv)=1073; jm(iriv)=449
! 14. Irrawaddy (Indian)
      iriv=14; im(iriv)=1099; jm(iriv)=424
! 15. St Lawrence  (North America, Atlantic)
      iriv=15; im(iriv)=441; jm(iriv)=552
! 16. Amur  (Pacific)
      iriv=16; im(iriv)=1284; jm(iriv)=573
! 17. Machenzie
      iriv=17; im(iriv)=172; jm(iriv)=636
! 18. Xijiang
      iriv=18; im(iriv)=1173; jm(iriv)=449
! 19. Columbia (Pacific)
      iriv=19; im(iriv)=225; jm(iriv)=545
! 20. Magdalena (South America Atlantic)
      iriv=20; im(iriv)=421; jm(iriv)=405
! 21. Yukon
      iriv=21; im(iriv)=61; jm(iriv)=612
! 22. Atrato river (South America Atlantic)
      iriv=22; im(iriv)=413; jm(iriv)=394
! 23. Danube
      iriv=23; im(iriv)=839; jm(iriv)=541
! 24. Niger
      iriv=24; im(iriv)=745; jm(iriv)=378
! 25. Ogooue river (Gabon Atlantic)
      iriv=25; im(iriv)=756; jm(iriv)=356
! 26. Essequibo river (Atlantic south america)
      iriv=26; im(iriv)=487; jm(iriv)=389
! 27. Fraser river (canada pacific)
      iriv=27; im(iriv)=228; jm(iriv)=557
! 28. Pechora river (Arctic)
      iriv=28; im(iriv)=937; jm(iriv)=633
! 29. Nelson (Hadoson bay)
      iriv=29; im(iriv)=352; jm(iriv)=589
! 30. Khatanga river (Arctic)
      iriv=30; im(iriv)=1147; jm(iriv)=654
! 31. Sepik river (New Guinea )
      iriv=31; im(iriv)=1298; jm(iriv)=345
! 32. Kolyma river (Arctic sea  )
      iriv=32; im(iriv)=1367; jm(iriv)=638
! 33. Zambeze (Africa Indian Ocean)
      iriv=33; im(iriv)=866; jm(iriv)=285
! 34. Severnaya Dvina river (Arctic sea  )
      iriv=34; im(iriv)=881; jm(iriv)=620
! 35. Indus
      iriv=35; im(iriv)=990; jm(iriv)=456
! 36. Sanaga river (Africa, Atlantic  )
      iriv=36; im(iriv)=759; jm(iriv)=375
! 37. Godavari river (India  )
      iriv=37; im(iriv)=1050; jm(iriv)=427
! 38. Sao Francisco river (Atlantic)
      iriv=38; im(iriv)=575; jm(iriv)=318
! 39. Sacramento river + San joaquin river (USA Pacific)
      iriv=39; im(iriv)=230; jm(iriv)=512
! 40. Nile
      iriv=40; im(iriv)=842; jm(iriv)=486
! 41. Murray
      iriv=41; im(iriv)=1276; jm(iriv)=218
! 42. Senegal
      iriv=42; im(iriv)=655; jm(iriv)=427
! 43. Colorado
      iriv=43; im(iriv)=263; jm(iriv)=487
! 44. Orange x
      iriv=44; im(iriv)=786; jm(iriv)=246
! 45. Huanghe river (yellow river, China)
      iriv=45; im(iriv)=1197; jm(iriv)=512
! 46. Rio Grande river (Mexico )
      iriv=46; im(iriv)=332; jm(iriv)=464  

      do iriv = 1, 46
        if ( im(iriv) <= nx/2 ) then
          im(iriv) = im(iriv) + nx/2
        else
          im(iriv) = im(iriv) - nx/2
        end if
        write(*,'(I2.2, I5)') iriv, im(iriv)
      end do
end subroutine change_index

end program remote_Greenland_Antarctica
