program find_waterhead
  use mod_waterhead
  implicit none

  call waterhead__ini
  call waterhead__read
  call waterhead__main
  call waterhead__write
  
end program find_waterhead

