set title "Stream function"
set xlabel "[Month]"
set ylabel "Stream function [Sv]"
set key right bottom
set xrange [1:640]
set terminal postscript eps color solid "Time-Roman" 18
set output "stream.eps"
plot 'fort.15'    t " " with lines 2, \
     'fort.16'    t " " with lines 4, \
     'fort.17'    t " " with lines 3, \
     'fort.18'    t " " with lines 5
