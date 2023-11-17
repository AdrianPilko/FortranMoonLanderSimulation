set xlabel "time"
set ylabel "altitude"
m = "./flightData.csv"
set terminal x11 0
set nokey
set grid
set title "Moon landing altitude time plot"
plot m using 1:2 with linespoints
