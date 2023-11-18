set xlabel "time"
set ylabel "fuel"
m = "./FuelTimeData.csv"
set terminal x11 0
set nokey
set grid
set title "Moon landing fuel time plot"
plot m using 1:2 with linespoints
