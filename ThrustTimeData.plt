set xlabel "time"
set ylabel "thrust"
m = "./ThrustTimeData.csv"
set terminal x11 0
set nokey
set grid
set title "Moon landing engine thrust / time plot"
plot m using 1:2 with linespoints
