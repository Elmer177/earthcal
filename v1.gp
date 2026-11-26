set terminal pngcairo size 800,600
set output "v1.png"

set xlabel "t [min]"
set ylabel "K (people who know)"
set xrange [0:50]
set yrange [0:30]
set grid
set key right bottom
set title "v1: Shizuka keeps talking"

plot "v1_b100.dat" u 1:3 w l lw 2 lc rgb "#d62728" t "{/Symbol b} = 1.0 (RK4)", \
     ""            u 1:4 w l lw 1 dt 2 lc "black" notitle, \
     "v1_b042.dat" u 1:3 w l lw 2 lc rgb "#ff7f0e" t "{/Symbol b} = 0.42 (RK4)", \
     ""            u 1:4 w l lw 1 dt 2 lc "black" notitle, \
     "v1_b030.dat" u 1:3 w l lw 2 lc rgb "#1f77b4" t "{/Symbol b} = 0.3 (RK4)", \
     ""            u 1:4 w l lw 1 dt 2 lc "black" t "exact", \
     15 w l lw 1.5 lc "gray" t "K = 15"
