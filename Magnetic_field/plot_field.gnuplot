set title 'B Field (plane at y=0.0) (Tesla)' 
unset grid
set xlabel 'Z(cm)' 
set xtics 
set ylabel 'X(cm)' 
set ytics 
unset logscale x
unset logscale y
unset logscale z
unset logscale cb
unset logscale x2
unset logscale y2
set key default
set xrange [540:760]
set yrange [-50:50]
set style line 1 lt -1 lw 1
set style line 2 lt 6 lw 1
vscale=0.1
set colorbox vertical
set pm3d map explicit corners2color c1
set format cb "%.1tE%+03T"
set log cb
set palette rgb 33,13,10
set palette maxcolors 32
splot 'MOLLER-hallA_plot09.dat' ind 1 us 5:3:11 notitle,'' ind 0 us 5:3:(0) w l ls 1 notitle
