set terminal pngcairo size 1280,720 enhanced font 'Arial,14'
set output 'Glass_particle_fall_adaptive.png'

set title "Glass Bead Falling with Adaptive Dissipation Simulation"
set xlabel "Time (s)"
set ylabel "Height/Velocity"
set grid
set key outside

set ytics nomirror
set y2label "Delta (m)"
set y2tics

plot "sim_data_adaptive.tx" using 1:2 with lines title "Height vs Time", \
     "sim_data_adaptive.tx" using 1:3 with lines title "Velocity vs Time", \
     "sim_data_adaptive.tx" using 1:4 axes x1y2 with lines title "Delta vs Time"
