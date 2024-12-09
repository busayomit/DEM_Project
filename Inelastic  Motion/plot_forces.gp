set terminal pngcairo size 1280,720 enhanced font 'Arial,14'
set output 'Glass_particle_forces.png'

set title "Forces on the Ball Over Time"
set xlabel "Time (s)"
set ylabel "Force (N)"
set grid
set key outside

plot "sim_data_adaptive.tx" using 1:2 with lines title "Height vs Time", \
     "sim_data_adaptive.tx" using 1:9 with lines title "Net Force", \
     "sim_data_adaptive.tx" using 1:10 with lines title "Contact Force"
