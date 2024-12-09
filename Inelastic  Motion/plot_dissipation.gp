set terminal pngcairo size 1280,720 enhanced font 'Arial,14'
set output 'Glass_particle_fall_dissipation.png'

set title "Glass Bead Falling with Dissipation Simulation"
set xlabel "Time (s)"
set ylabel "Values"
set grid
set key outside

# Set y-axis labels for clarity (since we have multiple plots)
set ytics nomirror
set y2label "Delta (m)"
set y2tics

# Plot height, velocity, and delta
plot "sim_data_dissipation" using 1:2 with lines title "Height vs Time", \
     "sim_data_dissipation" using 1:3 with lines title "Velocity vs Time", \
     "sim_data_dissipation" using 1:4 axes x1y2 with lines title "Delta vs Time"
