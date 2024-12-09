set terminal pngcairo size 1280,720 enhanced font 'Arial,14'
set output 'particle_fall.png'

set title "Particle Falling Simulation"
set xlabel "Time (s)"
set ylabel "Height (m)"
set grid

plot "simulation_data.txt" using 1:2 with lines title "Height vs Time",
     "simulation_data.txt" using 1:3 with lines title "Velocity vs Time"
