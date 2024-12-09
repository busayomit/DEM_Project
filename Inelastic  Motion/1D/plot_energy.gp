set terminal pngcairo size 1280,720 enhanced font 'Arial,14'  # Use a high-quality PNG output with specified dimensions and font
set output 'falling_disc_energy.png'

set title "Energy Dissipation of falling disc over Time"
set xlabel "Time (s)"
set ylabel "Height (m)"
set grid
set key outside

# Initial total energy (E0) from the first line of data
E0 = 294.41318  # <-- This is the actual initial total energy I got from column 5

# Y2 axis for energy
set ytics nomirror
set y2label "Energy Dissipated (J)"
set y2tics

# Plot height (column 2) vs time (column 1) on left axis
# Plot energy dissipated: E0 - E(t) on right axis
plot "sim_data_adaptive.tx" using 1:2 with lines title "Height vs Time", \
     "sim_data_adaptive.tx" using 1:(E0 - $5) axes x1y2 with lines title "Energy Dissipated vs Time"
