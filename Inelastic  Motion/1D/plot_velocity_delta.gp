# Set terminal and output format
set terminal pngcairo size 1280,720 enhanced font 'Arial,14'

###############################
# First Plot: Velocity and Height vs Time
###############################
set output 'Falling_Disc_Velocity_over_Time.png'
set title "Falling Disc Velocity over Time"
set xlabel "Time (s)"                                 # X-axis represents time
set ylabel "Velocity (m/s) and Height (m)"            # Shared Y-axis for both dimensions
set grid                                              # Enable grid for better readability
set key outside                                       # Legend positioned outside the plot

# Plot velocity and height on the same Y-axis
plot "sim_data_adaptive.tx" using 1:2 with lines title "Height vs Time", \
     "sim_data_adaptive.tx" using 1:3 with lines title "Velocity vs Time"

# Reset settings before the second plot
unset ylabel
unset title
unset grid
unset key

###############################
# Second Plot: Delta vs Time
###############################
set output 'Falling_Disc_Delta_over_Time.png'
set title "Falling Disc Delta over Time"
set xlabel "Time (s)"                                 # X-axis represents time
set ylabel "Delta (m)"                                # Y-axis for compression (delta)
set grid                                              # Enable grid for better readability
set key outside                                       # Legend positioned outside the plot

# Plot delta and height for context
plot "sim_data_adaptive.tx" using 1:2 with lines title "Height vs Time", \
     "sim_data_adaptive.tx" using 1:4 with lines title "Delta vs Time"

