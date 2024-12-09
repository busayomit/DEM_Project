# Set terminal
set terminal pngcairo size 1280,720 enhanced font 'Arial,14'

# First Plot: Falling Disc Velocity over Time
set output 'Falling_Disc_Velocity_over_Time.png'
set title "Falling Disc Velocity over Time"
set xlabel "Time (s)"
set ylabel "Velocity (m/s)"
set y2label "Height (m)"
set grid
set key outside
set ytics nomirror
set y2tics
plot "sim_data_adaptive.tx" using 1:2 with lines title "Height vs Time", \
     "sim_data_adaptive.tx" using 1:3 with lines title "Velocity vs Time"

# Reset settings before the second plot
unset y2tics
unset ylabel
unset y2label
unset title
unset grid
unset key

# Second Plot: Falling Disc Delta over Time
set output 'Falling_Disc_Delta_over_Time.png'
set title "Falling Disc Delta over Time"
set xlabel "Time (s)"
set ylabel "Delta (m)"
set y2label "Height (m)"
set grid
set key outside
set ytics nomirror
set y2tics
plot "sim_data_adaptive.tx" using 1:2 axes x1y2 with lines title "Height vs Time", \
     "sim_data_adaptive.tx" using 1:4 with lines title "Delta vs Time"
