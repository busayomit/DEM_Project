# Set terminal and output
set terminal pngcairo size 1280,720 enhanced font 'Arial,14'

##############################
# Plot 1: Positions vs. Time #
##############################
set output 'positions_vs_time.png'
set title "Ball Positions Over Time"
set xlabel "Time (s)"
set ylabel "Position (m)"
set grid
set key outside

plot "sim_data_with_rotation.txt" using 1:3 with lines title "Z(t) (height)", \
     "sim_data_with_rotation.txt" using 1:2 with lines title "X(t) (horizontal)"

##############################
# Plot 2: Trajectory (x vs z)#
##############################
set output 'trajectory.png'
set title "Ball Trajectory (X vs Z)"
set xlabel "X (m)"
set ylabel "Z (m)"
set grid
set key outside
# The ball starts at z=H and x=0, so we plot z (col 3) vs x (col 2)
plot "sim_data_with_rotation.txt" using 2:3 with lines title "Trajectory"

###################################
# Plot 3: Rotation Angle vs. Time #
###################################
set output 'rotation_angle_vs_time.png'
set title "Rotation Angle Over Time"
set xlabel "Time (s)"
set ylabel "Theta (radians)"
set grid
set key outside

plot "sim_data_with_rotation.txt" using 1:6 with lines title "Theta(t)"

#####################################
# Plot 4: Angular Velocity vs. Time #
#####################################
set output 'angular_velocity_vs_time.png'
set title "Angular Velocity Over Time"
set xlabel "Time (s)"
set ylabel "Omega (rad/s)"
set grid
set key outside

plot "sim_data_with_rotation.txt" using 1:7 with lines title "Omega(t)"

# After these runs, you will have four PNG files:
# positions_vs_time.png, trajectory.png, rotation_angle_vs_time.png, angular_velocity_vs_time.png
# Feel free to add or modify plots as needed.
