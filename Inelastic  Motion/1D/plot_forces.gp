# This Gnuplot script produces a plot showing the forces acting on a falling disc over time.
# The script:
# 1. Sets the output terminal to PNG with a specified size, font, and styling.
# 2. Names the output image file.
# 3. Configures plot labels (title, axes, grid) for clarity.
# 4. Plots multiple data sets from "sim_data_adaptive.tx":
#    - Height vs Time (as a reference to see motion over time)
#    - Net Force vs Time
#    - Contact Force vs Time
#
# This provides a visual representation of how forces evolve as the disc falls and interacts
# with the ground or other surfaces. The viewer can interpret how the net force changes over time,
# correlate it with the height of the disc, and understand the role of contact force at impacts.


set terminal pngcairo size 1280,720 enhanced font 'Arial,14'  # Use a high-quality PNG output with specified dimensions and font
set output 'falling_disc_forces.png'                         # Name of the output file that will contain the generated plot

set title "Forces on the disc over time"                     # Overall title of the plot, describing what is being shown
set xlabel "Time (s)"                                        # Label for the x-axis, representing time in seconds
set ylabel "Force (N)"                                       # Label for the y-axis, representing force in Newtons
set grid                                                     # Enable grid lines for easier data reading
set key outside                                              # Position the legend/key outside the plot area, improving clarity

# The "plot" command reads from the data file "sim_data_adaptive.tx".
# Assuming the data columns are: (1) Time, (2) Height, ... (9) Net Force, (10) Contact Force.
# The "using" clause selects which columns to plot:
#    "using 1:2"  means Time vs Height
#    "using 1:9"  means Time vs Net Force
#    "using 1:10" means Time vs Contact Force
# Each plot is styled with 'with lines' and given a title for identification in the legend.
plot "sim_data_adaptive.tx" using 1:2 with lines title "Height vs Time", \
     "sim_data_adaptive.tx" using 1:9 with lines title "Net Force", \
     "sim_data_adaptive.tx" using 1:10 with lines title "Contact Force"
