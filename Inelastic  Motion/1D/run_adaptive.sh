# This Bash script automates the process of:
# 1. Compiling the Fortran simulation code.
# 2. Running the resulting executable.
# 3. Generating various plots (velocity & delta, energy, forces) using gnuplot.
# The script provides error checks at each stage to ensure that if something fails,
# it stops and reports the issue, making it easier for us to diagnose problems.

#!/bin/bash

echo "Compiling the Fortran code..."
gfortran -o main_adaptive.exe ParticleFall_Adaptive.f90
if [ $? -ne 0 ]; then
  # If the exit code of the previous command is not zero, compilation failed.
  echo "Compilation failed!"
  exit 1
fi
echo "Compilation successful!"

echo "Running the simulation..."
./main_adaptive.exe
if [ $? -ne 0 ]; then
  # If the executable fails to run or returns an error code, report it.
  echo "Simulation failed!"
  exit 1
fi
echo "Simulation completed!"

echo "Generating the velocity & delta plot..."
gnuplot ./plot_velocity_delta.gp
if [ $? -ne 0 ]; then
  # If gnuplot cannot run the script successfully, plotting failed.
  echo "Velocity & Delta plotting failed!"
  exit 1
fi
echo "Velocity & Delta plot created!"

echo "Generating the energy plot..."
gnuplot ./plot_energy.gp
if [ $? -ne 0 ]; then
  echo "Energy plotting failed!"
  exit 1
fi
echo "Energy plot created!"

echo "Generating the forces plot..."
gnuplot ./plot_forces.gp
if [ $? -ne 0 ]; then
  echo "Forces plotting failed!"
  exit 1
fi
echo "Forces plot created!"

echo "All tasks completed successfully!"
