#!/bin/bash

echo "Compiling the Fortran code..."
gfortran -o main_adaptive.exe ParticleFall_Adaptive.f90
if [ $? -ne 0 ]; then
  echo "Compilation failed!"
  exit 1
fi
echo "Compilation successful!"

echo "Running the simulation..."
./main_adaptive.exe
if [ $? -ne 0 ]; then
  echo "Simulation failed!"
  exit 1
fi
echo "Simulation completed!"

echo "Generating the velocity & delta plot..."
gnuplot ./plot_velocity_delta.gp
if [ $? -ne 0 ]; then
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
