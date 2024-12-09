#!/bin/bash

echo "Compiling the Fortran code..."
gfortran -o main_dissipation.exe ParticleFall_dissipation.f90
if [ $? -ne 0 ]; then
  echo "Compilation failed!"
  exit 1
fi
echo "Compilation successful!"

echo "Running the simulation..."
./main_dissipation.exe
if [ $? -ne 0 ]; then
  echo "Simulation failed!"
  exit 1
fi
echo "Simulation completed!"

echo "Generating the dissipation plot..."
gnuplot ./plot_dissipation.gp
if [ $? -ne 0 ]; then
  echo "Dissipation plotting failed!"
  exit 1
fi
echo "Dissipation plot created!"

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
