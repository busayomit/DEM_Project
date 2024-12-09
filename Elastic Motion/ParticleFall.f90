PROGRAM ParticleFall
  IMPLICIT NONE

  ! Declare variables
  REAL :: dt, t, t_end
  REAL :: g, m, H, v, z, F_contact, delta, k
  INTEGER :: n, n_steps
  CHARACTER(LEN=20) :: filename
  INTEGER :: unit

  ! Initialization
  g = 9.81                ! Acceleration due to gravity (m/s^2)
  H = 10.0                ! Initial height (m)
  m = 1.0                 ! Mass of the ball (kg)
  k = 1000.0              ! Spring constant for contact force (N/m)
  dt = 0.01               ! Time step (s)
  t = 0.0                 ! Initial time (s)
  t_end = 25.0             ! Simulation end time (s)
  n_steps = INT(t_end / dt)
  
  z = H                   ! Initial position (m)
  v = 0.0                 ! Initial velocity (m/s)

  ! File setup
  filename = "simulation_data.txt"
  unit = 10
  OPEN(unit, FILE=filename, STATUS='REPLACE')

  ! Time loop
  DO n = 1, n_steps
    t = n * dt

    ! Compute overlap delta
    delta = MAX(0.0, -z)

    ! Compute contact force
    IF (delta > 0.0) THEN
      F_contact = k * delta
    ELSE
      F_contact = 0.0
    END IF

    ! Update velocity and position using Euler's method
    v = v + (F_contact / m - g) * dt
    z = z + v * dt

    ! Write results to file
    WRITE(unit, '(F10.3, 2F10.3)') t, z, v

    ! Stop simulation if the ball comes to rest on the ground
    IF (ABS(v) < 1.0E-5 .AND. delta > 0.0) EXIT
  END DO

  PRINT *, "Simulation data written to ", filename
  CLOSE(unit)

END PROGRAM ParticleFall
