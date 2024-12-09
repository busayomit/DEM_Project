PROGRAM ParticleFall_dissipation

  IMPLICIT NONE

  ! Declare variables
  REAL :: dt_looped, dt_stored, t, t_end
  REAL :: g, m, H, v, z, F_contact, delta, F_vis, k, c, R
  INTEGER :: n_steps, n_store, store_counter
  CHARACTER(LEN=20) :: filename
  INTEGER :: unit

  ! Initialization
  g = 9.81                ! Acceleration due to gravity (m/s^2)
  H = 30.0               ! Initial height (m)
  R = 1.0                 ! Radius of glass bead (m)
  m = 1.0                 ! Mass of glass bead (kg)
  k = 1000.0              ! Spring constant for contact force (N/m)
  c = 5.0                 ! Dampening coefficient (N·s/m)
  dt_looped = 0.0001      ! Small simulation time step (s)
  dt_stored = 0.001        ! Larger storage time step (s)
  t = 0.0                 ! Initial time (s)
  t_end = 50.0            ! Simulation end time (s)
  n_steps = INT(t_end / dt_looped)  ! Total number of loop steps
  n_store = INT(dt_stored / dt_looped)  ! Steps between storage
  store_counter = 0       ! Counter for storage intervals

  z = H                   ! Initial position (m)
  v = 0.0                 ! Initial velocity (m/s)

  ! File setup
  filename = "sim_data_dissipation.txt"
  unit = 10
  OPEN(unit, FILE=filename, STATUS='REPLACE')

  ! Time loop
  DO store_counter = 1, n_steps
    t = store_counter * dt_looped

    ! Compute overlap delta
    IF (z >= R) THEN
      delta = 0.0
    ELSEIF (z > 0.0 .AND. z < R) THEN
      delta = R - z
    ELSE
      PRINT *, "BUG DETECTED: Ball penetrated below ground level at time =", t, "Position =", z
      EXIT
    END IF

    ! Compute contact force
    IF (delta > 0.0) THEN
      F_contact = k * delta
    ELSE
      F_contact = 0.0
    END IF

    ! Compute viscous force
    IF (delta > 0.0) THEN
      F_vis = c * ABS(v)
      IF (v > 0.0) THEN
        F_vis = -F_vis  ! Opposes upward motion
      END IF
    ELSE
      F_vis = 0.0  ! No viscous force if no contact
    END IF

    ! Update velocity and position using Euler's method
    v = v + ((F_contact + F_vis) / m - g) * dt_looped
    z = z + v * dt_looped
     
    ! Write results to file only at intervals of dt_stored
    IF (MOD(store_counter, n_store) == 0) THEN
      WRITE(unit, '(F10.3, 2F10.3, F10.3)') t, z, v, delta
      PRINT *, "Time (s):", t, "Position (m):", z, "Velocity (m/s):", v, "Delta:", delta, "F_vis:", F_vis
    END IF

    ! Stop simulation if the ball comes to rest on the ground
    IF (ABS(v) < 1.0E-5 .AND. delta > 0.0) EXIT
  END DO

  PRINT *, "Simulation data written to ", filename
  CALL SYSTEM('gnuplot ./plot_dissipation.gp')
  CLOSE(unit)

END PROGRAM ParticleFall_dissipation
