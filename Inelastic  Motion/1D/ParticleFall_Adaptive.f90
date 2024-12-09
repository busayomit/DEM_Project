PROGRAM ParticleFall_Adaptive
  IMPLICIT NONE

  ! Physical and simulation parameters
  REAL, PARAMETER :: g = 9.81        ! Acceleration due to gravity (m/s^2)
  REAL, PARAMETER :: H = 30.0        ! Initial height from which the particle is dropped (m)
  REAL, PARAMETER :: R = 1.0         ! Radius of the ball (m)
  REAL, PARAMETER :: m = 1.0         ! Mass of the ball (kg)
  REAL, PARAMETER :: k = 1000.0      ! Spring constant for contact force (N/m)
  REAL, PARAMETER :: c = 5.0         ! Damping coefficient for viscous force (N·s/m)
  REAL, PARAMETER :: t_end = 50.0    ! Total simulation time (s)

  ! Simulation variables
  REAL :: dt, t, v, z, F_contact, delta, F_vis
  REAL :: dt_min, dt_max, dt_stored, t_next_store
  REAL :: tol, error_norm
  REAL :: z_mid, v_mid, z_new_1, v_new_1, z_new_2, v_new_2
  REAL :: E_p, E_k, E_s, E_tot, F_net
  INTEGER :: unit

  CHARACTER(LEN=20) :: filename

  ! Initial conditions
  z = H              ! Start at height H
  v = 0.0            ! Start with zero velocity
  t = 0.0            ! Start time

  ! Time step parameters
  dt = 0.0001        ! Initial time step
  dt_min = 1.0e-6     ! Minimum time step for adaptive stepping
  dt_max = 0.01       ! Maximum time step
  dt_stored = 0.001   ! Interval at which data will be stored (output to file)
  t_next_store = dt_stored
  tol = 1.0e-5        ! Tolerance for adaptive time step adjustment

  filename = "sim_data_adaptive.txt"
  unit = 10
  OPEN(unit, FILE=filename, STATUS='REPLACE')

  ! Write header line to output file describing each column of data
  WRITE(unit, '(A)') "t z v delta E_tot E_p E_k E_s F_net F_contact F_vis"

  ! Compute initial forces and energies before starting the time loop
  CALL ComputeDeltaForces(z, v, R, k, c, g, delta, F_contact, F_vis)

  ! Compute potential energy (E_p = m*g*z)
  E_p = m*g*z
  ! Compute kinetic energy (E_k = 1/2 m v^2)
  E_k = 0.5*m*v*v
  ! Compute spring (elastic) energy (E_s = 1/2 k delta^2 if in contact)
  IF (delta > 0.0) THEN
     E_s = 0.5*k*delta*delta
  ELSE
     E_s = 0.0
  END IF
  ! Total energy is sum of potential, kinetic, and elastic energies
  E_tot = E_p + E_k + E_s
  ! Net vertical force: (contact + viscous) - weight
  F_net = F_contact + F_vis - m*g

  ! Write initial state to the file
  WRITE(unit, '(F10.5, 2F10.5, F10.5, 4F10.5, 3F10.5)') &
       t, z, v, delta, E_tot, E_p, E_k, E_s, F_net, F_contact, F_vis

  PRINT *, "Starting simulation with adaptive time stepping..."

  ! Main simulation loop
  DO WHILE (t < t_end)

    ! Update forces based on current position and velocity
    CALL ComputeDeltaForces(z, v, R, k, c, g, delta, F_contact, F_vis)

    ! Check if ball is essentially at rest on the ground (very small velocity and delta>0)
    IF (ABS(v) < 1.0E-5 .AND. delta > 0.0) EXIT

    ! Adjust final step if we are near the end time
    IF (t + dt > t_end) dt = t_end - t


      ! Perform a full time step using RK4 method
    CALL RK4_Step(z, v, dt, m, R, k, c, g, z_new_1, v_new_1)
    
        ! Perform two half steps (for error estimation)
    CALL RK4_Step(z, v, dt/2.0, m, R, k, c, g, z_mid, v_mid)
    CALL RK4_Step(z_mid, v_mid, dt/2.0, m, R, k, c, g, z_new_2, v_new_2)
    
    ! Estimate local error using the difference between one full step and two half steps
    error_norm = SQRT((z_new_1 - z_new_2)**2 + (v_new_1 - v_new_2)**2)

    ! Adaptive time stepping: if error is too large, decrease dt; if too small, increase dt
    IF (error_norm > tol .AND. dt > dt_min) THEN
       dt = MAX(dt/2.0, dt_min)
       CYCLE
    ELSEIF (error_norm < tol/10.0 .AND. dt < dt_max) THEN
       dt = MIN(dt*2.0, dt_max)
    END IF

    ! Accept the second (more accurate) solution
    z = z_new_2
    v = v_new_2
    t = t + dt

    ! Check if it's time to store the current state in the output file
    IF (t >= t_next_store) THEN
       ! Recompute forces and energies for logging
       CALL ComputeDeltaForces(z, v, R, k, c, g, delta, F_contact, F_vis)

       E_p = m*g*z
       E_k = 0.5*m*v*v
       IF (delta > 0.0) THEN
         E_s = 0.5*k*(delta**2)
       ELSE
         E_s = 0.0
       END IF
       E_tot = E_p + E_k + E_s
       F_net = F_contact + F_vis - m*g

       ! Write current state to file
       WRITE(unit, '(F10.5, 2F10.5, F10.5, 4F10.5, 3F10.5)') &
            t, z, v, delta, E_tot, E_p, E_k, E_s, F_net, F_contact, F_vis

       ! Schedule next data storage time
       t_next_store = t_next_store + dt_stored
    END IF

  END DO

  PRINT *, "Simulation finished. Data in ", filename

  ! Automatically run gnuplot scripts to generate plots
  CALL SYSTEM('gnuplot ./plot_velocity_delta.gp')
  CALL SYSTEM('gnuplot ./plot_energy.gp')
  CALL SYSTEM('gnuplot ./plot_forces.gp')

  CLOSE(unit)

END PROGRAM ParticleFall_Adaptive


!----------------------------------------------------------
! Subroutine: ComputeDeltaForces
! Purpose: Compute the vertical compression delta, contact force, and viscous force
!----------------------------------------------------------
SUBROUTINE ComputeDeltaForces(z, v, R, k, c, g, delta, F_contact, F_vis)
  IMPLICIT NONE
  REAL, INTENT(IN) :: z, v, R, k, c, g
  REAL, INTENT(OUT) :: delta, F_contact, F_vis

  ! Check how far the ball is from the ground surface.
  ! If z >= R, no contact (ball above ground)
  IF (z >= R) THEN
    delta = 0.0
  ELSEIF (z > 0.0) THEN
    ! If the center of the ball is below R but above 0, it is compressing
    delta = R - z
  ELSE
    ! If z < 0, the ball went below ground level, which should not happen
    PRINT *, "BUG DETECTED: Ball penetrated below ground: z=", z
    STOP
  END IF

  ! Compute contact and viscous forces only if in contact
  IF (delta > 0.0) THEN
    F_contact = k * delta         ! Spring force proportional to compression
    F_vis = -c * v                ! Viscous force opposes motion
  ELSE
    F_contact = 0.0
    F_vis = 0.0
  END IF
END SUBROUTINE ComputeDeltaForces


!----------------------------------------------------------
! Subroutine: Derivatives
! Purpose: Compute the time derivatives (dz/dt and dv/dt) given z and v
!----------------------------------------------------------
SUBROUTINE Derivatives(z, v, m, R, k, c, g, dzdt, dvdt)
  IMPLICIT NONE
  REAL, INTENT(IN) :: z, v, m, R, k, c, g  ! Include 'm' as an input argument
  REAL, INTENT(OUT) :: dzdt, dvdt
  REAL :: delta, F_contact, F_vis

  CALL ComputeDeltaForces(z, v, R, k, c, g, delta, F_contact, F_vis)

  dzdt = v                              ! Velocity is the derivative of position
  dvdt = (F_contact + F_vis)/m - g      ! Acceleration from sum of forces / mass
END SUBROUTINE Derivatives



!----------------------------------------------------------
! Subroutine: RK4_Step
! Purpose: Perform one Runge-Kutta 4th order time integration step
!----------------------------------------------------------
SUBROUTINE RK4_Step(z, v, dt, m, R, k, c, g, z_out, v_out)
  IMPLICIT NONE
  REAL, INTENT(IN) :: z, v, dt, m, R, k, c, g
  REAL, INTENT(OUT) :: z_out, v_out
  REAL :: k1z, k2z, k3z, k4z
  REAL :: k1v, k2v, k3v, k4v
  REAL :: z_temp, v_temp
  ! Compute k1 derivatives at the start
  CALL Derivatives(z, v, m, R, k, c, g, k1z, k1v)
  ! Compute k2 derivatives (at midpoint)
  z_temp = z + 0.5*dt*k1z
  v_temp = v + 0.5*dt*k1v
  CALL Derivatives(z_temp, v_temp, m, R, k, c, g, k2z, k2v)
  ! Compute k3 derivatives (another midpoint evaluation)
  z_temp = z + 0.5*dt*k2z
  v_temp = v + 0.5*dt*k2v
  CALL Derivatives(z_temp, v_temp, m, R, k, c, g, k3z, k3v)
  ! Compute k4 derivatives (at the end)
  z_temp = z + dt*k3z
  v_temp = v + dt*k3v
  CALL Derivatives(z_temp, v_temp, m, R, k, c, g, k4z, k4v)
  ! Combine all k's to get the final integrated result
  z_out = z + (dt/6.0)*(k1z + 2.0*k2z + 2.0*k3z + k4z)
  v_out = v + (dt/6.0)*(k1v + 2.0*k2v + 2.0*k3v + k4v)
END SUBROUTINE RK4_Step
