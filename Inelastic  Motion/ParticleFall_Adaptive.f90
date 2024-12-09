PROGRAM ParticleFall_Adaptive
  IMPLICIT NONE

  ! Parameters
  REAL, PARAMETER :: g = 9.81
  REAL, PARAMETER :: H = 30.0
  REAL, PARAMETER :: R = 1.0
  REAL, PARAMETER :: m = 1.0
  REAL, PARAMETER :: k = 1000.0
  REAL, PARAMETER :: c = 5.0
  REAL, PARAMETER :: t_end = 50.0

  ! Variables
  REAL :: dt, t, v, z, F_contact, delta, F_vis
  REAL :: dt_min, dt_max, dt_stored, t_next_store
  REAL :: tol, error_norm
  REAL :: z_mid, v_mid, z_new_1, v_new_1, z_new_2, v_new_2
  REAL :: E_p, E_k, E_s, E_tot, F_net
  INTEGER :: unit

  CHARACTER(LEN=20) :: filename

  ! Initialization
  z = H
  v = 0.0
  t = 0.0

  dt = 0.0001
  dt_min = 1.0e-6
  dt_max = 0.01
  dt_stored = 0.001
  t_next_store = dt_stored
  tol = 1.0e-5

  filename = "sim_data_adaptive.txt"
  unit = 10
  OPEN(unit, FILE=filename, STATUS='REPLACE')
  WRITE(unit, '(A)') "t z v delta E_tot E_p E_k E_s F_net F_contact F_vis"

  ! Initial energies and forces
  CALL ComputeDeltaForces(z, v, R, k, c, g, delta, F_contact, F_vis)
  E_p = m*g*z
  E_k = 0.5*m*v*v
  IF (delta > 0.0) THEN
     E_s = 0.5*k*delta*delta
  ELSE
     E_s = 0.0
  END IF
  E_tot = E_p + E_k + E_s
  F_net = F_contact + F_vis - m*g

  WRITE(unit, '(F10.5, 2F10.5, F10.5, 4F10.5, 3F10.5)') &
       t, z, v, delta, E_tot, E_p, E_k, E_s, F_net, F_contact, F_vis

  PRINT *, "Starting simulation with adaptive time stepping..."

  DO WHILE (t < t_end)
    CALL ComputeDeltaForces(z, v, R, k, c, g, delta, F_contact, F_vis)
    IF (ABS(v) < 1.0E-5 .AND. delta > 0.0) EXIT

    IF (t + dt > t_end) dt = t_end - t

    ! Full step
    CALL RK4_Step(z, v, dt, R, k, c, g, z_new_1, v_new_1)

    ! Two half steps
    CALL RK4_Step(z, v, dt/2.0, R, k, c, g, z_mid, v_mid)
    CALL RK4_Step(z_mid, v_mid, dt/2.0, R, k, c, g, z_new_2, v_new_2)

    ! Error estimation
    error_norm = SQRT((z_new_1 - z_new_2)**2 + (v_new_1 - v_new_2)**2)

    IF (error_norm > tol .AND. dt > dt_min) THEN
       dt = MAX(dt/2.0, dt_min)
       CYCLE
    ELSEIF (error_norm < tol/10.0 .AND. dt < dt_max) THEN
       dt = MIN(dt*2.0, dt_max)
    END IF

    ! Accept step
    z = z_new_2
    v = v_new_2
    t = t + dt

    IF (t >= t_next_store) THEN
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

       WRITE(unit, '(F10.5, 2F10.5, F10.5, 4F10.5, 3F10.5)') &
            t, z, v, delta, E_tot, E_p, E_k, E_s, F_net, F_contact, F_vis
       t_next_store = t_next_store + dt_stored
    END IF

  END DO

  PRINT *, "Simulation finished. Data in ", filename
  CALL SYSTEM('gnuplot ./plot_dissipation.gp')
  CALL SYSTEM('gnuplot ./plot_energy.gp')
  CALL SYSTEM('gnuplot ./plot_forces.gp')

  CLOSE(unit)

END PROGRAM ParticleFall_Adaptive


SUBROUTINE ComputeDeltaForces(z, v, R, k, c, g, delta, F_contact, F_vis)
  IMPLICIT NONE
  REAL, INTENT(IN) :: z, v, R, k, c, g
  REAL, INTENT(OUT) :: delta, F_contact, F_vis

  IF (z >= R) THEN
    delta = 0.0
  ELSEIF (z > 0.0) THEN
    delta = R - z
  ELSE
    PRINT *, "BUG DETECTED: Ball penetrated below ground: z=", z
    STOP
  END IF

  IF (delta > 0.0) THEN
    F_contact = k * delta
    F_vis = -c * v
  ELSE
    F_contact = 0.0
    F_vis = 0.0
  END IF
END SUBROUTINE ComputeDeltaForces

SUBROUTINE Derivatives(z, v, R, k, c, g, dzdt, dvdt)
  IMPLICIT NONE
  REAL, INTENT(IN) :: z, v, R, k, c, g
  REAL, INTENT(OUT) :: dzdt, dvdt
  REAL :: delta, F_contact, F_vis

  CALL ComputeDeltaForces(z, v, R, k, c, g, delta, F_contact, F_vis)
  dzdt = v
  dvdt = (F_contact + F_vis)/1.0 - g   ! mass = 1.0 kg
END SUBROUTINE Derivatives

SUBROUTINE RK4_Step(z, v, dt, R, k, c, g, z_out, v_out)
  IMPLICIT NONE
  REAL, INTENT(IN) :: z, v, dt, R, k, c, g
  REAL, INTENT(OUT) :: z_out, v_out
  REAL :: k1z, k2z, k3z, k4z
  REAL :: k1v, k2v, k3v, k4v
  REAL :: z_temp, v_temp

  CALL Derivatives(z, v, R, k, c, g, k1z, k1v)

  z_temp = z + 0.5*dt*k1z
  v_temp = v + 0.5*dt*k1v
  CALL Derivatives(z_temp, v_temp, R, k, c, g, k2z, k2v)

  z_temp = z + 0.5*dt*k2z
  v_temp = v + 0.5*dt*k2v
  CALL Derivatives(z_temp, v_temp, R, k, c, g, k3z, k3v)

  z_temp = z + dt*k3z
  v_temp = v + dt*k3v
  CALL Derivatives(z_temp, v_temp, R, k, c, g, k4z, k4v)

  z_out = z + (dt/6.0)*(k1z + 2.0*k2z + 2.0*k3z + k4z)
  v_out = v + (dt/6.0)*(k1v + 2.0*k2v + 2.0*k3v + k4v)
END SUBROUTINE RK4_Step
