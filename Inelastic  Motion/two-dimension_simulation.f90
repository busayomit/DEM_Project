PROGRAM simulation_2d
    IMPLICIT NONE
  
    ! Parameters
    REAL, PARAMETER :: g = 9.81
    REAL, PARAMETER :: H = 30.0      ! Initial vertical height (m)
    REAL, PARAMETER :: R = 1.0       ! Ball radius (m)
    REAL, PARAMETER :: m = 1.0       ! Mass (kg)
    REAL, PARAMETER :: k = 1000.0    ! Spring constant (N/m)
    REAL, PARAMETER :: c = 5.0       ! Vertical damping coeff. (N·s/m)
    REAL, PARAMETER :: mu = 0.2      ! Friction coefficient
    REAL, PARAMETER :: t_end = 50.0
    REAL, PARAMETER :: I = (2.0/5.0)*m*R**2  ! Moment of inertia of solid sphere
  
    ! Variables
    REAL :: dt, t
    REAL :: x, z, v_x, v_z
    REAL :: theta, omega
    REAL :: F_contact, F_vis, F_fric, delta
    REAL :: E_tot, E_p, E_k, E_s
    REAL :: F_net_x, F_net_z
  
    INTEGER :: unit
    CHARACTER(LEN=30) :: filename
  
    ! Initialization
    x = 0.0;     v_x = 0.0
    z = H;       v_z = 0.0
    theta = 0.0; omega = 0.0
    t = 0.0
    dt = 0.001
  
    filename = "sim_data_with_rotation.txt"
    unit = 10
    OPEN(unit, FILE=filename, STATUS='REPLACE')
    WRITE(unit, '(A)') "t x z v_x v_z theta omega E_tot E_p E_k E_s F_contact F_vis F_fric"
  
    ! Initial forces & energies
    CALL ComputeForcesAndEnergies(x, z, v_x, v_z, R, k, c, g, mu, delta, &
    F_contact, F_vis, F_fric, E_tot, E_p, E_k, E_s, F_net_x, F_net_z)
    WRITE(unit, '(F10.5, 2F10.5, 2F10.5, 2F10.5, 4F10.5)') t, x, z, v_x, v_z, theta, &
    omega, E_tot, E_p, E_k, E_s, F_contact, F_vis, F_fric
  
    PRINT *, "Starting 2D simulation with rotation and Euler time stepping..."
  
    DO WHILE (t < t_end)
      CALL ComputeForcesAndEnergies(x, z, v_x, v_z, R, k, c, g, mu, delta, F_contact, &
       F_vis, F_fric, E_tot, E_p, E_k, E_s, F_net_x, F_net_z)
  
      ! Simple stopping condition: if ball settled
      IF (ABS(v_z)<1.0E-5 .AND. delta>0.0 .AND. ABS(v_x)<1.0E-5 .AND. ABS(omega)<1.0E-5) EXIT
  
      ! Update with Euler Method
      ! Positions
      x = x + dt*v_x
      z = z + dt*v_z
      theta = theta + dt*omega
  
      ! Accelerations
      ! dv_x/dt = F_fric/m
      v_x = v_x + dt*(F_fric/m)
  
      ! dv_z/dt = (F_contact + F_vis - m*g)/m
      v_z = v_z + dt*((F_contact + F_vis - m*g)/m)
  
      ! dω/dt = (F_fric*R)/I
      omega = omega + dt*((F_fric*R)/I)
  
      t = t + dt
  
      ! Store data periodically (for example, every 0.01 s)
      IF (MOD(INT(t*1000),10)==0) THEN
         CALL ComputeForcesAndEnergies(x, z, v_x, v_z, R, k, c, g, mu, delta, F_contact, &
          F_vis, F_fric, E_tot, E_p, E_k, E_s, F_net_x, F_net_z)
         WRITE(unit, '(F10.5, 2F10.5, 2F10.5, 2F10.5, 4F10.5)') t, x, z, v_x, v_z, theta, &
         omega, E_tot, E_p, E_k, E_s, F_contact, F_vis, F_fric
      END IF
  
    END DO
  
    PRINT *, "Simulation finished. Data in ", filename
    CLOSE(unit)
  
  CONTAINS
  
    SUBROUTINE ComputeForcesAndEnergies(x, z, v_x, v_z, R, k, c, g, mu, delta, &
       F_contact, F_vis, F_fric, E_tot, E_p, E_k, E_s, F_net_x, F_net_z)
      IMPLICIT NONE
      REAL, INTENT(IN) :: x, z, v_x, v_z, R, k, c, g, mu
      REAL, INTENT(OUT) :: delta, F_contact, F_vis, F_fric
      REAL, INTENT(OUT) :: E_tot, E_p, E_k, E_s, F_net_x, F_net_z
      REAL :: normal_force
  
      IF (z >= R) THEN
         delta = 0.0
      ELSEIF (z > 0.0) THEN
         delta = R - z
      ELSE
         PRINT *, "BUG DETECTED: Ball penetrated below ground: z=", z
         STOP
      END IF
  
      ! Vertical direction
      IF (delta > 0.0) THEN
         F_contact = k*delta
         F_vis = -c*v_z
      ELSE
         F_contact = 0.0
         F_vis = 0.0
      END IF
  
      ! Friction force
      IF (delta > 0.0) THEN
         normal_force = F_contact
         IF (v_x == 0.0) THEN
            F_fric = 0.0
         ELSE
            F_fric = -mu*normal_force*SIGN(1.0, v_x)
         END IF
      ELSE
         F_fric = 0.0
      END IF
  
      E_p = m*g*z
      E_k = 0.5*m*(v_x**2 + v_z**2)
      IF (delta > 0.0) THEN
         E_s = 0.5*k*(delta**2)
      ELSE
         E_s = 0.0
      END IF
      E_tot = E_p + E_k + E_s
  
      F_net_z = F_contact + F_vis - m*g
      F_net_x = F_fric
    END SUBROUTINE ComputeForcesAndEnergies

    SUBROUTINE WriteDataAsArray(input_file, output_file)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: input_file, output_file
      REAL, DIMENSION(:,:), ALLOCATABLE :: data
      INTEGER :: n_rows, n_cols, i, j, unit_in, unit_out
      CHARACTER(LEN=500) :: line
      CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: headers
  
      ! Open the input file to determine the size of the data
      OPEN(UNIT=20, FILE=input_file, STATUS='OLD', ACTION='READ')
      READ(20, '(A)') line  ! Read headers
      ALLOCATE(headers(14))  ! Adjust size to match number of columns
      READ(line, '(14A20)') headers
  
      n_rows = 0
      DO
          READ(20, '(A)', IOSTAT=i) line
          IF (i /= 0) EXIT
          n_rows = n_rows + 1
      END DO
      REWIND(20)
  
      ! Read the data into an array
      n_cols = 14
      ALLOCATE(data(n_rows, n_cols))
  
      READ(20, '(A)') line  ! Skip headers
      DO i = 1, n_rows
          READ(20, *) (data(i, j), j = 1, n_cols)
      END DO
      CLOSE(20)
  
      ! Write the array format to the output file
      OPEN(UNIT=30, FILE=output_file, STATUS='REPLACE', ACTION='WRITE')
      WRITE(30, '(A)') "Array Format Data"
      WRITE(30, '(14A20)') (headers(j), j = 1, n_cols)
  
      DO i = 1, n_rows
          WRITE(30, '(14F10.5)') (data(i, j), j = 1, n_cols)
      END DO
      CLOSE(30)
  
      ! Deallocate arrays
      DEALLOCATE(data, headers)
  END SUBROUTINE WriteDataAsArray


  END PROGRAM simulation_2d
