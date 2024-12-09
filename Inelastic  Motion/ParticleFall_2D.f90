PROGRAM ParticleFall_2D
    IMPLICIT NONE
    !======================================================================
    ! This program simulates the motion of a disc in 2D space (x-z plane).
    ! The disc is subjected to gravity, a repulsive (contact) force from
    ! the ground, a damping force (when in contact), and friction force
    ! (when in contact and moving horizontally).
    !
    ! The disc falls under gravity in the z-direction and can bounce off
    ! the ground. When the disc touches the ground, a contact force pushes
    ! it upwards (if it penetrates the ground level), a damping force acts
    ! against the direction of penetration movement, and a frictional force
    ! acts opposite to any horizontal motion (x-direction).
    !
    ! The simulation uses Euler integration to update velocities and positions
    ! over time. The data are periodically written to a file for analysis.
    !======================================================================
  
    !----------------------------------------------------------------------
    ! Variable declarations
    REAL :: dt_looped      ! Small simulation time step
    REAL :: dt_stored      ! Larger time step interval for storage
    REAL :: t              ! Current simulation time
    REAL :: t_end          ! End time of simulation
    REAL :: g              ! Gravitational acceleration (m/s^2)
    REAL :: m              ! Mass of the disc (kg)
    REAL :: H              ! Initial height (m)
    REAL :: R              ! Radius of the disc (m)
    REAL :: k              ! Spring constant for contact force (N/m)
    REAL :: c              ! Damping coefficient (N·s/m)
    REAL :: mu             ! Coefficient of friction (dimensionless)
    REAL :: z, vz          ! Vertical position (z) and velocity (vz)
    REAL :: x, vx          ! Horizontal position (x) and velocity (vx)
    REAL :: delta          ! Overlap (penetration depth) with ground
    REAL :: F_contact      ! Contact force (normal) from ground
    REAL :: F_vis          ! Viscous (damping) force when in contact
    REAL :: F_fric         ! Frictional force when in contact
    REAL :: normal_force   ! The normal force (equal to contact force)
    INTEGER :: n_steps     ! Number of time steps in the simulation
    INTEGER :: n_store     ! Steps between data storage
    INTEGER :: store_counter ! Loop counter
    CHARACTER(LEN=50) :: filename ! Name of output file
    INTEGER :: unit        ! I/O unit number for file output
  
    !----------------------------------------------------------------------
    ! Initialization of parameters
    g = 9.81                ! Gravity (m/s^2) downward
    H = 30.0                ! Initial height (m)
    R = 1.0                 ! Radius of the disc (m)
    m = 1.0                 ! Mass (kg)
    k = 1000.0              ! Stiffness for contact force (N/m)
    c = 5.0                 ! Damping coefficient (N·s/m)
    mu = 0.3                ! Friction coefficient (dimensionless)
    dt_looped = 0.0001      ! Simulation time step (s)
    dt_stored = 0.001       ! Data storage interval (s)
    t = 0.0                 ! Initial time (s)
    t_end = 50.0            ! Simulation end time (s)
    n_steps = INT(t_end / dt_looped)   ! Total number of steps
    n_store = INT(dt_stored / dt_looped) ! Steps between storing data
  
    !----------------------------------------------------------------------
    ! Initial conditions
    z = H            ! Start at height H
    vz = 0.0         ! Initial vertical velocity
    x = 0.0          ! Start horizontally at 0
    vx = 2.0         ! Give some initial horizontal velocity for demonstration
  
    !----------------------------------------------------------------------
    ! File output setup
    filename = "sim_data_2D.txt"   ! Output data filename
    unit = 10
    OPEN(unit, FILE=filename, STATUS='REPLACE') ! Open file for writing
  
    ! Write header line in output file
    WRITE(unit,'("time(s)   x(m)    z(m)    vx(m/s)  vz(m/s)  delta(m)   F_contact(N)   F_vis(N)   F_fric(N)")')
  
    !----------------------------------------------------------------------
    ! Main simulation loop
    DO store_counter = 1, n_steps
  
      ! Update current time
      t = store_counter * dt_looped
  
      !---------------------------------------------------------------
      ! Compute overlap delta with ground
      ! If z < R, the disc penetrates the ground by (R - z)
      IF (z >= R) THEN
         delta = 0.0
      ELSEIF (z > 0.0 .AND. z < R) THEN
         delta = R - z
      ELSE
         PRINT *, "BUG DETECTED: Ball penetrated below ground level:", t, z
         EXIT
      END IF
  
      !---------------------------------------------------------------
      ! Compute contact (normal) force
      ! Only if delta > 0, otherwise no contact
      IF (delta > 0.0) THEN
         F_contact = k * delta
      ELSE
         F_contact = 0.0
      END IF
  
      !---------------------------------------------------------------
      ! Compute viscous (damping) force if in contact
      ! This force opposes the relative velocity of penetration
      ! The vertical velocity vz indicates if the disc is moving into or away from the ground
      IF (delta > 0.0) THEN
         F_vis = c * ABS(vz)
         IF (vz > 0.0) THEN
            F_vis = -F_vis   ! If moving upward, viscous force acts downward
         END IF
      ELSE
         F_vis = 0.0
      END IF
  
      !---------------------------------------------------------------
      ! Compute friction force if in contact and vx != 0
      ! Friction force = -mu * normal_force * sign(vx)
      ! normal_force = F_contact (only if delta > 0)
      IF (delta > 0.0) THEN
         normal_force = F_contact
         IF (ABS(vx) > 1.0E-10) THEN
            F_fric = -mu * normal_force * (vx / ABS(vx))  ! Direction opposite to vx
         ELSE
            F_fric = 0.0
         END IF
      ELSE
         F_fric = 0.0
         normal_force = 0.0
      END IF
  
      !---------------------------------------------------------------
      ! Equations of motion (Euler integration)
      !
      ! Vertical (z-direction):
      !   m * dvz/dt = -m*g + F_contact + F_vis
      !   Because gravity is downward, it is negative (assuming z upwards)
      !
      ! Horizontal (x-direction):
      !   m * dvx/dt = F_fric (only friction acts horizontally)
      !
  
      vz = vz + ((F_contact + F_vis - m*g) / m) * dt_looped
      z  = z  + vz * dt_looped
  
      vx = vx + (F_fric / m) * dt_looped
      x  = x  + vx * dt_looped
  
      !---------------------------------------------------------------
      ! Write data to file at intervals of dt_stored
      IF (MOD(store_counter, n_store) == 0) THEN
         WRITE(unit,'(F10.5,4F10.5,F10.5,3F10.5)') t, x, z, vx, vz, delta, F_contact, F_vis, F_fric
         PRINT *, "Time:", t, "x:", x, "z:", z, "vx:", vx, "vz:", vz, "delta:", delta
      END IF
  
      !---------------------------------------------------------------
      ! Stop if the disc is resting on the ground (delta>0 and very small velocity)
      IF (delta > 0.0 .AND. ABS(vz) < 1.0E-5 .AND. ABS(vx) < 1.0E-5) EXIT
  
    END DO
  
    !----------------------------------------------------------------------
    ! Finalize
    PRINT *, "Simulation data written to ", filename
    CLOSE(unit)
  
  END PROGRAM ParticleFall_2D
  