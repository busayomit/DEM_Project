import turtle
import time
import os
from PIL import Image

# Constants
gravity = -9.8           # Gravitational acceleration (pixels per second^2)
dissipation = 0.7        # Energy loss per bounce (coefficient of restitution)
time_step = 0.02         # Time increment for simulation (seconds)
air_resistance = 0.01    # Air resistance coefficient
friction_coefficient = 0.05  # Coefficient of friction

# Ball properties
mass = 5                 # Mass of the ball (kilograms)
x, y = -300, 10          # Starting position of the ball
vx, vy = 30, -10         # Initial velocity of the ball (pixels per second)
radius = 10              # Radius of the ball

# Velocity threshold for stopping
velocity_threshold = 0.1

# Screen setup
screen = turtle.Screen()
screen.bgcolor("white")
screen.setup(width=450, height=450)
screen.tracer(0)  # Turn off automatic updates for frame control

# Ball setup
ball = turtle.Turtle()
ball.shape("circle")
ball.color("blue")
ball.penup()
ball.goto(x, y)

# Frame capture list
frames = []
frame_count = 0

while True:
    # Air resistance force (opposite direction of velocity)
    air_force_x = -air_resistance * vx
    air_force_y = -air_resistance * vy

    # Friction force (only when the ball is on the ground)
    if y - radius <= -screen.window_height() // 2:
        normal_force = mass * abs(gravity)  # Normal force due to gravity
        friction_force = friction_coefficient * normal_force
        if vx > 0:
            vx -= (friction_force / mass) * time_step
            if vx < 0:
                vx = 0  # Stop completely if velocity is very small
        elif vx < 0:
            vx += (friction_force / mass) * time_step
            if vx > 0:
                vx = 0  # Stop completely if velocity is very small

    # Update velocity with gravity and air resistance
    vx += (air_force_x / mass) * time_step
    vy += (gravity + (air_force_y / mass)) * time_step

    # Update position
    x += vx * time_step
    y += vy * time_step

    # Collision with ground
    if y - radius < -screen.window_height() // 2:
        y = -screen.window_height() // 2 + radius  # Reset position
        vy = -vy * dissipation  # Reverse and reduce vertical velocity

    # Collision with walls
    if x - radius < -screen.window_width() // 2:
        x = -screen.window_width() // 2 + radius
        vx = -vx * dissipation  # Reverse and reduce horizontal velocity
    elif x + radius > screen.window_width() // 2:
        x = screen.window_width() // 2 - radius
        vx = -vx * dissipation  # Reverse and reduce horizontal velocity

    # Update ball position
    ball.goto(x, y)

    # Update the screen
    screen.update()

    # Capture the screen and append to frames
    canvas = screen.getcanvas()
    frame_file = f"frame_{frame_count:03d}.eps"
    canvas.postscript(file=frame_file)
    frames.append(frame_file)
    frame_count += 1

    # Pause for a short time to control simulation speed
    time.sleep(time_step)

    # Check if both velocities are effectively zero
    if abs(vx) < velocity_threshold and abs(vy) < velocity_threshold:
        print("Ball has come to rest. Exiting simulation.")
        break

# After breaking out of the loop, convert the frames to GIF
print("Converting frames to GIF...")
images = [Image.open(frame).convert("RGB") for frame in frames]
images[0].save("ball_simulation.gif", save_all=True, append_images=images[1:], duration=50, loop=0)
print("GIF saved as 'ball_simulation.gif'")

# This is to remove the EPS files after conversion
for frame in frames:
    os.remove(frame)

print("Cleaned up EPS frames.")
