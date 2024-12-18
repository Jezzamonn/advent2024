import re
import sys

import numpy as np

# Regex that parses the following:
# Button A: X+94, Y+34
# Button B: X+22, Y+67
# Prize: X=8400, Y=5400

pattern = r"""Button A: X\+(\d+), Y\+(\d+)
Button B: X\+(\d+), Y\+(\d+)
Prize: X=(\d+), Y=(\d+)"""

# Read everything from stdin
input_str = sys.stdin.read()

class Coordinates:
    def __init__(self, button_a, button_b, prize):
        self.button_a = np.array(button_a, dtype=int)
        self.button_b = np.array(button_b, dtype=int)
        self.prize = np.array(prize, dtype=int)

score = 0

matches = re.findall(pattern, input_str, re.MULTILINE)
for match in matches:
    button_a = (match[0], match[1])
    button_b = (match[2], match[3])
    prize = (match[4], match[5])
    coordinates = Coordinates(button_a, button_b, prize)

    # print(f"Button A: {coordinates.button_a}")
    # print(f"Button B: {coordinates.button_b}")
    # print(f"Prize: {coordinates.prize}")

    # Check if button_a, button_b, and prize are colinear
    vector_ab = coordinates.button_b - coordinates.button_a
    vector_ap = coordinates.prize - coordinates.button_a
    cross_product = np.cross(vector_ab, vector_ap)

    if np.all(cross_product == 0):
        raise Exception("Button A, Button B, and Prize are colinear.")

    # Solve vector equation to find x and y such that
    # x * button_a + y * button_b = prize
    A = np.vstack([coordinates.button_a, coordinates.button_b]).T
    x, y = np.linalg.lstsq(A, coordinates.prize, rcond=None)[0]

    print(f"x: {x}, y: {y}")

    # If x and y are approximately integers, rounding, print the sum.
    if np.isclose(x, round(x), rtol = 0, atol = 0.0001) and np.isclose(y, round(y), rtol = 0, atol = 0.0001):
        local_score = 3 * round(x) + round(y)
        score += local_score
        print(f"Solution found: {round(x)}, {round(y)}")
    else:
        print("No solution found.")
    print()

print(f"Final score: {score}")

