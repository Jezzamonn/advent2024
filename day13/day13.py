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
    def __init__(self, button_a: tuple[int, int], button_b: tuple[int, int], prize: tuple[int, int]):
        self.button_a = np.array(button_a, dtype=int)
        self.button_b = np.array(button_b, dtype=int)
        self.prize = np.array(prize, dtype=int)

matches = re.search(pattern, input_str, re.MULTILINE)
if matches:
    button_a = (matches.group(1), matches.group(2))
    button_b = (matches.group(3), matches.group(4))
    prize = (matches.group(5), matches.group(6))
    coordinates = Coordinates(button_a, button_b, prize)

    print(f"Button A: {coordinates.button_a}")
    print(f"Button B: {coordinates.button_b}")
    print(f"Prize: {coordinates.prize}")


