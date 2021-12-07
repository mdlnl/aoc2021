import day07.inputs as inputs
from math import floor, ceil

def part1(xs):
    xs.sort()
    med = xs[floor(len(xs)/2)]
    return sum(abs(x - med) for x in xs)

print(part1(inputs.sample))
print(part1(inputs.full))

def nn12(n):
    a = abs(n)
    return int(a * (a + 1) / 2)

def part2(xs):
    mean = sum(xs) / len(xs)
    mean_floor = int(floor(mean))
    mean_ceil = int(ceil(mean))
    # total fuel = nsteps * (nsteps + 1) / 2
    return [
        sum(nn12(x - mean_floor) for x in xs),
        sum(nn12(x - mean_ceil) for x in xs)
    ]

print(part2(inputs.sample))
print(part2(inputs.full))