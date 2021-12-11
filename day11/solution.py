import inputs

def make_grid(lines):
    return [ [int(c) for c in line] for line in lines ]

deltas = [-1, 0, +1]

def dim(grid):
    return len(grid), len(grid[0])

def step_neighbors_inplace(grid, i, j):
    m, n = dim(grid)
    for ni in [i+d for d in deltas if i+d >= 0 and i+d < m]:
        for nj in [j+d for d in deltas if j+d >=0 and j+d < n and (ni != i or d != 0)]:
            grid[ni][nj] = grid[ni][nj] + 1

def get_flashes(grid, exclude):
    m, n = dim(grid)
    return [ (i,j) for i in range(m) for j in range(n) if grid[i][j] > 9 and (i,j) not in exclude ]

def reset_inplace(grid, octopi):
    for (i, j) in octopi:
        grid[i][j] = 0

def step(grid):
    m, n = dim(grid)
    next_grid = [ [g + 1 for g in row] for row in grid ]
    prior_flashes = set()
    flashes = get_flashes(next_grid, prior_flashes)
    while flashes:
        prior_flashes.update(flashes)
        for (fi, fj) in flashes:
            step_neighbors_inplace(next_grid, fi, fj)
        flashes = get_flashes(next_grid, prior_flashes)
    reset_inplace(next_grid, prior_flashes)
    return next_grid, len(prior_flashes)

def show_octopus(o):
    return str(o) if o else f'\033[31m{o}\033[0m'

def show_grid(grid):
    return '\n'.join(''.join(show_octopus(g) for g in row) for row in grid)

g, f = step(make_grid(inputs.mini_sample))
assert f == 9

def part1(lines, steps=100, debug=False):
    grid = make_grid(lines)
    if debug:
        print(f'\n{0}:\n')
        print(show_grid(grid))
    flashes = 0
    for i in range(steps):
        grid, f = step(grid)
        if debug:
            print(f'\n{i+1}:\n')
            print(show_grid(grid))
        flashes = flashes + f
    return flashes

assert part1(inputs.sample, 10) == 204
assert part1(inputs.sample, 100) == 1656

print(part1(inputs.full, 100))

def part2(lines):
    grid = make_grid(lines)
    s = 0
    flashes = 0
    m, n = dim(grid)
    while True:
        s = s + 1
        grid, flashes = step(grid)
        if flashes == m * n:
            return s

assert part2(inputs.sample) == 195
assert part2(inputs.full) == 273
