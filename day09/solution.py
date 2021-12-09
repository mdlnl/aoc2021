import day09.inputs as inputs

def parse(lines):
    return [
        [int(h) for h in line]
        for line in lines
    ]

def neighbors(m, n, i, j):
    nbrs = []
    if i > 0:
        nbrs.append((i - 1, j))
    if i < m - 1:
        nbrs.append((i + 1, j))
    if j > 0:
        nbrs.append((i, j - 1))
    if j < n - 1:
        nbrs.append((i, j + 1))
    return nbrs
        

def find_local_minima(height_map):
    m = len(height_map)
    n = len(height_map[0])
    minima = []
    for i in range(m):
        for j in range(n):
            res = [height_map[ni][nj] > height_map[i][j] for (ni, nj) in neighbors(m, n, i, j)]
            if all(res):
                minima.append((i, j))
    return minima

def risk_level(height_map, i, j):
    return height_map[i][i] + 1

def part1(height_map):
    return sum(height_map[i][j] + 1 for (i, j) in find_local_minima(height_map))

assert part1(parse(inputs.sample)) == 15
assert part1(parse(inputs.full)) == 425

def find_basin(height_map, i0, j0):
    queue = [(i0, j0)]
    m = len(height_map)
    n = len(height_map[0])
    basin = set()
    while queue:
        (i, j) = queue.pop(0)
        if height_map[i][j] < 9:
            basin.add((i, j))
            for (ni, nj) in neighbors(m, n, i, j):
                if (ni, nj) not in basin and (ni, nj) not in queue:
                    queue.append((ni, nj))
    return basin

def part2(height_map):
    basins = [
        find_basin(height_map, i, j)
        for (i, j) in find_local_minima(height_map)
    ]
    basin_sizes = [len(b) for b in basins]
    basin_sizes.sort()
    return basin_sizes[-1] * basin_sizes[-2] * basin_sizes[-3]

assert part2(parse(inputs.sample)) == 1134

print(part2(parse(inputs.full)))
