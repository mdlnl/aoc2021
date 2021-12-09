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

print(part1(parse(inputs.full)))
