def parse(filename):
    f = open(filename, 'r')
    lines = f.readlines()
    f.close()
    return [ [int(c) for c in line if c.isdigit()] for line in lines ]

# Assume can only go down or right
def shortest_path_dr(risk, rows, cols):
    best = [[0]] # no cost to get to top left square since we start there
    while len(best) < rows or len(best[-1]) < cols:
        # expand frontier to the right
        f = len(best[0])
        for i in range(len(best)): # exclude lower-right corner
            fromLeft  = best[i    ][-1] + risk(i, f)
            fromAbove = best[i - 1][ f] + risk(i, f) if i > 0 else fromLeft + 1
            best[i].append(min(fromAbove, fromLeft))
        # expand frontier down
        frontier = []
        for j in range(f+1): # include lower-right corner
            fromAbove = best[-1][ j] + risk(f, j)
            fromLeft  = frontier[-1] + risk(f, j) if j > 0 else fromAbove + 1
            frontier.append(min(fromAbove, fromLeft))
        best.append(frontier)
    return best

def part1(filename):
    grid = parse(filename)
    g = lambda i, j: grid[i][j]
    return shortest_path_dr(g, len(grid), len(grid))

def part2(filename, reps=5):
    grid = parse(filename)
    n = len(grid)
    g = lambda i, j: ((grid[i % n][j % n] - 1 + (i // n) + (j // n)) % 9) + 1
    #print('\n'.join(''.join(str(g(i, j)) for j in range(5*n)) for i in range(5*n)))
    return shortest_path_dr(g, n * reps, n * reps)

def part2copy(filename):
    grid = parse(filename)
    n = len(grid)

assert part1('sample.txt')[-1][-1] == 40
assert part1('full.txt')[-1][-1] == 824

assert part2('sample.txt')[-1][-1] == 315
#print(part2('full.txt')[-1][-1])
print(part1('virtual.txt')[-1][-1])


# ___f?????
# ___f?????
# ___f?????
# ffff?????
# ?????????
# ...

# 000009999
# 999909999
# 990009999
# 990999999
# 990000000
# 999999990
# 999999990
# 999999990
# 999999990
# 999999990
