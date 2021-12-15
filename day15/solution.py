def parse(filename):
    f = open(filename, 'r')
    lines = f.readlines()
    f.close()
    return [ [int(c) for c in line if c.isdigit()] for line in lines ]

# Assume can only go down or right
def shortest_path_dr(risk):
    rows, cols = len(risk), len(risk[0])
    assert rows == cols
    best = [[0]] # no cost to get to top left square since we start there
    while len(best) < rows or len(best[-1]) < cols:
        # expand frontier to the right
        f = len(best[0])
        for i in range(len(best)): # exclude lower-right corner
            fromLeft  = best[i    ][-1] + risk[i][f]
            fromAbove = best[i - 1][ f] + risk[i][f] if i > 0 else fromLeft + 1
            best[i].append(min(fromAbove, fromLeft))
        # expand frontier down
        frontier = []
        for j in range(f+1): # include lower-right corner
            fromAbove = best[-1][ j] + risk[f][j]
            fromLeft  = frontier[-1] + risk[f][j] if j > 0 else fromAbove + 1
            frontier.append(min(fromAbove, fromLeft))
        best.append(frontier)
    return best

def show_grid(grid):
    return '\n'.join(' '.join(f'{c if c else " ":2}') for row in grid for c in row)

assert shortest_path_dr(parse('sample.txt'))[-1][-1] == 40
print(shortest_path_dr(parse('full.txt'))[-1][-1])

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
