def parse_fold(line):
    [_, _, eq] = line.split(' ')
    [coord, val] = eq.split('=')
    return fold_along_x if coord == 'x' else fold_along_y, int(val)

def parse_point(line):
    [x, y] = line.split(',') 
    return (int(x), int(y))

def fold(u, v):
    return v if v < u else u - (v - u)

def fold_along_x(paper, u):
    return {(fold(u, x), y) for (x, y) in paper}

def fold_along_y(paper, u):
    return {(x, fold(u, y)) for (x, y) in paper}

def parse_input(filename):
    f = open(filename, 'r')
    lines = f.readlines()
    f.close()
    blank = next(i for i in range(len(lines)) if lines[i] == '\n')
    points = {parse_point(line) for line in lines[0:blank]}
    folds = [parse_fold(line) for line in lines[blank+1:]]
    return points, folds

def print_grid(points):
    m = max(y for (_, y) in points)
    n = max(x for (x, _) in points)
    grid = '     ' + ''.join(str(x%10) for x in range(n)) + '\n' + '\n'.join([
        f'{y:4} ' + ''.join([
            '#' if (x, y) in points else '.'
            for x in range(n)
        ])
        for y in range(m)
    ])
    print(grid)

def part1(filename):
    points, folds = parse_input(filename)
    print_grid(points)
    for (f, u) in folds:
        points = f(points, u)
        print(f'{f}({u})')
        print_grid(points)
    return len(points)

assert part1('sample.txt') == 16
#print(part1('full.txt'))
