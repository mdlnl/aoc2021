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

def part1(filename):
    points, folds = parse_input(filename)
    print(points)
    for (f, u) in folds:
        points = f(points, u)
        print(f'{f}({u})\n{points}')
    return len(points)

print(part1('sample.txt'))
