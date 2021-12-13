def parse_fold(line):
    [_, _, eq] = line.split(' ')
    [coord, val] = eq.split('=')
    return fold_along_x if coord == 'x' else fold_along_y, int(val)

def parse_point(line):
    [x, y] = line.split(',') 
    return (int(x), int(y))

def fold(u, v):
    return v if v < u else u - (v - u)

def fold_along_x(paper, axis):
    return {(fold(axis, x), y) for (x, y) in paper}

def fold_along_y(paper, axis):
    return {(x, fold(axis, y)) for (x, y) in paper}

def parse_input(filename):
    f = open(filename, 'r')
    lines = f.readlines()
    f.close()
    blank = next(i for i in range(len(lines)) if lines[i] == '\n')
    points = {parse_point(line) for line in lines[0:blank]}
    folds = [parse_fold(line) for line in lines[blank+1:]]
    return points, folds

def print_paper(points):
    m = max(y for (_, y) in points)
    n = max(x for (x, _) in points)
    paper = '     ' + ''.join(str(x%10) for x in range(n+1)) + '\n' + '\n'.join([
        f'{y:4} ' + ''.join([
            '#' if (x, y) in points else '.'
            for x in range(n+1)
        ])
        for y in range(m+1)
    ])
    print(paper)

def part1(filename, debug=False):
    points, folds = parse_input(filename)
    (f, u) = folds[0]
    return len(f(points, u))

def part2(filename, debug=False):
    points, folds = parse_input(filename)
    for (f, u) in folds:
        if debug:
            print(f'{f}({u})')
            print_paper(points)
        points = f(points, u)
        print_paper(points)
    return len(points)

assert part2('sample.txt') == 16
print(part1('full.txt'))
