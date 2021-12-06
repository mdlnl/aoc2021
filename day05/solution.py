import day05.inputs as inputs
import re

def parse_line(text_line):
    m = re.search('([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)', text_line)
    return (
        (int(m.group(1)), int(m.group(2))),
        (int(m.group(3)), int(m.group(4)))
    )

def parse_lines(text_lines):
    return [ parse_line(tl) for tl in text_lines ]

#print('\n'.join(str(l) for l in parse_lines(inputs.sample)))

def part1(lines):
    marks = {}
    at_least_two = set()
    for line in lines:
        lx = line[1][0] - line[0][0]
        ly = line[1][1] - line[0][1]
        if lx * ly != 0:
            # print(f'Skipping non-orthogonal line {line}')
            continue
        n = abs(lx) + abs(ly)
        dx = int(lx/n)
        dy = int(ly/n)
        # print(f'Traversing {line} with slope {dy}/{dx} and length {n}')
        for i in range(n+1):
            x = line[0][0] + i * dx
            y = line[0][1] + i * dy
            p = (x, y)
            marks[p] = marks[p] + 1 if p in marks else 1
            # print(f'Marking {p}')
            if marks[p] > 1:
                # print(f'{p} was hit twice')
                at_least_two.add(p)
    return len(at_least_two)

# print(part1(parse_lines(inputs.full)))

def part2(lines):
    marks = {}
    at_least_two = set()
    for line in lines:
        lx = line[1][0] - line[0][0]
        ly = line[1][1] - line[0][1]
        n = max(abs(lx), abs(ly))
        dx = int(lx/n)
        dy = int(ly/n)
        # print(f'Traversing {line} with slope {dy}/{dx} and length {n}')
        for i in range(n+1):
            x = line[0][0] + i * dx
            y = line[0][1] + i * dy
            p = (x, y)
            marks[p] = marks[p] + 1 if p in marks else 1
            # print(f'Marking {p}')
            if marks[p] > 1:
                # print(f'{p} was hit twice')
                at_least_two.add(p)
    return len(at_least_two)

print(part2(parse_lines(inputs.full)))