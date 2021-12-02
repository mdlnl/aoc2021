import inputs;

def parse(lines):
    moves = [
        line.split(' ')
        for line in lines
    ]
    f = [int(m[1]) for m in moves if m[0] == 'forward']
    u = [int(m[1]) for m in moves if m[0] == 'up']
    d = [int(m[1]) for m in moves if m[0] == 'down']
    return f, u, d


def slope(lines):
    f, u, d = parse(lines)
    return sum(f) * (sum(d) - sum(u))


def fly(lines):
    x = 0
    depth = 0
    aim = 0
    for line in lines:
        words = line.split(' ')
        #print(f'x={x}, depth={depth}, aim={aim}, words={words}')
        units = int(words[1])
        if words[0] == 'forward':
            x = x + units
            depth = depth + aim * units
        elif words[0] == 'up':
            aim = aim - units
        elif words[0] == 'down':
            aim = aim + units
        else:
            raise 'fuck'
    return depth * x


print(slope(inputs.sample))
print(slope(inputs.full))
print(fly(inputs.sample))
print(fly(inputs.full))
