import day04.inputs as inputs

class Board:
    def __init__(self, grid):
        self.grid = grid
    
    def mark(self, draw):
        row = first(self.grid, lambda z: draw in z)
        if row is None:
            #print(f'{draw} is not in any row')
            return False
        col = [c for c in range(5) if self.grid[row][c] == draw][0]
        self.grid[row][col] = None
        #print(f'Found {draw} at {row},{col}, now its:\n{self}')
        if zsum(self.grid[row]) == 0:
            #print(f'Row {row} is complete')
            return True
        if zsum(self.grid[r][col] for r in range(5)) == 0:
            #print(f'Col {col} is complete')
            return True
        return False

    def sum(self):
        return sum(zsum(row) for row in self.grid)

    def __str__(self):
        g = '\n'.join(' '.join(
            f'{r: <4}' if r is not None else '**  ' for r in row)
            for row in self.grid
        )
        return f'{g}\nsum = {self.sum()}'

def first(zs, f):
    inds = [i for i in range(len(zs)) if f(zs[i])]
    # print(inds)
    return inds[0] if inds else None

def zsum(zs):
    return sum(z if z else 0 for z in zs)

# lines = array of 5 strings
def read_board(lines):
    return [
        [ int(c) for c in line.split(' ') if c != '' ]
        for line in lines
    ]

# lines = n * 6, 5 lines + space
def read_boards(lines):
    starts = range(0, len(lines), 6)
    return [ Board(read_board(lines[s:s+5])) for s in starts ]

def read_draws(line):
    return [int(d) for d in line.split(',')]

# b2 = read_boards(inputs.sample[2:])[2]
# print(b0)
# print(b0.grid[0][4] > 0)
# print(first(b2.grid, lambda z: 4 in z)    )

def part1(lines):
    draws = read_draws(lines[0])
    boards = read_boards(lines[2:])
    for draw in draws:
        #print(f'Drew {draw}')
        for board in boards:
            #print(f'Checking {draw} against\n{board}')
            if board.mark(draw):
                return int(draw) * board.sum()
            # input('...')

def part2(lines):
    draws = read_draws(lines[0])
    boards = read_boards(lines[2:])
    for draw in draws:
        # print(f'Drew {draw}')
        if len(boards) == 1:
            if boards[0].mark(draw):
                return boards[0].sum() * draw
        else:
            boards = [b for b in boards if not b.mark(draw)]
        # nlnl = '\n\n'
        # print(f'Remaining boards:\n{nlnl.join(str(b) for b in boards)}')
        # input('...')

print(part1(inputs.full))
print(part2(inputs.full))