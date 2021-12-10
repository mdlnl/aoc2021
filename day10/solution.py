import inputs

lefts = '([{<'
rights = { ')':'(', ']':'[', '}':'{', '>':'<' }

def parse(str):
    stack = []
    for i in range(len(str)):
        if str[i] in lefts:
            stack.append(str[i])
        elif str[i] in rights:
            if stack[-1] == rights[str[i]]:
                stack.pop()
            else:
                return i
    return None

errorScores = { ')': 3, ']': 57, '}': 1197, '>': 25137 }

def errorScore(line):
    p = parse(line)
    return errorScores[line[p]] if p else 0

def part1(lines):
    return sum(errorScore(line) for line in lines)

def autocompleteScore(line):
    p = parse(line)

assert part1(inputs.sample) == 26397
assert part1(inputs.full) == 367227
