import inputs

lefts = { '(':')', '[':']', '{':'}', '<':'>' }
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
                return i, stack
    return None, stack

errorScores = { ')': 3, ']': 57, '}': 1197, '>': 25137 }

def errorScore(line):
    p, _ = parse(line)
    return errorScores[line[p]] if p else 0

def part1(lines):
    return sum(errorScore(line) for line in lines)

autocompleteScores = { ')': 1, ']': 2, '}': 3, '>': 4 }

def autocomplete(line):
    p, stack = parse(line)
    if not p:
        a = []
        score = 0
        #print(stack)
        while stack:
            r = lefts[stack.pop()]
            score = score * 5 + autocompleteScores[r]
            a.append(r)
        return a, score

assert part1(inputs.sample) == 26397
assert part1(inputs.full) == 367227

_, a = autocomplete('[({(<(())[]>[[{[]{<()<>>')
assert a == 288957
