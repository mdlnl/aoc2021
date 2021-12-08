import day08.inputs as inputs

def strsort(s):
    x = list(s)
    x.sort()
    return ''.join(x)

def parse(entry):
    [signal_strs, digit_strs] = entry.split(' | ')
    signals = [strsort(s) for s in signal_strs.split(' ')]
    digits = [strsort(d) for d in digit_strs.split(' ')]
    return (signals, digits)

def parse_all(entries):
    return [ parse(entry) for entry in entries ]

def count1478(digits):
    return len([
        digit
        for digit in digits
        if len(digit) in [2, 4, 3, 7]
    ])

# Just count the number of times a 1, 4, 7, or 8 appears among the entries
# 1 = 2 segments
# 4 = 4 segments
# 7 = 3 segments
# 8 = 7 segments
def part1(inp):
    digitses = [digits for (_, digits) in inp]
    return sum(count1478(digits) for digits in digitses)

assert part1(parse_all(inputs.sample)) == 26
assert part1(parse_all(inputs.full)) == 344

def part2(inp):
    return sum(output(signals, digits) for (signals, digits) in inp)

def only_unmapped(signal_map, signals):
    return [s for s in signals if s not in signal_map]

def search(sigmap, unknowns):
    if not unknowns:
        return sigmap
    n = len(unknowns[0])
    assert n in [5, 6]
    assert all(len(u) == n for u in unknowns)
    g = None

def subsig(s, t):
    return set(s).issubset(set(t))

def output(signals, digits):
    one = next(s for s in signals if len(s) == 2): 1
    two = None
    three = None
    four = next(s for s in signals if len(s) == 4): 4
    five = None
    six = None
    seven = next(s for s in signals if len(s) == 3): 7
    eight = next(s for s in signals if len(s) == 7): 8
    nine = None
    six_sigs = [s for s in signals if len(s) == 6]
    assert len(six_sigs) == 2
    if subsig(one, six_sigs[0])):
        six = six_sigs[1]
        nine = six_sigs[0]
    else:
        six = six_sigs[0]
        nine = six_sigs[1]
    five_sigs = [s for s in signals if len(s) == 5]
    assert(len(five_sigs) == 3)
    
    signal_map = { one: 1, four: 4, six: 6, seven: 7, eight: 8 }
    print(signal_map)
    return sum(signal_map[digits[i]] * 10 ** (3 - i) for i in range(4))

print(output(*parse(inputs.sample[0])))
# print(part2(parse_all(inputs.sample)))
