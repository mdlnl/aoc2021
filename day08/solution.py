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

def output(signals, digits):
    signal_map = {
        next(s for s in signals if len(s) == 2): 1,
        next(s for s in signals if len(s) == 4): 4,
        next(s for s in signals if len(s) == 3): 7,
        next(s for s in signals if len(s) == 7): 8,
    }
    signals = only_unmapped(signal_map, signals)
    print(signals)
    return sum(signal_map[digits[i]] * 10 ** (3 - i) for i in range(4))

print(output(*parse(inputs.sample[0])))
# print(part2(parse_all(inputs.sample)))
