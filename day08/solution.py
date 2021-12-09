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

def subsig(s, t):
    return set(s).issubset(set(t))

def output(signals, digits):
    one = next(s for s in signals if len(s) == 2)
    four = next(s for s in signals if len(s) == 4)
    seven = next(s for s in signals if len(s) == 3)
    eight = next(s for s in signals if len(s) == 7)

    # six segments
    sixes = set(s for s in signals if len(s) == 6)
    assert(len(sixes) == 3)
    nine = next(s for s in sixes if subsig(four, s))
    zero = next(s for s in sixes-{nine} if subsig(one, s))
    six = next(s for s in sixes - {zero, nine})

    fives = set(s for s in signals if len(s) == 5)
    assert(len(fives) == 3)
    three = next(s for s in fives if subsig(seven, s))
    five = next(s for s in fives if subsig(s, six))
    two = next(s for s in fives - {three, five})
    
    signal_map = { zero: 0, one: 1, two: 2, three: 3, four: 4, five: 5, six: 6, seven: 7, eight: 8, nine: 9 }
    return addup(signal_map, digits)

def addup(sigmap, digits):
    for d in digits:
        if d not in sigmap:
            raise Exception(f'Missing digit {d} in {sigmap}')
    return sum(sigmap[digits[i]] * 10 ** (3 - i) for i in range(4))

assert output(*parse('acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf')) == 5353
assert part2(parse_all(inputs.sample)) == 61229
assert part2(parse_all(inputs.full)) == 1048410
