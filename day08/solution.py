import day08.inputs as inputs

def parse(entry):
    [signal_strs, digit_strs] = entry.split(' | ')
    signals = signal_strs.split(' ')
    digits = digit_strs.split(' ')
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

def part2(inp):
    return None

assert part1(parse_all(inputs.sample)) == 26
print(part1(parse_all(inputs.full)))
