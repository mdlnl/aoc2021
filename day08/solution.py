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

def signals_to_binary(sigs):
    bin = 0
    if 'a' in sigs:
        bin = bin + 1
    if 'b' in sigs:
        bin = bin + 2
    if 'c' in sigs:
        bin = bin + 4
    if 'd' in sigs:
        bin = bin + 8
    if 'e' in sigs:
        bin = bin + 16
    if 'f' in sigs:
        bin = bin + 32
    if 'g' in sigs:
        bin = bin + 64
    return bin

assert signals_to_binary('abc') == 7

def binary_to_signals(bin):
    sig = ''
    if bin & 1:
        sig = sig + 'a'
    if bin & 2:
        sig = sig + 'b'
    if bin & 4:
        sig = sig + 'c'
    if bin & 8:
        sig = sig + 'd'
    if bin & 16:
        sig = sig + 'e'
    if bin & 32:
        sig = sig + 'f'
    if bin & 64:
        sig = sig + 'g'
    return sig

assert binary_to_signals(127) == 'abcdefg'

def find_symbol_map_by_search(signals):
    signal_map = {
        next(s for s in signals if len(s) == 2): 1,
        next(s for s in signals if len(s) == 4): 4,
        next(s for s in signals if len(s) == 3): 7,
        next(s for s in signals if len(s) == 7): 8,
    }
    return 0

def output(signals, digits):
    sigb = [signals_to_binary(s) for s in signals]
    digb = [signals_to_binary(d) for d in digits]

def part2(inp):
    return sum(output(signals, digits) for (signals, digits) in inp)

print(part2(parse_all(inputs.sample)))
