import inputs;

def parse(lines):
    return [list(line) for line in lines]

def count_bit_frequency(strs, pos):
    n0 = sum([s[pos] == '0' for s in strs])
    n1 = len(strs) - n0
    return n0, n1

# returns the most and least common bits in the given position
def count_commonest_bits(strs, pos):
    n0, n1 = count_bit_frequency(strs, pos)
    return (
        0 if n0 > n1 else 1,
        0 if n0 < n1 else 1
    )

# list  ->  binary number
def binlist(bits):
    return int(''.join(str(bit) for bit in bits), 2)

def gamma(strs):
    return binlist([
        count_commonest_bits(strs, pos)[0]
        for pos in range(0, len(strs[0]))
    ])

def epsilon(strs):
    return binlist([
        count_commonest_bits(strs, pos)[1]
        for pos in range(0, len(strs[0]))
    ])

def part1(strs):
    g = gamma(strs)
    e = epsilon(strs)
    print(f'gamma = {g}, epsilon = {e}, product = {g*e}')

# part1(inputs.sample)
# part1(inputs.full)

def oxygen_criterion_bit(strs, pos):
    n0, n1 = count_bit_frequency(strs, pos)
    #print(n0, n1)
    return 0 if n0 > n1 else 1 if n1 > n0 else 1

def co2_criterion_bit(strs, pos):
    n0, n1 = count_bit_frequency(strs, pos)
    return 0 if n0 < n1 else 1 if n1 < n0 else 0

def rating(strs, critf):
    dc = strs.copy()
    p = 0
    #print(dc)
    while len(dc) > 1:
        cbit = critf(dc, p)
        #print(f'cbit = {cbit}, pos = {p}')
        #print([s[p] for s in dc])
        dc = [s for s in dc if int(s[p]) == cbit]
        #print(dc)
        p = p + 1
    return binlist(dc[0])

def part2(strs):
    oxy = rating(strs, oxygen_criterion_bit)
    co2 = rating(strs, co2_criterion_bit)
    print(f'oxygen = {oxy}, co2 = {co2}, product = {oxy * co2}')

#part2(inputs.sample)
part2(inputs.full)