import day06.inputs as inputs
from collections import defaultdict

def build_count_by_timer_state_table(timers):
    table = defaultdict(lambda: 0)
    for t in timers:
        table[t] = table[t] + 1
    return table

def iterate(table):
    next_table = defaultdict(lambda: 0)
    next_table[8] = table[0]
    next_table[7] = table[8]
    next_table[6] = table[7] + table[0]
    next_table[5] = table[6]
    next_table[4] = table[5]
    next_table[3] = table[4]
    next_table[2] = table[3]
    next_table[1] = table[2]
    next_table[0] = table[1]
    return next_table

def part1(timers, niters):
    table = build_count_by_timer_state_table(timers)
    print(', '.join([ f'{t}:{table[t]}' for t in range(9) ]))
    for i in range(niters):
        table = iterate(table)
        print(', '.join([ f'{t}:{table[t]}' for t in range(9) ]))
    return sum(table.values())

def display(table):
    ', '.join([ f'{t}:{table[t]}' for t in range(9) ])

print(part1(inputs.full, 256))
# tab = build_count_by_timer_state_table(inputs.sample)
# print(', '.join([ f'{t}:{tab[t]}' for t in range(9) ]))