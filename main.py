import inputs;

def increases(measurements):
    nexts = measurements[1:]
    prevs = measurements[0:-1]
    deltas = list(n - p for (n,p) in zip(nexts, prevs))
    return sum(1 if d > 0 else 0 for d in deltas)

print('\nPART ONE')
print(increases(inputs.sample))
print(increases(inputs.full))

def window_increases(measurements, win):
    acc = sum(measurements[0:win-1])
    incs = 0
    for i in range(win, len(measurements)):
        nacc = acc - measurements[i - win] + measurements[i]
        if nacc > acc:
            incs = incs + 1
    return incs

print('\nPART TWO')
print(window_increases(inputs.sample, 3))
print(window_increases(inputs.full, 3))

# a + b + c < b + c + d
# a < d
def window_increases_smart(measurements, win):
    prevs = measurements[0:-win]
    nexts = measurements[win:]
    return sum(1 if n > p else 0 for (n,p) in zip(nexts, prevs))

print('\nSMART')
print(window_increases_smart(inputs.sample, 1))
print(window_increases_smart(inputs.sample, 3))
print(window_increases_smart(inputs.full, 1))
print(window_increases_smart(inputs.full, 3))