from collections import defaultdict

def parseGraph(lines):
    graph = defaultdict(lambda: set())
    for line in lines:
        endpoints = line.split('-')
        assert len(endpoints) == 2
        graph[endpoints[0]].add(endpoints[1].strip())
        graph[endpoints[1].strip()].add(endpoints[0])
    return graph

def traverseNoSecondSmall(graph, path=['start'], smalls=set(), debug=False):
    ''' traverse(graph, prefix): returns all paths that start with prefix and end at 'end' '''
    if debug:
        print(f'traversing from {",".join(path)}')
    if path[-1] == 'end':
        return [path]
    completePaths = []
    for outEdge in graph[path[-1]] - {'start'}:
        if outEdge in smalls:
            continue
        newSmalls = smalls.union({outEdge}) if outEdge.islower() else smalls
        completePaths = completePaths + traverseNoSecondSmall(
            graph, path + [outEdge], newSmalls, debug)
    return completePaths

def part1(filename):
    f = open(filename, 'r')
    lines = f.readlines()
    f.close()
    return len(traverseNoSecondSmall(parseGraph(lines)))

assert part1('sample.txt') == 10
assert part1('full.txt') == 4659

def traverseOneSecondSmall(graph, path=['start'], smalls=set(), secondSmall=None, debug=False):
    ''' traverse(graph, prefix): returns all paths that start with prefix and end at 'end' '''
    if debug:
        print(f'{",".join(path)}. Can reach {" ".join(graph[path[-1]])}')
    if path[-1] == 'end':
        return [path]
    completePaths = []
    for outEdge in graph[path[-1]] - {'start'}:
        newSecondSmall = secondSmall
        if outEdge in smalls:
            if secondSmall:
                if debug:
                    print(f'skipping {outEdge}, already double-visited {secondSmall}')
                continue
            newSecondSmall = outEdge
            if debug:
                print(f'allowed double-visit of {outEdge}')
        newSmalls = smalls.union({outEdge}) if outEdge.islower() else smalls
        completePaths = completePaths + traverseOneSecondSmall(
            graph, path + [outEdge], newSmalls, newSecondSmall, debug)
    return completePaths

def part2(filename):
    f = open(filename, 'r')
    lines = f.readlines()
    f.close()
    return len(traverseOneSecondSmall(parseGraph(lines)))

assert part2('sample.txt') == 36
assert part2('full.txt') == 148962
