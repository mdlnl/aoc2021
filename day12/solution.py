from collections import defaultdict

def parseGraph(lines):
    graph = defaultdict(lambda: set())
    for line in lines:
        endpoints = line.split('-')
        assert len(endpoints) == 2
        graph[endpoints[0]].add(endpoints[1].strip())
        graph[endpoints[1].strip()].add(endpoints[0])
    return graph

def traverse(graph, path=['start'], smalls=set()):
    ''' traverse(graph, prefix): returns all paths that start with prefix and end at 'end' '''
    #print(f'traversing from {path}')
    if path[-1] == 'end':
        return [path]
    completePaths = []
    for outEdge in graph[path[-1]] - smalls:
        completePaths = completePaths + traverse(
            graph,
            path + [outEdge],
            smalls.union({path[-1]}) if path[-1].islower() else smalls)
    return completePaths

def part1(filename):
    f = open(filename, 'r')
    lines = f.readlines()
    f.close()
    return len(traverse(parseGraph(lines)))

assert part1('sample.txt') == 10
assert part1('full.txt') == 4659
