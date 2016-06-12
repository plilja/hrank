import sys

def ints():
    return map(int, sys.stdin.readline().split())


def solve(point, cache = {}):
    if point in cache:
        return cache[point]
    (x, y) = point
    moves = [(x - 2, y + 1), (x - 2, y - 1), (x + 1, y - 2), (x - 1, y - 2)]
    valid_moves = []
    for (x_, y_) in moves:
        if x_ >= 1 and x_ <= 15 and y_ >= 1 and y_ <= 15:
            valid_moves += [(x_, y_)]
    ans = False
    for move in valid_moves:
        ans = ans or not solve(move, cache)
    cache[point] = ans
    return ans

[t] = ints()

for i in range(0, t):
    [x, y] = ints()
    if solve((x, y)):
        print('First')
    else:
        print('Second')
