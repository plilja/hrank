import sys

def ints():
    return map(int, sys.stdin.readline().split())

[t] = ints()

for i in range(0, t):
    [n] = ints()
    a = 0
    for j in ints():
        a ^= j
    if a == 0:
        print('Second')
    else:
        print('First')
