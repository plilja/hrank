import sys

def ints():
    return map(int, sys.stdin.readline().split())

[t] = ints()

for i in range(0, t):
    [n, m] = ints()
    if m % 2 == 0 or n == 1:
        print(1)
    else:
        print(2)
