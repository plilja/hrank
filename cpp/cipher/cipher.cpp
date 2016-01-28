#include <iostream>
#include <vector>

using namespace std;

int toInt(char c) {
    return (int) (c - '0');
}

int main() {
    int N, K;
    scanf("%d %d\n", &N, &K);
    char inp[2000010];
    fgets(inp, 2000010, stdin);
    vector<int> v(N);
    int acc = 0;
    for (int i = 0; i < K; ++i) {
        int t = acc ^ toInt(inp[i]);
        acc ^= t;
        v[i] = t;
    }
    for (int i = K; i < N; ++i) {
        acc ^= v[i - K];
        int t = acc ^ toInt(inp[i]);
        acc ^= t;
        v[i] = t;
    }
    for (int i = 0; i < N; ++i) {
        printf("%d", v[i]);
    }
    puts("");
}

