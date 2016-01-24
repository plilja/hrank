#include <iostream>

using namespace std;

typedef unsigned long long ull;

bool is2Pow(ull x) {
    return (x & (x - 1)) == 0;
}

ull closest2Pow(ull x) {
    int i = 0;
    while (x != 0) {
        x >>= 1;
        ++i;
    }
    ull r = 1;
    return (r << (i - 1));
}

bool solve(ull N) {
    if (N == 1) 
        return true;
    if (is2Pow(N)) {
        return !solve(N/2);
    } else {
        return !solve(N - closest2Pow(N));
    }
}

int main() {
    int T;
    cin >> T;
    for (int i = 0; i < T; ++i) {
        ull N;
        cin >> N;
        if (solve(N)) {
            cout << "Richard" << endl;
        } else {
            cout << "Louise" << endl;
        }
    }
}

