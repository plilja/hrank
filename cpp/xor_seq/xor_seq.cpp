#include <iostream>
#include <algorithm>

using namespace std;

typedef long long ll;

int main() {
    int q;
    cin >> q;
    for (int i = 0; i < q; ++i) {
        ll l, r;
        cin >> l >> r;
        ll res = 0;
        ll acc = 0;
        ll sumS = 4LL * (l / 4LL);

        for (ll j = sumS; j < l; ++j) {
            acc = (acc ^ j);
            res ^= acc;
        }
        acc = 0;
        ll sumE = 4LL * (r / 4LL);
        for (ll j = sumE; j < sumE + 4; ++j) {
            acc = (acc ^ j);
            if (j > r) {
                res ^= acc;
            }
        }
        if (((sumE - sumS) / 4LL) % 2 == 0) {
            res ^= 2;
        }
        cout << res << endl;
    }
}
