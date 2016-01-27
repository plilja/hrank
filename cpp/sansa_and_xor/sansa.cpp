#include <iostream>

using namespace std;

typedef long long ll;

bool odd(ll x) {
    return x % 2 == 1;
}

int main() {
    int t;
    scanf("%d", &t);
    while (t--) {
        ll n;
        scanf("%lld", &n);
        int acc = 0;
        for (ll i = 0; i < n; ++i) {
            int a;
            scanf("%d", &a);
            if (odd((i + 1) * (n - i))) {
                acc ^= a;
            }
        }
        printf("%d\n", acc);
    }
}
