#include <iostream>

typedef long long ll;

ll lowest_bit(ll r) {
    if (r == 0) {
        return 1;
    }
    int j = 0;
    while ((r & (1 << j)) == 0) {
        j++;
    }
    return 1LL << j;
}

int main() {
    int t;
    scanf("%d", &t);
    while (t--) {
        ll a, b;
        scanf("%lld %lld", &a, &b);
        ll r = a;
        for (ll i = a + 1; i <= b && r != 0; i = r + lowest_bit(r)) {
            r &= i;
        }
        printf("%lld\n", r);
    }
}
