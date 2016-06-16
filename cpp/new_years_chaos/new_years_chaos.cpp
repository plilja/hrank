#include <vector>
#include <iostream>
#include <queue>

using namespace std;

int main() {
    int t;
    cin >> t;
    while (t--) {
        int n;
        cin >> n;
        vector<int> q(n);
        for (int i = 0; i < n; ++i) {
            cin >> q[i];
        }
        int ans = 0;
        bool chaos = false;
        for (int i = 0; i < n; ++i) {
            if (q[i] - i - 1 > 2) {
                chaos = true;
            }
        }
        for (int j = 0; j < n && !chaos; ++j) {
            bool swapsPerformed = false;
            for (int i = 1; i < n; ++i) {
                if (q[i - 1] > q[i]) {
                    ans++;
                    swapsPerformed = true;
                    swap(q[i - 1], q[i]);
               }
            }
            if (!swapsPerformed) {
                break;
            }
        }
        if (chaos) {
            cout << "Too chaotic" << endl;
        } else {
            cout << ans << endl;
        }
    }
}
