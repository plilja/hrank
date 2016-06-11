#include <iostream>
#include <vector>
#include <map>

using namespace std;

int main()
{
    int n;
    cin >> n;
    vector<int> a(n);
    for (int i = 0; i < n; ++i) {
        cin >> a[i];
    }
    vector<int> b(n);
    for (int i = 0; i < n; ++i) {
        cin >> b[i];
    }

    map<int, int> countA;
    for (auto j : a) {
        countA[j] += 1;
    }
    map<int, int> countB;
    for (auto j : b) {
        countB[j] += 1;
    }
    bool swapUsed = false;
    int ans = 0;
    for (auto it : countA) {
        int t1 = it.second;
        int t2 = countB[it.first];
        if (t1 > t2 && !swapUsed) {
            t2++;
            swapUsed = true;
        }
        ans += min(t1, t2);
    }
    if (!swapUsed) {
        ans -= 1;
    }
    cout << ans << endl;
 }
