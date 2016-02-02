#include <iostream>
#include <vector>
#include <queue>
#include <map>
#include <algorithm>

using namespace std;

typedef priority_queue<int, vector<int>, greater<int>> PQ;

int solve(vector<int> &v, int n) {
    sort(v.begin(), v.end());
    map<int, PQ> teams;
    for (auto p : v) {
        auto &pq = teams[p - 1];
        if (pq.empty()) {
            teams[p].push(1);
        } else {
            int t = pq.top();
            pq.pop();
            teams[p].push(t + 1);
        }
    }
    int r = n;
    for (auto &it : teams) {
        if (!it.second.empty()) {
            r = min(r, it.second.top());
        }
    }
    return r;
}

int main() {
    int t;
    scanf("%d", &t);
    while (t--) {
        int n;
        scanf("%d", &n);
        vector<int> v(n);
        for (int i = 0; i < n; ++i) {
            scanf("%d", &v[i]);
        }
        int r = solve(v, n);
        printf("%d\n", r);
    }
}
