#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
#include <queue>

using namespace std;

typedef pair<int, int> pii;

vector<int> dijkstra(int s, int n, vector<vector<pii>> adj_list) {
    priority_queue<pii, vector<pii>, std::greater<pii>> pq;
    vector<bool> visited(n, false);
    vector<int> dist(n, -1);
    dist[s] = 0;
    pq.push({0, s});

    while (!pq.empty()) {
        auto p = pq.top();
        pq.pop();

        int d = p.first;
        int to = p.second;

        if (visited[to]) {
            continue;
        }
        visited[to] = true;
        dist[to] = d;
        for (auto neighbour : adj_list[to]) {
            if (visited[neighbour.first]) {
                continue;
            }
            pq.push({d + neighbour.second, neighbour.first});
        }
    }
    return dist;
}

int main() {
    int T;
    cin >> T;
    while (T--) {
        int N, M;
        cin >> N >> M;
        vector<vector<pii>> G(N);
        for (int i = 0; i < M; ++i) {
            int a, b, w;
            cin >> a >> b >> w;
            a--;
            b--;
            G[a].push_back({b, w});
            G[b].push_back({a, w});
        }
        int S;
        cin >> S;
        S--;
        auto dist = dijkstra(S, N, G);
        for (int i = 0; i < N; ++i) {
            if (i != S) {
                cout << dist[i] << " ";
            }
        }
        cout << endl;
    }
}
