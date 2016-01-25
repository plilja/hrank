#include <cassert>
#include <vector>
#include <iostream>
#include <algorithm>

using namespace std;

pair<int, int> find(vector<vector<char>> &v, char c) {
    for (int y = 0; y < v.size(); ++y) {
        for (int x = 0; x < v[y].size(); ++x) {
            if (v[y][x] == c) {
                return {x, y};
            }
        }
    }
    assert(false);
}

bool isValid(vector<vector<char>> &g, int n, int m, int x, int y) {
    return !(x < 0 || x >= m || y < 0 || y >= n || g[y][x] == 'X');
}

int search(vector<vector<char>> &g, int n, int m, pair<int, int> from, pair<int, int> to, vector<vector<bool>> &visited, int wand) {
    if (from == to) {
        return wand;
    }
    int x = from.first;
    int y = from.second;
    visited[y][x] = true;
    vector<pair<int, int>> neighbours;
    if (isValid(g, n, m, x + 1, y) && !visited[y][x + 1]) {
        neighbours.push_back({x + 1, y});
    }
    if (isValid(g, n, m, x - 1, y) && !visited[y][x - 1]) {
        neighbours.push_back({x - 1, y});
    }
    if (isValid(g, n, m, x, y + 1) && !visited[y + 1][x]) {
        neighbours.push_back({x, y + 1});
    }
    if (isValid(g, n, m, x, y - 1) && !visited[y - 1][x]) {
        neighbours.push_back({x, y - 1});
    }
    if (neighbours.size() > 1) {
        wand += 1;
    }
    int r = 0;
    for (auto p : neighbours) {
        r = max(r, search(g, n, m, p, to, visited, wand));
    }
    return r;
}

bool impressed(vector<vector<char>> &g, int n, int m, int k) {
    auto start = find(g, 'M');
    auto exit = find(g, '*');
    vector<vector<bool>> visited(n, vector<bool>(m, false));
    return k == search(g, n, m, start, exit, visited, 0);
}

int main() {
    int T;
    scanf("%d\n", &T);
    while (T--) {
        int N, M;
        scanf("%d %d\n", &N, &M);
        vector<vector<char>> grid(N, vector<char>(M));
        for (int y = 0; y < N; ++y) {
            char line[M + 2];
            scanf("%s", line);
            for (int x = 0; x < M; ++x) {
                grid[y][x] = line[x];
            }
        }
        int K;
        scanf("%d\n", &K);
        if (impressed(grid, N, M, K)) {
            puts("Impressed");
        } else {
            puts("Oops!");
        }
    }
}

