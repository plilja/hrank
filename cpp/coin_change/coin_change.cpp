#include <vector>
#include <iostream>

using namespace std;

typedef long long ll;

int main() {
    int N, M;
    scanf("%d %d", &N, &M);
    vector<int> coints(M);
    for (int i = 0; i < M; ++i) {
        scanf("%d", &coints[i]);
    }
    vector<vector<ll>> sol(M + 1, vector<ll>(N + 1, 0));
    sol[0][0] = 1;
    for (int i = 1; i <= M; ++i) {
        int coin = coints[i - 1];
        for (int j = 0; j < coin; ++j) {
            sol[i][j] = sol[i - 1][j];
        }
        for (int j = coin; j <= N; ++j) {
            sol[i][j] = sol[i - 1][j] + sol[i][j - coin];
        }
    }
    printf("%lld\n", sol[M][N]);
}
