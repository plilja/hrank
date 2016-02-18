#include <vector>
#include <iostream>
#include <bitset>

using namespace std;

int main(){
    int n;
    int m;
    cin >> n >> m;
    vector<vector<bitset<64>>> people(n, vector<bitset<64>>((n + 64 - 1)/64, bitset<64>(0)));
    for(int i = 0; i< n; i++){
       string p;
       cin >> p;
       for (int j = 0; j < m; ++j) {
           if (p[j] == '1')
               people[i][j / 64][j % 64] = 1;
       }
    }
    int best = -1;
    int count = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            int topics = 0;
            for (int k = 0; k < people[i].size(); ++ k) {
                auto b1 = people[i][k];
                auto b2 = people[j][k];
                bitset<64> b3(b1.to_ullong() | b2.to_ullong());
                topics += b3.count();
            }
            if (topics > best) {
                best = topics;
                count = 1;
            } else if (topics == best) {
                count++;
            } 
        }
    }
    cout << best << endl;
    cout << count << endl;
}
