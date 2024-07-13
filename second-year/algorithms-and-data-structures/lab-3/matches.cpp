#include <array>
#include <iostream>
#include <set>
#include <string>


using namespace std;
int n;
int k;

int main() {
  ios_base::sync_with_stdio(0);
  cin.tie(0);

  cin >> n;
  cin >> k;
  string binary_str[n];

  int current_num;

  for (int i = 0; i < k; i++) {
    for (int j = 0; j < n; j++) {
      cin >> current_num;
      if (j < (n + 1) / 2) {
        binary_str[current_num - 1] += "0";
      } else {
        binary_str[current_num - 1] += "1";
      }
    }
  }

  set<string> check;

  for (int i = 0; i < n; i++) {
    check.insert(binary_str[i]);
  }

  if (check.size() == n) {
    cout << "TAK";
  } else {
    cout << "NIE";
  }
  return 0;
}
