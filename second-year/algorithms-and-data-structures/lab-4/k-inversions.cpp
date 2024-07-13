#include <iostream>
#include <math.h>
using namespace std;

long long n{};
long long k{};
long long v[20000];

long long tree[65536];

long long sum(long long l, long long r, long long tree[], int max_level) {
  long long s = 0;
  l += pow(2, max_level) - 1;
  r += pow(2, max_level) - 1;

  while (l <= r) {
    if (l % 2 == 1)
      s += tree[l];
    if (r % 2 == 0)
      s += tree[r];
    l = (l + 1) / 2;
    r = (r - 1) / 2;
  }
  return s;
}

void add(int v, int x, long long tree[], int max_level) {

  v += pow(2, max_level) - 1;
  while (v > 1) {
    tree[v] += x;
    v /= 2;
  }
  tree[1] += x;
}

int main() {
  ios_base::sync_with_stdio(0);
  cin.tie(0);

  cin >> n;
  cin >> k;
  int add_levels = ceil(log(n) / log(2));

  long long prev_vals[n];

  for (int i = 0; i < n; i++) {
    cin >> v[i];
  }

  // k=1 permutations
  for (int i = 0; i < n; i++) {
    prev_vals[i] = 1;
  }

  for (int t = 2; t <= k; t++) {
    for (int i = 0; i <= 3 * n; i++) {
      tree[i] = 0;
    }

    for (int i = 0; i < n; i++) {
      long long num = v[i];
      long long next_val = sum(num, n, tree, add_levels) % 1'000'000'000;
      add(num, prev_vals[num - 1], tree, add_levels);
      prev_vals[num - 1] = next_val;
    }
  }

  long long s = 0;
  for (int i = 0; i < n; i++) {
    s = (s + prev_vals[i]) % 1'000'000'000;
  }

  cout << s;
}
