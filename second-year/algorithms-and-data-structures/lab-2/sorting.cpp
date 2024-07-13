#include <iostream>
#include <math.h>

long long n;
long long precalculated[1000][1000];
long long seq[1000];
long long current;
static long long path(int start, int end, long long element, bool direction);
static long long c(int start, int end);

using namespace std;

int main() {
  ios_base::sync_with_stdio(0);
  cin.tie(0);
  cin >> n;

  for (int i = 0; i < n; i++) {
    cin >> current;
    seq[i] = current;
  }
  if (n == 1) {
    cout << 1;
  } else {
    cout << c(0, n - 1) % (int)pow(10, 9);
  }
}
static long long c(int start, int end) {
  if (start - end == 1) {
    return start < end ? 2 : 0;
  }
  if (precalculated[start][end] != 0) {
    if (precalculated[start][end] == -1) {
      return 0;
    } else {
      return precalculated[start][end];
    }
  }
  long long s = 0;
  s += path(start + 1, end, seq[start], true);
  s += path(start, end - 1, seq[end], false);
  precalculated[start][end] = s == 0 ? -1 : (s % (int)pow(10, 9));
  return s % (int)pow(10, 9);
}

static long long path(int start, int end, long long element, bool direction) {
  if (start == end) {
    return direction ? (element < seq[start] ? 1 : 0)
                     : (element > seq[start] ? 1 : 0);
  }
  if (direction) {
    if (element < seq[start] && element < seq[end]) {
      return c(start, end);
    }
    if (element < seq[start]) {
      return path(start + 1, end, seq[start], true);
    }
    if (element < seq[end]) {
      return path(start, end - 1, seq[end], false);
    }
    return 0;
  } else {
    if (element > seq[start] && element > seq[end]) {
      return c(start, end);
    }
    if (element > seq[start]) {
      return path(start + 1, end, seq[start], true);
    }
    if (element > seq[end]) {
      return path(start, end - 1, seq[end], false);
    }
    return 0;
  }
}
