#include <cmath>
#include <iostream>


using namespace std;

int compare(const void *a, const void *b) {
  return (*(long long *)a - *(long long *)b);
}

long long sum(long long l, long long r, long long *tree, long long max_level) {

  l++;
  r++;

  long long s = 0;
  l += pow(2, max_level) - 1;
  r += pow(2, max_level) - 1;

  while (l <= r) {
    if (l % 2 == 1)
      s = (s + tree[l]) % 1000000007;
    if (r % 2 == 0)
      s = (s + tree[r]) % 1000000007;
    l = (l + 1) / 2;
    r = (r - 1) / 2;
  }
  return s;
}

void add(long long v, long long x, long long *tree, long long max_level) {
  v++;

  v += pow(2, max_level) - 1;
  while (v > 1) {
    tree[v] = (tree[v] + x) % 1000000007;
    v /= 2;
  }
  tree[1] = (tree[1] + x) % 1000000007;
}

long long find_index(long long x, long long l, long long r, bool direction,
                     long long *a) {
  long long orig_l = l;
  long long orig_r = r;
  long long s;
  while (l < r) {
    s = (l + r) / 2;
    if (a[s] < x) {
      l = s + 1;
    } else {
      r = s;
    }
  }

  if (a[l] > x && direction) {
    if (l > orig_l) {
      return l - 1;
    } else {
      return -1;
    }
  }
  if (a[l] < x && !direction) {
    if (l < orig_r) {
      return l + 1;
    } else {
      return 10'000'000;
    }
  }
  return l;
}

long long n;
long long k;
long long interval;
long long r[500'000];

long long r_temp[500'500];

long long pointers[500'000][2];

long long tree[1'048'577];

long long c[500'000];

int main() {
  ios_base::sync_with_stdio(0);
  cin.tie(0);

  cin >> n;
  cin >> k;        // max represent.
  cin >> interval; // min interval in committee
  for (long long i = 0; i < n; ++i) {
    cin >> r_temp[i];
  }
  for (long long i = 0; i < 1'048'577; i++) {
    tree[i] = 0;
  }

  qsort(r_temp, n, sizeof(long long), compare);

  r[0] = r_temp[0];
  c[0] = 1;
  long long current_index = 0;
  for (int i = 1; i < n; i++) {
    if (r_temp[i] == r_temp[i - 1]) {
      c[current_index]++;

    } else {
      current_index++;
      r[current_index] = r_temp[i];
      c[current_index]++;
    }
  }
  n = current_index + 1;
  long long min_size = 0;
  long long min_to_represent = r[0];
  long long prev_representative = 0;
  long long current_element = 0;

  while (current_element < n) {
    min_size++;
    min_to_represent = r[current_element];
    while (current_element < n - 1 &&
           r[current_element + 1] - min_to_represent <= k) {
      current_element++;
    }

    pointers[min_size - 1][1] = current_element;
    prev_representative = r[current_element];

    while (current_element < n &&
           r[current_element] <= prev_representative + k) {
      current_element++;
    }
  }

  current_element = n - 1;
  long long current_pointer = min_size;

  while (current_element >= 0) {
    current_pointer--;

    long long max_to_represent = r[current_element];
    while (current_element >= 1 &&
           max_to_represent - r[current_element - 1] <= k) {
      current_element--;
    }

    pointers[current_pointer][0] = current_element;
    prev_representative = r[current_element];

    while (current_element >= 0 &&
           r[current_element] >= prev_representative - k) {
      current_element--;
    }
  }

  long long levels = ceil(log(n) / log(2));

  for (long long i = pointers[min_size - 1][0]; i <= pointers[min_size - 1][1];
       i++) {
    add(i, c[i], tree, levels);
  }

  for (long long l = min_size - 2; l >= 0; l--) {
    for (long long pos = pointers[l][0]; pos <= pointers[l][1]; pos++) {
      // now i need to find positions of the sum

      long long min_possible;

      if (r[pointers[l + 1][0]] >= r[pos] + interval) {
        min_possible = pointers[l + 1][0];
      } else {
        min_possible =
            find_index(r[pos] + interval, pointers[l + 1][0], n - 1, false, r);
      }

      // the one that includes all of the elements after r[pos] + k

      long long first_to_take_index =
          find_index(r[pos] + k + 1, pos, n - 1, false, r);

      long long max_possible;

      max_possible = find_index(r[first_to_take_index] + k, pointers[l + 1][0],
                                pointers[l + 1][1], true, r);
      if (min_possible < pointers[l + 1][0])
        min_possible = pointers[l + 1][0];
      if (max_possible > pointers[l + 1][1])
        max_possible = pointers[l + 1][1];
      long long value = c[pos] * sum(min_possible, max_possible, tree, levels) %
                        1'000'000'007;

      add(pos, value, tree, levels);
    }
  }

  cout << min_size << " " << sum(pointers[0][0], pointers[0][1], tree, levels);
}
