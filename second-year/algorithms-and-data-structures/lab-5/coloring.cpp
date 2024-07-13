#include <iostream>
#include <math.h>

using namespace std;

int sum = 0;
int find_sum(int start, int level, int max_level, int *tree) {
  int range = pow(2, max_level - level);
  if (tree[start] != -1) {
    return tree[start] * range;
  } else {
    return find_sum(start * 2, level + 1, max_level, tree) +
           find_sum(start * 2 + 1, level + 1, max_level, tree);
  }
}

void insert(int l, int r, int c, int start, int level, int max_level, bool is_o,
            int *tree) {

  if (level == max_level) {
    if (is_o) {
      sum -= tree[start];
    }
    tree[start] = c;
    sum += c;
  }

  int range = pow(2, max_level - level);
  int l_cover = start * range;
  int r_cover = l_cover + range - 1;
  int first_right = l_cover + range / 2;

  if (l <= l_cover && r >= r_cover) {
    if (is_o) {
      sum -= find_sum(start, level, max_level, tree);
      is_o = false;
    }
    tree[start] = c;
    sum += c * range;

  } else {
    int temp = tree[start];
    if (temp != -1) {
      tree[2 * start] = temp;
      tree[2 * start + 1] = temp;
    }
    if (l < first_right) {
      insert(l, r, c, start * 2, level + 1, max_level, is_o, tree);
    }
    if (r >= first_right) {
      insert(l, r, c, start * 2 + 1, level + 1, max_level, is_o, tree);
    }

    if (tree[start * 2 + 1] == tree[start * 2] && tree[start * 2] != -1) {
      tree[start] = tree[start * 2];
    } else {
      tree[start] = -1;
    }
  }
}

int tree[2097153];
int n;
int k;

int l;
int r;
int c;
char col;

int main() {
  ios_base::sync_with_stdio(0);
  cin.tie(0);
  std::cin >> n;
  std::cin >> k;
  int max_level = ceil(log(n) / log(2));
  for (int i = 0; i < k; i++) {
    std::cin >> l;
    std::cin >> r;
    std::cin >> col;
    c = (col == 'B');
    l += pow(2, max_level) - 1;
    r += pow(2, max_level) - 1;

    insert(l, r, c, 1, 0, max_level, true, tree);
    std::cout << sum << std::endl;
  }
  return 0;
}
