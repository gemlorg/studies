#include <iostream>
#include <math.h>

using interval_node = std::pair<bool, uint64_t>; // 1. all agree 2. on what
                                                 // value
using fs_node = std::pair<uint64_t, uint64_t>;

uint64_t preord(long long x, fs_node *parent, uint64_t *find, uint64_t *sons,
                uint64_t *color, interval_node *tree, uint64_t index,
                uint64_t index_arr[]) {
  if (sons[x] > 0) {
    for (long long i = find[x]; i < find[x] + sons[x]; i++) {
      tree[index] = interval_node(true, color[parent[i].second]);
      index_arr[parent[i].second] = index;
      index++;
      index = preord(parent[i].second, parent, find, sons, color, tree, index,
                     index_arr);
    }
  }
  return index;
}
uint64_t fill_sub(long long x, fs_node parent[], uint64_t find[],
                  uint64_t sons[], interval_node *tree, uint64_t subtree[]) {
  uint64_t len = 0;
  for (long long i = find[x]; i < find[x] + sons[x]; i++) {
    len += fill_sub(parent[i].second, parent, find, sons, tree, subtree);
  }
  subtree[x] = len;
  return len + 1;
}

void write(fs_node parent[], uint64_t find[], uint64_t sons[], uint64_t color[],
           interval_node tree[], uint64_t n, uint64_t subtree[],
           uint64_t index_arr[]) {
  uint64_t index = pow(2, ceil(log(n) / log(2)));
  tree[index] = interval_node(true, color[1]);
  index_arr[1] = index;
  index++;
  preord(1, parent, find, sons, color, tree, index, index_arr);
  subtree[1] = n - 1;
  fill_sub(1, parent, find, sons, tree, subtree);

  // fill with res -- go by level and fill with values
  long long ind = pow(2, ceil(log(n) / log(2))) - 1;
  for (long long lvl = ceil(log(n) / log(2)) - 1; lvl >= 0; lvl--) {
    for (long long i = ind; i > ind - pow(2, lvl); i--) {
      // maybe we should manage the corner things.
      if (tree[i * 2].first == true == tree[i * 2].first &&
          tree[i * 2].second == tree[i * 2 + 1].second != 0) {
        tree[i] = interval_node(true, tree[i * 2].second);
      } else {
        tree[i] = interval_node(false, 0);
      }
    }
    ind -= pow(2, lvl);
  }
}

void print_tree(interval_node tree[], uint64_t n) {
  long long ind = 1;
  for (long long lvl = 0; lvl <= ceil(log(n) / log(2)); lvl++) {
    for (long long i = ind; i < ind + pow(2, lvl); i++) {
      std::cout << tree[i].second << " ";
    }
    ind += pow(2, lvl);

    std::cout << std::endl;
  }
}
uint64_t p(uint64_t i) { return i % 2 == 0 ? i + 1 : i - 1; }
// tree operations
void replace(uint64_t i, uint64_t v, interval_node tree[], uint64_t find[]) {
  i = find[i];
  if (tree[i].second == v) {
    return;
  }
  tree[i].second = v;
  while (i != 1) {
    if (tree[i].second == tree[p(i)].second && tree[i] == tree[p(i)] == true) {
      tree[i / 2] = interval_node(true, tree[i].second);
    } else {
      tree[i / 2] = interval_node(false, 0);
    }
    i /= 2;
  }
}

uint64_t investigate(uint64_t ind, uint64_t val, interval_node tree[],
                     uint64_t n) {
  uint64_t last_level_i = pow(2, ceil(log(n) / log(2)));
  if (ind >= last_level_i) {
    if (tree[ind].second == val)
      return 0;
    return 1;
  }
  if (tree[ind].first == true) {
    if (tree[ind].second == val)
      return 0;
    return 2;
  }
  uint64_t l = 2 * ind;
  uint64_t r = 2 * ind + 1;
  if ((tree[l].first == true && tree[l].second == val)) {
    return investigate(r, val, tree, n);
  }
  if ((tree[r].first == true && tree[r].second == val)) {
    return investigate(l, val, tree, n);
  }
  return 2;
}

bool check(uint64_t index, uint64_t find[], uint64_t subtree[],
           interval_node tree[], uint64_t n) {
  uint64_t l = find[index];
  uint64_t r = l + subtree[index];
  if (subtree[index] <= 1) {
    return true;
  }
  uint64_t val =
      (tree[l].second == tree[r].second)
          ? tree[l].second
          : ((tree[l].second == tree[l + 1].second) ? tree[l].second
                                                    : tree[r].second);
  long long mistakes = 0;
  while (l <= r) {
    // if 1 mistake already than return false.
    if (l % 2 == 1)
      mistakes += investigate(l, val, tree, n);
    if (mistakes > 1) {
      return false;
    }
    if (r % 2 == 0)
      mistakes += investigate(r, val, tree, n);
    l = (l + 1) / 2;
    r = (r - 1) / 2;
    if (mistakes > 1) {
      return false;
    }
  }
  return true;
}

interval_node tree[524'289];
uint64_t subtree_length[200'002];
uint64_t index_to_tree[200'002];
uint64_t color[200'002];
uint64_t count_sons[200'002];
fs_node parents[200'002];
uint64_t find_in_parents[200'202];

int main() {
  std::ios_base::sync_with_stdio(0);
  std::cin.tie(0);
  uint64_t n;
  uint64_t q;
  uint64_t temp;
  long long a;
  long long b;

  std::cin >> n;
  std::cin >> q;

  // create tree
  for (long long i = 0; i < n - 1; i++) {
    std::cin >> temp;
    count_sons[temp]++;                // how many sons element  has
    parents[i] = fs_node(temp, i + 2); // parent + son
  }
  qsort(parents, n - 1, sizeof(fs_node), [](const void *x, const void *y) {
    const auto a1 = static_cast<const fs_node *>(x)->first;
    const auto a2 = static_cast<const fs_node *>(y)->first;
    if (a1 < a2)
      return -1;
    if (a1 > a2)
      return 1;
    return 0;
  }); // -- sort by parent

  for (long long i = 0; i < n; i++) {
    if (find_in_parents[parents[i].first] == 0) { //
      find_in_parents[parents[i].first] = i;
    }
  }
  find_in_parents[1] = 0;

  for (long long i = 1; i < n + 1; i++) {
    std::cin >> temp;
    color[i] = temp;
  }

  // write in a tree
  // get indices of each position in a tree
  // fill subtree_table
  write(parents, find_in_parents, count_sons, color, tree, n, subtree_length,
        index_to_tree);

  for (long long i = 0; i < q; i++) {
    char c;

    std::cin >> c;
    if (c == '?') {
      std::cin >> a;
      if (check(a, index_to_tree, subtree_length, tree, n)) {
        std::cout << "TAK" << std::endl;
      } else {
        std::cout << "NIE" << std::endl;
      }
    } else {
      std::cin >> a;
      std::cin >> b;
      replace(a, b, tree, index_to_tree);
    }
  }
}
