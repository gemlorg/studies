#include <iostream>
#include <list>
#include <queue>
#include <stack>


using namespace std;

void topologicalSortUtil(uint64_t v, bool visited[], stack<uint64_t> &Stack,
                         list<uint64_t> *adj) {

  visited[v] = true;

  list<uint64_t>::iterator i;
  for (i = adj[v].begin(); i != adj[v].end(); ++i)
    if (!visited[*i])
      topologicalSortUtil(*i, visited, Stack, adj);

  Stack.push(v);
}

list<uint64_t> adj[500'000];
uint64_t vals[500'000];

int main() {
  ios_base::sync_with_stdio(0);
  cin.tie(0);
  uint64_t n;
  uint64_t m;
  uint64_t k;
  uint64_t t;
  uint64_t a;
  uint64_t b;
  std::cin >> n;
  std::cin >> m;
  std::cin >> k;
  for (uint64_t i = 0; i < n; i++) {
    std::cin >> t;
    vals[i] = t;
  }

  for (uint64_t i = 0; i < m; i++) {
    std::cin >> a;
    std::cin >> b;
    a--;
    b--;
    adj[b].push_back(a);
  }

  stack<uint64_t> Stack;
  bool *visited = new bool[n];
  for (uint64_t i = 0; i < n; i++)
    if (visited[i] == false)
      topologicalSortUtil(i, visited, Stack, adj);

  while (Stack.empty() == false) {
    auto next_vertex = Stack.top();
    Stack.pop();
    for (auto i = adj[next_vertex].begin(); i != adj[next_vertex].end(); i++) {

      vals[*i] = max(vals[next_vertex], vals[*i]);
    }
  }
  std::priority_queue<uint64_t, vector<uint64_t>, greater<uint64_t>> q;
  for (uint64_t i = 0; i < n; i++) {
    q.push(vals[i]);
  }
  uint64_t aa = q.top();
  for (uint64_t i = k; i > 0; i--) {
    aa = max(aa, q.top());
    q.pop();
  }
  std::cout << aa << std::endl;
}
