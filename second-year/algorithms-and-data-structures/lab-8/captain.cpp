#include <iostream>
#include <list>
#include <queue>
#include <stack>
#include <vector>

// #include <bits/stdc++.h>
using namespace std;

using u = uint64_t;
using edge = std::pair<u, u>;
class Graph {
  u V; // No. of vertices'

  std::list<edge> *adj;

  void topologicalSortUtil(u v, bool visited[], std::stack<u> &Stack);

public:
  Graph(u V); // Constructor

  void addEdge(u v, u w, u weight);

  int minDistance(u *dist, bool *sptSet);

  void shortestPath(u src);
};

Graph::Graph(u V) {
  this->V = V;
  adj = new std::list<edge>[V];
}

void Graph::addEdge(u v, u w, u weight) {
  adj[v].push_back(*new edge(w, weight)); // Add w to v’s list.
}

using vertix = std::pair<u, std::pair<u, u>>;

vertix vertices[200'001];
int compare_x(const void *a, const void *b) {

  auto *x = (vertix *)a;
  auto *y = (vertix *)b;
  if (x->second.first > y->second.first)
    return 1;
  if (x->second.first < y->second.first)
    return -1;
  return x->second.second > y->second.second ? 1 : -1;
}
int compare_y(const void *a, const void *b) {

  auto *x = (vertix *)a;
  auto *y = (vertix *)b;
  if (x->second.second > y->second.second)
    return 1;
  if (x->second.second < y->second.second)
    return -1;
  return x->second.first > y->second.first ? 1 : -1;
}
u d(vertix a, vertix b) {
  return std::min(
      abs(((long long)a.second.first) - (long long)b.second.first),
      abs(((long long)a.second.second) - (long long)b.second.second));
}

int Graph::minDistance(u dist[], bool sptSet[]) {
  // Initialize min value
  u min = 1'000'000'000'000, min_index;

  for (u v = 0; v < V; v++)
    if (sptSet[v] == false && dist[v] <= min)
      min = dist[v], min_index = v;

  return min_index;
}

void Graph::shortestPath(u src) {

  std::priority_queue<edge, std::vector<edge>, std::greater<edge>> pq;
  std::vector<u> dist(V, UINT64_MAX);

  pq.push(std::make_pair(0, src));
  dist[src] = 0;

  while (!pq.empty()) {
    int u = pq.top().second;
    pq.pop();

    std::list<std::pair<long long, long long>>::iterator i;
    for (auto i = adj[u].begin(); i != adj[u].end(); ++i) {
      int v = (*i).first;
      int weight = (*i).second;

      // If there is shorted path to v through u.
      if (dist[v] > dist[u] + weight) {
        // Updating distance of v
        dist[v] = dist[u] + weight;
        pq.push(std::make_pair(dist[v], v));
      }
    }
  }

  //    // Print shortest distances stored in dist[]
  //    printf("Vertex Distance from Source\n");
  //    for (int i = 0; i < V; ++i)
  //        printf("%d \t\t %d\n", i, dist[i]);
  std::cout << dist[V - 1] << std::endl;
}

int main() {
  std::ios_base::sync_with_stdio(0);
  std::cin.tie(0);
  u n;
  u a;
  u b;
  std::cin >> n;
  Graph g(n);
  for (u i = 0; i < n; i++) {
    std::cin >> a;
    std::cin >> b;
    vertices[i] = vertix{i, std::make_pair(a, b)};
  }
  //    for(u i = 0; i < n; i++){
  //        std::cout << vertices[i].first << " ";
  //    }
  //    std::cout << std::endl;
  qsort(vertices, n, sizeof(vertix), compare_x);
  //    for(u i = 0; i < n; i++){
  //        std::cout << vertices[i].first << " ";
  //    }
  //    std::cout << std::endl;
  g.addEdge(vertices[0].first, vertices[1].first, d(vertices[0], vertices[1]));
  for (int i = 1; i < n - 1; i++) {
    g.addEdge(vertices[i].first, vertices[i - 1].first,
              d(vertices[i], vertices[i - 1]));
    g.addEdge(vertices[i].first, vertices[i + 1].first,
              d(vertices[i], vertices[i + 1]));
  }
  g.addEdge(vertices[n - 1].first, vertices[n - 2].first,
            d(vertices[n - 1], vertices[n - 2]));

  qsort(vertices, n, sizeof(vertix), compare_y);
  g.addEdge(vertices[0].first, vertices[1].first, d(vertices[0], vertices[1]));
  for (int i = 1; i < n - 1; i++) {
    g.addEdge(vertices[i].first, vertices[i - 1].first,
              d(vertices[i], vertices[i - 1]));
    g.addEdge(vertices[i].first, vertices[i + 1].first,
              d(vertices[i], vertices[i + 1]));
  }
  g.addEdge(vertices[n - 1].first, vertices[n - 2].first,
            d(vertices[n - 1], vertices[n - 2]));
  g.shortestPath(0);
}
