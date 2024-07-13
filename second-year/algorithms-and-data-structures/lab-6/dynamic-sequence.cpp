#include <iostream>
using namespace std;

class Node {
public:
  uint64_t i;
  uint64_t x;
  uint64_t k;
  Node *left;
  Node *right;
  uint64_t height;
  Node() {}
  Node(Node *const &);
};

int max(int a, int b);

// Calculate height
int height(Node *N) {
  if (N == NULL)
    return 0;
  return N->height;
}

int max(int a, int b) { return (a > b) ? a : b; }

// New node creation
Node *newNode(uint64_t i, uint64_t x, uint64_t k) {
  Node *node = new Node;
  node->i = i;
  node->x = x;
  node->k = k;
  node->left = NULL;
  node->right = NULL;
  node->height = 1;
  return (node);
}
uint64_t getVal(Node *n) {
  if (n == NULL) {
    return 0;
  }
  return n->i + n->k;
}

// Rotate right
Node *rightRotate(Node *y) {
  //    Node* temp = y->left;
  Node *x = y->left;
  //    delete temp;
  //    Node* temp1 = x->right;
  Node *T2 = x->right;
  //    delete temp1;

  y->i -= getVal(x);
  x->right = y;
  y->left = T2;
  y->height = max(height(y->left), height(y->right)) + 1;
  x->height = max(height(x->left), height(x->right)) + 1;
  return x;
}
Node copy(Node *y, Node *x) {
  y->right = x->right;
  y->left = x->left;
  y->i = x->i;
  y->k = x->k;
  y->height = x->height;
  y->x = x->x;
  return *y;
}

Node *newNode(Node *n) {
  if (n == NULL)
    return NULL;
  Node *node = new Node;
  node->i = n->i;
  node->x = n->x;
  node->k = n->k;
  node->left = n->left;
  node->right = n->right;
  node->height = n->height;
  return (node);
}

Node::Node(Node *const &n) {
  if (n != NULL) {
    this->i = n->i;
    this->x = n->x;
    this->k = n->k;
    this->left = n->left;
    this->right = n->right;
    this->height = n->height;
  }
}

// Rotate left
Node *leftRotate(Node *x) {

  //    Node* temp = x->right;
  Node *y = new Node(x->right);
  //    delete temp;
  //    Node* temp1 = y->left;
  Node *T2 = y->left;
  //    delete temp1;
  y->i += getVal(x);
  y->left = x;
  x->right = T2;
  x->height = max(height(x->left), height(x->right)) + 1;
  y->height = max(height(y->left), height(y->right)) + 1;
  return y;
}

// Get the balance factor of each node
int getBalanceFactor(Node *N) {
  if (N == NULL)
    return 0;
  return height(N->left) - height(N->right);
}

// Insert a node
Node *insertNode(Node *node, uint64_t i, uint64_t x, uint64_t k) {
  // Find the correct postion and insert the node
  if (node == NULL)
    return (newNode(i, x, k));
  if (i <= node->i) {
    node->i += k;
    node->left = insertNode(node->left, i, x, k);
  } else if (i > node->i) {
    if (node->i + node->k > i) {
      uint64_t u = node->k;
      node->k = i - node->i;
      // maybe should insert to the top?
      node->right = insertNode(node->right, 0, node->x, u - node->k);

      node->right = insertNode(node->right, 0, x, k);

    } else {
      i = i - node->k - node->i;
      node->right = insertNode(node->right, i, x, k);
    }
  }

  // Update the balance factor of each node and
  // balance the tree
  node->height = 1 + max(height(node->left), height(node->right));
  int balanceFactor = getBalanceFactor(node);
  if (balanceFactor > 1) {
    if (i < node->left->i) {
      return rightRotate(node);
    } else if (i > node->left->i) {
      node->left = leftRotate(node->left);
      return rightRotate(node);
    }
  }
  if (balanceFactor < -1) {
    if (i > node->right->i) {
      return leftRotate(node);
    } else if (i < node->right->i) {
      node->right = rightRotate(node->right);
      return leftRotate(node);
    }
  }
  return node;
}

uint64_t find(Node *root, uint64_t i) {
  uint64_t depth = 0;
  try {

    while (!(root->i <= i && i < root->i + root->k)) {
      depth++;
      if (root->i > i)
        root = root->left;
      else {
        i -= root->i + root->k;
        root = root->right;
      }
    }
    return root->x;

  } catch (...) {
    std::cout << "depth: " << depth << " i: " << i << endl;
  }
}

// Print the tree
void printTree(Node *root, string indent, bool last) {
  if (root != nullptr) {
    printTree(root->right, indent + "   ", true);

    cout << indent;
    if (last) {
      cout << "";
      indent += "   ";
    } else {
      cout << "";
      indent += "|  ";
    }
    cout << "(i=" << root->i << ", x=" << root->x << ", k=" << root->k << ")"
         << endl;
    printTree(root->left, indent, false);
  }
}

int main() {
  Node *root = NULL;
  //    uint64_t w = 0;
  //    uint64_t len = 0;
  //
  //
  //
  //    for(int i = 0; i < 500'000; i++){
  //        uint64_t a =(uint64_t( rand())) * uint64_t(rand());
  //        uint32_t b =(uint8_t( rand())) * uint8_t(rand());
  //        uint8_t c = rand() ;
  //
  //        a = (a + w) % (len + 1);
  //
  //        root = insertNode(root, a, b, c);
  //        len =(len + c);
  //        for (int j = 0; j < i ; j+= 10'0000){
  //
  //            w = find(root, j);
  ////            if(j % 10000 == 0)
  //            cout << w << endl;
  //
  //        }
  //    }

  uint64_t n;
  std::cin >> n;
  char c;

  uint64_t len = 0;
  uint64_t w = 0;
  uint64_t j = 0;
  uint64_t x = 0;
  uint64_t k = 0;
  uint64_t a = 0;

  for (long long i = 0; i < n; i++) {
    cin >> c;
    if (c == 'i') {

      std::cin >> j;
      std::cin >> x;
      std::cin >> k;
      j = (j + w) % (len + 1);

      //            cout << "inserting: " << j << endl;
      root = insertNode(root, j, x, k);
      len += k;

      //            printTree(root, "", true);
      //            cout << endl;

    } else {
      a = 0;
      std::cin >> a;
      a = (a + w) % len;
      w = find(root, a);
      //            cout << "find j: " << a << " x: " << w << endl;
      std::cout << w << std::endl;
    }
  }
  return 0;
}

//    root = deleteNode(root, 13);
//    cout << "After deleting " << endl;
//    printTree(root, "", true);
