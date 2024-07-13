#include <iostream>
using namespace std;
long long numberOfProducts;
long long listOfProducts[1000000];
long long partialSums[1000000];
long long ps = 0;
long long leftPointers[1000000];
long long leftSamePointers[1000000];
long long rightPointers[1000000];
long long nextPrice;
long long leftEvenPointer = -1; //-1 means can't be used
long long leftOddPointer = -1;
long long rightEvenPointer = -1;
long long rightOddPointer = -1;
long long repeat;
long long k;

int main() {
  ios_base::sync_with_stdio(0);
  cin.tie(0);

  cin >> numberOfProducts;

  for (long long i = 0; i < numberOfProducts; i++) {
    cin >> nextPrice;
    listOfProducts[i] = nextPrice;
    if (nextPrice % 2 == 0) {
      leftPointers[i] = leftOddPointer;
      leftSamePointers[i] = leftEvenPointer;
    } else {
      leftPointers[i] = leftEvenPointer;
      leftSamePointers[i] = leftOddPointer;
    }
    nextPrice % 2 == 0 ? leftEvenPointer = i : leftOddPointer = i;
  }

  for (long long i = numberOfProducts - 1; i >= 0; i--) {
    rightPointers[i] =
        (listOfProducts[i] % 2 == 0 ? rightOddPointer : rightEvenPointer);
    ps += listOfProducts[i];
    partialSums[i] = ps;

    listOfProducts[i] % 2 == 0 ? rightEvenPointer = i : rightOddPointer = i;
  }

  cin >> repeat;
  long long answers[repeat];

  // repeat for each day
  for (long long t = 0; t < repeat; t++) {
    cin >> k;
    if (k > numberOfProducts || k == 0) {
      cout << -1 << endl;
      continue;
    }
    long long lastDigit = numberOfProducts - k;
    long long s = 0;

    // add numbers >= k
    s = partialSums[lastDigit];

    // if sum is odd
    if (s % 2 == 1) {
      cout << s << endl;
      continue;

    } else {

      if (lastDigit == 0) {
        cout << -1 << endl;
        continue;
      }
      // if sum is even
      if (s % 2 == 0) {
        // check if right or left pointers are not active. if active, swap odd
        // and even integers
        long long removeEven = -1;
        long long removeOdd = -1;
        if (leftPointers[lastDigit] != -1) {
          removeEven = s - listOfProducts[lastDigit] +
                       listOfProducts[leftPointers[lastDigit]];
        }
        if (rightPointers[lastDigit] != -1 &&
            leftSamePointers[lastDigit] != -1) {
          removeOdd = s - listOfProducts[rightPointers[lastDigit]] +
                      listOfProducts[leftSamePointers[lastDigit]];
        }
        cout << max(removeOdd, removeEven) << endl;
      }
    }
  }
}
