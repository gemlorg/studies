#include <iostream>
using namespace  std;
long long minDistance = 0;
long long currentDistance;
char currentColor;

int main() {
    ios_base::sync_with_stdio (0);
    cin.tie (0);

    string colors;
    cin >> currentColor;
    cin >> colors;
    minDistance = size(colors);

    for(char nextColor : colors) {
        if(currentColor == '*'){
            currentColor = nextColor;

        }else if (nextColor == '*') {
            currentDistance++;
        } else if (nextColor == currentColor) {
            currentDistance = 0;

        } else {
            minDistance = min(currentDistance, minDistance);
            currentDistance = 0;
            currentColor = nextColor;
        }
    }
    cout << size(colors) + 1 - minDistance  << endl;
}