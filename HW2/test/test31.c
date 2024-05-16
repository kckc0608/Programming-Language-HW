#include <stdio.h>
struct coord {
  int x, y;
  char w, z;
  short* a;
  double** b;
  int c[];
  char d[];
};
int main() {
  struct coord my_coord = {1, 1};
}