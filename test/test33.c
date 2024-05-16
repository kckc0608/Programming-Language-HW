#include <stdio.h>

struct coord {
  //int x, y;
  char w, z;
  short* a;
  double** b;
  int* c; // 가변 길이 배열 대신 포인터 사용
  char* d; // 가변 길이 배열 대신 포인터 사용
};

/*void f(struct coord x, struct coord y) {
  return;
}*/

int main() {
  struct coord x, y;
  f(x, y);
}