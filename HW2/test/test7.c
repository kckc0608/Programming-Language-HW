struct coord {
  short c;
};

int f(int*, int); // 무시
int g(int a, int b); // 무시

int f(int *a, int b) { // int 2, pointer 1
  return a + b;
}

int main() {
  struct coord a, b;
}

int g(int a, int b) { // int 2
  return a - b;
}