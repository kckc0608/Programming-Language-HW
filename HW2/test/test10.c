int (*p) (int);
int *p (int);

int* p, q;

int* p(int a) {
  return &a;
}