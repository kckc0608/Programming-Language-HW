int funct(int a);

void main() {
  funct(1);
  printf("%d", funct(2));
}

int funct(int a) {
  a += sizeof(int);

  a = -1;

  return a+1;
}