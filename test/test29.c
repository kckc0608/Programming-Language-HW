register int x;    // int 1
volatile char y;   // char 1
register unsigned float z;
volatile signed long w;
const static double t;
union MyUnion {
    int intVal;    // int 1
    float floatVal;
    char charVal;  // char 1
};

union MyUnion u;

void main() {
  A:
  goto A;
}