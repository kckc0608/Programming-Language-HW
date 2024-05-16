void main() {
  int i = 0;
  if (i == 0) {
    i = 1;
  }

  if (i == 1) {
    i = 2;
  } else {
    i = 3;
  }

  if (i == 2) {
    i = 3;
  } else if (i == 3) {
    i = 4;
  } else {
    i = 5;
  }

  switch(i) {
    case 1:
      i = 2;
      break;
    case 2:
      i = 3;
      break;
    default:
      i = 5;
      break;
  }
}