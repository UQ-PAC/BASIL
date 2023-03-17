int x = 5;

void add_two() {
  x = x + 2;
}

void add_six() {
  x = x + 6;
}

void sub_seven() {
  x = x - 7;
}

int main(int argc, char **argv) {
  switch (argc) {
    case 0:
      add_two();
      break;
    case 1:
      add_six();
    case 2:
      sub_seven();
      break;
    case 3:
      add_two();
      sub_seven();
      break;
    case 4:
      add_six();
      break;
    case 5:
      add_six();
      add_two();
    case 6:
      add_six();
    case 7:
      sub_seven();
    case 8:
      sub_seven();
      add_six();
      break;
    case 9:
      add_two();
      add_six();
      break;
    case 10:
      sub_seven();
      add_two();
      add_six();
    case 11:
      sub_seven();
      add_two();
      break;
    default:
      return 1;
  }
  return x;
}
