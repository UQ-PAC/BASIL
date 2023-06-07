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

void (*jump_table[3])() = {add_two, add_six, sub_seven};

int main(int argc, char **argv) {
    jump_table[0]();
    jump_table[1]();
    jump_table[2]();
    return 0;
}

