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
    void (*jump_table[3])() = {add_two, add_six, sub_seven};
    jump_table[1]();
    jump_table[2]();
    jump_table[3]();
    return 0;
}

