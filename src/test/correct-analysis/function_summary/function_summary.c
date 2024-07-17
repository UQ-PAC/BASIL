int x, y;
int get_two();

int main() {
    x = 1;
    y = get_two();
}

__attribute((noinline)) int get_two() {
    return 2;
}
