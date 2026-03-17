int main(int argc, char** argv) {
  int x = (char)argc;
  int y = (char)argv;
  int out = 0;
  // sets `out` to true iff (char)argc == (char) argv.
  asm (
    "cmp %1, %2\n"
    "cset %0, EQ" 
    : "=r"(out)
    : "r"(x) , "r" (y)
  );
  return out;
}