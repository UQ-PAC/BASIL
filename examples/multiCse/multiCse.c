int main(int argc, char** argv) {
  long x = (char)argc;
  long y = (char)argv;
  long out = 0;
  // sets `out` to true iff (char)argc == (char) argv.
  asm (
    "adds %0, %1, %2\n"
    "adds %0, %1, %2" 
    : "=r"(out)
    : "r"(x) , "r" (y)
  );
  return out;
}
