
#define BASIL_ASSERT(text, ...)   ((int (*)(char *, ...))0xfeeb)(text, __VA_ARGS__)

int y = 20;

int main() {

  int x = 0; 

  x += 1;
  x *= 2; 
  x *= y; // 40

  BASIL_ASSERT("(R1 + 1bv64)== 41bv64 && R1 != R2", x, y);

}
