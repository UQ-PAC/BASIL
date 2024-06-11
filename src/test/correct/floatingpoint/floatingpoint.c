int main(int argc, char **argv) {
  float x = 10.5438123;
  float y = 3.34929420;
  float xy = x * y;
  xy = x / y;
  xy = x - y;
  xy = x + y;
  double z = 51343242925.39582643;
  double zy = z * y;
  zy = z - y;
  zy = z + y;
  zy = z / y;
  x = (float) zy;
  z = (double) y;
  int a = (int) zy;

  return a;
}