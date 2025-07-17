int x = 5;
int y = 7;

int main() {
  *((short*)(((char*)(&x))+1)) = (short)y;

  return 0;
}