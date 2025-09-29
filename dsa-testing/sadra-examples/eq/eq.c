int *x;
int *y;
int *z;
int *p;
int *q;

int main(int c) {
   int a;
   int b;
   if (c) {
     x = &a;
     p = &x;
   } else {
     y = &b;
     p = &y;
   }
   q = &z;
}