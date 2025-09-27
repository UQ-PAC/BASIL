extern void print(int x);
extern void* mymalloc (unsigned int sz);

struct element {
  int x;
  int y;
}; typedef struct element* Elem;

struct node {
  struct node * next;
  Elem  head;  
};
typedef struct node* List;


List mkList (int sz, Elem e) {
  if (sz < 1) return 0;
    
  List l = (List) mymalloc(sizeof(struct node));
  List p = l;
  int i;
  for (i=0; i<sz; i++) {
    p->head = e;
    if (i == sz -1) {
      p->next = 0;
      break;
    }
    p->next = (List) mymalloc(sizeof(struct node));
    p = p->next;
  }
  return l;
}


int main (){
  int x = 4;
  int y = 2;

  Elem e = (Elem) mymalloc (sizeof(struct element));
  e->x = 5;
  e->y = 6;
  
  List p1 = mkList (5,e);
  List p2 = mkList (5,e);
  while (p1) {
    print(p1->head->x);
    p1=p1->next;
  }
  
  while (p2) {
    print(p2->head->y);
    p2=p2->next;
  }
   
  return 0;
}   
