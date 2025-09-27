/*Example of how to use sea_dsa_alias and sea_collapse */

#ifndef _SEADSA__H_
#define _SEADSA__H_

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// sea-dsa will unify all argument's cells
extern void sea_dsa_alias(const void *p, ...);
// sea-dsa will mark the node pointed to by p as read (R)
extern void sea_dsa_set_read(const void *p);
// sea-dsa will mark the node pointed to by p as modified (M)
extern void sea_dsa_set_modified(const void *p);
// sea-dsa will mark the node pointed to by p as heap (H)
extern void sea_dsa_set_heap(const void *p);
// sea-dsa will mark the node pointed to by p as ptr to int (2)
extern void sea_dsa_set_ptrtoint(const void *p);
// sea-dsa will mark the node pointed to by p as ptr to int (P)
extern void sea_dsa_set_inttoptr(const void *p);
// sea-dsa will mark the node pointed to by p as heap memory (H)
extern void sea_dsa_set_heap(const void *p);
// sea-dsa will mark the node pointed to by p as external (E)
extern void sea_dsa_set_external(const void *p);
// sea-dsa will collapse the argument's cell
extern void sea_dsa_collapse(const void *p);
// sea-dsa will return a fresh memory object
extern void *sea_dsa_new() __attribute__((malloc));
// like sea_dsa_new except used for creating a node representation of an
// existing memory object
extern void *sea_dsa_mk();
// links one cell to another cell. Can be manually extended with
// sea_dsa_link_<type>(void *p, unsigned offset, <type>> *p2) in order to
// describe what type it is linking to
extern void sea_dsa_link(const void *p, unsigned offset, const void *p2);
// access type of p. Can manually extend with
// sea_dsa_access_<type>(<type> *p, unsigned offset) in order to describe the
// type p accesses at an offset
extern void sea_dsa_access(const void *p, unsigned offset);
// sea-dsa will mark the node pointed by p as a sequence node of size sz
// The noded pointed by p cannot be already a sequence node and its
// size must be less or equal than sz.
extern void sea_dsa_mk_seq(const void *p, unsigned sz);
#ifdef __cplusplus
}
#endif
#endif

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

  sea_dsa_alias(p1,p2);
  return 0;
}   
