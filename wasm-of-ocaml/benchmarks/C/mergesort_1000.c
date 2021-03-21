#include <stdlib.h>

typedef struct cell *list;
struct cell {
    int head;
    list tail;
};

list cons(int head, list tail){
    list ptr = malloc(sizeof(struct cell));
    ptr->head = head;
    ptr->tail = tail;
    return ptr;
}

int a = 214013;
int c = 2531011;
int m = 65536;
int x = 12345;
int randm(int n){
    x = (a*x + c) % m;
    return x % n;
}

list merge(list l1, list l2){
    if (l1 == NULL) return l2;
    if (l2 == NULL) return l1;
    if (l1->head < l2->head) return cons(l1->head, merge(l1->tail, l2));
    return cons(l2->head, merge(l1, l2->tail));
}

// results passed in to avoid having to deal with returning pairs
void split (list *l1, list *l2, list l){
    while (l != NULL){
        list tmp = cons(l->head, *l1);
        *l1 = *l2;
        *l2 = tmp;
        l = l->tail;
    }
}

list mergesort(list l){
  if (l == NULL || l->tail == NULL){
    return l;
  }
  list l1 = NULL;
  list l2 = NULL;
  split(&l1, &l2, l);
  return merge(mergesort(l1), mergesort(l2));
}

list init(int n, int m){
    if (n == 0) return NULL;
    else return cons(randm(m), init(n-1, m));
}

int main(){
    int n = 1000;
    int m = 10000;
    list l = init(n, m);
    list sorted_l = mergesort(l);
    return sorted_l->head; // to avoid merge being optimised away (check code generated)
}
