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

void list_free(list l){
    while (l != NULL){
        list tail = l->tail;
        free(l);
        l = tail;
    }
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
    struct cell result = {0, 0}; // dummy node
    list ptr = &result;
    while (l1 != NULL && l2 != NULL){
        if (l1->head < l2->head){
            ptr->tail = l1;
            ptr = ptr->tail;
            l1 = l1->tail;
        } else {
            ptr->tail = l2;
            ptr = ptr->tail;
            l2 = l2->tail;
        }
    }
    if (l1 == NULL){
        ptr->tail = l2;
    } else {
        ptr->tail = l1;
    }

    return result.tail;
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
  list_free(l);

  list result = merge(mergesort(l1), mergesort(l2));

  return result;
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
