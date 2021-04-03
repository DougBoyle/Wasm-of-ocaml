#include <stdlib.h>

// trees require use of malloc
// lists freed once used, all nodes allocated only freed at end due to reuse of nodes
struct tree {
    struct tree *left;
    struct tree *right;
};
typedef struct tree *Tree;

typedef struct cell *list;
struct cell {
    Tree head;
    list tail;
};

list cons(Tree head, list tail){
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

// just a list of every node allocated, done this way due to reuse of trees
list arena = NULL;

void arena_free(){
    while (arena != NULL){
        free(arena->head);
        list tail = arena->tail;
        free(arena);
        arena = tail;
    }
}

Tree node(Tree l, Tree r){
    Tree ptr = malloc(sizeof(struct tree));
    ptr->left = l;
    ptr->right = r;

    arena = cons(ptr, arena);

    return ptr;
}

list all_trees(int n){
    list result = NULL;
    if (n == 0){return cons(NULL, result);} // NULL used for Leaf node
    for (int i = 0; i < n; i++){
        list left = all_trees(i);
        list right = all_trees(n-i-1);
        while (left != NULL){
            list pos = right;
            while (pos != NULL){
                result = cons(node(left->head, pos->head), result);
                pos = pos->tail;
            }
            left = left->tail;
        }
        list_free(left);
        list_free(right);
    }
    return result;
}

int main(){
    all_trees(9);
    arena_free();
    return 0;
}
