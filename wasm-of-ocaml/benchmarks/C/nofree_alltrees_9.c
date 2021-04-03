#include <stdlib.h>

// trees require use of malloc
// free ignored as it gets quite complicated for this problem, lots of reuse of nodes
// would be simpler if not done naively i.e. remember the generated trees for each smaller size
// fair comparison as my compiler also doesn't worry about freeing memory
struct tree {
    struct tree *left;
    struct tree *right;
};
typedef struct tree *Tree;

Tree node(Tree l, Tree r){
    Tree ptr = malloc(sizeof(struct tree));
    ptr->left = l;
    ptr->right = r;
    return ptr;
}

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

list all_trees(int n){
    list result = NULL;
    if (n == 0){return cons(NULL, result);}
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
    }
    return result;
}

int main(){
    all_trees(9);
    return 0;
}
