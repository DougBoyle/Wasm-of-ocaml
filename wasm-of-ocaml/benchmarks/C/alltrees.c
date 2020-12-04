#include <stdlib.h>

// trees do require use of malloc
// represent Leaf as null
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

// For purpose of this program. Strictly a tree, no reused nodes
// SHOULDN'T EVEN BE BOTHERING WITH FREE? NO GC IN OCAML SO DON'T FREE IN C?
// but actual C implementation would be doing memory management
void tree_free(Tree t){
    if (t != NULL){
        tree_free(t->left);
        tree_free(t->right);
        free(t);
    }
}

// Need to rewrite whole map/fold process to be iterative, don't want to have to do malloc etc. in mapping
typedef struct cell *list;
struct cell {
    Tree *head;
    list tail;
}

list cons(Tree head, list tail){
    list ptr = malloc(sizeof(struct cell));
    ptr->head = head;
    ptr->tail = tail;
    return ptr;
}

// at what step does list need to be freed?
void list_free(list l){
    if (l != NULL){
        list_free(l->tail);
        free(list);
    }
}

list all_trees(int n){
    list result = NULL;
    if (n == 0){return cons(NULL, result);}
    // TODO: Recursive cases of all_trees below
}

int main(){
    all_trees(10);
}

let rec map f = function
  | [] -> []
  | x::xs -> (f x)::(map f xs)
let rec foldr f l e = match l with
  | [] -> e
  | x::xs -> f x (foldr f xs e)

let rec all_trees = function
  | 0 -> [Leaf]
  | n ->
  let rec trees_i i =
    let left_side = all_trees i
    and right_side = all_trees (n-1-i) in
    foldr (fun left trees -> (map (fun right -> Node(left, right)) right_side) @ trees) left_side [] in
  let rec getall = function
    | 0 -> trees_i 0
    | n -> (trees_i n)@(getall (n-1)) in
  getall (n-1)
