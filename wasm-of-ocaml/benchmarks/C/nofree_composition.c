// Tries to show the profile cost of composing small functions.
// https://github.com/Chris00/ocaml-benchmark/blob/master/examples/composition.ml

// Example of a program that really isn't suited to being written in C, due to its lack of closures
// Need to define the data structures to be passed around and a composition aware function
// Demonstrates why we would like to compile other languages to Wasm
// Doesn't attempt to free the memory used

#include <stdlib.h>

// for fairness, use the same random number generation as the others
int a = 214013;
int c = 2531011;
int m = 65536;
int x = 12345;
int randm(int n){
    x = (a*x + c) % m;
    return x % n;
}

int n = 100;
int rotate(void* params, int k){
    int* args = (int*)params;
    return (k + args[0]) % n;
}
int reverse(void* params, int k){
    int* args = (int*)params;
    int i = args[0];
    int j = args[1];
    if (i <= k && k <= j) return j + i - k;
    else return k;
}
int splice(void* params, int k){
    int* args = (int*)params;
    int l = args[0];
    int i = args[1];
    int j = args[2];
    if (k < j){
        if (k < i) return k;
        else return k + l + 1;
    } else {
        if (k <= j + l) return k - j + 1;
        else {
            int k2 = k - l - 1;
            if (k2 < i) return k2;
            else return k;
        }
    }
}

// Biggest issue with this program is the need to define closures so we can compose functions by folding to get
// a function which applies every function in the list.
// Was a significant challenge to work out how to write this compared to writing it in Grain/JS/OCaml
// Due to having both functions on single elements and mapped across lists, also have lots of void* arguments.
// C++ would avoid some of these issues by allowing template functions. (Also pairs)
// Could also just duplicate definitions for functions on lists vs functions on ints
struct closure {
    void *(*f)(void*, void*);
    void* params;
};
typedef struct closure Closure;

int compose(void* p, int k){
    Closure** params = (Closure**)p;
    Closure* c1 = params[0];
    Closure* c2 = params[1];
    return (int)c2->f(c2->params, c1->f(c1->params, (void*)k));
}

Closure *mk_compose(Closure *c1, Closure *c2){
    Closure** params = malloc(sizeof(Closure*)*2);
    params[0] = c1;
    params[1] = c2;
    Closure *c = malloc(sizeof(Closure));
    c->f = (void * (*)(void *, void *))compose;
    c->params = params;
    return c;
}

int apply(Closure *c, int n){
    return (int)c->f(c->params, (void*)n);
}

typedef struct cell *list;
struct cell {
    void *head;
    list tail;
};

list cons(void *head, list tail){
    list ptr = malloc(sizeof(struct cell));
    ptr->head = head;
    ptr->tail = tail;
    return ptr;
}

list map(Closure *c, list l){
    list result = NULL;
    if (l == NULL) return result;
    else return cons((void*)apply(c, (int)l->head), map(c, l->tail)); // change to while loop, track top + tail
}

list init(int (*f)(int), int n, int i){
    if (n == 0) return NULL;
    else return cons((void*)(f(i)), init(f, n-1, i+1));
}

// In C++, could use std:pair
struct pair {
    list l1;
    list l2;
};
typedef struct pair Pair;

// gcc supports nested functions to make syntax easier, but clang doesn't :(
Pair random_perm(int n, Pair acc, int i){
        if (i <= 0) return acc;
        int c = randm(3);
        Closure *p = malloc(sizeof(Closure));
        if (c == 0){
            int *params = malloc(sizeof(int));
            params[0] = randm(n);
            p->f = (void * (*)(void *, void *))rotate;
            p->params = params;
        } else if (c == 1){
            int *params = malloc(sizeof(int)*2);
            params[0] = randm(n);
            params[1] = randm(n);
            p->f = (void * (*)(void *, void *))reverse;
            p->params = params;
        } else {
            int *params = malloc(sizeof(int)*3);
            params[0] = randm(n);
            params[1] = randm(n);
            params[2] = randm(n);
            p->f = (void * (*)(void *, void *))splice;
            p->params = params;
        }
        Closure *p_vec = malloc(sizeof(Closure));
        p_vec->f = (void * (*)(void *, void *))map;
        p_vec->params = p;
        acc.l1 = cons(p, acc.l1);
        acc.l2 = cons(p_vec, acc.l2);
        return random_perm(n, acc, i-1);
    }

Pair make_perms(int n){
    Pair acc = {NULL, NULL};
    return random_perm(n, acc, n);
}

int identity(int n){return n;}
int identity_param(void* params, int n){return n;}

int main(){
    int ncomp = 100;
    Pair perms = make_perms(ncomp);
    list p_f = perms.l1;
    list p_v = perms.l2;
    list v = init(identity, n, 0);
    Closure idClosure = {(void * (*)(void *, void *))identity_param, NULL};
    Closure *c = &idClosure;
    while (p_f != NULL){
        c = mk_compose(p_f->head, c);
        p_f = p_f->tail;
    }
    list l = v;
    while (p_v != NULL){
        Closure *p_vec = (Closure*)p_v->head;
        l = p_vec->f(p_vec->params, l);
        p_v = p_v->tail;
    }
    return 0;
}
