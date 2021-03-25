// https://github.com/Chris00/ocaml-benchmark/blob/master/examples/func_record.ml

typedef struct env Env;
struct env {
    int (*f)(int);
    int (*g)(int);
};

int h_struct(Env *e, int x){
    return 1 + e->f(x) + e->g(x);
}

int h_function(int (*f)(int), int (*g)(int), int x){
    return 1 + f(x) + g(x);
}

int f(int x){
    return x + 1;
}

int g(int x){
    return 2 * x;
}

int h(int x){
    return 1 + f(x) + g(x);
}

int iters = 10000;

int main(){
    int i;
    int a = 1, b = 1, c = 1;
    Env e = {&f, &g};
    // written as 3 loops to match ml version
    for (i = 0; i < iters; i++){
        a = h_struct(&e, a) % 1024;
    }
    for (i = 0; i < iters; i++){
        b = h_function(&f, &g, b) % 1024;
    }
    for (i = 0; i < iters; i++){
        c = h(c) % 1024;
    }
    return a+b+c;
}
