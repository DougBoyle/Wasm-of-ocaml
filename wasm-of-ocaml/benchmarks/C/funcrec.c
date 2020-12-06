// https://github.com/Chris00/ocaml-benchmark/blob/master/examples/func_record.ml

// TODO: Not well suited to writing in C, might not want to use this example program (or just not in C)
// TODO: Write the c++ version instead using classes? Except that my OCaml compiler doesn't have classes either
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

// Does choice of passing certain things by reference/value etc. affect performance noticeably?
// Added a,b,c and modulo to make equal to Grain version, where overflow not allowed
int main(){
    int i;
    int a = 1, b = 1, c = 1;
    Env e = {&f, &g}; // passing by reference to h_struct, but avoiding using malloc here
    for (i = 0; i < iters; i++){
        a = h_struct(&e, a) % 1024;
        b = h_function(&f, &g, b) % 1024;
        c = h(c) % 1024;
    }
    return a+b+c;
}
