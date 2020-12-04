// https://ocaml.org/learn/tutorials/99problems.html problems 32-34
// imperative implementation of the same problem. Some recursion replaced with iteration
int gcd(int a, int b){
    if (b == 0) return a;
    return gcd(b, a%b);
}

int coprime(int a, int b){
    return gcd(a, b) == 1;
}

int phi(int n){
    if (n == 1) return 1;
    int acc = 0;
    for (int d = 1; d < n; d++){
        if (coprime(n, d)) acc++;
    }
    return acc;
}

int main(){
    int x = phi(20000);
    int y = 0;
    for (int i = 0; i < 3000; i++){
        if (i != 0){
            y += phi(i);
        }
    }
    return 0;
}
