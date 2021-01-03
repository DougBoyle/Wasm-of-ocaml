int fib(int n){
    switch(n){
    case 0:
    case 1:
      return 1;
    default:
      return fib(n-1) + fib(n-2);
    }
}

int main(){
    return fib(38);
}
