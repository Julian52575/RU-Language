fn fib(n: int) -> int {
    if (n <= 1) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

fn main() -> int {
    let result: int = fib(20);
    printLn(result);
    return 0;
}
