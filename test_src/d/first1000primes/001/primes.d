import std.stdio;

bool isPrime(int n) {
    if (n < 2) return false;
    if (n == 2) return true;
    if (n % 2 == 0) return false;
    for (int i = 3; i * i <= n; i += 2) {
        if (n % i == 0) return false;
    }
    return true;
}

void main() {
    int count = 0;
    int num = 2;
    int lastPrime = 0;
    
    while (count < 1000) {
        if (isPrime(num)) {
            lastPrime = num;
            count++;
        }
        num++;
    }
    
    writeln(lastPrime);
}
