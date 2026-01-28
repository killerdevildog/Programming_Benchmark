#include <stdio.h>

int is_prime(int n) {
    if (n < 2) return 0;
    if (n == 2) return 1;
    if (n % 2 == 0) return 0;
    for (int i = 3; i * i <= n; i += 2) {
        if (n % i == 0) return 0;
    }
    return 1;
}

int main() {
    int count = 0;
    int num = 2;
    int last_prime = 0;
    
    while (count < 1000) {
        if (is_prime(num)) {
            last_prime = num;
            count++;
        }
        num++;
    }
    
    printf("%d\n", last_prime);
    return 0;
}
