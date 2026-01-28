#include <iostream>

bool is_prime(int n) {
    if (n < 2) return false;
    if (n == 2) return true;
    if (n % 2 == 0) return false;
    for (int i = 3; i * i <= n; i += 2) {
        if (n % i == 0) return false;
    }
    return true;
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
    
    std::cout << last_prime << std::endl;
    return 0;
}
