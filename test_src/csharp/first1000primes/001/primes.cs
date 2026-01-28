using System;

class Primes {
    static bool IsPrime(int n) {
        if (n < 2) return false;
        if (n == 2) return true;
        if (n % 2 == 0) return false;
        for (int i = 3; i * i <= n; i += 2) {
            if (n % i == 0) return false;
        }
        return true;
    }
    
    static void Main() {
        int count = 0;
        int num = 2;
        int lastPrime = 0;
        
        while (count < 1000) {
            if (IsPrime(num)) {
                lastPrime = num;
                count++;
            }
            num++;
        }
        
        Console.WriteLine(lastPrime);
    }
}
