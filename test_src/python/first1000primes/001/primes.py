def count_primes():
    count = 0
    num = 2
    last_prime = 0
    
    while count < 1000:
        is_prime = True
        if num < 2:
            is_prime = False
        elif num == 2:
            is_prime = True
        elif num % 2 == 0:
            is_prime = False
        else:
            i = 3
            while i * i <= num:
                if num % i == 0:
                    is_prime = False
                    break
                i += 2
        
        if is_prime:
            last_prime = num
            count += 1
        num += 1
    
    print(last_prime)

count_primes()
