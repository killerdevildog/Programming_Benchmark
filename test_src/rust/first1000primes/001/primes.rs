fn is_prime(n: u32) -> bool {
    if n < 2 {
        return false;
    }
    if n == 2 {
        return true;
    }
    if n % 2 == 0 {
        return false;
    }
    let mut i = 3;
    while i * i <= n {
        if n % i == 0 {
            return false;
        }
        i += 2;
    }
    true
}

fn main() {
    let mut count = 0;
    let mut num = 2;
    let mut last_prime = 0;
    
    while count < 1000 {
        if is_prime(num) {
            last_prime = num;
            count += 1;
        }
        num += 1;
    }
    
    println!("{}", last_prime);
}
