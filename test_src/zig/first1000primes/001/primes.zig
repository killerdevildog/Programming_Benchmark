const std = @import("std");

fn isPrime(n: u32) bool {
    if (n < 2) return false;
    if (n == 2) return true;
    if (n % 2 == 0) return false;
    var i: u32 = 3;
    while (i * i <= n) : (i += 2) {
        if (n % i == 0) return false;
    }
    return true;
}

pub fn main() !void {
    var count: u32 = 0;
    var num: u32 = 2;
    var last_prime: u32 = 0;
    
    while (count < 1000) {
        if (isPrime(num)) {
            last_prime = num;
            count += 1;
        }
        num += 1;
    }
    
    std.debug.print("{d}\n", .{last_prime});
}
