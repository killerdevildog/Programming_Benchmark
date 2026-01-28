proc isPrime(n: int): bool =
  if n < 2: return false
  if n == 2: return true
  if n mod 2 == 0: return false
  var i = 3
  while i * i <= n:
    if n mod i == 0: return false
    i += 2
  return true

var count = 0
var num = 2
var lastPrime = 0

while count < 1000:
  if isPrime(num):
    lastPrime = num
    inc count
  inc num

echo lastPrime
