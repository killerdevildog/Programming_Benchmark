is_prime <- function(n) {
  if (n < 2) return(FALSE)
  if (n == 2) return(TRUE)
  if (n %% 2 == 0) return(FALSE)
  
  i <- 3
  while (i * i <= n) {
    if (n %% i == 0) return(FALSE)
    i <- i + 2
  }
  return(TRUE)
}

count <- 0
num <- 2
last_prime <- 0

while (count < 1000) {
  if (is_prime(num)) {
    last_prime <- num
    count <- count + 1
  }
  num <- num + 1
}

cat(last_prime, "\n", sep="")
