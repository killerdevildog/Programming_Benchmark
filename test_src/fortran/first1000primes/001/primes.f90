PROGRAM Primes
    IMPLICIT NONE
    INTEGER :: count, num, last_prime
    LOGICAL :: is_prime_flag
    
    count = 0
    num = 2
    last_prime = 0
    
    DO WHILE (count < 1000)
        CALL is_prime(num, is_prime_flag)
        IF (is_prime_flag) THEN
            last_prime = num
            count = count + 1
        END IF
        num = num + 1
    END DO
    
    PRINT *, last_prime
    
CONTAINS
    SUBROUTINE is_prime(n, result)
        INTEGER, INTENT(IN) :: n
        LOGICAL, INTENT(OUT) :: result
        INTEGER :: i
        
        result = .TRUE.
        
        IF (n < 2) THEN
            result = .FALSE.
            RETURN
        END IF
        
        IF (n == 2) THEN
            result = .TRUE.
            RETURN
        END IF
        
        IF (MOD(n, 2) == 0) THEN
            result = .FALSE.
            RETURN
        END IF
        
        i = 3
        DO WHILE (i * i <= n)
            IF (MOD(n, i) == 0) THEN
                result = .FALSE.
                RETURN
            END IF
            i = i + 2
        END DO
    END SUBROUTINE is_prime
END PROGRAM Primes
