       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRIMES.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNT PIC 9(4) VALUE 0.
       01 WS-NUM PIC 9(5) VALUE 2.
       01 WS-LAST-PRIME PIC 9(5) VALUE 0.
       01 WS-I PIC 9(5).
       01 WS-IS-PRIME PIC 9 VALUE 1.
       01 WS-SQRT PIC 9(5).
       
       PROCEDURE DIVISION.
           PERFORM UNTIL WS-COUNT = 1000
               PERFORM CHECK-PRIME
               IF WS-IS-PRIME = 1
                   MOVE WS-NUM TO WS-LAST-PRIME
                   ADD 1 TO WS-COUNT
               END-IF
               ADD 1 TO WS-NUM
           END-PERFORM
           
           DISPLAY WS-LAST-PRIME
           STOP RUN.
       
       CHECK-PRIME.
           MOVE 1 TO WS-IS-PRIME
           
           IF WS-NUM < 2
               MOVE 0 TO WS-IS-PRIME
               EXIT PARAGRAPH
           END-IF
           
           IF WS-NUM = 2
               MOVE 1 TO WS-IS-PRIME
               EXIT PARAGRAPH
           END-IF
           
           DIVIDE WS-NUM BY 2 GIVING WS-I REMAINDER WS-SQRT
           IF WS-SQRT = 0
               MOVE 0 TO WS-IS-PRIME
               EXIT PARAGRAPH
           END-IF
           
           MOVE 3 TO WS-I
           COMPUTE WS-SQRT = WS-I * WS-I
           PERFORM UNTIL WS-SQRT > WS-NUM
               DIVIDE WS-NUM BY WS-I GIVING WS-SQRT REMAINDER WS-SQRT
               IF WS-SQRT = 0
                   MOVE 0 TO WS-IS-PRIME
                   EXIT PARAGRAPH
               END-IF
               ADD 2 TO WS-I
               COMPUTE WS-SQRT = WS-I * WS-I
           END-PERFORM.
