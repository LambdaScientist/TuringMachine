; Code from : http://morphett.info/turing/turing.html
; This example program checks if the input string is a binary palindrome.
; Input: a string of 0's and 1's, eg '1001001'


; Machine starts in state 0.

; State 0: read the leftmost symbol
d0 0 _ r d1o
d0 1 _ r d1i
d0 _ _ * accept     ; Empty input

; State 1o, 1i: find the rightmost symbol
d1o _ _ l d2o
d1o * * r d1o

d1i _ _ l d2i
d1i * * r d1i

; State 2o, 2i: check if the rightmost symbol matches the most recently read left-hand symbol
d2o 0 _ l d3
d2o _ _ * accept
d2o * * * reject

d2i 1 _ l d3
d2i _ _ * accept
d2i * * * reject

; State 3, 4: return to left end of remaining input
d3 _ _ * accept
d3 * * l d4
d4 _ _ r d0  ; Back to the beginning
d4 * * l d4