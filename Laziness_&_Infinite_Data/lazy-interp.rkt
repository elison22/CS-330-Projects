#lang lazy
(require racket/math)

#|
@author Joe Eklund, Brandt Ellison, Hayden Newey
BYU CS 330 Fall 2015
Project 8 - Laziness and Infinite Data
Submitted 10/20/2015
Licensed under the GNU GPL v3 found @ http://www.gnu.org/licenses/gpl-3.0.en.html
|#

;--------------------------------------------------------------------------------

;Testing definition from spec
(define print-only-errors #t)
(define (test l r)
  (if (equal? l r)
      (if print-only-errors
          (void)
          (printf "Test Passed~n"))
      (printf "Test Failed.~nActual:   ~S ~nExpected: ~S~n" l r)))

;--------------------------------------------------------------------------------


;(take-while p l) → (listof any/c)
;   p : (any/c . -> . boolean?)
;   l : (listof any/c)
;Returns the prefix of l such that for all elements p returns true. This is not filter.
(define (take-while p l)
  (cond
    [(empty? l) empty];empty case
    [(p (first l)) (cons (first l) (take-while p (rest l)))];I think this is right? I am not sure.
    [else empty]
    )
  )

(define (tw p l)
	(take-while p l))

;Tests for take-while
;From spec
(test (take-while (λ (n) (< n 5)) '(1 2 3 4 5 1 2))
      '(1 2 3 4))
;Check empty
(test (take-while (λ (n) true) '())
      '())
;Odd
(test (take-while odd? '(1 3 4))
      '(1 3))
;True
(test (take-while (λ (n) true) '(1 2 3))
      '(1 2 3))
;False
(test (take-while (λ (n) false) '(1 2 3))
      '())
;Satisfying prefix
(test (take-while (λ (n) (> n 5)) '(1 2 3 4 5 1 2))
      '())
(test (take-while (λ (n) (< n 100)) '(96 97 98 99 100 101 102 103 102 96 90))
      '(96 97 98 99))
;String
(test (take-while string? '("hello" "world"))
      '("hello" "world"))
;String/num
(test (take-while string? '("hello" "world" 1 2 3 "joe rules"))
      '("hello" "world"))
;Empty string/num
(test (take-while string? '(0 "hello" "world" 1 2 3 "joe rules"))
      '())
;Check non bool
(test (take-while (λ (n) +) '(1 2 3 4 5 1 2))
      '(1 2 3 4 5 1 2))
;--------------------------------------------------------------------------------

;Function to add1 starting at 0.
(define infinite
  (cons 0 (map add1 infinite))
  )

;(build-infinite-list f) → (listof any/c)
;  f : (exact-nonnegative-integer? . -> . any/c)
;Lazily constructs the infinite list such that (list-ref (build-infinite-list f) i) returns (f i).
(define (build-infinite-list f)
  (map f infinite)
  )

(define (bif f)
	(build-infinite-list f))

(define (% num div)
	(modulo num div))

(define (lr l i)
	(list-ref l i))

;Tests for build-infinite-list
(test(lr(bif (λ (n) 2)) 0) 2)
(test(lr(bif (λ (n) 2)) 1000) 2)
(test(lr(bif (λ (n) (+ n 2))) 1) 3)
(test(lr(bif (λ (n) (- n 2))) 0) -2)
(test(lr(bif (λ (n) (* n 2))) 5) 10)
(test(lr(bif (λ (n) (/ n 2))) 1000) 500)
(test(lr(bif odd?) 1000) #f)
(test(lr(bif even?) 1000) #t)
(test(lr(bif (λ (n) (even? (+ 1 n)))) 1000) #f)

(define (low-nn num)
	(tw (λ (x) (< x num)) (bif (λ (x) (+ x 2)))))

;--------------------------------------------------------------------------------

;(prime? n) → boolean?
;  n : exact-positive-integer?
;Returns true if n is prime.
(define (prime? n)
	(cond	[(= n 1) #f]
			[(< n 1) (error "must be positive")]
			[else (andmap	(λ (x) (not (zero? (% n x)))) (low-nn n))])
  )

;Tests for prime
(test (prime? 1) false)
(test (prime? 2) true)
(test (prime? 6) false)
(test (prime? 7) true)
(test (prime? 44178) false)
(test (prime? 103423) true)
;--------------------------------------------------------------------------------

;primes : (listof exact-positive-integer?)
;Returns the list of all primes.
(define primes
	(filter prime? (bif (λ (n) (+ n 1)))))

;Tests for primes
(test (list-ref primes 0) 2)
(test (list-ref primes 1) 3)
(test (list-ref primes 5) 13)
(test (list-ref primes 100) 547)

;--------------------------------------------------------------------------------

;(prime?/fast n) → boolean
;  n : exact-positive-integer?
;Returns true if n is prime, but tests only prime factors from primes/fast.
(define (prime?/fast num)
	(cond	[(= num 1) #f]
			[(< num 1) (error "must be positive")]
			[(= num 2) #t]
			[else (andmap	(λ (x) (not (zero? (% num x))))
							(tw (λ (x) (<= x (sqrt num))) primes/fast))]))

;primes/fast : (listof exact-positive-integer?)
;The list of all primes constructed with prime?/fast.
(define primes/fast
	(filter prime?/fast (bif (λ (x) (+ x 1)))))

;Tests for prime?/fast
;(test (prime?/fast 0) false)
(test (prime?/fast 1) false)
(test (prime?/fast 2) true)
(test (prime?/fast 4) false)
(test (prime?/fast 11) true)
(test (prime?/fast 25367) true)
(test (prime?/fast 25368) false)

;Tests for primes/fast
;do we need to show that this is faster than primes?
(test (list-ref primes/fast 0) 2)
(test (list-ref primes/fast 1) 3)
(test (list-ref primes/fast 25) 101)
(test (list-ref primes/fast 1500) 12569)
;--------------------------------------------------------------------------------

;Helper function from spec
(define (build-vector num f)
  (apply vector (build-list num f)))

;Tests for build-vector
(test (vector-ref (build-vector 1 (λ (x) x)) 0) 0)
(test (vector-ref (build-vector 3 (λ (x) (* x x))) 2) 4)

;(build-table rows cols f) → (vectorof (vectorof any/c))
;  rows : exact-positive-integer?
;  cols : exact-positive-integer?
;  f : (exact-nonnegative-integer? exact-nonnegative-integer? . -> . any/c)
;Lazily constructs a vector such that (vector-ref (vector-ref (build-table rows cols f) i) j)
;equals (f i j), when (< i rows) (< j cols).
(define (build-table rows cols f)
  (letrec ([map-vector (λ (x) (build-vector cols (λ (y) (f x y))))])
    (build-vector rows map-vector))
  )

;Tests for build-table
(test (vector-ref (vector-ref (build-table 5 5 (λ (x y) (* x y))) 4) 3) 12)
(test (vector-ref (vector-ref (build-table 1 1 (λ (x y) (+ x y))) 0) 0) 0)
;--------------------------------------------------------------------------------

;procedure
;(lcs-length s1 s2) → exact-nonnegative-integer?
;  s1 : string?
;  s2 : string?
;Computes the length of the longest common subsequence of two strings s1 and s2.
(define (lcs-length s1 s2)
  (letrec [(lcs-table (build-table (+ 1 (string-length s1));rows
                                   (+ 1 (string-length s2));cols
                                   (λ (x y)
                                     (if(or (= x 0) (= y 0));Fill first col and row with 0s
                                        0
                                        (if (char=? (string-ref s1 (- x 1)) (string-ref s2 (- y 1)))
                                            (+ (vector-ref (vector-ref lcs-table (- x 1)) (- y 1)) 1)
                                            (max
                                             (vector-ref (vector-ref lcs-table x) (- y 1))
                                             (vector-ref (vector-ref lcs-table (- x 1)) y))
                                     )))))]
    (vector-ref (vector-ref lcs-table (string-length s1)) (string-length s2))
    ))

;Tests for lcs-length
(test (lcs-length "katc" "catk") 2)
(test (lcs-length "helol" "heol") 4)
(test (lcs-length "yes" "no") 0)
(test (lcs-length "artist" "artsy") 4)
(test (lcs-length "abcdefghijklmnopqrstuvwxyz" "111abcdefghijklm111nopqrstuvw222xyzaaa") 26)
;--------------------------------------------------------------------------------
