(begin
  (def pi 3)
  (def nats (quote (1 2 3 4 5 6 7 8 9 10)))
  (def id (fn (a) a))
  (def nil ())
  (def not (fn (b) (if b nil t)))
  (def or (fn (a b) (if a t b)))
  (def and (fn (a b) (if a b nil)))
  (def succ (fn (n) (+ n 1)))
  (def pred (fn (n) (- n 1)))
  (def zero? (fn (n) (= n 0)))
  (def even? (fn (n) (= (mod n 2) 0)))
  (def odd? (fn (n) (= (mod n 2) 1)))
  (def nil? (fn (xs) (if xs nil t)))
  (def exp
        (fn (x p) (if (= 0 p) 1 (* x (exp x (- p 1))))))
  (def map
        (fn (f xs) (if xs (cons (f (car xs)) (map f (cdr xs))) ())))
  (def filter
        (fn (f xs)
            (if xs
              (if (f (car xs))
                (cons (car xs)
                      (filter f (cdr xs))) (filter f (cdr xs)))
              ())))
  (def fact
        (fn (x) (if (zero? x) 1 (* x (fact (- x 1))))))
  (def range
        (fn (min max) (if (= min max) () (cons min (range (succ min) max)))))
  (def nth
        (fn (n xs) (if (zero? n) (car xs) (nth (pred n) (cdr xs)))))
  (def append
        (fn (xs ys) (if xs (cons (car xs) (append (cdr xs) ys)) ys)))
  (def length
        (fn (xs) (if xs (succ (length (cdr xs))) 0)))
  (def take
        (fn (n xs)
            (if (zero? n) () (cons (car xs) (take (pred n) (cdr xs))))))
  (def <= (fn (a b) (or (< a b) (= a b))))
  (def > (fn (a b) (not (<= a b))))
  (def >= (fn (a b) (or (> a b) (= a b))))
  (def fib
        (fn (x)
           (if (<= x 1)
                1
                (+ (fib (- x 1)) (fib (- x 2))))))
)
