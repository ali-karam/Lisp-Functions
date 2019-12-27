;; Computes Fibonacci numbers
(defun fib(n)(fib-rec n 1 1))

(defun fib-rec(n b c)
(if (zerop n) nil
(cons b (fib-rec(- n 1) c (+ b c)))))


;; Returns the number of non-numbers in the list
(defun notnums (n)
(cond
((endp n) 0)
((listp (first n)) (+ (notnums (first n)) (notnums (rest n))))
((numberp (first n)) (notnums (rest n)))
(T (+ 1 (notnums (rest n))))
))

;; Checks for a valid NS License plate. (First three letters, and last three are numbers)
(defun NSLicense (a)
    (if(and
        (= 6(list-length a))
        (check4g (first a))
        (check4char (second a))
        (check4char (third a))
        (check4num (fourth a))
        (check4num (fifth a))
        (check4num (sixth a))
        )t nil))

(defun firstChar (a) (subseq(string-upcase(symbol-name a)) 0 1))

(defun check4char (a)
(if(and   (symbolp a)
    (eq(length(symbol-name a)) 1)
    (string>=(firstChar a)"A")
(string<=(firstChar a) "Z")
) T NIL))

(defun check4G (a)
(if(and
    (check4char a)
    (string<=(firstChar a)"G")
) T NIL))

(defun check4num (a)
(if(and
    (numberp a)
    (and (< a 10) (>= a 0))
    )t nil))

;; Checks if a number is prime
(defun isprime (x &optional (i 2))
  (case x
    (1     nil)
    ((2 3) t)
    (t     (or (not (<= i (sqrt x)))
               (and (/= (mod x i) 0)
                    (isprime x (+ i 1)))))))

;; Infix calculator
(defun infix (n)
(if (atom n) n
(eval (list (second n) (infix (first n))(infix (third n))))
    ))

;; Writes a function that adds to original parameter
(defun makefun (a)
    (defun addit (b)
        (eval (list '+ a b)))
    )

;; Checks if something is in a list
(defun isthere (n alist)
(cond
((endp alist) nil)
((eq n (first alist)) T)
(T (isthere n (rest alist)))
    ))

;; Reduces nesting in a list to a flat structure
(defun flatten (n)
(cond
((null n) nil)
((endp n) n)
((atom (first n)) (append (list (first n)) (flatten (rest n))))
(T (append(flatten (first n)) (flatten (rest n))))
    ))
