(declare (unit utils))
;; Various utilities needed for parsing the sxhkd files
(define (firstc string)
  ;; Get first character
  (car (string->list string)))
(define (not-equal? x y)
  ;; The "I can't believe Chicken Scheme doesn't have this in the std library" function
  (not (equal? x y)))
(define (generic-str-append thing string)
  ;; Append any two types supported by format together into a string (super helpful!)
  (format "~a~a" string thing))
(define (generic->string thing)
  ;; Generically (according to format) convert thing to a string
  (format "~a" thing))
(define (cartesian-product lst)
  ;; Cool piece of set theory (use this a lot in the parser) that creates all possible combinations of any given lists
  ;; So '((1 2) (2)) -> '((1 2) (2 2)).
  ;; This is how the compact multiple command syntax ("super + {a,b}") is expanded.
  (let ((first (car lst))
        (rest (cdr lst)))
    (define (iter l result)
      (define (prepend-all x)
        (map (cut cons <> x) l))
      (concatenate (map prepend-all result)))
    (map reverse (fold iter (map list first) rest))))
(define (maybe-list thing)
  ;; Return thing nested in a list if thing is not already a list
  ;; E.g 1 -> '(1), but '(1) -> '(1)
  (if (list? thing)
      thing
      (list thing)))
(define (merge-strings lst)
  ;; Squish all adjacent string items in a list together
  ;; E.g '("a" 2 "b" "c") -> '("a" 2 "bc")
  (reverse (foldl (lambda (acc cur)
                    (cond
                     ((and (string? cur) (> (length acc) 0) (string? (car acc))) (cons (string-append (car acc) cur) (cdr acc)))
                     (#t (cons cur acc)))) '() lst)))
(define (bundle-list lst)
  ;; Nest every element of a list in a list, if not already in one
  ;; E.g '(1 2 (3)) -> '((1) (2) (3))
  (map maybe-list lst))
(define (all-combonations lst)
  ;; get all possible hotkey/command combonations, using a cartesian-product
  (if (any list? lst) 
      (map string-concatenate (cartesian-product (bundle-list lst)))
      (string-concatenate lst)))
