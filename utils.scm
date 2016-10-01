(declare (unit utils))
(use data-structures)

(define not-equal?
  (complement equal?))

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
    (map reverse (fold iter
                       (map list first)
                       rest))))

(define (maybe-list thing)
  ;; Return thing nested in a list if thing is not already a list
  ;; E.g 1 -> '(1), but '(1) -> '(1)
  (cond
   ((and (list? thing)
         (= (length thing) 1)) (maybe-list (car thing)))
   ((list? thing) thing)
   (#t (list thing))))

(define (bundle-list lst)
  ;; Nest every element of a list in a list, if not already in one
  ;; E.g '(1 2 (3)) -> '((1) (2) (3))
  (map maybe-list lst))

(define (all-combonations lst)
  ;; get all possible hotkey/command combonations, using a cartesian-product
  (if (any list? lst) 
      (map string-concatenate (cartesian-product (bundle-list lst)))
      (string-concatenate lst)))
