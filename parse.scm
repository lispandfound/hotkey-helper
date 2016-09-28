(declare (unit parse) (uses utils))
(require-extension comparse irregex list-of)
(use srfi-1 srfi-13 comparse irregex list-of)
(define (split-line line split-regex)
  (irregex-split split-regex line))
(define tabs-and-spaces
  (char-set-difference char-set:whitespace (->char-set "\n"))
  )
(define (char-range lb ub)
  (map (o generic->string integer->char) (list-of x (x range (char->integer lb) (+ (char->integer ub) 1)))))
;;; List processing
(define whitespace
  (zero-or-more (in tabs-and-spaces)))
(define nl
  (is #\newline))
(define open-list (sequence whitespace (is #\{) whitespace))
(define non-list-charset
  (char-set-difference char-set:full (->char-set "{}\n")))
(define non-sep-charset
  (char-set-difference char-set:full (->char-set ",}")))
(define list-sep
  (is #\,))
(define close-list
  (sequence whitespace (is #\})))
(define str
  (as-string (repeated (in non-list-charset))))
(define wildcard
  (sequence* ((_ (is #\_)))
             (result "")))
(define ranged-list-item
  (sequence* ((lb (in non-sep-charset)) (_ (is #\-)) (ub (in non-sep-charset)))
             (result (char-range lb ub))))
(define literal-list-item
  (any-of wildcard (as-string (repeated (in non-sep-charset)))))
(define list-item
  (any-of ranged-list-item literal-list-item))
(define list-item*
  (sequence* ((_ list-sep) (i list-item))
             (result i)))
(define list-items
  (sequence* ((i list-item) (i* (zero-or-more list-item*)))
             (result (cons i i*))
             ) 
  )
(define chain-list
  (sequence* ((_ open-list) (l list-items) (_ close-list))
             (result l)))
(define str*
  (sequence chain-list str))
(define line
  (sequence* ((l str) (l* (zero-or-more str*)))
             (result (all-combonations (foldl append '() (cons (list l) l*) ))))) 

(define mandatory-whitespace
  (one-or-more (in tabs-and-spaces)))
(define command
  (sequence* ((_ mandatory-whitespace) (l line) (_ (maybe nl)))
             (result l)))
(define doc
  (sequence* ((_ (zero-or-more nl)) (_ (char-seq "# ;; ")) (l line))
             (result l)))
(define ordinary-comment
  (sequence* ((_ (zero-or-more nl)) (_ (is #\#)) (_ line))
             (result 'ignore)))
(define comment-or-docs
  (any-of doc ordinary-comment))
(define sxhkdrc
  (repeated (sequence* ((_ (zero-or-more nl)) (comments (zero-or-more comment-or-docs)) (doc (maybe doc)) (_ (one-or-more nl)) (hotkey line) (_ (one-or-more nl)) (cmds (one-or-more command)))
                       (result (list (flatten (filter (cut not-equal? 'ignore <>) comments)) (maybe-list hotkey) cmds)))))


(define (parse-file file)
  (parse sxhkdrc (call-with-input-file file
                                        (lambda (input-port)
                                          (let loop ((x (read-char input-port))
                                                     (acc ""))
                                            (if (not (eof-object? x))
                                                (begin
                                                  (loop (read-char input-port) (generic-str-append x acc )))
                                                acc))))))
(define (find-hotkey hotkeys string)
  (let* ((hotkey-equal?  (lambda (x)
                           (if (string? (car x))
                               (equal? (car x) string)
                               (member string (car x)))))
         (hotkey (find hotkey-equal? hotkeys)))
    (if hotkey
        (find hotkey-equal? (zip (maybe-list (car hotkey)) (maybe-list (caddr hotkey))))
        #f)))
