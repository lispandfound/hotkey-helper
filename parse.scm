(declare (unit parse) (uses utils))
;; Main parsing engine for sxhkd
;; The parse has the capability to produce a set of lists, each list containing a document set, all possible combinations of commands, and all possible combinations of keybindings
(use srfi-1 srfi-13)
;; The special characters for a command
(define command-separators '((#\{ . token-open-brace)
                             (#\} . token-closed-brace)
                             (#\, . token-separator)))
;; The separators for the main hotkey setup
(define separators '((#\space . token-space)
                     (#\_ . token-wildcard)
                     (#\+ . token-accumulator)
                     (#\{ . token-open-brace)
                     (#\} . token-closed-brace)
                     (#\, . token-separator)
                     (#\; . token-hotkey-separator)
                     (#\@ . token-key-release)
                     (#\~ . token-key-replay)))
(define (tokenize char-bag separators)
  (reverse (foldl (lambda (acc tok)
                    (if (or (not (member tok (map car separators))) )
                        (if (or (null-list? acc) (not (string? (car acc))))
                            (cons (generic->string tok) acc)
                            (cons (generic-str-append tok (car acc)) (cdr acc)))
                        (cons (cdr (assoc tok separators)) acc))) '() char-bag)))
(define (tokenize-hotkey char-bag)
  (tokenize char-bag separators))
(define (tokenize-command char-bag)
  (tokenize char-bag command-separators))
(define (lex-line line)
  ;; Line may either be
  ;; A comment '#' or ' '
  ;; Or the following structure
  #|
  HOTKEY
  [;]COMMAND
  HOTKEY      := CHORD_1 ; CHORD_2 ; … ; CHORD_n
  CHORD_i     := [MODIFIERS_i +] [~][@]KEYSYM_i
  MODIFIERS_i := MODIFIER_i1 + MODIFIER_i2 + … + MODIFIER_ik
  |#
  (tokenize-hotkey (string->list line)))
(define (parse-list tokens)
  (let* ((actual-list (take-while (cut not-equal? 'token-closed-brace <>) tokens)))
    (cons
     (parse-hotkey (filter (cut not-equal? 'token-separator <>) actual-list)) (+ (length actual-list) 1))))
(define (parse-hotkey tokens)
  ;; Interpret an already lexed line
  ;; Returns a list containing every hotkey interpreted
  (reverse
   (let loop ((acc '()) (tokens tokens))
     (if (> (length tokens) 0)
         (let ((token (car tokens)))
           (cond
            ((equal? token 'token-open-brace) (let* ((tokens (cdr tokens))
                                                     (combonations (parse-list tokens)))
                                                (loop (cons (car combonations) acc) (drop tokens (cdr combonations)))))
            ((equal? token 'token-accumulator) (loop (cons (string-append (car acc) " ") (cdr acc)) (cdr tokens)))
            ((equal? token 'token-wildcard) (loop (cons "" acc) (cdr tokens)))
            ((equal? token 'token-separator) (loop (cons "," acc) (cdr tokens)))
            ((member token (map cdr separators)) (loop acc (cdr tokens)))
            (#t (loop (cons token acc) (cdr tokens)))))
         acc))))
(define (doc-from-comment line)
  (string-join (cdr (string-split line ";; ")) " "))
(define (command? line)
  (let ((fc (firstc line)))
    (or (equal? fc #\space) (equal? fc #\tab))))
(define (doc? line)
  (string-prefix? "# ;;" line))
(define (comment? line)
  (let ((fc (firstc line)))
    (equal? fc #\#)))
(define (lex-command command)
  (tokenize-command (string->list command)))
(define (parse-file file)
  (let loop ((lines (string-split (call-with-input-file file
                                        (lambda (input-port)
                                          (let loop ((x (read-char input-port))
                                                     (acc ""))
                                            (if (not (eof-object? x))
                                                (begin
                                                  (loop (read-char input-port) (generic-str-append x acc )))
                                                acc)))) "\n"))
             (hotkey "")
             (comments '())
             (acc '()))
    (if (= (length lines) 0)
        acc
        (let* ((cur-line (car lines))
               (fc (firstc cur-line)))
          (cond
           ;; Current line is shell command for hotkey
           ((command? cur-line) (let ((commands (map (lambda (c)
                                                       (parse-hotkey (lex-command (string-trim c)))) (take-while command? lines))))
                                  (loop (drop lines (length commands)) "" '() (cons (append (list (all-combonations hotkey) comments  ) (map all-combonations commands)) acc))))
           ;; Current line is documentation
           ((doc? cur-line) (loop (cdr lines) hotkey (cons (all-combonations (parse-hotkey (lex-command (doc-from-comment cur-line)))) comments)  acc))
           ;; Current line is comment
           ((comment? cur-line) (loop (cdr lines) hotkey comments  acc))
           ;; Current line is hotkey
           (#t (loop (cdr lines) (parse-hotkey (lex-line cur-line)) comments  acc)))))))

(define (find-hotkey hotkeys string)
  (let* ((hotkey-equal?  (lambda (x)
                           (if (string? (car x))
                               (equal? (car x) string)
                               (member string (car x)))))
         (hotkey (find hotkey-equal? hotkeys)))
    (if hotkey
        (find hotkey-equal? (zip (maybe-list (car hotkey)) (maybe-list (caddr hotkey))))
        #f)))
