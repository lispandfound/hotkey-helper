(use srfi-1)
(use srfi-13)
;; Parse sxhkd files and return list of possible keybindings
(define (firstc string)
  (car (string->list string)))
(define (string-split-arr string arr)
  (foldl (lambda (acc tok) (flatten (map (lambda (item) (string-split item tok)) acc))) (list string) arr)
  )
(define separators '((#\space . token-space)
                     (#\+ . token-accumulator)
                     (#\{ . token-open-brace)
                     (#\} . token-closed-brace)
                     (#\, . token-separator)
                     (#\; . token-hotkey-separator)
                     (#\@ . token-key-release)
                     (#\~ . token-key-replay)))
(define modifiers '("super" "hyper" "meta" "alt" "control" "ctrl" "shift" "mode_switch" "lock" "mod1" "mod2" "mod3" "mod4" "mod5"))
(define mod-token '(mod-super mod-hyper mod-meta mod-alt mod-control mod-ctrl mod-shift mode-mode_switch mod-lock mod-mod1 mod-mod2 mod-mod3 mod-mod4 mod-mod5))
(define (generic-str-append thing string)
  (format "~a~a" string thing))
(define (generic->string thing)
  (format "~a" thing))
(define (tokenize char-bag)
  (map
   (lambda (item)
     (if (member item modifiers)
         (string->symbol (string-append "mod-" item))
         item))
   (reverse (foldl (lambda (acc tok)
                     (if (or (not (member tok (map car separators))) )
                         (if (not (string? (car acc)))
                             (cons (generic->string tok) acc)
                             (cons (generic-str-append tok (car acc)) (cdr acc)))
                         (cons (cdr (assoc tok separators)) acc))) '("") char-bag))))
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
  (tokenize (string->list line)))
(define (modifier? token)
  (member token mod-token))
(define (modifier->short-string token)
  (let ((modifier-string (symbol->string token)))
    (cond
     ((member token '(mod-mod1 mod-mod2 mod-mod3 mod-mod4 mod-mod5))
      (generic-string-append "m" (string-ref modifier-string (string-length modifier-string))))
     ((equal? token 'mod-mode_switch) "ms")
     ((equal? token 'mod-alt) "alt")
     (#t (generic->string (substring modifier-string 4 6))))))
(define (parse-tokens tokens)
  ;; Interpret an already lexed line
  ;; Returns a list containing every hotkey interpreted
  (foldl (lambda (acc token)
            (cond
             ((modifier? token) (string-append acc (modifier->short-string token)))
             ((equal? token 'token-open-brace) (string-append acc "("))
             ((equal? token 'token-closed-brace) (string-append acc ")"))
             ((equal? token 'token-separator) (string-append acc "|"))
             ((equal? token 'token-accumulator) (string-append acc "-"))
             ((equal? token 'token-hotkey-separator) (string-append acc "/"))
             (#t (string-append acc token)))) "" (filter (lambda (tok) (not (member tok '(token-space token-key-release token-key-replay)))) tokens)))
(define (parse-file file)
  (let* ((separated-file (string-split (call-with-input-file file
                                        (lambda (input-port)
                                          (let loop ((x (read-char input-port))
                                                     (acc ""))
                                            (if (not (eof-object? x))
                                                (begin
                                                  (loop (read-char input-port) (generic-str-append x acc )))
                                                acc)))) "\n"))
         (hotkeys (map (lambda (line)
                         (parse-tokens (lex-line line))) (filter (lambda (line) (let ((fc (firstc line)))
                                                                                  (not (or (equal? fc #\#) (equal? fc #\space) (equal? fc #\tab))))) separated-file)))
         (comment-list (map (lambda (line) (string-join (cdr (string-split line ";; ")) " ")) (filter (lambda (s) (string-prefix? "# ;; " s)) separated-file)))
         (diff (- (length hotkeys) (length comment-list))))
    (zip hotkeys (append  comment-list (make-list (if (< diff 0) 0 diff) "")))))
(define (format-output hotkeys )
  (display (string-join (map (lambda (hotkey) (format "~a -- ~a" (car hotkey) (cadr hotkey))) hotkeys) "\n")))
(define (main args)
  (format-output (parse-file "/home/jake/.config/sxhkd/sxhkdrc"))
  )
