
(define (sexy-write obj port)
    (write (sexy-view obj) port))

(define (sexy-print obj port)
    (display (sexy-view obj) port))

(define (sexy-read port)
    (define first-rune (peek-char port))
    (if (eof-object? first-rune)
        first-rune
        (sexy-parse (sexy-reader port))))

(define (sexy-reader port)
    (define token (peek-char port))
    (if (char-whitespace? token)
        (let ((_ (read-char port)) (next (peek-char port)))
            (if (eof-object? next)
                next
                (sexy-reader port)))
        (case token
            ((#\()
                (let ((t (read-char port)) (peek-a-boo (peek-char port)))
                    (cond
                        ((eq? peek-a-boo #\))
                            (read-char port)
                            '())
                        (else
                            (let ((head (sexy-reader port)))
                                (if (keyword? head)
                                    (let ((kw (keyword->symbol head)))
                                        (case kw
                                            ((pair)     (sexy-read-pair port))
                                            ((vector)   (sexy-read-vector port))
                                            ((record)   (sexy-read-record port))
                                            ((text)     (sexy-read-text port))
                                            ((template) (sexy-read-template port))
                                            ((rem)      (sexy-read-rem port))
                                            (else       (cons head (sexy-read-list port)))))
                                    (cons head (sexy-read-list port))))))))
            ((#\)) (error "read error: unexpected \")\"!\n"))
            ((#\') (sexy-read-quote port))
            ((#\` #\%) (sexy-read-quasiquote port))
            ((#\, #\$) (sexy-read-unquote port))
            ((#\@) (sexy-read-unquote-splicing port))
            ((#\\) (sexy-read-rune port))
            ((#\#) (sexy-read-matrix port))
            ((#\;) (sexy-read-comment port))
            ((#\|) (sexy-read-funky port))
            (else (read port)))))

(define (sexy-read-pair port)
    (define xs (sexy-read-list port))
    (cons (car xs) (cadr xs)))

(define (sexy-read-list port)
    (let loop ((token (peek-char port)) (acc '()))
        (cond
            ((eof-object? token)
                (error "read error: unexpected EOF in unterminated list!\n"))
            ((char-whitespace? token)
                (read-char port)
                (loop (peek-char port) acc))
            ((eq? token #\))
                (read-char port)
                (reverse acc))
            (else
                (let ((next (if (eq? token #\;) sexy-read-comment sexy-reader)))
                    (let ((new-acc (cons (next port) acc)))
                        (loop (peek-char port) new-acc)))))))

(define (sexy-read-vector port)
    (list->vector (sexy-read-list port)))

(define (sexy-read-matrix port)
    (read-char port)
    'matrix)

(define (sexy-read-record port)
    (apply sexy-record (sexy-read-list port)))

(define (sexy-read-text port)
    (let loop ((token (peek-char port)) (depth 0) (acc '()))
        (cond
            ((eof-object? token)
                (error "read error: unexpected EOF in text literal!\n"))
            ((eq? token #\()
                (let ((new-acc (cons (read-char port) acc)))
                    (loop (peek-char port) (+ depth 1) new-acc)))
            ((eq? token #\))
                (if (zero? depth)
                    (begin
                        (read-char port)
                        (string-trim-both (list->string (reverse acc))))
                    (begin
                        (let ((new-acc (cons (read-char port) acc)))
                            (loop (peek-char port) (- depth 1) new-acc)))))
            (else
                (let ((new-acc (cons (read-char port) acc)))
                    (loop (peek-char port) depth new-acc))))))

(define (sexy-read-template port)
    (define (get-str xs)
        (list->string (reverse xs)))
    (define (read-interpol port)
        (let loop ((token (peek-char port)) (acc '()))
            (cond
                ((eof-object? token)
                    (error "read error: unexpected EOF in template literal!\n"))
                ((eq? token #\})
                    (read-char port)
                    (if (eq? #\} (peek-char port))
                        (begin
                            (read-char port)
                            (sexy-reader (open-input-string (get-str acc))))
                        (let ((t (read-char port)))
                            (loop (peek-char port) (cons t (cons #\} acc))))))
                (else
                    (let ((t (read-char port)))
                        (loop (peek-char port) (cons t acc)))))))
    (define depth 0)
    (let loop ((t (peek-char port)))
        (if (char-whitespace? t)
            (begin
                (read-char port)
                (loop (peek-char port)))
            #f))
    (let loop ((token (peek-char port)) (acc '()) (texts '()))
        (cond
            ((eof-object? token)
                (error "read error: unexpected EOF in template literal!\n"))
            ((eq? token #\{)
                (read-char port)
                (if (eq? #\{ (peek-char port))
                    (begin
                        (read-char port)
                        (let ((txt (get-str acc))
                              (symbol (read-interpol port)))
                            (loop (peek-char port) '() (cons symbol (cons txt texts)))))
                    (let ((t (read-char port)))
                        (loop (peek-char port) (cons t (cons #\{ acc)) texts))))
            ((eq? token #\()
                (set! depth (+ depth 1))
                (let ((t (read-char port)))
                    (loop (peek-char port) (cons t acc) texts)))
            ((eq? token #\))
                (if (= depth 0)
                    (begin
                        (read-char port)
                        (cons 'cat (reverse (cons (string-trim-right (get-str acc)) texts))))
                    (begin
                        (set! depth (- depth 1))
                        (let ((t (read-char port)))
                            (loop (peek-char port) (cons t acc) texts)))))
            (else
                (let ((t (read-char port)))
                    (loop (peek-char port) (cons t acc) texts))))))
        

(define (sexy-read-quote port)
    (read-char port)
    (list 'quote (sexy-reader port)))

(define (sexy-read-quasiquote port)
    (read-char port)
    (list 'qq (sexy-reader port)))

(define (sexy-read-unquote port)
    (read-char port)
    (list 'unq (sexy-reader port)))

(define (sexy-read-unquote-splicing port)
    (read-char port)
    (list 'unqs (sexy-reader port)))

(define (sexy-read-rune port)
    (read-char port)
    (let ((next (peek-char port)))
        (case next
            ((#\( #\)) (read-char port))
            (else
                (if (char-alphabetic? next)
                    (let ((sym (read port)))
                        (case sym
                            ((lf) #\newline)
                            ((cr) #\return)
                            ((space) #\space)
                            ((tab) #\tab)
                            (else (string-ref (symbol->string sym) 0))))
                    (read-char port))))))

(define (sexy-read-comment port)
    (read-line port)
    (sexy-reader port))

(define (sexy-read-rem port)
    (sexy-read-list port)
    (sexy-reader port))

(define (sexy-read-funky port) ; hackity-hack
    (read-char port)
    (let ((next (peek-char port)))
        (if (eq? next #\\)
            (begin
                (read-char port)
                (let ((rune (read-char port)))
                    (read-char port)
                    rune))
            (sexy-reader port))))

(define (sexy-parse form)
	(define (desc form mt)
		(descend form (car mt) (cdr mt)))
    (define order
        (list
            (doterator)))
	(define atomized
		(let loop ((f form) (fns order))
			(if (eq? fns '())
				f
				(loop (desc f (car fns)) (cdr fns)))))
	atomized)

(define (warp form match? transform)
	(if (match? form)
		(let ((changed (transform form)))
			(if (equal? form changed)
				changed
				(begin 
					;(display form) (display " -> ") (display changed) (newline) (newline)
					changed)))
		form))

(define (descend form match? transform)
	(define (curses x) (descend x match? transform))
	(define newform (warp form match? transform))
	(if (pair? newform)
		(cons (curses (car newform)) (curses (cdr newform)))
		newform))

(define (doterator)
    ; foo.bar.baz.bax -> (send (send (send foo 'bar) 'baz) 'bax)
    (define (match? x)
        (and
            (symbol? x)
            (let ((s (symbol->string x)))
                (and
                    (not (string-contains s "/"))
                    (string-contains s ".")))))
    (define (transform x)
        (define (sym-or-num x)
            (define the-num (string->number x))
            (if the-num
                the-num
                (string->symbol x)))
        (let* (
            (str (symbol->string x))
            (words (string-split str ".")))
            (let loop ((this (sym-or-num (car words))) (left (cdr words)))
                (if (eq? left '())
                    this
                    (loop (list 'send this `(quote ,(sym-or-num (car left)))) (cdr left))))))
    (cons match? transform))

