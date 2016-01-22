
(define mkht make-hash-table)
(define htr hash-table-ref)
(define htks hash-table-keys)
(define htvs hash-table-values)
(define hte? hash-table-exists?)
(define hts! hash-table-set!)
(define htd! hash-table-delete!)

(define not-found 'this-sexy-name-was-not-found)
(define will-exist 'this-sexy-name-is-about-to-be-defined)

(define (idk obj msg cont err)
    (err (list 'message-not-understood (sexy-view obj) msg) cont))

(define (debug x . xs)
    (display (list x xs) (current-error-port))
    (newline (current-error-port)))

(define (debug-obj x)
    (define ps (sexy-view x))
    (map debug ps))

(define (for-pairs fn args)
    (if (not (eq? (modulo (length args) 2) 0))
        (error (list "for-pairs requires an even number of arguments!" args))
        (let loop ((newlist '()) (pairs args))
            (if (atom? pairs)
                newlist
                (let ((key (first pairs)) (val (second pairs)))
                    (loop (fn key val) (cddr pairs)))))))

(define (sexy-error form . args)
    (newline)
    (display "ERRORED!!") (newline)
    (display (sexy-view form)) (newline)
    (display (sexy-view args)) (newline)
    (newline)
    (exit))

(define (sexy-bool obj cont err)
    (sexy-send obj 'to-bool cont err))

(define (sexy-view obj)
    (sexy-send-atomic obj 'view))

(define (sort-symbol-alist ps)
    (sort ps
        (lambda (a b)
            (string<? (symbol->string (car a)) (symbol->string (car b))))))

(define (sexy-bool? x)
    (or (eq? x 'true) (eq? x 'false)))

(define (sexy-null? x)
    (eq? x 'null))

(define (sexy-equal? x y)
    (define (no-way)
        (sexy-error "= cannot compare objects " x " and " y "!")
        #f)
    (cond
        ((and (number? x) (number? y))
            (= x y))
        ((and (hash-table? x) (hash-table? y))
            (let ((xt (htr x 'type)) (yt (htr y 'type)))
                (if (not (eq? xt yt))
                    #f
                    (case xt
                        ((env fn operator) (eq? x y))
                        ((record)
                            (let ((x-pairs (sort-symbol-alist (hash-table->alist (htr x 'vars))))
                                  (y-pairs (sort-symbol-alist (hash-table->alist (htr y 'vars)))))
                                (equal? x-pairs y-pairs)))
                        (else (no-way))))))
        (else
            (equal? x y))))

(define (sexy-type-ord x)
    (cond
        ((sexy-bool? x) 1)
        ((boolean? x)   1)
        ((number? x)    2)
        ((char? x)      3)
        ((symbol? x)    4)
        ((string? x)    5)
        (else #f)))

(define (sexy-< x y)
    (define (no-way)
        (sexy-error "< cannot compare objects " x " and " y "!")
        #f)
    (cond
        ((and (number? x) (number? y)) (< x y))
        ((sexy-null? x) #t)
        ((sexy-null? y) #f)
        ((and (sexy-bool? x) (sexy-bool? y)) (and (eq? x 'false) (eq? y 'true)))
        ((and (sexy-bool? x) (boolean? y)) (and (eq? x 'false) y))
        ((and (char? x) (char? y) (char<? x y)))
        ((and (symbol? x) (symbol? y)) (string<? (symbol->string x) (symbol->string y)))
        ((and (string? x) (string? y)) (string<? x y))
        ((and (boolean? x) (boolean? y)) (and (not x) y))
        ((and (boolean? x) (sexy-bool? y)) (and (not x) (eq? y 'true)))
        (else
            (let ((x-ord (sexy-type-ord x)) (y-ord (sexy-type-ord y)))
                (if (not (and x-ord y-ord))
                    (no-way)
                    (< x-ord y-ord))))))

(define (sexy-> x y)
    (define (no-way)
        (sexy-error "> cannot compare objects " x " and " y "!")
        #f)
    (cond
        ((and (number? x) (number? y)) (> x y))
        ((sexy-null? x) #f)
        ((sexy-null? y) #t)
        ((and (sexy-bool? x) (sexy-bool? y)) (and (eq? x 'true) (eq? y 'false)))
        ((and (sexy-bool? x) (boolean? y)) (and (eq? x 'true) (not y)))
        ((and (char? x) (char? y) (char>? x y)))
        ((and (symbol? x) (symbol? y)) (string>? (symbol->string x) (symbol->string y)))
        ((and (string? x) (string? y)) (string>? x y))
        ((and (boolean? x) (boolean? y)) (and x (not y)))
        ((and (boolean? x) (sexy-bool? y)) (and x (eq? y 'false)))
        (else
            (let ((x-ord (sexy-type-ord x)) (y-ord (sexy-type-ord y)))
                (if (not (and x-ord y-ord))
                    (no-way)
                    (> x-ord y-ord))))))

(define (nodef x)
    (sexy-error x "Symbol " x " is not defined"))

(define (keyword->symbol k)
    (string->symbol (keyword->string k)))

(define (sexy-compile-method code)
    ((sexy-compile-fn (sexy-parse code)) (local-env) identity identity))

(define blessed
    '(def quote if seq set! macro fn wall gate capture ensure guard error env opt rest return))

(define (holy? name)
    (or (member name blessed)
        (let ((x (glookup name)))
            (not
                (or
                    (eq? x not-found)
                    (eq? x will-exist))))))

(define (blasphemy code name)
    (sexy-error code
        (string-join
            (list
                "The name \""
                (symbol->string name)
                "\" is sacred.  It cannot be redefined.")
            "")))

(define (sexy-read-file port)
    (define one (peek-char port))
    (define hash-bang
        (if (eq? one #\#)
            (read-line port)
            #f))
    (define program
        (let loop ((noob (sexy-read port)) (code '()))
            (if (eof-object? noob)
                (reverse code)
                (loop (sexy-read port) (cons noob code)))))
    (close-input-port port)
    program)

(define (sexy-cli-args xs)
    (define (rval args opts)
        (cons
            (if (and (pair? args) (> (length args) 1))
                (cddr (reverse args))
                '())
            opts))
    (if (pair? xs)
        (let* ((options (sexy-record))
               (setopt! (sexy-send-atomic options 'set!)))
            (let loop ((head (car xs)) (tail (cdr xs)) (args '()))
                (if (eq? (string-ref head 0) #\-)
                    (let ((k (string->symbol (irregex-replace/all "^-+" head ""))) (v (car tail)))
                        (setopt! k v)
                        (if (pair? (cdr tail))
                            (loop (cadr tail) (cddr tail) args)
                            (rval args options)))
                    (if (pair? tail)
                        (loop (car tail) (cdr tail) (cons head args))
                        (rval (cons head args) options)))))
        (rval '() (sexy-record))))

(define (sexy-run program)
    (define (get-with-the-program prog)
        (define head (car prog))
        (if (pair? head)
            (let ((sub-head (car head)))
                (if (eq? sub-head 'modules)
                    (let ((mods (cdr head)))
                        (map def-sexy-module mods)
                        (cdr prog))
                    prog))
            prog))
    (if (pair? program)
        ((sexy-seq-subcontractor (get-with-the-program program) #t)
            (cli-env)
            (lambda (v) (exit))
            top-err)
        (exit)))

