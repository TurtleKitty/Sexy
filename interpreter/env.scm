
(define (local-env)
    (sexy-environment #f))

(define (cli-env)
    (define lenv (local-env))
    (extend lenv
        '(sys)
        (list sys)
        top-cont
        top-err))

(define (symbols-env)
    (define expanded (read-expand-cache-prog sexy-use-symbols (local-env)))
    (define compiled (sexy-seq-subcontractor (cdr expanded) #t))
    (define the-env (local-env))
    (compiled the-env top-cont top-err)
    (set! load-symbols-env the-env))

(define (global-env)
    (define (make-new)
        (define prelude (local-env))
        (define preset! (sexy-send-atomic prelude 'def!))
        (define (fill-prelude fs)
            (define (setem! p)
                (preset! (car p) (cdr p)))
            (map setem! fs))
        (define primitives
            (list
                (cons 'is? eq?)
                (cons '+ +)
                (cons '- -)
                (cons '* *)
                (cons '/ /)
                (cons '= sexy-equal?)
                (cons '> sexy->)
                (cons '< sexy-<)
                (cons 'div quotient)
                (cons 'rem remainder)
                (cons 'mod modulo)
                (cons 'num? number?)
                (cons 'int? integer?)
                (cons 'nint?
                    (lambda (x)
                        (and (number? x) (not (integer? x)))))
                (cons 'rune? char?)
                (cons 'pair cons)
                (cons 'pair? pair?)
                (cons 'list list)
                (cons 'list? list?)
                (cons 'vector
                    (sexy-proc
                        'primitive-function
                        'global
                        (lambda (args opts cont err)
                            (define size ((sexy-send-atomic opts 'get) 'size))
                            (define init ((sexy-send-atomic opts 'get) 'init))
                            (cont
                                (if (integer? size)
                                    (let ((v (make-vector size init)))
                                        (vector-map (lambda (i x) (vector-set! v i x)) (list->vector args))
                                        v)
                                    (apply vector args))))))
                (cons 'vector? vector?)
                (cons 'text
                    (sexy-proc
                        'primitive-function
                        'global
                        (lambda (args opts cont err)
                            (define size ((sexy-send-atomic opts 'get) 'size))
                            (define init ((sexy-send-atomic opts 'get) 'init))
                            (cont
                                (if (integer? size)
                                    (let ((s (make-string size (if (char? init) init #\space))))
                                        (vector-map (lambda (i x) (string-set! s i x)) (list->vector args))
                                        s)
                                    (apply string args))))))
                (cons 'text? string?)
                (cons 'rand random)
                (cons 'record
                    (sexy-proc
                        'primitive-function
                        'global
                        (lambda (args opts cont err)
                            (cont (apply sexy-record args)))))
                (cons 'object
                    (sexy-proc
                        'primitive-function
                        'global
                        (lambda (args opts cont err)
                            (define autos (sexy-send-atomic opts 'auto))
                            (define rsend (sexy-send-atomic opts 'resend))
                            (define default (sexy-send-atomic opts 'default))
                            (if (eq? autos 'null) (set! autos #f) #f)
                            (if (eq? rsend 'null) (set! rsend #f) #f)
                            (if (eq? default 'null) (set! default #f) #f)
                            (cont (sexy-object args autos rsend default)))))
                (cons 'send
                    (sexy-proc
                        'primitive-function
                        'global
                        (lambda (args opts cont err)
                            (define l (length args))
                            (if (< l 2)
                                (err (list 'arity "send requires two arguments: an object and a message.") cont)
                                (sexy-send (car args) (cadr args) cont err)))))
                (cons 'fetch
                    (lambda (uri)
                        (define got (get-uri uri))
                        (if (eq? got not-found)
                            'null
                            got)))
                (cons 'math
                    (sexy-object
                        (list
                            'e      2.718281828459045
                            'phi    1.618033988749895
                            'pi     3.141592653589793
                            'tau    6.283185307179587
                            'root-2 1.414213562373095
                            'max max
                            'min min
                            'sum (lambda (xs) (apply + xs))
                            'product (lambda (xs) (apply * xs))
                            'pow (lambda (x y) (expt x y))
                            'sqrt sqrt
                            'log log
                            'sin sin
                            'cos cos
                            'tan tan
                        )
                        #f
                        #f
                        #f))
                (cons 'gensym sexy-gensym)
                (cons 'uuid uuid-v4)
                (cons 'cat
                    (sexy-proc
                        'primitive-function
                        'global
                        (lambda (args opts cont err)
                            (define l (length args))
                            (define texts (map (lambda (x) (sexy-send-atomic x 'to-text)) args))
                            (define strings (map (lambda (t) (if (string? t) t "???")) texts))
                            (define joiner
                                (let ((j (sexy-send-atomic opts 'with)))
                                    (if (string? j)
                                        j
                                        "")))
                            (cont
                                (if (< l 1)
                                    ""
                                    (string-join strings joiner))))))
                (cons 'debug
                    (lambda (crap)
                        (define stderr (current-error-port))
                        (sexy-write crap stderr)
                        (newline stderr)
                        'null))
                (cons 'FILE_NOT_FOUND 'neither-true-nor-false)
                (cons 'T_PAAMAYIM_NEKUDOTAYIM (quote ::))))
        (fill-prelude primitives)
        prelude)
    (if genv
        genv
        (let ((noob (make-new)))
            (set! genv noob)
            (set! g-has? (sexy-send-env noob 'has? top-cont top-err))
            (set! g-get (sexy-send-env noob 'get top-cont top-err))
            noob)))

(define-syntax import-global-prelude
    (ir-macro-transformer
         (lambda (expr inject compare)
            (define global-prelude-file "global.sxy")
            (define text
                (with-input-from-file global-prelude-file read-string))
            `(define ,(inject 'global-prelude-text) ,text))))

(import-global-prelude)

(define-syntax import-default-symbols.sxy
    (ir-macro-transformer
         (lambda (expr inject compare)
            (define symbols-file "symbols.sxy")
            (define text
                (with-input-from-file symbols-file read-string))
            `(define ,(inject 'symbols.sxy) ,text))))

(import-default-symbols.sxy)

(define (add-global-prelude)
    (define cpath "~/.sexy/global.sxy")
    (define is-cached (file-exists? cpath))
    (define expanded-prelude
        (if is-cached
            (with-input-from-file
                cpath
                (lambda ()
                    (read)))
            (let ((expanded
                    (sexy-expand
                        (sexy-read-file
                            (open-input-string global-prelude-text))
                        (local-env))))
                (with-output-to-file
                    cpath
                    (lambda ()
                        (write expanded)))
                expanded)))
    (define prelude-c
        (sexy-seq-subcontractor expanded-prelude #t))
    (define full
        (prelude-c
                genv
                top-cont
                top-err))
    'null)

(define (sexy-global? x)
    (not (eq? not-found (glookup x))))

(define (lookup env x cont err)
    (sexy-send-env
        env
        'has?
        (lambda (has?)
            (if (has? x)
                (sexy-send-env
                    env
                    'get
                    (lambda (getter)
                        (cont (getter x)))
                    err)
                (sexy-send-env
                    env
                    'mama
                    (lambda (mom)
                        (if (and mom (not (eq? mom 'null)))
                            (lookup mom x cont err)
                            (cont not-found)))
                    err)))
        err))

(define (extend env names vals cont err)
    (define noob (sexy-environment env))
    (define args
        (let loop ((ns names) (vs vals) (yargs '()))
            (if (eq? '() ns)
                yargs
                (loop (cdr ns) (cdr vs) (cons (car ns) (cons (car vs) yargs))))))
    (define params
        (append
            (list
                noob
                (lambda (null) (cont noob))
                err)
            args))
    (apply mutate! params))

(define (mutate! env cont err . args)
    (sexy-send-env
        env
        'def!
        (lambda (def!)
            (apply def! args)
            (cont 'null))
        err))

(define (update! env k v cont err)
    (sexy-send-env
        env
        'has?
        (lambda (has?)
            (if (has? k)
                (sexy-send-env
                    env
                    'def!
                    (lambda (def!)
                        (cont (def! k v)))
                    err)
                (sexy-send-env
                    env
                    'mama
                    (lambda (mom)
                        (if (and mom (not (eq? mom 'null)))
                            (update! mom k v cont err)
                            (cont not-found)))
                    err)))
        err))

(define (glookup x)
    (if (g-has? x)
        (g-get x)
        not-found))

