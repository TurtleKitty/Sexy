
(define (prep-defs seq env cont err)
    ; predefine all defs for mutual recursion
    (define (get-names seq)
        (map
            cadr
            (filter
                (lambda (x)
                    (and (list? x)
                         (> (length x) 1)
                         (or
                            (eq? (car x) 'def)
                            (and (eq? (car x) 'proc) (symbol? (cadr x)))
                            (eq? (car x) 'macro))))
                seq)))
    (define names (get-names seq))
    (define haz? (sexy-send-env env 'has? top-cont top-err))
    (define needed (filter (lambda (n) (not (haz? n)))  names))
    (define margs (flatten (zip needed (make-list (length needed) will-exist))))
    (if (> (length margs) 0)
        (apply mutate! (cons env (cons cont (cons err margs))))
        (cont 'null)))

(define (prepare-sexy-args xs)
    (define (rval args opts)
        (cons (reverse args) (reverse opts)))
    (if (pair? xs)
        (let loop ((head (car xs)) (tail (cdr xs)) (args '()) (opts '()))
            (if (keyword? head)
                (let ((k head) (v (car tail)))
                    (if (pair? (cdr tail))
                        (loop (cadr tail) (cddr tail) args (cons v (cons k opts)))
                        (rval args (cons v (cons k opts)))))
                (if (pair? tail)
                    (loop (car tail) (cdr tail) (cons head args) opts)
                    (rval (cons head args) opts))))
        (rval '() '())))

(define my-empty-record (sexy-record))

(define (prep-options opts)
    (if (= 0 (length opts))
        my-empty-record
        (apply sexy-record
               (map (lambda (k) (if (keyword? k) (keyword->symbol k) k)) opts))))

(define-syntax frag
    (ir-macro-transformer
        (lambda (expr inject compare)
            (let ((body (cdr expr)))
                `(lambda (,(inject 'env) ,(inject 'cont) ,(inject 'err)) ,@body)))))

(define (sexy-compile code)
    (if (and (pair? code) (list? code))
        (case (car code)
            ((def)      (sexy-compile-def code))
            ((quote)    (sexy-compile-quote code))
            ((if)       (sexy-compile-if code))
            ((seq)      (sexy-compile-seq code))
            ((macro)    (sexy-compile-macro code))
            ((λ)        (sexy-compile-lambda code))
            ((proc)     (sexy-compile-proc code))
            ((wall)     (sexy-compile-wall code))
            ((gate)     (sexy-compile-gate code))
            ((capture)  (sexy-compile-capture code))
            ((guard)    (sexy-compile-guard code))
            ((fail)     (sexy-compile-fail code))
            ((use)      (sexy-compile-use code))
            (else       (sexy-compile-application code)))
        (sexy-compile-atom code)))

(define (sexy-compile-atom code)
    (define pass (frag (cont code)))
    (if (symbol? code)
        (if (keyword? code)
            pass
            (case code
                ((true) (frag (cont #t)))
                ((false) (frag (cont #f)))
                ((null) pass)
                ((env) (frag (cont env)))
                (else
                    (if (sexy-global? code)
                        (frag
                            (cont (glookup code)))
                        (frag
                            (lookup
                                env
                                code
                                (lambda (v)
                                    (if (eq? not-found v)
                                        (err (sexy-object `(type error name undefined-symbol form ,code to-text "Name not defined.") #f #f #f) cont)
                                        (cont v)))
                                err))))))
        pass))

(define (sexy-compile-def code)
    (define name (cadr code))
    (define val (caddr code))
    (if (not (symbol? name))
        (sexy-error "def: first argument must be a symbol.  Got " code)
        (frag
            (sexy-send-env env 'has?
                (lambda (haz?)
                    (sexy-send-env env 'get
                        (lambda (getter)
                            (if (and
                                    (haz? name)
                                    (not (eq? will-exist (getter name))))
                                (err (sexy-object `(type error name bad-def form ,code to-text "Name already defined in the local environment.") #f #f #f) cont)
                                (let ((val-c (sexy-compile val)))
                                    (val-c
                                        env
                                        (lambda (v)
                                            (mutate!
                                                env
                                                (lambda (null)
                                                    (cont v))
                                                err
                                                name
                                                v))
                                        err))))
                        err))
                err))))

(define (sexy-compile-quote code)
    (frag
        (cont (cadr code))))

(define (sexy-compile-if code)
    (define pred (sexy-compile (cadr code)))
    (define if-true (sexy-compile (caddr code)))
    (define if-false (sexy-compile (cadddr code)))
    (frag
        (pred
            env
            (lambda (b)
                (sexy-bool
                    b
                    (lambda (is-true)
                        (if is-true
                            (if-true env cont err)
                            (if-false env cont err)))
                    err))
            err)))

(define (sexy-compile-seq code)
    (define seq (cdr code))
    (if (pair? seq)
        (sexy-seq-subcontractor seq #t)
        (sexy-error code "Empty sequences are forbidden!")))

(define (sexy-seq-subcontractor xs prep?)
    (define head (car xs))
    (define tail (cdr xs))
    (let ((head-c (sexy-compile head)))
        (if (pair? tail)
            (let ((tail-c (sexy-seq-subcontractor tail #f)))
                (if prep?
                    (frag
                        (prep-defs
                            xs
                            env
                            (lambda (null)
                                (head-c
                                    env
                                    (lambda (h) (tail-c env cont err))
                                    err))
                            err))
                    (frag
                        (head-c
                            env
                            (lambda (h) (tail-c env cont err))
                            err))))
            head-c)))

(define (check-formals formals)
    (if (pair? formals)
        (let loop ((f (car formals)) (fs (cdr formals)))
            (if (holy? f)
                (begin (sexy-error (blasphemy f)) #f)
                (if (pair? fs)
                    (loop (car fs) (cdr fs))
                    #t)))
        #t))

(define (make-sexy-lambda code env formals body)
    (define arity (length formals))
    (define bodies-c (sexy-seq-subcontractor body #f))
    (if (check-formals formals)
        (let ((p
            (sexy-proc
                code
                env 
                (lambda (args opts cont err)
                    (if (not (= arity (length args)))
                        (err (list 'arity code (sprintf "This lambda requires ~A arguments. Given: " arity) args) cont)
                        (let* ((fargs (if (pair? args) (take args arity) '())))
                               (extend
                                    env 
                                    formals
                                    fargs
                                    (lambda (noob)
                                        (bodies-c noob cont err))
                                    err)))))))
            (hts! p 'type 'λ)
            p)
        (sexy-error 'bad-formals-in-lambda code)))

(define (sexy-compile-lambda code)
    (let ((formals (cadr code)) (bodies (cddr code)))
        (frag
            (cont (make-sexy-lambda code env formals bodies)))))

(define (make-sexy-proc code env formals bodies)
    (define arity (length formals))
    (define bodies-c (sexy-seq-subcontractor bodies #t))
    (if (check-formals formals)
        (sexy-proc
            code
            env 
            (lambda (args opts cont err)
                (if (< (length args) arity)
                    (err (list 'arity code (sprintf "This procedure requires at least ~A arguments. Given: " arity) args) cont)
                    (let* ((fargs (if (pair? args) (take args arity) '()))
                           (the-rest (if (pair? args) (drop args arity) '()))
                           (returner cont))
                           (extend
                                env 
                                (append formals '(opt rest return))
                                (append fargs (list opts the-rest returner))
                                (lambda (noob)
                                    (bodies-c noob cont err))
                                err)))))
        (sexy-error 'bad-formals-in-proc code)))

(define (sexy-compile-proc code)
    (define is-named (symbol? (cadr code)))
    (if is-named
        (sexy-compile `(def ,(cadr code) (proc ,(caddr code) ,@(cdddr code))))
        (let ((formals (cadr code)) (bodies (cddr code)))
            (frag
                (cont (make-sexy-proc code env formals bodies))))))

(define (sexy-compile-macro code)
    (define name (cadr code))
    (define formals (caddr code))
    (define bodies (cdddr code))
    (if (not (symbol? name))
        (sexy-error "macro expects it's first argument to be a symbol.  Got " code)
        (frag
            (sexy-send-env env 'has?
                (lambda (haz?)
                    (sexy-send-env env 'get
                        (lambda (getter)
                            (if (and
                                    (haz? name)
                                    (not (eq? will-exist (getter name))))
                                (err (list 'bad-def code name " is already defined in the local environment.") cont)
                                (let ((thing (make-sexy-proc code env formals bodies)))
                                    (hts! thing 'type 'operator)
                                    (mutate!
                                        env
                                        (lambda (null)
                                            (cont thing))
                                        err
                                        name
                                        thing))))
                        err))
                err))))

(define (sexy-compile-wall code)
    (define args (cadr code))
    (define exprs (cddr code))
    (define expr-c (sexy-seq-subcontractor exprs #t))
    ; create new env and copy args
    (frag
        (define noob (sexy-environment #f))
        (sexy-send noob 'def!
            (lambda (def!)
                (let loop ((travellers args))
                    (if (pair? travellers)
                        (let ((x (car travellers)) (xs (cdr travellers)))
                            (lookup env x
                                (lambda (v)
                                    (def! x v)
                                    (loop xs))
                                err))
                        (expr-c noob cont err))))
                    err)))

(define (sexy-compile-gate code)
    (define exprs (cdr code))
    (define expr-c (sexy-seq-subcontractor exprs #t))
    (frag
        (cont
            (expr-c env identity err))))

(define (sexy-compile-capture code)
    (define name (cadr code))
    (define lamb (cons 'proc (cons (list name) (cddr code))))
    (define lamb-c (sexy-compile lamb))
    (frag
        (lamb-c
            env
            (lambda (funk)
                (sexy-apply
                    funk
                    (list (lambda (k-val) (cont k-val)))
                    'null
                    top-cont
                    err))
            err)))

(define (sexy-compile-guard code)
    (define handler (cadr code))
    (define exprs (cddr code))
    (define handler-c (sexy-compile handler))
    (define expr-c (sexy-seq-subcontractor exprs #t))
    (frag
        (handler-c
            env
            (lambda (handler-fn)
                (define (new-err-cont e k)
                    (sexy-apply handler-fn (list e k) 'null cont err))
                (expr-c env cont new-err-cont))
            err)))

(define (sexy-compile-fail code)
    (define errobj (cadr code))
    (define erob-c (sexy-compile errobj))
    (frag
        (erob-c
            env
            (lambda (e)
                (err e cont))
            err)))

(define (sexy-compile-use code)
    (define name (cadr code))
    (define path (caddr code))
    (define module
        (if (hte? sexy-modules path)
            (htr sexy-modules path)
            (lambda args 'null)))
    (define load-env (local-env))
    (define args-opts (prepare-sexy-args (cddr code)))
    (define args-c (sexy-compile-list (cdar args-opts)))
    (define opts-c (sexy-compile-list (cdr args-opts)))
    (frag 
        (args-c
            env
            (lambda (args)
                (opts-c
                    env
                    (lambda (opts)
                        (module load-env top-cont top-err)
                        (lookup load-env 'sexy-internal-library-export-procedure
                            (lambda (exporter)
                                (if (eq? exporter not-found)
                                    (cont (lambda args 'null))
                                    (sexy-apply
                                        exporter
                                        args
                                        (prep-options opts)
                                        (lambda (v)
                                            (mutate!
                                                env
                                                (lambda (null)
                                                    (cont v))
                                                err
                                                name
                                                v))
                                        err)))
                            err))
                    err))
            err)))

(define (sexy-compile-list xs)
    (if (pair? xs)
        (let ((head (sexy-compile (car xs))) (tail (sexy-compile-list (cdr xs))))
            (frag
                (head
                    env
                    (lambda (h)
                        (tail
                            env
                            (lambda (t) (cont (cons h t)))
                            err))
                    err)))
        (frag (cont '()))))

(define (sexy-compile-application code)
    (define fn-c (sexy-compile (car code)))
    (define args-opts (prepare-sexy-args (cdr code)))
    (define args-c (sexy-compile-list (car args-opts)))
    (define opts-c (sexy-compile-list (cdr args-opts)))
    (frag
        (fn-c
            env
            (lambda (f) 
                (args-c
                    env
                    (lambda (args)
                        (opts-c
                            env
                            (lambda (opts)
                                (sexy-apply f args (prep-options opts) cont err))
                            err))
                    err))
            err)))

