
; CHICKEN!

(use srfi-1)
(use srfi-69)

(use numbers)
(use posix)
(use utils)
(use uuid)
(use vector-lib)


; start

(define usage-text #<<END

Usage:

sexy repl
sexy run <filename>
sexy check <filename>
sexy compile <filename>
sexy expand <filename>

END
)

(define (usage)
    (display usage-text)
    (newline))

(define (niy)
    (newline)
    (display "Umm... that's not done yet.")
    (newline)
    (newline))

(define (start)
    (define args (command-line-arguments))
    (define (check-file)
        (if (pair? (cdr args))
            (let ((file (cadr args)))
               (if (file-exists? file)
                   (open-input-file file) 
                   (debug "File not found!")))
            (usage)))
    (define (setup-run)
        (define fport (check-file))
        (if (port? fport)
            (sexy-run fport)
            (exit)))
    (if (not (pair? args))
        (usage)
        (let ((cmd (string->symbol (car args))))
            (case cmd
                ((run) (setup-run))
                ((repl) (sexy-repl))
                ((check) (niy))
                ((compile) (niy))
                ((expand) (niy))
                (else (printf "Unknown command: ~A~%" cmd))))))


; utils

(define mkht make-hash-table)
(define htr hash-table-ref)
(define hte? hash-table-exists?)
(define hts! hash-table-set!)

(define (debug x)
    (display x) (newline))

(define (map-pairs fn args)
    (if (not (eq? (modulo (length args) 2) 0))
        (error (list "map-pairs requires an even number of arguments!" args))
        (let loop ((newlist '()) (pairs args))
            (if (atom? pairs)
                newlist
                (let ((key (first pairs)) (val (second pairs)))
                    (loop (fn key val) (cddr pairs)))))))

(define (idk obj msg)
    (sexy-error `(send ,obj ,msg) "Message not understood!"))

(define (sexy-error form . args)
    (newline)
    (display "ERRORED!!!") (newline)
    (display (sexy-view form)) (newline)
    (display args) (newline)
    (newline)
    'null)

(define (sexy-view obj)
    (sexy-send obj 'view))

; mini-parser

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
        (and (symbol? x)
             (string-contains (symbol->string x) ".")))
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

(define (get-sexy-options xs)
    ; fixme - keyword corner case
    (define rval (sexy-object '() #f #f #f))
    (if (pair? xs)
        (let loop ((head (car xs)) (tail (cdr xs)) (options rval))
            (if (keyword? head)
                (begin
                    (hts! (htr options 'fields) (string->symbol (keyword->string head)) (car tail))
                    (if (pair? (cdr tail))
                        (loop (cadr tail) (cddr tail) options)
                        options))
                (if (pair? tail)
                    (loop (car tail) (cdr tail) options)
                    options)))
        rval))

(define (remove-sexy-options xs)
    (if (pair? xs)
        (let loop ((head (car xs)) (tail (cdr xs)) (argv '()))
            (if (keyword? head)
                (if (pair? (cdr tail))
                    (loop (car (cdr tail)) (cddr tail) argv)
                    (reverse argv))
                (if (pair? tail)
                    (loop (car tail) (cdr tail) (cons head argv))
                    (reverse (cons head argv)))))
        '()))

(define (bool-fixer op)
    (lambda (x y)
        (if (op x y)
            'true
            'false)))

(define (transbool x)
    (if x
        'true
        'false))

; sexy objects

(define (sexy-object args autos resends initial)
    ; (obj x 1 y 2 meh (lambda (x) (* x 10)) mah (lambda () 7) resend: ((obj2 'foo 'bar) (obj3 'baz)) auto: (mah) default: true)
    (define this (mkht))
    (define fields (mkht))
    (define delegates (mkht))
    (define autoexec (mkht))
    (define (tset! k v)
        (hts! this k v))
    (define (fset! k v)
        (hts! fields k v))
    (define (aset! k)
        (hts! autoexec k #t))
    (define (rset! k v)
        (hts! delegates k v))
    (define (set-resend! rlist)
        (let ((delegate (car rlist)) (msgs (cdr rlist)))
            (map (lambda (msg) (rset! msg (lambda () (sexy-send delegate msg)))) msgs)))
    (map-pairs fset! args)
    (if resends
        (map set-resend! resends)
        #f)
    (if autos
        (map aset! autos))
    (tset! 'type 'obj)
    (tset! 'fields fields)
    (tset! 'autos autoexec)
    (tset! 'resends delegates)
    (tset! 'default (or initial (lambda (msg) 'null)))
    this)

(define (sexy-proc code env compiled)
    (define this (mkht))
    (define (tset! k v) (hts! this k v))
    (tset! 'type 'fn)
    (tset! 'env env)
    (tset! 'code code)
    (tset! 'exec compiled)
    (if (pair? code)
        (let ((formals (cadr code)))
            (tset! 'formals formals)
            (tset! 'arity (length formals)))
        (begin
            (tset! 'formals 'null)
            (tset! 'arity 'null)))
    this)

(define (sexy-environment parent)
    (define this (mkht))
    (define vars (mkht))
    (define (def! name val)
        (hts! vars name val)
        val)
    (define (set-var! name val)
        (if (eq? ((sexy-send this 'has?) name) 'true)
            (if (hte? vars name)
                (hts! vars name val)
                ((sexy-send parent 'set!) name val))
            (sexy-error 'set-var! "Environment has no definition for " name))
        val)
    (define (lookup name)
        (if (eq? name 'env)
            (reify-env this)
            (if (hte? vars name) 
                (htr vars name)
                (if parent
                    ((sexy-send parent 'lookup) name)
                    'null))))
    (define (extend names vals)
        (define noob (sexy-environment this))
        (define xs (zip names vals))
        (define setter! (sexy-send noob 'def!))
        (define (setme! pr)
            (setter! (car pr) (cadr pr)))
        (map setme! xs)
        noob)
    (define (my-eval code)
        (sexy-eval code this identity))
    (hts! this 'type 'env)
    (hts! this 'vars vars)
    (hts! this 'def! def!)
    (hts! this 'set! set-var!)
    (hts! this 'lookup lookup)
    (hts! this 'extend extend)
    (hts! this 'eval my-eval)
    (hts! this 'parent (if parent (reify-env parent) 'null))
    this)


; message passing

(define (sexy-send obj msg)
    (cond
        ((symbol? obj) (sexy-send-symbol obj msg))
        ((number? obj) (sexy-send-number obj msg))
        ((string? obj) (sexy-send-string obj msg))
        ((null? obj) (sexy-send-null obj msg))
        ((pair? obj) (sexy-send-pair obj msg))
        ((procedure? obj) (sexy-send-primitive obj msg))
        ((vector? obj) (sexy-send-vector obj msg))
        ((port? obj) (sexy-send-port obj msg))
        ((hash-table? obj)
            (let ((t (htr obj 'type)))
                (case t
                    ((obj) (sexy-send-obj obj msg))
                    ((fn)  (sexy-send-fn obj msg))
                    ((env)  (sexy-send-env obj msg)))))
        ((eof-object? obj) (newline) (newline) (exit))
        (else (error (list "WTF kind of object was THAT?" obj msg)))))

(define (sexy-send-symbol obj msg)
    (case msg
        ((to-string) (symbol->string obj))
        ((view) obj)
        (else
            (case obj
                ((true false) (sexy-send-bool obj msg))
                ((null) (sexy-send-null obj msg))
                (else
                    (case msg
                        ((type) 'symbol)
                        ((null?) 'false)
                        ((to-bool) 'true)
                        (else (idk obj msg))))))))

(define (sexy-send-bool obj msg)
    (case msg
        ((type) 'bool)
        ((null?) 'false)
        ((to-bool) obj)
        (else (idk obj msg))))

(define (sexy-send-null obj msg)
    (case msg
        ((type) 'null)
        ((null?) 'true)
        ((to-bool) 'false)
        (else 'null)))

(define (sexy-send-number obj msg)
    (case msg
        ((zero?) (if (eq? obj 0) 'true 'false))
        ((null?) 'false)
        ((to-bool) (if (eq? obj 0) 'false 'true))
        ((to-string) (number->string obj))
        ((view) obj)
        (else
            (cond
                ((integer? obj) (sexy-send-int obj msg))
                ((real? obj) (sexy-send-real obj msg))
                (else (idk obj msg))))))

(define (sexy-send-int obj msg)
    (case msg
        ((type) 'int)
        ((times) 'niy)
        (else (idk obj msg))))
 
(define (sexy-send-real obj msg)
    (case msg
        ((type) 'real)
        ((floor) (floor obj))
        ((ceil) (ceiling obj))
        ((round) (round obj))
        (else (idk obj msg))))

(define (sexy-send-string obj msg)
    (case msg
        ((type) 'string)
        ((null?) 'false)
        ((view) obj)
        ((to-bool) (if (eq? (string-length obj) 0) 'false 'true))
        ((to-symbol) (string->symbol obj))
        ((to-number) (string->number obj))
        ((join) 'niy)
        ((split) 'niy)
        (else (idk obj msg))))

(define (sexy-send-pair obj msg)
    (case msg
        ((type) 'pair)
        ((null?) 'false)
        ((view) obj)
        ((to-bool) (if (eq? (length obj) 0) 'false 'true))
        ((head) (car obj))
        ((tail) (cdr obj))
        ((len) (length obj))
        ((has?) (lambda (item) (member item obj)))
        ((map) (lambda (funk) (map funk obj)))
        ((filter) (lambda (funk) (filter funk obj)))
        ((fold) (lambda (init funk) (fold funk init obj)))
        ((foldr) (lambda (init funk) (fold-right funk init obj)))
        ((sort) (lambda (funk) (sort obj funk)))
        (else
            (if (number? msg)
                (list-ref obj msg)
                (idk obj msg)))))

(define (sexy-send-primitive obj msg)
    (case msg
        ((type) 'fn)
        ((null?) 'false)
        ((view) 'compiled)
        ((to-bool) 'true)
        ((env) 'global)
        ((code) 'compiled)
        ((arity) (let ((pinfo (procedure-information obj)))
            (if (list? pinfo)
                (sub1 (length pinfo))
                '*)))
        ((apply)
            (lambda (args)
                (sexy-apply obj args identity)))))

(define (sexy-send-obj obj msg)
    (define fields (htr obj 'fields))
    (define resends (htr obj 'resends))
    (define autos (htr obj 'autos))
    (if (hte? fields msg)
        (let ((v (htr fields msg)))
            (if (hte? autos msg)
                (sexy-apply v '() identity) ; exec the thunk
                v))
        (if (hte? resends msg)
            (sexy-apply (htr resends msg) '() identity) ; exec the thunk
            (case msg
                ((type) 'obj)
                ((null?) 'false)
                ((view) (hash-table->alist fields))
                ((to-bool) (if (eq? 0 (length (hash-table-keys fields))) 'false 'true))
                ((apply?) (lambda (args cont) (cont (sexy-send obj msg))))
                ((has?) (lambda (x) (transbool (hte? fields x))))
                ((keys) (hash-table-keys fields))
                ((values) (hash-table-values fields))
                ((pairs) (hash-table->alist fields))
                ((clone) 'niy)
                ((set!) 
                    (lambda args
                        (map-pairs
                            (lambda (k v)
                                (hts! fields k v))
                            args)
                        'null))
                (else (sexy-apply (htr obj 'default) (list msg) identity))))))

(define (sexy-send-fn obj msg)
    (case msg
        ((type) 'fn)
        ((null?) 'false)
        ((view) (sexy-send obj 'code))
        ((to-bool) 'true)
        ((env) (reify-env (htr obj 'env)))
        ((code arity) (htr obj msg))
        ((apply)
            (lambda (args)
                (sexy-apply obj args identity)))
        (else (idk obj msg))))

(define (sexy-send-env obj msg)
    (define vars (htr obj 'vars))
    (case msg
        ((type) 'env)
        ((null?) 'false)
        ((view) (hash-table->alist vars))
        ((to-bool) 'true)
        ((has?)
            (lambda (x)
                (sexy-bool ((sexy-send obj 'lookup) x))))
        ((local?)
            (lambda (x)
                (if (hte? vars x)
                    (let ((v (htr vars x)))
                        (if (eq? v 'null)
                            'false
                            'true))
                    'false)))
        ((vars) (hash-table->alist vars))
        (else
            (if (hte? obj msg)
                (htr obj msg)
                (idk obj msg)))))

(define (sexy-send-vector obj msg)
    (case msg
        ((type) 'vector)
        ((null?) 'false)
        ((view) obj)
        ((to-bool) (if (eq? (vector-length obj) 0) 'false 'true))
        ((len) (vector-length obj))
        ((has?) (lambda (item) (member item obj)))
        ((map) (lambda (funk) (vector-map funk obj)))
        ((filter) (lambda (funk) (filter funk obj)))
        ((fold) (lambda (funk init) (fold funk init obj)))
        ((foldr) (lambda (funk init) (fold-right funk init obj)))
        (else
            (if (number? msg)
                (vector-ref obj msg)
                (idk obj msg)))))

(define (sexy-read port)
    (sexy-parse (read port)))

(define (sexy-write obj port)
    (write (sexy-view obj)))

(define (sexy-send-port obj msg)
    (case msg
        ((type) 'port)
        ((null?) 'false)
        ((to-bool) 'true)
        ((view) obj)
        ((read) (lambda () (sexy-read obj)))
        ((write) (lambda (x) (sexy-write x obj) 'null))
        ((print) (lambda (x) (sexy-write x obj) (newline) 'null))
        (else (idk msg obj))))

(define (sexy-bool obj)
    (sexy-send obj 'to-bool))

; eval/apply

(define (sexy-eval code env cont)
    (define (nodef x)
        (sexy-error x "Symbol " x " is not defined"))
    (if (atom? code)
        (if (symbol? code)
            (if (keyword? code)
                (cont code)
                (case code
                    ((true false null) (cont code))
                    ((env) (cont (reify-env env)))
                    (else
                        (let ((looked-up ((sexy-send env 'lookup) code)))
                            (if (eq? 'null looked-up)
                                (nodef code)
                                (cont looked-up))))))
            (cont code))
        (case (car code)
            ((def) (sexy-eval-def code env cont))
            ((quote) (sexy-eval-quote code env cont))
            ((if) (sexy-eval-if code env cont))
            ((seq) (begin (prep-defs (cdr code) env) (sexy-eval-seq code env cont)))
            ((set!) (sexy-eval-set! code env cont))
            ((fn) (sexy-eval-fn code env cont))
            ((wall) (cont (sexy-eval (caddr code) env identity)))
            (else
                (sexy-eval
                    (car code)
                    env
                    (lambda (f) 
                        (sexy-eval-list
                            (cdr code)
                            env
                            (lambda (args) (sexy-apply f args cont)))))))))

(define (sexy-apply obj xs cont)
    (define opts (get-sexy-options xs))
    (define args (remove-sexy-options xs))
    (define (send-or-die)
        (if (pair? args)
            (cont (sexy-send obj (car args)))
            (sexy-error `((,obj) => (send ,obj)) "send requires a message.")))
    (cond
        ((procedure? obj) (cont (apply obj args)))
        ((or (pair? obj) (vector? obj) (string? obj)) (send-or-die))
        ((hash-table? obj)
            (if (eq? 'fn (htr obj 'type))
                ((htr obj 'exec) args opts cont)
                (send-or-die)))
        (else (sexy-error obj (list obj " is not applicable!")))))

(define (prep-defs seq env)
    ; predefine all defs for mutual recursion
    (define mutate!
        (sexy-send env 'def!))
    (define (set-null! name)
        (mutate! name 'null))
    (define (get-defs seq)
        (filter (lambda (x) (and (pair? x) (eq? (car x) 'def))) seq))
    (map set-null! (map cadr (get-defs seq))))

(define (sexy-eval-def code env cont)
    (define mutate!
        (sexy-send env 'def!))
    (define (set-null! name)
        (mutate! name 'null))
    (let ((name (cadr code)) (val (caddr code)))
        (if (eq? 'true ((sexy-send env 'local?) name))
            (sexy-error code name " is already defined in the local environment.")
            (begin
                (set-null! name)
                (sexy-eval val env (lambda (v) (mutate! name v) (cont v)))))))

(define (sexy-eval-quote code env cont)
    (cont (cadr code)))

(define (sexy-eval-if code env cont)
    (let ((pred (cadr code)) (iftrue (caddr code)) (iffalse (cadddr code)))
        (sexy-eval
            pred
            env
            (lambda (b)
                (if (eq? (sexy-bool b) 'true)
                    (sexy-eval iftrue env cont)
                    (sexy-eval iffalse env cont))))))

(define (sexy-eval-seq code env cont)
    (define seq (cdr code))
    (define (subcontractor xs env cont)
        (if (pair? xs)
            (let ((head (car xs)) (tail (cdr xs)))
                (if (pair? tail)
                    (sexy-eval
                        head
                        env
                        (lambda (h) (subcontractor tail env cont))))
                (sexy-eval head env cont))
            (sexy-error code "Empty sequences are forbidden!")))
    (subcontractor seq env cont))

(define (sexy-eval-set! code env cont)
    (let ((name (cadr code)) (val (caddr code)))
        (if (symbol? name)
            (if ((sexy-send env 'has?) name)
                (sexy-eval
                    val
                    env
                    (lambda (v) ((sexy-send env 'set!) name v) (cont 'null)))
                (sexy-error code "Unknown name" name))
            (sexy-error code "set! wants a symbol!"))))

(define (sexy-eval-fn code env cont)
    (let* ((formals (cadr code)) (bodies (cddr code)) (arity (length formals)))
        (cont
            (sexy-proc
                code
                env 
                (lambda (args opts kont)
                    (if (< (length args) arity)
                        (sexy-error code (sprintf "Procedure requires ~A arguments!" arity))
                        (let* ((fargs (if (pair? args) (take args arity) '()))
                               (the-rest (if (pair? args) (drop args arity) '()))
                               (noob
                                   ((sexy-send env 'extend)
                                        (append formals '(opt rest))
                                        (append fargs (list opts the-rest)))))
                            (sexy-eval (cons 'seq bodies) noob kont))))))))

(define (sexy-eval-list xs env cont)
    (if (pair? xs)
        (sexy-eval
            (car xs)
            env
            (lambda (v)
                (sexy-eval-list
                    (cdr xs)
                    env
                    (lambda (t) (cont (cons v t))))))
        (cont '())))


; reflection

(define (reify-env env)
    env)

(define (reify-cont cont)
    (cons cont 'fixme))


; setup 

(define (global-env)
    (define prelude
        (sexy-environment #f))
    (define (preset! k v)
        (sexy-apply
            (sexy-send prelude 'def!)
            (list k v)
            identity))
    (define (fill-prelude fs)
        (define (setem! p)
            (preset! (car p) (cdr p)))
        (map setem! fs))
    (define (istrue x)
        (eq? 'true (sexy-bool x)))
    (define snarfs
        (map (lambda (x) (cons x (eval x)))
            '(+ - * / cons list vector)))
    (define primitives
        (list
            (cons 'stdin (current-input-port))
            (cons 'stdout (current-output-port))
            (cons 'stderr (current-error-port))
            (cons 'div quotient)
            (cons 'rem remainder)
            (cons 'mod modulo)
            (cons 'is? (bool-fixer eq?))
            (cons 'eq? (bool-fixer equal?))
            (cons '= (bool-fixer equal?))
            (cons '> (bool-fixer >))
            (cons '>= (bool-fixer >=))
            (cons '< (bool-fixer <))
            (cons '<= (bool-fixer <=))
            (cons 'and?
                (lambda args
                    (let loop ((a (car args)) (xs (cdr args)))
                        (if (istrue a)
                            (if (pair? xs)
                                (loop (car xs) (cdr xs))
                                'true)
                            'false))))
            (cons 'or?
                (lambda args
                    (let loop ((a (car args)) (xs (cdr args)))
                        (if (istrue a)
                            'true
                            (if (pair? xs)
                                (loop (car xs) (cdr xs))
                                'false)))))
            (cons 'not
                (lambda (b)
                    (if (istrue b)
                        'false
                        'true))) 
            (cons 'send sexy-send)
            (cons 'test
                (lambda (tname ok)
                    (debug (list tname (if (eq? ok 'true) 'ok 'FAIL))) 'null))
            (cons 'show
                (lambda (x)
                    (sexy-write x (current-output-port))
                    (newline)
                    x))
            (cons 'obj
                (sexy-proc
                    'compiled
                    'global
                    (lambda (args opts cont)
                        (define autos (sexy-send opts 'auto))
                        (define rsend (sexy-send opts 'resend))
                        (define default (sexy-send opts 'default))
                        (if (eq? autos 'null) (set! autos #f) #f)
                        (if (eq? rsend 'null) (set! rsend #f) #f)
                        (if (eq? default 'null) (set! default #f) #f)
                        (cont (sexy-object args autos rsend default)))))))
    (fill-prelude (append snarfs primitives))
    (hts! (htr prelude 'vars) 'stdin (current-input-port))
    (hts! (htr prelude 'vars) 'stdout (current-output-port))
    (hts! (htr prelude 'vars) 'stderr (current-error-port))
    prelude)

(define (sexy-run port)
    (define program
        (let loop ((noob (sexy-read port)) (code '()))
            (if (eof-object? noob)
                (cons 'seq (reverse code))
                (loop (sexy-read port) (cons noob code)))))
    (close-input-port port)
    ;(debug program)
    (sexy-eval
        program
        (global-env)
        (lambda (v) (exit))))

(define (sexy-repl)
    (define stdin (current-input-port))
    (define stdout (current-output-port))
    (define stderr (current-error-port))
    (define genv (global-env))
    (define (loop env)
        (display "(sexy) ")
        (sexy-apply
            (sexy-send stdin 'read)
            '()
            (lambda (expr)
                (sexy-eval
                    expr
                    env
                    (lambda (v)
                        (sexy-apply
                            (sexy-send stdout 'print)
                            (list v)
                            (lambda (null) (loop env))))))))
    (newline)
    (display "Welcome to the Sexy Read-Eval-Print Loop.  Press Ctrl-D to exit.")
    (newline)
    (newline)
    (loop genv))

(start)


