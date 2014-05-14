
; CHICKEN!

(use srfi-1)
(use srfi-69)

(use numbers)
(use posix)
(use uuid)
(use vector-lib)

(define (start)
    (define cmd (string->symbol (car (command-line-arguments))))
    (case cmd
        ((compile) 'niy)
        ((expand) 'niy)
        ((run) 'niy)
        ((repl) (repl))
        (else (printf "Unknown command: ~A~%" cmd))))

(define (identity x) x)

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

(define (thunk? fn)
    (define pinfo (procedure-information fn))
    (and
        (list? pinfo)
        (= 0 (sub1 (length pinfo)))))

(define (idk obj msg)
    (error (list "Message not understood!" obj msg)))

(define (sexy-object args autos resends initial)
    (define this (make-hash-table))
    (define fields (make-hash-table))
    (define delegates (make-hash-table))
    (define autoexec (make-hash-table))
    (define (tset! k v)
        (hash-table-set! this k v))
    (define (fset! k v)
        (hash-table-set! fields k v))
    (define (aset! k)
        (hash-table-set! autoexec k #t))
    (define (rset! k v)
        (hash-table-set! delegates k v))
    (define (set-resend! rlist)
        (let ((delegate (car rlist)) (msgs (cdr rlist)))
            (map (lambda (msg) (rset! msg (sexy-snarf (lambda () (sexy-send delegate msg))))) msgs)))
    (map-pairs fset! args)
    (if resends
        (map set-resend! resends)
        #f)
    (if autos
        (map aset! autos))
    (tset! 'fields fields)
    (tset! 'autos autoexec)
    (tset! 'resends delegates)
    (tset! 'default (or initial (sexy-snarf (lambda (msg) 'null))))
    this)

(define (sexy-proc code env compiled)
    (define this (make-hash-table))
    (define fields (make-hash-table))
    (define my-apply-obj (make-hash-table))
    (define (my-apply-proc xs cont)
        (sexy-apply compiled xs cont))
    (define proc-default
        (define tmp (make-hash-table))
        (hash-table-set! tmp 'exec
            (lambda (args opts cont)
                (error (sprintf "Procedure objects have no method ~A!" (car args))))))
    (define (tset! k v)
        (hash-table-set! this k v))
    (define (fset! k v)
        (hash-table-set! fields k v))
    (define (appl-set! k v)
        (hash-table-set! my-apply-obj k v))
    (define (appl-fset! k v)
        (hash-table-set! (hash-table-ref my-apply-obj 'fields) k v))

    (fset! 'type 'fn)
    (fset! 'null? 'false)
    (fset! 'to-bool 'true)
    (fset! 'env env)
    (fset! 'code code)

    (if (pair? code)
        (let ((formals (car (cdr code))))
            (fset! 'formals formals)
            (fset! 'arity (length formals)))
        'null)

    (fset! 'apply my-apply-obj)

    (appl-set! 'exec my-apply-proc)
    (appl-set! 'fields (make-hash-table))
    (appl-set! 'autos (make-hash-table))
    (appl-set! 'resends (make-hash-table))
    (appl-set! 'default proc-default)
    (appl-fset! 'type 'fn)
    (appl-fset! 'null? 'false)
    (appl-fset! 'to-bool 'true)
    (appl-fset! 'env env)
    (appl-fset! 'code 'compiled)
    (appl-fset! 'arity 2)
    (appl-fset! 'apply 'WAT)
    
    (tset! 'exec compiled)
    (tset! 'fields fields)
    (tset! 'autos (make-hash-table))
    (tset! 'resends (make-hash-table))
    (tset! 'default proc-default)

    this)

(define (sexy-apply proc xs cont)
    (define opts (get-sexy-options xs))
    (define args (remove-sexy-options xs))
    (if (and (hash-table? proc) (eq? 'fn (sexy-send proc 'type)))
        ((hash-table-ref proc 'exec) args opts cont)
        (error (list "Not a procedure!" proc))))

(define (sexy-send obj msg)
    (cond
        ((symbol? obj) (sexy-send-symbol obj msg))
        ((number? obj) (sexy-send-number obj msg))
        ((string? obj) (sexy-send-string obj msg))
        ((pair? obj) (sexy-send-pair obj msg))
        ((hash-table? obj) (sexy-send-obj obj msg))
        ((vector? obj) (sexy-send-vector obj msg))
        ((port? obj) (sexy-send-port obj msg))
        (else (error (list "WTF kind of object was THAT?" obj msg)))))

(define (sexy-send-symbol obj msg)
    (case msg
        ((to-string) (symbol->string obj))
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
        (else (idk obj msg))))

(define (sexy-send-number obj msg)
    (case msg
        ((zero?) (if (eq? obj 0) 'true 'false))
        ((null?) 'false)
        ((to-bool) (if (eq? obj 0) 'false 'true))
        ((to-string) (number->string obj))
        (else
            (cond
                ((integer? obj) (sexy-send-int obj msg))
                ((rational? obj) (sexy-send-rat obj msg))
                ((real? obj) (sexy-send-real obj msg))
                (else (idk obj msg))))))

(define (sexy-send-int obj msg)
    (case msg
        ((type) 'int)
        ((times) 'niy)
        (else (idk obj msg))))
 
(define (sexy-send-rat obj msg)
    (case msg
        ((type) 'rational)
        ((numerator) (numerator obj))
        ((denominator) (denominator obj))
        ((floor) (floor obj))
        ((ceiling) (ceiling obj))
        ((round) (round obj))
        (else (idk obj msg))))

(define (sexy-send-real obj msg)
    (case msg
        ((type) 'real)
        ((floor) (floor obj))
        ((ceiling) (ceiling obj))
        ((round) (round obj))
        (else (idk obj msg))))

(define (sexy-send-string obj msg)
    (case msg
        ((type) 'string)
        ((null?) 'false)
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
        ((to-bool) (if (eq? (length obj) 0) 'false 'true))
        ((head) (car obj))
        ((tail) (cdr obj))
        ((len) (length obj))
        ((has?) (lambda (item) (member item obj)))
        ((map) (lambda (funk) (map funk obj)))
        ((filter) (lambda (funk) (filter funk obj)))
        ((fold) (lambda (init funk) (fold funk init obj)))
        ((foldr) (lambda (init funk) (fold-right funk init obj)))
        (else
            (if (number? msg)
                (list-ref obj msg)
                (idk obj msg)))))

(define (sexy-send-obj obj msg)
    (define fields (hash-table-ref obj 'fields))
    (define resends (hash-table-ref obj 'resends))
    (define autos (hash-table-ref obj 'autos))
    (if (hash-table-exists? fields msg)
        (let ((v (hash-table-ref fields msg)))
            (if (hash-table-exists? autos msg)
                (sexy-apply v '() identity) ; exec the thunk
                v))
        (if (hash-table-exists? resends msg)
            (sexy-apply (hash-table-ref resends msg) '() identity) ; exec the thunk
            (case msg
                ((type) 'obj)
                ((has?) (sexy-snarf (lambda (x) (hash-table-exists? fields x))))
                ((keys) (hash-table-keys fields))
                ((values) (hash-table-values fields))
                ((pairs) (hash-table->alist fields))
                ((clone) 'niy) ; fixme
                ((set!) (sexy-snarf
                            (lambda args
                                (map-pairs
                                    (lambda (k v)
                                        (hash-table-set! fields k v))
                                    args))))
                (else (sexy-apply (hash-table-ref obj 'default) (list msg) identity))))))

(define (sexy-send-vector obj msg)
    (case msg
        ((type) 'vector)
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

(define (sexy-send-port obj msg)
    (case msg
        ((type) 'port)
        ((null?) 'false)
        ((to-bool) 'true)
        ((read) (sexy-snarf
            (lambda () 
                (define blob (read))
                (sexy-parse blob))))
        ((write) (sexy-snarf (lambda (x) (write (sexy-send x 'view) obj))))
        ((print) (sexy-snarf (lambda (x) (display x) (newline))))
        (else (idk msg obj))))

(define (sexy-error form . args)
    (newline)
    (newline)
    (display "ERRORED!!!") (newline)
    (display form) (newline)
    (display args) (newline)
    (newline))


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
        (let* (
            (str (symbol->string x))
            (words (string-split str ".")))
            (let loop ((this (string->symbol (car words))) (left (cdr words)))
                (if (eq? left '())
                    this
                    (loop (list 'send this `(quote ,(string->symbol (car left)))) (cdr left))))))
    (cons match? transform))


; (obj x 1 y 2 meh (lambda (x) (* x 10)) mah (lambda () 7) resend: ((obj2 'foo 'bar) (obj3 'baz)) auto: (mah) default: true)

(define (get-sexy-options xs)
    (define rval (sexy-object '() #f #f #f))
    (if (pair? xs)
        (let loop ((head (car xs)) (tail (cdr xs)) (options rval))
            (if (keyword? head)
                (begin
                    (hash-table-set! (hash-table-ref options 'fields) (string->symbol (keyword->string head)) (car tail))
                    (if (pair? (cdr tail))
                        (loop (cdr tail) (cddr tail) options)
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

(define (sexy-environment parent)
    (define env (sexy-object '() #f #f #f))
    (define (lookup name)
        (if (eq? name 'env)
            this
            (let ((val (sexy-send env name)))
                (if (eq? val 'null)
                    (sexy-send parent name)
                    val))))
    (define (extend names vals)
        (define noob (sexy-environment env))
        (define xs (zip names vals))
        (define setter (sexy-send noob 'set!))
        (define (setme! pr)
            (sexy-apply setter pr identity))
        (map setme! xs)
        noob)
    (define (mutate! name val)
        (sexy-apply (lookup 'set!) (list name val) identity))
    (define (set-null! name)
        (mutate! name 'null))
    (define (prep-defs seq)
        ; predefine all defs for mutual recursion
        (define (get-defs seq)
            (filter (lambda (x) (and (pair? x) (eq? (car x) 'def))) seq))
        (map set-null! (map cadr (get-defs seq))))
    (define (sexy-def code cont)
        (let ((name (car code)) (val (cadr code)))
            (set-null! name)
            (sexy-eval val (lambda (x) (mutate! name x) (cont x)))))
    (define (sexy-quote code cont)
        (cont (car code)))
    (define (sexy-if code cont)
        (let ((pred (car code)) (iftrue (cadr code)) (iffalse (caddr code)))
            (sexy-eval pred
                (lambda (b)
                    (if (eq? (sexy-send b 'to-bool) 'true)
                        (sexy-eval iftrue cont)
                        (sexy-eval iffalse cont))))))
    (define (sexy-seq code cont)
        (if (pair? code)
            (begin
                ; predefine all defs for mutual recursion
                (map set-null! (map cadr (filter (lambda (x) (and (pair? x) (eq? (car x) 'def))) (cdr code))))
                (let ((head (car code)) (tail (cdr code)))
                    (if (pair? tail)
                        (sexy-eval head
                            (lambda (h) (sexy-seq tail cont)))
                        (sexy-eval head cont))))
            (cont 'null)))
    (define (sexy-set! code cont)
        (let ((name (car code)) (val (cadr code)))
            (if (symbol? name)
                (if ((sexy-send env 'has?) name)
                    (sexy-eval val (lambda (v) (cont (begin (mutate! name v) 'null))))
                    (error (list "Unknown name" name)))
                (error "set! wants a symbol!"))))
    (define (sexy-fn code cont)
        (let* ((formals (car code)) (bodies (cdr code)) (flen (length formals)))
            ; what do do about opts and rest?
            (cont
                (sexy-proc
                    (cons 'fn code)
                    env 
                    (lambda (args opts kont)
                        (define fargs (if (pair? args) (take args flen) '()))
                        (define the-rest (if (pair? args) (drop args flen) '()))
                        (define noob
                            (sexy-apply
                                (sexy-send this 'extend)
                                (list
                                    (append formals '(opt rest))
                                    (append fargs (list opts the-rest)))
                                identity))
                        ((sexy-send noob 'eval) (cons 'seq bodies) kont))))))
    (define (sexy-eval-list xs cont)
        (if (pair? xs)
            (sexy-eval (car xs)
                (lambda (v) (sexy-eval-list (cdr xs)
                    (lambda (t) (cont (cons v t))))))
            (sexy-eval xs cont)))
    (define (sexy-eval code cont)
        (if (atom? code)
            (if (symbol? code)
                (case code
                    ((true false null) (cont code))
                    (else
                        (if (keyword? code)
                            (cont code)
                            (cont (lookup code)))))
                (cont code))
            (let ((head (car code)) (tail (cdr code)))
                (case head
                    ((def) (sexy-def tail cont))
                    ((quote) (sexy-quote tail cont))
                    ((if) (sexy-if tail cont))
                    ((seq) (begin (prep-defs tail) (sexy-seq tail cont)))
                    ((set!) (sexy-set! tail cont))
                    ((fn) (sexy-fn tail cont))
                    (else (sexy-eval head
                        (lambda (f) 
                            (sexy-eval-list tail
                                (lambda (args) (sexy-apply f args cont))))))))))
    (define this
        (sexy-object
            (list 'type 'env
                  'lookup (sexy-snarf lookup)
                  'extend (sexy-snarf extend)
                  'eval (lambda (args opts cont) (sexy-eval (car args) cont))
                  'parent parent)
            #f
            (list (list env 'set! 'has? 'keys))
            #f))
        this)

(define (sexy-snarf proc)
    (define compiled
        (lambda (args opts cont)
            (cont (apply proc args))))
    (sexy-proc 'compiled 'global compiled))

(define (global-env)
    (define (nodef x)
        (error (list "Symbol not defined" x)))
    (define toplevel
        (sexy-object (list 'lookup nodef) #f #f #f))
    (define prelude
        (sexy-environment toplevel))
    (define (preset! k v)
        (sexy-apply (sexy-send prelude 'set!) (list k v) identity))
    (define (fill-prelude fs)
        (define (setem! p)
            (preset! (car p) (cdr p)))
        (map setem! fs))
    (define snarfs
        (map (lambda (x) (cons x (sexy-snarf (eval x))))
            '(+ - * / > >= < <= eq? equal? list vector)))
    (define primitives
        (list
            (cons 'send (sexy-snarf sexy-send))
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
    prelude)


(define (repl)
    (define stdin (current-input-port))
    (define stdout (current-output-port))
    (define stderr (current-error-port))
    (define env (global-env))
    (define (loop)
        (display "(sexy) ")
        (sexy-apply
            (sexy-send stdin 'read)
            '()
            (lambda (expr)
                ((sexy-send env 'eval)
                    (list expr)
                    (sexy-object '() #f #f #f)
                    (lambda (v)
                        (sexy-apply
                            (sexy-send stdout 'print)
                            (list v)
                            (lambda (null) (loop))))))))
    (loop))

; (start)


