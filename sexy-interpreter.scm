
; CHICKEN!

(use srfi-1)
(use srfi-13)
(use srfi-69)

(use numbers)
(use posix)
(use utils)
;(use uuid)
(use vector-lib)

;(use openssl)
(use http-client)

(declare
    (block)
    (inline)
    (local)
;    (unsafe)
)


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

(define top-cont identity)
(define top-err  (lambda (ex continue) (sexy-error "Uncaught error: " ex)))

(define (start)
    (define args (command-line-arguments))
    (define (check-file)
        (if (pair? (cdr args))
            (let ((file (cadr args)))
               (if (file-exists? file)
                   (open-input-file file) 
                   (debug "File not found!")))
            (usage)))
    (define (read-prog)
        (define fport (check-file))
        (if (port? fport)
            (sexy-read-file fport)
            (exit)))
    (global-env)
    (add-global-prelude)
    (if (not (pair? args))
        (usage)
        (let ((cmd (string->symbol (car args))))
            (case cmd
                ((run)
                    (sexy-run
                        (sexy-expand (read-prog) (local-env))))
                ((repl) (sexy-repl))
                ((check) (niy))
                ((compile)
                    (sexy-compile
                        (sexy-expand (read-prog) (local-env))))
                ((expand)
                    (sexy-write
                        (sexy-expand (read-prog) (local-env))
                        (current-output-port))
                        (newline))
                (else (printf "Unknown command: ~A~%" cmd))))))


; utils

(define mkht make-hash-table)
(define htr hash-table-ref)
(define hte? hash-table-exists?)
(define hts! hash-table-set!)

(define (nop v) 'null)

(define (debug . xs)
    (display xs) (newline))

(define (debug-obj x)
    (define ps (hash-table->alist x))
    (map debug ps))

(define (for-pairs fn args)
    (if (not (eq? (modulo (length args) 2) 0))
        (error (list "for-pairs requires an even number of arguments!" args))
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
    identity)

(define (sexy-view obj)
    (sexy-send obj 'view))

(define (bool-fixer op)
    (lambda args
        (if (apply op args)
            'true
            'false)))

(define (eq-fixer op)
    (lambda args
        (if (all? null-or-empty? args)
            'true
            (apply op args))))

(define (transbool x)
    (if x
        'true
        'false))

(define (unbool x)
    (if (eq? (sexy-bool x) 'true)
        #t
        #f))

(define (nodef x)
    (sexy-error x "Symbol " x " is not defined"))

(define (null-or-empty? x)
    (or (eq? x 'null) (eq? x '())))

(define (all? funk xs)
    (let loop ((y (car xs)) (ys (cdr xs)))
        (if (funk y)
            (if (pair? ys)
                (loop (car ys) (cdr ys))
                #t)
            #f)))

(define (get-uri uri)
    (define (reader port)
        (read-string #f port))
    (call-with-input-request
        uri
        #f
        reader))

(define (get-file fname)
    (define (reader port)
        (read-string #f port))
    (call-with-input-file
        fname
        reader))


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

(define (prepare-sexy-args xs)
    (define (rval args opts)
        (cons (reverse args) (cons 'rec opts)))
    (if (pair? xs)
        (let loop ((head (car xs)) (tail (cdr xs)) (args '()) (options '()))
            (if (keyword? head)
                (let ((k (string->symbol (keyword->string head))) (v (car tail)))
                    (if (pair? (cdr tail))
                        (loop (cadr tail) (cddr tail) args (cons (cons k v) options))
                        (rval args (cons (cons k v) options))))
                (if (pair? tail)
                    (loop (car tail) (cdr tail) (cons head args) options)
                    (rval (cons head args) options))))
        (rval '() '())))

(define (prettify-alist al)
    (define (transmute p)
        (list (car p) ': (cdr p)))
    (map transmute al))

; sexy objects

(define (sexy-record args)
    (if (null? args)
        (cons 'rec '())
        (let loop ((k (car args)) (v (list-ref args 1)) (this '()) (rest (cddr args)))
            (if (null? rest)
                (cons 'rec (cons (cons k v) this))
                (loop (list-ref args 2) (list-ref args 3) (cons (cons k v) this) (cddr rest))))))

(define (sexy-object args autos resends initial)
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
    (for-pairs fset! args)
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
            (tset! 'arity 0)))
    this)

(define (sexy-environment mama)
    (define this (mkht))
    (define vars (mkht))
    (hts! this 'type 'env)
    (hts! this 'vars vars)
    (if mama
        (hts! this 'mama mama)
        #f)
    this)


; message passing

(define (sexy-record? x)
    (and (pair? x) (eq? 'rec (car x))))

(define (sexy-send obj msg)
    (cond
        ((symbol? obj) (sexy-send-symbol obj msg))
        ((number? obj) (sexy-send-number obj msg))
        ((string? obj) (sexy-send-string obj msg))
        ((null? obj) (sexy-send-empty obj msg))
        ((sexy-record? obj) (sexy-send-record obj msg))
        ((pair? obj) (sexy-send-pair obj msg))
        ((procedure? obj) (sexy-send-primitive obj msg))
        ((vector? obj) (sexy-send-vector obj msg))
        ((port? obj) (sexy-send-port obj msg))
        ((hash-table? obj)
            (let ((t (htr obj 'type)))
                (case t
                    ((obj) (sexy-send-obj obj msg))
                    ((fn)  (sexy-send-fn obj msg))
                    ((macro)  (sexy-send-macro obj msg))
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

(define (sexy-send-empty obj msg)
    (case msg
        ((type) 'pair)
        ((null?) 'false)
        ((empty?) 'true)
        ((to-bool) 'false)
        ((head tail) 'null)
        ((size) 0)
        (else (sexy-send-pair obj msg))))

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
        ((empty?) 'false)
        ((view)
            (if (list? obj)
                (map sexy-view obj)
                (cons (sexy-view (car obj)) (sexy-view (cdr obj)))))
        ((to-bool) 'true)
        ((to-vector) (list->vector obj))
        ((head) (car obj))
        ((tail) (cdr obj))
        ((size) (length obj))
        ((reverse) (reverse obj))
        ((has?) (lambda (item) (transbool (member item obj))))
        ((append) (lambda (other) (append obj other)))
        ((fold)
            (lambda (init funk)
                (fold (sexy-apply-wrapper funk) init obj)))
        ((foldr)
            (lambda (init funk)
                (fold-right (sexy-apply-wrapper funk) init obj)))
        ((map)
            (lambda (funk)
                (map (sexy-apply-wrapper funk) obj)))
        ((filter)
            (lambda (funk)
                (filter
                    (lambda (x)
                        (unbool ((sexy-apply-wrapper funk) x)))
                    obj)))
        ((sort)
            (lambda (funk)
                (sort
                    obj
                    (lambda (x y)
                        (unbool ((sexy-apply-wrapper funk) x y))))))
        (else
            (if (number? msg)
                (list-ref obj msg)
                (idk obj msg)))))

(define (sexy-send-primitive obj msg)
    (case msg
        ((type) 'fn)
        ((null?) 'false)
        ((view code) 'primitive-function)
        ((to-bool) 'true)
        ((env) 'global)
        ((arity) (let ((pinfo (procedure-information obj)))
            (if (list? pinfo)
                (sub1 (length pinfo))
                '*)))
        ((apply)
            (lambda (args)
                (apply (sexy-apply-wrapper obj) args)))))

(define (sexy-send-record obj msg)
    (define al (cdr obj))
    (case msg
        ((type) 'rec)
        ((null?) 'false)
        ((view)
            (if al
                (prettify-alist al)
                '()))
        ((to-bool)
            (transbool (pair? al)))
        ((apply)
            (lambda (args cont err)
                (cont (sexy-send obj (car args)))))
        ((has?)
            (lambda (x)
                (transbool (assoc x al))))
        ((keys) (map car al))
        ((values) (map cdr al))
        ((pairs) al)
        ((clone) 'niy)
        (else
            (begin
                (let ((p (assoc msg al)))
                    (if p
                        (cdr p)
                        'null))))))

(define (sexy-send-obj obj msg)
    (define fields (htr obj 'fields))
    (define resends (htr obj 'resends))
    (define autos (htr obj 'autos))
    (if (hte? fields msg)
        (let ((v (htr fields msg)))
            (if (hte? autos msg)
                ((sexy-apply-wrapper v)) ; exec the thunk
                v))
        (if (hte? resends msg)
            ((sexy-apply-wrapper (htr resends msg))) ; exec the thunk
            (case msg
                ((type) 'obj)
                ((null?) 'false)
                ((view)
                    (sexy-view
                        (prettify-alist (hash-table->alist fields))))
                ((to-bool) (if (eq? 0 (length (hash-table-keys fields))) 'false 'true))
                ((apply) (lambda (args cont err) (cont (sexy-send obj (car args)))))
                ((has?) (lambda (x) (transbool (hte? fields x))))
                ((keys) (hash-table-keys fields))
                ((values) (hash-table-values fields))
                ((pairs) (hash-table->alist fields))
                ((clone) 'niy)
                ((set!) 
                    (lambda args
                        (for-pairs
                            (lambda (k v)
                                (hts! fields k v))
                            args)
                        'null))
                (else ((sexy-apply-wrapper (htr obj 'default)) msg))))))

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
                (apply (sexy-apply-wrapper obj) args)))
        (else (idk obj msg))))

(define (sexy-send-macro obj msg)
    (case msg
        ((type) 'macro)
        ((null?) 'false)
        ((view) (sexy-send obj 'code))
        ((to-bool) 'true)
        ((env) (reify-env (htr obj 'env)))
        ((code arity) (htr obj msg))
        ((apply)
            (lambda (args)
                ((sexy-apply-wrapper obj) args)))
        (else (idk obj msg))))

(define (sexy-send-env obj msg)
    (define vars (htr obj 'vars))
    (case msg
        ((lookup)
            (lambda (name)
                (cond
                    ((eq? name 'env) obj)
                    ((hte? vars name) (htr vars name))
                    ((hte? obj 'mama) ((sexy-send-env (htr obj 'mama) 'lookup) name))
                    (else 'null))))
        ((def!)
            (lambda (name val)
                (hts! vars name val)
                val))
        ((extend)
            (lambda (names vals)
                (define noob (sexy-environment obj))
                (define xs (zip names vals))
                (define setter! (sexy-send-env noob 'def!))
                (define (setme! pr)
                    (setter! (car pr) (cadr pr)))
                (map setme! xs)
                noob))
        ((set!)
            (lambda (name val)
                (define (oops)
                    (sexy-error 'set-var! "Environment has no definition for " name))
                (cond
                    ((hte? vars name) (hts! vars name val))
                    ((hte? obj 'mama) ((sexy-send-env (htr obj 'mama) 'set!) name val))
                    (else (oops)))
                val))
        ((type) 'env)
        ((null?) 'false)
        ((view) (sexy-view (hash-table->alist vars)))
        ((to-bool) 'true)
        ((has?)
            (lambda (x)
                (not (eqv? 'null ((sexy-send-env obj 'lookup) x)))))
        ((local?)
            (lambda (x)
                (if (hte? vars x)
                    (let ((v (htr vars x)))
                        (if (eq? v 'null)
                            'false
                            'true))
                    'false)))
        ((eval)
            (lambda (code)
                (sexy-eval code obj)))
        ((vars) (hash-table->alist vars))
        (else
            (if (hte? obj msg)
                (htr obj msg)
                (idk obj msg)))))

(define (sexy-send-vector obj msg)
    (case msg
        ((type) 'vector)
        ((null?) 'false)
        ((view)
            (vector-map
                (lambda (i x) (sexy-view x))
                obj))
        ((to-bool) (if (eq? (vector-length obj) 0) 'false 'true))
        ((to-list) (vector->list obj))
        ((size) (vector-length obj))
        ((has?)
            (lambda (item)
                (transbool
                    (vector-index
                        (lambda (x) (eq? x item))
                        obj))))
        ((fold)
            (lambda (init funk)
                (vector-fold (sexy-apply-wrapper funk) init obj)))
        ((foldr)
            (lambda (init funk)
                (vector-fold-right (sexy-apply-wrapper funk) init obj)))
        ((map)
            (lambda (funk)
                (vector-map (sexy-apply-wrapper funk) obj)))
        ((filter)
            (lambda (funk)
                (list->vector
                    (filter
                        (lambda (x)
                            (unbool ((sexy-apply-wrapper funk) x)))
                        (vector->list obj)))))
        ((sort)
            (lambda (funk)
                (sort
                    obj
                    (lambda (x y)
                        (unbool ((sexy-apply-wrapper funk) x y))))))
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

    
; macro expansion

(define (sexy-expand code env)
    (define (expand x)
        (sexy-expand x env))
    (define (lookup x)
        (if (sexy-global? x)
            (glookup x)
            ((sexy-send-env env 'lookup) x)))
    (define mutate!
        (sexy-send-env env 'def!))
    (define (sexy-macro? name)
        (define gmac (glookup name))
        (define obj
            (if (eq? 'null gmac)
                (lookup name)
                gmac))
        (if (and (hash-table? obj) (eq? (htr obj 'type) 'macro))
            #t
            #f))
    (cond
        ((atom? code) code)
        ((sexy-macro? (car code))
            (let* ((macname (car code)) (looked-up (lookup macname)))
                (if (eq? 'null looked-up)
                    (nodef macname)
                    (sexy-expand
                        (apply (sexy-apply-wrapper looked-up) (cdr code))
                        env))))
        (else 
            (case (car code)
                ((use) (sexy-expand (sexy-expand-use code env) env))
                ((def)
                    (let ((dval (caddr code)))
                        (if (and (pair? dval) (eq? (car dval) 'fn)) 
                            (let* ((expanded (map expand (cdr code)))
                                   (nucode (cons 'def expanded)))
                                ((sexy-compile nucode) env top-cont top-err)
                                nucode)
                            (cons 'def (map expand (cdr code))))))
                ((seq)
                    (begin
                        (let ((expanded (map expand code)))
                            (prep-defs (cdr expanded) env)
                            expanded)))
                ((quote) code)
                ((macro)
                    (let* ((noob (sexy-environment env))
                           (noob-expand (lambda (x) (sexy-expand x noob)))
                           (expanded (noob-expand (cdr code)))
                           (nucode (cons 'macro expanded)))
                        ((sexy-compile-macro nucode) env top-cont top-err)
                        nucode))
                (else (map expand code))))))

(define (sexy-expand-use code env)
    (define arg-pair (prepare-sexy-args (cdr code)))
    (define args (car arg-pair))
    (define opts (cdr arg-pair))
    (define uri (car args))
    (define as
        (let ((it (sexy-send opts 'as)))
            (if (unbool it)
                it
                #f)))
    (define (uri? str)
        (string-contains str ":"))
    (define code-str
        ; should check the cache somewhere here
        (cond
            ((symbol? uri) (niy))
            ((string? uri)
                (if (uri? uri)
                    (get-uri uri)
                    (get-file uri)))
            (else (sexy-error code "use: Identifier must be a symbol or a string."))))
    (define code-port
        (open-input-string code-str))
    (define prog
        (cons 'seq (sexy-read-file code-port)))
    ; write expanded to .sexy/cache
    (if as
        `(def ,as
            (obj default:
                ((fn () ,prog
                    (fn (msg) ((send env 'lookup) msg))))))
        prog))


; eval/apply

(define (sexy-eval code env)
    (define prog
        (sexy-compile (sexy-expand code env)))
    (prog env top-cont top-err))

(define (send-or-die obj msg cont err)
    (if msg
        (cont (sexy-send obj msg))
        (sexy-error `((,obj) => (send ,obj)) "send requires a message.")))

(define (sexy-apply obj xs cont err)
    (define arg-pair (prepare-sexy-args xs))
    (define args (car arg-pair))
    (define opts (cdr arg-pair))
    (cond
        ((procedure? obj) (cont (apply obj args)))
        ((or (pair? obj) (vector? obj) (string? obj)) (send-or-die obj (car args) cont err))
        ((hash-table? obj)
            (let ((type (htr obj 'type)))
                (if (or (eq? type 'fn) (eq? type 'macro))
                    ((htr obj 'exec) args opts cont err)
                    (send-or-die obj (car args) cont err))))
        (else (sexy-error obj (list obj " is not applicable!")))))

(define (sexy-apply-wrapper obj)
    (lambda xs
        (sexy-apply obj xs top-cont top-err)))

(define (prep-defs seq env)
    ; predefine all defs for mutual recursion
    (define mutate!
        (sexy-send env 'def!))
    (define (set-null! name)
        (mutate! name 'null))
    (define (get-defs seq)
        (filter (lambda (x) (and (pair? x) (eq? (car x) 'def))) seq))
    (map set-null! (map cadr (get-defs seq))))

(define-syntax frag
    (ir-macro-transformer
        (lambda (expr inject compare)
            (let ((body (cdr expr)))
                `(lambda (,(inject 'env) ,(inject 'cont) ,(inject 'err)) ,@body)))))

(define (capture-cont conts tag)
    (define (bail)
        (sexy-error "Hit the wall!" "Gate " tag " not found!"))
    (if (null? conts)
        (bail)
        (let loop ((this (car conts)) (rest (cdr conts)) (sofar '()))
            (if (eq? tag (car this))
                (cons (reverse sofar) rest)
                (if (null? rest)
                    (bail)
                    (loop (car rest) (cdr rest) (cons this sofar)))))))

(define (sexception ex err cont)
    (sexy-apply err (list ex cont) top-cont top-err)) ; FIXME

(define blessed
    '(def quote if seq set! fn gate capture ensure guard error macro env return))

(define (holy? x)
    (or (member x blessed)
        (not (eq? 'null (glookup x)))))
 
(define (sexy-compile code)
    (if (atom? code)
        (sexy-compile-atom code)
        (case (car code)
            ((def) (sexy-compile-def code))
            ((quote) (sexy-compile-quote code))
            ((if) (sexy-compile-if code))
            ((seq) (sexy-compile-seq code))
            ((set!) (sexy-compile-set! code))
            ((fn) (sexy-compile-fn code))
            ((gate) (sexy-compile-gate code))
            ((capture) (sexy-compile-capture code))
            ((ensure) (sexy-compile-ensure code))
            ((guard) (sexy-compile-guard code))
            ((error) (sexy-compile-error code))
            ((macro) (sexy-compile-macro code))
            (else (sexy-compile-application code)))))

(define (sexy-compile-atom code)
    (define pass (frag (cont code)))
    (if (symbol? code)
        (if (keyword? code)
            pass
            (case code
                ((true false null) pass)
                ((env) (frag (cont env)))
                (else
                    (if (sexy-global? code)
                        (let ((gvalue (glookup code)))
                            (lambda (env cont err)
                                (cont gvalue)))
                        (frag
                            (let ((looked-up ((sexy-send env 'lookup) code)))
                                (if (eq? 'null looked-up)
                                    (sexception (cons 'undefined_symbol code) err cont)
                                    (cont looked-up))))))))
        pass))

(define (sexy-compile-def code)
    (define name (cadr code))
    (define val (caddr code))
    (define val-c (sexy-compile val))
    (if (not (symbol? name))
        (sexy-error "def expects it's first argument to be a symbol.  Got " code)
        (if (holy? name)
            (sexy-error code name " is blessed and holy.  It cannot be redefined.")
            (frag
                (let ((mutate!  (sexy-send env 'def!)))
                    (if (eq? 'true ((sexy-send env 'local?) name))
                        (sexy-error code name " is already defined in the local environment.")
                        (begin
                            (mutate! name 'null)
                            (val-c
                                env
                                (lambda (v) (mutate! name v) (cont v))
                                err))))))))

(define (sexy-compile-quote code)
    (frag
        (cont (cadr code))))

(define (sexy-compile-if code)
    (define pred (sexy-compile (cadr code)))
    (define iftrue (sexy-compile (caddr code)))
    (define iffalse (sexy-compile (cadddr code)))
    (frag
        (pred
            env
            (lambda (b)
                (if (eq? (sexy-bool b) 'true)
                    (iftrue env cont err)
                    (iffalse env cont err)))
            err)))

(define (sexy-compile-seq code)
    (define seq (cdr code))
    (if (pair? seq)
        (sexy-seq-subcontractor seq)
        (sexy-error code "Empty sequences are forbidden!")))

(define (sexy-seq-subcontractor xs)
    (define head (car xs))
    (define tail (cdr xs))
    (define head-c (sexy-compile head))
    (if (pair? tail)
        (let ((tail-c (sexy-seq-subcontractor tail)))
            (frag
                (prep-defs xs env)
                (head-c
                    env
                    (lambda (h) (tail-c env cont err))
                    err)))
        (sexy-compile head)))

(define (sexy-compile-set! code)
    (define name (cadr code))
    (define val (caddr code))
    (define val-c (sexy-compile val))
    (if (symbol? name)
        (if (holy? name)
            (sexy-error code name " is blessed and holy.  It cannot be redefined.")
            (frag
                (if ((sexy-send env 'has?) name)
                    (val-c
                        env
                        (lambda (v) ((sexy-send env 'set!) name v) (cont v))
                        err)
                    (sexy-error code "Symbol not defined: " name))))
        (sexy-error code "set! wants a symbol as its first argument!")))

(define (make-sexy-proc code env formals bodies-c)
    (define arity (length formals))
    (sexy-proc
        code
        env 
        (lambda (args opts cont err)
            (if (< (length args) arity)
                (sexy-error code (sprintf "Procedure requires ~A arguments!" arity))
                (let* ((fargs (if (pair? args) (take args arity) '()))
                       (the-rest (if (pair? args) (drop args arity) '()))
                       (returner (lambda (v) (cont v)))
                       (noob
                           ((sexy-send-env env 'extend)
                                (append formals '(opt rest return))
                                (append fargs (list opts the-rest returner)))))
                    (bodies-c noob cont err))))))

(define (sexy-compile-fn code)
    (define formals (cadr code))
    (define bodies (cddr code))
    (define bodies-c (sexy-seq-subcontractor bodies))
    (frag
        (cont (make-sexy-proc code env formals bodies-c))))

(define (sexy-compile-macro code)
    (define name (cadr code))
    (define formals (caddr code))
    (define bodies (cdddr code))
    (define bodies-c (sexy-seq-subcontractor bodies))
    (if (holy? name)
        (sexy-error code name " is blessed and holy.  It cannot be redefined.")
        (frag
            (define thing (make-sexy-proc (cdr code) env formals bodies-c))
            (hts! thing 'name name)
            (hts! thing 'type 'macro)
            ((sexy-send-env env 'def!) name thing)
            (cont thing))))

(define (sexy-compile-gate code)
    (define expr (cadr code))
    (define expr-c (sexy-compile expr))
    (frag
        (cont
            (expr-c env identity err))))

(define (sexy-compile-capture code)
    (define lamb (cadr code))
    (define lamb-c (sexy-compile lamb))
    (frag
        (lamb-c
            env
            (lambda (funk)
                (sexy-apply
                    funk
                    (list (lambda (k-val) (cont k-val)))
                    top-cont
                    err))
            err)))

(define (sexy-compile-guard code)
    (define handler (cadr code))
    (define expr (caddr code))
    (define handler-c (sexy-compile handler))
    (define expr-c (sexy-compile expr))
    (frag
        (handler-c
            env
            (lambda (handler-fn)
                (define (new-err-cont e k)
                    (sexy-apply handler-fn (list e k) cont err))
                (expr-c env cont new-err-cont))
            err)))

(define (sexy-compile-error code)
    (define errobj (cadr code))
    (define erob-c (sexy-compile errobj))
    (frag
        (erob-c
            env
            (lambda (e)
                (err e cont))
            err)))

(define (sexy-compile-ensure code)
    (define protector-c (sexy-compile (cadr code)))
    (define expr-c (sexy-compile (caddr code)))
    (frag
        (protector-c
            env
            (lambda (protector-thunk)
                (define (p-cont v)
                    (sexy-apply protector-thunk '() identity err)
                    (cont v))
                (define (p-err e k)
                    (sexy-apply protector-thunk '() identity err)
                    (err e k))
                (p-cont (expr-c env identity p-err)))
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
    (define args-c (sexy-compile-list (cdr code)))
    (frag
        (fn-c
            env
            (lambda (f) 
                (args-c
                    env
                    (lambda (args) (sexy-apply f args cont err))
                    err))
            err)))

(define reify-env identity)

; setup 

(define (sexy-read-file port)
    (define program
        (let loop ((noob (sexy-read port)) (code '()))
            (if (eof-object? noob)
                (reverse code)
                (loop (sexy-read port) (cons noob code)))))
    (close-input-port port)
    program)

(define prelude-uri "~/dev/sexy/global.sex")

(define prelude-struct
    (sexy-read-file (open-input-file prelude-uri)))

(define genv #f)

(define (local-env)
    (sexy-environment #f))

(define (global-env)
    (define (make-new)
        (define prelude (local-env))
        (define (preset! k v)
            ((sexy-send prelude 'def!) k v))
        (define (fill-prelude fs)
            (define (setem! p)
                (preset! (car p) (cdr p)))
            (map setem! fs))
        (define (istrue x)
            (eq? 'true (sexy-bool x)))
        (define primitives
            (list
                (cons 'stdin (current-input-port))
                (cons 'stdout (current-output-port))
                (cons 'stderr (current-error-port))
                (cons 'is? (eq-fixer (bool-fixer eq?)))
                (cons '+ +)
                (cons '- -)
                (cons '* *)
                (cons '/ /)
                (cons 'div quotient)
                (cons 'rem remainder)
                (cons 'mod modulo)
                (cons 'cons cons)
                (cons 'list list)
                (cons 'vector vector)
                (cons '= (eq-fixer (bool-fixer equal?)))
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
                (cons 'show
                    (lambda (x)
                        (sexy-write x (current-output-port))
                        (newline)
                        x))
                (cons 'rec
                    (sexy-proc
                        'primitive-function
                        'global
                        (lambda (args opts cont err)
                            (cont (sexy-record args)))))
                (cons 'obj
                    (sexy-proc
                        'primitive-function
                        'global
                        (lambda (args opts cont err)
                            (define autos (sexy-send opts 'auto))
                            (define rsend (sexy-send opts 'resend))
                            (define default (sexy-send opts 'default))
                            (if (eq? autos 'null) (set! autos #f) #f)
                            (if (eq? rsend 'null) (set! rsend #f) #f)
                            (if (eq? default 'null) (set! default #f) #f)
                            (cont (sexy-object args autos rsend default)))))
                (cons 'test
                    (lambda (tname ok)
                        (debug tname (if (eq? ok 'true) 'ok 'FAIL))
                        'null))))
        (fill-prelude primitives)
        prelude)
    (if genv
        genv
        (let ((noob (make-new)))
            (set! genv noob)
            noob)))

(define (add-global-prelude)
    (define prelude-env (sexy-environment genv))
    (define prelude-c
        (sexy-seq-subcontractor
            (sexy-expand prelude-struct prelude-env)))
    (define full
        (prelude-c
                genv
                top-cont
                top-err))
    'null)

(define (sexy-global? x)
    (if (not (eq? (glookup x) 'null))
        #t
        #f))

(define (glookup x)
    ((sexy-send genv 'lookup) x))

(define (sexy-run program)
    (if (pair? program)
        ((sexy-seq-subcontractor program)
            (local-env)
            (lambda (v) (exit))
            top-err)
        (exit)))

(define (sexy-repl)
    (define stdin (current-input-port))
    (define stdout (current-output-port))
    (define stderr (current-error-port))
    (define (loop env)
        (display "(sexy) ")
        (sexy-apply
            (sexy-send stdin 'read)
            '()
            (lambda (expr)
                (define compiled
                    (sexy-compile
                        (sexy-expand expr (sexy-environment env))))
                (compiled
                    env
                    (lambda (v)
                        (sexy-apply
                            (sexy-send stdout 'print)
                            (list v)
                            (lambda (null) (loop (sexy-environment env)))
                            top-err))
                    top-err))
            top-err))
    (newline)
    (display "Welcome to the Sexy Read-Eval-Print Loop.  Press Ctrl-D to exit.")
    (newline)
    (newline)
    (loop (local-env)))

(start)


