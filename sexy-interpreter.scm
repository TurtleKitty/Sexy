
; CHICKEN!

(use srfi-1)
(use srfi-13)
(use srfi-69)

(use numbers)
(use posix)
(use utils)
(use uuid)
(use vector-lib)

(use openssl)
(use http-client)

(declare
    (block)
    (inline)
    (local))


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
    (newline)
    (exit))

(define (niy)
    (newline)
    (display "Umm... that's not done yet.")
    (newline)
    (newline))

(define top-cont identity)
(define top-err  (lambda (ex continue) (sexy-error "Uncaught error: " ex)))

(define sexy-cache-dir "~/.sexy/compiled")


(define (check-file f)
    (if (file-exists? f)
        (if (equal? (string-ref f 0) #\/)
            f   ; absolute filename
            (string-append (current-directory) "/" f))
        (begin
            (debug "File not found!" f)
            (exit))))

(define (get-sexy-cached-path f)
    (string-append
        sexy-cache-dir
        "/"
        (irregex-replace/all "[^a-zA-Z0-9_]" f "_")))

(define (file-newer? f1 f2)
    (> (file-modification-time f1) (file-modification-time f2)))

(define (read-expand-cache-prog fname)
    (define fpath (check-file fname))
    (define cpath (get-sexy-cached-path fpath))
    (define is-cached (and (file-exists? cpath) (file-newer? cpath fpath)))
    (if is-cached
        (read
            (open-input-file cpath))
        (let ((expanded
                (sexy-expand
                    (sexy-read-file
                        (open-input-file fpath))
                    (cli-env)))
               (fport (open-output-file cpath)))
            (write expanded fport)
            (close-output-port fport)
            expanded)))

(define (start)
    (define args (command-line-arguments))
    (define (fname)
        (if (pair? (cdr args))
            (cadr args)
            (usage)))
    (if (not (directory? sexy-cache-dir))
        (create-directory sexy-cache-dir #t)
        #f)
    (global-env)
    (add-global-prelude)
    (if (not (pair? args))
        (usage)
        (let ((cmd (string->symbol (car args))))
            (case cmd
                ((run) (sexy-run (read-expand-cache-prog (fname))))
                ((repl) (sexy-repl))
                ((check) (niy))
                ((compile)
                    (begin
                        (read-expand-cache-prog (fname))
                        (debug "Wrote compiled file to " (get-sexy-cached-path (check-file (cadr args))))))
                ((expand)
                    (begin
                        (sexy-write
                            (read-expand-cache-prog (fname))
                            (current-output-port))
                        (newline)))
                (else (printf "Unknown command: ~A~%" cmd))))))


; utils

(define mkht make-hash-table)
(define htr hash-table-ref)
(define htks hash-table-keys)
(define htvs hash-table-values)
(define hte? hash-table-exists?)
(define hts! hash-table-set!)
(define htd! hash-table-delete!)

(define (nop v) 'null)

(define not-found 'this-sexy-name-was-not-found)
(define will-exist 'this-sexy-name-is-about-to-be-defined)

(define (idk obj msg cont err)
    (debug "IDK!!!" obj msg cont err)
    (err (list 'message-not-understood obj msg) cont))

(define (debug . xs)
    (display xs) (newline))

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
    (display "ERRORED!!!") (newline)
    (display (sexy-view form)) (newline)
    (display args) (newline)
    (newline)
    identity)

(define (sexy-view obj)
    (sexy-send obj 'view top-cont top-err))

(define (bool-fixer op)
    (lambda args
        (if (apply op args)
            'true
            'false)))

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
        (cons (reverse args) opts))
    (if (pair? xs)
        (let* ((options (sexy-record))
               (setopt! (sexy-send-record options 'set! top-cont top-err)))
            (let loop ((head (car xs)) (tail (cdr xs)) (args '()))
                (if (keyword? head)
                    (let ((k (string->symbol (keyword->string head))) (v (car tail)))
                        (setopt! k v)
                        (if (pair? (cdr tail))
                            (loop (cadr tail) (cddr tail) args)
                            (rval args options)))
                    (if (pair? tail)
                        (loop (car tail) (cdr tail) (cons head args))
                        (rval (cons head args) options)))))
        (rval '() (sexy-record))))

; sexy objects

(define (sexy-record . args)
    (define this (mkht))
    (define vars (mkht))
    (hts! this 'type 'record)
    (for-pairs (lambda (k v) (hts! vars k v)) args)
    (hts! this 'vars vars)
    this)

(define (sexy-environment mama)
    (define this (mkht))
    (define vars (sexy-record))
    (hts! this 'type 'env)
    (hts! this 'vars vars)
    (hts! this 'mama (if mama mama 'null))
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
            (map (lambda (msg) (rset! msg delegate)) msgs)))
    (for-pairs fset! args)
    (if resends
        (map set-resend! resends)
        #f)
    (if autos
        (map aset! autos))
    (tset! 'type 'object)
    (tset! 'fields fields)
    (tset! 'autos autoexec)
    (tset! 'resends delegates)
    (tset! 'default
        (or initial
            (sexy-proc
                'primitive-function
                'object
                (lambda (args opts cont err)
                    (err (list 'message-not-understood this (car args)) cont)))))
    this)

(define (sexy-compile-method code)
    ((sexy-compile-fn (sexy-parse code)) (local-env) identity identity))


; message passing

(define (sexy-send obj msg cont err)
    (define (wtf)
        (error (list "WTF kind of object was THAT?" obj msg)))
    (cond
        ((symbol? obj) (sexy-send-symbol obj msg cont err))
        ((number? obj) (sexy-send-number obj msg cont err))
        ((string? obj) (sexy-send-string obj msg cont err))
        ((null? obj) (sexy-send-empty obj msg cont err))
        ((pair? obj) (sexy-send-pair obj msg cont err))
        ((procedure? obj) (sexy-send-primitive obj msg cont err))
        ((vector? obj) (sexy-send-vector obj msg cont err))
        ((port? obj) (sexy-send-port obj msg cont err))
        ((hash-table? obj)
            (let ((t (htr obj 'type)))
                (if (eq? msg 'type)
                    (cont t)
                    (case t
                        ((env)    (sexy-send-env obj msg cont err))
                        ((record) (sexy-send-record obj msg cont err))
                        ((object) (sexy-send-object obj msg cont err))
                        ((fn)     (sexy-send-fn obj msg cont err))
                        ((macro)  (sexy-send-fn obj msg cont err))
                        (else (wtf))))))
        ((eof-object? obj) (newline) (newline) (exit))
        (else (wtf))))

(define (sexy-send-symbol obj msg cont err)
    (case msg
        ((to-string) (cont (symbol->string obj)))
        ((view) (cont obj))
        (else
            (case obj
                ((true false) (sexy-send-bool obj msg cont err))
                ((null) (sexy-send-null obj msg cont err))
                (else
                    (case msg
                        ((type) (cont 'symbol))
                        ((to-bool) (cont 'true))
                        (else (idk obj msg cont err))))))))

(define (sexy-send-bool obj msg cont err)
    (case msg
        ((type) (cont 'bool))
        ((to-bool view) (cont obj))
        ((not) (cont (if (eq? 'true obj) 'false 'true)))
        (else (idk obj msg cont err))))

(define (sexy-send-null obj msg cont err)
    (cont
        (case msg
            ((to-bool) 'false)
            (else 'null))))

(define (sexy-send-number obj msg cont err)
    (case msg
        ((zero?) (cont (if (eq? obj 0) 'true 'false)))
        ((to-bool) (cont (if (eq? obj 0) 'false 'true)))
        ((to-string) (cont (number->string obj)))
        ((view) (cont obj))
        (else
            (cond
                ((integer? obj) (sexy-send-int obj msg cont err))
                ((real? obj) (sexy-send-real obj msg cont err))
                (else (idk obj msg cont err))))))

(define (sexy-send-int obj msg cont err)
    (case msg
        ((type) (cont 'int))
        ((times) (cont 'niy))
        (else (idk obj msg cont err))))
 
(define (sexy-send-real obj msg cont err)
    (case msg
        ((type) (cont 'real))
        ((floor) (cont (floor obj)))
        ((ceil) (cont (ceiling obj)))
        ((round) (cont (round obj)))
        (else (idk obj msg cont err))))

(define (sexy-send-string obj msg cont err)
    (case msg
        ((type view to-bool to-symbol to-number size)
            (cont
                (case msg
                    ((type) 'string)
                    ((view) obj)
                    ((to-bool) (if (eq? (string-length obj) 0) 'false 'true))
                    ((to-symbol) (string->symbol obj))
                    ((to-number) (string->number obj))
                    ((size) (string-length obj)))))
        ((join) (cont 'niy))
        ((split) (cont 'niy))
        (else
            (if (number? msg)
                (if (> (string-length obj) msg)
                    (cont (string-ref obj msg))
                    (err (list 'out-of-bounds obj msg) cont))
                (idk obj msg cont err)))))

(define (sexy-send-empty obj msg cont err)
    (case msg
        ((type empty? view to-bool head tail key val car cdr size)
            (cont
                (case msg
                    ((type) 'pair)
                    ((empty?) 'true)
                    ((view) '())
                    ((to-bool) 'false)
                    ((head tail key val car cdr) 'null)
                    ((size) 0))))
        (else (sexy-send-pair obj msg cont err))))

(define (sexy-send-pair obj msg cont err)
    (case msg
        ((type empty? view to-bool to-vector head key car tail val cdr size reverse has? append apply)
            (cont
                (case msg
                    ((type) 'pair)
                    ((empty?) 'false)
                    ((view)
                        (if (list? obj)
                            (map sexy-view obj)
                            (cons (sexy-view (car obj)) (sexy-view (cdr obj)))))
                    ((to-bool) 'true)
                    ((to-vector) (list->vector obj))
                    ((head key car) (car obj))
                    ((tail val cdr) (cdr obj))
                    ((size) (length obj))
                    ((reverse) (reverse obj))
                    ((has?) (lambda (item) (transbool (member item obj))))
                    ((append) (lambda (other) (append obj other)))
                    ((apply)
                        (sexy-proc
                            'primitive-function
                            'pair
                            (lambda (args opts cont err)
                                (sexy-send-pair obj (car args) cont err)))))))
        ((fold)
            (sexy-compile-method
                `(fn (acc funk)
                    (def xs (quote ,obj))
                    (if xs.empty?
                        acc
                        (xs.tail.fold (funk acc xs.head) funk)))))
        ((reduce)
            (sexy-compile-method
                `(fn (acc funk)
                    (def xs (quote ,obj))
                    (if xs.empty?
                        acc
                        (funk xs.head (xs.tail.reduce acc funk))))))
        ((map)
            (sexy-compile-method
                `(fn (funk)
                    (def xs (quote ,obj))
                    (xs.reduce '() (fn (x y) (pair (funk x) y))))))
        ((filter)
            (sexy-compile-method
                `(fn (funk)
                    (def xs (quote ,obj))
                    (xs.reduce '() (fn (x y) (if (funk x) (cons x y) y))))))
        ((sort)
            (lambda (funk)
                (sort
                    obj
                    (lambda (x y)
                        (unbool ((sexy-apply-wrapper funk) x y))))))
        (else
            (if (number? msg)
                (if (> (length obj) msg)
                    (cont (list-ref obj msg))
                    (err (list 'out-of-bounds obj msg) cont))
                (idk obj msg cont err)))))

(define (sexy-send-primitive obj msg cont err)
    (cont 
        (case msg
            ((type) 'fn)
            ((view code) 'primitive-function)
            ((to-bool) 'true)
            ((env) 'global)
            ((arity)
                (let ((pinfo (procedure-information obj)))
                    (if (list? pinfo)
                        (sub1 (length pinfo))
                        '*)))
            ((apply)
                (lambda (args)
                    (apply obj args))))))

(define (sexy-send-record obj msg cont err)
    (define vars (htr obj 'vars))
    (cont
        (case msg
            ((view)
                (let ((keys (htks vars)))
                    (cons ': (map (lambda (k) (list k (sexy-view (htr vars k)))) keys))))
            ((size) (hash-table-size vars))
            ((to-bool)
                (transbool (> 0 ((hash-table-size vars)))))
            ((get)
                (lambda (k)
                    (if (hte? vars k)
                        (htr vars k)
                        'null)))
            ((put)
                (lambda args
                    (define noob (sexy-record))
                    (hts! noob 'vars (hash-table-copy vars))
                    (apply (sexy-send-record noob 'set! top-cont top-err) args)
                    noob))
            ((set!)
                (lambda args
                    (for-pairs (lambda (k v) (hts! vars k v)) args)
                    'null))
            ((del!)
                (lambda args (map (lambda (k) (htd! vars k)) args))
                'null)
            ((has?)
                (lambda (x)
                    (transbool (hte? vars x))))
            ((apply)
                (lambda (args)
                    (sexy-send-record obj (car args) cont err)))
            ((keys) (htks vars))
            ((values) (htvs vars))
            ((pairs) (hash-table->alist vars))
            ((merge) 'niy)
            ((fold) 'niy)
            ((reduce) 'niy)
            ((map) 'niy)
            ((filter) 'niy)
            ((sort) 'niy)
            (else
                (if (hte? vars msg)
                    (htr vars msg)
                    'null)))))

(define (sexy-send-object obj msg cont err)
    (define fields (htr obj 'fields))
    (define resends (htr obj 'resends))
    (define autos (htr obj 'autos))
    (if (hte? fields msg)
        (let ((v (htr fields msg)))
            (if (hte? autos msg)
                (sexy-apply v '() cont err) ; exec the thunk
                (cont v)))
        (if (hte? resends msg)
            (sexy-send (htr resends msg) msg cont err)
            (case msg
                ((view) (cont 'object))
                ((to-bool) (cont (if (eq? 0 (length (hash-table-keys fields))) 'false 'true)))
                ((apply) (cont (lambda args (sexy-send obj (car args) top-cont err))))
                ((responds?) (cont (lambda (x) (transbool (hte? fields x)))))
                (else (sexy-apply (htr obj 'default) (list msg) cont err))))))

(define (sexy-send-fn obj msg cont err)
    (case msg
        ((view) (sexy-send obj 'code cont err))
        ((to-bool) (cont 'true))
        ((arity code env formals) (cont (htr obj msg)))
        ((apply)
            (cont 
                (sexy-proc
                    'primitive-function
                    'fn
                    (lambda (args opts cont err)
                        (sexy-apply obj (car args) cont err)))))
        (else (idk obj msg cont err))))

(define (sexy-send-env obj msg cont err)
    (case msg
        ((get put has? set! del! view to-bool pairs)
            (sexy-send-record (htr obj 'vars) msg cont err))
        ((lookup) (cont (lambda (k) (lookup obj k))))
        ((extend) (cont (lambda (names vals) (extend obj names vals))))
        ((mama) (cont (htr obj 'mama)))
        ((eval)
            (cont
                (lambda (code)
                    (sexy-eval code obj))))
        (else (idk obj msg cont err))))

(define (sexy-send-vector obj msg cont err)
    (case msg
        ((type view to-bool to-list size has? apply)
            (cont 
                (case msg
                    ((type) 'vector)
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
                    ((apply)
                        (lambda (args)
                            (sexy-send-vector obj (car args) cont err))))))
        ((fold)
            (lambda (init funk)
                (vector-fold (sexy-apply-wrapper funk) init obj)))
        ((reduce)
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
                (if (> (vector-length obj) msg)
                    (cont (vector-ref obj msg))
                    (err (list 'out-of-bounds obj msg) cont))
                (idk obj msg cont err)))))

(define (sexy-read port)
    (sexy-parse (read port)))

(define (sexy-write obj port)
    (write (sexy-view obj)))

(define (sexy-print obj port)
    (display (sexy-view obj)))

(define (sexy-send-port obj msg cont err)
    (case msg
        ((type to-bool view read list read-line to-list to-string write print say nl)
            (cont 
                (case msg
                    ((type) 'port)
                    ((to-bool) 'true)
                    ((view) obj)
                    ((read) (lambda () (sexy-read obj)))
                    ((list) (lambda () (map sexy-read (read-file obj))))
                    ((read-line) (lambda () (read-line obj)))
                    ((to-list) (lambda () (read-lines obj)))
                    ((to-string) (lambda () (read-string obj)))
                    ((write) (lambda (x) (sexy-write x obj) 'null))
                    ((print) (lambda (x) (sexy-print x obj) 'null))
                    ((say) (lambda (x) (sexy-print x obj) (newline obj) 'null))
                    ((nl) (newline obj)))))
        (else (idk msg obj cont err))))

(define (sexy-bool obj)
    (sexy-send obj 'to-bool top-cont top-err))

    
; macro expansion

(define (sexy-expand code env)
    (define (expand x)
        (sexy-expand x env))
    (define (look-it-up x)
        (if (sexy-global? x)
            (glookup x)
            (lookup env x)))
    (define (sexy-macro? name)
        (define gmac (glookup name))
        (define obj
            (if (eq? not-found gmac)
                (look-it-up name)
                gmac))
        (if (and (hash-table? obj) (eq? (htr obj 'type) 'macro))
            #t
            #f))
    (cond
        ((atom? code) code)
        ((sexy-macro? (car code))
            (let* ((macname (car code)) (looked-up (look-it-up macname)))
                (if (eq? not-found looked-up)
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
        (let ((it (sexy-send opts 'as top-cont top-err)))
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
            (object default:
                ((fn () ,prog
                    (fn (msg) ((send env 'get) msg))))))
        prog))


; eval/apply

(define (sexy-eval code env)
    (define prog
        (sexy-compile (sexy-expand code env)))
    (prog env top-cont top-err))

(define (sexy-apply obj xs cont err)
    (define (apply-or-die)
        (sexy-send obj 'apply
            (lambda (af)
                (sexy-apply af xs cont err))
              err))
    (cond
        ((procedure? obj)
            (handle-exceptions exn
                (err exn top-cont)
                (cont (apply obj xs))))
        ((hash-table? obj)
            (let ((type (htr obj 'type)))
                (if (or (eq? type 'fn) (eq? type 'macro))
                    (let* ((arg-pair (prepare-sexy-args xs)) (args (car arg-pair)) (opts (cdr arg-pair)))
                        ((htr obj 'exec) args opts cont err))
                    (apply-or-die))))
        (else (apply-or-die))))

(define (sexy-apply-wrapper obj)
    (lambda xs
        (sexy-apply obj xs top-cont top-err)))

(define (prep-defs seq env)
    ; predefine all defs for mutual recursion
    (define (set-ready! name)
        (mutate! env name will-exist))
    (define (get-defs seq)
        (filter
            (lambda (x)
                (and (pair? x)
                     (or
                        (eq? (car x) 'macro)
                        (eq? (car x) 'fun)
                        (eq? (car x) 'def))))
            seq))
    (map set-ready! (map cadr (get-defs seq))))

(define-syntax frag
    (ir-macro-transformer
        (lambda (expr inject compare)
            (let ((body (cdr expr)))
                `(lambda (,(inject 'env) ,(inject 'cont) ,(inject 'err)) ,@body)))))

(define blessed
    '(def quote if seq set! fn wall gate capture ensure guard error macro env opt rest return))

(define (holy? name)
    (or (member name blessed)
        (let ((x (glookup name)))
            (not
                (or
                    (eq? x not-found)
                    (eq? x will-exist))))))

(define (blasphemy code name)
    (sexy-error code name " is sacred.  It cannot be redefined."))

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
            ((wall) (sexy-compile-wall code))
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
                ((global) (frag (cont genv)))
                (else
                    (if (sexy-global? code)
                        (frag
                            (cont (glookup code)))
                        (frag
                            (let ((looked-up (lookup env code)))
                                (if (eq? not-found looked-up)
                                    (err (cons 'undefined_symbol code) cont)
                                    (cont looked-up))))))))
        pass))

(define (sexy-compile-def code)
    (define name (cadr code))
    (define val (caddr code))
    (if (not (symbol? name))
        (sexy-error "def expects it's first argument to be a symbol.  Got " code)
        (if (holy? name)
            (blasphemy code name)
            (frag
                (if (and
                        (eq? 'true ((sexy-send-env env 'has? top-cont top-err) name))
                        (not (eq? will-exist ((sexy-send-env env 'get top-cont top-err) name))))
                    (sexy-error code name " is already defined in the local environment.")
                    (begin
                        (let ((val-c (sexy-compile val)))
                            (val-c
                                env
                                (lambda (v) (mutate! env name v) (cont v))
                                err))))))))

(define (sexy-compile-set! code)
    (define name (cadr code))
    (define val (caddr code))
    (define val-c (sexy-compile val))
    (if (symbol? name)
        (if (holy? name)
            (blasphemy code name)
            (frag
                (let loop ((this-env env))
                    (if (eq? 'true ((sexy-send-env this-env 'has? top-cont top-err) name))
                        (val-c
                            env
                            (lambda (v) (mutate! this-env name v) (cont v))
                            err)
                        (if (hte? this-env 'mama)
                            (loop (htr this-env 'mama))
                            (sexy-error code "Symbol not defined: " name))))))
        (sexy-error code "set! wants a symbol as its first argument!")))

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
    (let ((head-c (sexy-compile head)))
        (if (pair? tail)
            (let ((tail-c (sexy-seq-subcontractor tail)))
                (frag
                    (prep-defs xs env)
                    (head-c
                        env
                        (lambda (h) (tail-c env cont err))
                        err)))
            head-c)))

(define (make-sexy-proc code env formals bodies)
    (define arity (length formals))
    (define bodies-c (sexy-seq-subcontractor bodies))
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
                           (extend env 
                                (append formals '(opt rest return))
                                (append fargs (list opts the-rest returner)))))
                    (bodies-c noob cont err))))))

(define (sexy-compile-fn code)
    (define formals (cadr code))
    (define bodies (cddr code))
    (frag
        (cont (make-sexy-proc code env formals bodies))))

(define (sexy-compile-macro code)
    (define name (cadr code))
    (define formals (caddr code))
    (define bodies (cdddr code))
    (if (holy? name)
        (blasphemy code name)
        (frag
            (define thing (make-sexy-proc (cdr code) env formals bodies))
            (hts! thing 'name name)
            (hts! thing 'type 'macro)
            ((sexy-send-env env 'set! top-cont top-err) name thing)
            (cont thing))))

(define (sexy-compile-wall code)
    (define args (cadr code))
    (define exprs (cddr code))
    (define expr-c (sexy-seq-subcontractor exprs))
    ; create new env and copy args
    (frag
        (define noob (sexy-environment #f))
        (define setter! (sexy-send noob 'set! top-cont top-err))
        (define looker (sexy-send env 'lookup top-cont top-err))
        (map
            (lambda (x)
                (setter! x (looker x)))
            args)
            (expr-c noob cont err)))

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


; setup 

(define (sexy-read-file port)
    (define program
        (let loop ((noob (sexy-read port)) (code '()))
            (if (eof-object? noob)
                (reverse code)
                (loop (sexy-read port) (cons noob code)))))
    (close-input-port port)
    program)

(define genv #f)

(define (local-env)
    (sexy-environment #f))

(define (sexy-cli-args xs)
    (define (rval args opts)
        (cons
            (if (and (pair? args) (> (length args) 1))
                (cddr (reverse args))
                '())
            opts))
    (if (pair? xs)
        (let* ((options (sexy-record))
               (setopt! (sexy-send-record options 'set! top-cont top-err)))
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

(define global-arg-pair (sexy-cli-args (command-line-arguments)))

(define (cli-env)
    (define lenv (local-env))
    (extend lenv
        '(opt rest stdin stdout stderr)
        (list
            (cdr global-arg-pair)
            (car global-arg-pair)
            (current-input-port)
            (current-output-port)
            (current-error-port))))

(define (global-env)
    (define (make-new)
        (define prelude (local-env))
        (define preset!(sexy-send-env prelude 'set! top-cont top-err))
        (define (fill-prelude fs)
            (define (setem! p)
                (preset! (car p) (cdr p)))
            (map setem! fs))
        (define (istrue x)
            (eq? 'true (sexy-bool x)))
        (define primitives
            (list
                (cons 'is? (bool-fixer eq?))
                (cons '+ +)
                (cons '- -)
                (cons '* *)
                (cons '/ /)
                (cons '= (bool-fixer equal?))
                (cons '> (bool-fixer >))
                (cons '>= (bool-fixer >=))
                (cons '< (bool-fixer <))
                (cons '<= (bool-fixer <=))
                (cons 'div quotient)
                (cons 'rem remainder)
                (cons 'mod modulo)
                (cons 'num? (bool-fixer number?))
                (cons 'int? (bool-fixer integer?))
                (cons 'real? (bool-fixer real?))
                (cons 'pair cons)
                (cons 'pair? (bool-fixer pair?))
                (cons 'list list)
                (cons 'list? (bool-fixer list?))
                (cons 'vector vector)
                (cons 'vector? (bool-fixer vector?))
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
                            (define autos (sexy-send opts 'auto top-cont top-err))
                            (define rsend (sexy-send opts 'resend top-cont top-err))
                            (define default (sexy-send opts 'default top-cont top-err))
                            (if (eq? autos 'null) (set! autos #f) #f)
                            (if (eq? rsend 'null) (set! rsend #f) #f)
                            (if (eq? default 'null) (set! default #f) #f)
                            (cont (sexy-object args autos rsend default)))))
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
                (cons 'send
                    (sexy-proc
                        'primitive-function
                        'global
                        (lambda (args opts cont err)
                            (define l (length args))
                            (if (< l 2)
                                (err (list 'arity "Send requires two arguments: an object and a message.") cont)
                                (sexy-send (car args) (cadr args) cont err)))))
                (cons 'show
                    (lambda (x)
                        (sexy-write x (current-output-port))
                        (newline)
                        x))
                (cons 'gensym
                    (lambda ()
                        (string->symbol (string-append "symbol-" (uuid-v4)))))
                (cons 'test
                    (lambda (tname ok)
                        (debug tname (if (eq? ok 'true) 'ok 'FAIL))
                        'null))
                (cons 'FILE_NOT_FOUND 'neither-true-nor-false)
                (cons 'T_PAAMAYIM_NEKUDOTAYIM (quote ::))))
        (fill-prelude primitives)
        prelude)
    (if genv
        genv
        (let ((noob (make-new)))
            (set! genv noob)
            noob)))

(define prelude-file "./global.sex")

(define (add-global-prelude)
    (define expanded-code (read-expand-cache-prog prelude-file))
    (define prelude-c (sexy-seq-subcontractor expanded-code))
    (define full
        (prelude-c
                genv
                top-cont
                top-err))
    'null)

(define (sexy-global? x)
    (define got-it ((sexy-send-env genv 'has? top-cont top-err) x))
    (if (eq? 'true got-it)
        (not (eq? will-exist (glookup x)))
        #f))

(define (lookup env x)
    (define got-it ((sexy-send-env env 'has? top-cont top-err) x))
    (if (eq? got-it 'true)
        ((sexy-send-env env 'get top-cont top-err) x)
        (let ((mom (sexy-send-env env 'mama top-cont top-err)))
            (if (and mom (not (eq? mom 'null)))
                (lookup mom x)
                not-found))))

(define (extend env names vals)
    (define noob (sexy-environment env))
    (define (wreck! kv)
        (mutate! noob (car kv) (cadr kv)))
    (map wreck! (zip names vals))
    noob)

(define (mutate! env . args)
    (apply (sexy-send-env env 'set! top-cont top-err) args))

(define (glookup x)
    (lookup genv x))

(define (sexy-run program)
    (if (pair? program)
        ((sexy-seq-subcontractor program)
            (cli-env)
            (lambda (v) (exit))
            top-err)
        (exit)))

(define (sexy-repl)
    (define stdin (current-input-port))
    (define stdout (current-output-port))
    (define stderr (current-error-port))
    (define (loop env)
        (display "(sexy) ")
        (sexy-send stdin 'read
            (lambda (reader)
                (define expr (reader))
                (define compiled
                    (sexy-compile
                        (sexy-expand expr (sexy-environment env))))
                (compiled
                    env
                    (lambda (v)
                        (sexy-send stdout 'print
                            (lambda (printer)
                                (printer v)
                                (newline)
                                (loop (sexy-environment env)))
                            top-err))
                    top-err))
            top-err))
    (newline)
    (display "Welcome to the Sexy Read-Eval-Print Loop.  Press Ctrl-D to exit.")
    (newline)
    (newline)
    (loop (local-env)))

(start)


