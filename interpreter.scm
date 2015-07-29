
; CHICKEN!

(use srfi-1)
(use srfi-13)
(use srfi-69)

(use http-client)
(use medea)
(use numbers)
(use openssl)
(use posix)
(use symbol-utils)
(use tcp)
(use utf8)
(use utils)
(use uuid)
(use vector-lib)


; start

(define usage-text #<<END

Usage:

sexy repl
sexy exec "<code string>"
sexy run <filename>
sexy check <filename>
sexy expand <filename>
sexy compile <filename>
sexy clean

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
(define top-err  (lambda (ex continue) (debug "Uncaught error: " ex) (exit)))

(define *cwd* (current-directory))
(define *use-cache* #t)
(define sexy-use-symbols "~/.sexy/symbols.sex")
(define sexy-mod-dir     "~/.sexy/modules")
(define sexy-cache-dir   "~/.sexy/compiled")

(define (uri? str)
    (string-contains str ":"))

(define (absolute-path? path)
    (define idx0 (string-ref path 0))
    (or (equal? idx0 #\~) (equal? idx0 #\/)))

(define (get-uri uri)
    (define (reader port)
        (read-string #f port))
    (handle-exceptions exn
            not-found
            (call-with-input-request
                uri
                #f
                reader)))

(define (get-sexy-path f)
    (irregex-replace/all "[^a-zA-Z0-9_.]" f "_"))

(define (get-sexy-module-path f)
    (string-append sexy-mod-dir "/" (get-sexy-path f)))

(define (get-sexy-cached-path f)
    (string-append sexy-cache-dir "/" (get-sexy-path f)))

(define (find-file path)
    (define (fnf f)
        (debug "File not found!" f)
        (exit))
    (cond
        ((symbol? path)
            (niy))
        ((uri? path)
            (let ((module-path (get-sexy-module-path path)))
                (if (file-exists? module-path)
                    module-path
                    (let ((prog-text (get-uri path)))
                        (if (eq? prog-text not-found)
                            (fnf path)
                            (let ((mport (open-output-file module-path)))
                                (display prog-text mport)
                                (close-output-port mport)
                                module-path))))))
        ((file-exists? path)
            path)
        (else
            (fnf path))))

(define (file-newer? f1 f2)
    (> (file-modification-time f1) (file-modification-time f2)))

(define (make-module-absolute-path path)
    (cond
        ((symbol? path)
            (let ((str (symbol->string path)))
                (define xs (string-split str "/"))
                (define name (string->symbol (car xs)))
                (define the-rest (string-join (cdr xs) "/"))
                (define the-fun (lookup load-symbols-env name top-cont top-err))
                (if (not (and (hash-table? the-fun) (eq? (htr the-fun 'type) 'fn)))
                    (sexy-error code "No entry found in symbols.sex for " name)
                    (sexy-apply the-fun (list the-rest) top-cont top-err))))
        ((or (uri? path) (absolute-path? path)) path)
        (else (string-append *cwd* "/" path))))


(define (make-module-path-to path)
    (irregex-replace "(/.*)/.*$" path 1))

(define (read-expand-cache-prog path env)
    (define abs-path (make-module-absolute-path path))
    (define path-to (make-module-path-to abs-path))
    (define fpath (find-file abs-path))
    (define cpath (get-sexy-cached-path fpath))
    (define is-cached (and (file-exists? cpath) (file-newer? cpath fpath)))
    (if (and *use-cache* is-cached)
        (with-input-from-file
            cpath
            (lambda ()
                (read)))
        (let ((old-wd *cwd*))
            (set! *cwd* path-to)
            (set! *use-cache* #f)
            (let ((expanded
                    (sexy-expand
                        (sexy-read-file
                            (open-input-file fpath))
                        env))
                   (fport (open-output-file cpath)))
                (define finished
                    (cons
                        (delete-duplicates (find-modules expanded))
                        expanded))
                (write finished fport)
                (close-output-port fport)
                (set! *cwd* old-wd)
                (set! *use-cache* #t)
                finished))))

(define (find-modules prog)
    (define (finder prog xs)
        (let loop ((form (car prog)) (rest (cdr prog)) (mods xs))
            (if (pair? form)
                (if (eq? (car form) 'load)
                    (let ((numods (cons (make-module-absolute-path (cadr form)) mods)))
                        (if (pair? rest)
                            (loop (car rest) (cdr rest) numods)
                            numods))
                    (let ((numods (finder form mods)))
                        (if (pair? rest)
                            (finder rest numods)
                            numods)))
                (if (pair? rest)
                    (loop (car rest) (cdr rest) mods)
                    mods))))
    (cons 'modules (finder prog '())))

(define symbols.sex #<<END

(fun get-latest-version (repo path version)
    (fun frac (x) (x.split "."))
    (fun filt (n)
        (fn (x)
            (= x.0 n)))
    (fun ton (x) (x.map (_ _.to-number)))
    (fun sorter (x y)
        (if (< x.0 y.0)
            false
            (if (< x.1 y.1)
                false
                (if (< x.2 y.2)
                    false
                    true))))
    (def api-uri (cat "/" "https://api.github.com/repos" repo "contents" path))
    (def git-info (json.parse (fetch api-uri)))
    (when (not git-info)
        (error (list 'not-found git-dir)))
    (def names (git-info.map (_ _.name)))
    (def nums (names.map (_ (ton (frac _)))))
    (def only (nums.filter (filt version)))
    (def sorted (only.sort sorter))
    (def the-one (cat.apply (pair "." sorted.0)))
    (send (send (git-info.filter (_ (= _.name the-one))) 0) 'download_url))

(fun sexy (str)
    (def xs (str.split "/"))
    (def version xs.reverse.head)
    (def path
        (cat.apply
            (pair "/"
                (xs.take xs.size.dec))))
    (get-latest-version "TurtleKitty/sexy-lib" path version.to-number))

(fun github (str)
    (def xs (str.split "/"))
    (def user xs.0)
    (def repo xs.1)
    (def repo-path
        (cat.apply 
            (pair "/" ((send (xs.drop 2) 'take) (- xs.size 3)))))
    (def version xs.reverse.head)
    (get-latest-version (cat "/" user repo) repo-path version.to-number))

END
)

(define (start)
    (define args (command-line-arguments))
    (define (fname)
        (if (pair? (cdr args))
            (cadr args)
            (usage)))
    (define (prep-dir path)
        (if (not (directory? path))
            (create-directory path #t)
            #f))
    (prep-dir sexy-mod-dir)
    (prep-dir sexy-cache-dir)
    (if (not (file-exists? sexy-use-symbols))
        (with-output-to-file sexy-use-symbols
            (lambda ()
                (write-string symbols.sex))))
    (global-env)
    (add-global-prelude)
    (symbols-env)
    (if (not (pair? args))
        (usage)
        (let ((cmd (string->symbol (car args))))
            (case cmd
                ((repl) (sexy-repl))
                ((exec) 
                    (let ((code-str (fname)))
                        (sexy-run
                            (sexy-expand
                                (sexy-read-file
                                    (open-input-string code-str))
                                (cli-env)))))
                ((run) (sexy-run (read-expand-cache-prog (fname) (cli-env))))
                ((check) (niy))
                ((clean) (niy))
                ((compile)
                    (begin
                        (read-expand-cache-prog (fname) (cli-env))
                        (debug "Wrote compiled file to " (get-sexy-cached-path (find-file (cadr args))))))
                ((expand)
                    (begin
                        (pp
                            (sexy-view
                                (read-expand-cache-prog (fname) (cli-env))))
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

(define not-found 'this-sexy-name-was-not-found)
(define will-exist 'this-sexy-name-is-about-to-be-defined)

(define (idk obj msg cont err)
    (err (list 'message-not-understood (sexy-view obj) msg) cont))

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
    (display "ERRORED!!") (newline)
    (display (sexy-view form)) (newline)
    (display (sexy-view args)) (newline)
    (newline)
    identity)

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
    (if (or (hash-table? x) (hash-table? y))
        (let ((xt (htr x 'type)) (yt (htr y 'type)))
            (if (not (eq? xt yt))
                #f
                (case xt
                    ((env fn operator) (eq? x y))
                    ((record)
                        (let ((x-pairs (sort-symbol-alist (hash-table->alist (htr x 'vars))))
                              (y-pairs (sort-symbol-alist (hash-table->alist (htr y 'vars)))))
                            (equal? x-pairs y-pairs)))
                    (else
                        (sexy-send-object
                            x
                            '=
                            (lambda (f) (f y))
                            top-err)))))
        (equal? x y)))

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
               (vars (htr options 'vars))
               (setopt! (lambda (k v) (hts! vars k v))))
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

(define (sexy-gensym)
    (string->symbol (string-append "gensym-" (uuid-v4))))

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
                    (idk this (car args) cont err)))))
    this)

(define (sexy-compile-method code)
    ((sexy-compile-fn (sexy-parse code)) (local-env) identity identity))


; message passing

(define (sexy-send obj msg cont err)
    (define (wtf)
        (error (list "WTF kind of object was THAT?" obj msg)))
    (cond
        ((boolean? obj) (sexy-send-bool obj msg cont err))
        ((symbol? obj) (sexy-send-symbol obj msg cont err))
        ((number? obj) (sexy-send-number obj msg cont err))
        ((char? obj) (sexy-send-rune obj msg cont err))
        ((string? obj) (sexy-send-text obj msg cont err))
        ((null? obj) (sexy-send-empty obj msg cont err))
        ((pair? obj) (sexy-send-pair obj msg cont err))
        ((procedure? obj) (sexy-send-primitive obj msg cont err))
        ((vector? obj) (sexy-send-vector obj msg cont err))
        ((port? obj) (sexy-send-port obj msg cont err))
        ((hash-table? obj)
            (let ((t (htr obj 'type)))
                (case t
                    ((env)    (sexy-send-env obj msg cont err))
                    ((record) (sexy-send-record obj msg cont err))
                    ((fn)     (sexy-send-fn obj msg cont err))
                    ((operator)  (sexy-send-fn obj msg cont err))
                    (else (sexy-send-object obj msg cont err)))))
        ((eof-object? obj) (cont 'EOF))
        (else (wtf))))

(define (sexy-send-atomic obj msg)
    (sexy-send obj msg top-cont top-err))

(define (sexy-send-symbol obj msg cont err)
    (case msg
        ((view) (cont obj))
        ((to-text) (cont (symbol->string obj)))
        (else
            (case obj
                ((true) (sexy-send-bool #t msg cont err))
                ((false) (sexy-send-bool #f msg cont err))
                ((null) (sexy-send-null obj msg cont err))
                (else
                    (case msg
                        ((type) (cont 'symbol))
                        ((to-bool) (cont #t))
                        (else (idk obj msg cont err))))))))

(define (sexy-send-bool obj msg cont err)
    (case msg
        ((type) (cont 'bool))
        ((to-bool) (cont obj))
        ((to-text) (cont (if obj "true" "false")))
        ((view) (cont (if obj 'true 'false)))
        ((not) (cont (not obj)))
        (else (idk obj msg cont err))))

(define (sexy-send-null obj msg cont err)
    (case msg
        ((to-bool) (cont #f))
        ((to-text) (cont "null"))
        ((apply) (err 'null-is-not-applicable cont))
        (else (cont 'null))))

(define (sexy-send-number obj msg cont err)
    (case msg
        ((zero?) (cont (= obj 0)))
        ((pos?) (cont (> obj 0)))
        ((neg?) (cont (< obj 0)))
        ((abs) (cont (abs obj)))
        ((to-bool) (cont (not (= obj 0))))
        ((to-text) (cont (number->string obj)))
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
        ((inc) (cont (+ obj 1)))
        ((dec) (cont (- obj 1)))
        ((even?) (cont (even? obj)))
        ((odd?) (cont (odd? obj)))
        ((floor) (cont obj))
        ((ceil) (cont obj))
        ((round) (cont obj))
        ((truncate) (cont obj))
        (else (idk obj msg cont err))))
 
(define (sexy-send-real obj msg cont err)
    (case msg
        ((type) (cont 'real))
        ((floor) (cont (inexact->exact (floor obj))))
        ((ceil) (cont (inexact->exact (ceiling obj))))
        ((round) (cont (inexact->exact (round obj))))
        ((truncate) (cont (inexact->exact (truncate obj))))
        (else (idk obj msg cont err))))

(define (sexy-send-rune obj msg cont err)
    (case msg
        ((type) (cont 'rune))
        ((view) (cont obj))
        ((alpha?) (cont (char-alphabetic? obj)))
        ((digit?) (cont (char-numeric? obj)))
        ((whitespace?) (cont (char-whitespace? obj)))
        ((uc?) (cont (char-upper-case? obj)))
        ((lc?) (cont (char-lower-case? obj)))
        ((uc) (cont (char-upcase obj)))
        ((lc) (cont (char-downcase obj)))
        ((to-bool) (cont #t))
        ((to-number) (cont (char->integer obj)))
        ((to-text) (cont (string obj)))
        (else (idk obj msg cont err))))

(define (sexy-send-text obj msg cont err)
    (case msg
        ((type view clone to-bool to-symbol to-number to-list to-text size)
            (cont
                (case msg
                    ((type) 'text)
                    ((view) obj)
                    ((clone) (string-copy obj))
                    ((to-bool) (not (eq? (string-length obj) 0)))
                    ((to-symbol) (string->symbol obj))
                    ((to-number) (string->number obj))
                    ((to-list) (string->list obj))
                    ((to-text) obj)
                    ((size) (string-length obj)))))
        ((split)
            (cont
                (lambda (x)
                    (string-split obj x))))
        ((set!)
            (cont 
                (lambda (idx val)
                    (if (> idx (string-length obj))
                        (err (list 'out-of-bounds idx obj) cont)
                        (begin
                            (string-set! obj idx val)
                            obj)))))
        (else
            (if (number? msg)
                (if (> (string-length obj) msg)
                    (cont (string-ref obj msg))
                    (err (list 'out-of-bounds obj msg) cont))
                (idk obj msg cont err)))))

(define (sexy-send-empty obj msg cont err)
    (case msg
        ((type empty? view to-bool to-list head tail key val car cdr size)
            (cont
                (case msg
                    ((type) 'empty)
                    ((empty?) #t)
                    ((view) '())
                    ((to-bool) #f)
                    ((to-list) '())
                    ((head tail key val car cdr) 'null)
                    ((size) 0))))
        (else (sexy-send-pair obj msg cont err))))

(define (sexy-ho code obj cont err)
    (sexy-apply
        (sexy-compile-method code)
        (list obj)
        cont
        err))

(define (sexy-send-pair obj msg cont err)
    (case msg
        ((type empty? view to-bool to-list to-vector head key car tail val cdr size reverse has? append take drop apply)
            (cont
                (case msg
                    ((type) 'pair)
                    ((empty?) #f)
                    ((view)
                        (if (list? obj)
                            (map sexy-view obj)
                            (list '% (sexy-view (car obj)) (sexy-view (cdr obj)))))
                    ((to-bool) #t)
                    ((to-list) obj)
                    ((to-vector) (list->vector obj))
                    ((head key car) (car obj))
                    ((tail val cdr) (cdr obj))
                    ((cons) (lambda (v) (cons v obj)))
                    ((size) (length obj))
                    ((clone) (list-copy obj))
                    ((reverse) (reverse obj))
                    ((has?)
                        (lambda (item)
                            (if (member item obj)
                                #t
                                #f)))
                    ((append) (lambda (other) (append obj other)))
                    ((take) (lambda (n) (take obj n)))
                    ((drop) (lambda (n) (drop obj n)))
                    ((apply)
                        (sexy-proc
                            'primitive-function
                            'pair
                            (lambda (args opts cont err)
                                (sexy-send-pair obj (car args) cont err)))))))
        ((to-record)
            (if (not (every pair? obj))
                (err (list 'not-an-associative-list! obj 'to-record) cont)
                (let ((r (sexy-record)))
                    (define vars (htr r 'vars))
                    (for-each (lambda (p) (hts! vars (car p) (cdr p))) obj)
                    (cont r))))
        ((fold)
            (sexy-ho
                '(fn (xs)
                    (fn (acc funk)
                        (if xs.empty?
                            acc
                            (xs.tail.fold (funk acc xs.head) funk))))
                obj
                cont
                err))
        ((reduce)
            (sexy-ho
                '(fn (xs)
                    (fn (acc funk)
                        (if xs.empty?
                            acc
                            (funk xs.head (xs.tail.reduce acc funk)))))
                obj
                cont
                err))
        ((each)
            (sexy-ho
                '(fn (xs)
                    (fn (funk)
                        (if xs.empty?
                            null
                            (seq
                                (funk xs.head)
                                (xs.tail.each funk)))))
                obj
                cont
                err))
        ((map)
            (sexy-ho
                '(fn (xs)
                    (fn (funk)
                        (xs.reduce '() (fn (x y) (pair (funk x) y)))))
                obj
                cont
                err))
        ((filter)
            (sexy-ho
                '(fn (xs)
                    (fn (funk)
                        (xs.reduce '() (fn (x y) (if (funk x) (pair x y) y)))))
                obj
                cont
                err))
        ((sort)
            (sexy-ho
                '(fn (xs)
                    (fn (funk)
                        (def merge (fn (a b)
                            (if a.size.zero?
                                b
                                (if b.size.zero?
                                    a
                                    (if (funk a.head b.head)
                                        (pair a.0 (merge a.tail b))
                                        (pair b.0 (merge a b.tail)))))))
                        (def sort (fn (yarr)
                            (def len yarr.size)
                            (if (< len 2)
                                yarr
                                (seq
                                    (def half (send (/ len 2) 'floor))
                                    (merge (sort (yarr.take half)) (sort (yarr.drop half)))))))
                        (sort xs)))
                obj
                cont
                err))
        (else
            (if (number? msg)
                (if (> (length obj) msg)
                    (cont (list-ref obj msg))
                    (err (list 'out-of-bounds obj msg) cont))
                (idk obj msg cont err)))))

(define (sexy-send-primitive obj msg cont err)
    (cont 
        (case msg
            ((type) (cont 'fn))
            ((view) 'primitive-function)
            ((code) '0xDEADBEEF)
            ((to-bool) #t)
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
    (case msg
        ((type view size to-bool get put set! rm del! has? apply keys values pairs to-list to-plist merge)
            (cont
                (case msg
                    ((type) 'record)
                    ((view)
                        (let ((keys (htks vars)))
                            (cons ': 
                                (fold
                                    (lambda (p xs)
                                        (cons (car p) (cons (sexy-view (cdr p)) xs)))
                                    '()
                                    (hash-table->alist vars)))))
                    ((size) (hash-table-size vars))
                    ((to-bool)
                        (> (hash-table-size vars) 0))
                    ((get)
                        (lambda (k)
                            (if (hte? vars k)
                                (htr vars k)
                                'null)))
                    ((put)
                        (lambda args
                            (define noob (sexy-record))
                            (hts! noob 'vars (hash-table-copy vars))
                            (sexy-send-record
                                noob
                                'set!
                                (lambda (setter!)
                                    (apply setter! args)
                                    noob)
                                err)))
                    ((set!)
                        (lambda args
                            (for-pairs (lambda (k v) (hts! vars k v)) args)
                            'null))
                    ((rm)
                        (lambda args
                            (define noob (sexy-record))
                            (hts! noob 'vars (hash-table-copy vars))
                            (sexy-send-record
                                noob
                                'del!
                                (lambda (deleter!)
                                    (apply deleter! args)
                                    noob)
                                err)))
                    ((del!)
                        (lambda args
                            (map (lambda (k) (htd! vars k)) args)
                            'null))
                    ((has?)
                        (lambda (x)
                            (hte? vars x)))
                    ((apply)
                        (sexy-proc
                            'primitive-function
                            'record
                            (lambda (args opts cont err)
                                (sexy-send-record obj (car args) cont err))))
                    ((keys) (htks vars))
                    ((values) (htvs vars))
                    ((pairs to-list) (hash-table->alist vars))
                    ((to-plist)
                        (fold
                            (lambda (p xs)
                                (cons (symbol->keyword (car p)) (cons (cdr p) xs)))
                            '()
                            (hash-table->alist vars)))
                    ((merge)
                        (lambda (other)
                            (define nuvars (hash-table-merge (htr other 'vars) vars))
                            (define noob (mkht))
                            (hts! noob 'type 'record)
                            (hts! noob 'vars nuvars)
                            noob)))))
            ((fold) (sexy-send-pair
                        (hash-table->alist vars)
                        'fold
                        cont
                        err))
            ((reduce) (sexy-send-pair
                        (hash-table->alist vars)
                        'reduce
                        cont
                        err))
            ((map)
                (sexy-ho
                    '(fn (rec)
                        (fn (funk)
                            (def mapped (rec.to-list.map funk))
                            mapped.to-record))
                    obj
                    cont
                    err))
            ((filter) 
                (sexy-ho
                    '(fn (rec)
                        (fn (funk)
                            (def mapped (rec.to-list.filter funk))
                            mapped.to-record))
                    obj
                    cont
                    err))
            (else
                (if (hte? vars msg)
                    (cont (htr vars msg))
                    (cont 'null)))))

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
                ((type view) (cont 'object))
                ((to-bool) (cont (not (eq? 0 (length (hash-table-keys fields))))))
                ((=) (cont (lambda (other) #f)))
                ((apply) (cont (lambda args (sexy-send obj (car args) top-cont err))))
                ((responds?) (cont (lambda (x) (hte? fields x))))
                (else (sexy-apply (htr obj 'default) (list msg) cont err))))))

(define (sexy-send-fn obj msg cont err)
    (case msg
        ((type) (cont 'fn))
        ((view) (sexy-send obj 'code cont err))
        ((to-bool) (cont #t))
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
        ((get has? del! view to-bool pairs)
            (sexy-send-record (htr obj 'vars) msg cont err))
        ((type) (cont 'env))
        ((def!)
            (sexy-send-record (htr obj 'vars) 'set! cont err))
        ((set!)
            (cont
                (sexy-proc
                    'primitive-function
                    'env
                    (lambda (args opts cont err)
                        (if (not (eq? (length args) 2))
                            (err (list "set! requires 2 arguments!" args) cont)
                            (let ((name (car args)) (val (cadr args)))
                                (update!
                                    obj
                                    name
                                    val
                                    (lambda (v) (cont v))
                                    err)))))))
        ((put (niy)))
        ((lookup)
            (cont
                (sexy-proc
                    'primitive-function
                    'env
                    (lambda (args opts cont err)
                        (lookup
                            obj
                            (car args)
                            (lambda (val)
                                (cont
                                    (if (eq? val not-found)
                                        'null
                                        val)))
                            err)))))
        ((extend)
            (cont
                (sexy-proc
                    'primitive-function
                    'env
                    (lambda (args opts cont err)
                        (let loop ((names '()) (vals '()) (left args))
                            (if (eq? '() left)
                                (extend obj names vals cont err)
                                (loop (cons (car left) names) (cons (cadr left) vals) (cddr args))))))))
        ((mama) (cont (htr obj 'mama)))
        ((eval)
            (cont
                (lambda (code)
                    (sexy-eval code obj))))
        ((expand)
            (cont
                (lambda (code)
                    (sexy-expand code obj))))
        (else (idk obj msg cont err))))

(define (sexy-send-vector obj msg cont err)
    (case msg
        ((type view to-bool to-list pairs size has? set! apply)
            (cont 
                (case msg
                    ((type) 'vector)
                    ((view)
                        (cons '^
                            (map
                                sexy-view
                                (vector->list obj))))
                    ((to-bool) (not (eq? (vector-length obj) 0)))
                    ((to-list) (vector->list obj))
                    ((pairs) (vector->list (vector-map (lambda (i x) (cons i x)) obj)))
                    ((size) (vector-length obj))
                    ((has?)
                        (lambda (item)
                            (if (vector-index
                                    (lambda (x) (eq? x item))
                                    obj)
                                #t
                                #f)))
                    ((set!)
                        (lambda (idx val)
                            (if (> idx (vector-length obj))
                                (err (list 'out-of-bounds idx obj) cont)
                                (begin
                                    (vector-set! obj idx val)
                                    obj))))
                    ((apply)
                        (sexy-proc
                            'primitive-function
                            'pair
                            (lambda (args opts cont err)
                                (sexy-send-vector obj (car args) cont err)))))))
        ((fold)
            (sexy-ho
                '(fn (vec)
                    (fn (acc funk)
                        (vec.to-list.fold acc funk)))
                obj
                cont
                err))
        ((reduce)
            (sexy-ho
                '(fn (vec)
                    (fn (acc funk)
                        (vec.to-list.reduce acc funk)))
                obj
                cont
                err))
        ((map)
            (sexy-ho
                '(fn (vec)
                    (fn (funk)
                        (def mapped (vec.to-list.map funk))
                        mapped.to-vector))
                obj
                cont
                err))
        ((filter)
            (sexy-ho
                '(fn (vec)
                    (fn (funk)
                        (def mapped (vec.to-list.filter funk))
                        mapped.to-vector))
                obj
                cont
                err))
        ((sort)
            (sexy-ho
                '(fn (vec)
                    (fn (funk)
                        (def sorted (vec.to-list.sort funk))
                        sorted.to-vector))
                obj
                cont
                err))
        (else
            (if (number? msg)
                (if (> (vector-length obj) msg)
                    (cont (vector-ref obj msg))
                    (err (list 'out-of-bounds obj msg) cont))
                (idk obj msg cont err)))))

(define (sexy-read port)
    (sexy-parse (read port)))

(define (sexy-write obj port)
    (write (sexy-view obj) port))

(define (sexy-print obj port)
    (display (sexy-view obj) port))

(define (sexy-send-port obj msg cont err)
    (case msg
        ((type to-bool view read read-rune read-line to-list to-text to-sexy write print say nl close)
            (cont 
                (case msg
                    ((type) 'port)
                    ((to-bool) #t)
                    ((view) obj)
                    ((read) (sexy-read obj))
                    ((read-rune) (read-char obj))
                    ((read-line) (read-line obj))
                    ((to-list) (read-lines obj))
                    ((to-text) (read-string #f obj))
                    ((to-sexy) (sexy-read-file obj))
                    ((write) (lambda (x) (sexy-write x obj) 'null))
                    ((print) (lambda (x) (sexy-print x obj) 'null))
                    ((say) (lambda (x) (sexy-print x obj) (newline obj) 'null))
                    ((nl) (newline obj))
                    ((close) (begin
                        (if (input-port? obj)
                            (close-input-port obj)
                            (close-output-port obj))
                        'null))
                )))
        (else (idk msg obj cont err))))

(define (sexy-bool obj cont err)
    (sexy-send obj 'to-bool cont err))

    
; macro expansion

(define done-been-expanded (mkht))

(define (sexy-expand code env)
    (define (expand x)
        (sexy-expand x env))
    (define (look-it-up x)
        (if (sexy-global? x)
            (glookup x)
            (lookup env x top-cont top-err)))
    (define (sexy-macro? obj)
        (and (hash-table? obj) (eq? (htr obj 'type) 'operator)))
    (if (atom? code)
        code
        (let ((head (car code)))
            (case head
                ((load)
                    (let ((p (make-module-absolute-path (cadr code))))
                        (if (hte? done-been-expanded p)
                            (cons 'load (cons p (cddr code)))
                            (begin
                                (hts! done-been-expanded p #t)
                                (sexy-expand-load code env)))))
                ((def)
                    (let ((dval (caddr code)))
                        (if (pair? dval)
                            (let ((op (car dval)))
                                (case op
                                    ((fn operator) 
                                        (let* ((expanded (map expand (cdr code)))
                                               (nucode (cons 'def expanded)))
                                            ((sexy-compile nucode) env top-cont top-err)
                                            nucode))
                                    (else (cons 'def (map expand (cdr code))))))
                            (let ((nucode (cons 'def (map expand (cdr code)))))
                                ((sexy-compile nucode) env top-cont top-err)
                                nucode))))
                ((seq)
                    (let ((expanded (map expand code)))
                        (prep-defs (cdr expanded) env top-cont top-err)
                        expanded))
                ((quote) code)
                ((syntax)
                    (let ((syn-fn
                            (lambda ()
                                (apply sexy-record (cdr code))))
                          (setter! (sexy-apply-wrapper (sexy-send-atomic env 'def!))))
                        (setter! 'syntax syn-fn))
                    'null)
                ((fn operator)
                    (let ((noob (sexy-environment env)))
                        (cons head (sexy-expand (cdr code) noob))))
                (else 
                    (if (symbol? head)
                        (let ((obj (look-it-up head)))
                            (if (sexy-macro? obj)
                                (sexy-expand
                                    (apply (sexy-apply-wrapper obj) (cdr code))
                                    env)
                                (map expand code)))
                        (map expand code)))))))

(define (sexy-expand-load code env)
    (define arg-pair (prepare-sexy-args (cdr code)))
    (define args (car arg-pair))
    (define opts (cdr arg-pair))
    (define path (car args))
    (define abs-path (make-module-absolute-path path))
    (define prog-env (local-env))
    (define prog
        (if (or (symbol? path) (string? path))
            (read-expand-cache-prog path prog-env)
            (else (sexy-error code "load: path must be a symbol or a string."))))
    (define load-err
        (lambda (e cont)
            (debug 'LOAD-ERROR e)
            (exit)))
    (define (looker name)
        (lookup prog-env name top-cont load-err))
    (define exporter (looker 'syntax))
    (if (not (eq? exporter not-found))
        (let ()
            (define syn-rec (exporter))
            (define names (sexy-send-atomic syn-rec 'keys))
            (define (set-em! k)
                (define defr! (sexy-send-atomic env 'def!))
                (define op-val (looker ((sexy-send-atomic syn-rec 'get) k)))
                (defr! k op-val))
            (map set-em! names))
        #f)
    (cons 'load (cons abs-path (cddr code))))


; eval/apply

(define (sexy-eval code env)
    (define macro-env
        (sexy-environment env))
    (define prog
        (sexy-compile (sexy-expand code macro-env)))
    (prog env top-cont top-err))

(define (sexy-apply obj xs cont err)
    (define (apply-or-die)
        (sexy-send obj 'apply
            (lambda (af)
                (sexy-apply af xs cont err))
              err))
    (cond
        ((procedure? obj)
            (cont 
                (handle-exceptions exn
                    (err
                        (list
                            'exn
                            (list 'location  ((condition-property-accessor 'exn 'location) exn))
                            (list 'arguments ((condition-property-accessor 'exn 'arguments) exn))
                            (list 'message   ((condition-property-accessor 'exn 'message) exn)))
                        (lambda (ys) (cont (apply obj ys))))
                    (apply obj xs))))
        ((hash-table? obj)
            (let ((type (htr obj 'type)))
                (if (or (eq? type 'fn) (eq? type 'operator))
                    (let* ((arg-pair (prepare-sexy-args xs)) (args (car arg-pair)) (opts (cdr arg-pair)))
                        ((htr obj 'exec) args opts cont err))
                    (apply-or-die))))
        (else (apply-or-die))))

(define (sexy-apply-wrapper obj)
    (lambda xs
        (sexy-apply obj xs top-cont top-err)))

(define (prep-defs seq env cont err)
    ; predefine all defs for mutual recursion
    (define (get-names seq)
        (map
            cadr
            (filter
                (lambda (x)
                    (and (pair? x)
                         (or
                            (eq? (car x) 'macro)
                            (eq? (car x) 'operator)
                            (eq? (car x) 'fun)
                            (eq? (car x) 'def))))
                seq)))
    (define names (get-names seq))
    (define margs (flatten (zip names (make-list (length names) will-exist))))
    (apply mutate! (cons env (cons cont (cons err margs)))))

(define-syntax frag
    (ir-macro-transformer
        (lambda (expr inject compare)
            (let ((body (cdr expr)))
                `(lambda (,(inject 'env) ,(inject 'cont) ,(inject 'err)) ,@body)))))

(define blessed
    '(def quote if seq set! operator fn wall gate capture ensure guard error env opt rest return))

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
            ((def)      (sexy-compile-def code))
            ((quote)    (sexy-compile-quote code))
            ((if)       (sexy-compile-if code))
            ((seq)      (sexy-compile-seq code))
            ((set!)     (sexy-compile-set! code))
            ((operator) (sexy-compile-operator code))
            ((fn)       (sexy-compile-fn code))
            ((wall)     (sexy-compile-wall code))
            ((gate)     (sexy-compile-gate code))
            ((capture)  (sexy-compile-capture code))
            ((guard)    (sexy-compile-guard code))
            ((error)    (sexy-compile-error code))
            ((ensure)   (sexy-compile-ensure code))
            ((load)     (sexy-compile-load code))
            (else       (sexy-compile-application code)))))

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
                                        (err (cons 'undefined-symbol code) cont)
                                        (cont v)))
                                err))))))
        pass))

(define (sexy-compile-def code)
    (define name (cadr code))
    (define val (caddr code))
    (if (not (symbol? name))
        (sexy-error "def expects it's first argument to be a symbol.  Got " code)
        (if (holy? name)
            (blasphemy code name)
            (frag
                (sexy-send-env env 'has?
                    (lambda (haz?)
                        (sexy-send-env env 'get
                            (lambda (getter)
                                (if (and
                                        (haz? name)
                                        (not (eq? will-exist (getter name))))
                                    (err (list 'bad-def code name " is already defined in the local environment.") cont)
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
                    err)))))

(define (sexy-compile-set! code)
    (define name (cadr code))
    (define val (caddr code))
    (define val-c (sexy-compile val))
    (if (symbol? name)
        (if (holy? name)
            (blasphemy code name)
            (frag
                (lookup
                    env
                    name
                    (lambda (v)
                        (if (eq? v not-found)
                            (err (list 'symbol-not-defined name) cont)
                            (val-c
                                env
                                (lambda (v)
                                    (update!
                                        env
                                        name
                                        v
                                        (lambda (null)
                                            (cont v))
                                        err))
                                err)))
                    err)))
        (sexy-error code "set! wants a symbol as its first argument!")))

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

(define (make-sexy-proc code env formals bodies)
    (define arity (length formals))
    (define bodies-c (sexy-seq-subcontractor bodies #t))
    (if (pair? formals)
        (let loop ((f (car formals)) (fs (cdr formals)))
            (if (holy? f)
                (blasphemy code f)
                (if (pair? fs)
                    (loop (car fs) (cdr fs))
                    #f))))
    (sexy-proc
        code
        env 
        (lambda (args opts cont err)
            (if (< (length args) arity)
                (err (list 'arity code (sprintf "Procedure requires ~A arguments. Given: " arity) args) cont)
                (let* ((fargs (if (pair? args) (take args arity) '()))
                       (the-rest (if (pair? args) (drop args arity) '()))
                       (returner (lambda (v) (cont v))))
                       (extend
                            env 
                            (append formals '(opt rest return))
                            (append fargs (list opts the-rest returner))
                            (lambda (noob)
                                (bodies-c noob cont err))
                            err))))))

(define (sexy-compile-fn code)
    (define formals (cadr code))
    (define bodies (cddr code))
    (frag
        (cont (make-sexy-proc code env formals bodies))))

(define (sexy-compile-operator code)
    (define formals (cadr code))
    (define bodies (cddr code))
    (frag
        (define thing (make-sexy-proc code env formals bodies))
        (hts! thing 'type 'operator)
        (cont thing)))

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
    (define lamb (cons 'fn (cons (list name) (cddr code))))
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
    (define exprs (cddr code))
    (define handler-c (sexy-compile handler))
    (define expr-c (sexy-seq-subcontractor exprs #t))
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
    (define expr-c (sexy-seq-subcontractor (cddr code) #t))
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

(define (sexy-compile-load code)
    (define path (cadr code))
    (define module (if (hte? sexy-modules path) (htr sexy-modules path) (lambda args 'null)))
    (define load-env (local-env))
    (define args-c (sexy-compile-list (cddr code)))
    (frag 
        (args-c
            env
            (lambda (args)
                (module load-env top-cont top-err)
                (lookup load-env 'sexy-library-export-function
                    (lambda (exporter)
                        (if (eq? exporter not-found)
                            (cont (lambda args 'null))
                            (cont
                                (sexy-apply exporter args top-cont top-err))))
                    top-err))
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
(define g-has? #f)
(define g-get  #f)

(define load-symbols-env #f)
(define loaded-module-symbols)

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

(define global-arg-pair (sexy-cli-args (command-line-arguments)))

(define (cli-env)
    (define lenv (local-env))
    (define sys
        (sexy-object
            (list
                'stdin   (current-input-port)
                'stdout  (current-output-port)
                'stderr  (current-error-port)
                'env
                    (sexy-object
                        (list
                            'get
                                (lambda (x)
                                    (define envt (get-environment-variables))
                                    (define (try z)
                                        (define p (assoc z envt))
                                        (if p (cdr p) 'null))
                                    (if (symbol? x)
                                        (let ((y (symbol->string x)))
                                            (try y))
                                        (try x)))
                            'set!
                                (lambda (k v)
                                    (if (symbol? k)
                                        (setenv (symbol->string k) v)
                                        (setenv k v))
                                        v)
                            'del!
                                (lambda (k)
                                    (if (symbol? k)
                                        (unsetenv (symbol->string k))
                                    (unsetenv k))
                                    'null)
                        )
                        #f #f #f)
                'exit exit
                'srand
                    (lambda (v)
                        (randomize v)
                        'null)
                'file
                    (sexy-object
                        (list
                            'open
                                (sexy-object
                                    (list
                                        'in open-input-file
                                        'out open-output-file
                                    )
                                    #f #f #f)
                            'with
                                (sexy-object
                                    (list
                                        'in (sexy-proc
                                                'primitive-function
                                                'sys
                                                (lambda (args opts cont err)
                                                    (call-with-input-file (car args)
                                                        (lambda (f)
                                                            (sexy-apply (cadr args) (list f) cont err)))))
                                        'out (sexy-proc
                                                'primitive-function
                                                'sys
                                                (lambda (args opts cont err)
                                                    (call-with-output-file (car args)
                                                        (lambda (f)
                                                            (sexy-apply (cadr args) (list f) cont err)))))
                                    )
                                    #f #f #f)
                        )
                        #f #f #f)
                'socket
                    (sexy-object
                        (list)
                        #f #f #f)
                '64764 (lambda () (display "\n    **** COMMODORE 64 BASIC V2 ****\n\n 64K RAM SYSTEM  38911 BASIC BYTES FREE\n\n") 'READY.)
                'ts (lambda () (inexact->exact (current-seconds)))
                'uname (system-information)
                'hostname (get-host-name)
                'sleep (lambda (s) (sleep s))
                'shell (lambda (cmd)
                    (read-all (process cmd)))
                'run (lambda (cmd) (process-run cmd))
                'signal (lambda (pid sig) (process-signal pid sig))
                'fork (lambda (thunk) (process-fork thunk))
                'pid (lambda () (current-process-id))
                'uid (lambda () (current-user-id))
                'gid (lambda () (current-group-id))
                'parent-pid (lambda () (parent-process-id))
                'process-gid (lambda (pid) (process-group-id pid))
                'read
                    (sexy-proc
                        'primitive-function
                        'sys
                        (lambda (args opts cont err)
                            (sexy-send sys 'stdin
                                (lambda (in)
                                    (cont (sexy-read in)))
                                err)))
                'write
                    (sexy-proc
                        'primitive-function
                        'sys
                        (lambda (args opts cont err)
                            (sexy-send sys 'stdout
                                (lambda (out)
                                    (sexy-write (car args) out)
                                    (cont 'null))
                                err)))
                'print
                    (sexy-proc
                        'primitive-function
                        'sys
                        (lambda (args opts cont err)
                            (sexy-send sys 'stdout
                                (lambda (out)
                                    (sexy-print (car args) out)
                                    (cont 'null))
                                err)))
                'carp
                    (sexy-proc
                        'primitive-function
                        'sys
                        (lambda (args opts cont err)
                            (sexy-send sys 'stderr
                                (lambda (stderr)
                                    (sexy-print (car args) stderr)
                                    (newline stderr)
                                    (cont 'null))
                                err)))
                'say
                    (sexy-proc
                        'primitive-function
                        'sys
                        (lambda (args opts cont err)
                            (sexy-send sys 'print
                                (lambda (printer)
                                    (sexy-apply printer args
                                        (lambda (x)
                                            (newline)
                                            (cont 'null))
                                        err))
                                err)))
                'test
                    (lambda (tname ok)
                        (debug tname (if ok 'ok 'FAIL))
                        'null))
            '(ts uid gid pid parent-pid process-gid exit 64764)
            #f
            #f))
    (extend lenv
        '(opt rest sys)
        (list
            (cdr global-arg-pair)
            (car global-arg-pair)
            sys)
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
                (cons 'real? real?)
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
                            'e   2.71828182845904
                            'phi 1.61803398874989
                            'pi  3.14159265358979
                            'tau 6.28318530717959
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
                (cons 'json
                    (sexy-object
                        (list
                            'parse
                                (lambda (json-str)
                                    (define schemified (read-json json-str))
                                    (define (revise obj)
                                        (cond
                                            ((vector? obj)
                                                (vector-map
                                                    (lambda (i x) (revise x))
                                                    obj))
                                            ((list? obj)
                                                (let ((rec (sexy-record)))
                                                    (hts! rec 'vars
                                                        (alist->hash-table
                                                            (map
                                                                (lambda (x) (cons (car x) (revise (cdr x))))
                                                                obj)))
                                                    rec))
                                            (else obj)))
                                    (revise schemified))
                            'stringify
                                (lambda (obj)
                                    (define (revise x)
                                        (cond
                                            ((char? x) (string x))
                                            ((list? x) (revise (list->vector x)))
                                            ((vector? x)
                                                (vector-map
                                                    (lambda (i y) (revise y))
                                                    x))
                                            ((hash-table? x)
                                                (let ((t (htr x 'type)))
                                                    (if (eq? t 'record)
                                                        (let ((pairs (hash-table->alist (htr x 'vars))))
                                                            (map 
                                                                (lambda (x) (cons (revise (car x)) (revise (cdr x))))
                                                                pairs))
                                                        (error "json.stringify: I don't know how to stringify this object!" x))))
                                            (else x)))
                                    (json->string (revise obj)))
                        )
                        #f
                        #f
                        #f))
                (cons 'gensym sexy-gensym)
                (cons 'uuid uuid-v4)
                (cons 'parse
                    (lambda (code-str)
                        (sexy-read-file
                            (open-input-string code-str))))
                (cons 'cat
                    (lambda (separator . args)
                        (define strings (map (lambda (x) (sexy-send-atomic x 'to-text)) args))
                        (string-join strings separator)))
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

(define global-prelude-file "global.sex")

(define (add-global-prelude)
    (define global-prelude-text (cdr (read-expand-cache-prog global-prelude-file (local-env))))
    (define prelude-c (sexy-seq-subcontractor global-prelude-text #t))
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

(define sexy-modules (mkht))

(define (def-sexy-module path)
    (define has? (hte? sexy-modules path))
    (if has?
        #f
        (let ((expanded (read-expand-cache-prog path (local-env))))
            (hts! sexy-modules path 'loading)
            (sexy-eval-module expanded path))))

(define (sexy-run program)
    (if (pair? program)
        (let ((mods (cdar program)))
            (map def-sexy-module mods)
            ((sexy-seq-subcontractor (cdr program) #t)
                (cli-env)
                (lambda (v) (exit))
                top-err))
        (exit)))

(define (sexy-eval-module program path)
    (define nop (lambda args 'null))
    (if (pair? program)
        (let ((mods (cdar program)))
            (map def-sexy-module mods)
            (hts!
                sexy-modules
                path
                (sexy-seq-subcontractor (cdr program) #t)))
        (hts! sexy-modules path nop)))

(define (sexy-repl)
    (define stdin (current-input-port))
    (define stdout (current-output-port))
    (define stderr (current-error-port))
    (define (loop env)
        (define repl-err
            (lambda (ex continue)
                (debug "Uncaught error: " ex)
                (loop env)))
        (display "(sexy) ")
        (let ((expr (sexy-read stdin))) 
            (define compiled
                (sexy-compile
                    (sexy-expand expr (sexy-environment env))))
            (compiled
                env
                (lambda (v)
                    (define noob   (local-env))
                    (define mom    (htr env 'mama))
                    (define evars  (htr env 'vars))
                    (define mvars  (htr mom 'vars))
                    (sexy-send-record mvars 'merge
                        (lambda (fn)
                            (define nuvars (fn evars))
                            (hts! mom  'vars nuvars)
                            (hts! noob 'mama mom)
                            (if (eof-object? v)
                                (exit)
                                (begin
                                    (sexy-write v stdout)
                                    (newline)
                                    (loop noob))))
                        repl-err))
                repl-err)))
    (newline)
    (display "Welcome to the Sexy Read-Eval-Print Loop.  Press Ctrl-D to exit.")
    (newline)
    (newline)
    (loop (sexy-environment (cli-env))))

(start)

