
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
        ((eof-object? obj) (sexy-send-eof obj msg cont err))
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
        ((view)
            (cont
                (case obj
                    ((#\space) '$space)
                    ((#\newline) '$lf)
                    ((#\return) '$cr)
                    ((#\tab) '$tab)
                    (else (string->symbol (list->string (list #\$ obj)))))))
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
    (define (build-regex re flags)
        (define opts
            (append
                (list re 'fast 'utf8)
                (filter
                    (lambda (x) (not (eq? x 'g)))
                    (map string->symbol (string-split flags "")))))
        (apply irregex opts))
    (case msg
        ((type view clone to-bool to-symbol to-keyword to-number to-list to-text to-port size chomp index take drop trim ltrim rtrim lpad rpad)
            (cont
                (case msg
                    ((type) 'text)
                    ((view) obj)
                    ((clone) (string-copy obj))
                    ((to-bool) (not (eq? (string-length obj) 0)))
                    ((to-symbol) (string->symbol obj))
                    ((to-keyword) (string->keyword obj))
                    ((to-number) (string->number obj))
                    ((to-list) (string->list obj))
                    ((to-vector) (list->vector (string->list obj)))
                    ((to-text) obj)
                    ((to-port) (open-input-string obj))
                    ((take) (lambda (n) (string-take obj n)))
                    ((drop) (lambda (n) (string-drop obj n)))
                    ((trim) (string-trim-both obj))
                    ((ltrim) (string-trim obj))
                    ((rtrim) (string-trim-right obj))
                    ((lpad) (lambda (rune n) (string-pad obj n rune)))
                    ((rpad) (lambda (rune n) (string-pad-right obj n rune)))
                    ((chomp) (string-chomp obj))
                    ((index) (lambda (which) (substring-index which obj)))
                    ((size) (string-length obj)))))
        ((split)
            (cont
                (sexy-proc
                    'primitive-function
                    'text
                    (lambda (args opts cont err)
                        (define flags (sexy-send-atomic opts 'flags))
                        (define re (build-regex (car args) (if (eq? 'null flags) "" flags)))
                        (cont (irregex-split re obj))))))
        ((match)
            (cont
                (sexy-proc
                    'primitive-function
                    'text
                    (lambda (args opts cont err)
                        (define flags (sexy-send-atomic opts 'flags))
                        (define re (build-regex (car args) (if (eq? 'null flags) "" flags)))
                        (define rez (irregex-search re obj))
                        (cont 
                            (if rez
                                #t
                                #f))))))
        ((capture)
            (cont
                (sexy-proc
                    'primitive-function
                    'text
                    (lambda (args opts cont err)
                        (define flags (sexy-send-atomic opts 'flags))
                        (define re (build-regex (car args) (if (eq? 'null flags) "" flags)))
                        (cont
                            (irregex-fold
                                re
                                (lambda (idx match acc)
                                    (define n (irregex-match-num-submatches match))
                                    (let loop ((this n) (matches '()))
                                        (if (= this 0)
                                            (cons matches acc)
                                            (loop (- this 1) (cons (irregex-match-substring match this) matches)))))
                                '()
                                obj
                                (lambda (idx acc) (reverse acc))))))))
        ((replace)
            (cont
                (sexy-proc
                    'primitive-function
                    'text
                    (lambda (args opts cont err)
                        (define fopt (sexy-send-atomic opts 'flags))
                        (define flags (if (eq? 'null fopt) "" fopt))
                        (define re (build-regex (car args) flags))
                        (cont
                            (if (string-contains flags "g")
                                (apply irregex-replace/all (cons re (cons obj (cdr args))))
                                (apply irregex-replace (cons re (cons obj (cdr args))))))))))
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
        ((type empty? view to-bool to-list to-text to-vector head key car tail val cdr size reverse has? append take drop apply)
            (cont
                (case msg
                    ((type) 'pair)
                    ((empty?) #f)
                    ((view)
                        (if (list? obj)
                            (map sexy-view obj)
                            (list (string->keyword "pair") (sexy-view (car obj)) (sexy-view (cdr obj)))))
                    ((to-bool) #t)
                    ((to-list) obj)
                    ((to-text) (list->string obj))
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
        ((type view size clone to-bool get put set! rm del! has? apply keys values pairs to-list to-plist merge)
            (cont
                (case msg
                    ((type) 'record)
                    ((view)
                        (let ((keys (htks vars)))
                            (cons
                                (string->keyword "record")
                                (fold
                                    (lambda (p xs)
                                        (cons (car p) (cons (sexy-view (cdr p)) xs)))
                                    '()
                                    (hash-table->alist vars)))))
                    ((size) (hash-table-size vars))
                    ((clone)
                        (let ((noob (sexy-record)))
                            (hts! noob 'vars (hash-table-copy vars))
                            noob))
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
        ((type view to-bool to-list pairs size clone has? set! apply)
            (cont 
                (case msg
                    ((type) 'vector)
                    ((view)
                        (cons (string->keyword "vector")
                            (map
                                sexy-view
                                (vector->list obj))))
                    ((to-bool) (not (eq? (vector-length obj) 0)))
                    ((to-list) (vector->list obj))
                    ((to-text) (list->string (vector->list obj)))
                    ((pairs) (vector->list (vector-map (lambda (i x) (cons i x)) obj)))
                    ((size) (vector-length obj))
                    ((clone) (vector-copy obj))
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

(define (sexy-send-port obj msg cont err)
    (case msg
        ((type view to-bool input? output? open?)
            (cont 
                (case msg
                    ((type) 'port)
                    ((view) obj)
                    ((to-bool) #t)
                    ((input?) (input-port? obj))
                    ((output?) (output-port? obj))
                    ((open?) (not (port-closed? obj))))))
        (else
            (if (input-port? obj)
                (sexy-send-input-port obj msg cont err) 
                (sexy-send-output-port obj msg cont err)))))

(define (sexy-send-input-port obj msg cont err)
    (case msg
        ((read read-rune peek-rune read-line assert-rune skip skip-while skip-until
          read-token read-token-while read-token-until read-token-if to-list to-text to-sexy)
            (if (port-closed? obj)
                (err (list 'input-port-closed obj msg) cont)
                (cont 
                    (case msg
                        ((read) (sexy-read obj))
                        ((read-rune) (read-char obj))
                        ((peek-rune) (peek-char obj))
                        ((read-line) (read-line obj))
                        ((assert-rune)
                            (sexy-proc
                                'primitive-function
                                'port
                                (lambda (args opts cont err)
                                    (if (not (= 1 (length args)))
                                        (err '(assert-rune "requires one text argument") cont)
                                        (let ((runes (string->list (car args))))
                                            (define next (read-char obj))
                                            (if (member next runes)
                                                (cont next)
                                                (err (list 'assert-rune next (car args) "Assertion FAIL") cont)))))))
                        ((skip)
                            (lambda (n)
                                (read-string n obj)
                                'null))
                        ((skip-while)
                            (lambda (s)
                                (define runes (string->list s))
                                (let loop ((tok (peek-char obj)))
                                    (if (member tok runes)
                                        (begin
                                            (read-char obj)
                                            (loop (peek-char obj)))
                                        'null))))
                        ((skip-until)
                            (lambda (s)
                                (define runes (string->list s))
                                (let loop ((tok (peek-char obj)))
                                    (if (member tok runes)
                                        'null
                                        (begin
                                            (read-char obj)
                                            (loop (peek-char obj)))))))
                        ((read-token)
                            (lambda (n)
                                (read-string n obj)))
                        ((read-token-while)
                            (lambda (s)
                                (define runes (string->list s))
                                (let loop ((tok (peek-char obj)) (acc '()))
                                    (if (member tok runes)
                                        (let ((t (read-char obj)))
                                            (loop (peek-char obj) (cons t acc)))
                                        (list->string (reverse acc))))))
                        ((read-token-until)
                            (lambda (s)
                                (define runes (string->list s))
                                (let loop ((tok (peek-char obj)) (acc '()))
                                    (if (member tok runes)
                                        (list->string (reverse acc))
                                        (let ((t (read-char obj)))
                                            (loop (peek-char obj) (cons t acc)))))))
                        ((read-token-if)
                            (sexy-proc
                                'primitive-function
                                'env
                                (lambda (args opts cont err)
                                    (if (not (= 1 (length args)))
                                        (err '(read-token-if "requires one function argument.") cont)
                                        (let ((pred (car args)))
                                            (let loop ((tok (peek-char obj)) (acc '()))
                                                (sexy-apply
                                                    pred
                                                    (list tok)
                                                    (lambda (rv)
                                                        (sexy-bool
                                                            rv
                                                            (lambda (ok)
                                                                (if ok
                                                                    (let ((t (read-char obj)))
                                                                        (loop (peek-char obj) (cons t acc)))
                                                                    (cont (list->string (reverse acc)))))
                                                            err))
                                                    err)))))))
                        ((to-list) (read-lines obj))
                        ((to-text) (read-string #f obj))
                        ((to-sexy) (sexy-read-file obj))))))
        ((close) (close-input-port obj) (cont 'null))
        (else (idk msg obj cont err))))

(define (sexy-send-output-port obj msg cont err)
    (case msg
        ((write print say nl)
            (if (port-closed? obj)
                (err (list 'output-port-closed obj msg) cont)
                (cont
                    (case msg
                        ((write)
                            (lambda (x)
                                (sexy-write x obj)
                                'null))
                        ((print)
                            (lambda (x)
                                (sexy-print x obj)
                                'null))
                        ((say)
                            (lambda (x)
                                (sexy-print x obj)
                                (newline obj)
                                'null))
                        ((nl) (newline obj) 'null)))))
        ((flush) (flush-output obj) (cont 'null))
        ((close) (close-output-port obj) (cont 'null))
        (else (idk msg obj cont err))))

(define (sexy-send-eof obj msg cont err)
    (case msg
        ((type) (cont 'EOF))
        ((view) (cont 'EOF))
        ((to-bool) (cont #f))
        ((to-text) (cont "END OF LINE."))
        ((apply) (err 'eof-is-not-applicable cont))
        (else (idk msg obj cont err))))


