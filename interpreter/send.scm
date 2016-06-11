
(define default-default
    (sexy-proc
        primitive-type
        'proc
        (lambda (args opts cont err)
             (err (sexy-error-object 'message-not-understood '(send obj msg) "Message not understood.") cont))))

(define (sexy-send obj msg cont err)
    (define (wtf)
        (write (list `(send ,obj ,msg) "Unknown object."))
        (newline)
        (cont 'WTF))
    (cond
        ((boolean? obj) (sexy-send-bool obj msg cont err))
        ((symbol? obj) (sexy-send-symbol obj msg cont err))
        ((number? obj) (sexy-send-number obj msg cont err))
        ((char? obj) (sexy-send-rune obj msg cont err))
        ((string? obj) (sexy-send-text obj msg cont err))
        ((null? obj) (sexy-send-empty obj msg cont err))
        ((list? obj) (sexy-send-list obj msg cont err))
        ((pair? obj) (sexy-send-pair obj msg cont err))
        ((procedure? obj) (sexy-send-primitive obj msg cont err))
        ((vector? obj) (sexy-send-vector obj msg cont err))
        ((port? obj) (sexy-send-stream obj msg cont err))
        ((hash-table? obj)
            (let ((t (htr obj 'type)))
                (case t
                    ((env)    (sexy-send-env obj msg cont err))
                    ((record) (sexy-send-record obj msg cont err))
                    ((λ proc operator) (sexy-send-proc obj msg cont err))
                    (else (sexy-send-object obj msg cont err)))))
        ((eof-object? obj) (sexy-send-eof obj msg cont err))
        (else (wtf))))

(define (sexy-send-atomic obj msg)
    (sexy-send obj msg top-cont top-err))

(define (sexy-send-symbol obj msg cont err)
    (define msgs '(view to-text to-bool))
    (case msg
        ((autos) (cont '(view to-bool to-text)))
        ((resends) (cont '()))
        ((default) (cont default-default))
        ((view) (cont obj))
        ((to-symbol)
            (cont
                (if (keyword? obj)
                    (keyword->symbol obj)
                    obj)))
        ((to-text) (cont (symbol->string obj)))
        (else
            (case obj
                ((true)  (sexy-send-bool #t msg cont err))
                ((false) (sexy-send-bool #f msg cont err))
                ((null)  (sexy-send-null obj msg cont err))
                (else
                    (case msg
                        ((type) (cont 'symbol))
                        ((to-bool) (cont #t))
                        ((messages) (cont msgs))
                        ((responds?) (cont (lambda (msg) (if (member msg msgs) #t #f))))
                        (else (idk obj msg cont err))))))))

(define (sexy-send-bool obj msg cont err)
    (define msgs '(view to-text to-bool to-symbol not))
    (case msg
        ((type) (cont 'bool))
        ((autos) (cont '(view to-bool to-text to-symbol)))
        ((resends) (cont '()))
        ((default) (cont default-default))
        ((to-bool) (cont obj))
        ((view to-symbol) (cont (if obj 'true 'false)))
        ((to-text) (cont (if obj "true" "false")))
        ((not) (cont (not obj)))
        ((messages) (cont msgs))
        ((responds?) (cont (lambda (msg) (if (member msg msgs) #t #f))))
        (else (idk obj msg cont err))))

(define (sexy-send-null obj msg cont err)
    (define msgs '(view to-text to-bool to-symbol))
    (case msg
        ((to-bool) (cont #f))
        ((apply) (err (sexy-error-object 'null-is-not-applicable '(null ...) "Null can not be used as a procedure.") cont))
        ((messages) (cont msgs))
        ((responds?) (cont (lambda (msg) #t)))
        (else (cont 'null))))

(define (sexy-send-number obj msg cont err)
    (case msg
        ((zero?) (cont (= obj 0)))
        ((pos?) (cont (> obj 0)))
        ((neg?) (cont (< obj 0)))
        ((abs) (cont (abs obj)))
        ((to-bool) (cont (not (= obj 0))))
        ((to-text) (cont (number->string obj)))
        ((view to-number) (cont obj))
        ((resends) (cont '()))
        ((default) (cont default-default))
        (else
            (cond
                ((integer? obj) (sexy-send-int obj msg cont err))
                ((number? obj) (sexy-send-real obj msg cont err))
                (else (idk obj msg cont err))))))

(define (sexy-send-int obj msg cont err)
    (define msgs '(view to-text to-bool zero? pos? neg? abs floor ceil round truncate inc dec even? odd?))
    (case msg
        ((type) (cont 'int))
        ((autos) (cont '(view to-bool to-text zero? pos? neg? abs floor ceil round truncate inc dec even? odd?)))
        ((inc) (cont (+ obj 1)))
        ((dec) (cont (- obj 1)))
        ((even?) (cont (even? obj)))
        ((odd?) (cont (odd? obj)))
        ((floor) (cont obj))
        ((ceil) (cont obj))
        ((round) (cont obj))
        ((truncate) (cont obj))
        ((messages) (cont msgs))
        ((responds?) (cont (lambda (msg) (if (member msg msgs) #t #f))))
        (else (idk obj msg cont err))))
 
(define (sexy-send-real obj msg cont err)
    (define msgs '(view to-text to-bool zero? pos? neg? abs floor ceil round truncate))
    (case msg
        ((type) (cont 'number))
        ((floor) (cont (inexact->exact (floor obj))))
        ((ceil) (cont (inexact->exact (ceiling obj))))
        ((round) (cont (inexact->exact (round obj))))
        ((truncate) (cont (inexact->exact (truncate obj))))
        ((autos) (cont '(view to-bool to-text zero? pos? neg? abs floor ceil round truncate)))
        ((messages) (cont msgs))
        ((responds?) (cont (lambda (msg) (if (member msg msgs) #t #f))))
        (else (idk obj msg cont err))))

(define (sexy-send-rune obj msg cont err)
    (define msgs '(view code to-rune to-text to-bool to-number alpha? digit? whitespace? uc? lc? uc lc))
    (case msg
        ((type) (cont 'rune))
        ((autos) (cont '(view code to-bool to-rune to-text to-number alpha? digit? whitespace? uc? lc? uc lc)))
        ((resends) (cont '()))
        ((default) (cont default-default))
        ((view)
            (cont
                (list 'rune: 
                    (case obj
                        ((#\space) "space")
                        ((#\newline) "lf")
                        ((#\return) "cr")
                        ((#\tab) "tab")
                        (else (string obj))))))
        ((code) (cont (char->integer obj)))
        ((alpha?) (cont (char-alphabetic? obj)))
        ((digit?) (cont (char-numeric? obj)))
        ((whitespace?) (cont (char-whitespace? obj)))
        ((uc?) (cont (char-upper-case? obj)))
        ((lc?) (cont (char-lower-case? obj)))
        ((uc) (cont (char-upcase obj)))
        ((lc) (cont (char-downcase obj)))
        ((to-bool) (cont #t))
        ((to-number) (cont (string->number (string obj))))
        ((to-text) (cont (string obj)))
        ((to-rune) (cont obj))
        ((messages) (cont msgs))
        ((responds?) (cont (lambda (msg) (if (member msg msgs) #t #f))))
        (else (idk obj msg cont err))))

(define (sexy-send-text obj msg cont err)
    (define msgs
        '(view clone to-bool to-symbol to-keyword to-number
          to-list to-text to-stream size chomp index take drop
          trim ltrim rtrim lpad rpad set! split match capture replace))
    (define (build-regex re flags)
        (define opts
            (append
                (list re 'fast 'utf8)
                (filter
                    (lambda (x) (not (eq? x 'g)))
                    (map string->symbol (string-split flags "")))))
        (apply irregex opts))
    (case msg
        ((type view autos resends default clone to-bool to-symbol to-keyword to-number to-list to-text to-stream size chomp index uc lc take drop trim ltrim rtrim lpad rpad messages responds?)
            (cont
                (case msg
                    ((type) 'text)
                    ((view) obj)
                    ((autos) '(view to-bool to-symbol to-text to-keyword to-number to-list to-stream size chomp ltrim rtrim trim))
                    ((resends) '())
                    ((default) default-default)
                    ((clone) (string-copy obj))
                    ((to-bool) (not (eq? (string-length obj) 0)))
                    ((to-symbol) (string->symbol obj))
                    ((to-keyword) (string->keyword obj))
                    ((to-number) (string->number obj))
                    ((to-list) (string->list obj))
                    ((to-vector) (list->vector (string->list obj)))
                    ((to-text) obj)
                    ((to-stream) (open-input-string obj))
                    ((uc) (string-upcase obj))
                    ((lc) (string-downcase obj))
                    ((take) (lambda (n) (string-take obj n)))
                    ((drop) (lambda (n) (string-drop obj n)))
                    ((trim) (string-trim-both obj))
                    ((ltrim) (string-trim obj))
                    ((rtrim) (string-trim-right obj))
                    ((lpad) (lambda (rune n) (string-pad obj n rune)))
                    ((rpad) (lambda (rune n) (string-pad-right obj n rune)))
                    ((chomp) (string-chomp obj))
                    ((index) (lambda (which) (substring-index which obj)))
                    ((size) (string-length obj))
                    ((messages) msgs)
                    ((responds?)
                        (lambda (msg)
                            (or
                                (and (number? msg) (> (string-length obj) msg))
                                (if (member msg msgs) #t #f)))))))
        ((split)
            (cont
                (sexy-proc
                    primitive-type
                    'text
                    (lambda (args opts cont err)
                        (define flags (sexy-send-atomic opts 'flags))
                        (define re (build-regex (car args) (if (eq? 'null flags) "" flags)))
                        (cont (irregex-split re obj))))))
        ((match)
            (cont
                (sexy-proc
                    primitive-type
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
                    primitive-type
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
                    primitive-type
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
                        (err (sexy-error-object 'out-of-bounds `(,obj ,idx) "text: index out of bounds.") cont)
                        (begin
                            (string-set! obj idx val)
                            obj)))))
        (else
            (if (number? msg)
                (if (> (string-length obj) msg)
                    (cont (string-ref obj msg))
                    (err (sexy-error-object 'out-of-bounds `(,obj ,msg) "text: index out of bounds.") cont))
                (idk obj msg cont err)))))

(define (sexy-ho code obj cont err)
    (sexy-apply
        (sexy-compile-method code)
        (list obj)
        'null
        cont
        err))

(define (sexy-send-empty obj msg cont err)
    (case msg
        ((type empty? autos resends default view to-bool to-text to-list head tail key val car cdr size)
            (cont
                (case msg
                    ((type) 'list)
                    ((autos) '(view empty? to-bool to-text to-list head tail key val size))
                    ((resends) '())
                    ((default) default-default)
                    ((empty?) #t)
                    ((to-bool) #f)
                    ((view to-list) '())
                    ((to-text) "()")
                    ((head tail key val car cdr) 'null)
                    ((size) 0))))
        (else (sexy-send-list obj msg cont err))))

(define (sexy-send-list obj msg cont err)
    (define msgs
        '(type empty? view to-bool to-list to-text to-vector to-record head key car tail val cdr cons
          size reverse has? append take drop apply fold reduce each map filter sort))
    (define (ldefault msg)
        (if (number? msg)
            (if (> (length obj) msg)
                (cont (list-ref obj msg))
                (err (sexy-error-object 'out-of-bounds `(,obj ,msg) "list: index out of bounds.") cont))
            (idk obj msg cont err)))
    (case msg
        ((type autos resends default empty? view to-bool to-list to-text to-vector head key car tail val cdr cons size reverse has? append take drop apply messages responds?)
            (cont
                (case msg
                    ((type) 'list)
                    ((empty?) #f)
                    ((autos) '(view empty? to-bool to-text to-list to-vector to-record head tail key val size reverse))
                    ((resends) '())
                    ((default) ldefault)
                    ((view) (map sexy-view obj))
                    ((to-text) (apply string obj))
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
                    ((messages) msgs)
                    ((responds?)
                        (lambda (msg)
                            (or
                                (and (number? msg) (> (length obj) msg))
                                (if (member msg msgs) #t #f))))
                    ((apply)
                        (sexy-proc
                            primitive-type
                            'pair
                            (lambda (args opts cont err)
                                (if (pair? (car args))
                                    (sexy-send-list obj (caar args) cont err)
                                    (err (sexy-error-object 'bad-message! `(,obj ,args ,opts) "Message not understood.") cont))))))))
        ((to-record)
            (if (not (every pair? obj))
                (err (sexy-error-object 'not-an-associative-list! `(send ,obj to-record) "list: to-record only works on associative lists." ) cont)
                (let ((r (sexy-record)))
                    (define vars (htr r 'vars))
                    (for-each (lambda (p) (hts! vars (car p) (cdr p))) obj)
                    (cont r))))
        ((fold)
            (sexy-ho
                '(λ (xs)
                    (λ (acc funk)
                        (if xs.empty?
                            acc
                            (xs.tail.fold (funk acc xs.head) funk))))
                obj
                cont
                err))
        ((reduce)
            (sexy-ho
                '(λ (xs)
                    (λ (acc funk)
                        (if xs.empty?
                            acc
                            (funk xs.head (xs.tail.reduce acc funk)))))
                obj
                cont
                err))
        ((each)
            (sexy-ho
                '(λ (xs)
                    (λ (funk)
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
                '(λ (xs)
                    (λ (funk)
                        (xs.reduce '() (λ (x y) (pair (funk x) y)))))
                obj
                cont
                err))
        ((filter)
            (sexy-ho
                '(λ (xs)
                    (λ (funk)
                        (xs.reduce '() (λ (x y) (if (funk x) (pair x y) y)))))
                obj
                cont
                err))
        ((sort)
            (sexy-ho
                '(λ (xs)
                    (λ (funk)
                        (def merge (λ (a b)
                            (if a.size.zero?
                                b
                                (if b.size.zero?
                                    a
                                    (if (funk a.head b.head)
                                        (pair a.0 (merge a.tail b))
                                        (pair b.0 (merge a b.tail)))))))
                        (def sort (λ (yarr)
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
        (else (ldefault msg))))

(define (sexy-send-pair obj msg cont err)
    (define msgs
        '(empty? view to-text to-bool to-list to-record head key car tail val cdr cons size clone))
    (define msgs+ (append msgs '(messages responds? type)))
    (if (member msg msgs+)
        (cont 
            (case msg
                ((type) 'pair)
                ((view to-text) obj)
                ((autos) '(view empty? to-bool to-text to-list to-record head tail key val size))
                ((resends) '())
                ((default) default-default)
                ((to-bool) #t)
                ((to-list) (list (car obj) (cdr obj)))
                ((to-record) (sexy-record (car obj) (cdr obj)))
                ((head key car) (car obj))
                ((tail val cdr) (cdr obj))
                ((cons) (lambda (v) (cons v obj)))
                ((size) 2)
                ((clone) (cons (car obj) (cdr obj)))
                ((messages) msgs)
                ((responds?)
                    (lambda (msg)
                        (if (member msg msgs) #t #f)))))
        (idk obj msg cont err)))

(define (sexy-send-primitive obj msg cont err)
    (define msgs '(view code to-bool to-text env arity apply))
    (define msgs+ (append msgs '(messages responds? type autos resends default)))
    (if (member msg msgs+)
        (cont 
            (case msg
                ((type) 'proc)
                ((view) primitive-type)
                ((code) '0xDEADBEEF)
                ((to-bool) #t)
                ((to-text) "0xDEADBEEF")
                ((autos) '(view code to-bool to-text env arity))
                ((resends) '())
                ((default) default-default)
                ((env) 'global)
                ((arity)
                    (let ((pinfo (procedure-information obj)))
                        (if (list? pinfo)
                            (sub1 (length pinfo))
                            '*)))
                ((messages) msgs)
                ((responds?)
                    (lambda (msg)
                        (if (member msg msgs) #t #f)))
                ((apply)
                    (lambda (args opts)
                        (apply obj args)))))
        (idk obj msg cont err)))

(define (sexy-send-record obj msg cont err)
    (define msgs
        '(view size clone to-bool get put set! rm del! has? apply keys values pairs to-list to-opt to-text merge fold reduce map filter))
    (define vars (htr obj 'vars))
    (define (rdefault msg)
        (if (hte? vars msg)
            (htr vars msg)
            'null))
    (case msg
        ((type view size autos resends default clone to-bool get put set! rm del! has? apply keys values pairs to-list to-opt to-text merge messages responds?)
            (cont
                (case msg
                    ((type) 'record)
                    ((view to-text)
                        (let ((keys (htks vars)))
                            (cons
                                (string->keyword "record")
                                (fold
                                    (lambda (p xs)
                                        (cons (car p) (cons (sexy-view (cdr p)) xs)))
                                    '()
                                    (hash-table->alist vars)))))
                    ((size) (hash-table-size vars))
                    ((autos) '(view size clone to-bool to-list to-text keys values pairs))
                    ((resends) '())
                    ((default) rdefault)
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
                            primitive-type
                            'record
                            (lambda (args opts cont err)
                                (sexy-send-record obj (caar args) cont err))))
                    ((keys) (htks vars))
                    ((values) (htvs vars))
                    ((pairs to-list) (hash-table->alist vars))
                    ((to-opt)
                        (fold
                            (lambda (p xs)
                                (cons (symbol->keyword (car p)) (cons (cdr p) xs)))
                            '()
                            (hash-table->alist vars)))
                    ((messages) msgs)
                    ((responds?)
                        (lambda (msg)
                            (or 
                                (hte? vars msg)
                                (if (member msg msgs) #t #f))))
                    ((merge)
                        (lambda (other)
                            (define nuvars (hash-table-merge (htr other 'vars) vars))
                            (define noob (mkht))
                            (hts! noob 'type 'record)
                            (hts! noob 'vars nuvars)
                            noob)))))
            ((fold) (sexy-send-list
                        (hash-table->alist vars)
                        'fold
                        cont
                        err))
            ((reduce) (sexy-send-list
                        (hash-table->alist vars)
                        'reduce
                        cont
                        err))
            ((map)
                (sexy-ho
                    '(λ (rec)
                        (λ (funk)
                            (def mapped (rec.to-list.map funk))
                            mapped.to-record))
                    obj
                    cont
                    err))
            ((filter) 
                (sexy-ho
                    '(λ (rec)
                        (λ (funk)
                            (def mapped (rec.to-list.filter funk))
                            mapped.to-record))
                    obj
                    cont
                    err))
            (else (cont (rdefault msg)))))

(define (sexy-send-object obj msg cont err)
    (define fields (htr obj 'fields))
    (define resends (htr obj 'resends))
    (define autos (htr obj 'autos))
    (define (get-msgs)
        (append (hash-table-keys fields) (hash-table-keys resends)))
    (if (hte? fields msg)
        (let ((v (htr fields msg)))
            (if (hte? autos msg)
                (sexy-apply v '() 'null cont err) ; exec the thunk
                (cont v)))
        (if (hte? resends msg)
            (sexy-send (htr resends msg) msg cont err)
            (case msg
                ((type) (cont 'object))
                ((view) (cont `(,(if (hte? fields 'type) (htr fields 'type) 'object) ,@(get-msgs))))
                ((to-text) (cont "object"))
                ((to-bool) (cont (not (eq? 0 (length (hash-table-keys fields))))))
                ((responds?) (cont (lambda (x) (hte? fields x))))
                ((messages) (cont (get-msgs)))
                ((autos) (cont (hash-table-keys autos)))
                ((resends) (cont (hash-table-keys resends)))
                ((default) (cont (htr obj 'default)))
                (else (sexy-apply (htr obj 'default) (list msg) 'null cont err))))))

(define (sexy-send-proc obj msg cont err)
    (define msgs '(type view to-bool to-text arity code env formals apply))
    (case msg
        ((type) (cont (htr obj 'type)))
        ((view) (cont `(,(htr obj 'type) ,(htr obj 'formals) ...)))
        ((to-bool) (cont #t))
        ((to-text) (cont (htr obj 'code)))
        ((arity code env formals) (cont (htr obj msg)))
        ((apply)
            (cont 
                (sexy-proc
                    primitive-type
                    'proc
                    (lambda (args opts cont err)
                        (if (< (length args) 2)
                            (err (sexy-error-object 'arity `((send ,obj apply) ,args) "proc.apply requires 2 arguments!") cont)
                            (sexy-apply obj (car args) (cadr args) cont err))))))
        ((messages) (cont msgs))
        ((responds?) (cont (lambda (msg) (if (member msg msgs) #t #f))))
        ((autos) (cont '(view to-bool to-text arity code env formals)))
        ((resends) (cont '()))
        ((default) (cont default-default))
        (else (idk obj msg cont err))))

(define (sexy-send-env obj msg cont err)
    (define msgs '(view to-text def! set! has? get del! pairs lookup mama extend eval expand))
    (case msg
        ((get has? to-bool keys values pairs)
            (sexy-send-record (htr obj 'vars) msg cont err))
        ((type) (cont 'env))
        ((view to-text)
            (cont
                (cons (string->keyword "env")
                      (cdr (sexy-view (htr obj 'vars))))))
        ((autos) (cont '(view to-text to-bool keys values pairs)))
        ((resends) (cont '()))
        ((default) (cont default-default))
        ((def!)
            (sexy-send-record (htr obj 'vars) 'set! cont err))
        ((rm!)
            (sexy-send-record (htr obj 'vars) 'del! cont err))
        ((set!)
            (cont
                (sexy-proc
                    primitive-type
                    'env
                    (lambda (args opts cont err)
                        (define len (length args))
                        (if (not (> len 1))
                            (err (sexy-error-object 'env-error `(set! ,@args) "env.set! requires at least 2 arguments.") cont)
                            (if (not (= 0 (modulo len 2)))
                                (err (sexy-error-object 'env-error `(set! ,@args) "env.set! requires an even number of arguments.") cont)
                                (let loop ((name (car args)) (val (cadr args)) (rest (cddr args)))
                                    (update!
                                        obj
                                        name
                                        val
                                        (lambda (_)
                                            (if (pair? rest)
                                                (loop (car rest) (cadr rest) (cddr rest))
                                                (cont 'null)))
                                        err))))))))
        ((del!)
            (cont
                (sexy-proc
                    primitive-type
                    'env
                    (lambda (args opts cont err)
                        (if (not (> (length args) 0))
                            (err (sexy-error-object 'env-error `(del! ,@args) "env.del! requires at least 1 argument.") cont)
                            (let loop ((name (car args)) (rest (cdr args)))
                                (delete!
                                    obj
                                    name
                                    (lambda (_)
                                        (if (pair? rest)
                                            (loop (car rest) (cdr rest))
                                            (cont 'ok)))
                                    err)))))))
        ((lookup)
            (cont
                (sexy-proc
                    primitive-type
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
                    primitive-type
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
        ((messages) (cont msgs))
        ((responds?) (cont (lambda (msg) (if (member msg msgs) #t #f))))
        (else (idk obj msg cont err))))

(define (sexy-send-vector obj msg cont err)
    (define msgs '(view to-bool to-text to-list pairs size clone has? set! apply fold reduce map filter sort))
    (define (vdefault msg)
        (if (number? msg)
            (if (> (vector-length obj) msg)
                (cont (vector-ref obj msg))
                (err (sexy-error-object 'out-of-bounds `(,obj ,msg) "vector: index out of bounds.") cont))
            (idk obj msg cont err)))
    (case msg
        ((type view autos resends default to-bool to-text to-list pairs size clone has? set! apply messages responds?)
            (cont 
                (case msg
                    ((type) 'vector)
                    ((view) (cons (string->keyword "vector") (map sexy-view (vector->list obj))))
                    ((to-bool) (not (eq? (vector-length obj) 0)))
                    ((to-list) (vector->list obj))
                    ((to-text) (apply string (vector->list obj)))
                    ((autos) '(view to-text to-bool to-list size pairs clone))
                    ((resends) '())
                    ((default) vdefault)
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
                                (err (sexy-error-object 'out-of-bounds `(,obj ,msg) "vector: index out of bounds.") cont)
                                (begin
                                    (vector-set! obj idx val)
                                    obj))))
                    ((messages) msgs)
                    ((responds?)
                        (lambda (msg)
                            (or
                                (and (number? msg) (> (vector-length obj) msg))
                                (if (member msg msgs) #t #f))))
                    ((apply)
                        (sexy-proc
                            primitive-type
                            'pair
                            (lambda (args opts cont err)
                                (sexy-send-vector obj (caar args) cont err)))))))
        ((fold)
            (sexy-ho
                '(λ (vec)
                    (λ (acc funk)
                        (vec.to-list.fold acc funk)))
                obj
                cont
                err))
        ((reduce)
            (sexy-ho
                '(λ (vec)
                    (λ (acc funk)
                        (vec.to-list.reduce acc funk)))
                obj
                cont
                err))
        ((map)
            (sexy-ho
                '(λ (vec)
                    (λ (funk)
                        (def mapped (vec.to-list.map funk))
                        mapped.to-vector))
                obj
                cont
                err))
        ((filter)
            (sexy-ho
                '(λ (vec)
                    (λ (funk)
                        (def mapped (vec.to-list.filter funk))
                        mapped.to-vector))
                obj
                cont
                err))
        ((sort)
            (sexy-ho
                '(λ (vec)
                    (λ (funk)
                        (def sorted (vec.to-list.sort funk))
                        sorted.to-vector))
                obj
                cont
                err))
        (else
            (vdefault msg))))

(define (sexy-send-stream obj msg cont err)
    (case msg
        ((type view resends default to-bool to-stream input? output? open?)
            (cont 
                (case msg
                    ((type) 'stream)
                    ((view to-text) obj)
                    ((to-bool) #t)
                    ((to-stream) obj)
                    ((resends) '())
                    ((default) (cont default-default))
                    ((input?) (input-port? obj))
                    ((output?) (output-port? obj))
                    ((open?) (not (port-closed? obj))))))
        (else
            (if (input-port? obj)
                (sexy-send-input-stream obj msg cont err) 
                (sexy-send-output-stream obj msg cont err)))))

(define (sexy-send-input-stream obj msg cont err)
    (define msgs
        '(view to-bool input? output? open? close
          ready? read read-rune peek-rune read-line read-text assert-rune skip skip-while skip-until
          read-token read-token-while read-token-until read-token-if to-list to-text read-sexy))
    (case msg
        ((ready? autos read read-rune peek-rune read-line read-text assert-rune skip skip-while skip-until
          read-token read-token-while read-token-until read-token-if to-list to-text read-sexy
          messages responds?)
            (if (port-closed? obj)
                (err (sexy-error-object 'input-stream-closed `(send ,obj ,msg) "Input stream closed.") cont)
                (cont 
                    (case msg
                        ((autos) '(view to-text to-bool to-list ready? input? output? open? read read-rune peek-rune read-line read-text read-sexy)) 
                        ((ready?) (char-ready? obj))
                        ((read) (sexy-read obj))
                        ((read-rune) (read-char obj))
                        ((peek-rune) (peek-char obj))
                        ((read-line) (read-line obj))
                        ((read-text to-text) (read-string #f obj))
                        ((read-sexy) (sexy-read-file obj))
                        ((assert-rune)
                            (sexy-proc
                                primitive-type
                                'stream
                                (lambda (args opts cont err)
                                    (if (not (= 1 (length args)))
                                        (err (sexy-error-object 'arity `assert-rune "stream.assert-rune requires one text argument") cont)
                                        (let ((runes (string->list (car args))))
                                            (define next (read-char obj))
                                            (if (member next runes)
                                                (cont next)
                                                (err (sexy-error-object 'assert-rune-FAIL `(assert-rune next ,(car args)) "Assertion FAIL") cont)))))))
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
                        ((read-token read-tokens)
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
                                primitive-type
                                'env
                                (lambda (args opts cont err)
                                    (if (not (= 1 (length args)))
                                        (err (sexy-error-object 'arity `(read-token-if) "read-token-if: requires one proc argument.") cont)
                                        (let ((pred (car args)))
                                            (let loop ((tok (peek-char obj)) (acc '()))
                                                (sexy-apply
                                                    pred
                                                    (list tok)
                                                    'null
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
                        ((messages) msgs)
                        ((responds?)
                            (lambda (msg)
                                (if (member msg msgs) #t #f)))
                        ((to-list read-lines) (read-lines obj))))))
        ((close) (close-input-port obj) (cont 'null))
        (else (idk msg obj cont err))))

(define (sexy-send-output-stream obj msg cont err)
    (define msgs
        '(view to-bool input? output? open? write print say nl flush close))
    (case msg
        ((write print say nl autos)
            (if (port-closed? obj)
                (err (sexy-error-object 'output-stream-closed `(send ,obj ,msg) "Output stream closed.") cont)
                (cont
                    (case msg
                        ((autos) '(view to-bool ready? input? output? open? nl close)) 
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
        ((messages) (cont msgs))
        ((responds?)
            (cont
                (lambda (msg)
                    (if (member msg msgs) #t #f))))
        ((flush) (flush-output obj) (cont 'null))
        ((close) (close-output-port obj) (cont 'null))
        (else (idk msg obj cont err))))

(define (sexy-send-eof obj msg cont err)
    (case msg
        ((type) (cont 'EOF))
        ((view) (cont 'EOF))
        ((to-bool) (cont #f))
        ((to-text) (cont "END OF LINE."))
        ((autos) '(view to-text to-bool))
        ((resends) '())
        ((default) (cont default-default))
        ((apply) (err (sexy-error-object 'eof-is-not-applicable '(EOF ...) "EOF objects can not be used as procedures.") cont))
        (else (idk msg obj cont err))))


