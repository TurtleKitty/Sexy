
(define (read-sexps file)
	(let loop ((sexp (read file)) (sexps '()))
		(if (eof-object? sexp)
			(reverse sexps)
			(loop (read file) (cons sexp sexps)))))

(define (file->list file)
	(call-with-input-file file read-sexps))

(define prelude `(

	(use srfi-1)

	(define (head xs)
		(car xs))

	(define (tail xs)
		(cdr xs))

	(define (last xs)
		(car (reverse xs)))

	(define (show x)
		(display x)
		(newline))

	(define (flatten alist)
		(if (null? alist)
			alist
			(let loop ((end '()) (x (car alist)) (xs (cdr alist)))
				(cond ((null? x) (reverse end))
					  ((null? xs) (loop (cons (cdr x) (cons (car x) end)) '() '()))
					  (#t (loop (cons (cdr x) (cons (car x) end)) (car xs) (cdr xs)))))))

	(define (filter pred lst)
		(cond ((null? lst) lst)
			  ((pred (first lst))
				  (cons (first lst) (filter pred (cdr lst))))
			  (else (filter pred (cdr lst)))))

	(define (make-table . xs)
		(let f ((alist '()) (pairs xs))
			(if (null? pairs)
				(reverse alist)
				(f
					(cons (cons (first pairs) (second pairs)) alist)
					(cdr (cdr pairs))))))

	(define (merge-tables old new)
		(define news
			(filter
				(lambda (x) (equal? #f (assoc (car x) old)))
				new))
		(define (submerge kv)
			(let ((has (assoc (car kv) new)))
				(if has
					has
					kv)))
		(let ((alt (map submerge old)))
			(if (null? news)
				alt
				(let loop ((nooxs alt) (x (car news)) (xs (cdr news)))
					(cond ((null? x) nooxs)
						  ((null? xs) (cons x nooxs))
						  (#t (loop (cons x nooxs) (car xs) (cdr xs))))))))

	(define (obj . xs)
		(define vt (apply make-table xs))
		(define (mkobj vtable)
			(define (get f)
				(let ((has (assoc f vtable)))
					(if has (cdr has) #f)))
			(define (put . args)
				(define newt (apply make-table args))
				(mkobj (merge-tables vtable newt)))
			(case-lambda
				(() vtable)
				((x) (get x))
				(args (apply put args))))
		(mkobj vt))
))

(define (make-prelude fuckthis files fuckthat)
	(define file (car files))
	(define lines (file->list file))
	(define whole (append prelude lines))
	whole)

(user-read-pass make-prelude)

(define (last xs)
	(car (reverse xs)))

(define (alistify . argv)
	(let f ((alist '()) (pairs argv))
		(if (null? pairs)
			(reverse alist)
			(f
				(cons (cons (first pairs) (second pairs)) alist)
				(cdr (cdr pairs))))))

(define (warp form match? transform)
	(if (match? form)
		(let ((changed (transform form)))
			(if (equal? form changed)
				changed
				(begin 
					;(display form) (display " -> ") (display changed) (newline) (newline)
					changed)))
		form))

;(define (descend form match? transform)
;	(define (curses x) (descend x match? transform))
;	(if (pair? form)
;		(warp (cons (curses (car form)) (curses (cdr form))) match? transform)
;		(warp form match? transform)))
;
(define (descend form match? transform)
	(define (curses x) (descend x match? transform))
	(define newform (warp form match? transform))
	(if (pair? newform)
		(cons (curses (car newform)) (curses (cdr newform)))
		newform))

(define (optionator)
	(define (match? x)
		(if (pair? x)
			(let ((op (car x)))
				(and (not (member op '(def fn fun let)))
					 (not (keyword? op))))
			#f))
	(define (transform x)
		(let loop ((current (car x)) (options '()) (nons '()) (rest (cdr x)))
			(define (otherwise)
				(if (pair? rest)
					(loop (car rest) options (cons current nons) (cdr rest))
					(loop '() options (cons current nons) #f)))
			(cond
				((null? current)
					(if (equal? options '())
						x
						`(,@(reverse nons) argo: (obj ,@(reverse options)))))
				((and (keyword? current) (not (eq? current 'argo:)))
					(let* ((newsym (string->symbol (symbol->string current))) (newopts (cons (car rest) (cons `(quote ,newsym) options))))
						(if (pair? (cdr rest))
							(loop (second rest) newopts nons (cddr rest))
							(loop '() newopts nons #f))))
				(#t (otherwise)))))
	(cons match? transform))

(define (doterator)
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
					(loop (list this `(quote ,(string->symbol (car left)))) (cdr left))))))
	(cons match? transform))

(define (funmatch y)
	(lambda (x) 
		(and (pair? x) (eq? (car x) y))))

(define (def-wrangler)
	(define match? (funmatch 'def))
	(define (transform x)
		(define pairs (apply alistify (cdr x)))
		(define defs
			(map
				(lambda (p) (list 'define (car p) (cdr p)))
				pairs))
		`(values ,@defs))
	(cons match? transform))

(define (chicken-hack x)
	`(if (any (lambda (x) (eq? x 'argo:)) argv) (let ((argv (drop-right argv 2))) ,x) ,x))

(define (fn-wrangler)
	(define match? (funmatch 'fn))
	(define (transform x)
		(define args (reverse (cdr (reverse (cdr x)))))
		(define body (chicken-hack (last x)))
		(if (eq? args '())
			`(letrec ((this (lambda (#!rest argv #!key (argo (obj))) ,body))) this)
			`(letrec ((this (lambda (,@args #!rest argv #!key (argo (obj))) ,body))) this)))
	(cons match? transform))

(define (fun-maker)
	(define match? (funmatch 'fun))
	(define (transform x)
		(define rest (cdr x))
		(define name (car rest))
		(define-values (args bodies)
			(let loop ((args '()) (current (cadr rest)) (next (cddr rest)))
				(if (pair? current)
					(values (reverse args) (cons current next))
					(loop (cons current args) (car next) (cdr next)))))
		`(define (,name ,@args #!rest argv #!key (argo (obj))) ,(chicken-hack (cons 'begin bodies))))
	(cons match? transform))

(define order
	(list
	  (optionator)
	  (doterator)
	  (fn-wrangler)
	  (def-wrangler)
	  (fun-maker)))

(define (sexy form)
	(define (desc form mt)
		(descend form (car mt) (cdr mt)))
	(define atomized
		(let loop ((f form) (fns order))
			(if (eq? fns '())
				f
				(loop (desc f (car fns)) (cdr fns)))))
	(print atomized)
	(newline)
	atomized)

(user-preprocessor-pass sexy)
