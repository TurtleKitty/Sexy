#lang racket

; first attempt with hygienic macros... kill me

(define-syntax-rule (alias foo bar)
	(define-syntax foo
		(syntax-rules ()
			((foo x) (bar x))
			((foo x (... ...)) (bar x (... ...))))))

(alias def define)
(alias vars letrec)

(define-syntax (fn stx)
	(with-syntax ((argv (datum->syntax stx 'argv)) (loop (datum->syntax stx 'loop)))
		(syntax-case stx ()
			((_ body)
				#'(letrec ((loop (lambda argv body))) loop))
			((_ x body)
				#'(letrec ((loop (lambda (x . argv) body))) loop))
			((_ x ... body)
				#'(letrec ((loop (lambda (x ... . argv) body))) loop)))))

((fn x y z (list x y z (* x y z))) 3 4 5)
((fn argv) 3 4 5)
((fn x (list x argv)) 3 4 5)
((fn x (if (< x 2) x (* x (loop (- x 1))))) 5)

(define-syntax (fun stx)
	(with-syntax ((def-id (datum->syntax stx 'def)) (argv (datum->syntax stx 'argv)))
		(syntax-case stx ()
			((_ name body ...) #`(def-id (name . argv) body ...))
			((_ name (x ...) body ...) #`(def-id (name x ... . argv) body ...)))))

(fun ball argv)
(ball 6 7 8)
;(fun foo (x y) (list x y argv))
;(foo 3 4 5)


;(define-syntax vars
;	(syntax-rules ()
;		((vars x y z) (let ((x y)) z)
;		((vars x y ... z) (letrec ((x y) (...)) z)))))

(def (make-alist . argv)
	(let f ((alist empty) (pairs argv))
		(if (null? pairs)
			(reverse alist)
			(f
				(cons (cons (first pairs) (second pairs)) alist)
				(rest (rest pairs))))))

(def (mklets expr)
	(def rev (reverse expr))
	(def body (car rev))
	(def args (cdr (reverse (cdr rev))))
	(def asslist (apply make-alist args))
	(def letargs (map (fn x (list (car x) (cdr x))) asslist))
	`(let ,letargs ,body))

(def letta (mklets '(lets x 1 y 2 z "foo" (list z x y))))
letta
(eval letta (make-base-namespace))

(fun tuple
	(def vtable (apply make-alist argv))
	(def (get f) (cdr (assoc f vtable)))
	(def (put f v)
		(apply tuple
			(map
				(fn x (if (eq? (car x) f) (cons (car x) v) x))
				vtable)))
	(fn (x #f) (y #f)
		(cond
			((and x y) (put x y))
			(x (get x))
			(else vtable))))

(def fbb (tuple 'foo 1 'bar 2 'baz 3))

(fbb 'foo)
((fbb 'foo 7) 'foo)
(fbb)

#|
(def (obj . fns)
	(def ctrl (apply tuple fns))
	(fn (cmd . args)
		(apply (ctrl cmd) args)))

(def (1dx x) (+ 1 (random x)))
(def (d20) (1dx 20))

(def (mk-brawler name brawn brains moves cool karma)
	(obj
		'name (thunk name)
		'brawn (thunk brawn)
		'brains (thunk brains)
		'moves (thunk moves)
		'cool (thunk cool)
		'karma (thunk karma)
		'attack (fn (enemy) (- (+ moves (d20)) (enemy 'moves)))))

;(def (class name args methods)
;	(def (factory
;	(def methz
;		(map (ll arg (list arg (thunk arg)) args

;(class brawler
;	'(name brawn brains moves cool karma)
;	`(attack ,(fn (enemy) (- (+ moves (d20)) (enemy 'moves)))))

(def foomander
	(obj
		'foo (ll x (* x 2))
		'bar (ll x (+ x 10))
		'baz (ll x (expt 2 x))))

(foomander 'foo 7)
(foomander 'bar 7)
(foomander 'baz 7)

(vars ((x (* 2 3)) (y (+ 3 4)) (z 12)) (list x y z))

(def jack (mk-brawler 'jack 20 0 2 4 100))

(jack 'attack jack)

|#


