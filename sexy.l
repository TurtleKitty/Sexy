
;(load "uuid.lisp")

; playing with Common Lisp... still prefer Scheme

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro <-> (value place) `(push ,value ,place))

(defmacro macro (name (&rest args) &body body)
	`(defmacro ,name ,args ,body))

(macro fn (&rest args)
	(let ((rev (reverse args)))
		(let ((body (car rev)) (xs (reverse (cdr rev))))
			`(lambda (,@xs &rest argv) ,body))))

(macroexpand '(fn x y z (* x y z (apply #'+ argv))))

(apply #'(fn x y z (* x y z (apply #'+ argv))) 2 3 4 5 5)

(macro fun (name (&rest args) &body body)
	`(let ,name (fn ,@args ,body)))

;(macro mkobj ()
;	`(let obj (fn)))

;(macro let-to-labels (expr)
;	(if (consp expr)
;		(let ((verb (car expr)) (others (cdr expr)))
;			(if ))
;		(if (eq expr 'let)
;	(mapcar #'ttt expr)

;(macro fexpr ((&rest args) &body body))

(defun split-str-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-str-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))

(defun split-str (string &optional (separator " "))
  (split-str-1 string separator))
 
(defun is-struct (x)
	(and (symbolp x) (find #\. (symbol-name x)) t))

(defun builder (xs)
	(let ((x (car xs)) (y (cdr xs)))
		(if (null y)
			(intern x)
			(list (builder y) (intern x)))))

(defun split-struct (x)
	(let ((foo (split-str (symbol-name x) ".")))
		(builder (reverse foo))))

(defmacro destruct (x)
	(mapcar
		(lambda (y)
			(if (is-struct x)
				`(,@(split-struct y))
				y))
		x))

(defmacro fix-keywords (expr)
	(cond
		((atom expr) expr
		 t `(fix-keywords expr))))

(defvar tests '(foo.bar foo.bar.baz foo "bar" "baz.bax"))

(print
	(list
		;(destruct '(foo.bar))
		(mapcar #'is-struct tests)
		(mapcar #'split-struct '(foo.bar foo.bar.baz x.y.z.p.q))))

