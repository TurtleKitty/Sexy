Sexy
====

**(real.languages (have 'curves))**

This is a compiler extension for Chicken Scheme (http://call-cc.org/).

It implements a prototype of a Sexy language.

```scheme

(def foo (fn (x y) (* x y)))

(stdout.print (foo 3 4)) 

(fun bar (x y) 
    (if opt.snazz
        (list x y opt.snazz opt.snarf rest)
        (list x y rest)))

(map stdout.print
    (list
        (bar 2 3)
        (bar 2 3 4 5)
        (bar 2 snazz: true 3 4 5)
        (bar 2 snazz: true snarf: "Yarr!" 3 4 5)))

(def fibby
	(fn (x)
		(if (< x 3)
			1
			(+ (fibby (- x 2)) (fibby (- x 1))))))

(stdout.print (map fibby '(1 2 3 4 5)))

```

Output:

```scheme
12
(2 3 ())
(2 3 (4 5))
(2 3 #t #f (4 5))
(2 3 #t Yarr! (4 5))
(1 1 2 3 5)
```


