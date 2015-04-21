Sexy
====

**((real 'programs) have.curves)**

Sexy is an object-oriented dialect of Lisp inspired by Scheme, Self, and Perl.

Features:

    * Lisp-1
    * immutable global environment
    * classless object machinery for simple composition and delegation
    * delimited lexical scope
    * first-class environments
    * first-class delimited continuations
    * some degree of object capability security
    * restartable exceptions
    * dirty Common-Lisp-style macros
    * module import via HTTP

```scheme

(def foo (fn (x y) (* x y)))

(sys.stdout.say (foo 3 4)) 

(fun bar (x y) 
    (if opt.snazz
        (list x y opt.snazz opt.snarf rest)
        (list x y rest)))

(def baz
    (list
        (bar 2 3)
        (bar 2 3 4 5)
        (bar 2 snazz: true 3 4 5)
        (bar 2 snazz: true snarf: "Yarr!" 3 4 5)))

(let loop (x baz.head xs baz.tail)
    (if x
        (seq
            (sys.show x)
            (loop xs.head xs.tail))
        'null))

(def fibby
    (fn (x)
        (if (< x 3)
            1
            (+ (fibby (- x 2)) (fibby (- x 1))))))

(def things '(1 2 3 4 5 6 7))

(sys.show (things.map fibby))


```

Output:

```scheme
12
(2 3 ())
(2 3 (4 5))
(2 3 true null (4 5))
(2 3 true Yarr! (4 5))
(1 1 2 3 5 8 13)

```


