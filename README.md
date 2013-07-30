Sexy
====

*(real.languages (have 'curves))* 

This is a compiler extension for Chicken Scheme (http://call-cc.org/).

It implements a prototype of a Sexy language.

```scheme

(def foo (fn x y (* x y)))

(show (foo 3 4)) 

(fun bar x y 
    (if argo.snazz
        (list x y argo.snazz argo.snarf argv)
        (list x y argv)))

(map show
    (list
        (bar 2 3)
        (bar 2 3 4 5)
        (bar 2 snazz: #t 3 4 5)
        (bar 2 snazz: #t snarf: "Yarr!" 3 4 5)))
```

Output:

12
(2 3 ())
(2 3 (4 5))
(2 3 #t #f (4 5))
(2 3 #t Yarr! (4 5))



