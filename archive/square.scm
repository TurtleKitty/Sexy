
; tiny metacircular interpreter for a Schemelike

[use srfi-1]
[use srfi-69]
[use numbers]

[define [square-eval code env]
    [if [atom? code]
        [if [symbol? code]
            [[env 'find] code]
            code]
        [let [[head [car code]] [tail [cdr code]]]
            [cond
                [[eq? head 'quote] [car tail]]
                [[eq? head 'if] [if [square-eval [car tail] env]
                          [square-eval [cadr tail] env]
                          [square-eval [caddr tail] env]]]
                [[eq? head 'seq] [square-eval-seq tail env]]
                [[eq? head 'set!] [[env 'set!] [car tail] [square-eval [cadr tail] env]]]
                [[eq? head 'fn] [make-closure [car tail] [cdr tail] env]]
                [[eq? head '+] [apply + [square-eval-list tail env]]]
                [[eq? head '-] [apply - [square-eval-list tail env]]]
                [[eq? head '*] [apply * [square-eval-list tail env]]]
                [[eq? head '/] [apply / [square-eval-list tail env]]]
                [[eq? head '=] [apply equal? [square-eval-list tail env]]]
                [else [square-apply [square-eval head env] [square-eval-list tail env]]]]]]]

[define [square-eval-list xs env]
    [if [pair? xs]
        [cons
            [square-eval [car xs] env]
            [square-eval-list [cdr xs] env]]
        [square-eval xs env]]]

[define [square-eval-seq xs env]
    [if [pair? xs]
        [let [[head [car xs]] [tail [cdr xs]]]
            [if [pair? tail]
                [begin
                    [square-eval head env]
                    [square-eval-seq tail env]]
                [square-eval head env]]]
        '[]]]

[define [make-env parent]
    [define table [make-hash-table]]
    [define [lookup name]
        [if [hash-table-exists? table name]
            [hash-table-ref table name]
            [if parent
                [[parent 'find] name]
                #f]]]
    [define [extend names vals]
        [define noob [make-env self]]
        [map [noob 'set!] names vals]
        noob]
    [define [mutate! name value]
        [hash-table-set! table name value]]
    [define self
        [lambda [msg]
            [cond
                [[eq? msg 'find] lookup]
                [[eq? msg 'add] extend]
                [[eq? msg 'set!] mutate!]]]]
    self]

[define [make-closure args body env]
    [lambda [vals]
        [square-eval-seq body [[env 'add] args vals]]]]

[define [square-apply f args]
    [f args]]

[define world [make-env #f]]

[define [repl]
    [display "[square] "]
    [display [square-eval [read] world]]
    [newline]
    [repl]]

[define args [command-line-arguments]]

[define n [string->number [car args]]]

[define [speed-test]
    [display
        [square-eval
            `[seq
                [set! fact [fn [n acc] [if [= n 1] acc [fact [- n 1] [* n acc]]]]]
                [fact ,n 1]]
            world]]
    [newline]]

[speed-test]

;[repl]
