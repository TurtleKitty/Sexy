
; tiny metacircular interpreter for a subset of Scheme

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
                [[eq? head 'eq?] [apply eq? [square-eval-list tail env]]]
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
    [define self
        [lambda [msg]
            [cond
                [[eq? msg 'find] lookup]
                [[eq? msg 'add] extend]
                [[eq? msg 'set!] mutate!]]]]
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

[repl]
