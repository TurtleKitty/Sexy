
(define (sexy-repl)
    (define stdin (current-input-port))
    (define stdout (current-output-port))
    (define stderr (current-error-port))
    (define (loop env)
        (define repl-err
            (lambda (ex continue)
                (debug 'runtime-error
                    (if (and (hash-table? ex) (eq? (sexy-send-atomic ex 'type) 'error))
                        (map (lambda (f) (sexy-view (sexy-send-atomic ex f))) '(name to-text form)))
                        (sexy-view ex))
                (loop env)))
        (display "(sexy) ")
        (let ((expr (sexy-read stdin)))
            (if (eof-object? expr)
                (exit)
                (let ((expanded (sexy-expand expr (sexy-environment env))))
                    (define check? (check-sexy-syntax expanded))
                    (if check?
                        (let ((compiled (sexy-compile expanded)))
                            (compiled
                                env
                                (lambda (v)
                                    (define noob   (local-env))
                                    (define mom    (htr env 'mama))
                                    (define evars  (htr env 'vars))
                                    (define mvars  (htr mom 'vars))
                                    (sexy-send-record mvars 'merge
                                        (lambda (fn)
                                            (define nuvars (fn evars))
                                            (define print-me (if (eof-object? v) 'EOF v))
                                            (hts! mom  'vars nuvars)
                                            (hts! noob 'mama mom)
                                            (sexy-write print-me stdout)
                                            (newline)
                                            (loop noob))
                                        repl-err))
                                repl-err))
                        (begin
                            (display "Syntax error!\n")
                            (loop env)))))))
    (newline)
    (display "Welcome to the Sexy Read-Eval-Print Loop.  Press Ctrl-D to exit.")
    (newline)
    (newline)
    (loop (sexy-environment (cli-env))))

