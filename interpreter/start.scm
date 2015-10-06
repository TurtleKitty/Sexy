
(define (start)
    (define args (command-line-arguments))
    (define (fname)
        (if (pair? (cdr args))
            (cadr args)
            (usage)))
    (define (prep-dir path)
        (if (not (directory? path))
            (create-directory path #t)
            #f))
    (prep-dir sexy-mod-dir)
    (prep-dir sexy-cache-dir)
    (if (not (file-exists? sexy-use-symbols))
        (with-output-to-file sexy-use-symbols
            (lambda ()
                (write-string symbols.sxy))))
    (global-env)
    (add-global-prelude)
    (symbols-env)
    (if (not (pair? args))
        (usage)
        (let ((cmd (string->symbol (car args))))
            (case cmd
                ((repl) (sexy-repl))
                ((exec) 
                    (let ((code-str (fname)))
                        (define code
                            (sexy-read-file
                                (open-input-string code-str)))
                        (define expanded
                            (sexy-expand code (cli-env)))
                        (if (check-sexy-syntax expanded)
                            (sexy-run expanded)
                            (exit))))
                ((run)
                    (let ((expanded (read-expand-cache-prog (fname) (cli-env))))
                        (if (check-sexy-syntax expanded)
                            (sexy-run expanded)
                            (exit))))
                ((check)
                    (let ((its-good (check-sexy-syntax (cdr (read-expand-cache-prog (fname) (cli-env))))))
                        (display "Sexy syntax check complete: ")
                        (say (if its-good 'ok 'FAIL))))
                ((clean)
                    (let ((cached (append (glob "~/.sexy/compiled/*") (glob "~/.sexy/modules/*") (list "~/.sexy/global.sxy"))))
                        (let loop ((f (car cached)) (fs (cdr cached)))
                            (delete-file* f)
                            (if (eq? fs '())
                                (display "Sexy cache cleared.\n")
                                (loop (car fs) (cdr fs))))))
                ((compile)
                    (let ((expanded (read-expand-cache-prog (fname) (cli-env))))
                        (debug "Wrote compiled file to " (get-sexy-cached-path (find-file (cadr args))))))
                ((expand)
                    (begin
                        (pp
                            (sexy-view
                                (read-expand-cache-prog (fname) (cli-env))))
                        (newline)))
                (else (printf "Unknown command: ~A~%" cmd))))))

