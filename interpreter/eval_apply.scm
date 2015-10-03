
(define (sexy-eval code env)
    (define macro-env
        (sexy-environment env))
    (define prog
        (sexy-compile (sexy-expand code macro-env)))
    (prog env top-cont top-err))

(define (prepare-sexy-args xs)
    (define (rval args opts)
        (cons (reverse args) opts))
    (if (pair? xs)
        (let* ((options (sexy-record))
               (vars (htr options 'vars))
               (setopt! (lambda (k v) (hts! vars k v))))
            (let loop ((head (car xs)) (tail (cdr xs)) (args '()))
                (if (keyword? head)
                    (let ((k (keyword->symbol head)) (v (car tail)))
                        (setopt! k v)
                        (if (pair? (cdr tail))
                            (loop (cadr tail) (cddr tail) args)
                            (rval args options)))
                    (if (pair? tail)
                        (loop (car tail) (cdr tail) (cons head args))
                        (rval (cons head args) options)))))
        (rval '() (sexy-record))))

(define (sexy-apply obj xs cont err)
    (define (apply-or-die)
        (sexy-send obj 'apply
            (lambda (af)
                (sexy-apply af xs cont err))
              err))
    (cond
        ((procedure? obj)
            (cont 
                (handle-exceptions exn
                    (err
                        (handle-exceptions exn
                            (err "Something went seriously wrong in the Sexy interpreter." identity)
                            (list
                                'exn
                                (list 'location  ((condition-property-accessor 'exn 'location) exn))
                                (list 'arguments ((condition-property-accessor 'exn 'arguments) exn))
                                (list 'message   ((condition-property-accessor 'exn 'message) exn))))
                        (lambda (ys) (cont (apply obj ys))))
                    (apply obj xs))))
        ((hash-table? obj)
            (let ((type (htr obj 'type)))
                (if (or (eq? type 'fn) (eq? type 'operator))
                    (let* ((arg-pair (prepare-sexy-args xs)) (args (car arg-pair)) (opts (cdr arg-pair)))
                        ((htr obj 'exec) args opts cont err))
                    (apply-or-die))))
        (else (apply-or-die))))

(define (sexy-apply-wrapper obj)
    (lambda xs
        (sexy-apply obj xs top-cont top-err)))

