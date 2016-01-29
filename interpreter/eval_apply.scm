
(define (sexy-eval code env)
    (define macro-env
        (sexy-environment env))
    (define prog
        (sexy-compile (sexy-expand code macro-env)))
    (prog env top-cont top-err))

(define (sexy-apply obj xs opts cont err)
    (define (apply-or-die)
        (sexy-send obj 'apply
            (lambda (af)
                (sexy-apply af (list xs opts) 'null cont err))
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
                (if (or (eq? type 'λ) (eq? type 'operator) (eq? type 'proc))
                    ((htr obj 'exec) xs opts cont err)
                    (apply-or-die))))
        (else (apply-or-die))))

(define (sexy-apply-wrapper obj)
    (lambda xs
        (sexy-apply obj xs 'null top-cont top-err)))

